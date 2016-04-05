/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.wga.modules;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;

import de.innovationgate.utils.ClassLoaderProvider;
import de.innovationgate.utils.DynamicClassLoadingChain.SubLoader;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.options.OptionCategoryDefinition;
import de.innovationgate.wga.modules.options.OptionDefinition;
import de.innovationgate.wga.modules.options.ValidationContext;

/**
 * A general purpose module registry for WGA
 * 
 * The capabilities are:
 * <ul>
 * <li> Offers a queryable registration for {@link ModuleDefinition} objects, which are mapped by their module type and their implementation class. There can be exactly one module definition for a combination of module type and implementation class
 * <li> The amount of module types to use is open and can be dynamically extended by defining new implementations of {@link ModuleType}
 * <li> The registry is able to inject itself into {@link ModuleDefinition} instances or even module implementations themselves when they implement {@link RegistryAwareModuleDefinition} and {@link RegistryAwareModule} respectively. So those module (definitions) can use the registry to do further lookups.
 * <li> The registry can also register some arbitrary "context objects", that are needed by registry aware module (definitions) to work. This is a map that is retrievable via {@link #getContextObjects()}
 * <li> The registry can also register "Option Category Definitions" implemented by {@link OptionCategoryDefinition} to provide titles and descriptions for the category names that option definitions provide via {@link OptionDefinition#getCategory()}. There can be exactly one option category for a combination of category name and module type.
 * </ul> 
 *
 */
public class ModuleRegistry {
    
    /**
     * The name of the text file resource which should contain a full qualified name of a {@link ModuleRegistrar} implementation
     */
    public static final String MODULEREGISTRATORS_RESOURCE = "de/innovationgate/wga/modules/registrar.cfg";
    
    /**
     * Module definitions map, storing {@link ModuleDefinition} instances in a two-step String hierarchy
     * - Outer string is the class name of a {@link ModuleType} 
     * - Inner string is the class name of a module implementation class or of the mapped module definition class itself (if there is no implementation class) 
     */
    private Map<String, Map<String,ModuleDefinition>> _moduleDefinitions = new ConcurrentHashMap<String, Map<String,ModuleDefinition>>();
    
    /**
     * Map storing module definitions by the chain subloader they belong to
     */
    private Map<String,List<ModuleDefinition>> _modDefsByLoader = new ConcurrentHashMap<String, List<ModuleDefinition>>();
    
    /**
     * Map storing category definitions by the chain subloader they belong to
     */
    private Map<String,List<OptionCategoryDefinition>> _catDefsByLoader = new ConcurrentHashMap<String, List<OptionCategoryDefinition>>();

    /**
     * Module definitions map for key based module definitions, storing {@link ModuleDefinition} instances in a two-step String hierarchy
     * - Outer string is the class name of a {@link ModuleType} 
     * - Inner string is the key of the a module definition 
     */
    private Map<String, Map<String,ModuleDefinition>> _keyedModuleDefinitions = new ConcurrentHashMap<String, Map<String,ModuleDefinition>>();
    
    
    /**
     * Category definitions map, storing {@link OptionCategoryDefinition} instances
     * - Key string is the class name of the {@link ModuleType} to whose module options the category applies
     */
    private Map<String, Map<String,OptionCategoryDefinition>> _optionCategoryDefinitions = new ConcurrentHashMap<String, Map<String,OptionCategoryDefinition>>();
    
    private Map<String,List<ModuleRegistryChangeListener>> _changeListeners = new ConcurrentHashMap<String, List<ModuleRegistryChangeListener>>();
    
    private ThreadLocal<Set<Class<? extends ModuleType>>> _addedModuleTypes = new ThreadLocal<Set<Class<? extends ModuleType>>>();
    
    /**
     * Modules can register arbitrary context objects for registration operations here
     */
    private Map<Class<?>, Object> _contextObjects = new ConcurrentHashMap<Class<?>, Object>();

    private Logger _log = Logger.getLogger("wga.modules");
    /**
     * Add a module definition to the registry.
     * If the definition implements {@link RegistryAwareModuleDefinition}, this also injects a registry reference via method {@link RegistryAwareModuleDefinition#injectRegistry(ModuleRegistry)}
     */
    public synchronized void addModuleDefinition(ModuleDefinition modDef) {
        
        if (modDef instanceof RegistryAwareModuleDefinition) {
            ((RegistryAwareModuleDefinition) modDef).injectRegistry(this);
        }
        
        if (DeclaringModuleType.class.isAssignableFrom(modDef.getModuleType())) {
            validateModuleDefinition(modDef);
        }
        
        if (modDef instanceof KeyBasedModuleDefinition) {
            KeyBasedModuleDefinition keyModDef = (KeyBasedModuleDefinition) modDef;
            Map<String,ModuleDefinition> keyedModules = getKeyedModulesForType(modDef.getModuleType());
            keyedModules.put(keyModDef.getRegistrationKey(), modDef);
        }
        
        Map<String,ModuleDefinition> modules = getModulesForType(modDef.getModuleType());
        ModuleDefinition oldDefinition = null;
        if (modDef.getImplementationClass() != null) {
            oldDefinition = modules.put(modDef.getImplementationClass().getName(), modDef);
        }
        
        // On key based modules without impl class also use the key to map, mainly so that re-registrations overwrite the previous ones
        else if (modDef instanceof KeyBasedModuleDefinition) {
            KeyBasedModuleDefinition keyModDef = (KeyBasedModuleDefinition) modDef;
            oldDefinition = modules.put(keyModDef.getRegistrationKey(), modDef);
        }
        
        // Store the loader key from which the module definition was loaded, in order do be able to drop it on loader removal
        String loaderKey = null;
        if (modDef instanceof AlternateOriginModuleDefinition) {
            loaderKey = ((AlternateOriginModuleDefinition) modDef).getOriginKey();
        }
        else if (modDef.getClass().getClassLoader() instanceof SubLoader) {
            SubLoader subLoader = (SubLoader) modDef.getClass().getClassLoader();
            loaderKey = subLoader.getKey();
        }
        
        if (loaderKey != null) {
            List<ModuleDefinition> loaderDefs = _modDefsByLoader.get(loaderKey);
            if (loaderDefs == null) {
                loaderDefs = new ArrayList<ModuleDefinition>();
                _modDefsByLoader.put(loaderKey, loaderDefs);
            }
            loaderDefs.add(modDef);
        }
        
        Set<Class<? extends ModuleType>> addedTypes = _addedModuleTypes.get();
        if (addedTypes != null) {
            addedTypes.add(modDef.getModuleType());
        }
        
        if (oldDefinition == null) {
            try {
                _log.debug("Registering " + modDef.getModuleType().newInstance().getTitle() + ": " + modDef.getTitle(Locale.getDefault()));
            }
            catch (Exception e) {
            }
        }
    }
    
    private void validateModuleDefinition(ModuleDefinition modDef) throws ModuleDefinitionValidationException {

        try {
            DeclaringModuleType type = (DeclaringModuleType) modDef.getModuleType().newInstance();
            
            // Validations for whom the module dependencies must be fulfilled: Property and implementation class retrieval
            try {
                modDef.testDependencies(); 
                Object props = modDef.getProperties();
                if (props == null && type.isPropertiesNeeded()) {
                    throw new ModuleDefinitionValidationException("Module definition " + modDef.getClass() + " provides no properties object although module type demands it");
                }
                else if (type.getPropertyClass() != null && props != null && ! type.getPropertyClass().isAssignableFrom(props.getClass())) {
                    throw new ModuleDefinitionValidationException("Properties of module definition " + modDef.getClass() + " do not match neccessary property class " + type.getPropertyClass().getName());
                }
                
                Class<? extends Object> baseClass = type.getImplementationBaseClass();
                if (baseClass != null && !baseClass.isAssignableFrom(modDef.getImplementationClass())) {
                    throw new ModuleDefinitionValidationException("Implementation class of module definition " + modDef.getClass() + " does not match neccessary base class " + type.getImplementationBaseClass().getName());
                }
                
                if (type.isSelfRegistered()) {
                    if (!modDef.getClass().equals(modDef.getImplementationClass())) {
                        throw new ModuleDefinitionValidationException("Module definition " + modDef.getClass() + " is not self-registered, although module type demands it");
                    }
                }
                else {
                    if (modDef.getClass().equals(modDef.getImplementationClass())) {
                        throw new ModuleDefinitionValidationException("Module definition " + modDef.getClass() + " is self-registered, although module type prohibits it");
                    }
                }
                
                if (type.isKeyBased() && !(modDef instanceof KeyBasedModuleDefinition)) {
                    throw new ModuleDefinitionValidationException("Module definition " + modDef.getClass() + " does not implement " + KeyBasedModuleDefinition.class.getName() + " although property type demands it");
                }
            }
            catch (ModuleDependencyException e) {
                // We register modules with missing dependencies nevertheless, so OpenWGA can identify them and document their missing dependencies
            }
        }
        catch (ModuleDefinitionValidationException e) {
            throw e;
        }
        catch (Exception e) {
            throw new ModuleDefinitionValidationException("Exception on validation module definition " + modDef.getClass().getName(), e);
        }
        
    }

    /**
     * Adds an option category definition
     */
    public synchronized void addOptionCategoryDefinition(OptionCategoryDefinition catDef) {
        
        Map<String,OptionCategoryDefinition> cats =  getOptionCategoriesForType(catDef.getModuleType());
        cats.put(catDef.getKey(), catDef);
        
        // If from the class loading chain mark from which subloader it came, so it can be dropped when it is dropped
        if (catDef.getClass().getClassLoader() instanceof SubLoader) {
            SubLoader subLoader = (SubLoader) catDef.getClass().getClassLoader();
            List<OptionCategoryDefinition> loaderCats = _catDefsByLoader.get(subLoader.getKey());
            if (loaderCats == null) {
                loaderCats = new ArrayList<OptionCategoryDefinition>();
                _catDefsByLoader.put(subLoader.getKey(), loaderCats);
            }
            loaderCats.add(catDef);
        }
        
    }
    
    public synchronized void removeDefinitionsFromLoader(String loaderKey) {
        
        Set<Class<? extends ModuleType>> removedTypes = new HashSet<Class<? extends ModuleType>>();
        List<ModuleDefinition> loaderDefs = _modDefsByLoader.get(loaderKey);
        if (loaderDefs != null) {
            for (ModuleDefinition modDef : loaderDefs) {
                removedTypes.add(modDef.getModuleType());
                removeModuleDefinition(modDef);
            }
            loaderDefs.clear();
        }
        
        List<OptionCategoryDefinition> loaderCats = _catDefsByLoader.get(loaderKey);
        if (loaderCats != null) {
            for (OptionCategoryDefinition modDef : loaderCats) {
                removeOptionCategoryDefinition(modDef);
            }
            loaderCats.clear();
        }
        
        for (Class<? extends ModuleType> moduleType : removedTypes) {
            notifyListener(moduleType);
        }
        
    }

    private void removeOptionCategoryDefinition(OptionCategoryDefinition catDef) {
        Map<String,OptionCategoryDefinition> catMap = getOptionCategoriesForType(catDef.getModuleType());
        catMap.remove(catDef.getKey());
        
        if (catDef instanceof RemovalAwareModule) {
            ((RemovalAwareModule) catDef).moduleWasRemoved(this);
        }
    }

    private void removeModuleDefinition(ModuleDefinition modDef) {

        Map<String,ModuleDefinition> defMap = getModulesForType(modDef.getModuleType());
        if (modDef.getImplementationClass() != null) {
            defMap.remove(modDef.getImplementationClass().getName());
        }
        else if (modDef instanceof KeyBasedModuleDefinition) {
            KeyBasedModuleDefinition keyModDef = (KeyBasedModuleDefinition) modDef;
            defMap.remove(keyModDef.getRegistrationKey());
        }
        
        if (modDef instanceof KeyBasedModuleDefinition) {
            KeyBasedModuleDefinition keyModDef = (KeyBasedModuleDefinition) modDef;
            Map<String,ModuleDefinition> keyedModules = getKeyedModulesForType(modDef.getModuleType());
            keyedModules.remove(keyModDef.getRegistrationKey());
        }
        
        if (modDef instanceof RemovalAwareModule) {
            ((RemovalAwareModule) modDef).moduleWasRemoved(this);
        }
        
    }

    /**
     * Returns the {@link Map} that stores all option category definitions for a given module type.
     * Modifying this map also modifies the registration!
     * @param moduleType The type of module for whom the registered option categories are retrieved
     * @return The map. The keys are option category names. The values are the {@link OptionCategoryDefinition}s themselves
     */
    public Map<String, OptionCategoryDefinition> getOptionCategoriesForType(Class<? extends ModuleType> moduleType) {
        Map<String, OptionCategoryDefinition> cats = _optionCategoryDefinitions.get(moduleType.getName());
        if (cats == null) {
            cats = new ConcurrentHashMap<String,OptionCategoryDefinition>();
            _optionCategoryDefinitions.put(moduleType.getName(), cats);
        }
        return cats;
    }
    
    /**
     * Returns an option category definition
     * @param moduleType The module type for whom the category was registered
     * @param key The category name as returned by option definitions via {@link OptionDefinition#getCategory()}
     * @return The option category definition or null if nothing is registered
     */
    public OptionCategoryDefinition getOptionCategoryDefinition(Class<? extends ModuleType> moduleType, String key) {
        
        Map<String, OptionCategoryDefinition> cats = getOptionCategoriesForType(moduleType);
        return cats.get(key);
        
    }

    /**
     * Returns the {@link Map} that stores all module definitions of a given type.
     * Modifying this map also modifies the registration!
     * @param moduleType The type of module
     * @return The map. The keys are the full qualified names of implementation classes or {@link #KEYBASED_MODULE_PREFIX} plus key for {@link KeyBasedModuleDefinition} instantations. The values are the {@link ModuleDefinition}s themselves
     */
    public synchronized Map<String,ModuleDefinition> getModulesForType(Class<? extends ModuleType> moduleType) {
        return getModulesForType(moduleType.getName());
    }
    
    public synchronized Map<String,ModuleDefinition> getModulesForType(String moduleTypeClassName) {
        Map<String, ModuleDefinition> modules = _moduleDefinitions.get(moduleTypeClassName);
        if (modules == null) {
            modules = new LinkedHashMap<String,ModuleDefinition>();
            _moduleDefinitions.put(moduleTypeClassName, modules);
        }
        return modules;
    }
    
    private synchronized Map<String,ModuleDefinition> getKeyedModulesForType(Class<? extends ModuleType> moduleType) {
        Map<String, ModuleDefinition> modules = _keyedModuleDefinitions.get(moduleType.getName());
        if (modules == null) {
            modules = new LinkedHashMap<String,ModuleDefinition>();
            _keyedModuleDefinitions.put(moduleType.getName(), modules);
        }
        return modules;
        
    }
    
    /**
     * Returns a module definition for a given module type and implementation class
     * @param modType The full qualified class name of the module type
     * @param className The full qualified class name of the implementation class
     * @return The definiton or null if there is nothing registered
     */
    public ModuleDefinition getModuleDefinition(String modType, String className) {
        Map<String,ModuleDefinition> modules = getModulesForType(modType);
        return modules.get(className);
    }

    /**
     * Returns a module definition for a givne module type and implementation class
     * @param moduleType The module type
     * @param className the full qualified name of the implementation class
     * @return The definiton or null if there is nothing registered
     */
    public ModuleDefinition getModuleDefinition(Class<? extends ModuleType> moduleType, String className) {
        Map<String,ModuleDefinition> modules = getModulesForType(moduleType);
        return modules.get(className);
    }
    
    /**
     * Tries to retrieve a {@link KeyBasedModuleDefinition} from registry via it's key
     * @param moduleType The type of module
     * @param regKey The registration key that the module definition returns via {@link KeyBasedModuleDefinition#getRegistrationKey()}
     * @return The module definition or null if nothing is registered
     */
    public ModuleDefinition getModuleDefinitionByKey(Class<? extends ModuleType> moduleType, String regKey) {
        Map<String,ModuleDefinition> modules = getKeyedModulesForType(moduleType);
        return modules.get(regKey);
    }
    
    /**
     * Returns a module definition for a givne module type and implementation class
     * @param moduleType The module type
     * @param implClass The implementation class
     * @return The definiton or null if there is nothing registered
     */
    public ModuleDefinition getModuleDefinition(Class<? extends ModuleType> moduleType, Class<?> implClass) { 
        return getModuleDefinition(moduleType, implClass.getName());
    }
    
    /**
     * Triggers the search for {@link ModuleRegistrar}s on the given class loader.
     * The method searches for all known text file resources of name {@value #MODULEREGISTRATORS_RESOURCE} on the class loader.
     * These files should contain the full qualified name of a {@link ModuleRegistrar} implementation which then is instantiated.
     * The method {@link ModuleRegistrar#registerModules(ModuleRegistry)} is called so it can register it's known module definitions.
     * @param classLoader The class loader to load registrars from
     * @throws IOException
     */
    public synchronized void searchModuleDefinitions(ClassLoaderProvider classLoaderProvider) throws IOException {

        _addedModuleTypes.set(new HashSet<Class<? extends ModuleType>>());
        
        ClassLoader classLoader = classLoaderProvider.getClassLoader();
        Enumeration<URL> registratorResources = classLoader.getResources(MODULEREGISTRATORS_RESOURCE);
        while (registratorResources.hasMoreElements()) {
            registerModules(classLoader, registratorResources.nextElement());
        }
        
        for (CustomModuleRegistrationService service : getCustomModuleRegistrationServices()) {
            service.searchModuleDefinitions(this, classLoader);
        }
        
        for (Class<? extends ModuleType> moduleType : _addedModuleTypes.get()) {
            notifyListener(moduleType);
        }
        _addedModuleTypes.remove();
        
    }
    
    
    private Iterable<CustomModuleRegistrationService> getCustomModuleRegistrationServices() {
        
        List<CustomModuleRegistrationService> services = new ArrayList<CustomModuleRegistrationService>();
        for (ModuleDefinition def : getModulesForType(CustomModuleRegistrationServiceType.class).values()) {
            try {
                CustomModuleRegistrationService service = (CustomModuleRegistrationService) instantiate(def);
                services.add(service);
            }
            catch (ModuleInstantiationException e) {
                _log.error("Exception instantiating custom module registration service " + def.getImplementationClass(), e);
            }
            
        }
        return services;
    }

    /**
     * Triggers the search for {@link ModuleRegistrar}s on the given class loader, but only those whose registration configs are on a list of registrar class loaders. 
     * The method searches for all known text file resources of name {@value #MODULEREGISTRATORS_RESOURCE} on the registrarCfgLoaders.
     * However it only searches for these resources right on these class loaders, not on their parent loaders.
     * These files should contain the full qualified name of a {@link ModuleRegistrar} implementation which then is instantiated.
     * The method {@link ModuleRegistrar#registerModules(ModuleRegistry)} is called so it can register it's known module definitions.
     * @param classLoader The class loader to load registrar and module classes from
     * @param registrarCfgLoaders The class loader to load registrar configurations from.
     * @throws IOException
     */
    public synchronized void searchModuleDefinitions(ClassLoaderProvider classLoaderProvider, Iterable<URLClassLoader> registrarCfgLoaders) throws IOException {
        
        _addedModuleTypes.set(new HashSet<Class<? extends ModuleType>>());
        
        ClassLoader classLoader = classLoaderProvider.getClassLoader();
        for (URLClassLoader registrarCfgLoader : registrarCfgLoaders) {
            Enumeration<URL> registratorResources = registrarCfgLoader.findResources(MODULEREGISTRATORS_RESOURCE);
            while (registratorResources.hasMoreElements()) {
                registerModules(classLoader, registratorResources.nextElement());
            }
        }
        
        for (CustomModuleRegistrationService service : getCustomModuleRegistrationServices()) {
            service.searchModuleDefinitions(this, classLoader, registrarCfgLoaders);
        }
        
        for (Class<? extends ModuleType> moduleType : _addedModuleTypes.get()) {
            notifyListener(moduleType);
        }
        _addedModuleTypes.remove();
        
    }

    private void registerModules(ClassLoader classLoader, URL url) throws UnsupportedEncodingException, IOException {
        _log.debug("Processing module registrar at URL " + url.toString());
        LineNumberReader reader = new LineNumberReader(new InputStreamReader(url.openStream(), "UTF-8"));
        String line;
        while ((line = reader.readLine()) != null) {
            String className = line.trim();
            try {
                @SuppressWarnings("unchecked")
                Class<ModuleRegistrar> registratorClass = (Class<ModuleRegistrar>) classLoader.loadClass(className);
                registratorClass.newInstance().registerModules(this);
            }
            catch (Throwable e) {
                _log.error("Exception using module registrator " + className + " from registrator resource " + url.toString(), e);
            }
        }
        reader.close();
    }

    private void notifyListener(Class<? extends ModuleType> moduleType) {
        List<ModuleRegistryChangeListener> listeners = _changeListeners.get(moduleType.getName());
        if (listeners != null) {
            for (ModuleRegistryChangeListener listener : listeners) {
                listener.moduleRegistryChanged(this, moduleType);
            }
        }
        
    }

    /**
     * Creates a validation context for option validations
     */
    public ValidationContext createValidationContext(WGAConfiguration configCopy) {
        return new ValidationContext(this, configCopy);
    }
    
    

    /**
     * Returns a map of arbitrary context objects that may be needed for registry aware {@link ModuleDefinition} implementations or modules themselves to work
     */
    public Map<Class<?>, Object> getContextObjects() {
        return _contextObjects;
    }
    
    /**
     * Method to create a module instantiation.
     * While modules are obliged to have a default constructor and therefor could be directly instantiated, module instantiation functions SHOULD use this method
     * (or {@link #instantiate(Class)} to create module instances, because it is capable of injecting the registry to {@link RegistryAwareModule}s.
     * @param def The definition of the module
     * @return The module instance
     * @throws ModuleInstantiationException
     */
    public Object instantiate(ModuleDefinition def) throws ModuleInstantiationException {
        
        if (def.getImplementationClass() == null) {
            throw new ModuleInstantiationException("The module definition does not offer an implementation class");
        }
        
        Class<? extends Object> moduleClass = def.getImplementationClass();
        return instantiate(moduleClass);
    }

    /**
     * Method to create a module instantiation.
     * While modules are obliged to have a default constructor and therefor could be directly instantiated, module instantiation functions SHOULD use this method
     * (or {@link #instantiate(ModuleDefinition)} to create module instances, because it is capable of injecting the registry to {@link RegistryAwareModule}s.
     * @param moduleClass The implementation class of the module
     * @return The module instance
     * @throws ModuleInstantiationException
     */
    public Object instantiate(Class<? extends Object> moduleClass) throws ModuleInstantiationException {
        try {
            Object obj = moduleClass.newInstance();
            if (obj instanceof RegistryAwareModule) {
                ((RegistryAwareModule) obj).injectRegistry(this);
            }
            return obj;
        }
        catch (Throwable e) {
            throw new ModuleInstantiationException("Exception instantiating module " + moduleClass, e);
        }
    }

    public Set<String> getModuleTypes() {
        return _moduleDefinitions.keySet();
    }
    
    public synchronized void addChangeListener(ModuleRegistryChangeListener listener, Class<? extends ModuleType> moduleType) {
        List<ModuleRegistryChangeListener> listeners = _changeListeners.get(moduleType.getName());
        if (listeners == null) {
            listeners = new ArrayList<ModuleRegistryChangeListener>();
            _changeListeners.put(moduleType.getName(), listeners);
        }
        listeners.add(listener);
    }
    
    public void clearChangeListeners() {
        _changeListeners.clear();
    }
    
    public void removeChangeListener(ModuleRegistryChangeListener listener) {
        
        for (List<ModuleRegistryChangeListener> listeners : _changeListeners.values()) {
            listeners.remove(listener);
        }
        
    }
}