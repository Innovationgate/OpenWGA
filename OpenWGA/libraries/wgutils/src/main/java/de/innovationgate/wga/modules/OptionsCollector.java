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

import java.util.Iterator;
import java.util.Locale;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;

/**
 * Base class for a special kind of {@link ModuleDefinition} that, instead of directly providing option definitions, will collect option definitions from yet other {@link ModuleDefinition}s.
 * To use this:
 * <ul>
 * <li>Create a subclass of {@link OptionsCollector} implementing the abstract classes as usual. Overwrite {@link #getImplementationClass()}, {@link #getProperties()}, {@link #testDependencies()} as neccessary
 * <li>On all {@link ModuleDefinition}s that should provide options for this collector return the {@link OptionsCollector} subclass itself on {@link ModuleDefinition#getModuleType()} (OptionCollectors are a ModuleDefinition AND a ModuleType!) 
 * </ul>
 * 
 * When that's done your OptionsCollector should return all option definitions on {@link #getOptionDefinitions()} that are returned by your "special type" module definitions
 *   
 */
public abstract class OptionsCollector implements ModuleDefinition, DeclaringModuleType, RegistryAwareModuleDefinition {

    public boolean isKeyBased() {
        return false;
    }

    public boolean isSelfRegistered() {
        return true;
    }

    public Class<? extends Object> getImplementationBaseClass() {
        return null;
    }

    public boolean isPropertiesNeeded() {
        return false;
    }

    public Class<? extends Object> getPropertyClass() {
        return null;
    }

    protected ModuleRegistry _registry;

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        Class<? extends OptionsCollector> typeClass = getClass();
        collectOptions(options, typeClass);
        
        return options;
        
    }

    protected void collectOptions(OptionDefinitionsMap options, Class<? extends OptionsCollector> typeClass) {
        Iterator<ModuleDefinition> optionDefs = _registry.getModulesForType(typeClass).values().iterator();
        while (optionDefs.hasNext()) {
             ModuleDefinition optDef = optionDefs.next();
             options.putAll(optDef.getOptionDefinitions());
        }
    }

    public Object getProperties() {
        return null;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }
    
    public String getTitle() {
        return getTitle(Locale.getDefault());
    }
    
    public String getDescription() {
        return getDescription(Locale.getDefault());
    }

}
