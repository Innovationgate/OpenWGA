package de.innovationgate.wgpublisher.modules;

import java.util.Locale;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.types.HttpSessionManagerModuleType;
import de.innovationgate.wgpublisher.sessions.InMemoryHttpSessionManager;

public class InMemoryHttpSessionManagerModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    private ModuleRegistry _registry;
    
    @Override
    public String getTitle(Locale locale) {        
        return "OpenWGA HttpSessionManager";
    }

    @Override
    public String getDescription(Locale locale) {
        return "Manages HTTP Sessions";
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();        
        
//        LocalizedOptionDefinition jvmRoute = new LocalizedOptionDefinition(InMemoryHttpClusterSessionManager.OPTION_JVMROUTE, StringOptionType.INSTANCE, _bundleLoader);
//        jvmRoute.setOptional(true);
//        options.addOption(jvmRoute);
        
        return options;
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return HttpSessionManagerModuleType.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return InMemoryHttpSessionManager.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        return null;
    }

    @Override
    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

}
