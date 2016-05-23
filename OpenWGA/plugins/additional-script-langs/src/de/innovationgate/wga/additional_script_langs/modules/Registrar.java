package de.innovationgate.wga.additional_script_langs.modules;

import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;

public class Registrar implements ModuleRegistrar {

    public void registerModules(ModuleRegistry registry) {
        registry.addModuleDefinition(new SassConversionModuleDefinition());
        registry.addModuleDefinition(new TMLScriptCoffeeScriptConversionModuleDefinition());
        registry.addModuleDefinition(new JSCoffeeScriptConversionModuleDefinition());
        registry.addModuleDefinition(new TMLScriptTypeScriptConversionModuleDefinition());
        registry.addModuleDefinition(new JSTypeScriptConversionModuleDefinition());
    }

}
