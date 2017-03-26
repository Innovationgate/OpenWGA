package de.innovationgate.wga.services.rest.modules;

import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;

public class Registrar implements ModuleRegistrar {

    public void registerModules(ModuleRegistry registry) {
        registry.addModuleDefinition(new RestServiceModuleDefinition());
        registry.addModuleDefinition(new RestServicePublisherOptionsModuleDefinition());
        registry.addModuleDefinition(new RestServiceServicesOptionsModuleDefinition());
    }

}
