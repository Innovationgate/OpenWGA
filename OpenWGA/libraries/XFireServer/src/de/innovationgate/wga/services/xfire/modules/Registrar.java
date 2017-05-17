/*
 * Created on 26.08.2009 from oliver
 *
 */
package de.innovationgate.wga.services.xfire.modules;

import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;

public class Registrar implements ModuleRegistrar {

    public void registerModules(ModuleRegistry registry) {
        
        // WGAServices protocol implementation
        registry.addModuleDefinition(new WGAServicesXFireCoreModuleDefinition());
        registry.addModuleDefinition(new WGAServicesXFireCustomModuleDefinition());

    }

}
