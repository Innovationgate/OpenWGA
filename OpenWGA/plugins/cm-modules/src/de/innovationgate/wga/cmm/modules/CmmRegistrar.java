package de.innovationgate.wga.cmm.modules;

import de.innovationgate.wga.cmm.modules.CmmHtmlHeadInclusionModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;

public class CmmRegistrar implements ModuleRegistrar {

    public void registerModules(ModuleRegistry registry) {
        registry.addModuleDefinition(new CmmRegistrationServiceModuleDefinition());
        registry.addModuleDefinition(new CmmHtmlHeadInclusionModuleDefinition());
    }

}
