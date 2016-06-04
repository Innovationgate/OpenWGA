package de.innovationgate.wga.cmm.modules;

import java.util.Locale;

import de.innovationgate.wga.cmm.CmmRegistrationService;
import de.innovationgate.wga.modules.CustomModuleRegistrationServiceType;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;

public class CmmRegistrationServiceModuleDefinition implements ModuleDefinition {

    public String getTitle(Locale locale) {
        return "CMM Registration Service";
    }

    public String getDescription(Locale locale) {
        return "Registers CM modules based on cmm.json";
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Class<? extends ModuleType> getModuleType() {
        return CustomModuleRegistrationServiceType.class;
    }

    public Class<? extends Object> getImplementationClass() {
        return CmmRegistrationService.class;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public Object getProperties() {
        return null;
    }

}
