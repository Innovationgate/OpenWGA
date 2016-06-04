package de.innovationgate.wga.modules.types;

import de.innovationgate.wga.cmm.definition.Definition;
import de.innovationgate.wga.modules.DeclaringModuleType;
import de.innovationgate.wga.modules.ModuleType;

public class CmmDefinitionModuleType implements DeclaringModuleType {

    public String getDescription() {
        return "Definition of an OpenWGA content management module";
    }

    public String getTitle() {
        return "Content Management Module Definition Type";
    }

    public boolean isKeyBased() {
        return true;
    }

    public boolean isSelfRegistered() {
        return false;
    }

    public Class<? extends Object> getImplementationBaseClass() {
        return null;
    }

    public boolean isPropertiesNeeded() {
        return true;
    }

    public Class<? extends Object> getPropertyClass() {
        return Definition.class;
    }

}
