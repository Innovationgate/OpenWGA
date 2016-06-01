package de.innovationgate.wga.cmm.modules;

import java.util.Locale;

import de.innovationgate.wga.cmm.definition.Definition;
import de.innovationgate.wga.modules.AlternateOriginModuleDefinition;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.CmmDefinitionModuleType;

public class CmmModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition, AlternateOriginModuleDefinition {

    private Definition _def;
    private String _dbKey;
    
    public CmmModuleDefinition(Definition def, String dbKey) {
        _def = def;
        _dbKey = dbKey;
    }

    public String getDescription(Locale locale) {
        return _def.getConfigtml();
    }

    public Class<? extends Object> getImplementationClass() {
        return null;
    }

    public Class<? extends ModuleType> getModuleType() {
        return CmmDefinitionModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Object getProperties() {
        return _def;
    }

    public String getTitle(Locale locale) {
        return _def.getConfigtml();
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public String getRegistrationKey() {
        return _def.getDbkey() + "/" + _def.getConfigtml();
    }

    public String getOriginKey() {
        return _dbKey;
    }

}
