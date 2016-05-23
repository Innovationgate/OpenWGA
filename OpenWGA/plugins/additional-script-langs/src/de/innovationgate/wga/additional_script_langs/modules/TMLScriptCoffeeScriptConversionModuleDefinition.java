package de.innovationgate.wga.additional_script_langs.modules;

import java.util.Locale;

import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.additional_script_langs.coffeescript.TMLScriptCoffeeScriptConversion;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.DesignResourceConversionModuleType;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversionProperties;

public class TMLScriptCoffeeScriptConversionModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {

    public String getTitle(Locale locale) {
        return "CoffeeScript Conversion for TMLScript";
    }

    public String getDescription(Locale locale) {
        return "Converts TMLScript module files with Suffix .coffee to TMLScript";
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Class<DesignResourceConversionModuleType> getModuleType() {
        return DesignResourceConversionModuleType.class;
    }

    public Class<TMLScriptCoffeeScriptConversion> getImplementationClass() {
        return TMLScriptCoffeeScriptConversion.class;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public Object getProperties() {
        DesignResourceConversionProperties props = new DesignResourceConversionProperties();
        props.setDesignType(WGDocument.TYPE_CSSJS);
        props.setCodeType(WGScriptModule.CODETYPE_TMLSCRIPT);
        props.getSuffixes().add("coffee");
        return props;
    }

    public String getRegistrationKey() {
        return WGDocument.TYPENAME_CSSJS + "/" + WGScriptModule.CODETYPE_TMLSCRIPT + "/coffee";
    }

}
