package de.innovationgate.wga.additional_script_langs.modules;

import java.util.Locale;

import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.additional_script_langs.coffeescript.JSCoffeeScriptConversion;
import de.innovationgate.wga.additional_script_langs.typescript.JSTypeScriptConversion;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.DesignResourceConversionModuleType;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversionProperties;

public class JSTypeScriptConversionModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {

    public String getTitle(Locale locale) {
        return "TypeScript Conversion for JavaScript";
    }

    public String getDescription(Locale locale) {
        return "Converts TypeScript module files with Suffix .ts to JavaScript";
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Class<DesignResourceConversionModuleType> getModuleType() {
        return DesignResourceConversionModuleType.class;
    }

    public Class<JSTypeScriptConversion> getImplementationClass() {
        return JSTypeScriptConversion.class;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public Object getProperties() {
        DesignResourceConversionProperties props = new DesignResourceConversionProperties();
        props.setDesignType(WGDocument.TYPE_CSSJS);
        props.setCodeType(WGScriptModule.CODETYPE_JS);
        props.getSuffixes().add("ts");
        return props;
    }

    public String getRegistrationKey() {
        return WGDocument.TYPENAME_CSSJS + "/" + WGScriptModule.CODETYPE_JS + "/ts";
    }

}
