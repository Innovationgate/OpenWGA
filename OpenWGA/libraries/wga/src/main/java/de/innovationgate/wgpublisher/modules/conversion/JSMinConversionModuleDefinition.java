package de.innovationgate.wgpublisher.modules.conversion;

import java.util.Locale;

import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.DesignResourceConversionModuleType;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversionProperties;
import de.innovationgate.wgpublisher.design.conversion.jsmin.JSMinConversion;

public class JSMinConversionModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {

    public String getTitle(Locale locale) {
        return "JSMin Conversion";
    }

    public String getDescription(Locale locale) {
        return "Converts JS module files with Suffix .jsmin to JS";
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Class<DesignResourceConversionModuleType> getModuleType() {
        return DesignResourceConversionModuleType.class;
    }

    public Class<JSMinConversion> getImplementationClass() {
        return JSMinConversion.class;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public Object getProperties() {
        DesignResourceConversionProperties props = new DesignResourceConversionProperties();
        props.setDesignType(WGDocument.TYPE_CSSJS);
        props.setCodeType(WGScriptModule.CODETYPE_JS);
        props.getSuffixes().add("jsmin");
        return props;
    }

    public String getRegistrationKey() {
        return WGDocument.TYPENAME_CSSJS + "/" + WGScriptModule.CODETYPE_JS + "/jsmin";
    }

}
