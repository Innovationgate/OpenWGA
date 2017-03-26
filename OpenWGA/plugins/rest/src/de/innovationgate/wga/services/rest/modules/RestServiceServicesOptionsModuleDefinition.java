package de.innovationgate.wga.services.rest.modules;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wgpublisher.modules.serveroptions.ServicesCollector;

public class RestServiceServicesOptionsModuleDefinition implements ModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/serveroptions", getClass().getClassLoader());

    @Override
    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("module.title");
    }

    @Override
    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("module.description");
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition webServices = new LocalizedOptionDefinition(RestApplication.SERVEROPTION_SERVICE_REST, BooleanOptionType.INSTANCE, _bundleLoader);
        webServices.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(webServices);

        
        return options;
        
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return ServicesCollector.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return RestServiceServicesOptionsModuleDefinition.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        return null;
    }

}
