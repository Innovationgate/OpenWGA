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
import de.innovationgate.wga.modules.options.MultiPredefinedValuesOptionType;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.cms.CmsApiResource;
import de.innovationgate.wga.services.rest.v1.resources.custom.CustomApiResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelApiResource;
import de.innovationgate.wga.services.rest.v1.resources.query.QueryApiResource;
import de.innovationgate.wgpublisher.modules.poptions.ContentStorePublisherOptionsCollector;

public class RestServicePublisherOptionsModuleDefinition implements ModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/poptions", getClass().getClassLoader());

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
        
        MultiPredefinedValuesOptionType apis = new MultiPredefinedValuesOptionType(_bundleLoader, RestApplication.DBATTRIB_ENABLED_APIS);
        apis.addValue(CmsApiResource.RESOURCE_TYPE);
        apis.addValue(HdbmodelApiResource.RESOURCE_TYPE);
        apis.addValue(QueryApiResource.RESOURCE_TYPE);
        apis.addValue(CustomApiResource.RESOURCE_TYPE);
        
        LocalizedOptionDefinition enabledApis = new LocalizedOptionDefinition(RestApplication.DBATTRIB_ENABLED_APIS, apis, _bundleLoader);
        enabledApis.setOptional(true);
        options.addOption(enabledApis);
        
        LocalizedOptionDefinition forceRegularLogin = new LocalizedOptionDefinition(RestApplication.DBATTRIB_FORCE_REGULAR_LOGIN, BooleanOptionType.INSTANCE, _bundleLoader);
        forceRegularLogin.setOptional(true);
        forceRegularLogin.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(forceRegularLogin);
        
        return options;
        
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return ContentStorePublisherOptionsCollector.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return RestServicePublisherOptionsModuleDefinition.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        return null;
    }

}
