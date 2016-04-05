/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.wgpublisher.modules.serveroptions;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;

public class BasicServicesModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {

    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/services", getClass().getClassLoader());
    @SuppressWarnings("unused")
    private ModuleRegistry _registry;
    
    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("options.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return this.getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return ServicesCollector.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition webServices = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_WEBSERVICES, BooleanOptionType.INSTANCE, _bundleLoader);
        webServices.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_SERVICE_WEBSERVICES);
        options.addOption(webServices);
        
        LocalizedOptionDefinition startPage = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_STARTPAGE, BooleanOptionType.INSTANCE, _bundleLoader);
        startPage.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_SERVICE_STARTPAGE);
        options.addOption(startPage);
        
        LocalizedOptionDefinition adminPage = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_ADMIMPAGE, BooleanOptionType.INSTANCE, _bundleLoader);
        adminPage.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_SERVICE_ADMINPAGE);
        adminPage.setOptional(true);
        options.addOption(adminPage);
        
        LocalizedOptionDefinition jmxEnabled = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_ENABLED, BooleanOptionType.INSTANCE, _bundleLoader);
        jmxEnabled.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(jmxEnabled);
        
        LocalizedOptionDefinition jmxHost = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_HOST, StringOptionType.INSTANCE, _bundleLoader);
        jmxHost.setOptional(true);
        options.addOption(jmxHost);
        
        LocalizedOptionDefinition jmxSslEnabled = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_SSL, BooleanOptionType.INSTANCE, _bundleLoader);
        jmxSslEnabled.setDefaultValue(Boolean.FALSE.toString());
        jmxSslEnabled.setOptional(true);
        options.addOption(jmxSslEnabled);
        
        LocalizedOptionDefinition jmxPort = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_PORT_SERVICE, IntegerOptionType.INSTANCE, _bundleLoader);
        jmxPort.setDefaultValue((new Integer(WGAConfiguration.SERVEROPTIONDEFAULT_SERVICE_INTEGRATEDJMX_PORT_SERVICE)).toString());
        jmxPort.setOptional(true);
        options.addOption(jmxPort);
        
        LocalizedOptionDefinition jmxRegistryPort = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_PORT_REGISTRY, IntegerOptionType.INSTANCE, _bundleLoader);
        jmxRegistryPort.setDefaultValue((new Integer(WGAConfiguration.SERVEROPTIONDEFAULT_SERVICE_INTEGRATEDJMX_PORT_REGISTRY)).toString());
        jmxRegistryPort.setOptional(true);
        options.addOption(jmxRegistryPort);
        
        LocalizedOptionDefinition jmxLegacyMonitoring = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_INTEGRATEDJMX_LEGACY_DBCP, BooleanOptionType.INSTANCE, _bundleLoader);
        jmxLegacyMonitoring.setOptional(true);
        jmxLegacyMonitoring.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(jmxLegacyMonitoring);
        
        LocalizedOptionDefinition webSocketsEnabled = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_WEBSOCKETS, BooleanOptionType.INSTANCE, _bundleLoader);
        webSocketsEnabled.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(webSocketsEnabled);

        return options;
        
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("options.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
        
    }

}
