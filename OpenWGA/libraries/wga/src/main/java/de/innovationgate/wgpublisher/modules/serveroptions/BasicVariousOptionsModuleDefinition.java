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
import de.innovationgate.wga.modules.options.MultilineStringOptionType;
import de.innovationgate.wga.modules.options.PasswordEncodingsOptionType;
import de.innovationgate.wga.modules.options.SemicolonSeparatedListOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.options.TextEncodingOptionType;
import de.innovationgate.wga.modules.options.WorkflowEngineOptionType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.events.EventManager;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType;

public class BasicVariousOptionsModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/various", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("options.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return VariousOptionsCollector.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition outputEncoding = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_ENCODING_OUTPUT, TextEncodingOptionType.INSTANCE, _bundleLoader);
        outputEncoding.setDefaultValue("UTF-8");
        options.addOption(outputEncoding);

        LocalizedOptionDefinition designEncoding = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_ENCODING_DESIGN, TextEncodingOptionType.INSTANCE, _bundleLoader);
        designEncoding.setDefaultValue("UTF-8");
        options.addOption(designEncoding);
        
        LocalizedOptionDefinition webTMLBuffer = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_WEBTML_OUTPUT_BUFFER, IntegerOptionType.INSTANCE, _bundleLoader);
        webTMLBuffer.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_WEBTML_OUTPUT_BUFFER);
        webTMLBuffer.setOptional(true);
        webTMLBuffer.setExpert(true);
        options.addOption(webTMLBuffer);
        
        LocalizedOptionDefinition webTMLHeader = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_WEBTML_HEADER, MultilineStringOptionType.INSTANCE, _bundleLoader);
        webTMLHeader.setOptional(true);
        options.addOption(webTMLHeader);
        
        LocalizedOptionDefinition webTmlDirectOutput = new LocalizedOptionDefinition(WGACore.SERVEROPTION_WEBTML_DIRECT_OUTPUT, BooleanOptionType.INSTANCE, _bundleLoader);
        webTmlDirectOutput.setDefaultValue(Boolean.TRUE.toString());
        webTmlDirectOutput.setExpert(true);
        webTmlDirectOutput.setOptional(true);
        options.addOption(webTmlDirectOutput);
        
        LocalizedOptionDefinition pwdEncoding = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_SECURITY_PASSWORD_ENCODING, new PasswordEncodingsOptionType(_registry), _bundleLoader);
        pwdEncoding.setOptional(true);
        pwdEncoding.setExpert(true);
        pwdEncoding.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_SECURITY_PASSWORD_ENCODING);
        options.addOption(pwdEncoding);
        
        LocalizedOptionDefinition defaultDB = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_RESOURCES_DEFAULTDB, new DatabasesOptionType(_registry, true), _bundleLoader);
        defaultDB.setOptional(true);
        options.addOption(defaultDB);
        
        LocalizedOptionDefinition favicon = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_RESOURCES_FAVICON, StringOptionType.INSTANCE, _bundleLoader);
        favicon.setOptional(true);
        options.addOption(favicon);
        
        LocalizedOptionDefinition libraries = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_LIBRARIES, SemicolonSeparatedListOptionType.INSTANCE, _bundleLoader);
        libraries.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_LIBRARIES);
        libraries.setOptional(true);
        libraries.setExpert(true);
        options.addOption(libraries);
        
        LocalizedOptionDefinition defaultWFE = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_DEFAULT_WORKFLOW_ENGINE, new WorkflowEngineOptionType(_registry), _bundleLoader);
        defaultWFE.setOptional(true);
        options.addOption(defaultWFE);
        
        LocalizedOptionDefinition permanentRedirect = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_PERMANENT_REDIRECT, BooleanOptionType.INSTANCE, _bundleLoader);
        permanentRedirect.setOptional(true);
        permanentRedirect.setExpert(true);
        permanentRedirect.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_PERMANENT_REDIRECT);
        options.addOption(permanentRedirect);
        
        LocalizedOptionDefinition scalingThreshold = new LocalizedOptionDefinition(WGACore.SERVEROPTION_SERVER_SCALINGTHRESHOLD, IntegerOptionType.INSTANCE, _bundleLoader);
        scalingThreshold.setOptional(true);
        scalingThreshold.setDefaultValue("10");
        options.addOption(scalingThreshold);
        
        LocalizedOptionDefinition maxUploadSize = new LocalizedOptionDefinition(WGACore.SERVEROPTION_WEBTML_FILEUPLAD_MAXSIZE, IntegerOptionType.INSTANCE, _bundleLoader);
        maxUploadSize.setOptional(true);
        maxUploadSize.setDefaultValue("10");
        options.addOption(maxUploadSize);
        
        LocalizedOptionDefinition testSessionVar = new LocalizedOptionDefinition(WGACore.SERVEROPTION_SERVER_TESTSESSIONVARSERIALIZABLE, BooleanOptionType.INSTANCE, _bundleLoader);
        testSessionVar.setOptional(true);
        testSessionVar.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(testSessionVar);
        
        LocalizedOptionDefinition sessionTimeout = new LocalizedOptionDefinition(WGACore.SERVEROPTION_SERVER_SESSIONTIMEOUT, IntegerOptionType.INSTANCE, _bundleLoader);
        sessionTimeout.setOptional(true);
        sessionTimeout.setDefaultValue(WGACore.SERVEROPTIONDEFAULT_SESSIONTIMEOUT.toString());
        options.addOption(sessionTimeout);
        
        LocalizedOptionDefinition loginRedirectHost = new LocalizedOptionDefinition(WGACore.SERVEROPTION_LOGINREDIRECTHOST, StringOptionType.INSTANCE, _bundleLoader);
        loginRedirectHost.setOptional(true);
        options.addOption(loginRedirectHost);
        
        LocalizedOptionDefinition loginRedirectPort = new LocalizedOptionDefinition(WGACore.SERVEROPTION_LOGINREDIRECTPORT, IntegerOptionType.INSTANCE, _bundleLoader);
        loginRedirectPort.setOptional(true);
        options.addOption(loginRedirectPort);
        
        LocalizedOptionDefinition loginRedirectProtocol = new LocalizedOptionDefinition(WGACore.SERVEROPTION_LOGINREDIRECTPROTOCOL, StringOptionType.INSTANCE, _bundleLoader);
        loginRedirectProtocol.setOptional(true);
        options.addOption(loginRedirectProtocol);
        
        LocalizedOptionDefinition eventManagerThreadPoolSize = new LocalizedOptionDefinition(WGACore.SERVEROPTION_EVENTMANAGER_THREADPOOLSIZE, IntegerOptionType.INSTANCE, _bundleLoader);
        eventManagerThreadPoolSize.setOptional(true);
        eventManagerThreadPoolSize.setExpert(true);
        eventManagerThreadPoolSize.setDefaultValue(EventManager.DEFAULT_THREADPOOLSIZE.toString());
        options.addOption(eventManagerThreadPoolSize);
        
        LocalizedOptionDefinition webSocketsSessionWorkaround = new LocalizedOptionDefinition(WGACore.SERVEROPTION_WEBSOCKETS_SESSION_WORKAROUND, BooleanOptionType.INSTANCE, _bundleLoader);
        webSocketsSessionWorkaround.setOptional(true);
        webSocketsSessionWorkaround.setExpert(true);
        webSocketsSessionWorkaround.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(webSocketsSessionWorkaround);
                
        
        return options;
        
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("options.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry arg0) {
        _registry = arg0;
    }
    
    

}

