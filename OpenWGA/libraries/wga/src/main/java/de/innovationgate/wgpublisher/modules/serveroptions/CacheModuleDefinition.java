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
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.WGAServerOptionsModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cache.WebTMLCache;

public class CacheModuleDefinition implements ModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/cache", getClass().getClassLoader());

    public String getDescription(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("options.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return WGAServerOptionsModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition cacheWebTML = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_CACHE_WEBTML_SIZE, IntegerOptionType.INSTANCE, _bundleLoader);
        cacheWebTML.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_CACHE_WEBTML_SIZE);
        options.addOption(cacheWebTML);
        
        LocalizedOptionDefinition cacheWebTMLPreloadInterval = new LocalizedOptionDefinition(WebTMLCache.SERVEROPTION_CACHE_PRELOAD_INTERVAL, IntegerOptionType.INSTANCE, _bundleLoader);
        cacheWebTMLPreloadInterval.setOptional(true);
        cacheWebTMLPreloadInterval.setDefaultValue(Integer.toString(WebTMLCache.SERVEROPTIONDEFAULT_CACHE_PRELOAD_INTERVAL));
        options.addOption(cacheWebTMLPreloadInterval);
        
        LocalizedOptionDefinition cacheWebTMLPreloadRefreshInterval = new LocalizedOptionDefinition(WebTMLCache.SERVEROPTION_CACHE_PRELOAD_REFRESHINTERVAL, IntegerOptionType.INSTANCE, _bundleLoader);
        cacheWebTMLPreloadRefreshInterval.setOptional(true);
        cacheWebTMLPreloadRefreshInterval.setDefaultValue(Integer.toString(WebTMLCache.SERVEROPTIONDEFAULT_CACHE_PRELOAD_REFRESHINTERVAL));
        options.addOption(cacheWebTMLPreloadRefreshInterval);
        
        LocalizedOptionDefinition designFilesSize = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_CACHE_DESIGN_SIZE, IntegerOptionType.INSTANCE, _bundleLoader);
        designFilesSize.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_CACHE_DESIGN_SIZE);
        options.addOption(designFilesSize);
        
        LocalizedOptionDefinition designFilesNoBackgroundChanges = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_CACHE_DESIGN_NO_BACKGROUND_CHANGES, BooleanOptionType.INSTANCE, _bundleLoader);
        designFilesNoBackgroundChanges.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_CACHE_DESIGN_NO_BACKGROUND_CHANGES);
        if ("true".equals(System.getProperty("de.innovationgate.license.DevelopmentModeEnabled"))) { // In dev mode this is a stupid setting
            designFilesNoBackgroundChanges.setOptional(true);
        }
        options.addOption(designFilesNoBackgroundChanges);
        
        LocalizedOptionDefinition staticExpiration = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_CACHE_STATIC_RESOURCES_EXPIRATION, IntegerOptionType.INSTANCE, _bundleLoader);
        staticExpiration.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_CACHE_STATIC_RESOURCES_EXPIRATION);
        staticExpiration.setOptional(true);
        options.addOption(staticExpiration);
        
        LocalizedOptionDefinition userCacheLatency = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_CACHE_USERCACHE_LATENCY, IntegerOptionType.INSTANCE, _bundleLoader);
        userCacheLatency.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_CACHE_USERCACHE_LATENCY);
        userCacheLatency.setOptional(true);
        options.addOption(userCacheLatency);
        
        LocalizedOptionDefinition fileServingEnabled = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ENABLED, BooleanOptionType.INSTANCE, _bundleLoader);
        fileServingEnabled.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_EXTERNAL_FILE_SERVING_ENABLED);
        fileServingEnabled.setOptional(true);
        fileServingEnabled.setExpert(true);
        options.addOption(fileServingEnabled);
        
        LocalizedOptionDefinition fileServingDirectory = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_DIRECTORY, StringOptionType.INSTANCE, _bundleLoader);        
        fileServingDirectory.addDependentOption(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ENABLED);
        fileServingDirectory.setExpert(true);
        options.addOption(fileServingDirectory);
        
        LocalizedOptionDefinition fileServingRootURL = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ROOT_URL, StringOptionType.INSTANCE, _bundleLoader);        
        fileServingRootURL.addDependentOption(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ENABLED);    
        fileServingRootURL.setExpert(true);
        options.addOption(fileServingRootURL);
        
        LocalizedOptionDefinition fileServingThreshold = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_THRESHOLD, IntegerOptionType.INSTANCE, _bundleLoader);        
        fileServingThreshold.setOptional(true);
        fileServingThreshold.setDefaultValue("5120");
        fileServingThreshold.addDependentOption(WGAConfiguration.SERVEROPTION_EXTERNAL_FILE_SERVING_ENABLED);
        fileServingThreshold.setExpert(true);
        options.addOption(fileServingThreshold);
        
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

}
