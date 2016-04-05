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

package de.innovationgate.wgpublisher.modules.poptions;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.Constants;
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
import de.innovationgate.wga.modules.types.ContentDatabasePublisherOptionsModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.modules.ItemEncodingOptionType;
import de.innovationgate.wgpublisher.modules.MediaKeyOptionType;
import de.innovationgate.wgpublisher.modules.PersonalisationModesOptionType;
import de.innovationgate.wgpublisher.modules.PersonalisationStatisticModesOptionType;

public class BaseContentDatabasePublisherOptionsModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/poptions_database", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return "Publisher options defined by WGA Core";
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentDatabasePublisherOptionsCollector.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition defItemEncoding = new LocalizedOptionDefinition(WGACore.DBATTRIB_DEFAULT_ITEM_ENCODING, new ItemEncodingOptionType(_registry), _bundleLoader);
        defItemEncoding.setOptional(true);
        defItemEncoding.setDefaultValue("none");
        options.addOption(defItemEncoding);
        
        LocalizedOptionDefinition maxQueryResults = new LocalizedOptionDefinition(WGACore.DBATTRIB_MAXQUERYRESULTS, IntegerOptionType.INSTANCE, _bundleLoader);
        maxQueryResults.setOptional(true);
        maxQueryResults.setDefaultValue(String.valueOf(WGACore.DEFAULT_QUERY_MAXRESULTS));
        options.addOption(maxQueryResults);
        
        LocalizedOptionDefinition queryDefault = new LocalizedOptionDefinition(WGACore.DBATTRIB_QUERY_DEFAULT, StringOptionType.INSTANCE, _bundleLoader);
        queryDefault.setOptional(true);
        options.addOption(queryDefault);
        
        LocalizedOptionDefinition persMode = new LocalizedOptionDefinition(WGACore.DBATTRIB_PERSMODE, PersonalisationModesOptionType.INSTANCE, _bundleLoader);
        persMode.setOptional(true);
        persMode.setDefaultValue(String.valueOf(Constants.PERSMODE_AUTO));
        options.addOption(persMode);
        
        LocalizedOptionDefinition persStatMode = new LocalizedOptionDefinition(WGACore.DBATTRIB_PERSSTATMODE, PersonalisationStatisticModesOptionType.INSTANCE, _bundleLoader);
        persStatMode.setOptional(true);
        persStatMode.setDefaultValue(String.valueOf(Constants.PERSSTATMODE_OFF));
        persStatMode.addDependentOption(WGACore.DBATTRIB_PERSMODE);
        options.addOption(persStatMode);
        
        LocalizedOptionDefinition fcThreshold = new LocalizedOptionDefinition(WGACore.DBATTRIB_FILECACHE_THRESHOLD, IntegerOptionType.INSTANCE, _bundleLoader);
        fcThreshold.setOptional(true);
        fcThreshold.setExpert(true);
        fcThreshold.setDefaultValue(String.valueOf(Constants.DEFAULT_FILECACHE_THRESHOLD));
        options.addOption(fcThreshold);
        
        LocalizedOptionDefinition fcEntries = new LocalizedOptionDefinition(WGACore.DBATTRIB_FILECACHE_ENTRIES, IntegerOptionType.INSTANCE, _bundleLoader);
        fcEntries.setOptional(true);
        fcEntries.setExpert(true);
        fcEntries.setDefaultValue(String.valueOf(Constants.DEFAULT_FILECACHE_ENTRIES));
        options.addOption(fcEntries);
        
        LocalizedOptionDefinition pprEntries = new LocalizedOptionDefinition(WGACore.DBATTRIB_PPRCACHE_ENTRIES, IntegerOptionType.INSTANCE, _bundleLoader);
        pprEntries.setOptional(true);
        pprEntries.setExpert(true);
        pprEntries.setDefaultValue(String.valueOf(Constants.DEFAULT_PPRCACHE_ENTRIES));
        options.addOption(pprEntries);

        
        LocalizedOptionDefinition fcExpiration = new LocalizedOptionDefinition(WGACore.DBATTRIB_FILEEXPIRATION_MINUTES, IntegerOptionType.INSTANCE, _bundleLoader);
        fcExpiration.setOptional(true);
        fcExpiration.setExpert(true);
        fcExpiration.setDefaultValue("10");
        options.addOption(fcExpiration);
        
        LocalizedOptionDefinition externalFileServingEnabled = new LocalizedOptionDefinition(WGACore.DBATTRIB_EXTERNAL_FILE_SERVING_ENABLED, BooleanOptionType.INSTANCE, _bundleLoader);
        externalFileServingEnabled.setOptional(true);
        externalFileServingEnabled.setExpert(true);
        externalFileServingEnabled.setDefaultValue("false");
        options.addOption(externalFileServingEnabled);
        
        LocalizedOptionDefinition publishContentFilesWithDesignEncoding = new LocalizedOptionDefinition(WGACore.DBATTRIB_PUBLISH_CONTENTFILES_WITH_DESIGNENCODING, BooleanOptionType.INSTANCE, _bundleLoader);
        publishContentFilesWithDesignEncoding.setOptional(true);
        publishContentFilesWithDesignEncoding.setExpert(true);
        publishContentFilesWithDesignEncoding.setDefaultValue(String.valueOf(WGACore.DBATTRIBDEFAULT_PUBLISH_CONTENTFILES_WITH_DESIGNENCODING));
        options.addOption(publishContentFilesWithDesignEncoding);
        
        return options;
        
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return "Base publisher options";
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

}

