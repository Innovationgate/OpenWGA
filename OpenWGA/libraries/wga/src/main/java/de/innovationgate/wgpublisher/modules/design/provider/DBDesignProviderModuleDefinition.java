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

package de.innovationgate.wgpublisher.modules.design.provider;

import java.util.Locale;

import org.hsqldb.lib.FileAccess.FileSync;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.WGAXML;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.AvailabilityTestableModuleDefinition;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleAvailabilityException;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.properties.DesignSourceProperties;
import de.innovationgate.wga.modules.types.DesignProviderModuleType;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.db.DBDesignProvider;
import de.innovationgate.wgpublisher.design.db.DBDesignSource;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignSource;
import de.innovationgate.wgpublisher.design.sync.DesignSyncManager;

public class DBDesignProviderModuleDefinition implements ModuleDefinition, AvailabilityTestableModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/db", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("dp.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return DBDesignProvider.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return DesignProviderModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        PredefinedValuesOptionType crossloginType = new PredefinedValuesOptionType(_bundleLoader, DBDesignProvider.OPTION_CROSSLOGINMODE);
        crossloginType.addValue(WGAXML.DESIGNSHARING_MODE_ANONYMOUS);
        crossloginType.addValue(WGAXML.DESIGNSHARING_MODE_PARALLEL);
        
        LocalizedOptionDefinition crosslogin = new LocalizedOptionDefinition(DBDesignProvider.OPTION_CROSSLOGINMODE, crossloginType, _bundleLoader);
        crosslogin.setOptional(true);
        crosslogin.setDefaultValue(WGAXML.DESIGNSHARING_MODE_ANONYMOUS);
        options.addOption(crosslogin);
        
        LocalizedOptionDefinition variants = new LocalizedOptionDefinition(WGADesignProvider.OPTION_DESIGNVARIANTS, BooleanOptionType.INSTANCE, _bundleLoader);
        variants.setOptional(true);
        variants.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(variants);
        
        return options;
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("dp.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }
    
    public void testAvailability(ModuleRegistry reg) throws ModuleAvailabilityException {
        
        WGACore core = (WGACore) reg.getContextObjects().get(WGACore.class);
        if (core == null) {
            throw new ModuleAvailabilityException("Cannot retrieve WGACore to test availability");
        }
        
        boolean csFound = false;
        for (WGDatabase db : core.getContentdbs().values()) {
            if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && !db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX) && db.getDesignProvider() == null) {
                csFound = true;
                break;
            }
        }
        if (!csFound) {
            throw new ModuleAvailabilityException("No WGA Content Store is connected that could be used as design provider");
        }
        
    }

}
