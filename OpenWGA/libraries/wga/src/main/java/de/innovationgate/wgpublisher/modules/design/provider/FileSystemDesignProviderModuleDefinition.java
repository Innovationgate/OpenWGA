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
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.properties.DesignSourceProperties;
import de.innovationgate.wga.modules.types.DesignProviderModuleType;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignSource;
import de.innovationgate.wgpublisher.design.sync.DesignSyncManager;

public class FileSystemDesignProviderModuleDefinition implements ModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/filesystem", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("dp.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return FileSystemDesignProvider.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return DesignProviderModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition noBackgroundChanges = new LocalizedOptionDefinition(FileSystemDesignProvider.OPTION_NO_BACKGROUND_CHANGES, BooleanOptionType.INSTANCE, _bundleLoader);
        noBackgroundChanges.setOptional(true);
        noBackgroundChanges.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(noBackgroundChanges);
        
        LocalizedOptionDefinition sync = new LocalizedOptionDefinition(FileSystemDesignProvider.OPTION_SYNC, BooleanOptionType.INSTANCE, _bundleLoader);
        sync.setOptional(true);
        sync.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(sync);
        
        LocalizedOptionDefinition autoUpdate = new LocalizedOptionDefinition(DesignSyncManager.OPTION_AUTOUPDATE, BooleanOptionType.INSTANCE, _bundleLoader);
        autoUpdate.setOptional(true);
        autoUpdate.setDefaultValue(Boolean.TRUE.toString());
        autoUpdate.addDependentOption(FileSystemDesignProvider.OPTION_SYNC, Boolean.TRUE.toString());
        options.addOption(autoUpdate);
        
        LocalizedOptionDefinition designKey = new LocalizedOptionDefinition(FileSystemDesignManager.OPTION_DESIGNKEY, StringOptionType.INSTANCE, _bundleLoader);
        designKey.setOptional(true);
        designKey.addDependentOption(FileSystemDesignProvider.OPTION_SYNC, Boolean.TRUE.toString());
        options.addOption(designKey);
        
       
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

}
