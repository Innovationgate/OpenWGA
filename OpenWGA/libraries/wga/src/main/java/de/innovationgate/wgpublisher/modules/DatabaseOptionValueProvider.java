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

package de.innovationgate.wgpublisher.modules;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType.DatabaseType;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class DatabaseOptionValueProvider implements OptionValueProvider {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/emptylistmessages", getClass().getClassLoader());
    
    private WGAConfiguration _config;
    private List<DatabasesOptionType.DatabaseType> _databaseTypes;

    private WGACore _core;

    public DatabaseOptionValueProvider(WGACore core, WGAConfiguration configCopy, List<DatabasesOptionType.DatabaseType> databaseTypes) {
        _core = core;
        _config = configCopy;
        _databaseTypes = databaseTypes;
    }

    public List<String> getProvidedValues() {
        List<String> dbKeys = new ArrayList<String>();
        
        // Apps and data sources
        if (_databaseTypes.contains(DatabaseType.APP) || _databaseTypes.contains(DatabaseType.DATASOURCE)) {
            Iterator<ContentDatabase> dbs = _config.getContentDatabases().iterator();
            while (dbs.hasNext()) {
                ContentDatabase contentDatabase = (ContentDatabase) dbs.next();
                
                if (!contentDatabase.isEnabled()) {
                    continue;
                }
                
                if (contentDatabase instanceof ContentStore) {
                    if (_databaseTypes.contains(DatabaseType.APP)) {
                        dbKeys.add(contentDatabase.getKey());
                    }
                }
                else {
                    if (_databaseTypes.contains(DatabaseType.DATASOURCE)) {
                        dbKeys.add(contentDatabase.getKey());
                    }
                }
            }
        }
        
        // Plugins
        if (_databaseTypes.contains(DatabaseType.PLUGIN)) {
            for (WGAPlugin plugin : _core.getPluginSet().getActivePluginsByUniqueName().values()) {
                dbKeys.add(plugin.buildDatabaseKey());
            }
        }
        
        
        return dbKeys;
    }

    public String getValueTitle(String value, Locale locale) {
        
        if (value.startsWith("plugin-")) {
            WGAPlugin plugin = _core.getPluginSet().getPluginsByInstallationKey().get(value.substring(7));
            if (plugin != null) {
                return value + " (Plugin " + plugin.getCsConfig().getPluginConfig().getTitle() + ")";
            }
        }
        else {
            ContentDatabase db = _config.getContentDatabase(value);
            if (db != null) {
                return db.getKey() + (db.getTitle() != null ? " (" + db.getTitle() + ")" : "");
            }
        }
        
        return value + " (unknown)";
    }

    public String getEmptyListMessage(Locale arg0) {
        if (!_databaseTypes.contains(DatabaseType.DATASOURCE)) {
            return _bundleLoader.getBundle(arg0).getString("option.databases.emptylist.message.cs");
        }
        else {
            return _bundleLoader.getBundle(arg0).getString("option.databases.emptylist.message.alldbs");
        }
            
    }
}
