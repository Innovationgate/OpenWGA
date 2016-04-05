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
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class RuntimeDatabaseOptionValueProvider implements OptionValueProvider {

    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/emptylistmessages", getClass().getClassLoader());
    
    private WGAConfiguration _config;
    private int _selection;
    private WGACore _core;
    public RuntimeDatabaseOptionValueProvider(WGAConfiguration configCopy, WGACore core, int selection) {
        _config = configCopy;
        _core = core;
        _selection = selection;
    }

    public List<String> getProvidedValues() {
        
        List<String> dbKeys = new ArrayList<String>();
        for (WGDatabase db : _core.getContentdbs().values()) {
            
            if (db.getAttribute(WGACore.DBATTRIB_PLUGIN_ID) != null) {
                if ((_selection & RuntimeDatabasesOptionType.SELECTION_PLUGIN_DATABASES) != RuntimeDatabasesOptionType.SELECTION_PLUGIN_DATABASES) {
                    continue;
                }
            }
            else if (!db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                if ((_selection & RuntimeDatabasesOptionType.SELECTION_CONTENT_DATABASES) != RuntimeDatabasesOptionType.SELECTION_CONTENT_DATABASES) {
                    continue;
                }
            }
            else if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                if ((_selection & RuntimeDatabasesOptionType.SELECTION_CONTENT_STORES) != RuntimeDatabasesOptionType.SELECTION_CONTENT_STORES) {
                    continue;
                }
            }
            
            dbKeys.add(db.getDbReference());
            
        }
        return dbKeys;
        
    }

    public String getValueTitle(String value, Locale locale) {
        
        WGDatabase db = _core.getContentdbs().get(value);
        PluginID pid = (PluginID) db.getAttribute(WGACore.DBATTRIB_PLUGIN_ID);
        if (pid != null) {
            WGAPlugin plugin = _core.getPluginSet().getPluginByID(pid);
            if (plugin != null) {
                return "Plugin " + db.getTitle() + " (" + (plugin.getInstallationKey() + ")");
            }
            else {
                return "Plugin " + db.getTitle() + "(Installation key not retrievable)";
            }
        }
        else {
            return db.getTitle() + " (" + value + ")";
        }
        
    }

    public String getEmptyListMessage(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("option.databases.emptylist.message");    
    }
            
    
}
