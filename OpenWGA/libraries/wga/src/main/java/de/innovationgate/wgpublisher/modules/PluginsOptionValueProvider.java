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
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.options.OptionValueProvider;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class PluginsOptionValueProvider implements OptionValueProvider {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/emptylistmessages", getClass().getClassLoader());

    private WGACore _core;
    private int _usage;

    public PluginsOptionValueProvider(WGACore core, int usage) {
        _core = core;
        _usage = usage;
    }

    public List<String> getProvidedValues() {
        
        List<String> pluginDBs = new ArrayList<String>();
        Iterator<WGAPlugin> plugins = _core.getPluginSet().getPlugins().iterator();
        while (plugins.hasNext()) {
            WGAPlugin plugin = (WGAPlugin) plugins.next();
            if (plugin.isActive()) {
                switch (_usage) {
                    
                    case PluginsOptionType.USAGE_ANY:
                        pluginDBs.add(plugin.buildDatabaseKey());
                        break;
                        
                    case PluginsOptionType.USAGE_AUTHSOURCE:
                        if (plugin.getCsConfig().getPluginConfig().isUsageAsAuthSource()) {
                            pluginDBs.add(plugin.buildDatabaseKey());
                        }
                        break;
                    
                    case PluginsOptionType.USAGE_CONTENTSTORE:
                        if (plugin.getCsConfig().getPluginConfig().isUsageAsContentStore()) {
                            pluginDBs.add(plugin.buildDatabaseKey());
                        }
                        break;
                        
                    case PluginsOptionType.USAGE_DESIGNSOURCE:
                        if (plugin.getCsConfig().getPluginConfig().isUsageAsDesignProvider()) {
                            pluginDBs.add(plugin.buildDatabaseKey());
                        }
                        break;
                }
            }
        }
        
        return pluginDBs;
        
    }

    public String getValueTitle(String value, Locale locale) {
        
        WGDatabase pluginDB = _core.getContentdbs().get(value);
        CSConfig csConfig = (CSConfig) pluginDB.getAttribute(WGACore.DBATTRIB_CSCONFIG);
        if (csConfig != null && csConfig.getPluginConfig() != null) {
            return csConfig.getPluginConfig().getTitle() + " (" + csConfig.getPluginConfig().getId().getUniqueName() + ")";
        }
        
        return value;
        
    }

    public String getEmptyListMessage(Locale arg0) {
        switch (_usage) {
            
            case PluginsOptionType.USAGE_ANY:
                return _bundleLoader.getBundle(arg0).getString("option.plugins.emptylist.message.any");
                
            case PluginsOptionType.USAGE_AUTHSOURCE:
                return _bundleLoader.getBundle(arg0).getString("option.plugins.emptylist.message.authsource");
                
            case PluginsOptionType.USAGE_CONTENTSTORE:
                return _bundleLoader.getBundle(arg0).getString("option.plugins.emptylist.message.cs");
                
            case PluginsOptionType.USAGE_DESIGNSOURCE:
                return _bundleLoader.getBundle(arg0).getString("option.plugins.emptylist.message.designsource");
        }
        
        return null;
    }

}
