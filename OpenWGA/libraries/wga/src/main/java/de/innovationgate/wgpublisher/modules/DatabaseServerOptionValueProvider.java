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
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.options.OptionValueProvider;

public class DatabaseServerOptionValueProvider implements OptionValueProvider {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/emptylistmessages", getClass().getClassLoader());
    
    private WGAConfiguration _config;
    private String _type;

    public DatabaseServerOptionValueProvider(WGAConfiguration configCopy, String type) {
        _config = configCopy;
        _type = type;
    }

    public List<String> getProvidedValues() {
        List<String> serverUIDs = new ArrayList<String>();
        Iterator<DatabaseServer> servers = _config.getDatabaseServers().iterator();
        while (servers.hasNext()) {
            DatabaseServer server = (DatabaseServer) servers.next();
            if (server.isEnabled() && (_type == null || _type.equals(server.getImplClassName()))) {
                serverUIDs.add(server.getUid());
            }
        }
        return serverUIDs;
    }

    public String getValueTitle(String value, Locale locale) {
        DatabaseServer server = _config.getDatabaseServer(value);
        return server.getTitle();
    }

    public String getEmptyListMessage(Locale arg0) {
        if (_type != null) {
            return _bundleLoader.getBundle(arg0).getString("option.servers.emptylist.message.type");
        }
        else {
            return _bundleLoader.getBundle(arg0).getString("option.servers.emptylist.message.all");
        }
            
    }
}
