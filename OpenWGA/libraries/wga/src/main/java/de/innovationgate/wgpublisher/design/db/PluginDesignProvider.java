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

package de.innovationgate.wgpublisher.design.db;

import java.io.IOException;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;

public class PluginDesignProvider extends DBDesignProvider {

    private PluginID _pluginID;

    public PluginDesignProvider(DesignReference ref, WGACore core, WGDatabase db, String path, PluginID pluginID, Map<String, String> options) throws WGDesignSyncException, IOException, WGAPIException,
            InstantiationException, IllegalAccessException, InvalidCSConfigVersionException {
        super(ref, core, db, path, options);
        _pluginID = pluginID;
    }
    
    public FileSystemDesignProvider getSourceDesignProvider() {
        return (FileSystemDesignProvider) _designDB.getDesignProvider();
    }

    public PluginID getPluginID() {
        return _pluginID;
    }
    
    @Override
    public String getName() {
        return "Plugin design '" + _pluginID.toString() + "'";
    }
    
    @Override
    public String getFileEncoding() {
        return getSourceDesignProvider().getFileEncoding();
    }

}
