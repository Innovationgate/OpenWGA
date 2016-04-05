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

package de.innovationgate.wga.server.api;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGACore;

/**
 * The "Plugin" object provides information about an installed OpenWGA plugin
 * OpenWGA plugins extend the OpenWGA server in numerous ways, including new application designs, TMLScript functionalities, authentication sources and many more. This is an information object about the data and state of an installed and active OpenWGA plugin.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Plugin extends App {

    private PluginID _id;
    

    protected Plugin(WGA wga, WGDatabase db) throws WGException {
        super(wga, db);
        _id = (PluginID) _db.getAttribute(WGACore.DBATTRIB_PLUGIN_ID);
        if (_id == null) {
            throw new IllegalArgumentException("Database '" + _db.getDbReference() + "' is no content store of a WGAPlugin");
        }
    }
    
    /**
     * Returns the unique name of the plugin
     * This is an unique identifier for the plugin which normally resembles Java package syntax
     */
    public String getName() throws WGException {
        return _id.getUniqueName();
    }
    
    /**
     * Returns the version of the plugin
     */
    public Version getVersion() throws WGException {
        return _id.getVersion();
    }
    
    /**
     * Returns the complete plugin ID
     */
    @CodeCompletion
    public PluginID getId() throws WGException {
        return _id;
    }

}