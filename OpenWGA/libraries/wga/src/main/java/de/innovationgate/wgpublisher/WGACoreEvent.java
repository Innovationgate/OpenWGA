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

package de.innovationgate.wgpublisher;

import de.innovationgate.webgate.api.WGDatabase;

public class WGACoreEvent {

    // Event types that represent WGACore states
    public static final int TYPE_PRE_STARTUP = 0;
    
    public static final int TYPE_STARTUP_PRE_CONNECT = 1;
    public static final int TYPE_STARTUP_POST_CONNECT = 2;
    
    public static final int TYPE_ONLINE = 50;
    
    public static final int TYPE_PRE_SHUTDOWN = 51;
    
    public static final int TYPE_SHUTDOWN_PRE_DISCONNECT = 52;
    public static final int TYPE_SHUTDOWN_POST_DISCONNECT = 53;
    
    public static final int TYPE_POST_SHUTDOWN = 100;
    
    // Event types that represent no WGACore states
    public static final int TYPE_CS_CONNECTED = 101;
    public static final int TYPE_CS_DISCONNECTED = 102;


    // The "barrier value" to tell WGACore states from other events
    public static final int WGACORE_STATE_MAXTYPE = 100;
    
    private WGDatabase _database;
    private int _type;
    private WGACore _core;
    
    public WGACoreEvent(int type, WGDatabase db, WGACore core) {
        _type = type;
        _database = db;
        _core = core;
    }

    /**
     * @return Returns the database.
     */
    public WGDatabase getDatabase() {
        return _database;
    }



    /**
     * Returns the event type as constant TYPE_....
     */
    public int getType() {
        return _type;
    }

    public WGACore getCore() {
        return _core;
    }


    
}
