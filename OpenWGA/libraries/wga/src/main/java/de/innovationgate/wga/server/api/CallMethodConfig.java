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

/**
 * Detail configuration for calling a method via {@link TMLScript#callMethod(Object, String, java.util.Map, java.util.List, CallMethodConfig)}
 */
public class CallMethodConfig {
    
    private boolean _directAccess = false;

    /**
     * Returns if the method is called in direct access, i.e. the remote user directly calls this method via a public API (f.e. via Websocket "callGlobal" functionality).
     * This does NOT include WebTML action calls, as these are prepared by the WebTML engine and are called by but cannot be influenced by the user. Defaults to false. 
     */
    public boolean isDirectAccess() {
        return _directAccess;
    }

    /**
     * Sets if the method is called in direct access.
     */
    public CallMethodConfig setDirectAccess(boolean directAccess) {
        _directAccess = directAccess;
        return this;
    }

}
