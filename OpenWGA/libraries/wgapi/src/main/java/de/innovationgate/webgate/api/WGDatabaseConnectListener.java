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

package de.innovationgate.webgate.api;

import java.util.EventListener;

/**
 * A listener that will get notified, when the database is connected to its backend.
 * This event only occurs, when the database was not opened normally but only prepared for
 * opening by using WGFactory.prepareDatabase. In this mode the database will only test its
 * connectivity to the backend but disconnect from it directly after that. 
 * 
 * The connection is reestablished with the first session that is opened. At this time 
 * the event is fired. The database has an open session when this event is fired.
 * 
 * It is guaranteed, that the listeners will be called in the order that they were registered
 * with the database.
 * 
 *
 */
public interface WGDatabaseConnectListener extends EventListener {

    /**
     * The database got connected to its backend
     * @param event
     */
    public void databaseConnected(WGDatabaseEvent event);
    
    /**
     * The backend connection has failed
     * @param event
     */
    public void databaseConnectionError(WGDatabaseEvent event);
    
}
