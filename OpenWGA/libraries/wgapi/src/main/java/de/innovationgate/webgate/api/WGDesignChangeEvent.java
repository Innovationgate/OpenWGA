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

import java.util.List;

/**
 * An event fired when a design provider or a database has changed it's design
 */
public class WGDesignChangeEvent {
    
    private WGDesignProvider _designProvider;
    private List _updateLogs;
    private WGDatabase _database;
    
    /**
     * Public constructor, taking the design provider and a list of update logs
     * @param designCoreProvider The design provider
     * @param database The database which uses this provider as design source
     * @param updateLogs List of WGUpdateLog-Objects
     */
    public WGDesignChangeEvent(WGDesignProvider designCoreProvider, WGDatabase database, List updateLogs) {
        _designProvider = designCoreProvider;
        _database = database;
        _updateLogs = updateLogs;
    }

    /**
     * Returns the design provider that throwed the event. Might be null if the event originated from a database.
     */
    public WGDesignProvider getDesignProvider() {
        return _designProvider;
    }

    /**
     * Returns update logs for the changed design documents
     */
    public List getUpdateLogs() {
        return _updateLogs;
    }

    /**
     * Returns the database that receives its design from the design provider.
     * Might have no open database session if the event originated from a design provider.
     */
    public WGDatabase getDatabase() {
        return _database;
    }

}
