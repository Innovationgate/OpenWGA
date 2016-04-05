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
package de.innovationgate.webgate.api.servers;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.OptionDefinitionsMap;

/**
 * A generic database retriever that merely passes on the open/create information
 *
 */
public class GenericServerDatabaseRetriever implements ServerDatabaseRetriever {
    
    private Class<? extends WGDatabaseServer> _databaseServerType;

    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String,String> options) throws WGAPIException {
        throw new WGNotSupportedException("Creating databases not supported for this type/server combination");
    }



    public Class<? extends WGDatabaseServer> getDatabaseServerType() {
        return _databaseServerType;
    }

    public boolean isCreatable() {
        return false;
    }

    public WGDatabase openDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String,String> options, boolean prepareOnly) throws WGAPIException {
        String path = options.get(Database.OPTION_PATH);
        return WGFactory.getInstance().openDatabase(dbServer, implClass.getName(), path, null, null, options, prepareOnly);       
    }

    public List<DatabaseInformation> getAvailableDatabases(WGDatabaseServer dbServer) {
        return null;
    }

    public boolean isAvailableDatabasesRetrievable() {
        return false;
    }

    public void setDatabaseServerType(Class<? extends WGDatabaseServer> databaseServerType) {
        _databaseServerType = databaseServerType;
    }
    
    public List<String> getDatabasePathOptions() {
        return Collections.singletonList(Database.OPTION_PATH);
    }

}

