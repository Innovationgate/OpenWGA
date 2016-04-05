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

import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;
import de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType;

/**
 * A ServerDatabaseRetriever defines the functionality to open or create a database for a special {@link WGDatabaseServer} implementation
 * Each WGA Module of Type {@link ContentDatabaseModuleType} or {@link PersonalisationDatabaseModuleType} offers a retriever per database server that it is valid on.
 *
 */
public interface ServerDatabaseRetriever {
    
    public WGDatabase openDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String,String> options, boolean prepareOnly) throws WGAPIException;
    
    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String,String> options) throws WGAPIException;
    
    public boolean isCreatable();
    
    public Class<? extends WGDatabaseServer> getDatabaseServerType();
    
    public boolean isAvailableDatabasesRetrievable();
    
    public List<DatabaseInformation> getAvailableDatabases(WGDatabaseServer dbServer) throws WGAPIException;
    
    public List<String> getDatabasePathOptions();

}
