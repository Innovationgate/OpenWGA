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

package de.innovationgate.webgate.api.jdbc;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGConfigurationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.mysql.MySqlDatabaseServer;
import de.innovationgate.webgate.api.servers.DatabaseInformation;
import de.innovationgate.webgate.api.servers.ServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class JNDIServerDatabaseRetriever implements ServerDatabaseRetriever {

    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options) throws WGAPIException {
        throw new WGNotSupportedException("Creating databases is not supported");
    }

    public List<DatabaseInformation> getAvailableDatabases(WGDatabaseServer dbServer) throws WGAPIException {
        throw new WGNotSupportedException("Retrieving databases is not supported");
    }

    public Class<? extends WGDatabaseServer> getDatabaseServerType() {
        return JNDIDatabaseServer.class;
    }

    public boolean isAvailableDatabasesRetrievable() {
        return false;
    }

    public boolean isCreatable() {
        return false;
    }

    public WGDatabase openDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options, boolean prepareOnly) throws WGAPIException {
        try {
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, getDatabaseServerType());
            OptionReader serverOptionReader = OptionReader.create(dbServer.getOptions(), serverDef);
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String jndiPath = options.get(Database.OPTION_JNDI_PATH);
            
            return WGFactory.getInstance().openDatabase(dbServer, implClass.getName(), jndiPath, masterUser, masterPassword, options, prepareOnly);
            
        }
        catch (Exception e) {
            throw new WGConfigurationException("Exception opening database", e);
        }
    }
    
    public List<String> getDatabasePathOptions() {
        return Collections.singletonList(Database.OPTION_JNDI_PATH);
    }

}
