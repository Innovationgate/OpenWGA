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

package de.innovationgate.webgate.api.mysql;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGConfigurationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;
import de.innovationgate.webgate.api.jdbc.custom.JDBCSource.TableName;
import de.innovationgate.webgate.api.servers.DatabaseInformation;
import de.innovationgate.webgate.api.servers.ServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class MySqlServerDatabaseRetriever implements ServerDatabaseRetriever {
    
    private boolean _csOnly;

    public MySqlServerDatabaseRetriever(boolean csOnly) {
        _csOnly = csOnly;
    }

    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options) throws WGAPIException {
        
        if (!_csOnly) {
            throw new WGNotSupportedException("Creating custom databases is not supported");
        }
        
        try {
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MySqlDatabaseServer.class);
            OptionReader serverOptionReader = OptionReader.create(dbServer.getOptions(), serverDef);
            
            String hostName = buildHostName(dbServer);
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String path = options.get(Database.OPTION_PATH);
            String jdbcPath;
            if (!path.contains("/")) {
                jdbcPath = MySqlDatabaseServer.JDBC_BASE_PATH + hostName + "/" + path;
            }
            else {
                jdbcPath = path;
            }
            
            // Retrieve driver
            Driver driver;
            try {
                driver = DriverManager.getDriver(MySqlDatabaseServer.JDBC_BASE_PATH);
            }
            catch (Exception e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when retrieving driver: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }

            // Build props, including the "createIfNotExist"-flag
            Properties props = new Properties();
            props.setProperty("user", masterUser);
            if (masterPassword != null) {
                props.setProperty("password", masterPassword);
            }
            
            // First connect to verify the database does not yet exist
            Connection con;
            try {
                //con = driver.connect(jdbcPath, props);
                con = DriverManager.getConnection(jdbcPath, props);
                con.close();
                throw new WGInvalidDatabaseException("Cannot create database bc. it already exists");
            }
            catch (SQLException e) {
                if (e.getErrorCode() != 1049) {
                    throw new WGInvalidDatabaseException("Cannot create database bc. of exception when testing for existence: " + e.getMessage(), e);
                }
            }
            
            // Connect to create
            props.setProperty("createDatabaseIfNotExist", "true");
            try {
                con = driver.connect(jdbcPath, props);
            }
            catch (SQLException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception on creation: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }
            
            // If script is null retrieve default script
            String script;
            try {
                Reader read = new InputStreamReader(getClass().getClassLoader().getResourceAsStream("de/innovationgate/webgate/api/mysql/wgacs5_mysql.ddl"));
                script = WGUtils.readString(read);
                read.close();
            }
            catch (IOException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when reading initialisation script: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }
            

            try {
                con.setAutoCommit(false);
                
                // Execute script
                Iterator statements = WGUtils.deserializeCollection(script, ";", false, new Character('\'')).iterator();
                Statement st = con.createStatement();
                while (statements.hasNext()) {
                   String code = ((String) statements.next()).trim();
                   if (!code.equals("")) {
                       st.addBatch(code);
                   }
                }
                st.executeBatch();
                con.commit();
                
            }
            catch (SQLException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when executing initialisation script: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }
            finally {
                try {
                    if (con != null) {
                        con.close();
                    }
                }
                catch (SQLException e) {
                    throw new WGInvalidDatabaseException("Exception when closing connection: " + e.getClass().getName() + " - " + e.getMessage(), e);
                }
            }
            
            DatabaseInformation info = new DatabaseInformation(implClass);
            info.getOptions().put(Database.OPTION_PATH, path);
            info.setLocation(path);
            return info;

            
        }
        catch (Exception e) {
            throw new WGConfigurationException("Exception creating database", e);
        }

    }

    public List<DatabaseInformation> getAvailableDatabases(WGDatabaseServer dbServer) throws WGAPIException {
        
        try {
            List<DatabaseInformation> dbs = new ArrayList<DatabaseInformation>();
            
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MySqlDatabaseServer.class);
            OptionReader serverOptionReader = OptionReader.create(dbServer.getOptions(), serverDef);
            
            String hostName = buildHostName(dbServer);
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String jdbcPath = MySqlDatabaseServer.JDBC_BASE_PATH + hostName;
            
            Properties props = new Properties();
            
            props.put("user", masterUser);
            if (masterPassword != null) {
                props.put("password", masterPassword);
            }
            else {
                props.put("password", "");
            }
            
            //Connection con = driver.connect(jdbcPath, props);
            Connection con = DriverManager.getConnection(jdbcPath, props);
            Statement showDbsStmt = null;
            try {
               showDbsStmt = con.createStatement();
               showDbsStmt.execute("show databases");
               ResultSet dbRS = showDbsStmt.getResultSet();
               while (dbRS.next()) {
                   String dbName = dbRS.getString(1);
                   
                   Statement stmt = con.createStatement();
                   stmt.execute("use `" + dbName + "`");
                   stmt.close();
                   
                   boolean isDesiredType = (_csOnly ? isContentStore(con) : true);
                   if (isDesiredType == true) {
                       DatabaseInformation info = new DatabaseInformation(WGDatabaseImpl.class);
                       info.getOptions().put(Database.OPTION_PATH, dbName);
                       info.setLocation(dbName);
                       dbs.add(info);
                   }
               }
               return dbs;
            }
            finally {
                try {
                    if (showDbsStmt != null) {
                        showDbsStmt.close();
                    }
                }
                catch (Exception e) {
                }
                try {
                    if (con != null) {
                        con.close();
                    }
                }
                catch (Exception e) {
                }
            }
        }
        catch (Exception e) {
            throw new WGBackendException("Exception fetching available databases", e);
        }
        
    }

    private boolean isContentStore(Connection con) throws SQLException {
           Statement stmt = con.createStatement();
           stmt.execute("show tables");
           ResultSet tables = stmt.getResultSet();
           boolean isContentStore = false;
           while (tables.next()) {
               String tableName = tables.getString(1);
               if (tableName.equalsIgnoreCase("WEBAREAS") || tableName.equals("webarea")) {
                   isContentStore = true;
                   break;
               }
           }
          stmt.close();
        return isContentStore;
    }

    public Class<? extends WGDatabaseServer> getDatabaseServerType() {
        return MySqlDatabaseServer.class;
    }

    public boolean isAvailableDatabasesRetrievable() {
        return true;
    }

    public boolean isCreatable() {
        if (_csOnly) {
            return true;
        }
        else {
            return false;
        }
    }

    public WGDatabase openDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options, boolean prepareOnly) throws WGAPIException {

        try {
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MySqlDatabaseServer.class);
            OptionReader serverOptionReader = OptionReader.create(dbServer.getOptions(), serverDef);
            
            String hostName = buildHostName(dbServer);
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String path = options.get(Database.OPTION_PATH);
            String jdbcPath;
            if (!path.contains("/")) {
                jdbcPath = MySqlDatabaseServer.JDBC_BASE_PATH + hostName + "/" + path;
            }
            else {
                jdbcPath = path;
            }
            return WGFactory.getInstance().openDatabase(dbServer, implClass.getName(), jdbcPath, masterUser, masterPassword, options, prepareOnly);
            
        }
        catch (Exception e) {
            throw new WGConfigurationException("Exception opening database on server " + dbServer.getTitle(Locale.getDefault()), e);
        }

        
    }
    
    protected String buildHostName(WGDatabaseServer dbServer) {
        String serverName = dbServer.getOptions().get(DatabaseServer.OPTION_PATH);
        String serverPort = dbServer.getOptions().get(MySqlDatabaseServer.OPTION_PORT);
        String server = serverName +  (serverPort != null ? ":" + serverPort : "");
        return server;
    }
    
    public List<String> getDatabasePathOptions() {
        return Collections.singletonList(Database.OPTION_PATH);
    }


}
