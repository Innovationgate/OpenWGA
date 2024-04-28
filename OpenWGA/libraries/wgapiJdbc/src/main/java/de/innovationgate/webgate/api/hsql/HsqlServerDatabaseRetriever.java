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

package de.innovationgate.webgate.api.hsql;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionProvider;
import de.innovationgate.webgate.api.servers.DatabaseInformation;
import de.innovationgate.webgate.api.servers.ServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;

public class HsqlServerDatabaseRetriever implements ServerDatabaseRetriever {
    
    private Class _implClass;

    public HsqlServerDatabaseRetriever(Class implClass) {
        _implClass = implClass;
    }
    

    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options) throws WGAPIException {
        
        if (WGDatabaseImpl.class.isAssignableFrom(_implClass)) {
            createContentStore(implClass, dbServer, options);
        }
        else {
            createCustomDB(implClass, dbServer, options);
        }
        
        String path = options.get(Database.OPTION_PATH);
        DatabaseInformation dbInfo = new DatabaseInformation(implClass);
        dbInfo.setLocation(path);
        dbInfo.getOptions().put(Database.OPTION_PATH, path);
        return dbInfo;
        
    }


    private void createCustomDB(Class implClass, WGDatabaseServer dbServer, Map<String, String> options) throws WGAPIException {
        
     // HSQL databases are created by just opening them
        WGDatabase db = openDatabase(implClass, dbServer, options, false);
        db.close();
        
    }


    private void createContentStore(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options) throws WGAPIException {
        
        HsqlDatabaseServer hsqlServer = (HsqlDatabaseServer) dbServer;
        File dir = hsqlServer.getDatabaseDirectory();

        String dbPath = options.get(Database.OPTION_PATH);
        File dbFile = new File(dir, dbPath);
        
        // Look if already exists
        File propFile = new File(dbFile.getAbsolutePath() + ".properties");
        if (propFile.exists()) {
            throw new WGInvalidDatabaseException("Cannot create database because it already exists");
        }
        
        String hsqlPath = WGDatabaseImpl.buildHsqlPath(null, dbFile.getAbsolutePath());
        String path = "jdbc:hsqldb:" + hsqlPath;
        
        WGDatabaseImpl.initializeContentStore(path, "sa", "", null,true);
    }

    public List<DatabaseInformation> getAvailableDatabases(WGDatabaseServer dbServer) throws WGAPIException {
        
        HsqlDatabaseServer hsqlServer = (HsqlDatabaseServer) dbServer;
        File dir = hsqlServer.getDatabaseDirectory();
        List<DatabaseInformation> dbs = new ArrayList<DatabaseInformation>();
        
        File[] dbFiles = dir.listFiles();
        for (int i = 0; i < dbFiles.length; i++) {
            File dbFile = dbFiles[i];
            if (dbFile.getName().endsWith(".properties")) {
                String dbName = dbFile.getName().substring(0, dbFile.getName().length() - 11);
                
                // When searching for content stores we must inspect the tables of available dbs
                if (WGDatabaseImpl.class.isAssignableFrom(_implClass)) {
                    if (!isContentStore(dbName, dbServer)) {
                        continue;
                    }
                }
                

                DatabaseInformation dbInfo = new DatabaseInformation(WGDatabaseImpl.class);
                dbInfo.setLocation(dbName);
                dbInfo.getOptions().put(Database.OPTION_PATH, dbName);
                dbs.add(dbInfo);
            }
        }
        
        return dbs;
        
        
        
        
    }



    private boolean isContentStore(String dbName, WGDatabaseServer dbServer) {
        
        JDBCConnectionProvider connProvider = null;
        try {
            Properties props = new Properties();
            props.put("user", "sa");
            props.put("password", "");
            
            HsqlDatabaseServer hsqlServer = (HsqlDatabaseServer) dbServer;
            File dir = hsqlServer.getDatabaseDirectory();
            File dbFile = new File(dir, dbName);
            String path = "jdbc:hsqldb:file:" + dbFile.getPath();
            
            connProvider = new JDBCConnectionProvider(path, props, false);
            Iterator<String> tables = connProvider.getDatabaseTables().iterator();
            boolean isContentStore = false;
            while (tables.hasNext()) {
                String tableName = (String) tables.next();
                if (tableName.equalsIgnoreCase("WEBAREAS") || tableName.equals("webarea")) {
                    isContentStore = true;
                    break;
                }
                
            }
            
            return isContentStore;
            
        }
        catch (Exception e) {
            WGFactory.getLogger().error("Exception determining database type on hsqldb " + dbName + " on server '" + dbServer.getTitle(Locale.getDefault()) + "'", e);
            return false;
        }
        finally {
            if (connProvider != null) {
                try {
                    connProvider.close();
                }
                catch (JDBCConnectionException e) {
                    WGFactory.getLogger().error("Exception closing connection on hsqldb " + dbName + " on server '" + dbServer.getTitle(Locale.getDefault()) + "'", e);                }
            }
        }
        
    }


    public Class<? extends WGDatabaseServer> getDatabaseServerType() {
        return HsqlDatabaseServer.class;
    }

    public boolean isAvailableDatabasesRetrievable() {
        return true;
    }

    public boolean isCreatable() {
        return true;
    }

    public WGDatabase openDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options, boolean prepareOnly) throws WGAPIException {
        
        HsqlDatabaseServer hsqlServer = (HsqlDatabaseServer) dbServer;
        File dir = hsqlServer.getDatabaseDirectory();

        String dbPath = options.get(Database.OPTION_PATH);
        File dbFile = new File(dir, dbPath);
        
        return WGFactory.getInstance().openDatabase(dbServer, implClass.getName(), dbFile.getAbsolutePath(), "sa", "", options);
        
    }
    
    
    
    public List<String> getDatabasePathOptions() {
        return Collections.singletonList(Database.OPTION_PATH);
    }

}
