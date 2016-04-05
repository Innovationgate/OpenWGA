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

package de.innovationgate.webgate.api.postgresql;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.JDBCDatabaseServerExtension;

import de.innovationgate.webgate.api.servers.DatabaseFilter;
import de.innovationgate.webgate.api.servers.DatabaseInformation;
import de.innovationgate.webgate.api.servers.ServerConnectionException;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class PostgresqlDatabaseServer extends WGDatabaseServer implements JDBCDatabaseServerExtension {

    public static final String OPTION_PORT = "Port";
    public static final String OPTION_GLOBAL_DATABASE = "GlobalDatabase";
    
    public static final int DEFAULT_PORT = 5432;
    public static final String DEFAULT_GLOBAL_DATABASE = "template1";
    
    @Override
    public void testConnection() throws ServerConnectionException {
        
        try {
        	Connection con = createJDBCConnection();
        	con.close();
        }
        catch (Exception e) {
            throw new ServerConnectionException("Connection test failed", e);
        }
        
    }

	public String buildJDBCPath(Map<String, String> options) {
		String hostName = buildHostName();        
        String path = options.get(Database.OPTION_PATH);
        String jdbcPath;
        if (!path.contains("/")) {
            jdbcPath = "jdbc:postgresql://" + hostName + "/" + path;
        }
        else {
            jdbcPath = path;
        }
        return jdbcPath;
	}
	
    protected String buildHostName() {
        String serverName = getOptions().get(DatabaseServer.OPTION_PATH);
        String serverPort = getOptions().get(PostgresqlDatabaseServer.OPTION_PORT);
        String server = serverName +  (serverPort != null ? ":" + serverPort : "");
        return server;
    }

    public Connection createJDBCConnection(DatabaseInformation db, Properties props) throws WGBackendException {
    	try {
			ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, PostgresqlDatabaseServer.class);
			OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
			
			String hostName = buildHostName();
			
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
			
			String jdbcPath = "jdbc:postgresql://" + hostName;
			if (db != null) {
				jdbcPath += "/" + db.getOptions().get(Database.OPTION_PATH);
			}
			else {
			    jdbcPath += "/" + getGlobalDatabase();
			}
			
			Driver driver = (Driver) Class.forName(WGDatabaseImpl.DRIVER).newInstance();
			
			if (props == null) {
				props = new Properties();
			}
			if (!props.containsKey("user")) {
				props.put("user", masterUser);
			}
			if (masterPassword != null && !props.containsKey("password")) {
			    props.put("password", masterPassword);
			}
			else {
			    props.put("password", "");
			}
			
			return driver.connect(jdbcPath, props);
		} catch (Exception e) {
			throw new WGBackendException("Unable to create connection.", e);
		}
    }

    public List<DatabaseInformation> getAvailableJDBCDatabases(DatabaseFilter filter) throws WGAPIException {
        
        try {
            List<DatabaseInformation> dbs = new ArrayList<DatabaseInformation>();
            
            // First connect globally to gather database names
            Connection con = createJDBCConnection();
            Statement showDbsStmt = null;
            List<String> dbNames = new ArrayList<String>();
            try {
               showDbsStmt = con.createStatement();
               showDbsStmt.execute("SELECT datname FROM pg_database");
               ResultSet dbRS = showDbsStmt.getResultSet();
               
               while (dbRS.next()) {
                   String dbName = dbRS.getString(1);
                   dbNames.add(dbName);
               }
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
            
            // Then connect to each database to filter (if filter given)
            for (String dbName : dbNames) {
                
                DatabaseInformation info = new DatabaseInformation(null);
                info.getOptions().put(Database.OPTION_PATH, dbName);
                info.setLocation(dbName);
                
                boolean accept = false;
                
                if (filter != null) {
                    try {
                        con = createJDBCConnection(info);
                        accept = filter.accept(con);
                    }
                    catch (Throwable e) {
                    }
                    finally {
                        try {
                            if (con != null) {
                                con.close();
                            }
                        }
                        catch (Exception e) {
                        }
                    }
                }
                
                if (accept) {
                    dbs.add(info);
                }
            }

        
            return dbs;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception fetching available databases", e);
        }
        
    }
    
    public Connection createJDBCConnection(DatabaseInformation db) throws WGBackendException {
    	return createJDBCConnection(db, null);
    }
    
    public Connection createJDBCConnection() throws WGBackendException {
    	return createJDBCConnection(null);
    }

    public DatabaseInformation createJDBCDatabase(Map<String, String> options, InputStream ddl) throws WGBackendException {
    	
        String path = options.get(Database.OPTION_PATH);
        DatabaseInformation info = new DatabaseInformation(null);
        info.getOptions().put(Database.OPTION_PATH, path);
        info.setLocation(path);
    	
        List<DatabaseInformation> dbs = null;
		try {
			dbs = getAvailableJDBCDatabases((DatabaseFilter)null);
		} catch (WGAPIException e) {
			throw new WGBackendException("Cannot create database bc. of exception when testing for existence", e);
		}
		
		if (dbs != null) {
		    Iterator<DatabaseInformation> it = dbs.iterator();
			while (it.hasNext()) {
				if (it.next().getOptions().get(Database.OPTION_PATH).equalsIgnoreCase(path)) {
					throw new WGBackendException("Cannot create database bc. it already exists"); 
				}
			}
		} 
		
		Properties props = new Properties();
        Connection con = createJDBCConnection(info, props);
        
        String ddlScript = null;
        try {
        	if (ddl != null) {
                Reader read = new InputStreamReader(ddl);
                StringWriter write = new StringWriter();
                WGUtils.inToOut(read, write, 2048);
                read.close();
                ddlScript = write.toString();
            }
        }
        catch (IOException e) {
            throw new WGBackendException("Cannot create database bc. of exception when reading initialisation script: " + e.getClass().getName() + " - " + e.getMessage());
        }
        
        try {
        	if (ddlScript != null) {
                // Execute script
        		con.setAutoCommit(false);
        		Iterator statements = WGUtils.deserializeCollection(ddlScript, ";", false, new Character('\'')).iterator();
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
        	return info;
        }
        catch (SQLException e) {
            throw new WGBackendException("Cannot create database bc. of exception when executing initialisation script: " + e.getClass().getName() + " - " + e.getMessage());
        }
        finally {
            try {
                if (con != null) {
                    con.close();
                }
            }
            catch (SQLException e) {
                throw new WGBackendException("Exception when closing connection: " + e.getClass().getName() + " - " + e.getMessage());
            }
        }
    }

    @Override
    public boolean isConnectionTestable() {
        return true;
    }

    public String getGlobalDatabase() {
        
        String globalDatabase = (String) getOptions().get(OPTION_GLOBAL_DATABASE);
        if (globalDatabase == null) {
            globalDatabase = DEFAULT_GLOBAL_DATABASE;
            
        }
        
        return globalDatabase;
    }
}
