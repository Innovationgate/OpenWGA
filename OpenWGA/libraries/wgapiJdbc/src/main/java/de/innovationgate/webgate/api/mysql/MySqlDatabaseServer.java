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

import java.io.File;
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
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import org.hibernate.HibernateException;
import org.hibernate.MappingException;
import org.hibernate.MultiTenancyStrategy;
import org.hibernate.SessionBuilder;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.context.spi.CurrentTenantIdentifierResolver;
import org.hibernate.dialect.MySQL5Dialect;
import org.hibernate.service.Service;
import org.hibernate.service.ServiceRegistryBuilder;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.jdbc.connections.spi.MultiTenantConnectionProvider;
import org.hibernate.service.spi.ServiceRegistryImplementor;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.JDBCDatabaseServerExtension;
import de.innovationgate.webgate.api.jdbc.SharedPoolJDBCDatabaseServer;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl.CSVersion;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLCSModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.pool.JDBCCatalogSwitchingConnectionPool;
import de.innovationgate.webgate.api.servers.DatabaseFilter;
import de.innovationgate.webgate.api.servers.DatabaseInformation;
import de.innovationgate.webgate.api.servers.ServerConnectionException;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class MySqlDatabaseServer extends WGDatabaseServer implements JDBCDatabaseServerExtension, SharedPoolJDBCDatabaseServer {

    public static final String OPTION_PORT = "Port";
    public static final int DEFAULT_PORT = 3306;
    
    public static final int DEFAULT_SHAREDPOOL_MAX_CONNECTIONS = 95;
    public static final int DEFAULT_SHAREDPOOL_MAX_IDLE = 80;
    public static final int DEFAULT_SHAREDPOOL_MIN_IDLE = 10;
    public static final int DEFAULT_SHAREDPOOL_MAX_WAIT = 30;
    public static final int DEFAULT_SHAREDPOOL_MAX_CONNECTION_LIFETIME = 1000 * 60 * 10;
    
    private JDBCCatalogSwitchingConnectionPool _pool = null;
    private Boolean _usePool;
    
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
            jdbcPath = "jdbc:mysql://" + hostName + "/" + path;
        }
        else {
            jdbcPath = path;
        }
        return jdbcPath;
	}
	
    protected String buildHostName() {
        String serverName = getOptions().get(DatabaseServer.OPTION_PATH);
        String serverPort = getOptions().get(MySqlDatabaseServer.OPTION_PORT);
        String server = serverName +  (serverPort != null ? ":" + serverPort : "");
        return server;
    }

    public Connection createJDBCConnection(DatabaseInformation db, Properties props) throws WGBackendException {
    	try {
			ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MySqlDatabaseServer.class);
			OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
			
			String hostName = buildHostName();
			
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
			
			String jdbcPath = "jdbc:mysql://" + hostName;
			if (db != null) {
				jdbcPath += "/" + db.getOptions().get(Database.OPTION_PATH);
			}
			
			Driver driver = (Driver) Class.forName(WGDatabaseImpl.DRIVER).newInstance();
			
			if (props == null) {
				props = new Properties();
			}
			
			Map<String,String> so = serverOptionReader.getOptions();
			for(Map.Entry<String, String> entry: so.entrySet()){
				if(entry.getKey().startsWith("jdbc.")){
					props.put(entry.getKey().substring("jdbc.".length()), entry.getValue());
				}
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
            
            Connection con = createJDBCConnection();
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
                   
                   boolean accept = true;
                   if (filter != null) {
                	   accept = filter.accept(con);
                   }                                     
                   if (accept) {
                       DatabaseInformation info = new DatabaseInformation(null);
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
        props.setProperty("createDatabaseIfNotExist", "true");
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

    @Override
    public boolean isPoolAvailable(CSVersion csVersion) {
        return _usePool;
    }



    public synchronized ConnectionProvider createPoolConnectionProvider(String dbName) throws OptionConversionException, WGAPIException {
        
        if (!_usePool) {
            throw new WGNotSupportedException("Shared server pool is not supported");
        }
        
        if (_pool == null) {
            
            // Build configuration
            String path = "jdbc:mysql://" + buildHostName();
    
            // Configure and build pool
            Properties poolProps = new Properties();
            WGDatabaseImpl.putDefaultServerConPoolProps(this, poolProps);
            
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MySqlDatabaseServer.class);
            OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            if (masterUser != null) {
                poolProps.put("hibernate.connection.username", masterUser);
                if (masterPassword != null) {
                    poolProps.put("hibernate.connection.password", masterPassword);
                }
            }
            
            poolProps.put("hibernate.dbcp.maxActive", String.valueOf(serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL_MAX_CONNECTIONS)));
            poolProps.put("hibernate.dbcp.maxIdle", String.valueOf(serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL_MAX_IDLE_CONNECTIONS)));
            poolProps.put("hibernate.dbcp.minIdle", String.valueOf(serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL_MIN_IDLE_CONNECTIONS)));
            poolProps.put("hibernate.dbcp.maxConnLifetimeMillis", String.valueOf(serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL_MAX_CONNECTION_LIFETIME)));

            poolProps.put("hibernate.dbcp.validationQuery", "select 1");
            poolProps.put("hibernate.dbcp.validationQueryTimeout", "5");
            poolProps.put("hibernate.dbcp.testOnBorrow", "true");
            poolProps.put("hibernate.dbcp.testWhileIdle", "true");
            Integer maxWait = (Integer) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL_MAX_WAIT);
            poolProps.put("hibernate.dbcp.maxWait", String.valueOf(maxWait * 1000));
            poolProps.put("hibernate.connection.connectTimeout", WGDatabaseImpl.CONNECT_TIMEOUT_DEFAULT);
            poolProps.put("hibernate.connection.socketTimeout", WGDatabaseImpl.SOCKET_TIMEOUT_DEFAULT);
            poolProps.put("hibernate.dbcp.dbserver.id", getUid());
            poolProps.put("hibernate.dbcp.dbserver.title", getTitle(Locale.getDefault()));
            poolProps.put(Environment.ISOLATION, String.valueOf(Connection.TRANSACTION_READ_COMMITTED));
            poolProps.putAll(getOptions());
            try {
                WGFactory.getLogger().info("Creating shared connection pool for server " + getTitle(Locale.getDefault()));
                _pool = new JDBCCatalogSwitchingConnectionPool(path, WGDatabaseImpl.DRIVER, poolProps);
                
            }
            catch (JDBCConnectionException e) {
                throw new WGInvalidDatabaseException("Exception creating shared connection pool", e);
            }
            
        }
        
        try {
            return _pool.createTenantConnectionProvider(dbName);
        }
        catch (JDBCConnectionException e) {
            throw new WGInvalidDatabaseException("Exception creating connection provider from shared connection pool", e);
        }
        
    }
    

    
    @Override
    public void importState(WGDatabaseServer oldServer) {
        if (oldServer instanceof MySqlDatabaseServer) {
            MySqlDatabaseServer oldMySql = (MySqlDatabaseServer) oldServer;
            if (oldMySql._pool != null) {
                synchronized (oldMySql._pool) {
                    if (oldMySql._pool.getActiveTenantProviders().size() > 0) {
                        _pool = oldMySql._pool;
                        oldMySql._pool = null;
                    }
                    else {
                        WGFactory.getLogger().info("Dropping old shared connection pool of server " + getTitle(Locale.getDefault()));
                        try {
                            oldMySql._pool.close();
                            oldMySql._pool = null;
                        }
                        catch (JDBCConnectionException e) {
                            WGFactory.getLogger().error("Exception dropping old shared connection pool", e);
                        }
                    }
                }
            }
        }
    }

    @Override
    public void init(String uid, String title, Map<String, String> options) throws WGAPIException {
        super.init(uid, title, options);
        
        ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MySqlDatabaseServer.class);
        OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
       try {
        _usePool = (Boolean) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL);
        }
        catch (OptionConversionException e) {
            throw new WGBackendException("Exception reading server configuration", e);
        } 
    
    }
    
    @Override
    protected void finalize() throws Throwable {
        if (_pool != null) {
            _pool.close();
            _pool = null;
        }
    }
}
