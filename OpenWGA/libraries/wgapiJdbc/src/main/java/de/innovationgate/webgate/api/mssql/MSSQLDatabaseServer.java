/*
 * Created on 21.04.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.mssql;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;

import org.hibernate.cfg.Environment;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGConfigurationException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.JDBCDatabaseServerExtension;
import de.innovationgate.webgate.api.jdbc.SharedPoolJDBCDatabaseServer;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl.CSVersion;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
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

public class MSSQLDatabaseServer extends WGDatabaseServer implements JDBCDatabaseServerExtension, SharedPoolJDBCDatabaseServer {

    public static final String OPTION_PORT = "Port";
    public static final int DEFAULT_PORT = 1433;
    public static final String JDBC_URL_PREFIX = "jdbc:jtds:sqlserver://";
    private Boolean _usePool;
    private JDBCCatalogSwitchingConnectionPool _pool = null;
    
    public static final int DEFAULT_SHAREDPOOL_MAX_CONNECTIONS = 100;
    public static final int DEFAULT_SHAREDPOOL_MAX_IDLE = 80;
    public static final int DEFAULT_SHAREDPOOL_MAX_WAIT = 30;
    public static final int DEFAULT_SHAREDPOOL_MIN_IDLE = 10;
    
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

	public String buildJDBCPath(Map<String, String> options) throws WGConfigurationException {
		try {
            String hostName = buildHostName();        
            String path = options.get(Database.OPTION_PATH);
            String jdbcPath;
            if (!path.contains("/")) {
                jdbcPath = JDBC_URL_PREFIX + hostName + "/" + path;
            }
            else {
                jdbcPath = path;
            }
            return jdbcPath;
        }
        catch (OptionConversionException e) {
            throw new WGConfigurationException("Unable to read neccessary options", e);
        }
	}

    public Connection createJDBCConnection(DatabaseInformation db, Properties props) throws WGBackendException {
    	try {
			ModuleDefinition serverDef = getModuleDefinition();
			OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
			
			String hostName = buildHostName();
			
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
			
			String jdbcPath = JDBC_URL_PREFIX + hostName;
			if (db != null) {
				jdbcPath += "/" + db.getOptions().get(Database.OPTION_PATH);
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
            
            ModuleDefinition serverDef = getModuleDefinition();
            OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
            
            String hostName = buildHostName();
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String jdbcPath = JDBC_URL_PREFIX + hostName;
            
            Driver driver = (Driver) Class.forName(WGDatabaseImpl.DRIVER).newInstance();
            
            Properties props = new Properties();
            
            props.put("user", masterUser);
            if (masterPassword != null) {
                props.put("password", masterPassword);
            }
            else {
                props.put("password", "");
            }
            
            Connection con = driver.connect(jdbcPath, props);
            Statement showDbsStmt = null;
            try {
               showDbsStmt = con.createStatement();
               showDbsStmt.execute("exec sp_databases");
               ResultSet dbRS = showDbsStmt.getResultSet();
               while (dbRS.next()) {
                   String dbName = dbRS.getString(1);
                   
                   Statement stmt = con.createStatement();
                   stmt.execute("use " + dbName);
                   stmt.close();
                   
                   if (filter.accept(con)) {
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

    private ModuleDefinition getModuleDefinition() {
        return WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MSSQLDatabaseServer.class);
    }
    
    public Connection createJDBCConnection(DatabaseInformation db) throws WGBackendException {
    	return createJDBCConnection(db, null);
    }
    
    public Connection createJDBCConnection() throws WGBackendException {
    	return createJDBCConnection(null);
    }

    public DatabaseInformation createJDBCDatabase(Map<String, String> options, InputStream ddl) throws WGAPIException {
    	throw new WGNotSupportedException("Not supported");
    }
    
    protected String buildHostName() throws OptionConversionException {
        OptionReader oReader = OptionReader.create(getOptions(), getModuleDefinition());
        String serverName = (String) oReader.readOptionValueOrDefault(DatabaseServer.OPTION_PATH);
        Integer serverPort = (Integer) oReader.readOptionValueOrDefault(MSSQLDatabaseServer.OPTION_PORT);
        String server = serverName +  ":" + serverPort;
        return server;
    }

    @Override
    public boolean isConnectionTestable() {
        return true;
    }

    @Override
    public boolean isPoolAvailable(CSVersion csVersion) {
        return _usePool;
    }

    @Override
    public ConnectionProvider createPoolConnectionProvider(String catalogName) throws WGAPIException, OptionConversionException {

        if (!_usePool) {
            throw new WGNotSupportedException("Shared server pool is not supported");
        }
        
        if (_pool == null) {
            
            // Build configuration
            String path = JDBC_URL_PREFIX + buildHostName();
    
            // Configure and build pool
            Properties poolProps = new Properties();
            WGDatabaseImpl.putDefaultServerConPoolProps(this, poolProps);
            
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MSSQLDatabaseServer.class);
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
            poolProps.put("hibernate.dbcp.dbserver.title", getTitle(Locale.getDefault()));
            poolProps.put("hibernate.dbcp.dbserver.id", getUid());
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
            return _pool.createTenantConnectionProvider(catalogName);
        }
        catch (JDBCConnectionException e) {
            throw new WGInvalidDatabaseException("Exception creating connection provider from shared connection pool", e);
        }
        
    }
    
    @Override
    public void init(String uid, String title, Map<String, String> options) throws WGAPIException {
        super.init(uid, title, options);
        
        
        ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MSSQLDatabaseServer.class);
        OptionReader serverOptionReader = OptionReader.create(getOptions(), serverDef);
        try {
        _usePool = (Boolean) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_SHAREDPOOL);
        }
        catch (OptionConversionException e) {
            throw new WGBackendException("Exception reading server configuration", e);
        } 
    }
    
    @Override
    public void importState(WGDatabaseServer oldServer) {
        if (oldServer instanceof MSSQLDatabaseServer) {
            MSSQLDatabaseServer oldMssql = (MSSQLDatabaseServer) oldServer;
            if (oldMssql._pool != null) {
                synchronized (oldMssql._pool) {
                    if (oldMssql._pool.getActiveTenantProviders().size() > 0) {
                        _pool = oldMssql._pool;
                        oldMssql._pool = null;
                    }
                    else {
                        WGFactory.getLogger().info("Dropping old shared connection pool of server " + getTitle(Locale.getDefault()));
                        try {
                            oldMssql._pool.close();
                            oldMssql._pool = null;
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
    protected void finalize() throws Throwable {
        if (_pool != null) {
            _pool.close();
            _pool = null;
        }
    }
}
