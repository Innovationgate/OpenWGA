/*
 * Created on 22.04.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.mssql;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGConfigurationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionProvider;
import de.innovationgate.webgate.api.servers.DatabaseFilter;
import de.innovationgate.webgate.api.servers.DatabaseInformation;
import de.innovationgate.webgate.api.servers.ServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class MSSQLServerDatabaseRetriever implements ServerDatabaseRetriever {
    
    private boolean _csOnly;

    public MSSQLServerDatabaseRetriever(boolean csOnly) {
        _csOnly = csOnly;
    }

    public DatabaseInformation createDatabase(Class<? extends WGDatabaseCore> implClass, WGDatabaseServer dbServer, Map<String, String> options) throws WGAPIException {
        
        if (!_csOnly) {
            throw new WGNotSupportedException("Creating custom databases is not supported");
        }
        
        try {
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MSSQLDatabaseServer.class);
            OptionReader serverOptionReader = OptionReader.create(dbServer.getOptions(), serverDef);
            
            MSSQLDatabaseServer msServer = (MSSQLDatabaseServer) dbServer;
            String globalJdbcPath = MSSQLDatabaseServer.JDBC_BASE_PATH + msServer.buildHostName();
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String dbName = options.get(Database.OPTION_PATH);
            if (dbName == null || !Pattern.matches("^[a-zA-Z][a-zA-Z0-9_]*$", dbName)) {
                throw new WGInvalidDatabaseException("The database name is invalid: " + dbName + ". It must consist of alphanumeric characters and the underscore, but start with a letter.");
            }
            
            // Retrieve driver
            Driver driver;
            try {
                driver = DriverManager.getDriver(MSSQLDatabaseServer.JDBC_BASE_PATH);
            }
            catch (Exception e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when retrieving driver: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }

            // Build props
            Properties props = new Properties();
            props.setProperty("encrypt", "false");
            props.setProperty("user", masterUser);
            if (masterPassword != null) {
                props.setProperty("password", masterPassword);
            }
            
            // First connect to verify the database does not yet exist
            Connection con = null;
            try {
                String jdbcPath = msServer.buildJDBCPath(options);
                con = driver.connect(jdbcPath, props);
                con.close();
                throw new WGInvalidDatabaseException("Cannot create database bc. it already exists");
            }
            catch (SQLException e) {
                if (e.getErrorCode() != 4060) {
                    throw new WGInvalidDatabaseException("Cannot create database bc. of exception when testing for existence: " + e.getMessage(), e);
                }
            }
            
            // Connect to create the database
            try {
                con = driver.connect(globalJdbcPath, props);
                String script = "create database " + dbName + ";";
                con.createStatement().execute(script);
            }
            catch (SQLException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception on creation: " + e.getClass().getName() + " - " + e.getMessage(), e);
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
            
            // Connect to initialize the database
            try {
                String jdbcPath = msServer.buildJDBCPath(options);
                con = driver.connect(jdbcPath, props);
            }
            catch (SQLException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when opening for initialisation", e);
            }
            
            // Load script
            String script;
            try {
                Reader read = new InputStreamReader(getClass().getClassLoader().getResourceAsStream("de/innovationgate/webgate/api/mssql/wgacs5_mssql.ddl"));
                script = WGUtils.readString(read);
                read.close();
            }
            catch (IOException e) {
                throw new WGInvalidDatabaseException("Cannot create database bc. of exception when reading initialisation script: " + e.getClass().getName() + " - " + e.getMessage(), e);
            }
            
            // Execute script
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
            info.getOptions().put(Database.OPTION_PATH, dbName);
            info.setLocation(dbName);
            return info;

            
        }
        catch (Exception e) {
            throw new WGConfigurationException("Exception creating database", e);
        }
    }

    public List<DatabaseInformation> getAvailableDatabases(WGDatabaseServer dbServer) throws WGAPIException {
        
        MSSQLDatabaseServer msServer = (MSSQLDatabaseServer) dbServer;
        return msServer.getAvailableJDBCDatabases(new DatabaseFilter() {
            public boolean accept(Object filterContext) throws WGAPIException {
                try {
                    Connection con = (Connection) filterContext;
                    if (_csOnly) {
                        return isContentStore(con);
                    }
                    else {
                        return true;
                    }
                }
                catch (SQLException e) {
                    throw new WGBackendException("Exception testing database nature", e);
                }
            }
        });
        
    }

    private boolean isContentStore(Connection con) throws SQLException {
        try {
           Iterator<String> tables = JDBCConnectionProvider.getDatabaseTables(con).iterator();
           boolean isContentStore = false;
           while (tables.hasNext()) {
               String tableName = tables.next();
               if (tableName.equalsIgnoreCase("WEBAREAS") || tableName.equals("webarea")) {
                   isContentStore = true;
                   break;
               }
           }
           return isContentStore;
        }
        catch(SQLException e) {
        	return false;
        }
    }
    
    public Class<? extends WGDatabaseServer> getDatabaseServerType() {
        return MSSQLDatabaseServer.class;
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
            MSSQLDatabaseServer msServer = (MSSQLDatabaseServer) dbServer;
            ModuleDefinition serverDef = WGFactory.getModuleRegistry().getModuleDefinition(DatabaseServerModuleType.class, MSSQLDatabaseServer.class);
            OptionReader serverOptionReader = OptionReader.create(dbServer.getOptions(), serverDef);
            
            String masterUser = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_USER);
            String masterPassword = (String) serverOptionReader.readOptionValueOrDefault(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD);
            
            String path = options.get(Database.OPTION_PATH);
            String jdbcPath;
            if (!path.contains("/")) {
                jdbcPath = msServer.buildJDBCPath(options);
            }
            else {
                jdbcPath = path;
            }
            
            options.put("encrypt", "false");
            
            return WGFactory.getInstance().openDatabase(dbServer, implClass.getName(), jdbcPath, masterUser, masterPassword, options, prepareOnly);
            
        }
        catch (Exception e) {
            throw new WGConfigurationException("Exception opening database on server " + dbServer.getTitle(Locale.getDefault()), e);
        }

        
    }
    
    public List<String> getDatabasePathOptions() {
        return Collections.singletonList(Database.OPTION_PATH);
    }

}
