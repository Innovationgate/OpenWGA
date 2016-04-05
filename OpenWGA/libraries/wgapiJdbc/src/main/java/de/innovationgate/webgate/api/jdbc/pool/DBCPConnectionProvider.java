/*
 * Copyright 2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package de.innovationgate.webgate.api.jdbc.pool;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import javax.management.ObjectName;

import org.apache.commons.dbcp2.BasicDataSource;
import org.apache.commons.dbcp2.BasicDataSourceFactory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.HibernateException;
import org.hibernate.cfg.Environment;
import org.hibernate.service.UnknownUnwrapTypeException;
import org.hibernate.service.jdbc.connections.internal.ConnectionProviderInitiator;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.spi.Configurable;
import org.hibernate.service.spi.Stoppable;





import de.innovationgate.monitoring.JmxManager;
import de.innovationgate.webgate.api.WGFactory;

/**
 * <p>A connection provider that uses an Apache commons DBCP connection pool.</p>
 * 
 * <p>To use this connection provider set:<br>
 * <code>hibernate.connection.provider_class&nbsp;org.hibernate.connection.DBCPConnectionProvider</code></p>
 *
 * <pre>Supported Hibernate properties:
 *   hibernate.connection.driver_class
 *   hibernate.connection.url
 *   hibernate.connection.username
 *   hibernate.connection.password
 *   hibernate.connection.isolation
 *   hibernate.connection.autocommit
 *   hibernate.connection.pool_size
 *   hibernate.connection (JDBC driver properties)</pre>
 * <br>
 * All DBCP properties are also supported by using the hibernate.dbcp prefix.
 * A complete list can be found on the DBCP configuration page:
 * <a href="http://jakarta.apache.org/commons/dbcp/configuration.html">http://jakarta.apache.org/commons/dbcp/configuration.html</a>.
 * <br>
 * <pre>Example:
 *   hibernate.connection.provider_class org.hibernate.connection.DBCPConnectionProvider
 *   hibernate.connection.driver_class org.hsqldb.jdbcDriver
 *   hibernate.connection.username sa
 *   hibernate.connection.password
 *   hibernate.connection.url jdbc:hsqldb:test
 *   hibernate.connection.pool_size 20
 *   hibernate.dbcp.initialSize 10
 *   hibernate.dbcp.maxWait 3000
 *   hibernate.dbcp.validationQuery select 1 from dual</pre>
 * 
 * <p>More information about configuring/using DBCP can be found on the 
 * <a href="http://jakarta.apache.org/commons/dbcp/">DBCP website</a>.
 * There you will also find the DBCP wiki, mailing lists, issue tracking 
 * and other support facilities</p>  
 * 
 * @see org.hibernate.connection.ConnectionProvider
 * @author Dirk Verbeeck
 */
public class DBCPConnectionProvider implements ConnectionProvider, Configurable, Stoppable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static final Log log = LogFactory.getLog("wga.dbcppool");
    private static final String PREFIX = "hibernate.dbcp.";
    protected BasicDataSource _ds;
    private JmxManager _jmxManager;
    private boolean _server = false;
    private String _entityKey;
    private String _entityTitle;

    // Old Environment property for backward-compatibility (property removed in Hibernate3)
    private static final String DBCP_PS_MAXACTIVE = "hibernate.dbcp.ps.maxActive";
    
    // Old DBCP1 property changed to maxTotal in DBCP2
    private static final String DBCP_MAXACTIVE = "hibernate.dbcp.maxActive";
    
    // Old DBCP1 property changed to maxWaitMillis in DBCP2
    private static final String DBCP_MAXWAIT = "hibernate.dbcp.maxWait";

    // Property doesn't exists in Hibernate2
    private static final String AUTOCOMMIT = "hibernate.connection.autocommit";
    public static final String JMX_BASE_ADDRESS = "de.innovationgate.WGAMonitor:name=DBCP-Connection-Pooling";
    public static final String JMX_DBPOOLS_ADDRESS = JMX_BASE_ADDRESS + ",poolType=Apps";
    public static final String JMX_SERVERPOOLS_ADDRESS = JMX_BASE_ADDRESS + ",poolType=Servers";
    
    public static final String JMX_DBCP2_BASE_ADDRESS = "de.innovationgate.WGAMonitor:name=DBCP2-Connection-Pooling";
    public static final String JMX_DBCP2_DBPOOLS_ADDRESS = JMX_DBCP2_BASE_ADDRESS + ",poolType=Apps";
    public static final String JMX_DBCP2_SERVERPOOLS_ADDRESS = JMX_DBCP2_BASE_ADDRESS + ",poolType=Servers";

    
    public void configure(Map propsMap) throws HibernateException {
        try {
            log.debug("Configure DBCPConnectionProvider");
            Properties props = new Properties();
            props.putAll(propsMap);
            
            String jdbcUrl = (String) props.getProperty(Environment.URL);
            
            // DBCP properties used to create the BasicDataSource
            Properties dbcpProperties = new Properties();

            // DriverClass & url
            String jdbcDriverClass = props.getProperty(Environment.DRIVER);
            
            // Try to determine driver by jdbc-URL
            if (jdbcDriverClass == null) {
                Driver driver = DriverManager.getDriver(jdbcUrl);
                if (driver != null) {
                    jdbcDriverClass = driver.getClass().getName();
                }
                else {
                    throw new HibernateException("Driver class not available");
                }
            }
            
            dbcpProperties.put("driverClassName", jdbcDriverClass);
            dbcpProperties.put("url", jdbcUrl);
            
            // Username / password
            String username = props.getProperty(Environment.USER);
            if (username != null) {
                dbcpProperties.put("username", username);
            }
            
            String password = props.getProperty(Environment.PASS);
            if (password != null) {
                dbcpProperties.put("password", password);
            }

            // Isolation level
            String isolationLevel = props.getProperty(Environment.ISOLATION);
            if ((isolationLevel != null) && (isolationLevel.trim().length() > 0)) {
                dbcpProperties.put("defaultTransactionIsolation", isolationLevel);
            }
            
            // Turn off autocommit (unless autocommit property is set) 
            String autocommit = props.getProperty(AUTOCOMMIT);
            if ((autocommit != null) && (autocommit.trim().length() > 0)) {
                dbcpProperties.put("defaultAutoCommit", autocommit);
            } else {
                dbcpProperties.put("defaultAutoCommit", String.valueOf(Boolean.FALSE));
            }

            // Pool size
            String poolSize = props.getProperty(Environment.POOL_SIZE);
            if ((poolSize != null) && (poolSize.trim().length() > 0) 
                && (Integer.parseInt(poolSize) > 0))  {
                dbcpProperties.put("maxActive", poolSize);
            }

            // Copy all "driver" properties into "connectionProperties"
            Properties driverProps = ConnectionProviderInitiator.getConnectionProperties(props);
            if (driverProps.size() > 0) {
                StringBuffer connectionProperties = new StringBuffer();
                for (Iterator iter = driverProps.keySet().iterator(); iter.hasNext();) {
                    String key = (String) iter.next();
                    String value = driverProps.getProperty(key);
                    connectionProperties.append(key).append('=').append(value);
                    if (iter.hasNext()) {
                        connectionProperties.append(';');
                    }
                }
                dbcpProperties.put("connectionProperties", connectionProperties.toString());
            }

            // Copy all DBCP properties removing the prefix
            for (Iterator iter = props.keySet().iterator() ; iter.hasNext() ;) {
                String key = String.valueOf(iter.next());
                if (key.startsWith(PREFIX)) {
                    String property = key.substring(PREFIX.length());
                    String value = props.getProperty(key);
                    dbcpProperties.put(property, value);
                }
            }
            
            // Backward-compatibility
            if (props.getProperty(DBCP_PS_MAXACTIVE) != null) {
                dbcpProperties.put("poolPreparedStatements", String.valueOf(Boolean.TRUE));
                dbcpProperties.put("maxOpenPreparedStatements", props.getProperty(DBCP_PS_MAXACTIVE));
            }
            if (props.getProperty(DBCP_MAXACTIVE) != null) {
                dbcpProperties.put("maxTotal", props.getProperty(DBCP_MAXACTIVE));
            }
            if (props.getProperty(DBCP_MAXWAIT) != null) {
                dbcpProperties.put("maxWaitMillis", props.getProperty(DBCP_MAXWAIT));
            }
            
            // Some debug info
            if (log.isDebugEnabled()) {
                log.debug("Creating a DBCP BasicDataSource with the following DBCP factory properties:");
                StringWriter sw = new StringWriter();
                dbcpProperties.list(new PrintWriter(sw, true));
                log.debug(sw.toString());
            }
            
            String dbKey = (String) props.get("hibernate.dbcp.dbkey");
            String databaseServerId = (String) props.get("hibernate.dbcp.dbserver.id");
            
            // Enable DBCP2 JMX monitoring information
            if (dbKey != null) {
                dbcpProperties.put("jmxName", JMX_DBCP2_DBPOOLS_ADDRESS + ",pool=" + JmxManager.normalizeJmxKey(dbKey));
            }
            else if (databaseServerId != null) {
                String entityTitle = props.getProperty("hibernate.dbcp.dbserver.title");
                dbcpProperties.put("jmxName", JMX_DBCP2_SERVERPOOLS_ADDRESS + ",pool=" + JmxManager.normalizeJmxKey(entityTitle));
            }

            // Let the factory create the pool
            _ds = BasicDataSourceFactory.createDataSource(dbcpProperties);
            
            // The BasicDataSource has lazy initialization
            // borrowing a connection will start the DataSource
            // and make sure it is configured correctly.
            Connection conn = _ds.getConnection();
            conn.close();
            
            // Create Legacy JMX monitoring information, provided by WGA
            if ("true".equals(props.getProperty("hibernate.dbcp.legacyJMX"))) {
                try {
                    if (dbKey != null) {
                        _entityKey = dbKey;
                        _entityTitle = dbKey;
                        _jmxManager = new JmxManager(new DBCPPoolInformation(this), new ObjectName(JMX_DBPOOLS_ADDRESS + ",pool=" + JmxManager.normalizeJmxKey(dbKey)));
                    }
                    else  if (databaseServerId != null) {
                        _server = true;
                        _entityKey = databaseServerId;
                        _entityTitle = (String) props.get("hibernate.dbcp.dbserver.title");
                        _jmxManager = new JmxManager(new DBCPPoolInformation(this), new ObjectName(JMX_SERVERPOOLS_ADDRESS + ",pool=" + JmxManager.normalizeJmxKey(_entityTitle)));
                    }
                }
                catch (Throwable e) {
                    log.error("Error enabling JMX metrics for connection pool", e);
                }
            }

        }
        catch (Exception e) {
            String message = "Could not create a DBCP pool";
            if (_ds != null) {
                try {
                    _ds.close();
                }
                catch (Exception e2) {
                    // ignore
                }
                _ds = null;
            }
            throw new HibernateException(message, e);
        }
        log.debug("Configure DBCPConnectionProvider complete");
        
        
        
        
    }

    public Connection getConnection() throws SQLException {
        Connection conn = null;
        conn = _ds.getConnection();
        return conn;
    }

    public void closeConnection(Connection conn) throws SQLException {
        conn.close();
    }

    @Override
    public void stop() {
        log.debug("Close DBCPConnectionProvider");
        try {
            if (_ds != null) {
                _ds.close();
                    _ds = null;
            }
            else {
                log.warn("Cannot close DBCP pool (not initialized)");
            }
            
            if (_jmxManager != null) {
                _jmxManager.unregister();
            }
            
        }
        catch (Exception e) {
            WGFactory.getLogger().error("Exception closing DBCPConnectionProvider", e);
        }
        log.debug("Close DBCPConnectionProvider complete");
    }
    
    public boolean supportsAggressiveRelease() {
        return false;
    }

    protected BasicDataSource getDs() {
        return _ds;
    }

    public boolean isUnwrappableAs(Class unwrapType) {
        return BasicDataSource.class.isAssignableFrom(unwrapType);
    }

    public <T> T unwrap(Class<T> unwrapType) {
        if (isUnwrappableAs(unwrapType)) {
            return (T) _ds;
        }
        else {
            throw new UnknownUnwrapTypeException(unwrapType);
        }
    }

    public boolean isServer() {
        return _server;
    }

    public String getEntityKey() {
        return _entityKey;
    }

    public String getEntityTitle() {
        return _entityTitle;
    }


}
