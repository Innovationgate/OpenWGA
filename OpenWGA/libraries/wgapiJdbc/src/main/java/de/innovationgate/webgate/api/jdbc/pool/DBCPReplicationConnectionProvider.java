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
package de.innovationgate.webgate.api.jdbc.pool;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;

import org.apache.commons.dbcp2.BasicDataSource;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.hibernate.HibernateException;
import org.hibernate.cfg.Environment;
import org.hibernate.service.UnknownUnwrapTypeException;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.spi.Configurable;


/**
 * This class represents a connection provider which can be used with database backends supporting replication like
 * MySQL (Master/ Slave) or MS-SQL (PeerToPeer Cluster)
 * 
 * The provider can be enabled by the following DBOption:
 * hibernate.connection.provider_class = de.innovationgate.webgate.api.jdbc.pool.DBCPReplicationConnectionProvider
 * 
 * JDBC-Urls are given as normal, but you can provide multiple urls splited by "|".
 * 
 * When using this provider all database sessions are readonly by default. To perform an update within WGA you have to call
 * db().beginUpdate() first.
 * 
 * The connection provider ensures that all updates are done on the first given JDBC-Url (Master). All other (readonly) requests
 * are loadbalanced between the master and all given slave urls.
 * 
 * The provider can handle database server failures automatically. If the master is unreachable all connections will be readonly,
 * until the master comes back. If a slave connection fails all work will be done on the master, until a slave is reachable again.
 * 
 * 
 */
public class DBCPReplicationConnectionProvider implements ConnectionProvider, Configurable {

	
    private static final long serialVersionUID = 1L;
    private BasicDataSource _masterDS;
	private List _slaveDSList = new ArrayList();
	private String[] _jdbcUrls;
		
    private static final Log log = LogFactory.getLog("wga.dbcppool");
    
    // delay after an stale connection is retried
	private static final long RETRY_DELAY = 1000 * 10;
    
    private Date _masterFailure = null;
    
    // last slave failures (Date)) mapped by slaveDS
    private Map _slaveFailures = new HashMap();

    public void configure(Map propsMap) throws HibernateException {
        try {
            log.info("Configure DBCPReplicationConnectionProvider");
            Properties props = new Properties();
            props.putAll(propsMap);

            
            String jdbcUrl = props.getProperty(Environment.URL);
            
            // split jdbcURL by "|" and configure masterDS for first and slaveDataSources for all other URLs
            _jdbcUrls = jdbcUrl.split("\\|");
            
            // spec. configuration for dbcp
            if (!props.containsKey("hibernate.connection.connectTimeout")) {
            	props.setProperty("hibernate.dbcp.poolPreparedStatements", "true");
            }
            if (!props.containsKey("hibernate.connection.connectTimeout")) {
            	props.setProperty("hibernate.connection.connectTimeout", "5000");
            }
            if (!props.containsKey("hibernate.connection.socketTimeout")) {
            	props.setProperty("hibernate.connection.socketTimeout", "20000");
            }
            if (!props.containsKey("hibernate.dbcp.testOnBorrow")) {
            	props.setProperty("hibernate.dbcp.testOnBorrow", "true");
            }
            if (!props.containsKey("hibernate.dbcp.validationQuery")) {
            	props.setProperty("hibernate.dbcp.validationQuery", "select 1");
            }
            
            // create master DS
        	log.info("configuring master datasource on url: " + _jdbcUrls[0]);
            Properties masterProps = (Properties) props.clone();
            masterProps.setProperty(Environment.URL, _jdbcUrls[0]);
            DBCPConnectionProvider masterProvider = new DBCPConnectionProvider();
            masterProvider.configure(masterProps);
            _masterDS = masterProvider.getDs();
            
            // create slave datasources
            for (int i = 1; i < _jdbcUrls.length; i++) {
            	log.info("configuring slave datasource on url: " + _jdbcUrls[i]);
            
            	Properties slaveProps = (Properties) props.clone();
                slaveProps.setProperty(Environment.URL, _jdbcUrls[i]);
                DBCPConnectionProvider slaveProvider = new DBCPConnectionProvider();
                slaveProvider.configure(slaveProps);
                _slaveDSList.add(slaveProvider.getDs());
            }
            log.info("Configure DBCPReplicationConnectionProvider complete");
        } catch (Exception e) {
        	log.fatal("Could not create DBCPReplicationConnectionProvider.", e);
        	throw new HibernateException("Could not create DBCPReplicationConnectionProvider.", e);
        }
    }

    public Connection getConnection() throws SQLException {        
    	
    	SQLException connectionException = null;
    	
    	Connection master = null;
    	
    	if (_masterFailure == null || (System.currentTimeMillis() - RETRY_DELAY) >= _masterFailure.getTime() ) {
	    	try {
	    		// master 
	    		master = _masterDS.getConnection();
	    	} catch (SQLException e) {
	    		log.warn("Unable to establish a connection to master database on '" + _jdbcUrls[0] + "'.");
	    		connectionException = e;
	    		_masterFailure = new Date();
	    	}
    	}
    	
        // if slave urls given - randomize one
        Connection slave = null;
        if (_slaveDSList.size() > 0) {
        	int slaveIdx = new Random().nextInt(_slaveDSList.size());
        	BasicDataSource slaveDS = (BasicDataSource) _slaveDSList.get(slaveIdx);
        	
        	Date failureDate = (Date) _slaveFailures.get(slaveDS);
        	
        	if (failureDate == null || (System.currentTimeMillis() - RETRY_DELAY) >= failureDate.getTime() ) {
	        	try {
	            	slave = slaveDS.getConnection();
	            } catch (SQLException e) {
	            	log.warn("Unable to establish a connection to database on '" + _jdbcUrls[slaveIdx + 1] + "'.");
	            	connectionException = e;
	            	// set slave failure timestamp
	            	_slaveFailures.put(slaveDS, new Date());
	            }
        	}
        }
        
        if (master == null && slave == null) {
        	throw connectionException;
        }
        
        return new DBCPReplicationConnection(master, slave);
    }

    public void closeConnection(Connection conn) throws SQLException {
        conn.close();
    }

    public void close() throws HibernateException {
        log.info("Close DBCPReplicationConnectionProvider");
        try {
        	try {
        		if (_masterDS != null) {
        			_masterDS.close();
        		}
        	} catch (SQLException e) {
        		log.error("Cannot close masterDS", e);
        	}

            Iterator _slaves = _slaveDSList.iterator();
            while (_slaves.hasNext()) {
            	try {
            		BasicDataSource slaveDS = (BasicDataSource) _slaves.next();
            		if (slaveDS != null) {
            			slaveDS.close();
            		}
            	} catch (SQLException e) {
            		log.error("Cannot close slaveDS", e);
            	}            	
            }
            _slaveDSList.clear();
            
        } catch (Exception e) {
            throw new HibernateException("Could not close DBCPReplicationConnectionProvider", e);
        }
        
        log.info("Close DBCPReplicationConnectionProvider complete");
    }
    
    public boolean supportsAggressiveRelease() {
        return false;
    }

    public boolean isUnwrappableAs(Class unwrapType) {
        return BasicDataSource.class.isAssignableFrom(unwrapType);
    }

    public <T> T unwrap(Class<T> unwrapType) {
        if (isUnwrappableAs(unwrapType)) {
            return (T) _masterDS;
        }
        else {
            throw new UnknownUnwrapTypeException(unwrapType);
        }
    }

}
