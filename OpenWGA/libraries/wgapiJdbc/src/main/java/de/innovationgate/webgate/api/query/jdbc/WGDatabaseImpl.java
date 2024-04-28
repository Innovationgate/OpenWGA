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
package de.innovationgate.webgate.api.query.jdbc;

import java.io.InputStream;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
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
import java.util.Set;

import javax.sql.DataSource;

import org.apache.log4j.Logger;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.spi.Stoppable;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGResultSetCore;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.MasterLoginAuthSession;
import de.innovationgate.webgate.api.fake.WGFakeStructEntry;
import de.innovationgate.webgate.api.jdbc.SharedPoolJDBCDatabaseServer;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl.CSVersion;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionProvider;
import de.innovationgate.webgate.api.jdbc.custom.JDBCSource;
import de.innovationgate.wga.config.Database;


public class WGDatabaseImpl extends de.innovationgate.webgate.api.fake.WGFakeDatabase {
	
	public static final String DBTYPE = "jdbc/custom";
	
	public static final String COPTION_DRIVER = "Driver";
	
	protected WGDatabase db;
	private String path;
	private String driverName = null;


	private ThreadLocal _connection = new ThreadLocal();

    private ConnectionProvider _connProvider;


	/**
	 * @throws WGBackendException 
	 * @see WGDatabaseImpl#open(WGDatabase, String, String, String, boolean)
	 */
	public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGInvalidDatabaseException, WGBackendException {
		
		super.open(db, path, user, pwd, prepareOnly);
		
		// Get basic data
		this.db = db;
		this.path = path;
		
		this.driverName= getJDBCDriver();
		if (this.driverName != null && this.driverName.equals("")) {
			this.driverName = null;
		}
		
		if (this.driverName == null && this.path.startsWith("jdbc:")) {
			throw new WGInvalidDatabaseException("No jdbc driver specified for database \"" + path + "\". Add database option \"Driver\" with name of driver class.");
		}
		
		// Try to get connection provider for shared server pool
		boolean useSharedPool = WGUtils.getBooleanMapValue(db.getCreationOptions(), WGDatabase.COPTION_SHAREDPOOL, true);
        if (useSharedPool && db.getCreationOptions().containsKey(Database.OPTION_PATH) && db.getServer() instanceof SharedPoolJDBCDatabaseServer) {
            SharedPoolJDBCDatabaseServer poolServer = (SharedPoolJDBCDatabaseServer) db.getServer();
            if (poolServer.isPoolAvailable(new CSVersion(WGDatabase.CSVERSION_NO_CONTENTSTORE, 0))) {
                try {
                    _connProvider = poolServer.createPoolConnectionProvider((String) db.getCreationOptions().get(Database.OPTION_PATH));
                    WGFactory.getLogger().info("Database '" + db.getDbReference() + "' uses the shared connection pool of database server '" + db.getServer().getTitle(Locale.getDefault()) + "'");
                }
                catch (WGInvalidDatabaseException e) {
                    throw e;
                }
                catch (Exception e) {
                    throw new WGInvalidDatabaseException("Exception connecting to shared database server pool", e);
                }
            }
        }

		// Create regular connection provider
		if (_connProvider == null) {
    		// Default props
    		Properties props = new Properties();
    		props.put("autocommit", "true");
    
    		// Gather configured JDBC props
            Map creationOptions =  db.getCreationOptions();
            Iterator optsIt = creationOptions.keySet().iterator();
            while (optsIt.hasNext()) {
                String option = (String) optsIt.next();
                if (option.startsWith(("jdbc."))) {
                    props.put(option.substring(5), creationOptions.get(option));
                }
            }
            
            // Set login props
            if (user != null && !user.equals("")) {
                props.put("user", user);
                if (pwd != null) {
                	props.put("password", pwd);         
                }
            }
            
            // Set dbkey property so we see DBCP metrics via JMX
            props.put("dbcp.dbkey", db.getDbReference());
    		
    		try {
                _connProvider = new JDBCConnectionProvider(path, props, true);
            }
            catch (JDBCConnectionException e) {
                throw new WGInvalidDatabaseException("Exception setting up JDBC connection", e);
            }
		}
		
		return this.openSession(MasterLoginAuthSession.getInstance(), pwd, true);

	}

    protected String getJDBCDriver() {
        return (String) this.db.getCreationOptions().get(COPTION_DRIVER);
    }
	
	public List getRoles() {
		List list = new ArrayList();
		list.add(WGDatabase.ROLE_CONTENT);
		return list;
	}
	
	protected Connection getConnection() throws SQLException {
        Connection connection = (Connection) _connection.get();
        if (connection == null) {
            connection = _connProvider.getConnection();
            _connection.set(connection);
        }
        return connection;
	}

	/**
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#query(String, String, Map)
	 */
	public WGResultSetCore query(String type, String query, Map parameters) throws WGQueryException {
	    
        List nativeOptions = new ArrayList();
        String nativeOptionsString = (String) parameters.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS);
        if (nativeOptionsString != null) {
            nativeOptions.addAll(WGUtils.deserializeCollection(nativeOptionsString.toLowerCase(), ",", true));
        }

        Boolean resetAutocommit = null;
        Connection con = null;
		try {
			con = this.getConnection();
			if (con == null) {
				return null;
			}
			
			boolean isUpdate = nativeOptions.contains("update");
            if (isUpdate && !con.getAutoCommit()) {
                resetAutocommit = con.getAutoCommit();
                con.setAutoCommit(true);
            }
			
			PreparedStatement stmt = con.prepareStatement(query.trim(), ResultSet.TYPE_SCROLL_INSENSITIVE, (isUpdate ? ResultSet.CONCUR_UPDATABLE : ResultSet.CONCUR_READ_ONLY));
			if (parameters.containsKey(WGDatabase.QUERYOPTION_QUERY_PARAMETERS)) {
    			for (Map.Entry paramEntry : (Set<Map.Entry>) ((Map) parameters.get(WGDatabase.QUERYOPTION_QUERY_PARAMETERS)).entrySet()) {
    			    String paramName = String.valueOf(paramEntry.getKey());
    			    if (JDBCSource.INDEX_PARAMETER_NAME.matcher(paramName).matches()) {
        			    try {
                            Integer paramIndex = Integer.valueOf(paramName);
                            stmt.setObject(paramIndex, paramEntry.getValue());
                        }
                        catch (Exception e) {
                            WGFactory.getLogger().error("Exception setting SQL query parameter " + paramEntry.getKey(), e);
                        }
    			    }
    			    
    			    /* We can't do that because some default parameters come as named parameters. So we just ignore them.
    			    else {
    			        throw new WGQueryException(query, "Only indexed parameters (use ? in query and index number as parameter name) are supported on this database type");
    			    }*/
    			}
			}
			
			stmt.execute();
			ResultSet resultSet =  stmt.getResultSet();
			return new WGResultSetImpl(this, resultSet);
		}
		catch (Exception e) {
			throw new WGQueryException(query, "Exception executing query", e);
		}
		finally {
		    if (con != null && resetAutocommit != null) {
                try {
                    con.setAutoCommit(resetAutocommit);
                }
                catch (SQLException e) {
                    throw new WGQueryException(query, "Exception resetting autocommit", e);
                }
            }
		}

	}

	/**
	 * @throws WGBackendException 
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#closeSession()
	 */
	public void closeSession() throws WGBackendException {
		
		Connection con = (Connection) this._connection.get();
		if (con != null) {
			try {
				con.close();
				this._connection.remove();
			}
			catch (SQLException e) {
				throw new WGBackendException("Unable to close session.", e);
			}
		}
	}

	/**
	 * @throws WGBackendException 
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#openSession(String, String)
	 */
	public WGUserAccess openSession(AuthenticationSession authSession, Object pwd, boolean master) throws WGBackendException {
	    return new WGUserAccess(authSession.getDistinguishedName(), WGDatabase.ACCESSLEVEL_READER);
	}

	/**
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
	 */
	public String getTypeName() {
		return DBTYPE;
	}

	/**
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#hasFeature(String)
	 */
	public boolean hasFeature(String feature) {

		if (feature.equals(WGDatabase.FEATURE_QUERYABLE)) {
			return true;
		}
		else {
			return false;
		}

	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
	 */
	public String getServerName() throws WGBackendException {
		try {
			DatabaseMetaData metaData = getConnection().getMetaData();
			return metaData.getDatabaseProductName() + " " + metaData.getDatabaseProductVersion();
		}
		catch (SQLException e) {
            throw new WGBackendException("Error retrieving server name", e);
		}
	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTitle()
     */
    public String getTitle() {
        return path;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#moveStructEntry(de.innovationgate.webgate.api.WGStructEntry, de.innovationgate.webgate.api.WGDocument)
     */
    public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) {
        return false;
    }



    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getAllowedCredentialClasses()
     */
    public Class[] getAllowedCredentialClasses() {
        return new Class[] { String.class };
    }
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getDeletions(java.util.Set)
     */
    public Set getDeletions(Set contentKeys) throws WGAPIException {
        // unsupported for this implementation
        return Collections.EMPTY_SET;
    }
    
    public Object getNativeObject() throws WGBackendException {
        try {
            return getConnection();
        }
        catch (SQLException e) {
            throw new WGBackendException("Error retrieving native object form JDBCSource", e);
        }
    }

	public void beginUpdate() throws WGBackendException {
	}

    @Override
    public void close() throws WGBackendException {
        
        if (_connProvider instanceof Stoppable) {
            ((Stoppable) _connProvider).stop();
        }

    }

    public WGDocumentCore getStructEntryByName(String strName) throws WGAPIException {
        throw new WGNotSupportedException("This operation is not supported by this database type");
    }

    public boolean isContentTypeUsed(WGContentType ct) throws WGAPIException {
        return true;
    }

    public boolean isLanguageUsed(WGLanguage lang) throws WGAPIException {
        return true;
    }
    
    
    public boolean isBackendServiceSupported(String serviceName) {
        return false;
    }
    
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException {
        throw new WGNotSupportedException("No backend services");
    }
    
    @Override
    public WGDocumentCore getStructEntryByKey(Object key) {
        return new WGFakeStructEntry(db, key);
    }
    
}

