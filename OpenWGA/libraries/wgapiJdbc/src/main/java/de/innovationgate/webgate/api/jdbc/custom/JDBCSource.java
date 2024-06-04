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
package de.innovationgate.webgate.api.jdbc.custom;

import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Pattern;

import org.apache.commons.collections.map.LinkedMap;
import org.apache.log4j.Logger;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.spi.Stoppable;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.jdbc.SharedPoolJDBCDatabaseServer;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl.CSVersion;
import de.innovationgate.webgate.api.templates.ContentSourceSpecs;
import de.innovationgate.webgate.api.templates.SimpleContentSource;
import de.innovationgate.wga.config.Database;

public class JDBCSource extends SimpleContentSource {
    
    public static final Pattern INDEX_PARAMETER_NAME = Pattern.compile("^\\d+$");
	
	public class TableName {



		private String _name;

		private String _schema;

		private String _catalog;

		public TableName(ResultSet resultSet) throws SQLException {
			_catalog = resultSet.getString("TABLE_CAT");
			_schema = resultSet.getString("TABLE_SCHEM");
			_name = resultSet.getString("TABLE_NAME");
		}
		
		public TableName(String catalog, String schema, String name) {
			_catalog = catalog;
			_schema = schema;
			_name = name;
			
			if (_catalog.equals("")) {
				_catalog = null;
			}
			
			if (_schema.equals("")) {
				_schema = null;
			}
			
			if (_name.equals("")) {
				_name = null;
			}
		}
		
		/**
		 * @return
		 */
		public String getCatalog() {
			return _catalog;
		}

		/**
		 * @return
		 */
		public String getCompleteName() {
			return (_schema != null ? _schema + "." + _name : _name);
		}

		/**
		 * @return
		 */
		public String getName() {
			return _name;
		}

		/**
		 * @return
		 */
		public String getSchema() {
			return _schema;
		}

	}

	private int _resultSetType = ResultSet.TYPE_FORWARD_ONLY;
	private String[] _folders;
	public static final String COPTION_REFRESH = "RefreshSeconds";
	private Timer _timer;
	private String _title;
	public static final String LOGGER_NAME = "wga.api.jdbc.custom";
	private String _server;
	private Map _tables = new HashMap();
	private ThreadLocal<Connection> _connection = new ThreadLocal<Connection>();
    private ConnectionProvider _connProvider;
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#login(java.lang.String, java.lang.String)
	 */
	public static final String COPTION_DRIVER = "Driver";
	public int login(String user, String pwd) throws WGAPIException {
		return WGDatabase.ACCESSLEVEL_EDITOR;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#logout()
	 */
	public void logout() throws WGBackendException {
		
		Connection conn = (Connection) _connection.get();
		if (conn != null) {
			try {
				_connProvider.closeConnection(conn);
			}
			catch (SQLException e) {
				throw new WGBackendException("Unable to logout.", e);
			}
			finally {
				//B00004AC2
				_connection.remove();
			}
		}
			
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTitle()
	 */
	public String getTitle() {
		return _title;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
	 */
	public String getTypeName() {
		return "jdbc/custom/v2";
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getCreated()
	 */
	public Date getCreated() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#convertToKey(java.lang.String)
	 */
	public Object convertToKey(String key, String folder) throws WGIllegalArgumentException {
		try {
		    if (folder.equals("sql")) {
		        return new TemporaryKeyMap(key);
		    }
		    else {
		        return new KeyMap(this, key, folder);
		    }
		}
		catch (ParseException e) {
			throw new WGIllegalArgumentException("Unparsable: key '" + key + "' folder '" + folder + "'.", e);
		}
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getContent(java.lang.String, java.lang.Object)
	 */
	public Object getContent(String folder, Object key) throws WGBackendException {
		
		ResultSet resultSet = null;
		try {
			resultSet = getTableResultSet(folder, getWhereClause(folder, key), false);
			Map row = null;
            if (resultSet != null) {
                startResultSet(resultSet);
    			if (resultSet.next()) {
    				row = extractRowData(resultSet);
    			}
    			return row;
            }
            else {
                return null;
            }

		}
		catch (SQLException e) {
            String message = "";
            if (key != null) {
                message = "Error retrieving content with key '" + key.toString() + "'.";
            } else {
                message = "Error retrieving content with key 'null'";
            }
			throw new WGBackendException(message, e);
		}
		finally {
			closeResultSet(resultSet);
		}
		
	}

	/**
	 * @param key
	 * @return
	 */
	private String getWhereClause(String folder, Object key) {

		KeyMap keyMap = (KeyMap) key;
		List keyColumns = (List) _tables.get(folder.toLowerCase());
		if (keyColumns == null) {
		    return null;
		}
		
        Iterator keyColumnsIt = keyColumns.iterator();
		StringBuffer clause = new StringBuffer();
		while (keyColumnsIt.hasNext()) {
			String keyColumn = (String) keyColumnsIt.next();
            
            // Since the key columns from KeyMap originate from the _tables list, we should use the column names unmodified
			//Object keyValue = keyMap.get(keyColumn.toLowerCase());
            Object keyValue = keyMap.get(keyColumn);
			
            if (keyValue instanceof String) {
				keyValue = "'" + keyValue + "'";
			}
			if (keyValue instanceof Date) {
				keyValue = new Timestamp(((Date) keyValue).getTime()).toString();
			}
			
			if (keyValue != null) {
				clause.append(keyColumn).append("=").append(keyValue);
			}
			else {
				clause.append(keyColumn).append(" IS NULL");
			}
			
			
			if (keyColumnsIt.hasNext()) {
				clause.append(" AND ");
			}
		}
		return clause.toString();

	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#putContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean insertContent(String folder, Object key, Object bean) throws WGBackendException {

		ResultSet resultSet = null;
		try {
		    
		    // Gather all columns to set as values, including those keys that are set. Find out the key to generate, if present.
		    Map allColumns = new HashMap();
		    Map valuesMap = (Map) bean;
            allColumns.putAll(valuesMap);
		    String keyToGenerate = null;
		    Map keyMap = (Map) key;
            Iterator keys = keyMap.keySet().iterator();
		    while (keys.hasNext()) {
                String keyName = (String) keys.next();
                Object value = keyMap.get(keyName);
                if (value != null) {
                    allColumns.put(keyName, value);
                }
                else {
                    keyToGenerate = keyName;
                }
            }
		    
		    // Execute Statement
		    PreparedStatement stmt = getInsertStatement(folder, allColumns);
		    int rows = stmt.executeUpdate();
		    if (rows != 1) {
		        throw new WGBackendException("Insert failed. Wrong number of inserted rows returned: " + rows);
		    }

		    if (keyToGenerate == null) {
		        return true;
		    }

		    // Try to retrieve generated key and store it at the bean and the key map
		    if (!stmt.getConnection().getMetaData().supportsGetGeneratedKeys()) {
		        throw new WGBackendException("Row was inserted but JDBC Driver does not support returning of generated keys. Usage of a table with generated key is not possible with this driver.");
		    }
		    ResultSet generatedKeys = stmt.getGeneratedKeys();
		    generatedKeys.first();
		    Object generatedKey = generatedKeys.getObject(1);
		    valuesMap.put(keyToGenerate, generatedKey);
		    keyMap.put(keyToGenerate, generatedKey);
		    
		    if (getConnection().getAutoCommit() == false) {
			    getConnection().commit();
			}
		    
		    return true;
			
		    /*String whereClause = getWhereClause(folder, key);
			resultSet = getTableResultSet(folder, whereClause, true);

			if (resultSet != null) {
                startResultSet(resultSet);
				if (!resultSet.next()) {
					resultSet.moveToInsertRow();
					pushRowData(resultSet, (Map) bean);
					resultSet.insertRow();
					if (getConnection().getAutoCommit() == false) {
					    getConnection().commit();
					}
					return true;
				}
				else {
					throw new WGBackendException("The key '" + key + "' already exists in table '" + folder + "'");
				}
			}
			else {
				return false;
			}*/

		}
		catch (SQLException e) {
			try {
                if (getConnection().getAutoCommit() == false) {
                    getConnection().rollback();
                }
            } catch (SQLException e1) {
                Logger.getLogger(LOGGER_NAME).error("Error rolling back content insertion", e);
            }
			throw new WGBackendException("Error inserting row", e);
		}
		finally {
			closeResultSet(resultSet);
		}

	}

	/**
	 * @param resultSet
	 * @param map
	 */
	private void pushRowData(ResultSet resultSet, Map map) throws SQLException {
		
		ResultSetMetaData rsMeta = resultSet.getMetaData();
		for (int idx=rsMeta.getColumnCount(); idx > 0; idx--) {
			String colName = rsMeta.getColumnName(idx);
			resultSet.updateObject(colName, map.get(colName.toLowerCase()));
		}
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#createContent(java.lang.String, java.lang.Object, java.lang.String)
	 */
	public Object createContent(String folder) {
			return new HashMap();			
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#find(java.lang.String, java.lang.String, java.util.Map)
	 */
	public Map find(String type, String query, Map parameters) throws WGQueryException {

		if (type == null || type.equals("native")) {
			type = "sql";
		}

		List nativeOptions = new ArrayList();
		String nativeOptionsString = (String) parameters.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS);
		if (nativeOptionsString != null) {
			nativeOptions.addAll(WGUtils.deserializeCollection(nativeOptionsString.toLowerCase(), ",", true));
		}

		ResultSet resultSet = null;
		String table = null;
		PreparedStatement stmt;
		Boolean resetAutocommit = null; 
		Connection connection = null;
		try {
		    
		    // Create statement
		    connection = getConnection();
	        boolean isUpdate = nativeOptions.contains("update");
            if (isUpdate && !connection.getAutoCommit()) {
	            resetAutocommit = connection.getAutoCommit();
	            connection.setAutoCommit(true);
	        }
			
            if (type.equals("sql")) {

				stmt = connection.prepareStatement(
					    query,
						ResultSet.TYPE_FORWARD_ONLY,
						(isUpdate ? ResultSet.CONCUR_UPDATABLE : ResultSet.CONCUR_READ_ONLY));

			}
			else if (type.startsWith("table:")) {

				table = type.substring(6).trim();
				if (!_tables.keySet().contains(table.toLowerCase())) {
				    throw new WGQueryException(query, "Table '" + table + "' does not exist or has no primary key");
				}
				    
			    if (query != null && !query.trim().equals("")) {
                    query = "SELECT * FROM " + table + " WHERE " + query;
                }
                else {
                    query = "SELECT * FROM " + table;
                }
				stmt = connection.prepareStatement(query, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
				
			}
			else {
				throw new WGQueryException(query, "Unknown query type: " + type);
			}
			
			// Apply parameters
            if (parameters.containsKey(WGDatabase.QUERYOPTION_MAXRESULTS)) {
                stmt.setMaxRows(((Number) parameters.get(WGDatabase.QUERYOPTION_MAXRESULTS)).intValue());
            }
            
            applyQueryParameters(parameters, stmt, query);

            // Execute and extract data
            stmt.execute();
            resultSet = stmt.getResultSet();
            Map results;
            if (resultSet != null) {
                results = extractRowResults(resultSet, table);
            }
            else {
                results = new HashMap();
            }
            
            return results;

		}
		catch (SQLException e) {
			throw new WGQueryException(query, e.getMessage(), e);
		}
		finally {
			if (connection != null && resetAutocommit != null) {
			    try {
                    connection.setAutoCommit(resetAutocommit);
                }
                catch (SQLException e) {
                    throw new WGQueryException(query, "Exception resetting autocommit", e);
                }
			}
			closeResultSet(resultSet);
		}
	}

    private void applyQueryParameters(Map parameters, PreparedStatement stmt, String query) {
        if (parameters.containsKey(WGDatabase.QUERYOPTION_QUERY_PARAMETERS)) {
            for (Map.Entry paramEntry : (Set<Map.Entry>) ((Map) parameters.get(WGDatabase.QUERYOPTION_QUERY_PARAMETERS)).entrySet()) {
                String paramName = String.valueOf(paramEntry.getKey());
                try {
                    // If it contains only digits it is used as index parameter
                    if (INDEX_PARAMETER_NAME.matcher(paramName).matches()) {
                        Integer paramIndex = Integer.parseInt(paramName);
                        stmt.setObject(paramIndex, paramEntry.getValue());
                    }
                    /* We can't do that because some default parameters come as named parameters. So we just ignore them.
                    else {
                        throw new WGQueryException(query, "Only indexed parameters (use ? in query and index number as parameter name) are supported on this database type");
                    }*/
                    
                    /* Unfortunately not possible since this would need a CallableStatement which has other issues. See #00000156
                    // Else it is used as named parameter
                    else {
                        stmt.setObject(paramName, paramEntry.getValue());
                    }*/
                    
                    
                }
                
                catch (Exception e) {
                    WGFactory.getLogger().error("Exception setting SQL query parameter " + paramEntry.getKey(), e);
                }
            }
        }
    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#removeContent(java.lang.String, java.lang.Object)
	 */
	public void removeContent(String folder, Object key) throws WGBackendException {
	
		ResultSet resultSet = null;
		try {
			String whereClause = getWhereClause(folder, key);
			resultSet = getTableResultSet(folder, whereClause, true);
			if (resultSet != null) {
                startResultSet(resultSet);
				if (resultSet.next()) {
					resultSet.deleteRow();
					if (getConnection().getAutoCommit() == false) {
					    getConnection().commit();
					}
				}
                else {
                    throw new WGBackendException("Cannot remove row '" + key + "' of table '" + folder + "' because it cannot be retrieved");
                }
			}
		}
		catch (SQLException e) {
			try {
                if (getConnection().getAutoCommit() == false) {
                    getConnection().rollback();
                }
            } catch (SQLException e1) {
                Logger.getLogger(LOGGER_NAME).error("Error rolling back content deletion", e);
            }
            throw new WGBackendException("Error removing row '" + key + "' of table '" + folder + "'", e);
		}
		finally {
			closeResultSet(resultSet);
		}
	
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#init(de.innovationgate.webgate.api.WGDatabase, java.lang.String)
	 */
	public ContentSourceSpecs init(WGDatabase db, String path) throws WGInvalidDatabaseException {

	    Map creationOptions =  db.getCreationOptions();
	    
	    // Try to get shared connection pool from server
	    boolean useSharedPool = WGUtils.getBooleanMapValue(db.getCreationOptions(), WGDatabase.COPTION_SHAREDPOOL, true);
        if (useSharedPool && db.getCreationOptions().containsKey(Database.OPTION_PATH) && db.getServer() instanceof SharedPoolJDBCDatabaseServer) {
            SharedPoolJDBCDatabaseServer poolServer = (SharedPoolJDBCDatabaseServer) db.getServer();
            if (poolServer.isPoolAvailable(new CSVersion(0, 0))) {
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
	    

        // Create regular pool
	    if (_connProvider == null) {
    	    // Default JDBC props
    	    Properties props = new Properties();
    	    props.put("autocommit", "true");
    
            // Gather configured JDBC props
    		Iterator optsIt = creationOptions.keySet().iterator();
    		while (optsIt.hasNext()) {
    			String option = (String) optsIt.next();
    			if (option.startsWith(("jdbc."))) {
    				props.put(option.substring(5), creationOptions.get(option));
    			}
    		}
    		
    		// Set login props
    		if (db.getMasterLoginName() != null && !db.getMasterLoginName().equals("")) {
                props.put("user", db.getMasterLoginName());
                if (db.getMasterLoginPassword() != null) {
                	props.put("password", db.getMasterLoginPassword());
                }
            }
    		
            // Set dbkey property so we see DBCP metrics via JMX
            props.put("dbcp.dbkey", db.getDbReference());
    		
    		// Build JDBC Connection Creator
    		try {
                _connProvider = new JDBCConnectionProvider(path, props, true);
            }
            catch (JDBCConnectionException e3) {
                throw new WGInvalidDatabaseException("Exception setting up JDBC connection", e3);
            }
	    }
		
		// Gather other options
		try {
			if (creationOptions.containsKey("ResultSetType")) {
				_resultSetType = Integer.parseInt((String) creationOptions.get("ResultSetType"));
			}
		}
		catch (NumberFormatException e2) {
			throw new WGInvalidDatabaseException("Cannot parse db option 'ResultSetType' as integer: " + _resultSetType);
		}
				
		// Gather meta data
		try {
			Connection connection = getConnection();
			if (connection == null) {
				throw new WGInvalidDatabaseException("Unable to get connection");
			}
			
			DatabaseMetaData dbMeta = connection.getMetaData();
			String dbname = (String)creationOptions.get("Path");
			ResultSet resultSet = dbMeta.getTables(dbname, null, null, new String[] {"TABLE", "VIEW", "GLOBAL TEMPORARY", "LOCAL TEMPORARY"});
			startResultSet(resultSet);
			while (resultSet.next()) {
			
				TableName tableName = new TableName(resultSet);
				ResultSet keyResultSet = dbMeta.getPrimaryKeys(tableName.getCatalog(), tableName.getSchema(), tableName.getName());
				List keyColumns = new ArrayList();
				startResultSet(keyResultSet);
				while(keyResultSet.next()) {
					keyColumns.add(keyResultSet.getString("COLUMN_NAME").toLowerCase());
				}
				
				if (keyColumns.size() > 0) {
					_tables.put(tableName.getCompleteName().toLowerCase(), keyColumns);
				}
				keyResultSet.close();

			}
			resultSet.close();
			
			_server = dbMeta.getDatabaseProductName() + " Version " + dbMeta.getDatabaseProductVersion();
			_title = _server;
		}
		catch (SQLException e) {
			throw new WGInvalidDatabaseException("SQL Error building connection to path " + path + ": " + e.getMessage());
		}
		
		// Last changed update process
		int refreshSeconds = 60; 
		if (creationOptions.containsKey(COPTION_REFRESH)) {
			try {
				refreshSeconds = Integer.parseInt((String) creationOptions.get(COPTION_REFRESH));
			}
			catch (NumberFormatException e1) {
				Logger.getLogger(LOGGER_NAME).error("Cannot parse option " + COPTION_REFRESH + " as integer: " + creationOptions.get(COPTION_REFRESH));
			}
		}
		
		// Gather specs
		ContentSourceSpecs specs = new ContentSourceSpecs();
		specs.setBrowseable(true);
		specs.setWritable(true);
		specs.setCalculatesKeys(true);
		specs.setMaintainsLastChanged(false);
		specs.setLowerCaseItems(true);
        specs.setServePropertiesAsMetas(false);
        specs.setContentReadProtected(false);
		return specs;

	}

    protected String getJDBCDriver(WGDatabase db) {
        return (String) db.getCreationOptions().get(COPTION_DRIVER);
    }

	/**
	 * @return
	 */
	private Connection getConnection() throws SQLException {
		
		Connection connection = (Connection) _connection.get();
		if (connection == null) {
		    connection = _connProvider.getConnection();
			_connection.set(connection);
		}
		return connection;
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#destroy()
	 */
	public void destroy() {
	    
        if (_connProvider instanceof Stoppable) {
            ((Stoppable) _connProvider).stop();
        }
	
		if (_timer != null) {
			_timer.cancel();
		}
	
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#getFolders()
	 */
	public String[] getFolders() {
		
		if (_folders == null) {
			_folders = new String[_tables.size()];
			Iterator keys = _tables.keySet().iterator();
			int idx = 0;
			while (keys.hasNext()) {
				_folders[idx] = String.valueOf(keys.next());
				idx++;
			}
		}
		return _folders;
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#browse(java.lang.String)
	 */
	public Map browse(String folder) throws WGBackendException {

		ResultSet resultSet = null;
		try {
			resultSet = getTableResultSet(folder, null, false);
			if (resultSet != null) {
				Map results = extractRowResults(resultSet, folder);
				return results;
			}
			else {
				return null;
			}

		}
		catch (SQLException e) {
			throw new WGBackendException("Error browsing JDBC source", e);
		}
		finally {
			closeResultSet(resultSet);
		}

	}
	private ResultSet getTableResultSet(String folder, String specify, boolean updatable) throws SQLException {
    	
    	if (!_tables.containsKey(folder.toLowerCase())) {
    		return null;
    	}
    	
    	StringBuffer query = new StringBuffer();
    	query.append("SELECT * FROM " + folder);
    	if (specify != null) {
    		query.append(" WHERE ").append(specify);
    	}
    	Statement stmt = getConnection().createStatement(_resultSetType, (updatable ? ResultSet.CONCUR_UPDATABLE : ResultSet.CONCUR_READ_ONLY));
    	stmt.execute(query.toString());
    	ResultSet resultSet  = stmt.getResultSet();
    	return resultSet;
    }

    private PreparedStatement getInsertStatement(String folder, Map values) throws SQLException {
		
		if (!_tables.containsKey(folder.toLowerCase())) {
			return null;
		}
		
		// Prepare statement
		StringBuffer query = new StringBuffer();
		query.append("INSERT INTO " + folder);

		List columnNames = new ArrayList(values.keySet());
		query.append(" (").append(WGUtils.serializeCollection(columnNames, ",")).append(")");
		
		List columnValues = Collections.nCopies(columnNames.size(), "?");
		query.append(" VALUES (").append(WGUtils.serializeCollection(columnValues, ",")).append(")");
		
		PreparedStatement stmt = getConnection().prepareStatement(query.toString(), Statement.RETURN_GENERATED_KEYS);
		
		// Insert parameter values
		for (int idx=0; idx < columnNames.size(); idx++) {
		    String column = (String) columnNames.get(idx);
		    stmt.setObject(idx + 1, values.get(column));
		}
		
		return stmt;
	}
	private Map extractRowResults(ResultSet resultSet, String tableHint) throws SQLException {
		Map results = new LinkedMap();
		
		startResultSet(resultSet);
		
		while (resultSet.next()) {
			Map row = extractRowData(resultSet);
			Map key = null;
			if (tableHint != null) {
				key = extractRowKey(resultSet, tableHint);
			}
			else {
				key =  new TemporaryKeyMap(resultSet);
			}
			results.put(key, row);
		}
		return results;
	}

    private void startResultSet(ResultSet resultSet) throws SQLException {
        if (resultSet.getType() != ResultSet.TYPE_FORWARD_ONLY && !resultSet.isBeforeFirst()) {
			resultSet.beforeFirst();
		}
    }

	/**
	 * @param resultSet
	 * @return
	 */
	private Map extractRowKey(ResultSet resultSet, String tableHint) throws SQLException {
		ResultSetMetaData rsMeta = resultSet.getMetaData();
		TableName tableName = new TableName(rsMeta.getCatalogName(1), rsMeta.getSchemaName(1), rsMeta.getTableName(1));
		String completeTableName = tableName.getCompleteName();
		if (completeTableName == null || completeTableName.trim().equals("")) {
			completeTableName = tableHint;
		}
		
		Map keys = new KeyMap();
		List keyColumns = (List) _tables.get(String.valueOf(completeTableName).toLowerCase());
		
		// If key columns are not retrievable, just return the empty map as key
		if (keyColumns == null) {
			return keys;
		}
				
		Iterator keyColumnsIt = keyColumns.iterator();
		while (keyColumnsIt.hasNext()) {
			String keyColumn = (String) keyColumnsIt.next();
			Object keyValue = resultSet.getObject(keyColumn);
            
            // Since the key columns from KeyMap originate from the _tables list, we should use the column names unmodified
            // keys.put(keyColumn.toLowerCase(), keyValue);
			keys.put(keyColumn, keyValue);
		}
		
		return keys;
		
		
	}
	private Map extractRowKey(Map map, String tableName) {
		List keyColumns = (List) _tables.get(tableName.toLowerCase());
		if (keyColumns == null) {
			throw new IllegalArgumentException("The table " + tableName + " does not exist or has no primary key!");
		}
		
		Map keys = new KeyMap();
		Iterator keyColumnsIt = keyColumns.iterator();
		while (keyColumnsIt.hasNext()) {
			String keyColumn = (String) keyColumnsIt.next();
            
            // Since the key columns from KeyMap originate from the _tables list, we should use the column names unmodified
			//Object keyValue = map.get(keyColumn.toLowerCase());
            
            Object keyValue = map.get(keyColumn);
			if (keyValue instanceof List) {
				keyValue = ((List) keyValue).get(0);
			}
            
			// keys.put(keyColumn.toLowerCase(), keyValue);
            keys.put(keyColumn, keyValue);
		}
		
		return keys;
		
	}

	/**
	 * @param resultSet
	 * @return
	 */
	private Map extractRowData(ResultSet resultSet) throws SQLException {
		
		Map row = new HashMap();
		ResultSetMetaData rsMeta = resultSet.getMetaData();
		for (int idx=rsMeta.getColumnCount(); idx > 0; idx--) {
			Object value = resultSet.getObject(idx);
            row.put(rsMeta.getColumnLabel(idx).toLowerCase(), value);
            row.put(rsMeta.getColumnName(idx).toLowerCase(), value);
		}
		return row;
				
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
	 */
	public String getServerName() {
		return _server;
	}

	/**
	 * @return
	 */
	public Map getTables() {
		return _tables;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#calculateKey(java.lang.String, java.lang.Object)
	 */
	public Object calculateKey(String folder, Object bean) {
		
		Map map = (Map) bean;
		return extractRowKey(map, folder);
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#updateContent(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	public boolean updateContent(String folder, Object key, Object bean) throws WGBackendException {
		ResultSet resultSet = null;
		try {
			String whereClause = getWhereClause(folder, key);
			resultSet = getTableResultSet(folder, whereClause, true);

			if (resultSet != null) {
                startResultSet(resultSet);
				if (resultSet.next()) {
					pushRowData(resultSet, (Map) bean);
					resultSet.updateRow();
					if (getConnection().getAutoCommit() == false) {
					    getConnection().commit();
					}
					return true;
				}
				else {
					throw new WGBackendException("Unretrievable row '" + key + "' in table '" + folder + "'");
				}
			}
			else {
                throw new WGBackendException("Unretrievable table result set '" + folder + "'");
			}
		}
		catch (SQLException e) {
            try {
                if (getConnection().getAutoCommit() == false) {
                    getConnection().rollback();
                }
            } catch (SQLException e1) {
                Logger.getLogger(LOGGER_NAME).error("Error rolling back content storage", e);
            }
            throw new WGBackendException("Error storing jdbc row", e);
		}
		finally {
			closeResultSet(resultSet);
		}
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.templates.SimpleContentSource#extractFolder(java.lang.String, java.lang.String, java.util.Map)
	 */
	public String extractFolder(String type, String query, Map parameters) {
		
		if (type.startsWith("table:")) {
			return type.substring(6);
		}
		else {
			return null;
		}
		
	}
	
	public static void closeResultSet(ResultSet resultSet) {
		if (resultSet != null) {
			try {
				if (resultSet.getStatement() != null) {
					resultSet.getStatement().close();
				}
				else {
					resultSet.close();
				}
			}
			catch (SQLException e1) {
				Logger.getLogger(JDBCSource.LOGGER_NAME).error("Error closing result set in JDBC Source", e1);
			}
		}
	}
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getDeletions(java.util.Set)
     */
    public Set getDeletions(Set documentKeys) throws WGAPIException {
        Set deletions = new HashSet();
        Iterator keys = documentKeys.iterator();
        while (keys.hasNext()) {
            String documentKey = (String) keys.next();
            // check if doc exists in db
            WGDocument document = getDb().getDocumentByDocumentKey(documentKey);
            if (document == null) {
                // document does not exists in db --> add to deletions
                deletions.add(documentKey);
            }
        }
        return deletions;
    }

    public Object getNativeObject() throws WGBackendException {
        try {
            return getConnection();
        }
        catch (SQLException e) {
            throw new WGBackendException("Error retrieving native object form JDBCSource", e);
        }
    }

    @Override
    public Date getLastModified() throws WGAPIException {
        return null;
    }

}
