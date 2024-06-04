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

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import org.hibernate.HibernateException;
import org.hibernate.cfg.Environment;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.spi.Stoppable;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.jdbc.HotPatch;
import de.innovationgate.webgate.api.jdbc.pool.DBCPConnectionProvider;

public class JDBCConnectionProvider implements ConnectionProvider, Stoppable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private Driver _driver;

    private DataSource _dataSource;

    private String _path;

    private Properties _props;

    private DBCPConnectionProvider _connectionProvider;

    public JDBCConnectionProvider(String path, Properties props, boolean pool) throws JDBCConnectionException {

        _path = path;
        _props = props;
        
        // Get DataSource or Driver object
        if (path.startsWith("jdbc:")) {
            if (pool) {
                setupDBCPDataSource(path, props);
            }
            else {
                setupNonpoolingDriver(path, props);
            }
        }
        else {
            setupJNDIDataSource(path);
        }

    }
    
    public void close() throws JDBCConnectionException {
        stop();
    }



    private void setupDBCPDataSource(String path, Properties props) throws JDBCConnectionException {
        
        try {
            // Divide properties up between JDBC properties and DBCP properties, store them as hibernate properties so the hibernate DBCP connection provider understands them
            Properties providerProps = new Properties();
            providerProps.put(Environment.URL, path);
            
            Iterator<Map.Entry<Object,Object>> propsIt = props.entrySet().iterator();
            while (propsIt.hasNext()) {
                Map.Entry<java.lang.Object, java.lang.Object> entry = (Map.Entry<java.lang.Object, java.lang.Object>) propsIt.next();
                String key = (String) entry.getKey();
                
                if (key.startsWith("hibernate.")) {
                    providerProps.put(key, (String) entry.getValue());
                }
                else if (key.startsWith("dbcp.")) {
                    providerProps.put("hibernate." + key, (String) entry.getValue());
                }
                else {
                    providerProps.put("hibernate.connection." + key, (String) entry.getValue());
                }
            }
            
            // Create a DBCPConnectionProvider with the created hibernate properties
            _connectionProvider = new DBCPConnectionProvider();
            _connectionProvider.configure(providerProps);
        }
        catch (HibernateException e) {
            throw new JDBCConnectionException("Exception creating DBCP data source", e);
        }
        /*
        catch (ClassNotFoundException e) {
            throw new JDBCConnectionException("Exception creating DBCP data source", e);
        }
        */
    }

    private void setupJNDIDataSource(String path) throws JDBCConnectionException {
        try {
            Context context = new InitialContext();
            Object dataSourceObject = context.lookup(path);
            _dataSource = (DataSource) javax.rmi.PortableRemoteObject.narrow(dataSourceObject, DataSource.class);
            if (_dataSource == null) {
                throw new JDBCConnectionException("Could not load data source under JNDI path " + path);
            }
        }
        catch (ClassCastException e) {
            throw new JDBCConnectionException("No data source object under JNDI path " + path);
        }
        catch (NamingException e) {
            throw new JDBCConnectionException("JNDI error retrieving data source: " + e.getMessage());
        }
    }

    private void setupNonpoolingDriver(String path, Properties props) throws JDBCConnectionException {
        _driver = null;
        try {
			_driver = DriverManager.getDriver(path);
		} catch (SQLException e) {
			throw new JDBCConnectionException("Cannot find JDBC driver for url: " + path);
		}
                
        if (_driver == null) {
            throw new JDBCConnectionException("Cannot find JDBC driver for url: " + path);
        }
        
        Properties providerProps = new Properties();
        
        // Extract props meant for the connection and strip the hibernate prefix from them
        Iterator<Map.Entry<Object,Object>> propsIt = props.entrySet().iterator();
        while (propsIt.hasNext()) {
            Map.Entry<java.lang.Object, java.lang.Object> entry = (Map.Entry<java.lang.Object, java.lang.Object>) propsIt.next();
            String key = (String) entry.getKey();
            if (key.startsWith("hibernate.connection.")) {
                providerProps.put(key.substring(21), (String) entry.getValue());
            }
            else {
                providerProps.put(key, (String) entry.getValue());
            }
        }
        _props = providerProps;
        
    }

    public Connection getConnection() throws SQLException {

        if (_driver != null) {
            return _driver.connect(_path, _props);
        }
        else if (_connectionProvider != null) {
            return _connectionProvider.getConnection();
        }
        else {
            return _dataSource.getConnection();
        }

    }

    @Override
    public void stop() {
        
        try {
            if (_connectionProvider != null) {
                _connectionProvider.stop();
            }
        }
        catch (Exception e) {
            WGFactory.getLogger().error("Exception closing connection provider", e);
        }
        
    }
    
    public List<String> getDatabaseTables() throws SQLException {
        Connection conn = getConnection();
        try {
            return getDatabaseTables(conn);
        }
        finally {
            try {
                conn.close();
            }
            catch (SQLException e) {
            }
        }
        
    }
    
    public boolean performHotPatch(HotPatch hotPatch) throws SQLException {
        
        Connection conn = getConnection();
        try {
            for (String condition : hotPatch.getConditions()) {
                if (!testCondition(conn, condition)) {
                    return false;
                }
            }
            
            WGFactory.getLogger().info("Performing hot patch: " + hotPatch.getDescription());
            executePatch(conn, hotPatch);
            return true;
            
        }
        finally {
            try {
                conn.close();
            }
            catch (SQLException e) {
            }
        }
        
        
    }

    private void executePatch(Connection conn, HotPatch hotPatch) throws SQLException {
            Boolean autoCommitMode = null;
            autoCommitMode = conn.getAutoCommit();
            conn.setAutoCommit(false);
            try {
                Iterator<String> statements = WGUtils.deserializeCollection(hotPatch.getCode().trim(), ";", false, new Character('\'')).iterator();
                Statement st = conn.createStatement();
                while (statements.hasNext()) {
                   String code = ((String) statements.next()).trim();
                   if (!code.equals("")) {
                       st.executeUpdate(code);
                   }
                }
                conn.commit();
            }
            finally {
                if (autoCommitMode != null) {
                    try {
                        conn.setAutoCommit(autoCommitMode);
                    }
                    catch (SQLException e) {
                    }
                }
            }
        
        
    }

    public boolean testCondition(Connection conn, String condition) throws SQLException {
        int colonIdx = condition.indexOf(":");
        String conditionType = condition.substring(0, colonIdx).trim();
        String conditionValue = condition.substring(colonIdx + 1).trim();
        if (conditionType.equals("exists")) {
            
            List<String> valueParts = WGUtils.deserializeCollection(conditionValue, "/");
            List<String> tables = getDatabaseTables(conn);
            String table = valueParts.get(0).toLowerCase();
            if (!tables.contains(table)) {
                return false;
            }
            
            if (valueParts.size() > 1) {
                List<String> columns = getTableColumns(conn, table);
                String column = valueParts.get(1).toLowerCase();
                if (!columns.contains(column)) {
                    return false;
                }
            }
            
            return true;
                    
        }
        
        else if (conditionType.equals("notexists")) {
            
            List<String> valueParts = WGUtils.deserializeCollection(conditionValue, "/");
            List<String> tables = getDatabaseTables(conn);
            String table = valueParts.get(0).toLowerCase();
            if (!tables.contains(table)) {
                return true;
            }
            
            if (valueParts.size() > 1) {
                List<String> columns = getTableColumns(conn, table);
                String column = valueParts.get(1).toLowerCase();
                if (!columns.contains(column)) {
                    return true;
                }
            }
            
            return false;
                    
        };
        
        return false;
    }

    public static List<String> getDatabaseTables(Connection conn) throws SQLException {
        List<String> tables = new ArrayList<String>();
        DatabaseMetaData dbMeta = conn.getMetaData();
        ResultSet resultSet = dbMeta.getTables(null, null, null, new String[] {"TABLE"});
        if (resultSet.getType() != ResultSet.TYPE_FORWARD_ONLY && !resultSet.isBeforeFirst()) {
            resultSet.beforeFirst();
        }
        while (resultSet.next()) {
            tables.add(resultSet.getString("TABLE_NAME").toLowerCase());
        }
        resultSet.close();
        
        return tables;
    }
    
    public static List<String> getTableColumns(Connection conn, String table) throws SQLException {
        List<String> tables = new ArrayList<String>();
        DatabaseMetaData dbMeta = conn.getMetaData();
        ResultSet resultSet = dbMeta.getColumns(null, null, table.toUpperCase(), null); 
        if (resultSet.getType() != ResultSet.TYPE_FORWARD_ONLY && !resultSet.isBeforeFirst()) {
            resultSet.beforeFirst();
        }
        while (resultSet.next()) {
            tables.add(resultSet.getString("COLUMN_NAME").toLowerCase());
        }
        resultSet.close();
        
        return tables;
    }

    @Override
    public boolean isUnwrappableAs(Class arg0) {
        
        if (_driver != null) {
            return (arg0.isAssignableFrom(_driver.getClass()));
        }
        else if (_connectionProvider != null) {
            return _connectionProvider.isUnwrappableAs(arg0);
        }
        else if (_dataSource != null) {
            return (arg0.isAssignableFrom(_dataSource.getClass()));
        }
        else {
            return false;
        }
        
    }

    @Override
    public <T> T unwrap(Class<T> arg0) {

        if (_driver != null) {
            if (arg0.isAssignableFrom(_driver.getClass())) {
                return (T) _driver;
            }
            else {
                throw new ClassCastException("Cannot unwrap as " + arg0.getName());
            }
        }
        else if (_connectionProvider != null) {
            return _connectionProvider.unwrap(arg0);
        }
        else if (_dataSource != null) {
            if (arg0.isAssignableFrom(_dataSource.getClass())) {
                return (T) _dataSource;
            }
            else {
                throw new ClassCastException("Cannot unwrap as " + arg0.getName());
            }
        }
        else {
            throw new IllegalStateException("No backend connection yet present");
        }
        
    }

    @Override
    public void closeConnection(Connection arg0) throws SQLException {
        if (_driver != null) {
            arg0.close();
        }
        else if (_connectionProvider != null) {
            _connectionProvider.closeConnection(arg0);
        }
        else {
            arg0.close();
        }
        
    }

    @Override
    public boolean supportsAggressiveRelease() {
        // TODO Auto-generated method stub
        return false;
    }

}
