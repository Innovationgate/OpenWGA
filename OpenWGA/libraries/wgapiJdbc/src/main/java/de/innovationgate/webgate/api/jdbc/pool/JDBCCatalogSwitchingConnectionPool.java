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
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.jdbc.connections.spi.MultiTenantConnectionProvider;
import org.hibernate.service.spi.Stoppable;

import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionProvider;

public class JDBCCatalogSwitchingConnectionPool extends JDBCConnectionProvider implements MultiTenantConnectionProvider {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private List<TenantConnectionProvider> _activeTenantProviders = new ArrayList<JDBCCatalogSwitchingConnectionPool.TenantConnectionProvider>();
    
    public class TenantConnectionProvider implements ConnectionProvider, Stoppable {
        
        /**
         * 
         */
        private static final long serialVersionUID = 1L;
        private String _catalogName;

        private TenantConnectionProvider(String catalogName) {
            _catalogName = catalogName;
        }

        @Override
        public boolean isUnwrappableAs(Class arg0) {
            return JDBCCatalogSwitchingConnectionPool.this.isUnwrappableAs(arg0);
        }

        @Override
        public <T> T unwrap(Class<T> arg0) {
            return JDBCCatalogSwitchingConnectionPool.this.unwrap(arg0);
        }

        @Override
        public void closeConnection(Connection arg0) throws SQLException {
            JDBCCatalogSwitchingConnectionPool.this.releaseConnection(_catalogName, arg0);
        }

        @Override
        public Connection getConnection() throws SQLException {
            return JDBCCatalogSwitchingConnectionPool.this.getConnection(_catalogName);
        }

        @Override
        public boolean supportsAggressiveRelease() {
            return true;
        }

        @Override
        public void stop() {
            synchronized (JDBCCatalogSwitchingConnectionPool.this) {
                _activeTenantProviders.remove(this);
            }
        }
    }

    public JDBCCatalogSwitchingConnectionPool(String path, String driverClass, Properties props) throws JDBCConnectionException {
        super(path, driverClass, props, true);
    }

    @Override
    public boolean isUnwrappableAs(Class arg0) {
        return super.isUnwrappableAs(arg0);
    }

    @Override
    public <T> T unwrap(Class<T> arg0) {
        return unwrap(arg0);
    }

    @Override
    public Connection getAnyConnection() throws SQLException {
        throw new SQLException("Retrieving 'any' connection not supported");
    }

    @Override
    public Connection getConnection(String tenantIdentifier) throws SQLException {
        Connection conn = super.getConnection();
        conn.setCatalog(tenantIdentifier);
        if (!tenantIdentifier.equals(conn.getCatalog())) {
            throw new SQLException("Switching catalog to '" + tenantIdentifier + "' failed");
        }
        return conn;
    }

    @Override
    public void releaseAnyConnection(Connection connection) throws SQLException {
        super.closeConnection(connection);        
    }

    @Override
    public void releaseConnection(String tenantIdentifier, Connection connection) throws SQLException {
        super.closeConnection(connection);
    }

    @Override
    public boolean supportsAggressiveRelease() {
        return super.supportsAggressiveRelease();
    }
    
    public synchronized TenantConnectionProvider createTenantConnectionProvider(String catalogName) throws JDBCConnectionException {
        TenantConnectionProvider provider = new TenantConnectionProvider(catalogName);
        try {
            Connection conn = provider.getConnection();
            provider.closeConnection(conn);
        }
        catch (SQLException e) {
            throw new JDBCConnectionException("Exception connecting to catalog "+  catalogName, e);
        }
        _activeTenantProviders.add(provider);
        return provider;
    }

    public List<TenantConnectionProvider> getActiveTenantProviders() {
        return Collections.unmodifiableList(_activeTenantProviders);
    }

}