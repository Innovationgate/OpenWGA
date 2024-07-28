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

import java.sql.SQLException;

import org.apache.commons.dbcp2.BasicDataSource;


public class DBCPPoolInformation implements DBCPPoolInformationMBean {

    private DBCPConnectionProvider _provider;
    private BasicDataSource _ds;

    public DBCPPoolInformation(DBCPConnectionProvider provider) {
        _provider = provider;
        _ds = _provider.getDs();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getNumActive()
     */
    public int getNumActive() {
        return _provider.getDs().getNumActive();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getNumIdle()
     */
    public int getNumIdle() {
        return _provider.getDs().getNumIdle();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getMaxActive()
     */
    public int getMaxActive() {
        return _provider.getDs().getMaxTotal();
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getMaxIdle()
     */
    public int getMaxIdle() {
        return _provider.getDs().getMaxIdle();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getDefaultAutoCommit()
     */
    public boolean getDefaultAutoCommit() {
        return _ds.getDefaultAutoCommit();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getDefaultCatalog()
     */
    public String getDefaultCatalog() {
        return _ds.getDefaultCatalog();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getDefaultReadOnly()
     */
    public boolean getDefaultReadOnly() {
        return _ds.getDefaultReadOnly();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getDefaultTransactionIsolation()
     */
    public int getDefaultTransactionIsolation() {
        return _ds.getDefaultTransactionIsolation();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getDriverClassName()
     */
    public String getDriverClassName() {
        return _ds.getDriverClassName();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getInitialSize()
     */
    public int getInitialSize() {
        return _ds.getInitialSize();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getLoginTimeout()
     */
    public int getLoginTimeout() throws SQLException {
        return _ds.getLoginTimeout();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getMaxOpenPreparedStatements()
     */
    public int getMaxOpenPreparedStatements() {
        return _ds.getMaxOpenPreparedStatements();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getMaxWait()
     */
    public long getMaxWait() {
        return _ds.getMaxWaitMillis();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getMinEvictableIdleTimeMillis()
     */
    public long getMinEvictableIdleTimeMillis() {
        return _ds.getMinEvictableIdleTimeMillis();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getMinIdle()
     */
    public int getMinIdle() {
        return _ds.getMinIdle();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getNumTestsPerEvictionRun()
     */
    public int getNumTestsPerEvictionRun() {
        return _ds.getNumTestsPerEvictionRun();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getTestOnBorrow()
     */
    public boolean getTestOnBorrow() {
        return _ds.getTestOnBorrow();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getTestOnReturn()
     */
    public boolean getTestOnReturn() {
        return _ds.getTestOnReturn();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getTestWhileIdle()
     */
    public boolean getTestWhileIdle() {
        return _ds.getTestWhileIdle();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getTimeBetweenEvictionRunsMillis()
     */
    public long getTimeBetweenEvictionRunsMillis() {
        return _ds.getTimeBetweenEvictionRunsMillis();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getUrl()
     */
    public String getUrl() {
        return _ds.getUrl();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getUsername()
     */
    public String getUsername() {
        return _ds.getUsername();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#getValidationQuery()
     */
    public String getValidationQuery() {
        return _ds.getValidationQuery();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#isPoolPreparedStatements()
     */
    public boolean isPoolPreparedStatements() {
        return _ds.isPoolPreparedStatements();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setDefaultAutoCommit(boolean)
     */
    public void setDefaultAutoCommit(boolean arg0) {
        _ds.setDefaultAutoCommit(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setDefaultCatalog(java.lang.String)
     */
    public void setDefaultCatalog(String arg0) {
        _ds.setDefaultCatalog(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setDefaultReadOnly(boolean)
     */
    public void setDefaultReadOnly(boolean arg0) {
        _ds.setDefaultReadOnly(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setDefaultTransactionIsolation(int)
     */
    public void setDefaultTransactionIsolation(int arg0) {
        _ds.setDefaultTransactionIsolation(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setInitialSize(int)
     */
    public void setInitialSize(int arg0) {
        _ds.setInitialSize(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setLoginTimeout(int)
     */
    public void setLoginTimeout(int arg0) throws SQLException {
        _ds.setLoginTimeout(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setMaxActive(int)
     */
    public void setMaxActive(int arg0) {
        _ds.setMaxTotal(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setMaxIdle(int)
     */
    public void setMaxIdle(int arg0) {
        _ds.setMaxIdle(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setMaxOpenPreparedStatements(int)
     */
    public void setMaxOpenPreparedStatements(int arg0) {
        _ds.setMaxOpenPreparedStatements(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setMaxWait(long)
     */
    public void setMaxWait(long arg0) {
        _ds.setMaxWaitMillis(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setMinEvictableIdleTimeMillis(long)
     */
    public void setMinEvictableIdleTimeMillis(long arg0) {
        _ds.setMinEvictableIdleTimeMillis(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setMinIdle(int)
     */
    public void setMinIdle(int arg0) {
        _ds.setMinIdle(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setNumTestsPerEvictionRun(int)
     */
    public void setNumTestsPerEvictionRun(int arg0) {
        _ds.setNumTestsPerEvictionRun(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setPoolPreparedStatements(boolean)
     */
    public void setPoolPreparedStatements(boolean arg0) {
        _ds.setPoolPreparedStatements(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setTestOnBorrow(boolean)
     */
    public void setTestOnBorrow(boolean arg0) {
        _ds.setTestOnBorrow(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setTestOnReturn(boolean)
     */
    public void setTestOnReturn(boolean arg0) {
        _ds.setTestOnReturn(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setTestWhileIdle(boolean)
     */
    public void setTestWhileIdle(boolean arg0) {
        _ds.setTestWhileIdle(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setTimeBetweenEvictionRunsMillis(long)
     */
    public void setTimeBetweenEvictionRunsMillis(long arg0) {
        _ds.setTimeBetweenEvictionRunsMillis(arg0);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.pool.DBCPPoolInformationMBean#setValidationQuery(java.lang.String)
     */
    public void setValidationQuery(String arg0) {
        _ds.setValidationQuery(arg0);
    }
    
    /*
     * @deprecated
     * use getRemoveAbandonedOnMaintenance()
     */    
    public boolean getRemoveAbandoned() {
        return _ds.getRemoveAbandonedOnMaintenance();
    }

    public boolean getRemoveAbandonedOnBorrow() {
        return _ds.getRemoveAbandonedOnBorrow();
    }

    public boolean getRemoveAbandonedOnMaintenance() {
        return _ds.getRemoveAbandonedOnMaintenance();
    }

    public void setRemoveAbandoned(boolean arg0) {
        _ds.setRemoveAbandonedOnMaintenance(arg0);
        _ds.setRemoveAbandonedOnBorrow(arg0);
    }
    
    public int getRemoveAbandonedTimeout() {
        return _ds.getRemoveAbandonedTimeout();
    }
    
    public void setRemoveAbandonedTimeout(int arg0) {
        _ds.setRemoveAbandonedTimeout(arg0);
    }

    public boolean getLogAbandoned() {
        return _ds.getLogAbandoned();
    }

    public boolean getLogExpiredConnections() {
    	return _ds.getLogExpiredConnections();
    }
    
    public int getValidationQueryTimeout() {
        return _ds.getValidationQueryTimeout();
    }

    public boolean isAccessToUnderlyingConnectionAllowed() {
        return _ds.isAccessToUnderlyingConnectionAllowed();
    }

    public void setAccessToUnderlyingConnectionAllowed(boolean allow) {
        _ds.setAccessToUnderlyingConnectionAllowed(allow);
    }

    public void setLogAbandoned(boolean logAbandoned) {
        _ds.setLogAbandoned(logAbandoned);
    }

    public void setValidationQueryTimeout(int timeout) {
        _ds.setValidationQueryTimeout(timeout);
    }
    
    public boolean isServer() {
        return _provider.isServer();
    }
    
    public String getEntityKey() {
        return _provider.getEntityKey();
    }
    
    public String getEntityTitle() {
        return _provider.getEntityTitle();
    }
    
    
}
