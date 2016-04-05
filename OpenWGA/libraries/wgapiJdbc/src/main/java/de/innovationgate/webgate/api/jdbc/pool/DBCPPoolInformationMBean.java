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

public interface DBCPPoolInformationMBean {

    public int getNumActive();

    public int getNumIdle();

    public int getMaxActive();

    public int getMaxIdle();

    public boolean getDefaultAutoCommit();

    public String getDefaultCatalog();

    public boolean getDefaultReadOnly();

    public int getDefaultTransactionIsolation();

    public String getDriverClassName();

    public int getInitialSize();

    public int getLoginTimeout() throws SQLException;

    public int getMaxOpenPreparedStatements();

    public long getMaxWait();

    public long getMinEvictableIdleTimeMillis();

    public int getMinIdle();

    public int getNumTestsPerEvictionRun();

    public boolean getTestOnBorrow();

    public boolean getTestOnReturn();

    public boolean getTestWhileIdle();

    public long getTimeBetweenEvictionRunsMillis();

    public String getUrl();

    public String getUsername();

    public String getValidationQuery();

    public boolean isPoolPreparedStatements();

    public void setDefaultAutoCommit(boolean arg0);

    public void setDefaultCatalog(String arg0);

    public void setDefaultReadOnly(boolean arg0);

    public void setDefaultTransactionIsolation(int arg0);

    public void setInitialSize(int arg0);

    public void setLoginTimeout(int arg0) throws SQLException;

    public void setMaxActive(int arg0);

    public void setMaxIdle(int arg0);

    public void setMaxOpenPreparedStatements(int arg0);

    public void setMaxWait(long arg0);

    public void setMinEvictableIdleTimeMillis(long arg0);

    public void setMinIdle(int arg0);

    public void setNumTestsPerEvictionRun(int arg0);

    public void setPoolPreparedStatements(boolean arg0);

    public void setTestOnBorrow(boolean arg0);

    public void setTestOnReturn(boolean arg0);

    public void setTestWhileIdle(boolean arg0);

    public void setTimeBetweenEvictionRunsMillis(long arg0);

    public void setValidationQuery(String arg0);

    public boolean getRemoveAbandoned();

    public void setRemoveAbandoned(boolean arg0);

    public int getRemoveAbandonedTimeout();

    public void setRemoveAbandonedTimeout(int arg0);

    public boolean getLogAbandoned();

    public int getValidationQueryTimeout();

    public boolean isAccessToUnderlyingConnectionAllowed();

    public void setAccessToUnderlyingConnectionAllowed(boolean allow);

    public void setLogAbandoned(boolean logAbandoned);

    public void setValidationQueryTimeout(int timeout);
    
    public boolean isServer();
    
    public String getEntityKey();
    
    public String getEntityTitle();

}
