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

package de.innovationgate.webgate.api.hsql;

import java.sql.Driver;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.jdbc.custom.JDBCSource;

public class HsqlEnhancedQuerySource extends JDBCSource {

    private HsqlJDBCServer _hsqlServer;

    @Override
    protected String getJDBCDriver(WGDatabase db) {
        return de.innovationgate.webgate.api.hsql.WGDatabaseImpl.DRIVER;
    }

    @Override
    public void close() throws WGAPIException {
        if (_hsqlServer != null) {
            _hsqlServer.shutdown();
            _hsqlServer = null;
        }
        
        super.close();
    }

    @Override
    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException {
     // Register drivers with driver manager
        Driver driver = null;
        try {
             driver = (Driver) Class.forName(de.innovationgate.webgate.api.hsql.WGDatabaseImpl.DRIVER).newInstance();
        }
        catch (ClassNotFoundException e) {
            throw new WGInvalidDatabaseException("Necessary JDBC driver not found: " + e.getMessage());
        }
        catch (InstantiationException e) {
            throw new WGInvalidDatabaseException("Cannot instantiate neccessary JDBC driver", e);
        }
        catch (IllegalAccessException e) {
            throw new WGInvalidDatabaseException("Cannot instantiate neccessary JDBC driver", e);
        }
        
        // Build hsql path
        String hsqlPath = de.innovationgate.webgate.api.hsql.WGDatabaseImpl.buildHsqlPath(db, path);
        path = "jdbc:hsqldb:" + hsqlPath;
        
        WGUserAccess userAccess = super.open(db, path, user, pwd, prepareOnly);
        
        if (userAccess.getAccessLevel() > WGDatabase.ACCESSLEVEL_NOACCESS && db.getCreationOptions().containsKey(de.innovationgate.webgate.api.hsql.WGDatabaseImpl.COPTION_JDBCPORT)) {
            try {
                int port = Integer.parseInt((String) db.getCreationOptions().get(de.innovationgate.webgate.api.hsql.WGDatabaseImpl.COPTION_JDBCPORT));
                _hsqlServer = new HsqlJDBCServer(hsqlPath, port);
                WGFactory.getLogger().info("JDBC Server for HSQL database '" + db.getDbReference() + "' started on port " + _hsqlServer.getPort());
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Exception creating JDBC server for HSQL database '" + db.getDbReference() + "'", e);
            }
        }
        
        return userAccess;
    }

}
