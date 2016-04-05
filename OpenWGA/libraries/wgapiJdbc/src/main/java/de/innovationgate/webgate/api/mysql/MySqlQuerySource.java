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

package de.innovationgate.webgate.api.mysql;

import java.util.Map;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.query.jdbc.WGDatabaseImpl;

public class MySqlQuerySource extends WGDatabaseImpl {
	
	

    @Override
    protected String getJDBCDriver() {
        return de.innovationgate.webgate.api.mysql.WGDatabaseImpl.DRIVER;
    }

	@Override
	public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGInvalidDatabaseException, WGBackendException {
        // Build creations options
        Map creationOptions = db.getCreationOptions();
        
        // configuration of connection related properties
        WGDatabase.putDefaultOption(creationOptions, "jdbc.dbcp.validationQuery", "select 1");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.dbcp.testOnBorrow", "true");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.connection.connectTimeout", "60000");
        WGDatabase.putDefaultOption(creationOptions, "jdbc.connection.socketTimeout", "120000");
        
		return super.open(db, path, user, pwd, prepareOnly);
	}

}
