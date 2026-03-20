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

package de.innovationgate.webgate.api.jdbc.modules.dbs;

import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Locale;

import de.innovationgate.webgate.api.mariadb.MariaDbDatabaseServer;
import de.innovationgate.wga.modules.ModuleDependencyException;

public class MariaDbDatabaseServerModuleDefinition extends MySQLDatabaseServerModuleDefinition {

    public Class<? extends Object> getImplementationClass() {
        return MariaDbDatabaseServer.class;
    }

    public String getTitle(Locale locale) {
        return "MariaDB Server";
    }

    public String getDescription(Locale locale) {
        return "MariaDB Relational Database Server";    
    }

    public void testDependencies() throws ModuleDependencyException {
        try {
        	DriverManager.getDriver(MariaDbDatabaseServer.JDBC_BASE_PATH);
        }
        catch (SQLException e) {
            throw new ModuleDependencyException("MariaDb JDBC Driver not found.");
        }
    }

}
