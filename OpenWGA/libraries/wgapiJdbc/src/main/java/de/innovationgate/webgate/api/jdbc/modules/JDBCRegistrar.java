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

package de.innovationgate.webgate.api.jdbc.modules;

import de.innovationgate.webgate.api.jdbc.modules.dbs.HsqlCSModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.HsqlDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.HsqlEnhancedQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.HsqlPersDBModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.HsqlQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.JDBCCSModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.JDBCDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.JDBCEnhancedQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.JDBCPersDBModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.JDBCQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.JNDIDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MSSQLCSModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MSSQLDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MSSQLEnhancedQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MSSQLPersDBModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MSSQLQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLCSModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLEnhancedQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLPersDBModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.MySQLQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.PostgresqlCSModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.PostgresqlDatabaseServerModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.PostgresqlEnhancedQueryDatabaseModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.PostgresqlPersDBModuleDefinition;
import de.innovationgate.webgate.api.jdbc.modules.dbs.PostgresqlQueryDatabaseModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistrar;
import de.innovationgate.wga.modules.ModuleRegistry;

public class JDBCRegistrar implements ModuleRegistrar {

    public void registerModules(ModuleRegistry registry) {

        // Generic JDBC types are disabled by default cause they need to be tweaked a lot to work
        // Can be enabled by sysproperty de.innovationgate.wga.genericjdbc := true
        String useGenericStr = System.getProperty("de.innovationgate.wga.genericjdbc");
        boolean useGeneric = "true".equals(useGenericStr);
        
        // Servers
        if (useGeneric) {
            registry.addModuleDefinition(new JDBCDatabaseServerModuleDefinition());
        }
        registry.addModuleDefinition(new MySQLDatabaseServerModuleDefinition());
        registry.addModuleDefinition(new HsqlDatabaseServerModuleDefinition());
        registry.addModuleDefinition(new JNDIDatabaseServerModuleDefinition());
        registry.addModuleDefinition(new MSSQLDatabaseServerModuleDefinition());
        registry.addModuleDefinition(new PostgresqlDatabaseServerModuleDefinition());
        
        // Content Databases
        if (useGeneric) {
            registry.addModuleDefinition(new JDBCCSModuleDefinition());
            registry.addModuleDefinition(new JDBCQueryDatabaseModuleDefinition());
            registry.addModuleDefinition(new JDBCEnhancedQueryDatabaseModuleDefinition());
        }
            
        registry.addModuleDefinition(new MySQLCSModuleDefinition());
        registry.addModuleDefinition(new MySQLQueryDatabaseModuleDefinition());
        registry.addModuleDefinition(new MySQLEnhancedQueryDatabaseModuleDefinition());
        
        registry.addModuleDefinition(new HsqlCSModuleDefinition());
        registry.addModuleDefinition(new HsqlQueryDatabaseModuleDefinition());
        registry.addModuleDefinition(new HsqlEnhancedQueryDatabaseModuleDefinition());
        
        registry.addModuleDefinition(new MSSQLCSModuleDefinition());
        registry.addModuleDefinition(new MSSQLQueryDatabaseModuleDefinition());
        registry.addModuleDefinition(new MSSQLEnhancedQueryDatabaseModuleDefinition());
        
        registry.addModuleDefinition(new PostgresqlCSModuleDefinition());
        registry.addModuleDefinition(new PostgresqlQueryDatabaseModuleDefinition());
        registry.addModuleDefinition(new PostgresqlEnhancedQueryDatabaseModuleDefinition());

        
        // Personalisation Databases
        if (useGeneric) {
            registry.addModuleDefinition(new JDBCPersDBModuleDefinition());
        }
        registry.addModuleDefinition(new MySQLPersDBModuleDefinition());
        registry.addModuleDefinition(new HsqlPersDBModuleDefinition());    
        registry.addModuleDefinition(new MSSQLPersDBModuleDefinition());
        registry.addModuleDefinition(new PostgresqlPersDBModuleDefinition());
        
        
        

    }

}
