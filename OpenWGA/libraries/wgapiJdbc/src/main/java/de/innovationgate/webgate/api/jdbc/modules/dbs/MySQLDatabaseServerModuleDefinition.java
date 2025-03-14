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

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.webgate.api.mysql.MySqlDatabaseServer;
import de.innovationgate.webgate.api.mysql.WGDatabaseImpl;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PasswordOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class MySQLDatabaseServerModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/mysqlserver", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("server.description");    
    }

    public Class<? extends Object> getImplementationClass() {
        return MySqlDatabaseServer.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return DatabaseServerModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition name = new LocalizedOptionDefinition(DatabaseServer.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        name.setDefaultValue("localhost");
        options.addOption(name);
        
        LocalizedOptionDefinition port = new LocalizedOptionDefinition(MySqlDatabaseServer.OPTION_PORT, IntegerOptionType.INSTANCE, _bundleLoader);
        port.setOptional(true);
        port.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_PORT));
        options.addOption(port);
        
        LocalizedOptionDefinition masterUser = new LocalizedOptionDefinition(DatabaseServer.OPTION_MASTERLOGIN_USER, StringOptionType.INSTANCE, _bundleLoader);
        options.addOption(masterUser);
        
        LocalizedOptionDefinition masterPassword = new LocalizedOptionDefinition(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD, new PasswordOptionType(_registry), _bundleLoader);
        options.addOption(masterPassword);
        
        LocalizedOptionDefinition usePool = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL, BooleanOptionType.INSTANCE, _bundleLoader);
        usePool.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(usePool);
        
        LocalizedOptionDefinition poolMax = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_CONNECTIONS, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMax.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMax.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MAX_CONNECTIONS));
        options.addOption(poolMax);
       
        LocalizedOptionDefinition poolMaxIdle = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_IDLE_CONNECTIONS, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMaxIdle.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMaxIdle.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MAX_IDLE));
        options.addOption(poolMaxIdle);
        
        LocalizedOptionDefinition poolMinIdle = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MIN_IDLE_CONNECTIONS, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMinIdle.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMinIdle.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MIN_IDLE));
        options.addOption(poolMinIdle);
        
        LocalizedOptionDefinition poolMaxWait = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_WAIT, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMaxWait.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMaxWait.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MAX_WAIT));
        poolMaxWait.setOptional(true);
        options.addOption(poolMaxWait);
        
        
        LocalizedOptionDefinition poolConnLifetime = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_CONNECTION_LIFETIME, IntegerOptionType.INSTANCE, _bundleLoader);
        poolConnLifetime.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolConnLifetime.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MAX_CONNECTION_LIFETIME));
        poolConnLifetime.setOptional(true);
        options.addOption(poolConnLifetime);
		
        LocalizedOptionDefinition minEvictableIdleTimeMillis = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MIN_EVICTABLE_IDLE_TIME_MILLIS, IntegerOptionType.INSTANCE, _bundleLoader);
        minEvictableIdleTimeMillis.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        minEvictableIdleTimeMillis.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MIN_EVICTABLE_IDLE_TIME_MILLIS));
        minEvictableIdleTimeMillis.setOptional(true);
        options.addOption(minEvictableIdleTimeMillis);
        
        LocalizedOptionDefinition removeAbandonedTimeout = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_REMOVE_ABANDONED_TIMEOUT, IntegerOptionType.INSTANCE, _bundleLoader);
        removeAbandonedTimeout.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        removeAbandonedTimeout.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_REMOVE_ABANDONED_TIMEOUT));
        removeAbandonedTimeout.setOptional(true);
        options.addOption(removeAbandonedTimeout);

        return options;
        
    }

    public Object getProperties() {
        DatabaseServerProperties props = new DatabaseServerProperties();
        props.setSingleton(false);
        props.setAvailableInInitWizard(true);
        return props;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("server.title");
    }
    
    public void testDependencies() throws ModuleDependencyException {
        try {
        	DriverManager.getDriver(MySqlDatabaseServer.JDBC_BASE_PATH);
        }
        catch (SQLException e) {
            throw new ModuleDependencyException("No MySQL JDBC Driver found.");
        }
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

}
