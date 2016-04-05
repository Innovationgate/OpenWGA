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

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.webgate.api.postgresql.PostgresqlDatabaseServer;
import de.innovationgate.webgate.api.postgresql.WGDatabaseImpl;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PasswordOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.DatabaseServerModuleType;

public class PostgresqlDatabaseServerModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/postgresqlserver", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");    
    }

    public Class<? extends Object> getImplementationClass() {
        return PostgresqlDatabaseServer.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return DatabaseServerModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition name = new LocalizedOptionDefinition(DatabaseServer.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        options.addOption(name);
        
        LocalizedOptionDefinition port = new LocalizedOptionDefinition(PostgresqlDatabaseServer.OPTION_PORT, IntegerOptionType.INSTANCE, _bundleLoader);
        port.setOptional(true);
        port.setDefaultValue(String.valueOf(PostgresqlDatabaseServer.DEFAULT_PORT));
        options.addOption(port);
        
        LocalizedOptionDefinition masterUser = new LocalizedOptionDefinition(DatabaseServer.OPTION_MASTERLOGIN_USER, StringOptionType.INSTANCE, _bundleLoader);
        options.addOption(masterUser);
        
        LocalizedOptionDefinition masterPassword = new LocalizedOptionDefinition(DatabaseServer.OPTION_MASTERLOGIN_PASSWORD, new PasswordOptionType(_registry), _bundleLoader);
        options.addOption(masterPassword);
        
        LocalizedOptionDefinition globalDb = new LocalizedOptionDefinition(PostgresqlDatabaseServer.OPTION_GLOBAL_DATABASE, StringOptionType.INSTANCE, _bundleLoader);
        globalDb.setOptional(true);
        globalDb.setDefaultValue(PostgresqlDatabaseServer.DEFAULT_GLOBAL_DATABASE);
        options.addOption(globalDb);

        
        return options;
        
    }

    public Object getProperties() {
        DatabaseServerProperties props = new DatabaseServerProperties();
        props.setSingleton(false);
        return props;
    }

    public String getTitle(Locale locale) {
        return "PostgreSQL Database Server";
    }
    
    public void testDependencies() throws ModuleDependencyException {
        try {
            Class.forName(WGDatabaseImpl.DRIVER);
        }
        catch (ClassNotFoundException e) {
            throw new ModuleDependencyException("The PostgreSQL JDBC Driver " + WGDatabaseImpl.DRIVER + " is not in classpath");
        }
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

}
