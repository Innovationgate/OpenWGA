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
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.hsql.HsqlServerDatabaseRetriever;
import de.innovationgate.webgate.api.jdbc.JNDIServerDatabaseRetriever;
import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.modules.dbs.GenericPersonalisationDatabaseModuleDefinition;
import de.innovationgate.webgate.api.mysql.MySqlServerDatabaseRetriever;
import de.innovationgate.webgate.api.mysql.WGDatabaseImpl;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType;

public class MySQLPersDBModuleDefinition extends GenericPersonalisationDatabaseModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/mysqlpers", getClass().getClassLoader());


    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = super.getOptionDefinitions();
        
        LocalizedOptionDefinition path = new LocalizedOptionDefinition(Database.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        path.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(path);

        LocalizedOptionDefinition jndiPath = new LocalizedOptionDefinition(Database.OPTION_JNDI_PATH, StringOptionType.INSTANCE, _bundleLoader);
        jndiPath.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(jndiPath);

        
        return options;
    }


    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");
    }


    public Class<? extends Object> getImplementationClass() {
        return WGDatabaseImpl.class;
    }


    public Object getProperties() {
        DatabaseProperties props = new DatabaseProperties();
        props.addServerDatabaseRetriever(new MySqlServerDatabaseRetriever(true));
        props.addServerDatabaseRetriever(new JNDIServerDatabaseRetriever());
        return props;    
    }


    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.title");
    }
    
    public void testDependencies() throws ModuleDependencyException {
        try {
            Class.forName(WGDatabaseImpl.DRIVER);
        }
        catch (ClassNotFoundException e) {
            throw new ModuleDependencyException("The MySQL JDBC Driver \"Connector/J\" is not in classpath");
        }
    }

}
