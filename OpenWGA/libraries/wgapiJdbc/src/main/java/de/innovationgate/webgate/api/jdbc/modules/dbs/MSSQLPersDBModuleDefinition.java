/*
 * Created on 12.06.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.jdbc.modules.dbs;

import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.hsql.HsqlServerDatabaseRetriever;
import de.innovationgate.webgate.api.jdbc.JNDIServerDatabaseRetriever;
import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.modules.dbs.GenericPersonalisationDatabaseModuleDefinition;
import de.innovationgate.webgate.api.mssql.MSSQLDatabaseServer;
import de.innovationgate.webgate.api.mssql.MSSQLServerDatabaseRetriever;
import de.innovationgate.webgate.api.mssql.WGDatabaseImpl;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType;

public class MSSQLPersDBModuleDefinition extends GenericPersonalisationDatabaseModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/mssqlpers", getClass().getClassLoader());


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
        props.addServerDatabaseRetriever(new MSSQLServerDatabaseRetriever(true));
        props.addServerDatabaseRetriever(new JNDIServerDatabaseRetriever());
        return props;    }


    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.title");
    }
    
    public void testDependencies() throws ModuleDependencyException {
        try {
            DriverManager.getDriver(MSSQLDatabaseServer.JDBC_BASE_PATH);
        }
        catch (SQLException e) {
            throw new ModuleDependencyException("No MSSQL JDBC Driver found.");
        }
    }

}
