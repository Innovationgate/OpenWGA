/*
 * Created on 10.06.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.jdbc.modules.dbs;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.jdbc.JDBCServerDatabaseRetriever;
import de.innovationgate.webgate.api.jdbc.JNDIServerDatabaseRetriever;
import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.modules.dbs.GenericContentDatabaseModuleDefinition;
import de.innovationgate.webgate.api.mssql.MSSQLQuerySource;
import de.innovationgate.webgate.api.mssql.MSSQLServerDatabaseRetriever;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;

public class MSSQLQueryDatabaseModuleDefinition extends GenericContentDatabaseModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/mssqlquerydb", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return MSSQLQuerySource.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentDatabaseModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition path = new LocalizedOptionDefinition(Database.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        path.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(path);

        LocalizedOptionDefinition jndiPath = new LocalizedOptionDefinition(Database.OPTION_JNDI_PATH, StringOptionType.INSTANCE, _bundleLoader);
        jndiPath.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(jndiPath);
        
        options.putAll(super.getOptionDefinitions());
        
        return options;

    }

    public Object getProperties() {
        DatabaseProperties props = new DatabaseProperties();
        props.addServerDatabaseRetriever(new MSSQLServerDatabaseRetriever(false));
        props.addServerDatabaseRetriever(new JNDIServerDatabaseRetriever());
        return props;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.title");
    }

    public void testDependencies() throws ModuleDependencyException {
        try {
            Class.forName(de.innovationgate.webgate.api.mssql.WGDatabaseImpl.DRIVER);
        }
        catch (ClassNotFoundException e) {
            throw new ModuleDependencyException("The jTDS JDBC Driver is not in classpath");
        }
    }



}
