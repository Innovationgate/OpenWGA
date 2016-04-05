/*
 * Created on 22.04.2009 from oliver
 *
 */
package de.innovationgate.webgate.api.jdbc.modules.dbs;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.webgate.api.mssql.MSSQLDatabaseServer;
import de.innovationgate.webgate.api.mssql.WGDatabaseImpl;
import de.innovationgate.webgate.api.mysql.MySqlDatabaseServer;
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

public class MSSQLDatabaseServerModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/mssqlserver", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");    
    }

    public Class<? extends Object> getImplementationClass() {
        return MSSQLDatabaseServer.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return DatabaseServerModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition name = new LocalizedOptionDefinition(DatabaseServer.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        options.addOption(name);
        
        LocalizedOptionDefinition port = new LocalizedOptionDefinition(MSSQLDatabaseServer.OPTION_PORT, IntegerOptionType.INSTANCE, _bundleLoader);
        port.setOptional(true);
        port.setDefaultValue(String.valueOf(MSSQLDatabaseServer.DEFAULT_PORT));
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
        poolMax.setDefaultValue(String.valueOf(MSSQLDatabaseServer.DEFAULT_SHAREDPOOL_MAX_CONNECTIONS));
        options.addOption(poolMax);
       
        LocalizedOptionDefinition poolMaxIdle = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_IDLE_CONNECTIONS, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMaxIdle.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMaxIdle.setDefaultValue(String.valueOf(MSSQLDatabaseServer.DEFAULT_SHAREDPOOL_MAX_IDLE));
        options.addOption(poolMaxIdle);
        
        LocalizedOptionDefinition poolMinIdle = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MIN_IDLE_CONNECTIONS, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMinIdle.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMinIdle.setDefaultValue(String.valueOf(MSSQLDatabaseServer.DEFAULT_SHAREDPOOL_MIN_IDLE));
        options.addOption(poolMinIdle);
        
        LocalizedOptionDefinition poolMaxWait = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_WAIT, IntegerOptionType.INSTANCE, _bundleLoader);
        poolMaxWait.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolMaxWait.setDefaultValue(String.valueOf(MSSQLDatabaseServer.DEFAULT_SHAREDPOOL_MAX_WAIT));
        poolMaxWait.setOptional(true);
        options.addOption(poolMaxWait);
        
        LocalizedOptionDefinition poolConnLifetime = new LocalizedOptionDefinition(DatabaseServer.OPTION_SHAREDPOOL_MAX_CONNECTION_LIFETIME, IntegerOptionType.INSTANCE, _bundleLoader);
        poolConnLifetime.addDependentOption(DatabaseServer.OPTION_SHAREDPOOL, Boolean.TRUE.toString());
        poolConnLifetime.setDefaultValue(String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MAX_CONNECTION_LIFETIME));
        options.addOption(poolConnLifetime);
        
        return options;
        
    }

    public Object getProperties() {
        DatabaseServerProperties props = new DatabaseServerProperties();
        props.setSingleton(false);
        props.setAvailableInInitWizard(true);
        return props;
    }

    public String getTitle(Locale locale) {
        return "Microsoft SQLServer Database Server";
    }
    
    public void testDependencies() throws ModuleDependencyException {
        try {
            Class.forName(de.innovationgate.webgate.api.mssql.WGDatabaseImpl.DRIVER);
        }
        catch (ClassNotFoundException e) {
            throw new ModuleDependencyException("The jTDS JDBC Driver is not in classpath");
        }
    }


    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
    }

}
