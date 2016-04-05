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
import de.innovationgate.webgate.api.jdbc.JDBCServerDatabaseRetriever;
import de.innovationgate.webgate.api.jdbc.JNDIServerDatabaseRetriever;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.modules.dbs.GenericCSModuleDefinition;
import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;

import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.ContentStoreModuleType;

public class JDBCCSModuleDefinition extends GenericCSModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/jdbccs", getClass().getClassLoader());
    public LocalisationBundleLoader getBundleLoader() {
        return _bundleLoader;
    }

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return WGDatabaseImpl.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentStoreModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition path = new LocalizedOptionDefinition(Database.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        path.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(path);
        
        LocalizedOptionDefinition jndiPath = new LocalizedOptionDefinition(Database.OPTION_JNDI_PATH, StringOptionType.INSTANCE, _bundleLoader);
        jndiPath.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(jndiPath);

        LocalizedOptionDefinition optFileHandlingQueryPaging = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING, BooleanOptionType.INSTANCE, _bundleLoader);
        optFileHandlingQueryPaging.setOptional(true);
        optFileHandlingQueryPaging.setExpert(true);
        optFileHandlingQueryPaging.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(optFileHandlingQueryPaging);
        
        LocalizedOptionDefinition readerProfileCreation = new LocalizedOptionDefinition(WGDatabase.COPTION_READERPROFILECREATION, BooleanOptionType.INSTANCE, _bundleLoader);
        readerProfileCreation.setOptional(true);
        readerProfileCreation.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(readerProfileCreation);
        
        LocalizedOptionDefinition mappingFile = new LocalizedOptionDefinition(WGDatabaseImpl.DBOPTION_MAPPINGFILE, StringOptionType.INSTANCE, _bundleLoader);
        mappingFile.setOptional(true);
        mappingFile.setExpert(true);
        options.addOption(mappingFile);
        
        LocalizedOptionDefinition mappingResource = new LocalizedOptionDefinition(WGDatabaseImpl.DBOPTION_MAPPINGRESOURCE, StringOptionType.INSTANCE, _bundleLoader);
        mappingResource.setOptional(true);
        mappingResource.setExpert(true);
        mappingResource.setDefaultValue(WGDatabaseImpl.HIBERNATE_V5_MAPPING_FILE);
        options.addOption(mappingResource);
        
        PredefinedValuesOptionType hqlFetchType = new PredefinedValuesOptionType(_bundleLoader, WGDatabaseImpl.COPTION_HQL_FETCH_TYPE);
        hqlFetchType.addValue(WGDatabaseImpl.HQL_FETCHTYPE_LAZY);
        hqlFetchType.addValue(WGDatabaseImpl.HQL_FETCHTYPE_STRAIGHT);
        LocalizedOptionDefinition hqlFetch = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_HQL_FETCH_TYPE, hqlFetchType, _bundleLoader);
        hqlFetch.setOptional(true);
        hqlFetch.setExpert(true);
        hqlFetch.setDefaultValue(WGDatabaseImpl.HQL_FETCHTYPE_LAZY);
        options.addOption(hqlFetch);
        
//        LocalizedOptionDefinition loadBalance = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_LOADBALANCE, BooleanOptionType.INSTANCE, _bundleLoader);
//        loadBalance.setOptional(true);
//        loadBalance.setExpert(true);
//        loadBalance.setDefaultValue(Boolean.FALSE.toString());
//        options.addOption(loadBalance);
//        
//        LocalizedOptionDefinition masterPersistenceTimeout = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_MASTERPERSISTENCE_TIMEOUT, IntegerOptionType.INSTANCE, _bundleLoader);
//        masterPersistenceTimeout.setOptional(true);
//        masterPersistenceTimeout.setExpert(true);
//        masterPersistenceTimeout.setDefaultValue(String.valueOf(WGDatabaseImpl.DEFAULT_MASTERPERSISTENCE_TIMEOUT));
//        masterPersistenceTimeout.addDependentOption(WGDatabaseImpl.COPTION_LOADBALANCE, Boolean.TRUE.toString());
//        options.addOption(masterPersistenceTimeout);
        
        LocalizedOptionDefinition showSQL = new LocalizedOptionDefinition("hibernate.show_sql", BooleanOptionType.INSTANCE, _bundleLoader);
        showSQL.setOptional(true);
        showSQL.setExpert(true);
        showSQL.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(showSQL);
        
        LocalizedOptionDefinition jdbcBatchSize = new LocalizedOptionDefinition("hibernate.jdbc.batch_size", IntegerOptionType.INSTANCE, _bundleLoader);
        jdbcBatchSize.setOptional(true);
        jdbcBatchSize.setExpert(true);
        jdbcBatchSize.setDefaultValue("5");
        options.addOption(jdbcBatchSize);
        
        LocalizedOptionDefinition openPreparedStatements = new LocalizedOptionDefinition("hibernate.dbcp.maxOpenPreparedStatements", IntegerOptionType.INSTANCE, _bundleLoader);
        openPreparedStatements.setOptional(true);
        openPreparedStatements.setExpert(true);
        openPreparedStatements.setDefaultValue(WGDatabaseImpl.DEFAULT_MAXOPENPREPAREDSTATEMENTS);
        options.addOption(openPreparedStatements);
        
        LocalizedOptionDefinition lazyParentCheck = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_HQL_LAZY_PARENTCHECK, BooleanOptionType.INSTANCE, _bundleLoader);
        lazyParentCheck.setOptional(true);
        lazyParentCheck.setExpert(true);
        lazyParentCheck.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(lazyParentCheck);

        
        options.putAll(super.getOptionDefinitions());
        
        return options;
    }

    public Object getProperties() {
       DatabaseProperties props = new DatabaseProperties();
       props.addServerDatabaseRetriever(new JDBCServerDatabaseRetriever());
       props.addServerDatabaseRetriever(new JNDIServerDatabaseRetriever());
       props.setSelfPersonalizable(true);
       return props;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.title");
    }
    
    public void testDependencies() throws ModuleDependencyException {
    }

}
