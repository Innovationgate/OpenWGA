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
import de.innovationgate.webgate.api.hsql.WGDatabaseImpl;
import de.innovationgate.webgate.api.modules.dbs.DatabaseProperties;
import de.innovationgate.webgate.api.modules.dbs.GenericCSModuleDefinition;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;

public class HsqlCSModuleDefinition extends GenericCSModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/hsqlcs", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");
    }

    public Object getProperties() {
       DatabaseProperties props = new DatabaseProperties();
       props.addServerDatabaseRetriever(new HsqlServerDatabaseRetriever(getImplementationClass()));
       props.setSelfPersonalizable(true);
       return props;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.title");
    }
    
    public void testDependencies() throws ModuleDependencyException {
    }

    public Class<? extends Object> getImplementationClass() {
        return WGDatabaseImpl.class;
    }
    
    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition path = new LocalizedOptionDefinition(Database.OPTION_PATH, StringOptionType.INSTANCE, _bundleLoader);
        path.addFlag(Database.OPTIONFLAG_PATH_OPTION);
        options.addOption(path);

        LocalizedOptionDefinition optFileHandlingQueryPaging = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING, BooleanOptionType.INSTANCE, _bundleLoader);
        optFileHandlingQueryPaging.setOptional(true);
        optFileHandlingQueryPaging.setExpert(true);
        optFileHandlingQueryPaging.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(optFileHandlingQueryPaging);
        
        LocalizedOptionDefinition readerProfileCreation = new LocalizedOptionDefinition(WGDatabase.COPTION_READERPROFILECREATION, BooleanOptionType.INSTANCE, _bundleLoader);
        readerProfileCreation.setOptional(true);
        readerProfileCreation.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(readerProfileCreation);
        
        PredefinedValuesOptionType hqlFetchType = new PredefinedValuesOptionType(_bundleLoader, WGDatabaseImpl.COPTION_HQL_FETCH_TYPE);
        hqlFetchType.addValue(WGDatabaseImpl.HQL_FETCHTYPE_LAZY);
        hqlFetchType.addValue(WGDatabaseImpl.HQL_FETCHTYPE_STRAIGHT);
        LocalizedOptionDefinition hqlFetch = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_HQL_FETCH_TYPE, hqlFetchType, _bundleLoader);
        hqlFetch.setOptional(true);
        hqlFetch.setExpert(true);
        hqlFetch.setDefaultValue(WGDatabaseImpl.HQL_FETCHTYPE_LAZY);
        options.addOption(hqlFetch);
        
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
        
        
        LocalizedOptionDefinition jdbcPort = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_JDBCPORT, IntegerOptionType.INSTANCE, _bundleLoader);
        jdbcPort.setOptional(true);
        jdbcPort.setExpert(true);
        options.addOption(jdbcPort);
        
        LocalizedOptionDefinition openPreparedStatements = new LocalizedOptionDefinition("hibernate.dbcp.maxOpenPreparedStatements", IntegerOptionType.INSTANCE, _bundleLoader);
        openPreparedStatements.setOptional(true);
        openPreparedStatements.setExpert(true);
        openPreparedStatements.setDefaultValue(WGDatabaseImpl.DEFAULT_MAXOPENPREPAREDSTATEMENTS);
        options.addOption(openPreparedStatements);
        
        
        LocalizedOptionDefinition distinctFileContents = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_DISTINCTFILECONTENTS, BooleanOptionType.INSTANCE, _bundleLoader);
        distinctFileContents.setOptional(true);
        distinctFileContents.setExpert(true);
        distinctFileContents.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(distinctFileContents);
        
        LocalizedOptionDefinition dailyCheckpoint = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_DAILY_CHECKPOINT, BooleanOptionType.INSTANCE, _bundleLoader);
        dailyCheckpoint.setOptional(true);
        dailyCheckpoint.setExpert(true);
        dailyCheckpoint.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(dailyCheckpoint);
        
        LocalizedOptionDefinition lazyParentCheck = new LocalizedOptionDefinition(WGDatabaseImpl.COPTION_HQL_LAZY_PARENTCHECK, BooleanOptionType.INSTANCE, _bundleLoader);
        lazyParentCheck.setOptional(true);
        lazyParentCheck.setExpert(true);
        lazyParentCheck.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(lazyParentCheck);
        
        options.putAll(super.getOptionDefinitions());
        
        return options;
    }
}
