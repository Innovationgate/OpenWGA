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
package de.innovationgate.webgate.api.modules.dbs;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.rss2.SimpleRSS;
import de.innovationgate.webgate.api.servers.GenericServerDatabaseRetriever;
import de.innovationgate.webgate.api.servers.OtherSourcesDatabaseServer;
import de.innovationgate.webgate.api.simple.BeanAdapter;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.ClassNameOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.OptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.ContentDatabaseModuleType;

public class BeanAdapterModuleDefinition extends GenericContentDatabaseModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/beanadapter", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("db.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return BeanAdapter.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentDatabaseModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition className = new LocalizedOptionDefinition(Database.OPTION_PATH, ClassNameOptionType.INSTANCE, _bundleLoader);
        options.addOption(className);
        
        LocalizedOptionDefinition jarFile = new LocalizedOptionDefinition(BeanAdapter.COPTION_BEAN_JARFILE, StringOptionType.INSTANCE, _bundleLoader);
        jarFile.setOptional(true);
        options.addOption(jarFile);
        
        LocalizedOptionDefinition creationHandler = new LocalizedOptionDefinition(BeanAdapter.COPTION_CREATION_HANDLER, StringOptionType.INSTANCE, _bundleLoader);
        creationHandler.setOptional(true);
        options.addOption(creationHandler);
        
        PredefinedValuesOptionType oneBeanPerType = new PredefinedValuesOptionType(_bundleLoader, BeanAdapter.COPTION_ONE_BEAN_PER);
        oneBeanPerType.addValue(BeanAdapter.COPTION_ONE_BEAN_PER_DB);
        oneBeanPerType.addValue(BeanAdapter.COPTION_ONE_BEAN_PER_REQUEST);
        oneBeanPerType.addValue(BeanAdapter.COPTION_ONE_BEAN_PER_USER);
        LocalizedOptionDefinition oneBeanPer = new LocalizedOptionDefinition(BeanAdapter.COPTION_ONE_BEAN_PER, oneBeanPerType, _bundleLoader);
        oneBeanPer.setOptional(true);
        oneBeanPer.setDefaultValue(BeanAdapter.COPTION_ONE_BEAN_PER_REQUEST);
        options.addOption(oneBeanPer);

        options.putAll(super.getOptionDefinitions());        
        
        return options;
    }

    public Object getProperties() {
        DatabaseProperties props = new DatabaseProperties();
        GenericServerDatabaseRetriever sdr = new GenericServerDatabaseRetriever();
        sdr.setDatabaseServerType(OtherSourcesDatabaseServer.class);
        props.addServerDatabaseRetriever(sdr);
        return props;
    }

    public String getTitle(Locale locale) {
        return "Bean Adapter";
    }

}
