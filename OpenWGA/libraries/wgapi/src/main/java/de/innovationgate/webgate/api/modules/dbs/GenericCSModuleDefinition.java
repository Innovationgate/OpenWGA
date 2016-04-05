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

import de.innovationgate.webgate.api.DefaultPageRightsFilter;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.modules.DefaultPageRightsFilterModuleDefinition;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.CommaSeparatedListOptionType;
import de.innovationgate.wga.modules.options.JSONListOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.ModuleDefinitionsOptionType;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.WorkflowEngineOptionType;
import de.innovationgate.wga.modules.types.ContentStoreModuleType;
import de.innovationgate.wga.modules.types.PageRightsFilterType;

public abstract class GenericCSModuleDefinition extends GenericContentDatabaseModuleDefinition {

    public Class<? extends ModuleType> getModuleType() {
        return ContentStoreModuleType.class;
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        PredefinedValuesOptionType csVersionType = new PredefinedValuesOptionType(getGenericLoader(), WGDatabase.COPTION_CONTENT_STORE_VERSION);
        csVersionType.addValue(String.valueOf(WGDatabase.CSVERSION_WGA3));
        csVersionType.addValue(String.valueOf(WGDatabase.CSVERSION_WGA4_1));
        csVersionType.addValue(String.valueOf(WGDatabase.CSVERSION_WGA5));
        LocalizedOptionDefinition csVersion = new LocalizedOptionDefinition(WGDatabase.COPTION_CONTENT_STORE_VERSION, csVersionType, getGenericLoader());
        csVersion.setOptional(true);
        csVersion.setExpert(true);
        options.addOption(csVersion);
        
        LocalizedOptionDefinition projectMode = new LocalizedOptionDefinition(WGDatabase.COPTION_PROJECTMODE, BooleanOptionType.INSTANCE, getGenericLoader());
        projectMode.setOptional(true);
        projectMode.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(projectMode);
        
        LocalizedOptionDefinition designProviderTypes = new LocalizedOptionDefinition(WGDatabase.COPTION_DESIGNPROVIDERTYPES, CommaSeparatedListOptionType.INSTANCE, getGenericLoader());
        designProviderTypes.setOptional(true);
        designProviderTypes.setExpert(true);
        designProviderTypes.setDefaultValue(WGDocument.TYPENAME_FILECONTAINER + "," + WGDocument.TYPENAME_CSSJS + "," + WGDocument.TYPENAME_TML);
        options.addOption(designProviderTypes);
        
        LocalizedOptionDefinition autoApprove = new LocalizedOptionDefinition(WGDatabase.COPTION_AUTOAPPROVE, BooleanOptionType.INSTANCE, getGenericLoader());
        autoApprove.setOptional(true);
        autoApprove.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(autoApprove);
        
        LocalizedOptionDefinition wfEngine = new LocalizedOptionDefinition(WGDatabase.COPTION_WORKFLOWENGINE, new WorkflowEngineOptionType(getRegistry()), getGenericLoader());
        wfEngine.setOptional(true);
        wfEngine.setExpert(true);
        options.addOption(wfEngine);
        
        LocalizedOptionDefinition pageReaders = new LocalizedOptionDefinition(WGDatabase.COPTION_PAGEREADERS_ENABLED, BooleanOptionType.INSTANCE, getGenericLoader());
        pageReaders.setOptional(true);
        pageReaders.setExpert(true);
        pageReaders.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(pageReaders);
        
        LocalizedOptionDefinition mandatoryReaders = new LocalizedOptionDefinition(WGDatabase.COPTION_MANDATORY_READERS, JSONListOptionType.INSTANCE, getGenericLoader());
        mandatoryReaders.setOptional(true);
        options.addOption(mandatoryReaders);
        
        ModuleDefinitionsOptionType pageRightsOptionType = new ModuleDefinitionsOptionType(_registry, PageRightsFilterType.class);
        LocalizedOptionDefinition pageRightsFilter = new LocalizedOptionDefinition(WGDatabase.COPTION_PAGERIGHTSFILTER, pageRightsOptionType, getGenericLoader());
        pageRightsFilter.setOptional(true);
        pageRightsFilter.setDefaultValue(DefaultPageRightsFilter.class.getName());
        options.addOption(pageRightsFilter);
        
        options.putAll(super.getOptionDefinitions());
        return options;
    }

}
