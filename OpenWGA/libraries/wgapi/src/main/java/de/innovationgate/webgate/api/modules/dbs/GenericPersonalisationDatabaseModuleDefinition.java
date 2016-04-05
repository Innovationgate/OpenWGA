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

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.CommaSeparatedListOptionType;
import de.innovationgate.wga.modules.options.IntegerOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.LocalizedOptionValueProvider;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.types.PersonalisationDatabaseModuleType;

public abstract class GenericPersonalisationDatabaseModuleDefinition implements ModuleDefinition {

    private LocalisationBundleLoader _genericLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(GenericPersonalisationDatabaseModuleDefinition.class) + "/genericpers", getClass().getClassLoader());  

    public OptionDefinitionsMap getOptionDefinitions() {
        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        PredefinedValuesOptionType csVersionType = new PredefinedValuesOptionType(_genericLoader, WGDatabase.COPTION_CONTENT_STORE_VERSION);
        csVersionType.addValue(String.valueOf(WGDatabase.CSVERSION_WGA3));
        csVersionType.addValue(String.valueOf(WGDatabase.CSVERSION_WGA5));
        LocalizedOptionDefinition csVersion = new LocalizedOptionDefinition(WGDatabase.COPTION_CONTENT_STORE_VERSION, csVersionType, _genericLoader);
        csVersion.setOptional(true);
        csVersion.setExpert(true);
        options.addOption(csVersion);
        
        LocalizedOptionDefinition deletionCheck = new LocalizedOptionDefinition(WGDatabase.COPTION_DELETIONCHECK, BooleanOptionType.INSTANCE, _genericLoader);
        deletionCheck.setOptional(true);
        deletionCheck.setExpert(true);
        deletionCheck.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(deletionCheck);

        LocalizedOptionDefinition caching = new LocalizedOptionDefinition(WGDatabase.COPTION_CACHING_ENABLED, BooleanOptionType.INSTANCE, _genericLoader);
        caching.setOptional(true);
        caching.setExpert(true);
        caching.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(caching);
        
        PredefinedValuesOptionType noItemBehaviourProvider = new PredefinedValuesOptionType(_genericLoader, WGDatabase.COPTION_NOITEMBEHAVIOUR);
        noItemBehaviourProvider.addValue("default");
        noItemBehaviourProvider.addValue("compatible");
        LocalizedOptionDefinition noItemBehaviour = new LocalizedOptionDefinition(WGDatabase.COPTION_NOITEMBEHAVIOUR, noItemBehaviourProvider, _genericLoader);
        noItemBehaviour.setOptional(true);
        noItemBehaviour.setExpert(true);
        noItemBehaviour.setDefaultValue("default");
        options.addOption(noItemBehaviour);
        
        return options;
    }
    
    public void testDependencies() throws ModuleDependencyException {
    }
    
    public Class<? extends ModuleType> getModuleType() {
        return PersonalisationDatabaseModuleType.class;
    }


}
