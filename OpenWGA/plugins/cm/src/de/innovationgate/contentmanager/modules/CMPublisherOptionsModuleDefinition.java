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

package de.innovationgate.contentmanager.modules;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
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
import de.innovationgate.wgpublisher.modules.poptions.ContentStorePublisherOptionsCollector;

public class CMPublisherOptionsModuleDefinition implements ModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/poptions_cs", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return "Publisher options defined by OpenWGA Content Manager";
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return ContentStorePublisherOptionsCollector.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition unamesOnContents = new LocalizedOptionDefinition("UniqueNamesOnContents", BooleanOptionType.INSTANCE, _bundleLoader);
        unamesOnContents.setOptional(true);
        unamesOnContents.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(unamesOnContents);
        
        LocalizedOptionDefinition readersOnContents = new LocalizedOptionDefinition("ReadersOnContents", BooleanOptionType.INSTANCE, _bundleLoader);
        readersOnContents.setOptional(true);
        readersOnContents.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(readersOnContents);

        LocalizedOptionDefinition accessRightsRestriction = new LocalizedOptionDefinition("CM.accessRightsRestriction", BooleanOptionType.INSTANCE, _bundleLoader);
        accessRightsRestriction.setOptional(true);
        accessRightsRestriction.setDefaultValue(Boolean.TRUE.toString());
        options.addOption(accessRightsRestriction);
        
        LocalizedOptionDefinition maxTextUploadSize = new LocalizedOptionDefinition("CM.MaxTextUploadSize", IntegerOptionType.INSTANCE, _bundleLoader);
        maxTextUploadSize.setOptional(true);
        maxTextUploadSize.setDefaultValue("10");
        options.addOption(maxTextUploadSize);
        
        PredefinedValuesOptionType preventFormattedPastingOptionType = new PredefinedValuesOptionType(_bundleLoader, "CM.PreventFormattedPasting");
        preventFormattedPastingOptionType.addValue("off");
        preventFormattedPastingOptionType.addValue("incapablebrowsers");
        LocalizedOptionDefinition preventFormattedPasting = new LocalizedOptionDefinition("CM.PreventFormattedPasting", preventFormattedPastingOptionType, _bundleLoader);
        preventFormattedPasting.setOptional(true);
        preventFormattedPasting.setDefaultValue("off");
        options.addOption(preventFormattedPasting);
        
        
//        LocalizedOptionDefinition autoScale = new LocalizedOptionDefinition("ScaleImageWidthOnUpload", IntegerOptionType.INSTANCE, _bundleLoader);
//        autoScale.setOptional(true);
//        options.addOption(autoScale);
//        
//        autoScale = new LocalizedOptionDefinition("ScaleImageHeightOnUpload", IntegerOptionType.INSTANCE, _bundleLoader);
//        autoScale.setOptional(true);
//        options.addOption(autoScale);
        
        return options;
    
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return "Content Manager Publisher Options";
    }

    public void testDependencies() throws ModuleDependencyException {
    }

}
