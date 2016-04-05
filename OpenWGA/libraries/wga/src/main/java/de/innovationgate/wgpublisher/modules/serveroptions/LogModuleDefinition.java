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

package de.innovationgate.wgpublisher.modules.serveroptions;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.LogLevelOptionType;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.ServerFilePathOptionType;
import de.innovationgate.wga.modules.types.WGAServerOptionsModuleType;
import de.innovationgate.wgpublisher.WGACore;

public class LogModuleDefinition implements ModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/log", getClass().getClassLoader());

    public String getDescription(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("options.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return WGAServerOptionsModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition logLevel = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_LOG_APPLOG_LEVEL, LogLevelOptionType.INSTANCE, _bundleLoader);
        logLevel.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_LOG_APPLOG_LEVEL);
        options.addOption(logLevel);
        
        LocalizedOptionDefinition warnings = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_LOG_WARNINGS, BooleanOptionType.INSTANCE, _bundleLoader);
        warnings.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_LOG_WARNINGS);
        options.addOption(warnings);
        
        PredefinedValuesOptionType warningsTMLType = new PredefinedValuesOptionType(_bundleLoader, WGAConfiguration.SERVEROPTION_LOG_WARNINGS_TML);
        warningsTMLType.addValue(Constants.WARNINGS_TML_OFF);
        warningsTMLType.addValue(Constants.WARNINGS_TML_AS_COMMENT);
        warningsTMLType.addValue(Constants.WARNINGS_TML_AS_HTML);
        LocalizedOptionDefinition warningsTML = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_LOG_WARNINGS_TML, warningsTMLType, _bundleLoader);
        warningsTML.setDefaultValue(WGAConfiguration.SERVEROPTIONDEFAULT_LOG_WARNINGS_TML);
        options.addOption(warningsTML);
        
        LocalizedOptionDefinition warningsConsole = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_LOG_WARNINGS_APPLOG, BooleanOptionType.INSTANCE, _bundleLoader);
        warningsConsole.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(warningsConsole);
        
        LocalizedOptionDefinition permanentLog = new LocalizedOptionDefinition(WGAConfiguration.SERVEROPTION_LOG_PERMANENTLOG_DIR, ServerFilePathOptionType.INSTANCE, _bundleLoader);
        permanentLog.setOptional(true);
        options.addOption(permanentLog);
        
        return options;
    
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale arg0) {
        return _bundleLoader.getBundle(arg0).getString("options.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }

}
