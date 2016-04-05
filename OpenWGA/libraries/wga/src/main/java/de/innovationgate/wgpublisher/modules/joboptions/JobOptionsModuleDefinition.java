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

package de.innovationgate.wgpublisher.modules.joboptions;

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
import de.innovationgate.wga.modules.types.JobOptionsModuleType;
import de.innovationgate.wgpublisher.scheduler.Job;

public class JobOptionsModuleDefinition implements ModuleDefinition {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(this.getClass()) + "/options", getClass().getClassLoader());

    @Override
    public String getTitle(Locale locale) {
        return "Job options";
    }

    @Override
    public String getDescription(Locale locale) {
        return "Global options for jobs";
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition permanentLog = new LocalizedOptionDefinition(Job.OPTION_PERMANENT_LOG, BooleanOptionType.INSTANCE, _bundleLoader);
        permanentLog.setDefaultValue(Boolean.FALSE.toString());
        options.addOption(permanentLog);
        
        LocalizedOptionDefinition permanentLogCleanup = new LocalizedOptionDefinition(Job.OPTION_PERMANENT_LOG_CLEANUP, IntegerOptionType.INSTANCE, _bundleLoader);
        permanentLogCleanup.addDependentOption(Job.OPTION_PERMANENT_LOG, Boolean.TRUE.toString());
        permanentLogCleanup.setDefaultValue(Job.LOG_CLEANUP_DEFAULT.toString());
        options.addOption(permanentLogCleanup);
        
        return options;
        
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return JobOptionsModuleType.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        return null;
    }

}
