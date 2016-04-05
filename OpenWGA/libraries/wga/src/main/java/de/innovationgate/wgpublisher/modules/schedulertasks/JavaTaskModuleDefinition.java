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

package de.innovationgate.wgpublisher.modules.schedulertasks;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.Task;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.ClassNameOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.SchedulerTaskModuleType;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType;
import de.innovationgate.wgpublisher.scheduler.JavaTask;
import de.innovationgate.wgpublisher.scheduler.ScriptTask;

public class JavaTaskModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/javatask", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("task.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return JavaTask.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return SchedulerTaskModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
        LocalizedOptionDefinition database = new LocalizedOptionDefinition(JavaTask.OPTION_TASKCLASS, ClassNameOptionType.INSTANCE, _bundleLoader);
        options.addOption(database);
        
        return options;
        
    }

    public Object getProperties() {
        return new SchedulerTaskProperties() {
            
            @Override
            public String createTitle(Task task, Locale locale) {
                String className = task.getOptions().get(JavaTask.OPTION_TASKCLASS);
                return "Execute java task: " + className;
            }
        };
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("task.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public void injectRegistry(ModuleRegistry registry) {
        _registry = registry;
        
    }

}
