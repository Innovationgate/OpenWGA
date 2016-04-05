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
import de.innovationgate.wga.modules.options.BooleanOptionType;
import de.innovationgate.wga.modules.options.LocalizedOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.options.StringOptionType;
import de.innovationgate.wga.modules.types.SchedulerTaskModuleType;
import de.innovationgate.wgpublisher.modules.DatabasesOptionType;
import de.innovationgate.wgpublisher.modules.RuntimeDatabasesOptionType;
import de.innovationgate.wgpublisher.scheduler.JavaTask;
import de.innovationgate.wgpublisher.scheduler.ScriptTask;

public class ScriptTaskModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {
    
    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/scripttask", getClass().getClassLoader());
    private ModuleRegistry _registry;

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("task.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return ScriptTask.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return SchedulerTaskModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {

        OptionDefinitionsMap options = new OptionDefinitionsMap();
        
       
        LocalizedOptionDefinition moduleFromPlugin = new LocalizedOptionDefinition(ScriptTask.OPTION_MODULEFROMPLUGIN, BooleanOptionType.INSTANCE, _bundleLoader);
        moduleFromPlugin.setDefaultValue("false");
        options.addOption(moduleFromPlugin);
        
        LocalizedOptionDefinition database = new LocalizedOptionDefinition(ScriptTask.OPTION_DATABASE, new RuntimeDatabasesOptionType(_registry, RuntimeDatabasesOptionType.SELECTION_CONTENT_STORES), _bundleLoader);
        database.addDependentOption(ScriptTask.OPTION_MODULEFROMPLUGIN, Boolean.FALSE.toString());
        options.addOption(database);
        
        LocalizedOptionDefinition plugindb = new LocalizedOptionDefinition(ScriptTask.OPTION_PLUGINDB, new RuntimeDatabasesOptionType(_registry, RuntimeDatabasesOptionType.SELECTION_PLUGIN_DATABASES), _bundleLoader);
        plugindb.addDependentOption(ScriptTask.OPTION_MODULEFROMPLUGIN, Boolean.TRUE.toString());
        options.addOption(plugindb);
        
        LocalizedOptionDefinition module = new LocalizedOptionDefinition(ScriptTask.OPTION_MODULE, StringOptionType.INSTANCE, _bundleLoader);
        options.addOption(module);
        
        return options;
        
    }

    public Object getProperties() {
        return new SchedulerTaskProperties() {
            @Override
            public String createTitle(Task task, Locale locale) {
                boolean usePlugin = Boolean.parseBoolean(task.getOptions().get(ScriptTask.OPTION_MODULEFROMPLUGIN));
                String db = (usePlugin ? task.getOptions().get(ScriptTask.OPTION_PLUGINDB) : task.getOptions().get(ScriptTask.OPTION_DATABASE));
                String module = task.getOptions().get(ScriptTask.OPTION_MODULE);
                return "Execute TMLScript module " + db + "/" + module;
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
