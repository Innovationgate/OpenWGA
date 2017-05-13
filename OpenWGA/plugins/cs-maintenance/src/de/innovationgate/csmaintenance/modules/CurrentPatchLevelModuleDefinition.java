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

package de.innovationgate.csmaintenance.modules;

import java.util.Locale;

import de.innovationgate.csmaintenance.CS5Patcher;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.CS5PatchLevelModuleType;
import de.innovationgate.wga.modules.types.PatchLevelProperties;

public class CurrentPatchLevelModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {

    public String getTitle(Locale locale) {
        return "Current CS5 Patch Level";
    }

    public String getDescription(Locale locale) {
        return "Current CS5 Patch Level from csmaintenance plugin";
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Class<? extends ModuleType> getModuleType() {
        return CS5PatchLevelModuleType.class;
    }

    public Class<? extends Object> getImplementationClass() {
        return CurrentPatchLevelModuleDefinition.class;
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public Object getProperties() {
        return new PatchLevelProperties() {

            public int getPatchLevel() {
                return CS5Patcher.CS5_PATCH_LEVEL;
            }
           
        };
    }

    public String getRegistrationKey() {
        return CS5PatchLevelModuleType.CURRENT_PATCH_LEVEL_MODULEKEY;
    }

}
