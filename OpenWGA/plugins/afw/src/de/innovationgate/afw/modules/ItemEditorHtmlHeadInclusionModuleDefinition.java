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

package de.innovationgate.afw.modules;

import java.util.Locale;

import de.innovationgate.afw.ItemEditorHtmlHeadInclusion;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.HTMLHeadInclusionModuleType;

public class ItemEditorHtmlHeadInclusionModuleDefinition implements ModuleDefinition {

    public String getDescription(Locale locale) {
        return "Adds HTML head functionalities for Content Manager";
    }

    public Class<? extends Object> getImplementationClass() {
        return ItemEditorHtmlHeadInclusion.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return HTMLHeadInclusionModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return "Content Manager HTML head inclusion";
    }

    public void testDependencies() throws ModuleDependencyException {
    }

}
