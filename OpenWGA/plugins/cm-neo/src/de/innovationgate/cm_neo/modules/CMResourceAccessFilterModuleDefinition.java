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

package de.innovationgate.cm_neo.modules;

import java.util.Collections;
import java.util.Locale;

import de.innovationgate.cm_neo.filter.ResourceAccessFilter;
import de.innovationgate.wga.config.FilterMapping;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.FilterConfigModuleType;

public class CMResourceAccessFilterModuleDefinition implements ModuleDefinition {

    public String getDescription(Locale locale) {
        return "A request filter handling authorized access to CM-Neo TML modules";
    }

    public Class<? extends Object> getImplementationClass() {
        return getClass();
    }

    public Class<? extends ModuleType> getModuleType() {
        return FilterConfigModuleType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Object getProperties() {
        FilterMapping mapping = new FilterMapping();
        mapping.setImplClassName(ResourceAccessFilter.class.getName());
        mapping.setName("CM-Neo Resource Access Filter");
        mapping.setUrlPatterns(Collections.singletonList("/plugin-cm-neo/json/*"));
        return mapping;
    }

    public String getTitle(Locale locale) {
        return "CM-Neo Resource Access Filter";
    }

    public void testDependencies() throws ModuleDependencyException {
    }

}
