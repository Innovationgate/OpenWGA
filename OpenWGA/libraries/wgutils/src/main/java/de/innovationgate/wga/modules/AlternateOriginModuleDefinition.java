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

package de.innovationgate.wga.modules;

import de.innovationgate.utils.DynamicClassLoadingChain.SubLoader;

/**
 * A module definition whose class loader origin is determined explictly.
 * The ModuleRegistry normally determines the origin of a ModuleDefinition from its classloader. That way it can remove this definition once the loader is removed.
 * If module of the definition however depends on an alternate class loader it should implement this interface and give the key of the loader (Normally the key of a {@link SubLoader}.
 */
public interface AlternateOriginModuleDefinition extends ModuleDefinition {
    
    public String getOriginKey();

}
