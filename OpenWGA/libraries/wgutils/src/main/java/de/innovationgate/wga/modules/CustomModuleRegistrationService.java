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

import java.io.IOException;
import java.net.URLClassLoader;

/**
 * A service to register modules from other registrar types than {@link ModuleRegistrar}.
 * This service will get called every time the registration runs, other than regular registrars which only run when their own source design is updated.
 */
public interface CustomModuleRegistrationService {
    
    /**
     * Searches modules from the given class loader
     * Used when the module registry is searched completely.
     * @param classLoader The class loader to search registrars and modules from
     * @throws IOException
     */
    public void searchModuleDefinitions(ModuleRegistry registry, ClassLoader classLoader) throws IOException;
    
    /**
     * Searches custom registrar configurations from registrarCfgLoaders and loads classes from classLoader
     * Used when the class loaders of certain module providers are updated.
     * @param classLoader The class loader to load module classes from
     * @param registrarCfgLoaders The class loaders to load custom registrar configurations from
     * @throws IOException
     */
    public void searchModuleDefinitions(ModuleRegistry registry, ClassLoader classLoader, Iterable<URLClassLoader> registrarCfgLoaders) throws IOException;

}
