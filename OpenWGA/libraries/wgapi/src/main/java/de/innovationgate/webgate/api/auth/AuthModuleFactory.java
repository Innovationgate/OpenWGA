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
package de.innovationgate.webgate.api.auth;

import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;

/**
 * Interface for a factory creating auth modues
 *
 */
public interface AuthModuleFactory {

    /**
     * Creation option for databases that is used to determine the used auth modude. 
     */
    public static final String COPTION_AUTH_MODULE = "auth.module";

    /**
     * Retrieves an authentication module of the given type and for the given settings.
     * @param type Type of module. Use constants AUTHMODULE_...
     * @param settings The settings for this module
     * @param db The database that this authentication source should be for
     * @throws ConfigurationException 
     */
    public abstract AuthenticationModule getAuthModule(String type, Map<String,String> settings, WGDatabase db) throws ConfigurationException;
    
    /**
     * Should clear all caches of this auth module factory, so all resources are freed
     */
    public void clearCache();


}
