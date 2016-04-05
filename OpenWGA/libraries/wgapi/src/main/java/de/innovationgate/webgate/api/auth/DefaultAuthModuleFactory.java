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

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.types.AuthenticationSourceModuleType;

/**
 * Default implementation of an auth module factory.
 * The auth module factory creates authentication modules based on auth module names.
 */
public class DefaultAuthModuleFactory implements AuthModuleFactory {
	
    /**
	 * System property prefix by which Auth module implementations can be mapped to a short name, usable in "auth.module".
	 * The property name is to be completed by the short name. The property value must be the implementation class name
	 */
	public static final String SYSPROPERTY_AUTH_MAPPING = "de.innovationgate.wga.auth.module.";
	





    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthModuleFactory#createAuthModule(java.lang.String)
     */
	protected AuthenticationModule createAuthModule(String type) throws ConfigurationException {
		
	    try {
            Class<?> moduleClass = WGFactory.getImplementationLoader().loadClass(type);
            if (!AuthenticationModule.class.isAssignableFrom(moduleClass)) {
                return null;
            }
            
            if (WGFactory.getModuleRegistry() != null) {
                return (AuthenticationModule) WGFactory.getModuleRegistry().instantiate(moduleClass);
            }
            else {
                return (AuthenticationModule) moduleClass.newInstance();
            }
	    }
        catch (Exception e) {
            throw new ConfigurationException("Exception instantiating auth module " + type, e);
        }
	    
		
	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.AuthModuleFactory#getAuthModule(java.lang.String, java.util.Map, de.innovationgate.webgate.api.WGDatabase)
     */
    public synchronized AuthenticationModule getAuthModule(String type, Map<String,String> settings, WGDatabase db) throws ConfigurationException {
        
        if (type == null) {
            throw new IllegalArgumentException("Parameter type is missing for fetching auth module");
        }
        
        // Create a new module and initialize it.
        AuthenticationModule module = createAuthModule(type);
        if (module == null) {
            throw new ConfigurationException("Unknown auth modúle type: " + type);
        }
        
        module.init(settings, db);
        
        return module;
        
    }

    public void clearCache() {
    }




    


}
