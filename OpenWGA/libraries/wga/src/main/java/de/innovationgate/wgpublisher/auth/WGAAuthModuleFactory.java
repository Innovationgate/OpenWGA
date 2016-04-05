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

package de.innovationgate.wgpublisher.auth;

import java.util.List;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.auth.AuthModuleFactory;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.ConfigurationException;
import de.innovationgate.webgate.api.auth.DefaultAuthModuleFactory;
import de.innovationgate.wgpublisher.WGACore;

public class WGAAuthModuleFactory extends DefaultAuthModuleFactory {
    
    public static final String AUTHMODULE_CS = "cs";
    public static final String AUTHMODULE_DELEGATE = "delegate";
    public static final String AUTHMODULE_PLUGIN = "plugin";

    private WGACore _core;

    public WGAAuthModuleFactory(WGACore core) {
        _core = core;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.auth.DefaultAuthModuleFactory#createAuthModule(java.lang.String)
     */
    protected AuthenticationModule createAuthModule(String type) throws ConfigurationException {
        AuthenticationModule module = null;
        
        // First try. Create modules hosted by this module factory
        if (type.equals((AUTHMODULE_CS))) {
            module = new CSAuthModule();
        }
        else if (type.equals(AUTHMODULE_DELEGATE)) {
            module = new DelegatingAuthModule();
        }
        /*
        else if (type.equals(AUTHMODULE_DOMINO)) {
            module = new DominoAuthenticationModule();
        }*/
        
        // Second try. Create modules hosted by the parent factory
        if (module == null) {
            module = super.createAuthModule(type);
        }
        
        // Third try. Take type as class name and try to instantiate.
        if (module == null) {
            try {
                Class<?> moduleClass = WGACore.getLibraryLoader().loadClass(type);
                module = (AuthenticationModule) _core.getModuleRegistry().instantiate(moduleClass);
            }
            catch (Exception e) {
            }
        }
        
        
        // If it is core-aware, put the core
        if (module instanceof CoreAwareAuthModule) {
            ((CoreAwareAuthModule) module).setCore(_core);
        }
        
        return module;
    }

}
