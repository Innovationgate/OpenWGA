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

package de.innovationgate.wgpublisher.expressions.tmlscript;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.ManagedGlobalConfig;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public class ManagedTMLScriptGlobalDefinition {
    
    private DesignResourceReference _designReference;
    private ManagedGlobalConfig _config;
   
    public ManagedTMLScriptGlobalDefinition(DesignResourceReference designReference, ManagedGlobalConfig config) {
        _designReference = designReference;
        _config = config;
    }

    public ManagedGlobalConfig getConfig() {
        return _config;
    }

    public DesignResourceReference getDesignReference() {
        return _designReference;
    }
    
    public ScopeObject provideGlobal(WGA wga, boolean create) throws WGException {
        return provideGlobal(wga, getConfig().getScope().resolve(wga, getDesignReference()), create);
    }
    
    public ScopeObject provideGlobal(WGA wga, ScopeObjectRegistry reg, boolean create) throws WGException {
        
        if (reg == null) {
            return null;
        }
        
        return reg.getOrCreateScopeObject(wga, getDesignReference(), _config.isIsolated(), create);
    }

}
