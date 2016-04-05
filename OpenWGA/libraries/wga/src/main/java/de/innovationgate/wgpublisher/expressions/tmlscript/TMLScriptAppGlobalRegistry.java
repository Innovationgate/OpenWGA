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

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;

import com.google.common.collect.ConcurrentHashMultiset;

import de.innovationgate.wga.server.api.ObjectScope;

public class TMLScriptAppGlobalRegistry {
    
    private Map<String, TMLScriptGlobal> _globals = new ConcurrentHashMap<String, TMLScriptGlobal>();
    private Set<ObjectScope> _usedManagedGlobalScopes = new ConcurrentSkipListSet<>();
    
    public TMLScriptGlobal getGlobal(String name) {
        return _globals.get(name);
    }

    public Collection<? extends String> getGlobalNames() {
        return Collections.unmodifiableSet(_globals.keySet());
    }

    public void registerGlobal(String name, TMLScriptGlobal scriptGlobal) {
        _globals.put(name, scriptGlobal);
        
        Object global = scriptGlobal.getRef();
        if (global instanceof ManagedTMLScriptGlobalDefinition) {
            _usedManagedGlobalScopes.add(((ManagedTMLScriptGlobalDefinition) global).getConfig().getScope());
        }
    }

    public Set<ObjectScope> getUsedManagedGlobalScopes() {
        return _usedManagedGlobalScopes;
    }

}
