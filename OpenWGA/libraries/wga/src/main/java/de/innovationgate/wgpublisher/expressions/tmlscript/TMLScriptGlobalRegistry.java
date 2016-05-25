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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wgpublisher.WGACore;

public class TMLScriptGlobalRegistry {
    
    private final WGACore _wgaCore;

    /**
     * @param wgaCore
     */
    public TMLScriptGlobalRegistry(WGACore wgaCore) {
        _wgaCore = wgaCore;
    }

    private Map<String, TMLScriptGlobal> _tmlscriptGlobals = new ConcurrentHashMap<String, TMLScriptGlobal>();
    
    public List<String> getGlobalNames(WGDatabase db) {
        
        List<String> names = new ArrayList<String>();
        names.addAll(_tmlscriptGlobals.keySet());
        if (db != null) {
            names.addAll(getAppGlobalRegistry(db).getGlobalNames());
        }
        return names;
        
    }
    
    public boolean registerGlobal(TMLScriptGlobal scriptGlobal) {
        _wgaCore.getLog().info("Registering TMLScript Global \"" + scriptGlobal.getName() + "\"");
        TMLScriptGlobal previousGlobal = _tmlscriptGlobals.get(scriptGlobal.getName());
        _tmlscriptGlobals.put(scriptGlobal.getName(), scriptGlobal);
        return true;
    }
    
    public boolean unregisterGlobal(String globalName) {
        _tmlscriptGlobals.remove(globalName);
        return true;
    }
    
    public boolean registerAppGlobal(TMLScriptGlobal scriptGlobal, WGDatabase db) {
        _wgaCore.getLog().info("Registering TMLScript App Global \"" + scriptGlobal.getName() + "\" for database " + db.getDbReference());
        TMLScriptAppGlobalRegistry globals = getAppGlobalRegistry(db);
        TMLScriptGlobal previousGlobal = globals.getGlobal(scriptGlobal.getName());
        globals.registerGlobal(scriptGlobal.getName(), scriptGlobal);
        return true;
    }

    public TMLScriptAppGlobalRegistry getAppGlobalRegistry(WGDatabase db) {
        return (TMLScriptAppGlobalRegistry) db.getAttribute(WGACore.DBATTRIB_DBGlOBALS);
    }
    
    public TMLScriptGlobal getGlobal(String name,  WGDatabase db) {
        
        if (db != null) {
            TMLScriptAppGlobalRegistry globals = getAppGlobalRegistry(db);
            TMLScriptGlobal global = globals.getGlobal(name);
            if (global != null) {
                return global;
            }
        }
        
        return _tmlscriptGlobals.get(name);
    }
    
    public boolean isGlobalDefined(String name, WGDatabase db) {
        
        if (db != null) {
            TMLScriptAppGlobalRegistry globals = getAppGlobalRegistry(db);
            if (globals.getGlobal(name) != null) {
                return true;
            }
        }
        
        return _tmlscriptGlobals.containsKey(name);
    }

    
}