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
package de.innovationgate.wgpublisher.expressions.tmlscript.scopes;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import de.innovationgate.ext.org.mozilla.javascript.NativeJavaClass;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.wga.server.api.GlobalExpressionScope;

public class TMLScriptRootScopeData implements Scriptable, GlobalExpressionScope {
    
    private RhinoScope _rhinoScope;

    public TMLScriptRootScopeData(RhinoScope rhinoScope) {
        _rhinoScope = rhinoScope;
    }
    
    private Map _globalScopeObjects = new HashMap(); 

    public void delete(String name) {
        _globalScopeObjects.remove(name.toLowerCase());
    }

    public void delete(int index) {
    }

    public Object get(String name, Scriptable start) {
        if (hasScopeObject(name)) {
            return getScopeObject(name);
        }
        else {
            return Scriptable.NOT_FOUND;
        }
    }

    private boolean hasScopeObject(String name) {
        return _globalScopeObjects.containsKey(name.toLowerCase());
    }

    public Object get(int index, Scriptable start) {
        return null;
    }

    public String getClassName() {
        return getClass().getName();
    }

    public Object getDefaultValue(Class hint) {
        return _rhinoScope.getDefaultValue(hint);
    }

    public Object[] getIds() {
        Set keys = _globalScopeObjects.keySet();
        return keys.toArray(new Object[keys.size()]);
    }

    public Scriptable getParentScope() {
        return _rhinoScope;
    }

    public Scriptable getPrototype() {
        return null;
    }

    public boolean has(String name, Scriptable start) {
        return _globalScopeObjects.containsKey(name.toLowerCase());
    }

    public boolean has(int index, Scriptable start) {
        return false;
    }

    public boolean hasInstance(Scriptable instance) {
        if (instance instanceof NativeJavaClass) {
            NativeJavaClass javaClass = (NativeJavaClass) instance;
            return javaClass.getClassObject().isAssignableFrom(TMLScriptRootScopeData.class);
        }
        return false;
    }

    public void put(String name, Scriptable start, Object value) {
        putScopeObject(name, value);
    }

    public void put(int index, Scriptable start, Object value) {
    }

    public void setParentScope(Scriptable parent) {
    }

    public void setPrototype(Scriptable prototype) {
    }

    public RhinoScope getRhinoScope() {
        return _rhinoScope;
    }

    public void putScopeObject(String name, Object value) {
        _globalScopeObjects.put(name.toLowerCase(), value);        
    }

    public Map getGlobalScopeObjects() {
        return _globalScopeObjects;
    }

    public Object getScopeObject(String name) {
        return _globalScopeObjects.get(name.toLowerCase());
    }

    public Map getUnwrappedGlobalScopeObjects() {
        Iterator objects = _globalScopeObjects.entrySet().iterator();
        Map unwrapped = new HashMap();
        while (objects.hasNext()) {
            Map.Entry entry = (Map.Entry) objects.next();
            Object value = entry.getValue();
            if (value instanceof NativeJavaObject) {
                value = ((NativeJavaObject) value).unwrap();
            }
            unwrapped.put(entry.getKey(), value);
                        
        }
        return unwrapped;
    }

    public Map<String, Object> getObjects() {
        return getUnwrappedGlobalScopeObjects();
    }


}
