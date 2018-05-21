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

import java.io.IOException;
import java.util.List;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.ScriptRuntime;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript.ObjectType;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.tmlscript.objects.ObjectCreator;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptRootScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionException;
import de.innovationgate.wgpublisher.webtml.utils.ObjectStrategy;

public class DesignLocator implements Scriptable, Wrapper {
    
    public static class IncludedDesign {
        
        public IncludedDesign(DesignResourceReference ref, int hashCode) {
            super();
            _ref = ref;
            this.hashCode = hashCode;
        }
        private DesignResourceReference _ref;
        private int hashCode;
        public DesignResourceReference getRef() {
            return _ref;
        }
        public int getHashCode() {
            return hashCode;
        }
        
        
        
    }

    public static final String TL_DESIGNTRACINGMODE = "designTracingMode";
    
    private Design _design;
    private Scriptable _parentScope;

    private WGA _wga;

    private String _baseName;

    private DesignLocator(WGA wga, Design design, Scriptable scope, String baseName) {
        _wga = wga;
        _design = design;
        _parentScope = scope;
        _baseName = baseName;
    }
    
    public DesignLocator(WGA wga, Design design, Scriptable scope) {
        this(wga, design, scope, "");
    }

    @Override
    public String getClassName() {
        return "DesignLocator";
    }

    @Override
    public Object get(String name, Scriptable start) {
        try {
            
            // Isolate the part after a dot (if any)
            int dotPos = name.indexOf(".");
            String nameAfterDot = name;
            if (dotPos != -1) {
                nameAfterDot = name.substring(dotPos + 1);
            }

            int colonPos = name.indexOf(":");
            // Some function
            if (colonPos != -1) {
                String function = name.substring(0, colonPos).trim();
                String param = name.substring(colonPos + 1).trim();
                return executeLocatorFunction(function, param);
            }

            // Retrieving a constructor
            if (Character.isUpperCase(nameAfterDot.charAt(0))) {
                // Retrieving a constructor
                Function c = getConstructor(_baseName + name);
                if (c != null) {
                    return c;
                }
                else {
                	_wga.getLog().warn("DesignLocator: Constructor '" + name + "' not found in " + _design + " file " + name + ".tmlscript");
                    return Scriptable.NOT_FOUND;
                }
            }
            
            // Descending on some reference name
            else {
                if (name.startsWith("@")) {
                    return new DesignLocator(_wga, _design.resolve(name), _parentScope, "");
                }
                else {
                    return new DesignLocator(_wga, _design.resolve(".:" + name), _parentScope, "");
                }
            }
        }
        catch (JavaScriptException e) {
            throw e;
        }
        catch (Exception e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }

    protected Function getConstructor(String name) throws WGException, TMLActionException, WGAPIException, IOException, ClassNotFoundException {
        Design moduleDesign = _design.resolve(".:" + name);
        Function f = getConstructor(_wga, moduleDesign);
        return f;
    }

    public static Function getConstructor(WGA wga, Design moduleDesign) throws WGException, TMLActionException, WGAPIException, IOException, ClassNotFoundException {
        ObjectCreator objectCreator = getObjectCreator(wga, moduleDesign);
        if (objectCreator == null) {
            return null;
        }
        
        Function f = objectCreator.getConstructor();
        if (f != null) {
            @SuppressWarnings("unchecked")
            List<IncludedDesign> includedDesigns = (List<IncludedDesign>) Context.getCurrentContext().getThreadLocal(TL_DESIGNTRACINGMODE);
            if (includedDesigns != null) {
                includedDesigns.add(new IncludedDesign(moduleDesign.getBaseReference(), moduleDesign.getTMLScriptCode().hashCode()));
                includedDesigns.addAll(objectCreator.getIncludedDesigns());
            }
            return f;
        }
        else {
            return null;
        }
    }
    
    public static TMLScriptObjectMetadata getObjectMetaData(WGA wga, Design moduleDesign) throws WGException, TMLActionException, WGAPIException, IOException, ClassNotFoundException {
        ObjectCreator objectCreator = getObjectCreator(wga, moduleDesign);
        if (objectCreator == null) {
            return null;
        }
        
        return objectCreator.getMetaData();
    }

    public static ObjectCreator getObjectCreator(WGA wga, Design moduleDesign) throws WGException, TMLActionException, WGAPIException, IOException, ClassNotFoundException {
        WGScriptModule module = moduleDesign.getTMLScriptModule();
        if (module == null) {
            return null;
        }
        
        TMLAction objectDefinition = TMLAction.buildActionFromScriptModule(module, ObjectStrategy.SINGLE_OR_EXPORTED_CONSTRUCTOR); 
        RhinoContext cx = (RhinoContext) Context.getCurrentContext();
        
        RhinoExpressionEngineImpl engine = RhinoExpressionEngineImpl.get();
        TMLScriptObjectParentScope scope = engine.getParentScopeForObjectType(ObjectType.V2_ISOLATED, objectDefinition);
        ObjectCreator objectCreator = engine.getObjectCreatorManager().getObjectCreator(wga, cx, objectDefinition, scope);
        return objectCreator;
    }

    private Object executeLocatorFunction(String function, String param) throws WGException {

        if ("db".equalsIgnoreCase(function)) {
            return new DesignLocator(_wga, _design.resolve(param, ""), _parentScope, "");
        }
        
        return ScriptableObject.NOT_FOUND;
        
    }

    @Override
    public Object get(int index, Scriptable start) {
        return null;
    }

    @Override
    public boolean has(String name, Scriptable start) {
        try {
            return get(name, start) != ScriptableObject.NOT_FOUND;
        }
        catch (Throwable t) {
            return false;
        }
    }

    @Override
    public boolean has(int index, Scriptable start) {
        return false;
    }

    @Override
    public void put(String name, Scriptable start, Object value) {
        throw ScriptRuntime.throwError(Context.getCurrentContext(), _parentScope, "Not supported");
    }

    @Override
    public void put(int index, Scriptable start, Object value) {
        throw ScriptRuntime.throwError(Context.getCurrentContext(), _parentScope, "Not supported");
        
    }

    @Override
    public void delete(String name) {
        throw ScriptRuntime.throwError(Context.getCurrentContext(), _parentScope, "Not supported");
    }

    @Override
    public void delete(int index) {
        throw ScriptRuntime.throwError(Context.getCurrentContext(), _parentScope, "Not supported");
    }

    @Override
    public Scriptable getPrototype() {
        return null;
    }

    @Override
    public void setPrototype(Scriptable prototype) {
        throw ScriptRuntime.throwError(Context.getCurrentContext(), _parentScope, "Not supported");   
    }

    @Override
    public Scriptable getParentScope() {
        return _parentScope;
    }

    @Override
    public void setParentScope(Scriptable parent) {
        _parentScope = parent;
    }

    @Override
    public Object[] getIds() {
        return new Object[] {};
    }

    @Override
    public Object getDefaultValue(Class<?> hint) {
        if (hint == null || hint == String.class) {
            return _design.getBaseReference().toString();
        }
        else {
            return null;
        }
    }

    @Override
    public boolean hasInstance(Scriptable instance) {
        return false;
    }

    @Override
    public Object unwrap() {
        return _design;
    }

    
}
