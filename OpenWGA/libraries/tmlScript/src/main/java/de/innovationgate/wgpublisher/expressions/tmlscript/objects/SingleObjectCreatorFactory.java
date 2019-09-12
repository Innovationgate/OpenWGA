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

package de.innovationgate.wgpublisher.expressions.tmlscript.objects;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.Script;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator.IncludedDesign;
import de.innovationgate.wgpublisher.expressions.tmlscript.FunctionArgumentSubstitutor;
import de.innovationgate.wgpublisher.expressions.tmlscript.MethodArgumentsBuilder.MethodArguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.ThreadLocalPreserver;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;

public class SingleObjectCreatorFactory implements ObjectCreatorFactory {
    
    private boolean _allowConstructorExtraction = false;
    
    public SingleObjectCreatorFactory(boolean allowConstructorExtraction) {
        _allowConstructorExtraction = allowConstructorExtraction;
    }

    @Override
    public String getObjectCode(TMLAction objectDefinition) {
        
        if (_allowConstructorExtraction && objectDefinition.getObjectName() != null) {
            try {
                //return "function _tmlfunction() {" + objectDefinition.getCode() +  "\nif (typeof(" + objectDefinition.getObjectName() + ")=='function') this.__constructor__=" + objectDefinition.getObjectName() + ";\n} _tmlfunction;";
            	return "(function(){" + objectDefinition.getCode() +  "\nif(typeof(" + objectDefinition.getObjectName() + ")=='function') this.__constructor__=" + objectDefinition.getObjectName() + "})";
            }
            catch (IllegalArgumentException e) { // In case the name is not suitable for an expected Object Name
            }
        }

        //return "function _tmlfunction() {" + objectDefinition.getCode() + "\n} _tmlfunction;";
        return "(function(){" + objectDefinition.getCode() + "})";

    }
   
        
    
    @Override
    public ObjectCreator constructObjectCreator(RhinoContext cx, final TMLAction objectDefinition, final Script script, TMLScriptObjectParentScope parentScope) throws WGException, IOException, ClassNotFoundException {
        
        // Execute the script
        final Function thisConstructor = (Function) script.exec(cx, parentScope);
        final List<DesignLocator.IncludedDesign> includedDesigns = new ArrayList<>();
        
        // TMLScript object 2.0 with exported constructor allowed?
        if (_allowConstructorExtraction) { 

            // We need to construct the object from this function once to see if it exports a constructor or if it "is" the object to construct.
            Scriptable obj;
            
            ThreadLocalPreserver preserver = new ThreadLocalPreserver(cx);
            
            preserver.preserve(DesignLocator.TL_DESIGNTRACINGMODE, includedDesigns);
            try {
                obj = constructObjectFromFunction(cx, objectDefinition, parentScope, new Object[]{}, thisConstructor);
            }
            finally {
                preserver.restore();
            }

            // If the object exports a TMLScript object 2.0 constructor, we use that function to construct the object 
            if (obj.has("__constructor__", obj)) {
                final Function exportedConstructor = (Function) obj.get("__constructor__", obj);
                final TMLScriptObjectMetadata objectMd = RhinoExpressionEngineImpl.get().getObjectMetadataParser().parseTMLScriptObjectMetaData(parentScope.getObjectDefinition(), exportedConstructor);
                parentScope.setObjectMetaData(objectMd);
                
                return new ObjectCreator() {
                    @Override
                    public Scriptable createManagedObject(RhinoContext cx, TMLScriptObjectParentScope parentScope, WGA wga, FunctionArgumentSubstitutor substitutor, Map<String, Object> namedArgs, Object[] unnamedArgs) throws WGAPIException, WGException, IOException, ClassNotFoundException {
                        return callExportedConstructor(cx, exportedConstructor, parentScope, wga, substitutor, unnamedArgs);
                    }
                    
                    @Override
                    public Function getConstructor() {
                        return exportedConstructor;
                    }
                    
                    @Override
                    public List<IncludedDesign> getIncludedDesigns() {
                        return includedDesigns;
                    }
                    
                    @Override
                    public TMLScriptObjectMetadata getMetaData() {
                        return objectMd;
                    }
                };
            }
        }
        
        // Otherwise we use the script function itself for construction and call the init() method (TMLScript object 1.0)
        return new ObjectCreator() {
            @Override
            public Scriptable createManagedObject(RhinoContext cx, TMLScriptObjectParentScope parentScope, WGA wga, FunctionArgumentSubstitutor substitutor, Map<String, Object> namedArgs, Object[] unnamedArgs) throws WGAPIException,
                    WGException, IOException, ClassNotFoundException {
                return constructLegacyObject(cx, objectDefinition, parentScope, wga, thisConstructor, unnamedArgs);
            }

            @Override
            public Function getConstructor() {
                return null;
            }
            
            @Override
            public List<IncludedDesign> getIncludedDesigns() {
                return includedDesigns;
            }
            
            @Override
            public TMLScriptObjectMetadata getMetaData() {
                return new TMLScriptObjectMetadata();
            }
            
        };
        
        
        
    }

    private Scriptable constructObjectFromFunction(RhinoContext cx, TMLAction objectDefinition, Scriptable parentScope, Object[] unnamedArgs, Function thisConstructor) throws WGException, WGAPIException, IOException, ClassNotFoundException {
        
        // Construct the "this" object
        Scriptable obj = thisConstructor.construct(cx, parentScope, new Object[] {});
        obj.put(RhinoExpressionEngine.PARAM_ACTIONDEFINITION, obj, objectDefinition);
        
        return obj;
    
    
        
    }

    private Scriptable callExportedConstructor(Context cx, Function exportedConstructor, Scriptable parentScope, WGA wga, FunctionArgumentSubstitutor substitutor,
            Object[] unnamedArgs) throws WGException, WGAPIException, IOException, ClassNotFoundException {
        try (MethodArguments cArgs = RhinoExpressionEngineImpl.get().getArgumentBuilder().buildArguments(exportedConstructor, wga, substitutor, new HashMap<String, Object>(), unnamedArgs)) {
            return exportedConstructor.construct(cx, parentScope, cArgs.getArguments(parentScope));
        }
    }
    
    public Scriptable constructLegacyObject(RhinoContext cx, TMLAction objectDefinition, Scriptable scope, WGA wga, Function func, Object[] args) throws WGAPIException, ClassNotFoundException, WGException, IOException  {
        Scriptable obj = constructObjectFromFunction(cx, objectDefinition, scope, new Object[]{}, func);
        callInitMethod(cx, scope, wga, null, args, obj);
        return obj;
    }

    private void callInitMethod(RhinoContext cx, Scriptable parentScope, WGA wga, FunctionArgumentSubstitutor substitutor, Object[] unnamedArgs, Scriptable obj)
            throws WGException, WGAPIException, IOException, ClassNotFoundException {
        // Execute "init" constructor
        Object initObj = obj.get("init", obj);
        if (initObj != null && initObj instanceof Function) {
            Function init = (Function) initObj;
            try (MethodArguments initArgs = RhinoExpressionEngineImpl.get().getArgumentBuilder().buildArguments(init, wga, substitutor, new HashMap<String, Object>(), unnamedArgs)) {
                init.call(cx, parentScope, obj, initArgs.getArguments(obj));
            }
        }
    }
    
}
