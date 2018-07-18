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

package de.innovationgate.wga.server.api;

import java.io.StringReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonStructure;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.ObjectStrategy;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * This object allows custom TMLScript expression and script execution
 * This object differentiates between expressions which just return the evaluated value and scripts which need a return statement to return a value.
 * If a script does not return a value its result is null.
 */
/**
 * 
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public class TMLScript {
    
    public enum ObjectType {
        /**
         * TMLScript Objects V1, implicit access to the WebTML context of the current environment via root scope 
         */
        V1,
        
        /**
         * TMLScript Objects V2, no implicit access, access to full WebTML environment via arguments and WGA object
         */
        V2,
        
        /**
         * TMLScript Objects V2, no implicit access, access to limited WebTML environment via arguments and WGA object, no direct access to optional environment data like request, session etc.
         */
        V2_ISOLATED
    }
    
    private WGA _wga;

    protected TMLScript(WGA wga) {
        _wga = wga;
    }
    

    /**
     * Runs a TMLScript expression
     * Uses the WebTML context of the environment and is therefor only usable in WebTML/TMLScript environments.
     * @param expression The expression
     * @return The expression result
     * @throws WGAServerException
     */
    @CodeCompletion
    public Object runExpression(String expression) throws WGException {
        return run(null, null, expression, null, ExpressionEngine.TYPE_EXPRESSION);
    }

    /**
     * Runs a TMLScript expression
     * Uses the WebTML context of the environment and is therefor only usable in WebTML/TMLScript environments.
     * @param design Base design context to run the expression under
     * @param expression The expression
     * @return The expression result
     * @throws WGAServerException
     */
    public Object runExpression(Design design, String expression) throws WGException {
        return run(design, null, expression, null, ExpressionEngine.TYPE_EXPRESSION);
    }
    
    /**
     * Runs a TMLScript expression
     * @param context WebTML context under which the expression runs and whose design context is used
     * @param expression The expression
     * @return The expression result
     * @throws WGAServerException
     */
    @CodeCompletion
    public Object runExpression(Context context, String expression) throws WGException {
        return run(new Design(_wga, ((TMLContext) context).getDesignContext()), (TMLContext) context, expression, null, ExpressionEngine.TYPE_EXPRESSION);
    }
    
    /**
     * Runs a TMLScript expression
     * @param design Base design context to run the expression for
     * @param cx WebTML context under which the expression runs
     * @param expression The expression
     * @return The expression result
     * @throws WGAServerException
     */
    public Object runExpression(Design design, Context cx, String expression) throws WGException {
        return run(design, (TMLContext) cx, expression, null, ExpressionEngine.TYPE_EXPRESSION);
    }

    /**
     * Runs a TMLScript expression
     * @param design Base design context to run the expression for
     * @param cx WebTML context under which the expression runs
     * @param expression The expression
     * @param extraObjects Objects to available to the expression. Keys are object names, their values the objects.
     * @return The expression result
     * @throws WGAServerException
     */
    public Object runExpression(Design design, Context cx, String expression, Map<String,Object> extraObjects) throws WGException {
        return run(design, (TMLContext) cx, expression, extraObjects, ExpressionEngine.TYPE_EXPRESSION);        
    }

    /**
     * Runs a TMLScript expression
     * @param cx WebTML context under which the expression runs and whose design context is used
     * @param expression The expression
     * @param extraObjects Objects to available to the expression. Keys are object names, their values the objects.
     * @return The expression result
     * @throws WGAServerException
     */
    public Object runExpression(Context cx, String expression, Map<String,Object> extraObjects) throws WGException {
        return run(new Design(_wga, ((TMLContext) cx).getDesignContext()), (TMLContext) cx, expression, extraObjects, ExpressionEngine.TYPE_EXPRESSION);        
    }
    
    /**
     * Runs a TMLScript script
     * Uses the WebTML context of the environment and is therefore only usable in WebTML/TMLScript environments.
     * @param expression The script
     * @return The script return result
     * @throws WGAServerException
     */
    @CodeCompletion
    public Object runScript(String expression) throws WGException {
        return run(null, null, expression, null, ExpressionEngine.TYPE_SCRIPT);
    }
    
    /**
     * Runs a TMLScript script
     * Uses the WebTML context of the environment and is therefore only usable in WebTML/TMLScript environments.
     * @param design Base design context to run the script under
     * @param expression The script
     * @return The script return result
     * @throws WGAServerException
     */
    public Object runScript(Design design, String expression) throws WGException {
        return run(design, null, expression, null, ExpressionEngine.TYPE_SCRIPT);
    }

    /**
     * Runs a TMLScript script
     * @param context WebTML context under which the expression runs and whose design context is used
     * @param expression The script
     * @return The script return result
     * @throws WGAServerException
     */
    @CodeCompletion
    public Object runScript(Context context, String expression) throws WGException {
        return run(new Design(_wga, ((TMLContext) context).getDesignContext()), (TMLContext) context, expression, null, ExpressionEngine.TYPE_SCRIPT);
    }

    /**
     * Runs a TMLScript script
     * @param design Base design context to run the script under
     * @param cx WebTML context under which the expression runs
     * @param expression The script
     * @return The script return result
     * @throws WGAServerException
     */
    public Object runScript(Design design, Context cx, String expression) throws WGException {
        return run(design, (TMLContext) cx, expression, null, ExpressionEngine.TYPE_SCRIPT);
    }

    /**
     * Runs a TMLScript script
     * @param design Base design context to run the script under
     * @param cx WebTML context under which the expression runs
     * @param expression The script
     * @param extraObjects Objects to available to the script. Keys are object names, their values the objects.
     * @return The script return result
     * @throws WGAServerException
     */
    public Object runScript(Design design, Context cx, String expression, Map<String,Object> extraObjects) throws WGException {
        return run(design, (TMLContext) cx, expression, extraObjects, ExpressionEngine.TYPE_SCRIPT);        
    }
    

    /**
     * Runs a TMLScript script
     * @param cx WebTML context under which the expression runs and whose design context is used
     * @param expression The script
     * @param extraObjects Objects to available to the script. Keys are object names, their values the objects.
     * @return The script return result
     * @throws WGAServerException
     */
    public Object runScript(Context cx, String expression, Map<String,Object> extraObjects) throws WGException {
        return run(new Design(_wga, ((TMLContext) cx).getDesignContext()), (TMLContext) cx, expression, extraObjects, ExpressionEngine.TYPE_SCRIPT);        
    }
    
    private Object run(Design design, TMLContext cx, String expression, Map<String,Object> extraObjects, int type) throws WGException {
        
        if (cx == null) {
            cx = (TMLContext) _wga.tmlcontext();
        }
        
        if (design == null) {
            design = _wga.design();
        }
        
        ExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        
        Map<String,Object> objects = new HashMap<String,Object>();
        
        objects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "Custom TMLScript expression");
        objects.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, design.getDesignContext().getBaseReference());
        if (extraObjects != null) {
            objects.putAll(extraObjects);
        }
        
        ExpressionResult result = engine.evaluateExpression(expression, cx, type, extraObjects);
        if (result.isError()) {
            throw result.getException();
        }
        else {
            return result.getResult();
        }
        
    }

    /**
     * Converts JSON data objects created in TMLScript into their Java form
     * @param dataObject The data object created in TMLScript
     * @return The data object. null if the object could not be converted.
     * @throws WGException 
     */
    public JsonStructure importJsonData(Object dataObject) throws WGException {
        
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        int scriptType = engine.determineTMLScriptType(dataObject);
        switch (scriptType) {
            
            case RhinoExpressionEngine.TYPE_SCRIPTABLE:
                return Json.createReader(new StringReader(engine.convertScriptableToJson(dataObject))).read();
                
            default:
                return null;
        }
        
        
    }

    /**
     * Converts TMLScript values to the corresponding Java types, eventually descending into JS arrays/objects to convert them to Lists/Maps.
     * Native java objects are kept as they are. This variant uses default values from {@link DescriptificationConfig}.
     * @param obj The TMLScript value
     * @param expectedType The java type expected to be returned for the top level object. Will throw exception if the converted type does not match it.
     * @return The converted value
     */
    public <T> T descriptify(Object obj, Class<T> expectedType) throws WGException {
        return descriptify(obj, expectedType, new DescriptificationConfig());
    }
    
    /**
     * Converts TMLScript values to the corresponding Java types, eventually descending into JS arrays/objects.
     * Native java objects are kept as they are.
     * @param obj The TMLScript value
     * @param expectedType The java type expected to be returned for the top level object. Will throw exception if the converted type does not match it.
     * @param config Configures descriptification
     * @return The converted value
     */
    public <T> T descriptify(Object obj, Class<T> expectedType, DescriptificationConfig config) throws WGException {
        return ExpressionEngineFactory.getTMLScriptEngine().descriptify(obj, expectedType, config);
    }
    
    /**
     * Converts Java objects to the corresponding Java types.
     * GSON Json elemented are converted to native JS objects.
     * @param obj The object to scriptify
     * @param scope The JS object in whose scope the scriptified objects should live
     * @return The scriptified object
     * @throws WGException
     */
    public Object scriptify(Object obj, Object scope) throws WGException {
        return ExpressionEngineFactory.getTMLScriptEngine().scriptify(obj, scope);
    }
    
    

    /**
     * Converts Json data into the TMLScript form, ready to be injected to a script 
     * @param dataObject The data object created in TMLScript
     * @return The data object. null if the object could not be converted.
     * @throws WGException 
     */
    public Object exportJsonData(JsonStructure dataObject) throws WGException {
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        return engine.convertJsonToScriptable(dataObject.toString());
        
    }
    
    /**
     * Determines if the given object is a native TMLScript object 
     * @param obj The object
     * @throws WGException
     */
    public boolean isNativeObject(Object obj) throws WGException {
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        return (engine.determineTMLScriptType(obj) != RhinoExpressionEngine.TYPE_NOTMLSCRIPT);
    }
    

    /**
     * Creates a WebTML object from a script module
     * @param design The design reference pointing to the script module
     * @param The type of TMLScript object to create
     * @param namedParams Named parameters for the object constructor.
     * @param unnamedParams Unnamed parameters for the object constructor.. Will be inserted on parameters that do not match named or substituted param names.
     * @return A new object
     * @throws WGException
     */
    public Object createObject(Design design, ObjectType objectType, Map<String,Object> namedParams, List<Object> unnamedParams) throws WGException {
        
        WGScriptModule mod = design.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT);
        if (mod == null) {
            throw new WGAServerException("No TMLScript module under reference: " + design.getBaseReference().toString());
        }
            
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        return engine.createObject(_wga, objectType, TMLAction.buildActionFromScriptModule(mod, ObjectStrategy.SINGLE_OR_EXPORTED_CONSTRUCTOR), new WebTMLFunctionArgumentSubstitutor(_wga), namedParams ,unnamedParams);
        
    }
    
    /**
     * Creates a WebTML object from a script module
     * @param design The design reference pointing to the script module
     * @return A new object
     * @throws WGException
     */
    @CodeCompletion
    public Object createObject(Design resolve) throws WGException {
        return createObject(resolve, ObjectType.V1, Collections.<String,Object>emptyMap(), Collections.<Object>emptyList());
    }
    
    /**
     * Creates a WebTML object from a script module
     * @param design The design reference pointing to the script module
     * @return A new object
     * @throws WGException
     */
    public Object createObject(Design resolve, ObjectType objectType) throws WGException {
        return createObject(resolve, objectType, Collections.<String,Object>emptyMap(), Collections.<Object>emptyList());
    }

    /**
     * Creates a WebTML object from a script module
     * @param design The design reference pointing to the script module
     * @param namedParams Named parameters for the object constructor.
     * @return A new object
     * @throws WGException
     */
    public Object createObject(Design resolve, Map<String,Object> namedParams) throws WGException {
        return createObject(resolve, ObjectType.V1, namedParams, Collections.<Object>emptyList());
    }

    /**
     * Creates a WebTML object from a script module
     * @param design The design reference pointing to the script module
     * @param params Unnamed parameters for the object constructor.. Will be inserted on parameters that do not match named or substituted param names.
     * @return A new object
     * @throws WGException
     */
    public Object createObject(Design resolve, List<Object> params) throws WGException {
        return createObject(resolve, ObjectType.V1, Collections.<String,Object>emptyMap(), params);
    }

    /**
     * Calls a method or retrieves a property from  a TMLScript object. 
     * @param object The object
     * @param method The method/property name. Use dots to chain multiple method calls/property retrievals together.
     * @param namedParams Named parameters for method arguments. Assigned to arguments of the same name
     * @param unnamedParams Unnamed (ordinal) parameters for method arguments. Assigned to unnamed regular method arguments.
     * @param config Detail configuration of/information about the method call
     * @return The property or return value
     * @throws WGException
     */
    public Object callMethod(Object object, String method, Map<String,Object> namedParams, List<Object> unnamedParams) throws WGException {
        return callMethod(object, method, namedParams, unnamedParams, new CallMethodConfig());
    }
    
    /**
     * Calls a method or retrieves a property from  a TMLScript object. 
     * @param object The object
     * @param method The method/property name. Use dots to chain multiple method calls/property retrievals together.
     * @param namedParams Named parameters for method arguments. Assigned to arguments of the same name
     * @param unnamedParams Unnamed (ordinal) parameters for method arguments. Assigned to unnamed regular method arguments.
     * @param config Detail configuration of/information about the method call
     * @return The property or return value
     * @throws WGException
     */
    public Object callMethod(Object object, String method, Map<String,Object> namedParams, List<Object> unnamedParams, CallMethodConfig config) throws WGException {
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        ExpressionResult result = engine.callMethod(_wga, object, method, new WebTMLFunctionArgumentSubstitutor(_wga), namedParams, unnamedParams, config);
        if (result.isError()) {
            throw result.getException();   
        }
        else {
            return result.getResult();
        }
    }
    
    /**
     * Calls a method without parameters or retrieves a property from  a TMLScript object 
     * @param object The object
     * @param method The method/property name. Use dots to chain multiple method calls/property retrievals together.
     * @return The property or return value
     * @throws WGException
     */
    @CodeCompletion
    public Object callMethod(Object object, String method) throws WGException {
        return callMethod(object, method, null, null);
    }
    
    /**
     * Returns if the given TMLScript object has a property of the given name
     * @param object The object
     * @param prop The property name
     */
    @CodeCompletion
    public boolean hasProperty(Object object, String prop) {
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        return engine.hasProperty(object, prop);
        
    }


}
