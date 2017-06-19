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

package de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.PropertyResourceBundle;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.WrappedException;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.Plugin;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.FormInfo;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.tmlscript.CachedScript;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.ThreadLocalPreserver;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser.Arguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.objects.SingleObjectCreatorFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptLegacyObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptRootScope;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@CodeCompletion(delegate=de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.cc.Design.class)
public class Design extends ScriptableObject implements Wrapper {

    private static final long serialVersionUID = 1L;
    public static VarArgParser _getGlobalArgs;
    private static VarArgParser _registerGlobalVarargs;
    private static VarArgParser _managedGlobalVarargs;
    private static VarArgParser _getLabelBundleVarargs;
    protected static VarArgParser _loadObjectDefVarags;
    
    static {
        
        _getGlobalArgs = new VarArgParser("getGlobal")
            .add("global", String.class)
            .pack();
        
        _registerGlobalVarargs = (new VarArgParser("registerGlobal"))
            .add("name", String.class, false)
            .add("ref", Object.class, false)
            .pack();
        
        _managedGlobalVarargs = (new VarArgParser("managedGlobal"))
                .add("name", String.class, false)
                .add("scope", ObjectScope.class, false)
                .add("ref", new Class[] {Object.class, Function.class}, true)
                .pack();
        
        _getLabelBundleVarargs = (new VarArgParser("getLabelBundle"))
            .add("container", String.class, true)
            .add("file", String.class, true)
            .add("language", String.class, false)
            .pack();
        
        _loadObjectDefVarags = VarArgParser.create("loadObjectDefinition")
                .add("id", String.class, true)
                .pack();
        
    }
    
    public static final String[] METHODS = {
        "callAction",
        "createFormInfo",
        "createObject",
        "db",
        "getGlobal",
        "getLabelBundle",
        "isCustomized",
        "label",
        "loadObjectDefinition",
        "plugin",
        "registerDbGlobal",
        "registerGlobal",
        "unregisterGlobal"
    };
    
    public static final String[] PROPERTIES = {
        "$",
        "Object"
    };

    private de.innovationgate.wga.server.api.Design _apiDesign;
    private NativeObject _currentObject = null;
    private boolean _isCurrentScriptDesign = false;

    public Design(Object ref, Scriptable scope) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
        
        defineFunctionProperties(METHODS, Design.class, DONTENUM);
        for (String prop : PROPERTIES) {
            defineProperty(prop, null, getClass().getMethod("jsGet_" + prop, new Class[] {ScriptableObject.class}), null, DONTENUM | READONLY);
        }
        
        if (ref instanceof Wrapper) {
            ref = ((Wrapper) ref).unwrap();
        }
        
        TMLContext tmlContext = WGAGlobal.fetchInitialContext(Context.getCurrentContext());
        try {
            if (ref == null) {
                DesignResourceReference baseScriptRef = WGAGlobal.getBaseReference(Context.getCurrentContext(), null, tmlContext);  
                _apiDesign = WGA.get(tmlContext).design(tmlContext.getDesignContext().createContextDelegate(null, baseScriptRef.normalize().toString()));
                _isCurrentScriptDesign = true;
            }
            else if (ref instanceof de.innovationgate.wga.server.api.Design) {
                _apiDesign = (de.innovationgate.wga.server.api.Design) ref;
            }
            else if (ref instanceof WGDatabase) {
                _apiDesign = WGA.get(tmlContext).design((WGDatabase) ref);
            }
            else if (ref instanceof String) {
                _apiDesign = WGA.get(tmlContext).design((String) ref);
                
            }
            else if (ref instanceof NativeObject) {
                _currentObject  = (NativeObject) ref;
                TMLAction currentAction = WGAGlobal.getObjectDefinition(_currentObject);
                if (currentAction == null) {
                    throw new IllegalArgumentException("Cannot determine design of parameter object");
                }

                String dbKey = currentAction.getModuleDatabase();
                WGDatabase designDB = tmlContext.db(dbKey);
                if (designDB == null || !designDB.isSessionOpen()) {
                    throw new IllegalArgumentException("Cannot determine design of " + dbKey);
                }
                _apiDesign = WGA.get(tmlContext).design(tmlContext.getDesignContext().createContextDelegate((WGDatabase) designDB, currentAction.getModuleName()));
             
            }
            else {
                throw new IllegalArgumentException("Invalid parameter object type " + ref.getClass().getName());            
            }
            
            setPrototype(new NativeJavaObject(scope, _apiDesign, de.innovationgate.wga.server.api.Design.class));
            setParentScope(scope);
            put("baseReference", this, _apiDesign.getBaseReference().getResourceName());
            put("HDBModel", this, _apiDesign.getHdbModel());
            sealObject();
            
        }
        catch (WGAServerException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }
    
    public static Object getGlobal(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException {

        Design design = unwrapThisObj(thisObj);
        
        Arguments arguments = _getGlobalArgs.parse(args);
        
        TMLContext context = WGAGlobal.fetchInitialContext(cx);
        RhinoExpressionEngineImpl runtime = WGAGlobal.fetchRuntime(cx);
        
        TMLScriptGlobal global = context.getwgacore().getTmlscriptGlobalRegistry().getGlobal((String) arguments.get("global"), design._apiDesign.db());  
        if (global != null) {
            return runtime.provideGlobal(WGAGlobal.fetchRootScope(cx).getWgaGlobal().getWga(), global);
        }
        else {
            return null;
        }
                
    }

    public static String label(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException {
        
        Design design = unwrapThisObj(thisObj);
        Arguments parsedArgs = WGAGlobal._localLabelVarargs.parse(args);
        
        return design._apiDesign.label((String) parsedArgs.get("container"), (String) parsedArgs.get("file"), (String) parsedArgs.get("key"), (List) parsedArgs.get("params")); 
        
    }
    
    public static PropertyResourceBundle getLabelBundle(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, IOException {
        
        TMLContext tmlContext = WGAGlobal.fetchInitialContext(cx);
        Design design = (Design) thisObj;
        
        Arguments varArgs = _getLabelBundleVarargs.parse(args);
                
        String container = (String) varArgs.get("container");
        String file = (String) varArgs.get("file");
        String language = (String) varArgs.get("language");
        
        return design._apiDesign.getLabelBundle(tmlContext, container, file, language);
        
    }
    
    public static void registerGlobal(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        VarArgParser.Arguments parsedArgs = _registerGlobalVarargs.parse(args);
        TMLContext context = WGAGlobal.fetchInitialContext(cx);
        
        String name = (String) parsedArgs.get("name");
        Object ref = parsedArgs.get("ref");
        
        // Register name against package or class name
        Design design = (Design) thisObj;
        design._apiDesign.registerGlobal(context, name, ref);
        
        
    }
    
    public static void unregisterGlobal(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        String globalName = String.valueOf(args[0]);
        Design design = (Design) thisObj;
        design._apiDesign.unregisterGlobal(WGAGlobal.fetchInitialContext(cx), globalName);
    }
    
    public static void registerDbGlobal(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        Design design = unwrapThisObj(thisObj);
        VarArgParser.Arguments parsedArgs = _registerGlobalVarargs.parse(args);
        TMLContext context = WGAGlobal.fetchInitialContext(cx);
        
        String name = (String) parsedArgs.get("name");
        Object ref = parsedArgs.get("ref");
        
        
        // Register name against package or class name
        design._apiDesign.registerDbGlobal(context, name, ref);
        
        
    }

    public static WGDatabase db(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException {
        Design design = unwrapThisObj(thisObj);
        return design._apiDesign.db();
    }

    private static Design unwrapThisObj(Scriptable thisObj) {
        return (Design) thisObj;
    }
    
    public static Plugin plugin(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException {
        
        Design design = unwrapThisObj(thisObj);
        return design._apiDesign.plugin();
       
    }
    
    public static Object callAction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {

        Design design = unwrapThisObj(thisObj);
        
        // For the current design redirect to WGAGlobal.callAction() which uses all location info from the current design
        if (design._isCurrentScriptDesign) {
            return WGAGlobal.callAction(cx, WGAGlobal.fetchRootScope(cx).getWgaGlobal(), args, funObj);
        }
        
        VarArgParser.Arguments parsedArgs = WGAGlobal._callActionVarargs.parse(args);

        // Determine action id and action context
        TMLContext context = (TMLContext) parsedArgs.get("context");
        if (context == null) {
            context = WGAGlobal.fetchInitialContext(cx);
        }

        String actionID = (String) parsedArgs.get("id");
        List actionArgs = new ArrayList(parsedArgs.getOverflowArgs());

        DesignResourceReference baseReference = null;
        if (design._currentObject != null) {
            baseReference = WGAGlobal.getBaseReference(cx, design._currentObject, context);
        }

        // Call action
        Object actionResult;
        try {
            TMLScriptRootScope rootScope = WGAGlobal.fetchRootScope(cx);
            actionResult = design._apiDesign.callAction(context, actionID, actionArgs, baseReference, rootScope.getData());
        }
        catch (Exception e) {
            // Look if we find an TMLScript exception somewhere down. If so throw its error value.
            TMLScriptException tmlscriptEx = WGUtils.getRootCause(e, TMLScriptException.class);
            if (tmlscriptEx != null) {
                throw Context.throwAsScriptRuntimeEx(tmlscriptEx.getJsException()); 
            }
            throw Context.throwAsScriptRuntimeEx(e);
        }

        return Context.javaToJS(actionResult, thisObj);

    }
    
    public static Scriptable createObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, IOException, ClassNotFoundException {
        
        Design design = unwrapThisObj(thisObj);
        VarArgParser.Arguments parsedArgs = WGAGlobal._createObjectVarargs.parse(args);

        // Retrieve object definition, either as script or as Function object
        Object definition = parsedArgs.get("definition");
        Function func;
        TMLContext context = WGAGlobal.fetchInitialContext(cx);

        TMLScriptRootScope rootScope = WGAGlobal.fetchRootScope(cx);
        RhinoExpressionEngineImpl runtime = rootScope.getData().getRhinoScope().getRuntime();
        
        // Legacy function to create an object from Function. Incompatible with more modern types of definition.
        if (definition instanceof Function) {
            func = (Function) definition;
            TMLAction objectDefinition = (TMLAction) func.get(RhinoExpressionEngine.PARAM_ACTIONDEFINITION, func);
            Scriptable scope = new TMLScriptLegacyObjectParentScope(context.getwgacore(), objectDefinition, context.getDesignDBKey(), context.designdb().getSessionContext().isMasterSession());
            return new SingleObjectCreatorFactory(false).constructLegacyObject((RhinoContext) cx, objectDefinition, scope, rootScope.getWgaGlobal().getWga(), func,  parsedArgs.getOverflowArgs().toArray());
        }
        
        // Redirecting to standard object definition functionality.
        else {
            TMLAction objectDefinition = loadObjectDefinitionAction(design, WGAGlobal.fetchInitialContext(cx), (String) definition);
            if(objectDefinition==null){
            	throw(new WGException("Unable to load Script module '" + (String) definition + "'"));
            }
            TMLScriptObjectParentScope scope = new TMLScriptLegacyObjectParentScope(context.getwgacore(), objectDefinition, context.getDesignDBKey(), context.designdb().getSessionContext().isMasterSession());
            return runtime.getObjectCreatorManager().getObjectCreator(WGA.get(context), (RhinoContext) cx, objectDefinition, scope).createManagedObject((RhinoContext) cx, scope, rootScope.getWgaGlobal().getWga(), null, null, parsedArgs.getOverflowArgs().toArray());
        }

        

    }

    public static Function loadObjectDefinition(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        Design design = unwrapThisObj(thisObj);
        VarArgParser.Arguments parsedArgs = WGAGlobal._loadObjectDefVarags.parse(args);

        TMLContext context = WGAGlobal.fetchInitialContext(cx);
        String actionID = null;
        if (parsedArgs.has("id")) {
            actionID = (String) parsedArgs.get("id");
        }
        
        TMLAction action = loadObjectDefinitionAction(design, context, actionID);
        
        if (action == null) {
            throw new EvaluatorException("Could not retrieve TMLScript module '" + args[0] + "'");
        }
        
        // Fetch runtime and return function object
        RhinoExpressionEngineImpl runtime = WGAGlobal.fetchRuntime(cx);
        
        // Preserve thread locals
        ThreadLocalPreserver preserver = new ThreadLocalPreserver((RhinoContext) cx);
        preserver.preserve(RhinoExpressionEngine.TL_ACTIONDEFINITION, action);
        String scriptName = "TMLScript-Object " + action.getModuleDatabase() + "/" + action.getModuleName();
        preserver.preserve(RhinoExpressionEngine.TL_SCRIPTNAME, scriptName);;
        try {
            TMLScriptLegacyObjectParentScope redirector = new TMLScriptLegacyObjectParentScope(context.getwgacore(), action, context.getDesignDBKey(), context.designdb().getSessionContext().isMasterSession());
            CachedScript script = runtime.getCompiledScript(new SingleObjectCreatorFactory(false).getObjectCode(action), (RhinoContext) cx, scriptName, WGAGlobal.fetchRootScope(cx));
            Function constructorFunction = (Function) script.getScript().exec(cx, redirector);
            constructorFunction.put(RhinoExpressionEngine.PARAM_ACTIONDEFINITION, constructorFunction, action);
            return constructorFunction;
        }
        catch (Exception e) {
            throw new WrappedException(e);
        }
        finally {
            preserver.restore();
        }
        
        
    }

    private static TMLAction loadObjectDefinitionAction(Design design, TMLContext context, String actionID) {
        // Get the function def (might need to expand local name by the name of
        // the current action)
        // Locate object definition
        TMLAction action = null;
        try {
            
            DesignResourceReference actionRef;
            if (actionID != null) {
                actionRef = design._apiDesign.resolveReference(new DesignResourceReference(null, actionID));
            }
            else {
                actionRef = design._apiDesign.getBaseReference();
            }
            action = context.getModuleActionByID(actionRef.getResourceName(), actionRef.getDesignApp());
            
        }
        catch (Exception e) {
            throw new WrappedException(e);
        }
        return action;
    }
    
    public static boolean isCustomized(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        Design design = unwrapThisObj(thisObj);
        return design._apiDesign.isCustomized();
        
    }
    
    public static FormInfo createFormInfo(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        Design design = unwrapThisObj(thisObj);
        
        if (args.length != 1) {
            throw new EvaluatorException("Method WGA.createFormInfo() needs a string as parameter");
        }
        
        String id = String.valueOf(args[0]);
        return design._apiDesign.createFormInfo(WGAGlobal.fetchInitialContext(cx), id);
    }

    public de.innovationgate.wga.server.api.Design getApiDesign() {
        return _apiDesign;
    }
    
    @Override
    public Object getDefaultValue(Class<?> typeHint) {
        
        if (typeHint == String.class) {
            return _apiDesign.toString();
        }
        
        return super.getDefaultValue(typeHint);
    }

    @Override
    public String getClassName() {
        return "Design";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_apiDesign == null) ? 0 : _apiDesign.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Design other = (Design) obj;
        if (_apiDesign == null) {
            if (other._apiDesign != null)
                return false;
        }
        else if (!_apiDesign.equals(other._apiDesign))
            return false;
        return true;
    }

    @Override
    public Object unwrap() {
        return _apiDesign;
    }
    
    @Override
    protected Object equivalentValues(Object value) {

        if (value instanceof Design) {
            if (_apiDesign.equals(((Design) value).getApiDesign())) {
                return Boolean.TRUE;
            }
            else {
                return Boolean.FALSE;
            }
        }
    
        return Scriptable.NOT_FOUND;
        
    }

    public static Scriptable jsGet_$(ScriptableObject thisObj) {
        return new DesignLocator(WGAGlobal.fetchWGA(), ((Design) thisObj).getApiDesign(), thisObj);
    }
    
    public static Scriptable jsGet_Object(ScriptableObject thisObj) throws TMLActionException, JavaScriptException, WGAPIException, ClassNotFoundException, WGException, IOException {
        return DesignLocator.getConstructor(WGAGlobal.fetchWGA(),  ((Design) thisObj).getApiDesign());
    }

    /**
     * Transforms anything given as design reference to a method to a TMLScript Design Object
     * @param designObj The design reference, either a Design object itself or anything a design could take as parameter
     * @param scope The scope for the new Design object
     * @throws WGException 
     * @throws SecurityException 
     * @throws NoSuchMethodException 
     * @throws JavaScriptException 
     */
    public static Design toDesign(Object designObj, Scriptable scope) throws JavaScriptException, NoSuchMethodException, SecurityException, WGException {

        if (designObj == null) {
            return null;
        }
        
        if (designObj instanceof Design) {
            return (Design) designObj;
        }
        
        return new Design(designObj, scope);
        
    }

    
    
}