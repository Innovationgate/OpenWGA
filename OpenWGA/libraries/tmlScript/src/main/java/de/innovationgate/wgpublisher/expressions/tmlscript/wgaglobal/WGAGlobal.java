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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.httpclient.HttpClient;
import org.apache.log4j.Logger;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaClass;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.RhinoException;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.WrappedException;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.server.api.Auth;
import de.innovationgate.wga.server.api.Call;
import de.innovationgate.wga.server.api.Html;
import de.innovationgate.wga.server.api.Jobs;
import de.innovationgate.wga.server.api.Lucene;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.Plugin;
import de.innovationgate.wga.server.api.Server;
import de.innovationgate.wga.server.api.Session;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.Validate;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WGAContext;
import de.innovationgate.wga.server.api.tml.FormInfo;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.WGABrand;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.expressions.tmlscript.JavascriptFunctionComparator;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.SynchronizedFunction;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser.Arguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.RhinoScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptRootScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.wrapping.ContextWrapper;
import de.innovationgate.wgpublisher.expressions.tmlscript.wrapping.ListWrapper;
import de.innovationgate.wgpublisher.mail.SmtpMail;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

@CodeCompletion(delegate=de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.cc.WGAGlobal.class)
public class WGAGlobal extends ScriptableObject implements Wrapper {
    
    public static class RedirectingWGAContext implements WGAContext {
        
        private WGAGlobal _wgaGlobal;

        @Override
        public WGACore getCore() {
            return getContext().getCore();
        }

        private WGAContext getContext() {
            return (WGAContext) Context.getCurrentContext().getThreadLocal(RhinoExpressionEngineImpl.TL_WGACONTEXT);
        }

        @Override
        public TMLContext getTMLContext() {
            return getContext().getTMLContext();
        }

        @Override
        public HttpServletRequest getServletRequest() {
            return getContext().getServletRequest();
        }

        @Override
        public HttpServletResponse getServletResponse() {
            return getContext().getServletResponse();
        }
        
        @Override
        public HttpSession getHttpSession() {
            return getContext().getHttpSession();
        }

        @Override
        public Logger getLog() {
            return getContext().getLog();
        }

        public WGAGlobal getWgaGlobal() {
            return _wgaGlobal;
        }

        public void setWgaGlobal(WGAGlobal wgaGlobal) {
            _wgaGlobal = wgaGlobal;
        }
        
        @Override
        public WGAContext narrow() {
            return getContext();
        }
        
        @Override
        public boolean isIsolated() {
            return false;
        }

        @Override
        public javax.websocket.Session getWebsocketSession() {
            return getContext().getWebsocketSession();
        }
        
        
    }
    
    interface LazyProperty {
        public Object call(WGA wga, Scriptable scope);
    }
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    protected static VarArgParser _createObjectVarargs;

    protected static VarArgParser _loadObjectDefVarags;
    
    static VarArgParser _localLabelVarargs;

    protected static VarArgParser _callActionVarargs;

    private static VarArgParser _getLookupKeysVarargs;
    
    private static VarArgParser _buildOptionsVarargs;
    
    private static VarArgParser _designVarargs;
    
    private static VarArgParser _scopedVarargs;
    
    static {

        _callActionVarargs = VarArgParser.create("callAction")
            .add("context", TMLContext.class, true)
            .add("id", String.class)
            .setUnwrapOverflowArgs(false)
            .pack();

        _loadObjectDefVarags = VarArgParser.create("loadObjectDefinition")
            .add("currentObject", NativeObject.class, true)
            .add("id", String.class)
            .pack();

        _createObjectVarargs = VarArgParser.create("createObject")
            .add("currentObject", NativeObject.class, true)
            .add("definition", new Class[] { Function.class, String.class })
            .setUnwrapOverflowArgs(false)
            .pack();

        _getLookupKeysVarargs = VarArgParser.create("getLookupKeys")
            .add("table", Map.class, false)
            .pack();
        
        _localLabelVarargs = VarArgParser.create("localLabel")
            .add("container", String.class, true)
            .add("file", String.class, true)
            .add("key", String.class, false)
            .add("params", List.class, true)
            .pack();
        
        _buildOptionsVarargs = VarArgParser.create("buildOptions")
            .add("contents", Iterable.class, false)
            .add("title", String.class, true)
            .add("emptyTitle", String.class, true)
            .pack();
        
        _designVarargs = VarArgParser.create("design")
            .add("ref", new Class[] { NativeObject.class, WGDatabase.class, String.class, de.innovationgate.wga.server.api.Design.class }, true)
            .pack();
        
        _scopedVarargs = VarArgParser.create("scoped")
            .add("value", String.class, false)
            .add("scope", String.class, true)
            .pack();

    }
    
    public static final String[] METHODS =  {
        "buildOptions",
        "callAction",
        "createDate",
        "createForm",
        "createFormInfo",
        "createHttpClient",
        "createImageScaler",
        "createList",
        "createLookupTable",
        "createMail",
        "createObject",
        "db",
        "deleteDoublets",
        "deserializeObject",
        "design",
        "encode",
        "format",
        "getLookupKeys",
        "hdb",
        "javaObject",
        "loadObjectDefinition",
        "logException",
        "lucene",
        "plugin",
        "redirectTo",
        "scoped",
        "scriptObject",
        "serializeObject",
        "sortList", 
        "synchronizedFunction"
    };
    
    public static final String[] PROPERTIES =  {
        "Auth",
        "Ajax",
        "Base64",
        "Brand",
        "Call",
        "Core",
        "EventScopes",
        "Html",
        "Jobs",
        "Master",
        "TMLPage",
        "TMLScript",
        "Scopes",
        "Server",
        "Session",
        "Utils",
        "Validate",
        "Version",
        "Xml"
    };
    
    public static Html jsGet_Html(ScriptableObject thisObj) {
        return getStaticWga(thisObj).html();
    }
    
    @SuppressWarnings("deprecation")
    public static Auth jsGet_Auth(ScriptableObject thisObj) {
        return getStaticWga(thisObj).auth();
    }
    
    public static TMLPage jsGet_TMLPage(ScriptableObject thisObj) {
        return getStaticWga(thisObj).tmlPage();
    }

    public static TMLScript jsGet_TMLScript(ScriptableObject thisObj) throws WGException {
        return getStaticWga(thisObj).tmlscript();
    }

    public static Server jsGet_Server(ScriptableObject thisObj) {
        return getStaticWga(thisObj).server();
    }
    
    public static Jobs jsGet_Jobs(ScriptableObject thisObj) {
        return getStaticWga(thisObj).jobs();
    }
    
    public static Master jsGet_Master(ScriptableObject thisObj) {
        return new Master(thisObj.getParentScope());
    }
    
    public static Xml jsGet_Xml(ScriptableObject thisObj) {
        return new Xml(getStaticWga(thisObj));
    }
    
    public static Validate jsGet_Validate(ScriptableObject thisObj) {
        return getStaticWga(thisObj).validate();
    }
    
    public static Call jsGet_Call(ScriptableObject thisObj) {
        return getStaticWga(thisObj).call();
    }
    
    public static Session jsGet_Session(ScriptableObject thisObj) {
        return getStaticWga(thisObj).session();
    }
    
    public static WGACore jsGet_Core(ScriptableObject thisObj) {
        return getStaticWga(thisObj).getCore();
    }
    
    public static NativeJavaClass jsGet_Utils(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), WGUtils.class);
    }

    public static NativeJavaClass jsGet_Base64(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), WGA.Base64.class);
    }

    public static NativeJavaClass jsGet_Version(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), WGAVersion.class);
    }
    
    public static NativeJavaClass jsGet_Brand(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), WGABrand.class);
    }
    
    public static NativeJavaClass jsGet_Scopes(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), ObjectScope.class);
    }
    
    public static NativeJavaClass jsGet_EventScopes(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), Event.Scope.class);
    }
    
    public static NativeJavaClass jsGet_Ajax(ScriptableObject thisObj) {
        return new NativeJavaClass(thisObj.getParentScope(), Constants.AjaxMode.class);
    }

    
    
    public static WGAGlobal get(Scriptable thisObj) {
        
        if (thisObj instanceof WGAGlobal) {
            return (WGAGlobal) thisObj;
        }
        
        throw new IllegalArgumentException("WGAGlobal method called with invalid reference object: " + thisObj + " (" + thisObj.getClass().getName() + ")");
    }

    public WGAGlobal(RhinoScope scope) throws SecurityException, NoSuchMethodException {
        super(scope, new NativeJavaObject(scope, WGA.get(new RedirectingWGAContext()), WGA.class) {
            @Override
            public Object unwrap() {
                return ((WGA) javaObject).narrow();
            }
        });
        defineFunctionProperties(METHODS, WGAGlobal.class, DONTENUM);
        for (String prop : PROPERTIES) {
            defineProperty(prop, null, getClass().getMethod("jsGet_" + prop, new Class[] {ScriptableObject.class}), null, DONTENUM | READONLY);
        }
    }
    
    @Override
    public String getClassName() {
        return "WGAGlobal";
    }
    
    public static Scriptable javaObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {

        if (args.length == 0 || args[0] == null) {
            return null;
        }

        Object object = args[0];

        if (object instanceof de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject) {
            object = ((de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject) object).unwrap();
        }

        return new de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject(thisObj, object, object.getClass());

    }

    
    public static Scriptable synchronizedFunction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {

        if (args.length == 0) {
            throw Context.reportRuntimeError("No parameters specified for synchronizedFunction(.");
        }

        if (!(args[0] instanceof Function)) {
            throw Context.reportRuntimeError("The first argument for synchronizedFunction() must be a Function object");
        }

        Scriptable scope = funObj.getParentScope();
        
        if (args.length == 1) {
            TMLContext context = fetchInitialContext(cx);
            return new SynchronizedFunction((Function) args[0], context.getwgacore(), scope);
        }
        else {
            return new SynchronizedFunction((Function) args[0], args[1], scope);
        }

    }

    
    public static Scriptable scriptObject(Context cx, Scriptable thisObj, Object[] args, Function funObj) {

        if (args.length == 0 || args[0] == null) {
            return null;
        }
        else {
            return Context.toObject(args[0], thisObj);
        }

    }
    
    
    public static String format(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {

        Object toBeFormatted = args[0];

        String formatString = null;
        if (args.length >= 2 && args[1] != null) {
            formatString = (String) Context.jsToJava(args[1], String.class);
        }
        
        String language = null;
        if (args.length >= 3 && args[2] != null) {
            language = (String) Context.jsToJava(args[2], String.class);
        }
        
        String formatTarget = null;

        if (toBeFormatted instanceof NativeJavaObject) {
            toBeFormatted = ((NativeJavaObject) toBeFormatted).unwrap();
        }
        else if (toBeFormatted instanceof Scriptable && toBeFormatted.getClass().getName().equals("de.innovationgate.ext.org.mozilla.javascript.NativeDate")) {
            Scriptable jsDate = (Scriptable) toBeFormatted;
            Double javaTime = (Double) ScriptableObject.callMethod(jsDate, "getTime", null);
            toBeFormatted = new Date(javaTime.longValue());
        }
        else if (toBeFormatted instanceof Scriptable) {
            toBeFormatted = ((Scriptable) toBeFormatted).getDefaultValue(null);
        }

        // Fetch locale to use
        try {
            return get(thisObj).getWga().format(
                    (Object) Context.jsToJava(toBeFormatted, Object.class), 
                    formatString,
                    language
             );
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }
    
    
    @SuppressWarnings("rawtypes")
    public static List sortList(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {

        Object listObj = args[0];
        Object comparatorObj = null;
        Object sortDirection = null;

        int sortDir = 1;

        if (args.length > 1) {
            comparatorObj = args[1];
        }

        if (args.length > 2) {
            sortDirection = args[2];
        }

        if (listObj instanceof NativeJavaObject) {
            listObj = ((NativeJavaObject) listObj).unwrap();
        }
        if (comparatorObj != null && comparatorObj instanceof NativeJavaObject) {
            comparatorObj = ((NativeJavaObject) comparatorObj).unwrap();
        }
        
        if (comparatorObj instanceof Function) {
            comparatorObj = new JavascriptFunctionComparator((Function) comparatorObj);
        }

        if (sortDirection != null) {
            String dir = (String) Context.jsToJava(sortDirection, String.class);
            if (dir.equalsIgnoreCase("descending") || dir.equalsIgnoreCase("down") || dir.equalsIgnoreCase("-")) {
                sortDir = -1;
            }
        }
        
        try {
            return get(thisObj).getWga().sortList(
                    Context.jsToJava(listObj, Object.class), 
                    Context.jsToJava(comparatorObj, Object.class), 
                    sortDir
            );
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }

    
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static List deleteDoublets(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
        List listObj = (List) Context.jsToJava(args[0], List.class);
        return get(thisObj).getWga().deleteDoublets(listObj);
    }

    
    public static Object callAction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {

        VarArgParser.Arguments parsedArgs = _callActionVarargs.parse(args);

        // Determine action id and action context
        TMLContext context = (TMLContext) parsedArgs.get("context");
        if (context == null) {
            context = fetchInitialContext(cx);
        }
        
        String actionID = (String) parsedArgs.get("id");
        List<Object> actionArgs = new ArrayList<Object>(parsedArgs.getOverflowArgs());

        // Get the current action, to be able to expand local name and search
        // the action to perform in it's database
        DesignResourceReference actionLocator = getBaseReference(cx, thisObj, context);

        // Call action
        try {
            Object actionResult = get(thisObj).getWga().callAction(
                    context, 
                    actionID, 
                    actionArgs, 
                    actionLocator, 
                    fetchRootScope(cx).getData()
            );
            return Context.javaToJS(actionResult, thisObj);
        }
        catch (Exception e) {
            // Look if we find an TMLScript exception somewhere down. If so throw its error value.
            TMLScriptException tmlscriptEx = WGUtils.getRootCause(e, TMLScriptException.class);
            if (tmlscriptEx != null) {
                throw Context.throwAsScriptRuntimeEx(tmlscriptEx.getJsException()); 
            }
            throw Context.throwAsScriptRuntimeEx(e);
        }

    }

    /**
     * Determines the base reference for design retrievals inside this script
     * 
     * @param cx
     *            The current rhino context
     * @param thisObj
     *            The current this object
     * @param context
     *            The TML Context
     * @return
     */
    protected static DesignResourceReference getBaseReference(Context cx, Scriptable thisObj, TMLContext context) {

        DesignResourceReference actionLocator = null;

        // Retrieve objects that may override the locator information from the current context
        TMLAction currentObjectDefinition = thisObj != null ? getObjectDefinition(thisObj) : null;
        DesignResourceReference currentLocator = currentActionLocator(cx);
        TMLAction currentAction = currentAction(cx);
        
        // Inside a TMLScript object
        if (currentObjectDefinition != null) {
            return new DesignResourceReference(currentObjectDefinition.getModuleDatabase(), currentObjectDefinition.getModuleName());
        }

        // A custom base reference was registered for the script (Form Validations etc.)
        else if (currentLocator != null) {
            return currentLocator;
        }
        
        // An action definition was registered (Inside WebTML Actions)
        else if (currentAction != null) {
            return new DesignResourceReference(currentAction.getModuleDatabase(), currentAction.getModuleName());
        }

        // Use TMLContext base reference information, try to extract dbkey information (All other scripts)
        else  {
            return context.getDesignContext().getBaseReference();
        }
        
    }

    public static TMLAction getObjectDefinition(Scriptable thisObj) {
        
        if (thisObj == null) {
            return null;
        }
        
        // Retrieve object def from the TMLScript object parent scope
        TMLScriptObjectParentScope parentScope = getObjectParentScope(thisObj);
        if (parentScope != null) {
            return parentScope.getObjectDefinition();
        }
        
        // Compatibility: Read from object property itself (most likely no more necessary)
        if (thisObj.has(RhinoExpressionEngine.PARAM_ACTIONDEFINITION, thisObj)) {
            return (TMLAction) thisObj.get(RhinoExpressionEngine.PARAM_ACTIONDEFINITION, thisObj);
        }
        else {
            return null;
        }
        
    }
    
    private static DesignResourceReference currentActionLocator(Context cx) {
        return (DesignResourceReference) cx.getThreadLocal(RhinoExpressionEngine.TL_ACTIONLOCATOR);
    }
    
    private static TMLScriptObjectParentScope getObjectParentScope(Scriptable thisObj) {
        Scriptable parentScope = thisObj;
        while (parentScope != null && !(parentScope instanceof TMLScriptRootScope)) {
            if (parentScope instanceof TMLScriptObjectParentScope) {
                return (TMLScriptObjectParentScope) parentScope;
            }
            parentScope = parentScope.getParentScope();
        }
        return null;
    }
    

    
    public static Function loadObjectDefinition(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, JavaScriptException, NoSuchMethodException, SecurityException {
        
        Arguments parsedArgs = _loadObjectDefVarags.parse(args);
        
        Object[] designArgs;
        if (parsedArgs.has("currentObject")) {
            designArgs = new Object[] { parsedArgs.get("currentObject") };
        }
        else {
            designArgs = new Object[] {};
        }
        
        List<Object> createArgs = new ArrayList<Object>();
        createArgs.add(parsedArgs.get("id"));
        
        Design design = design(cx, get(thisObj), designArgs, null);
        return Design.loadObjectDefinition(cx, design, createArgs.toArray(), null);
        
    }

    protected static RhinoExpressionEngineImpl fetchRuntime(Context cx) {
        return fetchRootScope(cx).getData().getRhinoScope().getRuntime();
    }
    
    public static TMLContext fetchInitialContext() throws JavaScriptException {
        return fetchInitialContext(Context.getCurrentContext());
    }
    
    public static WGA fetchWGA() throws JavaScriptException {
        return WGA.get(fetchInitialContext());
    }

    public static TMLContext fetchInitialContext(Context cx) throws JavaScriptException {
        return TMLContext.getThreadMainContext();
    }

    public static TMLScriptRootScope fetchRootScope(Context cx) throws JavaScriptException {
        return (TMLScriptRootScope) cx.getThreadLocal(RhinoExpressionEngine.TL_ROOTSCOPE);
    }
    
    
    public static Scriptable createObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, IOException, ClassNotFoundException, JavaScriptException, NoSuchMethodException, SecurityException  {

        Arguments parsedArgs = _createObjectVarargs.parse(args);
               
        Object[] designArgs;
        if (parsedArgs.has("currentObject") && WGAGlobal.getObjectDefinition((Scriptable) parsedArgs.get("currentObject")) != null) {
            designArgs = new Object[] { parsedArgs.get("currentObject") };
        }
        else {
            designArgs = new Object[] {};
        }
        
        List<Object> createArgs = new ArrayList<Object>();
        createArgs.add(parsedArgs.get("definition"));
        createArgs.addAll(parsedArgs.getOverflowArgs());
        
        Design design = design(cx, get(thisObj), designArgs, null);
        return Design.createObject(cx, design, createArgs.toArray(), null);

    }
    
    public static String getMainObjectName(String moduleName) {

        String objectName;
        
        // Take only the last part of the module name
        int colonPos = moduleName.lastIndexOf(":");
        if (colonPos >= 0) {
            objectName = moduleName.substring(colonPos + 1);
        }
        else {
            objectName = moduleName;
        }
        
        // If dots contained take only the last part after the dots
        int dotPos = objectName.lastIndexOf(".");
        if (dotPos >= 0) {
            objectName = objectName.substring(dotPos +1);
        }
        
        return WGUtils.toJSIdentifier(objectName, true);
        
    }

    
    public static void logException(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {

        TMLContext context = fetchInitialContext(cx);

        // Determine parameters
        String msg = null;
        Object error = null;

        if (args.length == 0) {
            context.getlog().error("Unloggable TMLScript error because of too few arguments for logException");
            return;
        }
        else if (args.length == 1) {
            msg = "Error in TMLScript";
            error = args[0];
        }
        else {
            msg = (String) args[0];
            error = args[1];
        }

        // Determine throwable to log and/or additional message to put out
        Throwable throwable = null;
        String additionalMsg = null;

        if (error instanceof Throwable) {
            throwable = (Throwable) error;
        }
        else if (error instanceof Scriptable) {
            Scriptable nativeError = (Scriptable) error;
            if (nativeError.has("rhinoException", nativeError)) {
                throwable = (RhinoException) ((NativeJavaObject) nativeError.get("rhinoException", nativeError)).unwrap();
            }
            else if (nativeError.has("javaException", nativeError)) {
                throwable = (Throwable) ((NativeJavaObject) nativeError.get("javaException", nativeError)).unwrap();
            }
            else if (nativeError.has("message", nativeError)) {
                additionalMsg = (String) nativeError.get("message", nativeError);
            }
            else {
                additionalMsg = "(scriptable error object without further information: " + nativeError.getClass().getName() + ")";
            }

            if (nativeError.has("lineNumber", nativeError) && !(throwable instanceof RhinoException)) {
                additionalMsg += ". Line number: " + (Integer) nativeError.get("lineNumber", nativeError);
            }
        }
        else {
            additionalMsg = String.valueOf(error);
        }
        
        // Try to find job context. If available use the job log to put out error
        Logger theLog = context.getlog();
        JobContext jobContext = null;
        Object jcObj = thisObj.get("jobContext", thisObj);
        if (jcObj != null && jcObj instanceof NativeJavaObject) {
            jcObj = ((NativeJavaObject) jcObj).unwrap();
            if (jcObj instanceof JobContext) {
                jobContext = (JobContext) jcObj;
                theLog = jobContext.getLog();
            }
        }

        // Put out
        if (additionalMsg != null) {
            msg += ": " + additionalMsg;
        }

        if (throwable != null) {

            if (throwable instanceof RhinoException) {
                RhinoException rhinoEx = (RhinoException) throwable;
                if (rhinoEx.lineNumber() != 0) {
                    msg += ". Line Number " + rhinoEx.lineNumber();
                }

                if (rhinoEx.lineSource() != null) {
                    msg += ". Line source: " + rhinoEx.lineSource();
                }
            }

            theLog.error(msg, throwable);
            
            context.addwarning(msg + ". Exception message: " + throwable.getMessage() + ". Exception type: " + throwable.getClass().getName());
        }
        else {
            theLog.error(msg);
            context.addwarning(msg);
        }

    }

    
    public static List<Object> getLookupKeys(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {

        VarArgParser.Arguments varArgs = _getLookupKeysVarargs.parse(args);

        if (!varArgs.has("table")) {
            throw new EvaluatorException("Function getLookupKeys() needs a lookup table as argument");
        }

        Map<?,?> table = (Map<?,?>) varArgs.get("table");
        return new ArrayList<Object>(table.keySet());

    }

    
    public static String serializeObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {

        Object obj = Context.jsToJava(args[0], Object.class);
        
        try {
            return get(thisObj).getWga().serializeObject(obj);
        }
        catch (Exception e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }        
        
    }
    
    
    public static Object deserializeObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {

        String encrypted  = String.valueOf(args[0]);
        try {
            return Context.javaToJS(get(thisObj).getWga().deserializeObject(encrypted), funObj);
        } catch (Exception e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }        
    }
    
    
    public static Object scoped(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        try {
            
            Arguments varargs = _scopedVarargs.parse(args);
            
            String str = (String) varargs.get("value");
            String scope = (String) varargs.get("scope");
            return get(thisObj).getWga().scoped(str, scope);
            
        }
        catch (Exception e) {
            throw new WrappedException(e);
        }
    }
    
    
    public static Plugin plugin(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        if (args.length == 0)
        	return get(thisObj).getWga().plugin();
        
        Object arg1 = Context.jsToJava(args[0], Object.class);
        
        if (arg1 instanceof String) {
            return get(thisObj).getWga().plugin((String) arg1);
        }
        else if (arg1 instanceof WGDatabase) {
            return get(thisObj).getWga().plugin((WGDatabase) arg1);
        }
        else {
            throw new EvaluatorException("Method WGA.plugin() called with wrong parameter");
        }
        
        
    }
    
    
    public static Lucene lucene(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        
        TMLContext tmlCx;
        if (args.length == 0) {
            tmlCx = fetchInitialContext(cx);
        }
        else if (args.length == 1) {
            tmlCx = (TMLContext) Context.jsToJava(args[0], TMLContext.class);
        }
        else {
            throw new EvaluatorException("Method get(thisObj).getWga().lucene() needs either no or a single TMLContext parameter");
        }
        
        try {
            return get(thisObj).getWga().lucene(tmlCx);
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }        
        
    }

    
    
    public static Date createDate(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        boolean includeMillis = true;
        if (args.length >= 1) {
             includeMillis = (Boolean) Context.jsToJava(args[0], Boolean.class);
        }
    
        return get(thisObj).getWga().createDate(includeMillis);
        
    }
    
    
    public static TMLForm createForm(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        if (args.length != 1) {
             throw new EvaluatorException("Method get(thisObj).getWga().createForm() needs an FormInfo object as parameter");
        }
        
        return fetchInitialContext(cx).createform(
                (TMLFormInfo) Context.jsToJava(args[0], TMLFormInfo.class)
        );
        
    }
    
    
    public static FormInfo createFormInfo(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException {
        
        if (args.length < 1) {
            throw new EvaluatorException("Method get(thisObj).getWga().createFormInfo() needs a string as parameter");
        }
        
        String id = String.valueOf(args[0]);
        
        if (args.length == 1) {
            return fetchInitialContext(cx).createforminfo(id);
        }
        
        
        // Backward compatibility
        boolean htmlInput = false;
        if (args.length >= 2) {
            htmlInput = (Boolean) args[1];
        }
        
        boolean persistent = false;
        if (args.length >= 3) {
             persistent = (Boolean) args[2];
        }
        
        return get(thisObj).getWga().createFormInfo(id, htmlInput, persistent);
        
    }
    
    
    public static ImageScaler createImageScaler(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, JavaScriptException, ModuleInstantiationException, IOException {
        
        if (args.length != 1) {
            throw new EvaluatorException("Method get(thisObj).getWga().createImageScaler() needs either a file or an input stream as parameter");
        }
        
        Object arg = Context.jsToJava(args[0], Object.class);

        if (arg instanceof File) {
            return get(thisObj).getWga().createImageScaler((File) arg);
        }
        else if (arg instanceof InputStream) {
            return get(thisObj).getWga().createImageScaler((InputStream) arg);
        }
        else {
            throw new EvaluatorException("Method get(thisObj).getWga().createImageScaler() needs either a file or an input stream as parameter");
        }
        
    }
    
    
    public static List<? extends Object> createList(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        
        if (args.length == 0) {
            return get(thisObj).getWga().createList();
        }
        if (args.length == 1) {
        	Object arg = args[0];
        	if(arg instanceof Collection || arg instanceof ListWrapper){
				@SuppressWarnings("unchecked")
				Collection<Object> objects = (Collection<Object>) Context.jsToJava(arg, Collection.class);
	        	return get(thisObj).getWga().createList(objects);
        	}
        	else {
				Object[] objects = (Object[]) Context.jsToJava(args[0], Object[].class);
	        	return get(thisObj).getWga().createList(objects);
        	}
        }
        else if (args.length == 2) {
            Object arg1 = args[0];
            Object arg2 = args[1];
            return get(thisObj).getWga().createList(String.valueOf(arg1), String.valueOf(arg2));
        }
        else {
            throw new EvaluatorException("Method get(thisObj).getWga().createList() needs either no parameters, an array or two string parameters");
        }
        
    }
    
    
    public static Map<Object,Object> createLookupTable(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        
        if (args.length > 0) {
            @SuppressWarnings("unchecked")
            Map<Object,Object> map = (Map<Object,Object>) Context.jsToJava(args[0], Map.class);
            return get(thisObj).getWga().createLookupTable(map);
        }
        
        return get(thisObj).getWga().createLookupTable();
    }
    
    
    public static SmtpMail createMail(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, TMLException, UnsupportedEncodingException {
        
        try {
            if (args.length == 0) {
                return get(thisObj).getWga().createMail();
            }
            if (args.length == 1) {
                return get(thisObj).getWga().createMail((Map<String,Object>)args[0]);
            }
            else if (args.length == 3) {
                Object arg1 = args[0];
                Object arg2 = args[1];
                Object arg3 = args[2];
                return get(thisObj).getWga().createMail(String.valueOf(arg1), String.valueOf(arg2), String.valueOf(arg3));
            }
            else {
                throw new EvaluatorException("Method WGA.createMail() needs either no parameter, one Map parameter or three string parameters");
            }
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
        
    }
    
    public static WGDatabase db(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException {
        
        if (args.length != 1) {
            throw new EvaluatorException("Method get(thisObj).getWga().db() needs one string parameter");
        }
        
        try {
            return get(thisObj).getWga().db(String.valueOf(args[0]));
        }
        catch (Exception e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
        
    }
    
    
    public static WGHierarchicalDatabase hdb(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException {
        
        if (args.length != 1) {
            throw new EvaluatorException("Method get(thisObj).getWga().hdb() needs one string parameter");
        }
        
        return get(thisObj).getWga().hdb(String.valueOf(args[0]));
        
    }
    
    
    public static String encode(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, FormattingException {
        
    	/*
        if (args.length != 2) {
            throw new EvaluatorException("Method get(thisObj).getWga().encode() needs a string and an object parameter");
        }
        */
        try {
        	
        	if(args.length == 2){            	
                return get(thisObj).getWga().encode(
                        String.valueOf(args[0]), 
                        Context.jsToJava(args[1], Object.class)
                );        		
        	}
        	else if(args.length == 3){
                return get(thisObj).getWga().encode(
                        String.valueOf(args[0]), 
                        Context.jsToJava(args[1], Object.class),
                        ((ContextWrapper)args[2]).getTmlContext()
                );        		
        	}
        	else{
        		throw new EvaluatorException("Invalid parameters for WGA.encode(String, Object[, TMLContext])");
        	}
        	
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }

        
    }
    
    protected static TMLAction currentAction(Context cx) {
        return (TMLAction) cx.getThreadLocal(RhinoExpressionEngine.TL_ACTIONDEFINITION);
    }

    
    public static void redirectTo(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, FormattingException, WGAPIException, IOException {
    
        if (args.length < 1) {
            throw new EvaluatorException("Method get(thisObj).getWga().redirectTo() needs a string parameter");
        }
        
        try {
            get(thisObj).getWga().redirectTo(String.valueOf(args[0])
            );
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    
    }
    
    public static List<String> buildOptions(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, FormattingException, WGAPIException, IOException {
        Arguments parsedArgs = _buildOptionsVarargs.parse(args);
        @SuppressWarnings("unchecked")
        Iterable<WGContent> contents = (Iterable<WGContent>) parsedArgs.get("contents");
        String titleExpr = (String) parsedArgs.get("title");
        String emptyTitle = (String) parsedArgs.get("emptyTitle");
        
        try {
            return get(thisObj).getWga().buildOptions(contents, titleExpr, emptyTitle);
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }
    
        
    
    public static Design design(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, JavaScriptException, NoSuchMethodException, SecurityException {
        Arguments arguments = _designVarargs.parse(args);
        return new Design(arguments.get("ref"), thisObj.getParentScope());
    }
    
    
    public static HttpClient createHttpClient(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, FormattingException, IOException, WGException {
        return WGFactory.getHttpClientFactory().createHttpClient();
    }

    public WGA getWga() {
        return (WGA) ((NativeJavaObject) getPrototype()).unwrap();
    }
    
    public static WGA getStaticWga(Scriptable thisObj) {
        return ((WGAGlobal) thisObj).getWga();
    }

    public Object unwrap() {
        return getWga();
    }

    public static TMLScriptObjectMetadata getObjectMetaData(Scriptable thisObj) {
        
        TMLScriptObjectParentScope parentScope = getObjectParentScope(thisObj);
        if (parentScope != null) {
            return parentScope.getObjectMetaData();
        }
        return null;

        
    }
    
}
