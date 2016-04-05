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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.dom4j.Document;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.FunctionObject;
import de.innovationgate.ext.org.mozilla.javascript.ImporterTopLevel;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.WrappedException;
import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.Require;
import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.RequireBuilder;
import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.provider.ModuleSource;
import de.innovationgate.ext.org.mozilla.javascript.commonjs.module.provider.SoftCachingModuleScriptProvider;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.TMLScriptDefinitions;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.tmlscript.CommonJSModuleSourceProvider;
import de.innovationgate.wgpublisher.expressions.tmlscript.Console;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser.Arguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Design;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Master;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Xml;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@CodeCompletion(delegate=RhinoScopeCC.class)
public class RhinoScope extends ImporterTopLevel {
    
    public static final String PROP_LISTENTOAPPEVENTS = "$listenToAppEvents";
    public static final String PROP_LISTENTOPORTLETEVENTS = "$listenToPortletEvents";
    public static final String PROP_LISTENTOWEBSOCKETCONNECT = "$listenToWebSocketConnect";
    public static final String PROP_DIRECTACCESS = "$directAccess";
    public static final String PROP_ISOLATED = "$isolated";
    public static final String PROP_SYNCED = "$synced";
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static VarArgParser _localLabelVarargs;
    static {
        _localLabelVarargs = VarArgParser.create("localLabel")
            .add("currentObject", NativeObject.class, true)
            .add("container", String.class, true)
            .add("file", String.class, true)
            .add("key", String.class, false)
            .add("params", List.class, true)
            .setCompatibleMode(true)
            .pack();
    }

    private boolean _inited = false;
    private CommonJSModuleSourceProvider _moduleLoader;
    private WGAGlobal _wgaGlobal;
    private RhinoExpressionEngineImpl _runtime;
    private Function _addMethodsFunction;

    private static final String[] METHODS ={
        "callAction",
        "createObject",
        "deleteDoubles",
        "deleteDoublets",
        "deserializeObject",
        "format",
        "getLookupKeys",
        "javaObject",
        "loadObjectDefinition",
        "localLabel",
        "localDB",
        "logException",
        "parseXML",
        "registerGlobal",
        "runMasterFunction",
        "runMasterMethod",
        "scriptObject",
        "serializeObject",
        "sortList", 
        "synchronizedFunction",
        "toString",
        "xpath",
        "xpathList",
    };
    
    private static final String[] PROPERTIES ={
        "runtime",
        "$",
        "$$",
        "console",
        "HDBModel"
    };
    
    private static final boolean silent = false;
    
    private static final List<String> REFERENCE_ERROR_NAMES = Arrays.asList("__defineGetter__", "__defineSetter__", "__lookupGetter__", "__lookupSetter__");
    
    public RhinoScope(RhinoExpressionEngineImpl runtime) throws IllegalAccessException, InstantiationException, InvocationTargetException, SecurityException, NoSuchMethodException {
        super();
        _runtime = runtime;
        defineFunctionProperties(METHODS, RhinoScope.class, DONTENUM);
        for (String prop : PROPERTIES) {
            defineProperty(prop, null, getClass().getMethod("jsGet_" + prop, new Class[] {ScriptableObject.class}), null, DONTENUM | READONLY);
        }
    }
    
    public static Scriptable jsGet_runtime(ScriptableObject thisObj) {
        return (Scriptable) Context.javaToJS(((RhinoScope) thisObj)._runtime, thisObj);
    }
    
    public static Scriptable jsGet_$(ScriptableObject thisObj) throws JavaScriptException, NoSuchMethodException, SecurityException {
        try {
            return new DesignLocator(WGAGlobal.fetchWGA(), getStaticWgaGlobalDesign(thisObj).getApiDesign().resolve(":"), thisObj);
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }
    
    public static Scriptable jsGet_$$(ScriptableObject thisObj) throws JavaScriptException, NoSuchMethodException, SecurityException {
        try {
            return new DesignLocator(WGAGlobal.fetchWGA(), getStaticWgaGlobalDesign(thisObj).getApiDesign().resolve(".."), thisObj);
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }
    
    public static Scriptable jsGet_HDBModel(ScriptableObject thisObj) throws JavaScriptException, NoSuchMethodException, SecurityException {
        try {
            return (Scriptable) Context.javaToJS(ScriptableObject.getProperty(getStaticWgaGlobalDesign(thisObj), "HDBModel"), thisObj);
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }
    
    public static Console jsGet_console(ScriptableObject thisObj) throws JavaScriptException, NoSuchMethodException, SecurityException {
        return new Console(WGAGlobal.fetchWGA().getLog());
    }

    /**
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getClassName()
     */
    public String getClassName() {
        return this.getClass().getName();
    }

    public synchronized void init(Context cx, WGACore wgaCore) throws IllegalAccessException, InstantiationException, InvocationTargetException, URISyntaxException, UnsupportedEncodingException, IOException, SecurityException, NoSuchMethodException {
        if (!_inited) {
            
            // Standard objects
            initStandardObjects(cx, false);
            defineFunctionProperties(new String[] {"print", "load"}, getClass(), DONTENUM);
            
            // Custom Additions to JS Object "Function"
            Scriptable functionPrototype = ScriptableObject.getFunctionPrototype(this);
            for (String functionMethodName : TMLScriptDefinitions.FUNCTION_MODIFICATION_METHODS.keySet()) {
                FunctionObject method = new FunctionObject(functionMethodName, getClass().getMethod("function_" + functionMethodName, new Class[] {Context.class, Scriptable.class, Object[].class, Function.class}), this); 
                ScriptableObject.defineProperty(functionPrototype, functionMethodName, method, ScriptableObject.PERMANENT | ScriptableObject.READONLY);
            }
                        
            _wgaGlobal = new WGAGlobal(this);
            put("WGA", this, _wgaGlobal);
            
            int oldLevel = cx.getOptimizationLevel();
            cx.setOptimizationLevel(-1);
            try {
                // CommonJS require
                if (!"false".equals(System.getProperty("de.innovationgate.wga.tmlscript.commonjs.enabled"))) {
                    initCommonJS(cx, WGA.get(wgaCore));
                }
                if ("true".equals(System.getProperty("de.innovationgate.wga.tmlscript.requirejs.enabled"))) {
                    initRequireJS(cx, WGA.get(wgaCore));
                }
            }
            finally {
                cx.setOptimizationLevel(oldLevel);
            }
            
            // Compile some static methods
            String code = WGUtils.readString(new InputStreamReader(getClass().getClassLoader().getResourceAsStream(WGUtils.getPackagePath(RhinoScope.class) + "/addMethodsFunction.js"), "UTF-8"));
            _addMethodsFunction = Context.getCurrentContext().compileFunction(this, code, null, 0, null);
            
            _inited = true;
            sealObject();
        }
    }


    public Function getAddMethodsFunction() {
        return _addMethodsFunction;
    }

    private void initRequireJS(Context cx, WGA wga) throws UnsupportedEncodingException, IOException, URISyntaxException {
        
        URL baseURI = getClass().getClassLoader().getResource("de/innovationgate/wgpublisher/expressions/tmlscript/requirejs/r.js");
        
        List<URI> uris = new ArrayList<URI>();
        _moduleLoader = new CommonJSModuleSourceProvider(uris);
        
        Scriptable argsObj = cx.newArray(this, new Object[] {});
        defineProperty("arguments", argsObj, ScriptableObject.DONTENUM);
        
        String bootstrap = WGUtils.readString(baseURI.openStream(), "UTF-8");
        cx.evaluateString(this, bootstrap , "RequireJS bootstrap", 1, null);
    }

    private void initCommonJS(Context cx, WGA wga) throws URISyntaxException, UnsupportedEncodingException, IOException {
        RequireBuilder rb = new RequireBuilder();
        rb.setSandboxed(false);
        
        URL baseURI = getClass().getClassLoader().getResource("de/innovationgate/wgpublisher/expressions/tmlscript/narwhal/bootstrap.js");
        
        List<URI> uris = new ArrayList<URI>();
        uris.add(new URL(baseURI, "engines/rhino/lib/").toURI());
        uris.add(new URL(baseURI, "engines/default/lib/").toURI());
        uris.add(new URL(baseURI, "lib/").toURI());
        
        _moduleLoader = new CommonJSModuleSourceProvider(uris);
        
        rb.setModuleScriptProvider(
                new SoftCachingModuleScriptProvider(
                        _moduleLoader));
        
        Require require = rb.createRequire(cx, this);
        require.install(this);
        
        String bootstrap = WGUtils.readString(baseURI.openStream(), "UTF-8");
        cx.evaluateString(this, bootstrap , "CommonJS bootstrap", 0, null);
        
    }

    /**
     * @return Returns the inited.
     */
    public boolean isInited() {
        return _inited;
    }
    
    public static WGDatabase localDB(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
        
        if (args.length != 0 && !(args[0] instanceof NativeObject)) {
            throw new EvaluatorException("Global function localDB() needs either no or a custom TMLScript object as parameter");
        }

        Design design = WGAGlobal.design(cx, getStaticWgaGlobal(thisObj), args, funObj);
        return design.getApiDesign().db();
    }
    
    public static String localLabel(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
        TMLContext context = WGAGlobal.fetchInitialContext(cx);
        Arguments parsedArgs = _localLabelVarargs.parse(args);
        WGDatabase localDB = localDB(cx, thisObj, new Object[] {parsedArgs.get("currentObject")}, funObj);
        return context.label(localDB, (String) parsedArgs.get("container"), (String) parsedArgs.get("file"), (String) parsedArgs.get("key"), (List<?>) parsedArgs.get("params")); 
    }
    
    public static void registerGlobal(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
        Design design = WGAGlobal.design(cx, getStaticWgaGlobal(thisObj), new Object[] {}, funObj);
        Design.registerGlobal(cx, design, args, funObj);
    }
    
    public static String serializeObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        return WGAGlobal.serializeObject(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }
    
    public static Object deserializeObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        return WGAGlobal.deserializeObject(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    

    public Object get(String name, Scriptable start) {

        // Return scope objects
        Object result = super.get(name, start);
        
        
        // Unknown references in TMLScript return null instead of throwing a reference error
        // (unless they are special names where a ref error is needed for normal execution #00004275) 
        if (result == Scriptable.NOT_FOUND && !(REFERENCE_ERROR_NAMES.contains(name))) {
            return null;
        }
        else {
            return result;
        }
    }

    public static void print(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
      if (silent)
        return;
        for (int i = 0; i < args.length; i++)
          WGAGlobal.fetchInitialContext(cx).getlog().info(Context.toString(args[i]));
    }

    public static void load(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) throws FileNotFoundException, IOException {
        
        RhinoScope shell = (RhinoScope) getTopLevelScope(thisObj);
        for (int i = 0; i < args.length; i++) {
            shell.processSource(cx, Context.toString(args[i]));
        }
    }
    
    public static Scriptable function_listenToAppEvents(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        ScriptableObject.putProperty(thisObj, PROP_LISTENTOAPPEVENTS, args);
        return thisObj;
    }
    
    public static Scriptable function_nonIsolated(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        ScriptableObject.putProperty(thisObj, PROP_ISOLATED, false);
        return thisObj;
    }
    
    public static Scriptable function_listenToPortletEvents(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        ScriptableObject.putProperty(thisObj, PROP_LISTENTOPORTLETEVENTS, args);
        return thisObj;
    }
    
    public static Scriptable function_listenToWebSocketConnect(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        ScriptableObject.putProperty(thisObj, PROP_LISTENTOWEBSOCKETCONNECT, true);
        return thisObj;
    }
    
    public static Scriptable function_directAccess(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        ScriptableObject.putProperty(thisObj, PROP_DIRECTACCESS, (Boolean) args[0]);
        return thisObj;
    }
    
    public static Scriptable function_synced(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        ScriptableObject.putProperty(thisObj, PROP_SYNCED, true);
        return thisObj;
    }

    private void processSource(Context cx, String filename)
            throws FileNotFoundException, IOException {
        
        if (_moduleLoader == null) {
            throw new FileNotFoundException("No module loader active");
        }
        
        try {
            ModuleSource source = _moduleLoader.loadSource(filename, null, null);
            if (source == null) {
                throw new FileNotFoundException(filename);
            }
            TMLScriptRootScope rootScope = (TMLScriptRootScope) cx.getThreadLocal(RhinoExpressionEngineImpl.TL_ROOTSCOPE);
            cx.evaluateReader(rootScope, source.getReader(), filename, 1, null);
        }
        catch (URISyntaxException e) {
            throw new IOException("Invalid URI syntax", e);
        }
    }
    
    public static String toString(Context cx, Scriptable thisObj, Object[] args,
            Function funObj) {
        return "RhinoScope";
    }

    public WGAGlobal getWgaGlobal() {
        return _wgaGlobal;
    }

    public static Object callAction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
           return WGAGlobal.callAction(cx, getStaticWgaGlobal(thisObj), args, funObj);
       }

    public static Scriptable createObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, IOException, ClassNotFoundException, JavaScriptException, NoSuchMethodException, SecurityException {
            WGAGlobal wga = getStaticWgaGlobal(thisObj);
            return WGAGlobal.createObject(cx, getStaticWgaGlobal(thisObj), args, funObj);
       }

    public static List<?> deleteDoubles(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
        return WGAGlobal.deleteDoublets(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static List<?> deleteDoublets(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
        return WGAGlobal.deleteDoublets(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static String format(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
        return WGAGlobal.format(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static List<?> getLookupKeys(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
           return WGAGlobal.getLookupKeys(cx, getStaticWgaGlobal(thisObj), args, funObj);
       }

    private static WGAGlobal getStaticWgaGlobal(Scriptable thisObj) {
        return ((RhinoScope) thisObj).getWgaGlobal();
    }
    
    private static Design getStaticWgaGlobalDesign(Scriptable thisObj) throws JavaScriptException, NoSuchMethodException, SecurityException, WGException {
        WGAGlobal wgaGlobal = getStaticWgaGlobal(thisObj);
        return WGAGlobal.design(Context.getCurrentContext(), wgaGlobal, new Object[] {}, null);
    }

    public static Scriptable javaObject(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        return WGAGlobal.javaObject(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static Function loadObjectDefinition(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, JavaScriptException, NoSuchMethodException, SecurityException {
           return WGAGlobal.loadObjectDefinition(cx, getStaticWgaGlobal(thisObj), args, funObj);
       }

    public static void logException(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
           WGAGlobal.logException(cx, getStaticWgaGlobal(thisObj), args, funObj);
       }

    public static Document parseXML(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        try {
            String xmlStr = String.valueOf(args[0]).trim();
            return getStaticWgaGlobal(thisObj).getWga().xml().parse(xmlStr);
        }
        catch (Exception e) {
            throw new WrappedException(e);
        }
    }

    public static Object runMasterFunction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
           return Master.runFunction(cx, WGAGlobal.jsGet_Master(getStaticWgaGlobal(thisObj)), args, funObj);        
       }

    public static Object runMasterMethod(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
           return Master.runMethod(cx, WGAGlobal.jsGet_Master(getStaticWgaGlobal(thisObj)), args, funObj);
       }

    public static Scriptable scriptObject(Context cx, Scriptable thisObj, Object[] args, Function funObj) {
        return WGAGlobal.scriptObject(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static List<?> sortList(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException {
        return WGAGlobal.sortList(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static Scriptable synchronizedFunction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        return WGAGlobal.synchronizedFunction(cx, getStaticWgaGlobal(thisObj), args, funObj);
    }

    public static Object xpath(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        try {
            Object object = args[0];
            String xpath = args[1].toString();
            Xml xml = WGAGlobal.jsGet_Xml(getStaticWgaGlobal(thisObj));
            return Context.javaToJS(xml.xpath(object, xpath), thisObj);
        }
        catch (Exception e) {
            return null;
        }
    }

    public static Object xpathList(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        try {
            Object object = args[0];
            String xpath = args[1].toString();
            Xml xml = WGAGlobal.jsGet_Xml(getStaticWgaGlobal(thisObj));
            return Context.javaToJS(xml.xpathList(object, xpath), thisObj);
        }
        catch (Exception e) {
            return null;
        }
    }

    public RhinoExpressionEngineImpl getRuntime() {
        return _runtime;
    }

}
