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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.swing.UIManager;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.websocket.Session;

import org.apache.log4j.Logger;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;

import com.google.gson.JsonElement;
import com.thoughtworks.xstream.converters.SingleValueConverter;

import de.innovationgate.ext.org.mozilla.javascript.Callable;
import de.innovationgate.ext.org.mozilla.javascript.ClassCache;
import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.ContextAction;
import de.innovationgate.ext.org.mozilla.javascript.ContextFactory;
import de.innovationgate.ext.org.mozilla.javascript.EcmaError;
import de.innovationgate.ext.org.mozilla.javascript.ErrorReporter;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.IdScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeArray;
import de.innovationgate.ext.org.mozilla.javascript.NativeGenerator;
import de.innovationgate.ext.org.mozilla.javascript.NativeIterator;
import de.innovationgate.ext.org.mozilla.javascript.NativeJSON;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaClass;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.RhinoException;
import de.innovationgate.ext.org.mozilla.javascript.Script;
import de.innovationgate.ext.org.mozilla.javascript.ScriptRuntime;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Undefined;
import de.innovationgate.ext.org.mozilla.javascript.UniqueTag;
import de.innovationgate.ext.org.mozilla.javascript.WrappedException;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.ext.org.mozilla.javascript.json.JsonParser;
import de.innovationgate.ext.org.mozilla.javascript.serialize.ScriptableInputStream;
import de.innovationgate.ext.org.mozilla.javascript.serialize.ScriptableOutputStream;
import de.innovationgate.ext.org.mozilla.javascript.tools.debugger.Main;
import de.innovationgate.ext.org.mozilla.javascript.xml.XMLObject;
import de.innovationgate.utils.ISO8601DateFormat;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.wga.server.api.DescriptificationConfig;
import de.innovationgate.wga.server.api.DescriptificationConfig.ObjectMode;
import de.innovationgate.wga.server.api.CallMethodConfig;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.WebTMLFunctionArgumentSubstitutor;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.InvalidDirectAccessCallException;
import de.innovationgate.wgpublisher.expressions.InvalidReferenceOnCallChainException;
import de.innovationgate.wgpublisher.expressions.tmlscript.MethodArgumentsBuilder.MethodArguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata.Method;
import de.innovationgate.wgpublisher.expressions.tmlscript.globals.TMLScriptGlobalCreator;
import de.innovationgate.wgpublisher.expressions.tmlscript.objects.ObjectCreator;
import de.innovationgate.wgpublisher.expressions.tmlscript.objects.ObjectCreatorManager;
import de.innovationgate.wgpublisher.expressions.tmlscript.objects.TMLScriptObjectMetadataParser;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.RhinoScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptIsolatedParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptLegacyObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptModernObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptRootScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptRootScopeData;
import de.innovationgate.wgpublisher.expressions.tmlscript.scriptlets.WebTMLScriptletResolver;
import de.innovationgate.wgpublisher.expressions.tmlscript.serialisation.SerializedScriptObject;
import de.innovationgate.wgpublisher.expressions.tmlscript.serialisation.TMLScriptInputStream;
import de.innovationgate.wgpublisher.expressions.tmlscript.serialisation.TMLScriptOutputStream;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.TMLScriptWGAContext;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.wrapping.ContextWrapper;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.JsonUtils;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;


public class RhinoExpressionEngineImpl implements ExpressionEngine, RhinoExpressionEngine, SingleValueConverter {
    
    public static final Logger logger = Logger.getLogger("wga.tmlscript");
    
    private static final String PREFIX_JSON = "$$json:";

    public static final String WGAGLOBAL_NAME = "WGA";
    
    public interface ObjectScript {
        public Object run(TMLScriptRootScope rootScope) throws WGException, ClassNotFoundException, IOException;
        public boolean isIsolated();
    }
    
    public static final int DEFAULT_CACHE_SIZE = 1000;
    
    public static final String SYSPROPERTY_DEBUG = "de.innovationgate.wga.tmlscript.debug";
    public static final String SYSPROPERTY_COMPILER_VERBOSE = "de.innovationgate.wga.tmlscript.compiler.verbose";
    public static final String SYSPROPERTY_CACHE_SIZE = "de.innovationgate.wga.tmlscript.cache.size";
    
    public static class NativeES6ToJavaIterator extends PrefetchingIterator<Object> {
        
        private Scriptable _it;
        private boolean _done = false;

        public NativeES6ToJavaIterator(Scriptable it) {
            _it = it;
        }

        @Override
        protected Object fetchNextValue() {

            if (_done) {
                return null;
            }
            
            Scriptable next = (Scriptable) ScriptableObject.callMethod(_it, "next", new Object[] {});
            _done = (Boolean) Context.jsToJava(ScriptableObject.getProperty(next, "done"), Boolean.class);
            return ScriptableObject.getProperty(next, "value"); 
            
        }
        
    }
    
    private Cache _cachedScripts = null;
    private boolean _debugEnabled;
    private boolean _verboseCompiling;
    
    private int _cacheMaxSize;
    private WGACore _core;

    private WebTMLScriptletResolver _scriptletResolver;

    private ObjectCreatorManager _objectCreatorManager;

    private MethodArgumentsBuilder _argumentBuilder;

    private TMLScriptObjectMetadataParser _objectMetadataParser;

    public static final FilenameFilter TMLSCRIPT_NAME_FILTER = new FilenameFilter() {

        public boolean accept(File file, String name) {
           return name.startsWith("TMLScript:");
        }
        
    };
    
    public static final RhinoExpressionEngineImpl get() {
        return (RhinoExpressionEngineImpl) ExpressionEngineFactory.getTMLScriptEngine();
    }

    
    public RhinoExpressionEngineImpl() {
        
        _scriptletResolver = new WebTMLScriptletResolver(this);
        _objectCreatorManager = new ObjectCreatorManager(this);
        _argumentBuilder = new MethodArgumentsBuilder(this);
        _objectMetadataParser = new TMLScriptObjectMetadataParser();

        // Get some configs
        Boolean enableDebug = Boolean.valueOf(System.getProperty(SYSPROPERTY_DEBUG)).booleanValue();
        if (enableDebug) {
            enableDebugger();
        }
        
        _verboseCompiling = Boolean.valueOf(System.getProperty(SYSPROPERTY_COMPILER_VERBOSE)).booleanValue();
        
        // Prepare code cache
        _cacheMaxSize = DEFAULT_CACHE_SIZE;
        String cacheSizeStr = System.getProperty(SYSPROPERTY_CACHE_SIZE);
        if (cacheSizeStr != null) {
            try {
                _cacheMaxSize = Integer.parseInt(cacheSizeStr);
            }
            catch (NumberFormatException e) {
                logger.error("Cannot interpret '"  + SYSPROPERTY_CACHE_SIZE + "' as an integer: " + cacheSizeStr);
            }
        }
        
        // collect properties to configure cache
        Properties props = new Properties();
        props.setProperty("cache.memory", "true");
        props.setProperty("cache.capacity", String.valueOf(_cacheMaxSize));
        props.setProperty("cache.algorithm", "com.opensymphony.oscache.base.algorithm.LRUCache");
        props.setProperty("cache.blocking", "true");
        
        // Create cache
        try {
            _cachedScripts = CacheFactory.createCache("TMLScript_CodeCache", _cacheMaxSize, null);
        }
        catch (CacheException e) {
            logger.fatal("Error initializing TMLScript code cache. TMLScript is inactive!", e);
        }
        

    }
    
    public MethodArgumentsBuilder getArgumentBuilder() {
        return _argumentBuilder;
    }

    public void init(WGACore core) {
        _core = core;
    }

    public void enableDebugger() {
        openDebugger(getOrCreateContextFactory(_core));
        RhinoContextFactory.defaultOptimizationLevel = -1;
        _debugEnabled = true;
    }
    
    public void disableDebugger() {
        if (_debugger != null) {
            _debugger.detach();
            _debugger.dispose();
            _debugger = null;
            RhinoContextFactory.defaultOptimizationLevel = 9;
            _debugEnabled = false;
        }
    }

    public class RhinoContextAction implements ContextAction {

        TMLContext _tmlContext;

        private Map<String,Object> _additionalObjects;

        private int _type;

        private String _expression = null;

        private Function _function = null;
        
        private NativeObject _parentObject = null;

        private Object[] _functionParams;

        private boolean _overrideFunctionScope;

        public RhinoContextAction(TMLContext tmlObject, String expression, int type, Map<String,Object> additionalObjects) {
            _tmlContext = tmlObject;
            _expression = expression;
            _type = type;
            _additionalObjects = additionalObjects;
        }
        
        public RhinoContextAction(TMLContext tmlContext, Function function, boolean overrideFunctionScope, Object[] params) {
            _tmlContext = tmlContext;
            _function  = function;
            _overrideFunctionScope = overrideFunctionScope;
            _functionParams = params;
            _type = ExpressionEngine.TYPE_SCRIPT;
            _additionalObjects = Collections.<String,Object>emptyMap();
        }

        public Object run(Context cx) {
            RhinoContext rcx = (RhinoContext) cx;
            rcx.initFields();

            if (_tmlContext.iswebenvironment()) {
                rcx.isWebsiteScript = true;
            }
            
            if (_debugEnabled) {
                rcx.setOptimizationLevel(0);
            }
            else if (rcx.getOptimizationLevel() != RhinoContextFactory.defaultOptimizationLevel) {
                rcx.setOptimizationLevel(RhinoContextFactory.defaultOptimizationLevel);
            }

            // Preserve previous ThreadLocal variables and init/clear them for the current script
            ThreadLocalPreserver preserver =  new ThreadLocalPreserver(rcx);
            preserver.preserve(TL_ACTIONDEFINITION, null);
            preserver.preserve(TL_ACTIONLOCATOR, null);
            preserver.preserve(TL_SCRIPTNAME, null);
            preserver.preserve(TL_WGACONTEXT, new TMLScriptWGAContext(_tmlContext, null, false));
            ErrorReporter oldErrorReporter = null;
            
            // Set new main TMLContext for this thread
            _tmlContext.makeThreadMainContext();
            
            try {
                // Create scope
                TMLScriptRootScope scope = null;
                try {
                    scope = createRootScope(_tmlContext, rcx, false);
                }
                catch (Exception exc) {
                    return createExpressionResult(null, new de.innovationgate.webgate.api.WGExpressionException("Error creating tmlscript context", _expression, exc));
                }
                
                preserver.preserve(TL_ROOTSCOPE, scope);
                
                // Redirect error reporter;
                oldErrorReporter = rcx.setErrorReporter(scope);
                
                // Put custom additional objects into scope
                if (_additionalObjects != null) {
                    Iterator<String> objectNames = _additionalObjects.keySet().iterator();
                    String objectName;
                    Object object;
                    while (objectNames.hasNext()) {
                        objectName = (String) objectNames.next();
                        object = _additionalObjects.get(objectName);
                        if (objectName.startsWith("$")) {
                            if (objectName.equals(PARAM_SCRIPTTIMEOUT)) {
                                rcx.scriptTimeout = ((Number) object).intValue();
                            }
                            else if (objectName.equals(PARAM_ACTIONDEFINITION)) {
                                rcx.putThreadLocal(TL_ACTIONDEFINITION, object);
                            }
                            else if (objectName.equals(PARAM_ACTIONLOCATOR)) {
                                rcx.putThreadLocal(TL_ACTIONLOCATOR, object);
                            }
                            else if (objectName.equals(RhinoExpressionEngine.PARAM_SCRIPTNAME)) {
                                rcx.putThreadLocal(TL_SCRIPTNAME, object);
                            }
                        }
                        else {
                            scope.getData().putScopeObject(objectName, Context.javaToJS(object, scope));
                        }
                    }
                }
                
                // Eventually disable TMLScript optimization, so Script stack traces are generated
                rcx.setOptimizationLevel(RhinoContextFactory.defaultOptimizationLevel);
                rcx.setGeneratingDebug(false);
                if (_tmlContext.hasVariable(RhinoExpressionEngine.SESSIONVAR_ENABLE_SCRIPTSTACKTRACE)) {
                    Boolean tmlscriptOptimizationDisabled = (Boolean) _tmlContext.item(RhinoExpressionEngine.SESSIONVAR_ENABLE_SCRIPTSTACKTRACE);
                    if (tmlscriptOptimizationDisabled != null && tmlscriptOptimizationDisabled.booleanValue() == true) {
                        if (rcx.getOptimizationLevel() > 0) {
                            rcx.setOptimizationLevel(0);
                        }
                        rcx.setGeneratingDebug(true);
                    }
                }
                
                // Execute
                Object result = null;
                try {
                    if (_function != null) {
                        if (_parentObject != null) {
                            result = executeMethod(_parentObject, _function, _overrideFunctionScope, _functionParams, rcx, scope);
                        }
                        else {
                            result = executeFunction(_function, _overrideFunctionScope, _functionParams, rcx, scope);
                        }
                        
                    }
                    else if (_type == ExpressionEngine.TYPE_SCRIPT) {
                        result = executeScript(_expression, rcx, scope);
                    }
                    else {
                        result = executeExpression(_expression, rcx, scope);
                    }
                }
                
                // Some syntax error
                catch (EcmaError exc) {
                    String exceptionMessage = exc.getName() + " executing tmlscript: " + exc.getMessage() + "\n" + buildExceptionDetails(exc);
                    WGExpressionException expressionException;
                    String scriptStackTrace = filterScriptStackTrace(exc.getScriptStackTrace());
                    Throwable cause = (WGUtils.isEmpty(scriptStackTrace) ? exc : null);
                    expressionException = new de.innovationgate.webgate.api.WGExpressionException(exceptionMessage, exc.lineSource(), cause, scriptStackTrace);
                    return createExpressionResult(null, expressionException);
                }
                
                // A script timeout
                catch (TimeoutError err) {
                    return createExpressionResult(null, new de.innovationgate.webgate.api.WGExpressionException("Script execution timed out after " + err.getTimeout() + " milliseconds.",
                            _expression));
                }
                
                // Some java script error explicitly thrown by the code: Converted to TMLScriptException so the class can be better used outside
                catch (JavaScriptException exc) {
                    String exceptionMessage = "JavaScript exception executing tmlscript: " + exc.getMessage()  + " (Exception value: " + exc.getValue() + ")\n" + buildExceptionDetails(exc);
                    de.innovationgate.webgate.api.WGExpressionException expressionException = new de.innovationgate.webgate.api.WGExpressionException(exceptionMessage, exc.lineSource(), convertToTMLScriptException(exc), filterScriptStackTrace(exc.getScriptStackTrace()));
                    return createExpressionResult(null, expressionException);
                }
                
                // Everything else: Wrapped java exceptions, internal rhino malfunctionings etc.
                catch (RhinoException exc) {
                    
                    Throwable cause = exc;
                    if (cause instanceof WrappedException) {
                        cause = ((WrappedException) exc).getCause();
                    }
                    
                    return createExpressionResult(null, new de.innovationgate.webgate.api.WGExpressionException("Exception executing tmlscript: " + cause.getClass().getName() + " - Message:" +cause.getMessage() + ".\n"
                            + buildExceptionDetails(exc), exc.lineSource(), cause, exc.getScriptStackTrace()));
                }

                // Evaluate and return result;
                if (result instanceof NativeJavaObject) {
                    result = ((NativeJavaObject) result).unwrap();
                }
                else if (result instanceof NativeArray) {
                    result = new ArrayList<Object>(Arrays.asList(rcx.getElements((NativeArray) result)));
                }
                else if (result instanceof Undefined) {
                    result = "";
                }

                return createExpressionResult(result, null);
            }
            catch (Throwable e) {
                Logger.getLogger("wga").error("Error in TMLScript processing", e);
                return createExpressionResult(null, new de.innovationgate.webgate.api.WGExpressionException("Unexpected exception: " + e.getClass().getName() + ": " + e.getMessage(),
                        "(No source available)", e));
            }
            finally {
                
                /// Remove thread main context
                _tmlContext.removeThreadMainContext();
                
                // Restore preserved ThreadLocal values
                preserver.restore();
                
                rcx.setErrorReporter(oldErrorReporter);
            }
            
        }

        /**
         * Filters out internal TMLScript-Engine-Calls from the stacktrace
         * @param scriptStackTrace
         * @return
         */
        private String filterScriptStackTrace(String scriptStackTrace) {
            List<String> lines = WGUtils.deserializeCollection(scriptStackTrace, "\n", false);
            String line;
            for (int idx=0; idx < lines.size(); idx++) {
                line = (String) lines.get(idx);
                
                // As TMLScript-Scripts internally are converted to a JS-Function "_tmlscript", that is called right after definition
                // we can eliminate that call right before the function, and cut out the function name from the stack trace 
                if (line.endsWith("(_tmlscript)")) {
                    lines.remove(idx+1);
                    lines.set(idx, line.substring(0, line.indexOf("(_tmlscript)")));
                }
            }
            
            return WGUtils.serializeCollection(lines, "\n");
          }

        public NativeObject getParentObject() {
            return _parentObject;
        }

        public void setParentObject(NativeObject parentObject) {
            _parentObject = parentObject;
        }







        

    }

    public class MasterFunction implements Runnable {
        
    	private TMLContext _context;
    
    	private Object _returnValue = null;

        private Object[] _params;
        
        private NativeObject _parentObject = null;

        private Function _function;

        private boolean _overrideFunctionScope;
    	
    	public MasterFunction(TMLContext context, boolean overrideFunctionScope, Function function, Object[] params) throws WGAPIException, TMLException {
    	    _context = TMLContext.createMasterSessionContext(context);
    	    _overrideFunctionScope = overrideFunctionScope;
    	    _function = function;
            _params = params;
            
    	}
    	
    	/* (Kein Javadoc)
    	 * @see java.lang.Runnable#run()
    	 */
    	public void run() {
            
    		try {
                String taskDescr = "Anonymous TMLScript Master Function";
                
                // Open database in master thread
                WGDatabase db = _context.getdocument().getDatabase();
    			db.openSession();
    			WGDatabase mainContextDb = _context.getmaincontext().getdocument().getDatabase();
                if (mainContextDb != db) {
                    mainContextDb.openSession();
                }
    			
                db.getSessionContext().setTask(taskDescr);
                
                
                // Eventually open pers db too so profile is available
                TMLUserProfile profile = _context.getprofile();
                
                if (profile != null && !profile.getprofile().getDatabase().isSessionOpen()) {
                    WGDatabase persDB = profile.getprofile().getDatabase();
                    persDB.openSession();
                    persDB.getSessionContext().setTask(taskDescr);
                }
    
                RhinoContextAction contextAction = new RhinoContextAction(_context, _function, _overrideFunctionScope, _params);
                if (_parentObject != null) {
                    contextAction.setParentObject(_parentObject);
    		    }
                
                ExpressionResult result = (ExpressionResult) ContextFactory.getGlobal().call(contextAction);
                
                if (result.isError()) {
                    WGExpressionException exc = result.getException();
                    _context.getlog().error(
                            "Error executing anonymous TMLScript Master Function",
                            result.getException());
                             
                }
                
                if (profile != null) {
                    WGPDispatcher.saveUserProfile(_context.getwgacore(), profile, null);
                }
                
                _returnValue = result.getResult();                
    
    		}
    		catch (Exception e) {
    			_context.getwgacore().getLog().error("Error creating TML Context for master function", e);
    		}
    		finally {
    			WGFactory.getInstance().closeSessions();
    		}
    		
    	}
    	
    	public void start() {
    		Thread actionThread = new Thread(this);
    		actionThread.start();
			try {
				actionThread.join();
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
    	}
    
    	public Object getReturnValue() {
    		return _returnValue;
    	}

        public NativeObject getParentObject() {
            return _parentObject;
        }

        public void setParentObject(NativeObject parentObject) {
            _parentObject = parentObject;
        }
    
    
    
    
    
    }

    public static class NativeJS17ToJavaIterator extends PrefetchingIterator<Object> {
        
        private Scriptable _it;
        private boolean _done = false;
    
        public NativeJS17ToJavaIterator(Scriptable it) {
            _it = it;
        }
    
        @Override
        protected Object fetchNextValue() {
    
            if (!_done) {
                try {
                    Scriptable next = (Scriptable) ScriptableObject.callMethod(_it, "next", new Object[] {});
                    return next; 
                }
                catch (Exception e) {
                    _done = true;
                }
            }
            
            return null;
            
        }
        
    }

    static {
        ContextFactory.initGlobal(new RhinoContextFactory());
    }

    private RhinoScope _sharedScope = null;

    private RhinoContextFactory _contextFactory = null;

    private Main _debugger;
    public static final String SCOPEOBJECT_RUNTIME = "runtime";
    public static final String SCOPEOBJECT_CONSOLE = "console";

    /**
     * @see ExpressionEngine#evaluateExpression(String, TMLContext)
     */
    public ExpressionResult evaluateExpression(String expression, TMLContext context, int type, Map<String,Object> objects) {

        if (objects == null) {
            objects = new HashMap<String,Object>();
        }
        
        if (context == null) {
            return createExpressionResult(null, new WGExpressionException("Tried to execute TMLScript with an tml object context of null", expression));
        }

        if (objects.containsKey("$tmlscriptDebug")) {
            debug();
        }

        RhinoContextAction contextAction = new RhinoContextAction(context, expression, type, objects);
        return (ExpressionResult) getOrCreateContextFactory(context.getwgacore()).call(contextAction);

    }

	public TMLScriptException convertToTMLScriptException(JavaScriptException exc) {
        
	    // Isolate cause
	    Throwable cause = exc.getCause();
	    
	    // If there is no "real" cause in getCause() we look if there is a wrapped java exception object "in there", and use it nesa
	    if (cause == null || cause == exc) {
    	    if (exc.getValue() != null) {
    	        if (exc.getValue() instanceof NativeJavaObject) {
        	        Object obj = ((NativeJavaObject) exc.getValue()).unwrap();
        	        if (obj instanceof Throwable) {
        	            cause = (Throwable) obj;
        	        }
    	        }
    	        else if (exc.getValue() instanceof IdScriptableObject) { // Actually NativeError, which is not visible
    	            Object obj = ((IdScriptableObject) exc.getValue()).get("javaException");
    	            if (obj instanceof Throwable) {
    	                cause = (Throwable) obj;
    	            }
    	        }
    	    }
	    }
	    
        TMLScriptException tmlEx = new TMLScriptException(exc.details(), cause);
        tmlEx.setJsException(exc);
        tmlEx.setLineNumber(exc.lineNumber());
        tmlEx.setLineSource(exc.lineSource());
        tmlEx.setColumnNumber(exc.columnNumber());
        tmlEx.setErrorValue(exc.getValue());
        return tmlEx;
        
    }

    public void debug() {
        if (_debugger != null) {
            _debugger.doBreak();
        }
    }

    public RhinoScope getOrCreateSharedScope(Context cx, WGACore core) throws IllegalAccessException, InstantiationException, InvocationTargetException, URISyntaxException, UnsupportedEncodingException, IOException, SecurityException, NoSuchMethodException {
        if (this._sharedScope == null) {
            this._sharedScope = new RhinoScope(this);
        }

        if (cx != null && !_sharedScope.isInited()) {
            this._sharedScope.init(cx, core);
        }

        return this._sharedScope;
    }
    
    protected RhinoContextFactory getOrCreateContextFactory(WGACore core) {
        if (this._contextFactory == null || this._contextFactory.getApplicationClassLoader() != WGACore.getLibraryLoader()) {
            RhinoContextFactory factory = new RhinoContextFactory();
            factory.initApplicationClassLoader(WGACore.getLibraryLoader());
            this._contextFactory = factory;
        }

        return this._contextFactory;
    }
    
    public RhinoScope getSharedScope() {
        return this._sharedScope;
    }

    /**
     * @param exc
     * @return
     */
    private String buildExceptionDetails(RhinoException exc) {
        StringBuffer out = new StringBuffer();
        
        out.append("At line ").append(exc.lineNumber()).append(", column ").append(exc.columnNumber());
        
        String details = exc.details();
        if (details.length() > exc.getMessage().length() || (!details.equals(exc.getMessage().substring(0, details.length())))) {
            out.append("\nDetails: " + exc.details());
        }
        return out.toString();
    }



    /**
     * Method resolveScriptlets.
     * 
     * @param result
     * @return Object
     * @throws ParseException 
     * @throws WGAPIException 
     */
    public String resolveScriptlets(Object input, TMLContext context, Map<String,Object> engineParams) throws WGException {
        return _scriptletResolver.resolveScriptlets(input, context, engineParams);
    }

    private void openDebugger(ContextFactory factory) {
        
        try {
            UIManager.setLookAndFeel(MetalLookAndFeel.class.getName());
        _debugger = new Main("TMLScript Debugger");
        _debugger.attachTo(factory);
        _debugger.setExitAction(null);
        _debugger.pack();
        _debugger.setSize(600, 460);
        _debugger.setVisible(true);
        }
        catch (Exception e) {
            Logger.getLogger("wga.tmlscript.debug").error("Exception starting debugger", e);
        }
        
    }

    /* (non-Javadoc)
     * @see java.lang.Object#finalize()
     */
    public void close() {
       if (_debugger != null) {
           _debugger.dispose();
       }
       
       if (_cachedScripts != null) {
           try {
            _cachedScripts.destroy();
        }
        catch (CacheException e) {
            logger.error("Exception closing TMLScript code cache", e);
        }
       }
       
    }

    /**
     * @return Returns the debugEnabled.
     */
    public boolean isDebugEnabled() {
        return _debugEnabled;
    }
    
    public long getScriptCacheCurrentSize() {
        try {
            return _cachedScripts.getSize();
        }
        catch (CacheException e) {
            return 0;
        }
    }

    /**
     * @return Returns the cacheMaxSize.
     */
    public int getScriptCacheMaxSize() {
        return _cacheMaxSize;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine#isTMLScriptBean(java.lang.Object)
     */
    public int determineTMLScriptType(Object obj) {
        
        if (obj instanceof Scriptable) {
            Scriptable scr = (Scriptable) obj;
            if (scr.getClassName().equals("XMLList")) {
                return RhinoExpressionEngine.TYPE_XMLLIST;
            }
            else if (scr instanceof XMLObject) {
                return RhinoExpressionEngine.TYPE_XMLOBJECT;
            }
            else {
                return RhinoExpressionEngine.TYPE_SCRIPTABLE;
            }
        }
        else if (obj instanceof Undefined) {
            return RhinoExpressionEngine.TYPE_UNDEFINED;
        }
        else if (obj != null && obj.equals(ScriptRuntime.NaN)) {
            return RhinoExpressionEngine.TYPE_NAN;
        }
        else {
            return RhinoExpressionEngine.TYPE_NOTMLSCRIPT;
        }

    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine#xpathTMLScriptBean(java.lang.Object, java.lang.String)
     */
    public List<?> xpathTMLScriptBean(Object obj, String xpath) {
        
        XMLObject xmlObj = (XMLObject) obj;
        Document doc;
        try {
            doc = convertNativeXMLtoDOM(xmlObj);
        }
        catch (DocumentException e) {
           throw Context.throwAsScriptRuntimeEx(e);
        }
        return doc.getRootElement().selectNodes(xpath);
        
    }

    public Document convertNativeXMLtoDOM(Object xmlObj) throws DocumentException {
        
        int scriptType = determineTMLScriptType(xmlObj);
        if (scriptType == RhinoExpressionEngine.TYPE_XMLOBJECT) {
            String xmlText = (String) ScriptableObject.callMethod((XMLObject) xmlObj, "toXMLString", new Object[] {});
            return DocumentHelper.parseText(xmlText);
        }
        else {
            return null;
        }
    }
    
    private void logCompilation(String expression) {
        if (_verboseCompiling) {
            Logger.getLogger("wga.tmlscript").info("Compiling TMLScript code nr. " +  (getScriptCacheCurrentSize() + 1) + ": " + WGUtils.strReplace(expression, "\n", "", true));
        }
    }
    
    public CachedScript getCompiledScript(String code, RhinoContext cx, String scriptName, Scriptable scope) throws IOException {
        String codeCacheKey = String.valueOf(scope.getClass().getName() + "/" + code.hashCode());
        CachedScript script = null;
        try {
            script = (CachedScript) _cachedScripts.read(codeCacheKey);
        }
        catch (CacheException e) {
            Logger.getLogger("wga.tmlscript").error("Unable to load cached TMLScript code", e);
        }
        
        if (script == null || isDebugEnabled() || cx.isGeneratingDebug()) {
            logCompilation(code);
            script = new CachedScript(cx.compileString(code, scriptName, 1, null));
            if (!cx.isGeneratingDebug()) {
                try {
                    _cachedScripts.write(codeCacheKey, script);
                }
                catch (CacheException e) {
                    Logger.getLogger("wga.tmlscript.").error("Unable to cache TMLScript code", e);
                }
            }
        }
        return script;
    }
    
    
    


    private String determineScriptName(RhinoContext cx) {
        
        // Look for directly set script name
        String scriptName = (String) cx.getThreadLocal(TL_SCRIPTNAME);
        
        // Look for TMLAction definition
        if (scriptName == null) {
            TMLAction action = (TMLAction) cx.getThreadLocal(TL_ACTIONDEFINITION);
            if (action != null) {
                scriptName =  action.getDescription();
            }
        }
        
        // Undeterminable: An anonymous script
        if (scriptName == null) {
            scriptName = "Anonymous script";
        }
        return "TMLScript: " + scriptName;
    }
    
    protected Object executeScript(String expression, RhinoContext cx, Scriptable scope) throws IOException {
        
        StringBuffer code = new StringBuffer(expression.length() + 64);
        
        code.append("(function() {").append(expression).append("\n})()");
        Script script = getCompiledScript(code.toString(), cx, determineScriptName(cx), scope).getScript();
        return script.exec(cx, scope);
    }
    
    protected Object executeExpression(String expression, RhinoContext cx, Scriptable scope) throws IOException {
        Script script = getCompiledScript(expression, cx, determineScriptName(cx), scope).getScript();
        return script.exec(cx, scope);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine#convertXMLListToList(java.lang.Object)
     */
    public List<Object> convertXMLListToList(Object obj) {
       
        List<Object> list  = new ArrayList<Object>();
        
        if (determineTMLScriptType(obj) != RhinoExpressionEngine.TYPE_XMLLIST) {
            return list;
        }
        
        Scriptable scr = (Scriptable) obj;
        for (int idx=0; scr.has(idx, scr); idx++) {
            list.add(scr.get(idx, scr));
        }
        
        return list;
    }

    public void clearCache() {
        try {
            if (_sharedScope != null) {
                ClassCache.get(_sharedScope).clearCaches();
                clearScriptableSlots((Scriptable) ScriptableObject.getProperty(_sharedScope, "Packages"));
            }
            
            _cachedScripts.flushAll();
        }
        catch (Exception e) {
            logger.error("Exception flushing TMLScript cache", e);
        }
        
    }

    private void clearScriptableSlots(Scriptable object) {
        for (Object id : object.getIds()) {
            if (id instanceof String) {
                ScriptableObject.deleteProperty(object, (String) id);
            }
        }
    }


    private boolean isJSFalse(Object result) {

        
        if (result == null) {
            return true;
        }
        else if (result instanceof Boolean) {
            return !((Boolean) result).booleanValue();
        }
        else if (result instanceof Undefined) {
            return true;
        }
        else if (result instanceof Double) {
            return ((Double) result).isNaN() || ((Double) result).doubleValue() == 0.0;
        }
        else if (result instanceof Number) {
            return ((Number) result).doubleValue() == 0.0;
        }
        else if (result instanceof String) {
            return ((String) result).equals(""); 
        }
        /*else if (result.equals("yakety yak")) {
            return true;
        }*/
         
        else {
            return false;
        }
    }
    
    public int getJsVersion() {
        int version = Context.getCurrentContext().getLanguageVersion();
        return version;
    }
    
    public String getRhinoVersion() {
        return Context.getCurrentContext().getImplementationVersion();
    }
    
    public Object runAsMaster(Function function, TMLContext targetContext, Object... params) throws WGAPIException, TMLException {
        boolean overrideFunctionScope = true;
        if (targetContext == null) {
            targetContext = WGAGlobal.fetchInitialContext(Context.getCurrentContext());
            overrideFunctionScope = false;
        }
        
        MasterFunction masterAction = new MasterFunction(targetContext, overrideFunctionScope, function, params);
        masterAction.start();
        return masterAction.getReturnValue();
    }
    
    public Object runAsMasterMethod(NativeObject parentObject, Function function, Object... params) throws WGAPIException, TMLException {
        MasterFunction masterAction = new MasterFunction(WGAGlobal.fetchInitialContext(Context.getCurrentContext()), false, function, params);
        masterAction.setParentObject(parentObject);
        masterAction.start();
        return masterAction.getReturnValue();
    }
    
    private Object executeFunction(Function function, boolean overrideScope, Object[] params, RhinoContext rcx, TMLScriptRootScope scope) {
        
        if (overrideScope) {
            function.setParentScope(scope);
        }
        return function.call(rcx, scope, function.getParentScope(), params);
    }
    
    private Object executeMethod(NativeObject parentObject, Function function, boolean overrideScope, Object[] params, RhinoContext rcx, TMLScriptRootScope scope) {
        
        if (overrideScope) {
            function.setParentScope(scope);
        }
        return function.call(rcx, scope, parentObject, params);
    }


    public Object fromString(String str) {

        try {
            int colonPos = str.indexOf(":");
            String typePrefix = str.substring(0, colonPos + 1);
            str = str.substring(colonPos + 1);
            
            // JSON Deserialisation                        
            if (typePrefix.equals(PREFIX_JSON)) {
                JsonParser parser = new JsonParser(Context.enter(), getOrCreateSharedScope(null, null));
                return parser.parseValue(str);
            }
            
            else {
                throw new RuntimeException("Unable to deserialize object with type prefix " + typePrefix);
            }
            
        }
        catch (Exception e) {
            throw new RuntimeException("Exception deserializing TMLScript object", e);
        }
        
        
    }

    public String toString(Object obj) {

        // JSON serialisation
       return PREFIX_JSON + NativeJSON.stringify(Context.enter(), _sharedScope, obj, null, null).toString();

        
    }

    public String convertScriptableToJson(Object obj) {
        
        Context cx = Context.enter();
        try {
            Callable replacer = new Callable() {

                public Object call(Context arg0, Scriptable scope, Scriptable thisObj, Object[] arg3) {
                    Object value = arg3[1];
                    
                    if (value == null) {
                        return null;
                    }
                    
                    if (value instanceof Wrapper) {
                        value = ((Wrapper) value).unwrap();
                    }
                    
                    // Keep valid JSON values intact
                    if (value instanceof Boolean || value instanceof Number) {
                        return value;
                    }
                    
                    // Prevent native objects from being interpreted as maps or lists
                    if (value instanceof Scriptable) {
                        return value;
                    }
                    
                    // Java date: converted to a ISO8601 date literal
                    else if (value instanceof Date) {
                        ISO8601DateFormat dateFormat = new ISO8601DateFormat();
                        return dateFormat.format(value);
                    }
                    
                    // TMLContext: convert to path expression
                    else if (value instanceof TMLContext) {
                        try {
                            return ((TMLContext) value).getpath();
                        }
                        catch (WGAPIException e) {
                            logger.error("Exception fetching path from TMLContext", e);
                        }
                    }
                    
                    // Lists: Convert to native array
                    else if (value instanceof List) {
                        NativeArray arr =  new NativeArray(((List<?>) value).toArray());
                        arr.setParentScope(scope);
                        return arr;
                    }
                    
                    // Maps: Convert to native object
                    else if (value instanceof Map) {
                        NativeObject nativeObject = new NativeObject();
                        nativeObject.setParentScope(scope);
                        for (Map.Entry<?,?> entry : ((Map<?,?>) value).entrySet()) {
                            nativeObject.put(String.valueOf(entry.getKey()), entry.getValue());
                        }
                        
                        return nativeObject;
                    }
                    
                    return value.toString();
                }
                
            };
            
            Object result = NativeJSON.stringify(cx, getSharedScope(), obj, replacer, 4);
            if (result != Undefined.instance) {
                return String.valueOf(result);
            }
            else {
                return null;
            }
            
        }
        finally {
            Context.exit();
        }
        
    }
    
    public Object convertJsonToScriptable(String json) {
        Context cx = Context.enter();
        try {
            Callable reviver = new Callable() {

                public Object call(Context arg0, Scriptable scope, Scriptable holder, Object[] arg3) {
                    
                    String name = String.valueOf(arg3[0]);
                    Object property = arg3[1];
                    
                    
                    Object value = arg3[1];
                    if (value instanceof NativeJavaObject) {
                        value = ((NativeJavaObject) value).unwrap();
                    }
                     
                    
                    TMLContext cx = TMLContext.getThreadMainContext();
                    if (cx != null) {
                        value = new JsonUtils(cx).jsonToJavaConversions(value);
                    }
                    
                    return value;
                    
                }
                
            };
            
            return NativeJSON.parse(cx, getSharedScope(), json, reviver);
        }
        finally {
            Context.exit();
        }        
    }
    
    public JsonValue convertJavaToJson(Object javaObj) {
        
        JsonValue val;
        if (javaObj == null) {
            return null;
        }
        else if (javaObj instanceof Map) {
            JsonObjectBuilder builder = Json.createObjectBuilder();
            for (Map.Entry<?,?> entry : ((Map<?,?>)javaObj).entrySet()) {
                builder.add(entry.getKey().toString(), convertJavaToJson(javaObj));
            }
            return builder.build();
        }
        else if (javaObj instanceof List) {
            JsonArrayBuilder builder = Json.createArrayBuilder();
            for (Object element: (List<?>) javaObj) {
                builder.add(convertJavaToJson(element));
            }
            return builder.build();
        }
        else if (javaObj instanceof Number) {
            JsonObjectBuilder builder = Json.createObjectBuilder();
            if (javaObj instanceof BigDecimal) {
                builder.add("value", ((BigDecimal) javaObj));
            }
            else if (javaObj instanceof Integer || javaObj instanceof Long) {
                builder.add("value", ((Number) javaObj).longValue());
            }
            else if (javaObj instanceof BigInteger) {
                builder.add("value", (BigInteger) javaObj);
            }
            else {
                builder.add("value", ((Number) javaObj).doubleValue());
            }
            return builder.build().get("value");
        }
        else if (javaObj instanceof Date) {
            return convertJavaToJson(ISO8601DateFormat.INSTANCE.format((Date) javaObj));
        }
        else if (javaObj instanceof Boolean) {
            JsonObjectBuilder builder = Json.createObjectBuilder();
            builder.add("value", (Boolean) javaObj);
            return builder.build().get("value");
        }
        else {
            JsonObjectBuilder builder = Json.createObjectBuilder();
            builder.add("value", String.valueOf(javaObj));
            return builder.build().get("value");
        }

        
    }

    public boolean canConvert(@SuppressWarnings("rawtypes") Class arg0) {
        return (Scriptable.class.isAssignableFrom(arg0));
    }
    
    @Override
    public Object createObject(final WGA wga, final TMLScript.ObjectType objectType, final TMLAction objectDefinition, final FunctionArgumentSubstitutor substitutor, final Map<String,Object> namedParams, final List<Object> unnamedParams) throws WGException {
        
        Object result = _contextFactory.call(new ContextAction() {

            @Override
            public Object run(final Context cx) {

                try {
                    String scriptName = "TMLScript object '" + objectDefinition.getModuleDatabase() + "/" + objectDefinition.getModuleName() + "'";
                    final TMLScriptObjectParentScope scope = getParentScopeForObjectType(objectType, objectDefinition);
                    return callObjectScript(cx, wga, objectDefinition, scriptName, new ObjectScript() {

                        @Override
                        public Object run(TMLScriptRootScope rootScope) throws WGException, IOException, ClassNotFoundException {
                            
                            ObjectCreator objectCreator = _objectCreatorManager.getObjectCreator(wga, (RhinoContext) cx, objectDefinition, scope);
                            Scriptable obj = objectCreator.createManagedObject((RhinoContext) cx, scope, rootScope.getWgaGlobal().getWga(), substitutor, null, new Object[] {});
                            enhanceObject(obj);
                            return obj;
                        }
                        
                        public boolean isIsolated() {
                            return TMLScriptIsolatedParentScope.isIsolated(scope);
                        };

                        
                    });

                }
                catch (WGException e) {
                    return e;
                }
                catch (Exception e) {
                    return new WGSystemException("Exception creating TMLScript object: " + objectDefinition.getDescription(), e);
                }
                
            }
            
        });
        
        if (result instanceof WGException) {
            throw (WGException) result;
        }
        else {
            return result;
        }
        
    }
    
    protected TMLScriptObjectParentScope getParentScopeForObjectType(TMLScript.ObjectType objectType, TMLAction objectDefinition) throws WGException {

        switch (objectType) {
            
            case V2_ISOLATED:
                return new TMLScriptIsolatedParentScope(objectDefinition, getSharedScope());
            
            case V2:
                return new TMLScriptModernObjectParentScope(objectDefinition, getSharedScope());
                
            default:
                return new TMLScriptLegacyObjectParentScope(_core, objectDefinition, objectDefinition.getModuleDatabase(), false);
        }
        
    }
    

    private void enhanceObject(Scriptable obj) {
        getSharedScope().getAddMethodsFunction().call(Context.getCurrentContext(), obj, null, new Object[] {obj});
    }
    
    
    public ExpressionResult callMethod(final WGA wga, final Object object, final String method, final FunctionArgumentSubstitutor substitutor, final Map<String,Object> namedParams, final List<Object> unnamedParams, final CallMethodConfig config) throws WGException {
    
        if (object == null) {
            throw new WGIllegalArgumentException("Cannot call method on null object");
        }
        
        Object result = _contextFactory.call(new ContextAction() {

            @Override
            public Object run(final Context cx) {

                
                Scriptable obj = (Scriptable) (object instanceof Scriptable ? object : ScriptRuntime.toObject(cx, getSharedScope(), object));
                TMLAction objectDefinition = WGAGlobal.getObjectDefinition(obj);
                TMLScriptObjectMetadata objectMd = WGAGlobal.getObjectMetaData(obj);
                
                String objectDescription = "Anonymous object";
                if (objectDefinition != null) {
                    objectDescription = objectDefinition.getModuleDatabase() + "/" + objectDefinition.getModuleName();
                }
                    
                Object result = null;
                for (Iterator<String> elements = WGUtils.deserializeCollection(method, ".").iterator(); elements.hasNext();) {
                    final String element = elements.next();
                    final Object prop = ScriptableObject.getProperty(obj, element);
                    if (prop instanceof Function && !(prop instanceof NativeJavaClass)) {
                        
                        if (objectMd != null) {
                            Method methodMd = objectMd.getMethods().get(element);
                            if (methodMd != null) {
                                if (!methodMd.isDirectAccess() && config.isDirectAccess()) {
                                    return new InvalidDirectAccessCallException(element, method, objectDescription);
                                }
                            }
                        }
                        
                        final Scriptable callObject = obj;
                        final Function function = (Function) prop;
                        String scriptName = "Call chain '" + method + "' on TMLScript object '" + objectDescription + "'";
                        
                        try {
                            result = callObjectScript(cx, wga, objectDefinition, scriptName, new ObjectScript() {

                                @Override
                                public Object run(TMLScriptRootScope rootScope) throws WGException, IOException, ClassNotFoundException {
                                    return invokeMethod(cx, function, callObject, rootScope, substitutor, namedParams, unnamedParams);
                                }
                                
                                
                                public boolean isIsolated() {
                                    
                                    if (ScriptableObject.hasProperty(function, RhinoScope.PROP_ISOLATED)) {
                                        return (Boolean) Context.jsToJava(ScriptableObject.getProperty(function, RhinoScope.PROP_ISOLATED), Boolean.class);
                                    }
                                    
                                    return TMLScriptIsolatedParentScope.isIsolated(callObject);
                                    
                                };
                                
                            });
                        }
                        catch (Exception e) {
                            if (element.equals(method)) {
                                return new WGExpressionException("Exception calling method/property '" + element + "' for object '" + objectDescription + "'", (String) ((Function) prop).getDefaultValue(String.class), e);
                            }
                            else {
                                return new WGExpressionException("Exception calling method/property '" + element + "' on call chain '" + method + "' for object '" + objectDescription + "'", (String) ((Function) prop).getDefaultValue(String.class), e);
                            }
                            
                        }
                    }
                    else {
                        if (config.isDirectAccess()) {
                            return new InvalidDirectAccessCallException(element, method, objectDescription);
                        }
                        result = prop;
                    }
                    
                    if (result == UniqueTag.NOT_FOUND) {
                        result = Undefined.instance;
                    }
                    
                    if (result != null && !Undefined.instance.equals(result)) {
                        obj = (Scriptable) (result instanceof Scriptable ? result : ScriptRuntime.toObject(cx, getSharedScope(), result));
                    }
                    else if (elements.hasNext()) {
                        return new InvalidReferenceOnCallChainException(element, method, objectDescription);
                    }
                }
                
                if (result instanceof NativeJavaObject) {
                    result = ((NativeJavaObject) result).unwrap();
                }
                
                return result;

            }

           
                
        });
    
        if (result instanceof WGExpressionException) {
            return createExpressionResult(null, (WGExpressionException) result);
        }
        else if (result instanceof Exception) {
            return createExpressionResult(null, new WGExpressionException("Exception executing object method", "(No code)"));
        }
        else {
            return createExpressionResult(result, null);
        }
            
        
        
    }
    
    private Object invokeMethod(final Context cx, final Function f, final Scriptable callObject, TMLScriptRootScope rootScope, FunctionArgumentSubstitutor substitutor, Map<String, Object> params, List<Object> unnamedParams) throws WGException {

        if (unnamedParams == null) {
            unnamedParams = Collections.emptyList();
        }
        
        
        try (MethodArguments arguments = getArgumentBuilder().buildArguments(f, rootScope.getWgaGlobal().getWga(), substitutor, params, unnamedParams.toArray())) {

            try {
            
                Object synced = ScriptableObject.getProperty(f,  RhinoScope.PROP_SYNCED);
                if (synced != null && synced.equals(Boolean.TRUE)) {
                    synchronized (callObject) {
                        return f.call(cx, rootScope, callObject, arguments.getArguments(callObject));
                    }
                }
                else {
                    return f.call(cx, rootScope, callObject, arguments.getArguments(callObject));
                }
            
            }
            catch (JavaScriptException e) {
                throw convertToTMLScriptException(e);
            }
            
            
        }
        
    }

    private Object callObjectScript(Context cx, WGA wga, TMLAction objectDefinition, String scriptName, ObjectScript script) throws IllegalAccessException, InstantiationException, ClassNotFoundException, WGException,
    InvocationTargetException, URISyntaxException, UnsupportedEncodingException, IOException, SecurityException, NoSuchMethodException {
        
        boolean isolated = script.isIsolated();

        // The TML context for the WGA global. Must not be isolated.
        wga = Unlocker.unlock(wga);
        TMLContext scriptTMLContext;
        if (wga.isTMLContextAvailable()) {
            if (objectDefinition != null) {
                scriptTMLContext = ((TMLContext) wga.tmlcontext()).designContext(objectDefinition.getDesignReference().toString());
            }
            else  {
                scriptTMLContext = (TMLContext) wga.tmlcontext();
            }
        }
        else {
            if (objectDefinition != null) {
                scriptTMLContext = (TMLContext) wga.createTMLContext(wga.db(objectDefinition.getModuleDatabase()), wga.design(objectDefinition.getDesignReference()));
            }
            else {
                throw new WGIllegalStateException("Cannot call method on a generic JavaScript object without a WebTML context available, as we cannot determine a design context");
            }
        }
        
        // The TML context of the root scope. Must be manually isolated here in case.
        TMLContext rootScopeTMLContext = scriptTMLContext;
        if (isolated) {
            rootScopeTMLContext =  rootScopeTMLContext.toIsolatedVersion();
        }
        
        TMLScriptRootScope rootScope = createRootScope(rootScopeTMLContext, (RhinoContext) cx, isolated);
        ThreadLocalPreserver preserver =  new ThreadLocalPreserver((RhinoContext) cx);
        
        preserver.preserve(TL_ROOTSCOPE, rootScope);
        preserver.preserve(TL_ACTIONDEFINITION, objectDefinition);
        preserver.preserve(TL_ACTIONLOCATOR, null);
        preserver.preserve(TL_SCRIPTNAME, scriptName);
        ErrorReporter oldErrorReporter = cx.setErrorReporter(rootScope);
        Session wsSession = wga.session().isWebSocketSessionAvailable() ? wga.session().getJavaWebSocketSession() : null;
        preserver.preserve(TL_WGACONTEXT, new TMLScriptWGAContext(scriptTMLContext, wsSession, isolated));
        scriptTMLContext.makeThreadMainContext();
        try {
            return script.run(rootScope);
        }
        finally {
            preserver.restore();
            cx.setErrorReporter(oldErrorReporter);
            scriptTMLContext.removeThreadMainContext();
        }
    }

    private TMLScriptRootScope createRootScope(TMLContext tmlContext, RhinoContext rcx, boolean isolated) throws IllegalAccessException, InstantiationException, InvocationTargetException, URISyntaxException,
            UnsupportedEncodingException, IOException, SecurityException, NoSuchMethodException {
        TMLScriptRootScope scope;
        TMLScriptRootScopeData scopePrototype = new TMLScriptRootScopeData(getOrCreateSharedScope(rcx, tmlContext.getwgacore()));
        scope = new TMLScriptRootScope(scopePrototype, tmlContext, isolated);
        return scope;
    }
    
    @Override
    public Object provideGlobal(WGA wga, TMLScriptGlobal global) throws WGException {
        return global.provide(wga);
    }
    
    @Override
    public TMLScriptGlobal createGlobal(String name, int type, Object ref) throws WGException {
        return TMLScriptGlobalCreator.createGlobal(name, type, ref);
    }
    
    @Override
    public SerializedScriptObject serializeScriptable(TMLContext context, Object scriptable) throws WGException {
        Map<String,Object> objects = new HashMap<String, Object>();
        objects.put("toBeSerialized", scriptable);
        ExpressionResult result = evaluateExpression("runtime.serialize(toBeSerialized)", context, TYPE_EXPRESSION, objects);
        if (!result.isError()) {
            return (SerializedScriptObject) result.getResult();
        }
        else if (result.getException() != null) {
            throw result.getException();
        }
        else {
            throw new WGExpressionException("Unknown exception serializing TMLScript object", "(No source)");
        }
    }
    
    public SerializedScriptObject serialize(Object scriptable) throws WGException {
        try {
            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
            ScriptableOutputStream out = new TMLScriptOutputStream(byteOut, getSharedScope());
            out.writeObject(scriptable);
            out.flush();
            out.close();
            
            return new SerializedScriptObject(byteOut.toByteArray());
        }
        catch (IOException e) {
            throw new WGIllegalArgumentException("Exception serializing script object", e);
        }
    }
    
    protected Scriptable deserialize(SerializedScriptObject serialized) throws IOException, ClassNotFoundException {
        
        ScriptableInputStream in = new TMLScriptInputStream(new ByteArrayInputStream(serialized.getData()), getSharedScope());
        Scriptable obj = (Scriptable) in.readObject();
        in.close();
        
        return obj;
        
    }
    
    public TMLScriptRootScope getRootScope() {
        return WGAGlobal.fetchRootScope(Context.getCurrentContext());
    }
    
    @Override
    public JsonElement extractProperty(Object controllerObj, String prop) {
        Scriptable controller = (Scriptable) controllerObj;
        Object state = ScriptableObject.getProperty(controller, prop);
        if (state instanceof Scriptable) {
            return new com.google.gson.JsonParser().parse(convertScriptableToJson(state));
        }
        else {
            return null;
        }
    }
    
    public boolean hasProperty(Object controllerObj, String prop) {
        
        if (!(controllerObj instanceof Scriptable)) {
            return false;
        }
        
        Scriptable controller = (Scriptable) controllerObj;
        return ScriptableObject.hasProperty(controller, prop);
    }
    
    @Override
    public void injectProperty(Object controllerObj, String prop, JsonElement state) {
        Scriptable controller = (Scriptable) controllerObj;
        controller.put(prop, controller, convertJsonToScriptable(state.toString()));
    }
    
    public Object invoke(Scriptable obj, String method, Object params) throws WGException {
        TMLScriptRootScope rootScope = WGAGlobal.fetchRootScope(Context.getCurrentContext());
        Map<String,Object> namedParams = new HashMap<String, Object>();
        if (params instanceof Map<?,?>) {
            for (Map.Entry<?,?> entry : ((Map<?,?>) params).entrySet()) {
                namedParams.put(String.valueOf(entry.getKey()), entry.getValue());
            }
        }
        ExpressionResult res = callMethod(rootScope.getData().getRhinoScope().getWgaGlobal().getWga(), obj, method, new WebTMLFunctionArgumentSubstitutor(rootScope.getWgaGlobal().getWga()), namedParams, Collections.<Object>emptyList(), new CallMethodConfig());
        if (res.isError()) {
            throw res.getException();
        }
        else {
            return res.getResult();
        }
    }
    
    public ObjectCreatorManager getObjectCreatorManager() {
        return _objectCreatorManager;
    }
    
    public ExpressionResult createExpressionResult(Object result, WGExpressionException e) {
        
        boolean isFalse = true;
        boolean isTrue = false;
        if (result != null) {
            if (result instanceof Boolean) {
                isTrue = (Boolean) result;
                isFalse = !isTrue;
            }
            else {
                isTrue = false;
                isFalse = isJSFalse(result);
            }
        }
        
        return new ExpressionResult(result, isTrue, isFalse, e);
        
    }
    
    public String objectToString(Scriptable obj) {
        TMLAction action = WGAGlobal.getObjectDefinition(obj);
        if (action != null) {
            return "TMLScript Object '" + action.getModuleDatabase() + "/" + action.getModuleName() + "'";
         }
        else {
            return "Object";
        }
    }

    @Override
    public TMLScriptObjectMetadata getTmlscriptObjectMetadata(WGA wga, Design design) throws  WGException {

        Context cx = Context.enter();
        try {
            return DesignLocator.getObjectMetaData(wga, design);
        }
        catch (WGException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGIllegalArgumentException("Exception retrieving TMLScript global metadata", e);
        }
        finally {
            Context.exit();
        }
        
    }


    
    @SuppressWarnings("unchecked")
    @Override
    public <T> T descriptify(Object obj, Class<T> expectedType, DescriptificationConfig config) throws WGException {
        try {
            
            if (obj == null || obj == Undefined.instance) {
                return null;
            }
            
            if (obj instanceof Scriptable) {
            
                // Descriptify simple JS Objects to Map of descriptified data
                if (obj instanceof NativeObject) {
                    
                    NativeObject nativeObject = (NativeObject) obj;
                    
                    // Special case: If we want a TMLContext, we look for a ContextWrapper in the prototype chain (#00004574)
                    if (expectedType == Context.class || expectedType == TMLContext.class) {
                        
                        Scriptable proto = nativeObject;
                        while (true) {
                            proto = nativeObject.getPrototype();
                            if (proto == null) {
                                break;
                            }
                            if (proto instanceof ContextWrapper) {
                                obj = ((ContextWrapper) obj).unwrap();
                                break;
                            }
                        }
                        
                    }
                    
                    // Do not descriptify TMLScript objects
                    else if (WGAGlobal.getObjectDefinition(nativeObject) != null) {
                        // Do nuffin
                    }
                    
                    // Descriptify simple JS Objects to either Maps or JsonObjects
                    else {        
                        
                        if (config.getObjectMode() == ObjectMode.TO_JAVA_COLLECTIONS) {
                            Map<Object,Object> map = new HashMap<Object,Object>();
                            for (Map.Entry<String,Object> entry : nativeObject.entrySet()) {
                                map.put(entry.getKey(), descriptify(entry.getValue(), Object.class, config));
                            }
                            obj = map;
                        }
                        else if (config.getObjectMode() == ObjectMode.TO_JSON) {
                            obj = new com.google.gson.JsonParser().parse(convertScriptableToJson(nativeObject));
                        }
                        else if (config.isForceDescriptification()) {
                            throw new WGIllegalDataException("Unconvertible script value type: " + obj.getClass().getName());
                        }
                    }
                    
                }
                
                // Descriptify simple JS Arrays to List of descriptified data or to JsonObjects        
                else if (obj instanceof NativeArray) {
                    
                    if (config.getObjectMode() == ObjectMode.TO_JAVA_COLLECTIONS) {
                        List<Object> list = new ArrayList<Object>();
                        for (Object elem : (NativeArray) obj) {
                            list.add(descriptify(elem, Object.class, config));
                        }
                        obj = list;
                    }
                    else if (config.getObjectMode() == ObjectMode.TO_JSON) {
                        obj = new com.google.gson.JsonParser().parse(convertScriptableToJson(obj));
                    }
                    else if (config.isForceDescriptification()) {
                        throw new WGIllegalDataException("Unconvertible script value type: " + obj.getClass().getName());
                    }
                    
                }
                
                // Unwrap native java objects
                else if( obj instanceof NativeJavaObject) {
                    obj = descriptify(((NativeJavaObject) obj).unwrap(), expectedType, config);
                }
                
                // Native generators can be converted to be (one-shot) scriptables
                else if (obj instanceof NativeGenerator && (expectedType == Iterable.class)) {
                 
                    final NativeGenerator generator = (NativeGenerator) obj;
                    obj = new Iterable<Object>() {
                        @Override
                        public Iterator<Object> iterator() {
                            return new NativeJS17ToJavaIterator(generator);
                        }
                    };
                    
                }
                
                // Native iterators can be converted to be Java iterators        
                else if (obj instanceof NativeIterator && (expectedType == Iterator.class)) {
                    obj = new NativeJS17ToJavaIterator((NativeIterator) obj);
                }
                
                else {
                    if (config.isForceDescriptification()) {
                        throw new WGIllegalDataException("Unconvertible script value type: " + obj.getClass().getName());
                    }
                }
            }
            
            if (expectedType.isAssignableFrom(obj.getClass())) {
                return (T) obj;
            }
            else {
                throw new WGIllegalDataException("Value is not compatible with expected type " + expectedType.getClass().getName() + ": " + obj.getClass().getName());
            }
        }
        catch (EvaluatorException e) {
            throw new WGIllegalDataException("Exception descriptifying value", e);
        }
        
    }

    @Override
    public Object scriptify(Object obj, Object scopeObj) {
        
        Context.enter();
        try {
            if (obj == null) {
                return Undefined.instance;
            }
            
            if (!(scopeObj instanceof Scriptable)) {
                throw new IllegalArgumentException("Scope object is no scriptable: " + scopeObj);
            }
            
            Scriptable scope = (Scriptable) scopeObj;
    
            if (obj instanceof Scriptable) {
                return obj;
            }
            else if (obj instanceof JsonElement) {
                return convertJsonToScriptable(((JsonElement) obj).toString());
            }
            
            else {
                return Context.javaToJS(obj, scope);
            }
        }
        finally {
            Context.exit();
        }
        
        
    }
    
    @Override
    public Class<?> getScriptableType() {
        return Scriptable.class;
    }

    public TMLScriptObjectMetadataParser getObjectMetadataParser() {
        return _objectMetadataParser;
    }


    @Override
    public Object getUndefined() {
        return Undefined.instance;
    }


    @Override
    public boolean scriptableEquals(Object o1, Object o2) {
        return ScriptRuntime.eq(o1, o2);
    }

}

