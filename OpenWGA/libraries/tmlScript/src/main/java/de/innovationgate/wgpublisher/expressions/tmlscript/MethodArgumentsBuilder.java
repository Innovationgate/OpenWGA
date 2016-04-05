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
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.Undefined;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.expressions.tmlscript.serialisation.SerializedScriptObject;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class MethodArgumentsBuilder {
    
    public class MethodArguments implements AutoCloseable {
        
        List<Object> _arguments = new ArrayList<Object>();
        List<Runnable> _cleanups = new ArrayList<Runnable>();
        private WGA _wga;
        public MethodArguments(WGA wga) {
            _wga = wga;
        }
        
        public void add(Object argument) throws WGException, ClassNotFoundException, IOException {
            add(argument, null);
        }
        
        public void add(Object argument, Runnable cleanup) throws WGException, ClassNotFoundException, IOException {
            
            if (argument instanceof SerializedScriptObject) {
                argument = _engine.deserialize((SerializedScriptObject) argument);
            }
            if (argument instanceof TMLContext && _wga.isIsolated()) {
                argument = ((TMLContext) argument).toIsolatedVersion();
            }
            
            _arguments.add(argument);
            if (cleanup != null) {
                _cleanups.add(cleanup);
            }
        }
        
        public Object[] getArguments(Scriptable parentScope) {
            List<Object> scriptableArguments = new ArrayList<Object>();
            for (Object arg : _arguments) {
                if (arg != null) {
                    arg = Context.javaToJS(arg, parentScope);
                }
                scriptableArguments.add(arg);
            }
            
            return scriptableArguments.toArray();
        }
    
        @Override
        public void close() {
            for (Runnable r : _cleanups) {
                r.run();
            }
        }
        
        
    }

    private RhinoExpressionEngineImpl _engine;
    public static final Pattern REGEX_ARGUMENT_NAMES = Pattern.compile("([^\\s,]+)");
    public static final Pattern REGEX_STRIP_COMMENTS = Pattern.compile("/((\\/\\/.*$)|(\\/\\*[\\s\\S]*?\\*\\/))", Pattern.MULTILINE);
    private static final String PROP_ARGUMENT_NAMES = "$argumentNames";

    public MethodArgumentsBuilder(RhinoExpressionEngineImpl engine) {
        _engine = engine;
    }

    public MethodArguments buildArguments(final Function f, WGA wgaParam, FunctionArgumentSubstitutor substitutor, Map<String, Object> namedParams, Object[] unnamedParams) throws WGException {
        
        
        
        // Determine argument names, either from function source or cached as function prop
        List<String> argumentNames;
        if (f.has(PROP_ARGUMENT_NAMES, f)) {
            @SuppressWarnings("unchecked")
            List<String> list = (List<String>) f.get(PROP_ARGUMENT_NAMES, f);
            argumentNames = list;
        }
        else {
            String fSource = (String) f.getDefaultValue(String.class);
            String strippedSource = REGEX_STRIP_COMMENTS.matcher(fSource).replaceAll("");
            String insideBrackets = strippedSource.substring(strippedSource.indexOf("(") + 1, strippedSource.indexOf(")"));
            Matcher argumentMatcher = REGEX_ARGUMENT_NAMES.matcher(insideBrackets);
            argumentNames = new ArrayList<String>();
            while (argumentMatcher.find()) {
                argumentNames.add(argumentMatcher.group());
            }
            f.put(PROP_ARGUMENT_NAMES, f, argumentNames);
        }
        
        // Build argument values
        MethodArguments arguments = new MethodArguments(wgaParam);
        int unnamedParamsIdx = 0;
        
        final WGA wga = Unlocker.unlock(wgaParam);
        for (String argumentName : argumentNames) {
            
            try {
            
                // First the named params are checked
                if (namedParams != null && namedParams.containsKey(argumentName)) {
                    arguments.add(namedParams.get(argumentName));
                    continue;
                }
                
                // Secondly, look if the substitutor knows the name
                if (substitutor != null) {
                    Object value = substitutor.getArgumentValue(argumentName);
                    if (value != null) {
                        arguments.add(value);
                        continue;
                    }
                }
                
                // Third try: Globals
                final TMLScriptGlobal global = wga.getCore().getTmlscriptGlobalRegistry().getGlobal(argumentName, wga.design().db());
                if (global != null) {
                    arguments.add(_engine.provideGlobal(wga, global), new Runnable() {
                        @Override
                        public void run() {
                            global.afterProvisioning(wga);
                        }
                    });
                    continue;
                }
                
                // Fourth try: Look if we have an unnamed param that may get injected here
                if (unnamedParams != null && unnamedParams.length > unnamedParamsIdx) {
                    arguments.add(unnamedParams[unnamedParamsIdx++]);
                    continue;
                }
                
                // Nothing worked, we insert undefined
                arguments.add(Undefined.instance);
            
            }
            catch (Exception e) {
                RhinoExpressionEngineImpl.logger.error("Exception injecting TMLScript method argument: " + argumentName, e);
            }
        }
        
        return arguments;
    }

}
