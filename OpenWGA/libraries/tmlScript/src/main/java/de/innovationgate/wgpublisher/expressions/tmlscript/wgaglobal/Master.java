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

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.WrappedException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@CodeCompletion(delegate=de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.cc.Master.class)
public class Master extends ScriptableObject {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private static VarArgParser _runMasterFunctionVarargs;
    private static VarArgParser _runMasterMethodVarargs;
    static {
        _runMasterFunctionVarargs = (new VarArgParser("runMasterFunction"))
        .add("context", TMLContext.class, true)
        .add("function", Function.class, false)
        .pack();
        _runMasterFunctionVarargs.setUnwrapOverflowArgs(true);
        
        _runMasterMethodVarargs = (new VarArgParser("runMasterMethod"))
        .add("object", NativeObject.class, true)
        .add("function", Function.class, false)
        .pack();
        _runMasterMethodVarargs.setUnwrapOverflowArgs(true);
    }

    protected Master(Scriptable scope) {
        super(scope, null);
        String[] functions = { "runFunction", "runMethod" };
        defineFunctionProperties(functions, Master.class, DONTENUM);
        sealObject();
        
    }
    
    public static Object runFunction(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        try {
            RhinoExpressionEngineImpl runtime = WGAGlobal.fetchRuntime(cx);
            VarArgParser.Arguments parsedArgs = _runMasterFunctionVarargs.parse(args);
            
            Function function = (Function) parsedArgs.get("function");
            TMLContext context = (TMLContext) parsedArgs.get("context");
            if (context == null) {
                context = WGAGlobal.fetchInitialContext(Context.getCurrentContext());
            }
            
            Object returnValue = runtime.runAsMaster(function, context, parsedArgs.getOverflowArgs().toArray());
            if (returnValue != null) {
                return Context.javaToJS(returnValue, thisObj);
            }
            else {
                return null;
            }
        }
        catch (Exception e) {
            throw new WrappedException(e);
        }
    }
    
    public static Object runMethod(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) {
        try {
            RhinoExpressionEngineImpl runtime = WGAGlobal.fetchRuntime(cx);
            VarArgParser.Arguments parsedArgs = _runMasterMethodVarargs.parse(args);
            
            Function function = (Function) parsedArgs.get("function");
            NativeObject obj = (NativeObject) parsedArgs.get("object");
            Object returnValue;
            if (obj != null) {
                returnValue = runtime.runAsMasterMethod(obj, function, parsedArgs.getOverflowArgs().toArray());
            }
            else {
                returnValue = runtime.runAsMaster(function, null, parsedArgs.getOverflowArgs().toArray());
            }
            
            if (returnValue != null) {
                return Context.javaToJS(returnValue, thisObj);
            }
            else {
                return null;
            }
            
        }
        catch (Exception e) {
            throw new WrappedException(e);
        }
    }

    @Override
    public String getClassName() {
        return "Master";
    }
    
    
}