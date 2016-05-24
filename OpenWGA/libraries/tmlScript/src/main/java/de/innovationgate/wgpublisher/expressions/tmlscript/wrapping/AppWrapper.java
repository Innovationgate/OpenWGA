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

package de.innovationgate.wgpublisher.expressions.tmlscript.wrapping;

import java.io.IOException;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.ManagedGlobalConfig;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser;
import de.innovationgate.wgpublisher.expressions.tmlscript.VarArgParser.Arguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;

public class AppWrapper extends ScriptableObject implements Wrapper {
    
    public static final String[] METHODS = {
        "managedGlobal",
    };
    
    private static VarArgParser _managedGlobalVarargs;
    static {
        _managedGlobalVarargs = (new VarArgParser("managedGlobal"))
                .add("name", String.class, false)
                .add("ref", new Class[] {de.innovationgate.wga.server.api.Design.class, Function.class}, false)
                .add("params", NativeObject.class, true)
                .pack();
    }
    
    private App _app;
    
    public AppWrapper(Scriptable scope, App app) {
        defineFunctionProperties(METHODS, AppWrapper.class, DONTENUM);
        _app = app;
        setPrototype(new NativeJavaObject(scope, app, App.class));
    }

    @Override
    public String getClassName() {
        return "App";
    }

    @Override
    public Object unwrap() {
        return _app;
    }
    
    public static void managedGlobal(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, ClassNotFoundException, IllegalArgumentException, IOException {
        ((AppWrapper) thisObj).instanceGetGlobal(WGAGlobal.fetchInitialContext(cx), _managedGlobalVarargs.parse(args));
    }

    private void instanceGetGlobal(TMLContext cx, Arguments args) throws WGException, ClassNotFoundException, IOException {

        Object ref = args.get("ref");
        Design targetDesign;
        if (ref instanceof Design) {
            targetDesign = (Design) ref;
            if(DesignLocator.getConstructor(WGA.get(), targetDesign)==null){
        		throw new WGException("Constructor " + targetDesign.getResourceName() + "() not found in TMLScript module " + targetDesign.getBaseReference().toString());
        	}        
        }
        else {
            TMLAction objectDefinition = WGAGlobal.getObjectDefinition((Function) ref);
            targetDesign = WGA.get(cx).design().resolve(objectDefinition.getDesignReference());
        }
        
        ManagedGlobalConfig config = new ManagedGlobalConfig();
        NativeObject params = (NativeObject) args.get("params");
        if (params != null) {
            if (ScriptableObject.hasProperty(params, "scope")) {
                config.setScope((ObjectScope) Context.jsToJava(ScriptableObject.getProperty(params, "scope"), ObjectScope.class));
            }
            if (ScriptableObject.hasProperty(params, "isolated")) {
                config.setIsolated((Boolean) Context.jsToJava(ScriptableObject.getProperty(params, "isolated"), Boolean.class));
            }
            if (ScriptableObject.hasProperty(params, "createOnEvent")) {
                config.setCreateOnEvent((Boolean) Context.jsToJava(ScriptableObject.getProperty(params, "createOnEvent"), Boolean.class));
            }
        }
        
        _app.managedGlobal((String) args.get("name"), targetDesign, config);    
        
    }

}
