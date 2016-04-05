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

import java.util.Map;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.JavaScriptException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.Design;

public class TMLPageWrapper extends ScriptableObject implements Wrapper {
    
    public static final String[] METHODS = {
        "render",
    };
    
    
    private TMLPage _page;
    
    public TMLPageWrapper(Scriptable scope, TMLPage page) {
        super(scope, new NativeJavaObject(scope, page, TMLPage.class));
        defineFunctionProperties(METHODS, TMLPageWrapper.class, DONTENUM);
        _page = page;
    }

    @Override
    public String getClassName() {
        return "TMLPage";
    }

    @Override
    public Object unwrap() {
        return _page;
    }
    

    public static void render(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws JavaScriptException, WGException, NoSuchMethodException, SecurityException {
        ((TMLPageWrapper) thisObj).instanceRender(cx, thisObj, args, funObj);
    }

    @SuppressWarnings("unchecked")
    private void instanceRender(Context cx, Scriptable thisObj, java.lang.Object[] args, Function funObj) throws WGException, JavaScriptException, NoSuchMethodException, SecurityException {
        
        // Only the version with a single map as argument is treated here, all others redirected to the NativeJavaObject
        if (args.length != 1 || !(args[0] instanceof Map)) {
            Function renderMethod = (Function) ScriptableObject.getProperty(getPrototype(), "render");
            renderMethod.call(cx, thisObj, thisObj, args);
            return;
        }
        
        Map<Object,Object> config = (Map<Object,Object>) args[0];
        Design design = Design.toDesign(config.get("design"), getParentScope());
        
        _page.render(
                design != null ? design.getApiDesign() : null,
                (String) config.get("medium"), 
                (de.innovationgate.wga.server.api.tml.Context) config.get("context"), 
                (Map<Object,Object>) config.get("options")
        );
    }

}
