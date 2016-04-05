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
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.wgpublisher.events.ApplicationEvent;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;

public class ApplicationEventWrapper extends NativeObject implements Wrapper {
    
    private ApplicationEvent _event;

    public ApplicationEventWrapper(Scriptable scope, ApplicationEvent event)  {
        setParentScope(scope);
        try {
            _event = event;
            setPrototype(new NativeJavaObject(scope, _event, ApplicationEvent.class));
            
            Scriptable params = new NativeObject();
            params.setParentScope(scope);
            for (Map.Entry<Object,Object> param : _event.getParams().entrySet()) {
                ScriptableObject.putProperty(params, String.valueOf(param.getKey()), RhinoExpressionEngineImpl.get().scriptify(param.getValue(), params));
            }
            put("params", params);
        }
        catch (Exception e) {
            Context.throwAsScriptRuntimeEx(e);
        }
    }

    @Override
    public String getClassName() {
        return "ApplicationEvent";
    }
    
    @Override
    public Object unwrap() {
        return _event;
    }


}
