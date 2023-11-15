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

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;

public class SynchronizedFunction extends ScriptableObject implements Function {
    
	private static final long serialVersionUID = 1L;
	private Object _syncObj;
    private Function _function;

    public SynchronizedFunction(Function funcObj, Object syncObj, Scriptable scope) {
        super(scope, funcObj);
        _function = funcObj;
        if (syncObj instanceof NativeJavaObject) {
            _syncObj = ((NativeJavaObject) syncObj).unwrap();
        }
        else {
            _syncObj = syncObj;
        }
    }

    public Object call(Context cx, Scriptable scope, Scriptable thisObj, Object[] args) {
        synchronized (_syncObj) {
            return _function.call(cx, scope, thisObj, args);
        }
    }

    @Override
    public Scriptable construct(Context cx, Scriptable scope, Object[] args) {
        synchronized (_syncObj) {
            return _function.construct(cx, scope, args);
        }
    }

    @Override
    public String getClassName() {
        return "SynchronizedFunction";
    }
    
    @Override
    public Object getDefaultValue(Class<?> typeHint) {
        return _function.getDefaultValue(typeHint);
    }

}
