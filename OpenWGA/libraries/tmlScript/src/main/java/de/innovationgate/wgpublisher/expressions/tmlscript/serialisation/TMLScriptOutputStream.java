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

package de.innovationgate.wgpublisher.expressions.tmlscript.serialisation;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.Date;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.Wrapper;
import de.innovationgate.ext.org.mozilla.javascript.serialize.ScriptableOutputStream;

public class TMLScriptOutputStream extends ScriptableOutputStream {
    
    public interface SerializedObject<Type> extends Serializable {
        public Type deserialize(Scriptable scope);
    }
    
    public static class SerializedDate implements SerializedObject<Date> {
        private long _time;

        public Date deserialize(Scriptable scope) {
            return new Date(_time);
        }

        public SerializedDate(Date date) {
            super();
            _time = date.getTime();
        }
    }
    
    public static class SerializedFunction implements SerializedObject<Function> {
        
        private String _code;

        public SerializedFunction(Function f) {
            _code = (String) f.getDefaultValue(String.class);
        }
        
        @Override
        public Function deserialize(Scriptable scope) {
            return Context.getCurrentContext().compileFunction(scope, _code, null, 1, null);
        }
        
    }

    public TMLScriptOutputStream(OutputStream out, Scriptable scope) throws IOException {
        super(out, scope);
        addExcludedName("runtime.rootScope");
    }
    
    @Override
    protected Object replaceObject(Object obj) throws IOException {
        
        if (obj instanceof Wrapper) {
            obj = ((Wrapper) obj).unwrap();
        }

        if (obj instanceof Date) {
            return new SerializedDate((Date) obj);
        }
        if (obj instanceof Function) {
            return new SerializedFunction((Function) obj);
        }
        
        
        return super.replaceObject(obj);
    }

}
