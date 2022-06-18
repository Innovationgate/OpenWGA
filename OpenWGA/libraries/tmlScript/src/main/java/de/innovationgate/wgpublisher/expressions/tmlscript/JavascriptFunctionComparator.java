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

import java.util.Comparator;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Function;

public class JavascriptFunctionComparator implements Comparator<Object> {

    private Function _function;

    public JavascriptFunctionComparator(Function function) {
        _function = function;
    }

    public int compare(Object arg0, Object arg1) {
        
        Object value = _function.call(Context.getCurrentContext(), _function.getParentScope(), null, new Object[] {arg0, arg1});
        value = Context.jsToJava(value, Number.class);
        
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        else {
            return 0;
        }
        
    }

}
