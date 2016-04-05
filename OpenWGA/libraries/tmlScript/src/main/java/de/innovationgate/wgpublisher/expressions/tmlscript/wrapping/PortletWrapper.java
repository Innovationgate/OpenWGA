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

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;

public class PortletWrapper extends de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private TMLPortlet portlet;
	
	public PortletWrapper(Scriptable scope, TMLPortlet portlet) {
		super(scope, portlet, TMLPortlet.class);
		this.portlet = portlet;
	}
	
	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.PortletWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {
		try {
    		String argLC = arg0.toLowerCase();
    		if (this.has(argLC, arg1)) {
    			return RhinoWrapFactory.notFoundToNull(super.get(argLC, arg1));
    		}
    		else {
    			char first = arg0.charAt(0);
    			if (first >= 'A' && first <= 'Z') {
    				return Context.javaToJS(this.portlet.meta(arg0), arg1);
    			}
    			else {
    				return Context.javaToJS(this.portlet.item(arg0), arg1);
    			}
    		}
        } catch (WGAPIException e) {
            RhinoContext.throwAsScriptRuntimeEx(e);
            return null;
        }
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#put(java.lang.String, de.innovationgate.ext.org.mozilla.javascript.Scriptable, java.lang.Object)
	 */
	public void put(String name, Scriptable start, Object value) {
		if (super.has(name.toLowerCase(), start)) {
            super.put(name.toLowerCase(), start, value);
        }
		else {
			try {
	            this.portlet.setitem(name, Context.jsToJava(value, Object.class));
	        }
	        catch (WGAPIException e) {
	            throw Context.throwAsScriptRuntimeEx(e);
	        }
        }
	}
 
}

