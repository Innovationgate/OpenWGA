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

import java.util.Date;

import org.dom4j.Element;
import org.dom4j.Node;

import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.ScriptRuntime;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;

public class NodeWrapper extends NativeJavaObject {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Node _node = null;
	private Element _element = null;

	/**
	 * @param scope
	 * @param obj
	 */
	public NodeWrapper(Scriptable scope, Node obj) {
	
		super(scope, obj, de.innovationgate.wgpublisher.webtml.utils.TMLContext.class);
		_node = obj;
		if (_node instanceof Element) {
			_element = (Element) _node;
		}
	
	} /* (Kein Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {

		if (obj instanceof String) {
			return (obj.equals(toString()));
		}
		else if (_element != null) {
			Object data = _element.getData();
			if ((obj instanceof Number && data instanceof Number) ||
				(obj instanceof Date && data instanceof Date)) {
				return obj.equals(data);
			}
			else {
				return _node.equals(obj);
			}
		}
		else {
			return _node.equals(obj);
		}

	}

	/* (Kein Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		if (_element != null) {
			return _element.getStringValue();
		}
		else {
			return _node.getText();
		}
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getDefaultValue(java.lang.Class)
	 */
	public Object getDefaultValue(Class<?> hint) {

		if (hint == null || hint == ScriptRuntime.StringClass)
			return toString();

		Object data = _element.getData();
		if ((hint == ScriptRuntime.BooleanClass && data instanceof Boolean)
			|| (hint == ScriptRuntime.NumberClass && data instanceof Number)) {
			return data;
		}

		return null;

	}

}
