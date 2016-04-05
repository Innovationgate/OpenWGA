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
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;

import de.innovationgate.wgpublisher.events.ContentTypeEvent;

public class EventWrapper extends NativeJavaObject {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private ContentTypeEvent event;

	public EventWrapper(Scriptable scope, ContentTypeEvent event) {
		super(scope, event, ContentTypeEvent.class);
		this.event = event;
	}

	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.EventWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {

		String argLC = arg0.toLowerCase();
		if (this.has(argLC, arg1)) {
			return RhinoWrapFactory.notFoundToNull(super.get(argLC, arg1));
		}
		else {
			return Context.javaToJS(this.event.getContextObjects().get(argLC), arg1);
		}
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#put(java.lang.String, de.innovationgate.ext.org.mozilla.javascript.Scriptable, java.lang.Object)
	 */
	public void put(String name, Scriptable start, Object value) {
		this.event.getContextObjects().put(name, Context.jsToJava(value, Object.class));
	}

}
