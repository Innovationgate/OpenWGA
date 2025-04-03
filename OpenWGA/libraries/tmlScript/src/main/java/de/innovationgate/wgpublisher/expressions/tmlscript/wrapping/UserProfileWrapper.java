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
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.RhinoScope;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class UserProfileWrapper extends de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile profile;
    private Scriptable scope;
	
	public UserProfileWrapper(Scriptable scope, TMLUserProfile profile) {
		super(scope, profile, de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile.class);
		this.scope = scope;
		this.profile = profile;
	}
	
	/**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return "de.innovationgate.wgpublisher.expressions.UserProfileWrapper";
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {
		try {
    		String argLC = arg0.toLowerCase();
            // Only use scope if it is RhinoScope (so this is the top level object of the script)
            if (scope instanceof RhinoScope && scope.has(arg0, arg1)) {
                return scope.get(arg0, arg1);
            }
            else if (super.has(argLC, arg1)) {
            	// compatible with older version: try lower case 
                return RhinoWrapFactory.notFoundToNull(super.get(argLC, arg1));
            }
            else if (super.has(arg0, arg1)) {
            	// preferred: exact case
    			return RhinoWrapFactory.notFoundToNull(super.get(arg0, arg1));
    		}
    		else {
    			char first = arg0.charAt(0);
    			if (first >= 'A' && first <= 'Z') {
    				return Context.javaToJS(this.profile.meta(arg0), arg1);
    			}
    			else {
    				return Context.javaToJS(this.profile.item(arg0), arg1);
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
	    
		try {
            this.profile.setitem(name, Context.jsToJava(value, Object.class));
        }
        catch (WGAPIException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
	} 
	
    public Object get(int arg0, Scriptable arg1) {
        Object result = super.get(arg0, arg1);
        if (result != null) {
            return result;
        }
        else {
            return scope.get(arg0, arg1);
        }
    }
    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#has(int, de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public boolean has(int arg0, Scriptable arg1) {
        return super.has(arg0, arg1) || (scope instanceof RhinoScope && scope.has(arg0, arg1));
    }
    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#has(java.lang.String, de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public boolean has(String arg0, Scriptable arg1) {
        return super.has(arg0.toLowerCase(), arg1) || (scope instanceof RhinoScope && scope.has(arg0, arg1));
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#put(int, de.innovationgate.ext.org.mozilla.javascript.Scriptable, java.lang.Object)
     */
    public void put(int index, Scriptable start, Object value) {
        scope.put(index, start, value);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#delete(java.lang.String)
     */
    public void delete(String name) {
        scope.delete(name);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#delete(int)
     */
    public void delete(int index) {
        scope.delete(index);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getPrototype()
     */
    public Scriptable getPrototype() {
        
       if (scope instanceof RhinoScope) {
           return scope;
       }
       else {
           return super.getPrototype();
       }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#setPrototype(de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public void setPrototype(Scriptable prototype) {
        
        if (scope instanceof RhinoScope) {
            scope.setPrototype(prototype);
        }
        else {
            super.setPrototype(prototype);
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getParentScope()
     */
    public Scriptable getParentScope() {
        
        if (scope instanceof RhinoScope) {
            return null;
        }
        else {
            return scope;
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#setParentScope(de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public void setParentScope(Scriptable parent) {
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getIds()
     */
    public Object[] getIds() {
        if (scope instanceof RhinoScope) {
            return scope.getIds();
        }
        else {
            return super.getIds();
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getDefaultValue(java.lang.Class)
     */
    public Object getDefaultValue(Class<?> hint) {
        if (scope instanceof RhinoScope) {
            return scope.getDefaultValue(hint);
        }
        else {
            return super.getDefaultValue(hint);
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#hasInstance(de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public boolean hasInstance(Scriptable instance) {
        
        if (scope instanceof RhinoScope) {
            return scope.hasInstance(instance);
        }
        else {
            return super.hasInstance(instance);
        }
    }

}

