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

import org.apache.log4j.Logger;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaObject;
import de.innovationgate.ext.org.mozilla.javascript.NativeObject;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.ext.org.mozilla.javascript.Undefined;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class ContextWrapper extends NativeJavaObject {
	
    private static final long serialVersionUID = 1L;
    private de.innovationgate.wgpublisher.webtml.utils.TMLContext tmlContext;
    private Scriptable scope;
    
    public ContextWrapper(Scriptable scope, TMLContext context) {
	    super(scope, context, TMLContext.class);
		this.scope = scope;
		this.tmlContext = context;
	}

	/**
	 * @see ScriptableObject#get(String, Scriptable)
	 */
	public Object get(String arg0, Scriptable arg1) {
		try {
    		String argLC = arg0.toLowerCase();  
            
            // Return props and methods of this TMLContext object
            if (super.has(argLC, arg1)) {
            	// compatible with older version: try lower case 
                return RhinoWrapFactory.notFoundToNull(super.get(argLC, arg1));
            }
            else if (super.has(arg0, arg1)) {
            	// preferred: exact case
                return RhinoWrapFactory.notFoundToNull(super.get(arg0, arg1));
            }            
    		else {
    		    
    		    // Use as item or meta short form
    			if (isMetaName(arg0, arg1)) {
    				return Context.javaToJS(this.tmlContext.meta(arg0), arg1);
    			}
    			else if (isItemName(arg0, arg1)) {
    			    return Context.javaToJS(this.tmlContext.item(arg0), arg1);
    			}
    			
    			return Scriptable.NOT_FOUND;
    		    			
    		}
        }
		catch (WGAPIException e) {
            RhinoContext.throwAsScriptRuntimeEx(e);
            return Scriptable.NOT_FOUND;
        }		
	}

    protected boolean isItemName(String arg0, Scriptable scope) throws WGAPIException {
        // When we are no root scope we take everything for an item that starts with a lowercase letter
        char first = arg0.charAt(0);
        if (!(first >= 'a' && first <= 'z')) {
            return false;
        }
        
        return true;
    }
	
	protected boolean isMetaName(String arg0, Scriptable scope) {
        
	    // Look if it starts with an uppercase letter
	    char first = arg0.charAt(0);
	    if (!(first >= 'A' && first <= 'Z')) {
	        return false;
	    }
	    
	    return true;
    }

    /**
	 * @see ScriptableObject#getClassName()
	 */
	public String getClassName() {
		return getClass().getName();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#put(java.lang.String, de.innovationgate.ext.org.mozilla.javascript.Scriptable, java.lang.Object)
	 */
	public void put(String name, Scriptable start, Object value) {
        
        if (super.has(name.toLowerCase(), start)) {
            super.put(name.toLowerCase(), start, value);
        }
        else {
            value = jsToVar(value);
            try {
                this.tmlContext.updateOrSetVar(name, value);
            }
            catch (WGException e) {
                throw Context.throwAsScriptRuntimeEx(e);
            }
        }
		
	}

    public static Object jsToVar(Object value) {
        
        // Rhino types that may be set as var natively
        if (value instanceof Undefined) {
            return value;
        }
        if (value instanceof NativeObject) {
            return value;
        }
        
        // Try Java conversion
        try {
            return Context.jsToJava(value, Object.class);
        }
        catch (EvaluatorException e) {
        }
        
        // If conversion fails we allow setting of the original value
        return value;
    }
	


    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#get(int, de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public Object get(int arg0, Scriptable arg1) {
        return Scriptable.NOT_FOUND;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#has(int, de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public boolean has(int arg0, Scriptable arg1) {
        return false;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#has(java.lang.String, de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public boolean has(String arg0, Scriptable arg1) {
        try {
            String argLC = arg0.toLowerCase();
            return super.has(argLC, arg1)  || 
                    isMetaName(arg0, arg1) ||
                    isItemName(arg0, arg1);
        }
        catch (WGAPIException e) {
            Logger.getLogger("wga.tmlscript").error("Exception testing existence of property " + arg0, e);
            return false;
        }
                
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#put(int, de.innovationgate.ext.org.mozilla.javascript.Scriptable, java.lang.Object)
     */
    public void put(int index, Scriptable start, Object value) {
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#delete(java.lang.String)
     */
    public void delete(String name) {
        try {
            tmlContext.removevar(name);
        }
        catch (WGAPIException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#delete(int)
     */
    public void delete(int index) {
    }



    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#setPrototype(de.innovationgate.ext.org.mozilla.javascript.Scriptable)
     */
    public void setPrototype(Scriptable prototype) {
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getParentScope()
     */
    public Scriptable getParentScope() {
        return scope;
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
        return super.getIds();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.ext.org.mozilla.javascript.Scriptable#getDefaultValue(java.lang.Class)
     */
    public Object getDefaultValue(Class<?> hint) {
        return super.getDefaultValue(hint);
    }



    public de.innovationgate.wgpublisher.webtml.utils.TMLContext getTmlContext() {
        return tmlContext;
    }


}

