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
package de.innovationgate.wgpublisher.expressions.tmlscript.scopes;

import org.apache.log4j.Logger;

import de.innovationgate.ext.org.mozilla.javascript.ErrorReporter;
import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.wrapping.ContextWrapper;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class TMLScriptRootScope extends ContextWrapper implements ErrorReporter {

    private static final long serialVersionUID = 1L;
    private TMLScriptRootScopeData _data;
    private boolean _isolated;
    
    public TMLScriptRootScope(TMLScriptRootScopeData scopePrototype, Object context, boolean isolated) {
        super(scopePrototype.getRhinoScope(), (TMLContext) context);
        _data = scopePrototype;
        _isolated = isolated;
    }

    public Object getDefaultValue(@SuppressWarnings("rawtypes") Class hint) {
        return _data.getRhinoScope().getDefaultValue(hint);
    }

    public void warning(String message, String sourceName, int line, String lineSource, int lineOffset) {
        String msg = "TMLScript-Warning: " + message + " (at line " + line + ", column " + lineOffset + ")";
        getTmlContext().addwarning(msg, false);
    }

    public void error(String message, String sourceName, int line, String lineSource, int lineOffset) {
        throw runtimeError(message, sourceName, line, lineSource, lineOffset);

    }

    public EvaluatorException runtimeError(String message, String sourceName, int line, String lineSource, int lineOffset) {
        return new EvaluatorException(message, sourceName, line, lineSource, lineOffset);
    }

    public TMLScriptRootScopeData getData() {
        return _data;
    }

    public Scriptable getParentScope() {
        return _data;
    }

    protected boolean isMetaName(String arg0, Scriptable scope) {
        boolean superResult = super.isMetaName(arg0, scope);
        if (superResult == false) {
            return false;
        }

        // Look if the name is defined as top level object on RhinoScope. If so,
        // we may not serve it as meta
        if (_data.getRhinoScope().has(arg0, scope)) {
            return false;
        }

        if (_data.has(arg0, scope)) {
            return false;
        }

        return true;
    }

    public boolean has(String arg0, Scriptable arg1) {
        
        boolean result = super.has(arg0, arg1);
        if (result == true) {
            return true;
        }

        try {
            if (!_isolated) {
                if (getTmlContext().getwgacore().getTmlscriptGlobalRegistry().isGlobalDefined(arg0, getTmlContext().designdb())) {
                    return true;
                }
            }
        }
        catch (WGException e) {
            Logger.getLogger("wga.tmlscript").error("Exception testing existence of TMLScript global " + arg0, e);
        }

        return false;
    }

    public Object get(String arg0, Scriptable arg1) {

        // Look if it is a TMLScript global
        try {
            if (!_isolated) {
                TMLScriptGlobal global = null;
                global = getTmlContext().getwgacore().getTmlscriptGlobalRegistry().getGlobal(arg0, getTmlContext().db());
                if (global != null) {
                    Object globalObj = _data.getRhinoScope().getRuntime().provideGlobal(getWgaGlobal().getWga(), global);
                    if (globalObj != null) {
                        return globalObj;
                    }
                }
            	global = getTmlContext().getwgacore().getTmlscriptGlobalRegistry().getGlobal(arg0, getTmlContext().designdb());
                if (global != null) {
                    Object globalObj = _data.getRhinoScope().getRuntime().provideGlobal(getWgaGlobal().getWga(), global);
                    if (globalObj != null) {
                        return globalObj;
                    }
                }
            }
        }
        catch (WGException e) {
            Logger.getLogger("wga.tmlscript").error("Exception testing existence of TMLScript global " + arg0, e);
        }
        
        return super.get(arg0, arg1);

    }

    protected boolean isItemName(String arg0, Scriptable scope) throws WGAPIException {

        // At this level we interpret all names as items that are not defined at
        // higher level
        // That way Items can be set using direct setter syntax: item = value
        if (_data.getRhinoScope().has(arg0, scope)) {
            return false;
        }

        if (_data.has(arg0, scope)) {
            return false;
        }

        return true;

    }

    public Scriptable getPrototype() {
        return _data;
    }
    
    public WGAGlobal getWgaGlobal() {
        return _data.getRhinoScope().getWgaGlobal();
    }

}
