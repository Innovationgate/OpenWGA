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

import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;

/**
 * A class to be used as parent scope for isolated TMLScript objects,
 * which are managed objects that have no access to any non-obligatory environment data.
 * Isolation is enforced via {@link RhinoExpressionEngineImpl#callObjectScript}
 */
public class TMLScriptIsolatedParentScope extends TMLScriptModernObjectParentScope {

    public TMLScriptIsolatedParentScope(TMLAction action, RhinoScope rhinoScope) throws WGException {
        super(action, rhinoScope);
    }

    private static final long serialVersionUID = 1L;

    public String getClassName() {
        return "TMLScriptControllerParentScope";
    }

    public static boolean isIsolated(Scriptable object) {

        while (object.getClass() != RhinoScope.class) {
            if (object instanceof TMLScriptObjectParentScope) {
                break;
            }
            object = object.getParentScope();
        }
        
        return (object instanceof TMLScriptIsolatedParentScope);

    }

}
