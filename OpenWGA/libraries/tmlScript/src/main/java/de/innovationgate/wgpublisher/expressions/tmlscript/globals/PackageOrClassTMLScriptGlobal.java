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

package de.innovationgate.wgpublisher.expressions.tmlscript.globals;

import java.util.Iterator;

import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.NativeJavaClass;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;

public class PackageOrClassTMLScriptGlobal extends TMLScriptGlobal {

    public PackageOrClassTMLScriptGlobal(String name, Object ref) {
        super(name, ref);
    }

    @Override
    public Object provide(WGA wga) {

        RhinoExpressionEngineImpl engineImpl= (RhinoExpressionEngineImpl) ExpressionEngineFactory.getTMLScriptEngine();
        try {
            Class<?> clazz = WGACore.getLibraryLoader().loadClass(String.valueOf(getRef()));
            return new NativeJavaClass(engineImpl.getSharedScope(), clazz);
        }
        catch (ClassNotFoundException e) {
            // Seems to be a package. Descend from shared scope to the package
            Scriptable scope = engineImpl.getSharedScope();
            Iterator<String> packages = WGUtils.deserializeCollection("Packages." + getRef(), ".").iterator();
            while (packages.hasNext()) {
                scope = (Scriptable) scope.get(packages.next(), scope);
                if (scope == null) {
                    throw new EvaluatorException("Unknown class or package in TMLScript global: " + getRef());
                }
            }
    
            return scope;
        }
        
    }


}
