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

package de.innovationgate.wgpublisher.expressions.tmlscript.objects;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;
import de.innovationgate.wgpublisher.expressions.tmlscript.FunctionArgumentSubstitutor;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;

public interface ObjectCreator {
    
    public Scriptable createManagedObject(RhinoContext cx, TMLScriptObjectParentScope parentScope, WGA wga, FunctionArgumentSubstitutor substitutor,
            Map<String, Object> namedArgs, Object[] unnamedArgs) throws WGAPIException, WGException, IOException, ClassNotFoundException;
    
    public Function getConstructor();
    
    public List<DesignLocator.IncludedDesign> getIncludedDesigns();
    
    public TMLScriptObjectMetadata getMetaData();
}