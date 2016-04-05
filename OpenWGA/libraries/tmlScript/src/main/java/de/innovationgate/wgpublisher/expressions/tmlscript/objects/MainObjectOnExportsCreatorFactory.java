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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.ext.org.mozilla.javascript.EvaluatorException;
import de.innovationgate.ext.org.mozilla.javascript.Function;
import de.innovationgate.ext.org.mozilla.javascript.Script;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.tmlscript.FunctionArgumentSubstitutor;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator.IncludedDesign;
import de.innovationgate.wgpublisher.expressions.tmlscript.MethodArgumentsBuilder.MethodArguments;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptRootScope;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wga.server.api.WGA;

public class MainObjectOnExportsCreatorFactory implements ObjectCreatorFactory {

    public String getObjectCode(TMLAction objectDefinition) {
        return "var exports = {};" + objectDefinition.getCode() + "exports;";
    }
    
    @Override
    public ObjectCreator constructObjectCreator(RhinoContext cx, TMLAction objectDefinition, Script script, TMLScriptObjectParentScope parentScope) throws WGException, IOException, ClassNotFoundException {

        Scriptable obj = (Scriptable) ((Function) script).call(cx, parentScope, null, new Object[] {});
        
        // Find the exported constructor
        Function exportedConstructor = null;
        String mainObjectName = WGAGlobal.getMainObjectName(objectDefinition.getModuleName());
        for (Object id : obj.getIds()) {
            if (id instanceof String && String.valueOf(id).equalsIgnoreCase(mainObjectName)) {
                exportedConstructor = (Function) obj.get((String) id, obj);
                break;
            }
        }
                        
        if (exportedConstructor == null) {
            throw new EvaluatorException("Cannot find main class in TMLScript object definition " + objectDefinition.getModuleDatabase() + "/" + objectDefinition.getModuleName());
        }
        
        final Function objectCreatorConstructor = exportedConstructor;
        
        return new ObjectCreator() {
            
            @Override
            public Scriptable createManagedObject(RhinoContext cx, TMLScriptObjectParentScope parentScope, WGA wga, FunctionArgumentSubstitutor substitutor, Map<String, Object> namedArgs, Object[] unnamedArgs)
                    throws WGAPIException, WGException, IOException, ClassNotFoundException {
                
                try (MethodArguments args = RhinoExpressionEngineImpl.get().getArgumentBuilder().buildArguments(objectCreatorConstructor, wga, substitutor, new HashMap<String, Object>(), unnamedArgs)) {
                    return objectCreatorConstructor.construct(cx, parentScope, args.getArguments(parentScope));
                }
                
            }
            
            @Override
            public Function getConstructor() {
                return objectCreatorConstructor;
            }
            
            @Override
            public List<IncludedDesign> getIncludedDesigns() {
                return Collections.emptyList();
            }
            
            @Override
            public TMLScriptObjectMetadata getMetaData() {
                return new TMLScriptObjectMetadata();
            }
        
        };
        
    }

}
