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
import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.MissingDesignResourceException;
import de.innovationgate.wgpublisher.expressions.tmlscript.CachedScript;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoContext;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.scopes.TMLScriptObjectParentScope;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.ObjectStrategy;

public class ObjectCreatorManager {
    
    private static final Map<ObjectStrategy, ObjectCreatorFactory> _objectCreatorFactories = new HashMap<ObjectStrategy, ObjectCreatorFactory>();
    static {
        _objectCreatorFactories.put(ObjectStrategy.SINGLE, new SingleObjectCreatorFactory(false));
        _objectCreatorFactories.put(ObjectStrategy.SINGLE_OR_EXPORTED_CONSTRUCTOR, new SingleObjectCreatorFactory(true));
        _objectCreatorFactories.put(ObjectStrategy.MAINOBJECT_ON_THIS, new MainObjectOnThisObjectCreatorFactory());
        _objectCreatorFactories.put(ObjectStrategy.MAINOBJECT_ON_EXPORTS, new MainObjectOnExportsCreatorFactory());
    }

    private RhinoExpressionEngineImpl _engine;
    
    public ObjectCreatorManager(RhinoExpressionEngineImpl engine) {
        _engine = engine;
    }
    
    public ObjectCreatorFactory forDefinition(TMLAction objectDefinition) {
        return forStrategy(objectDefinition.getObjectStrategy());
    }
    
    public ObjectCreatorFactory forStrategy(ObjectStrategy strategy) {
        return _objectCreatorFactories.get(strategy);
    }
    
    

    public ObjectCreator getObjectCreator(WGA wga, RhinoContext cx, TMLAction objectDefinition, TMLScriptObjectParentScope parentScope) throws WGException, IOException, ClassNotFoundException {
        return getObjectCreator(wga, cx, objectDefinition, null, parentScope);
    }
    
    public ObjectCreator getObjectCreator(WGA wga, RhinoContext cx, TMLAction objectDefinition, ObjectStrategy strategy, TMLScriptObjectParentScope parentScope) throws WGException, IOException, ClassNotFoundException {
        
        if (strategy == null) {
            strategy = objectDefinition.getObjectStrategy();
        }
        ObjectCreatorFactory creatorFactory = _objectCreatorFactories.get(strategy);
        
        // Get the script, use a cached creator if available
        String code = creatorFactory.getObjectCode(objectDefinition);
        String scriptName = "TMLScript-Object " + objectDefinition.getModuleDatabase() + "/" + objectDefinition.getModuleName();
        CachedScript script = _engine.getCompiledScript(code, cx, scriptName, parentScope);
        if (isValid(wga, script.getObjectCreator())) {
            return script.getObjectCreator();
        }
        
        // Construct and cache the creator
        ObjectCreator creator = creatorFactory.constructObjectCreator(cx, objectDefinition, script.getScript(), parentScope);
        script.setObjectCreator(creator);
        return creator;

        
    }

    private boolean isValid(WGA wga, ObjectCreator objectCreator) throws WGException {

        if (objectCreator == null) {
            return false;
        }
        
        for (DesignLocator.IncludedDesign includedDesign : objectCreator.getIncludedDesigns()) {
            try {
                Design design = wga.design().resolve(includedDesign.getRef());
                String code = design.getTMLScriptCode();
                if (code.hashCode() != includedDesign.getHashCode()) {
                    return false;
                }
            }
            catch (MissingDesignResourceException e) {
                return false;
            }
        }
        
        return true;
        
    }
    
    

}
