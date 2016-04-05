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

import java.util.ArrayList;
import java.util.List;

import de.innovationgate.ext.org.mozilla.javascript.Context;
import de.innovationgate.ext.org.mozilla.javascript.Scriptable;
import de.innovationgate.ext.org.mozilla.javascript.ScriptableObject;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.tmlscript.DesignLocator;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngineImpl;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal.WGAGlobal;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;

/**
 * Abstract base functionality for all TMLScript object parent scopes.
 */
public abstract class TMLScriptObjectParentScope extends ScriptableObject {
    
    protected TMLAction _objectDefinition;
    protected TMLScriptObjectMetadata _objectMetaData;
    private List<DesignResourceReference> _usedDesigns = new ArrayList<>();
    
    public TMLAction getObjectDefinition() {
        return _objectDefinition;
    }

    public TMLScriptObjectParentScope(TMLAction objectDefinition, Scriptable scope, Scriptable prototype) {
        super(scope, prototype);
        _objectDefinition = objectDefinition;
    }
    
    /**
     * Constructor for legacy objects which regulate their parent scope dynamically, therefor do not determine it here
     * @param objectDefinition
     */
    public TMLScriptObjectParentScope(TMLAction objectDefinition) {
        _objectDefinition = objectDefinition;
    }

    @Override
    public Object get(String name, Scriptable start) {
    
        try {
            RhinoScope rhinoScope = RhinoExpressionEngineImpl.get().getSharedScope();
            if ("$".equals(name)) {
                Design design = rhinoScope.getWgaGlobal().getWga().design(_objectDefinition.getModuleDatabase()).resolve(_objectDefinition.getDesignReference().getResourceOverlayReference());
                return new DesignLocator(WGAGlobal.fetchWGA(), design, this);
            }
            else if ("$$".equals(name)) {
                Design design = rhinoScope.getWgaGlobal().getWga().design(_objectDefinition.getModuleDatabase()).resolve(_objectDefinition.getModuleName() + ":..");
                return new DesignLocator(WGAGlobal.fetchWGA(), design, this);
            }
            
            return super.get(name, start);
        }
        catch (WGException e) {
            throw Context.throwAsScriptRuntimeEx(e);
        }
    }

    @Override
    public boolean has(String name, Scriptable start) {
    
        if ("$".equals(name) || "$$".equals(name)) {
            return true;
        }
        
        return super.has(name, start);
    }

    public void addUsedDesign(DesignResourceReference ref) {
        _usedDesigns.add(ref);
    }

    public TMLScriptObjectMetadata getObjectMetaData() {
        return _objectMetaData;
    }

    public void setObjectMetaData(TMLScriptObjectMetadata objectMetaData) {
        _objectMetaData = objectMetaData;
    }

}