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

package de.innovationgate.wgpublisher.so;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpSessionActivationListener;
import javax.servlet.http.HttpSessionEvent;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;

/**
 * Generic registry for objects stored on scopes.
 * The objects themselves are stored as {@link ScopeObjectData} and can be cached on a ProcessContext (like done for portlet scope). They are transient on this object so they are not serialized.
 * The serializable state of objects is stored and serialized with the ScopeObjectRegistry. 
 * To the outside a {@link ScopeObject} is returned which contains the ScopeObjectData plus a reference to the registry, so it can extract/inject state on itself.
 */
public class ScopeObjectRegistry implements Serializable, HttpSessionActivationListener {
    
    /**
     * Serializable wrapper for the scope state JSON object, ridiculously being not Java serializable
     */
    public static class ScopeState implements Serializable {
        
        private JsonObject _json;
        
        public ScopeState(JsonObject json) {
            _json = json;
        }

        public JsonObject getJson() {
            return _json;
        }
        
        
        private void writeObject(ObjectOutputStream oos) throws IOException { 
            oos.writeObject(_json.toString());
        }
        
        private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException {
            _json = (JsonObject) new JsonParser().parse((String) ois.readObject());
        }
            
        
    }
    
    /**
     * Objects to store instantiated objects with their local status on the server, normally in process contexts
     */
    public static class ScopeObjectData {
        private Object _object;
        protected Object getObject() {
            return _object;
        }
        protected void setObject(Object object) {
            _object = object;
        }
        protected Date getLastModified() {
            return _lastModified;
        }
        protected void setLastModified(Date lastModified) {
            _lastModified = lastModified;
        }
        protected DesignResourceReference getRef() {
            return _ref;
        }
        public ScopeObjectData(Object object, Date lastModified, DesignResourceReference ref) {
            super();
            _object = object;
            _lastModified = lastModified;
            _ref = ref;
        }
        private  Date _lastModified;
        private DesignResourceReference _ref;
        
        public boolean extractState(ScopeObjectRegistry reg, ScopeObject so) throws WGException {
            
            if (_object != null) {
                JsonObject oldState = reg.getScopeObjectState(_ref);
                RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
                
                JsonObject state = (JsonObject) engine.extractProperty(_object, ScopeObject.PROP_STATE);
                if (state != null) {
                    if (so.getContext() != null) {
                        state = so.getContext().transformStateToExtract(state, _ref);
                    }
                }
                
                if (state != null) {
                    if (oldState == null || !oldState.equals(state)) {
                        reg.setScopeObjectState(_ref, state);
                        return true;
                    }
                }
                else if (oldState != null) {
                    reg.removeScopeObjectState(_ref);
                    return true;
                }
            }
            
            return false;
            
        }
        
        public void injectState(ScopeObjectRegistry reg, ScopeObject so) throws WGException {
            if (_object != null) {
                JsonObject state = reg.getScopeObjectState(_ref);
                if (state != null && reg.isInjectableControllerState(state)) {
                    RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
                    if (so.getContext() != null) {
                        state = so.getContext().transformStateToInject(state, _ref);
                    }
                    engine.injectProperty(_object, ScopeObject.PROP_STATE , state);
                }
            }
        }
    }
    
    
    /**
     * Object to hand out a scope object to the outside
     */
    public static class ScopeObject implements ManagedObject {
        
        public static final String PROP_STATE = "state";
        private ScopeObjectRegistry _reg;
        private ScopeObjectData _data;
        private ScopeObjectContext _context;
        
        protected ScopeObjectRegistry getReg() {
            return _reg;
        }

        protected void setReg(ScopeObjectRegistry reg) {
            _reg = reg;
        }

        public ScopeObject(ScopeObjectRegistry reg, ScopeObjectData data, ScopeObjectContext context) {
            _reg = reg;
            _data = data;
            _context = context;
        }

        public Object getObject() {
            return _data.getObject();
        }

        public Date getLastModified() {
            return _data.getLastModified();
        }

        public boolean extractState() throws WGException {
            return _data.extractState(_reg, this);
        }

        public void injectState() throws WGException {
            _data.injectState(_reg, this);
        }

        public DesignResourceReference getRef() {
            return _data.getRef();
        }

        public void setLastModified(Date lastModified) {
            _data.setLastModified(lastModified);
        }

        public ScopeObjectRegistry getRegistry() {
            return _reg;
        }
        
        @Override
        public void afterUsage() throws WGException {
            if (_context != null) {
                _context.afterUsage(this);
            }
            if (extractState()) {
                _reg.getScope().notifyScopeObjectStateChange(this, WGA.get(), getRef());
            }
        }
        
        @Override
        public DesignResourceReference getDesign() {
            return getRef();
        }

        public JsonObject getState() {
            return _reg.getScopeObjectState(_data.getRef());
        }

        public ScopeObjectContext getContext() {
            return _context;
        }

        public void beforeUsage() throws WGException {
            if (_context != null) {
                _context.beforeUsage(this);
            }
        }
        
        
    }
    
    /**
     * Stores object instances transiently
     */
    private transient Map<DesignResourceReference,ScopeObjectData> _scopeObjects = new ConcurrentHashMap<DesignResourceReference, ScopeObjectData>();
    
    /**
     * Stores serializable object states for persistence/cluster syncing 
     */
    private Map<DesignResourceReference,ScopeState> _scopeStates = new ConcurrentHashMap<DesignResourceReference, ScopeState>();
    
    
    private transient String _name;
    private ObjectScope _scope;
    private transient ScopeObjectContextCreator _scopeObjectContextCreator = new NoopScopeObjectContextCreator();
    
    private void readObject(ObjectInputStream in) throws ClassNotFoundException, IOException {
        in.defaultReadObject();
        _scopeObjects = new ConcurrentHashMap<DesignResourceReference, ScopeObjectData>();
        _name = "Anonymous ScopeObjectRegistry";
    }
    
    public String getName() {
        return _name;
    }

    private static final String STATEPROP_MIXED = "$mixed";
    
    public ScopeObjectRegistry(ObjectScope scope, String name, ScopeObjectContextCreator context) {
        _name = name;
        _scope = scope;
        _scopeObjectContextCreator = context;
    }

    public boolean isScopeObjectExisting(DesignResourceReference ref) {
        return _scopeObjects != null && _scopeObjects.containsKey(ref);
    }
    
    public synchronized ScopeObject getOrCreateScopeObject(WGA wga, DesignResourceReference ref, boolean isController, boolean create) throws WGException {
        
        ScopeObjectData soData = _scopeObjects.get(ref);
        Design design = wga.design(ref);
        WGScriptModule module = design.getTMLScriptModule();
        if (module == null) {
            return null;
        }
        
        if (soData != null && !soData.getLastModified().equals(module.getLastModified())) {
            WGACore.staticLog().info("Re-creating managed TMLScript object '" + ref.toString() + "' for scope '" + _name + "' because the source code has changed");
            soData = null;
            create = true;
        }
        
        JsonObject state = getScopeObjectState(ref);
        boolean extracted = false;
        ScopeObject so;
        if (soData != null) {
            so = new ScopeObject(this, soData, _scopeObjectContextCreator.createScopeObjectContext(wga));    
        }
        else {
            if (state == null && !create) {
                return null;
            }
        
            Object obj;
            if (isController) {
                obj = wga.tmlscript().createObject(design, TMLScript.ObjectType.V2_ISOLATED, null, null);
            }
            else {
                obj = wga.tmlscript().createObject(design, TMLScript.ObjectType.V2, null, null);
            }
            soData = new ScopeObjectData(obj, module.getLastModified(), ref);
            so = new ScopeObject(this, soData, _scopeObjectContextCreator.createScopeObjectContext(wga));
            if (state == null) {
                so.extractState();
                extracted = true;
            }
            
            _scopeObjects.put(ref, soData);
        }
        
        if (!extracted) {
            so.injectState();
        }
        
        return so;
        
    }
    
    public JsonObject getScopeObjectState(DesignResourceReference ref) {
        ScopeState state = _scopeStates.get(ref);
        if (state != null) {
            return state.getJson();
        }
        else {
            return null;
        }
            
    }
    
    public JsonObject removeScopeObjectState(DesignResourceReference ref) {
        ScopeState state = _scopeStates.remove(ref);
        if (state != null) {
            return state.getJson();
        }
        else {
            return null;
        }
    }
    
    public void setScopeObjectState(DesignResourceReference ref, JsonObject state) {
        _scopeStates.put(ref, new ScopeState(state));
    }

    @Override
    public void sessionDidActivate(HttpSessionEvent arg0) {
    }

    @Override
    public void sessionWillPassivate(HttpSessionEvent arg0) {
    }

    public boolean isInjectableControllerState(JsonObject state) {
        if (state.has(STATEPROP_MIXED) && state.get(STATEPROP_MIXED).getAsBoolean() == true) {
            return false;
        }
        
        return true;
    }

    public Map<DesignResourceReference, ScopeState> getScopeStates() {
        return _scopeStates;
    }

    public Map<DesignResourceReference, ScopeObjectData> getScopeObjects() {
        return _scopeObjects;
    }

    public void setScopeObjects(Map<DesignResourceReference, ScopeObjectData> scopeObjects) {
        _scopeObjects = scopeObjects;
    }

    public synchronized JsonObject getOrCreateScopeObjectState(DesignResourceReference ref) {
        JsonObject state = getScopeObjectState(ref);
        if (state == null) {
           state = new JsonObject();
           setScopeObjectState(ref, state);
        }
        return state;
    }

    public void setName(String name) {
        _name = name;
    }

    protected ObjectScope getScope() {
        return _scope;
    }

    public ScopeObjectContextCreator getScopeObjectContextCreator() {
        return _scopeObjectContextCreator;
    }

    public void setScopeObjectContextCreator(ScopeObjectContextCreator scopeObjectContextCreator) {
        _scopeObjectContextCreator = scopeObjectContextCreator;
    }

}
