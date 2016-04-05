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

package de.innovationgate.wgpublisher.webtml.portlet;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObjectData;
import de.innovationgate.wgpublisher.webtml.utils.JsonUtils;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Collects all session based settings belonging to a portlet
 */
public class TMLPortletState implements Serializable {
    
    private static final long serialVersionUID = 1L;
    public static final String STATEPROP_MODE = "$mode";
    public static final String STATEPROP_CONTEXT = "$context";
    public static final String STATEPROP_OPTIONS = "$options";
    public static final String PROP_CONFIG = "config";
    
    public static final int STATE_VERSION = 1;
    
    private transient long _lastProcessedEventIndex = Long.MIN_VALUE;
    private transient boolean _transmitToClient = true;
    private transient boolean _forceReload = false;
    private transient boolean _sentFromClient = false;
    private transient TMLPortletProcessContext _processContext = null;
    
    
    private transient boolean _transient = false;
    private String _processId = null;
    private transient Version _complianceVersion;
    private String _portletKey;
    private String _portletName;
    private String _parentPortletKey;
    private String _profileName;
    private String _sessionId;
    private ScopeObjectRegistry _scopeObjectRegistry;
    private DesignResourceReference _design;
    
    // The root portlet state is never serialized, so defaulting this transient field to false
    // while setting it to true initially for the root portlet will provide correct data 
    private transient boolean _root = false;
   
    
    /**
     * Constructor for state creation by portlet
     * @param wga
     * @param storage
     * @param compatibilityVersion
     * @param portlet
     * @param sessionId
     * @param controllerDesign
     * @throws WGAPIException
     */
    TMLPortletState(Version compatibilityVersion, TMLPortlet portlet, String sessionId, DesignResourceReference controllerDesign) throws WGAPIException {
        _complianceVersion = compatibilityVersion;
        _portletKey = portlet.getportletkey();
        _profileName = portlet.getProfile().getprofile().getName();
        _portletName = portlet.getname();
        _parentPortletKey = (portlet.getparentportlet().isroot() ? null : portlet.getparentkey());
        _sessionId = sessionId;
        _scopeObjectRegistry = createScopeObjectRegistry();
        _design = controllerDesign;
        _root = portlet.isroot();
        initProcessContext();
    }
    
    /**
     * Constructor for deserialisation of default state
     * @param storage
     * @param portletId
     * @param portletName
     * @param parentId
     * @param profileId
     * @param processId
     * @param sessionId
     * @param controllerDesign
     * @throws WGAPIException
     */
    TMLPortletState(String portletId, String portletName, String parentId, String profileId, String processId, String sessionId, DesignResourceReference controllerDesign) {
        _portletKey = portletId;
        _profileName = profileId;
        _portletName = portletName;
        _parentPortletKey = parentId;
        _sessionId = sessionId;
        _processId = processId;
        _scopeObjectRegistry = createScopeObjectRegistry();
        _design = controllerDesign;
    }

    private ScopeObjectRegistry createScopeObjectRegistry() {
        return new ScopeObjectRegistry(ObjectScope.PORTLET, getScopeObjectRegistryName(), new PortletScopeObjectContextCreator(_portletKey));
    }

    private String getScopeObjectRegistryName() {
        return "WebTML portlet " + _portletName;
    }
    
    protected void initializeControllerState(WGA wga) throws WGAPIException {
        
        fetchController(wga);
        JsonObject state = getControllerState();
        if (!state.has(STATEPROP_MODE)) {
            state.addProperty(STATEPROP_MODE, TMLPortlet.DEFAULT_PORTLET_MODE);
        }

    }

    private JsonObject getControllerState() {
        
        DesignResourceReference ref;
        if (_design != null) {
            ref = _design;
        }
        else {
            // Fallback for legacy portlets without design
            ref = new DesignResourceReference("default", "default");
        }
        
        return getScopeObjectRegistry().getOrCreateScopeObjectState(ref);
    }
    

    public ScopeObject fetchController(WGA wga) throws WGAPIException {
        try {
            if (_root) { // No portlet controller on root portlet
                return null;
            }
            if (_design == null) {
                return null;
            }
            return getScopeObjectRegistry().getOrCreateScopeObject(wga, _design, true, true);
        }
        catch (WGException e) {
            throw new WGBackendException("Exception fetching portlet controller", e);
        }
    }
    
    public TMLScriptObjectMetadata getControllerMetaData(WGA wga) throws WGException {
        return ExpressionEngineFactory.getTMLScriptEngine().getTmlscriptObjectMetadata(wga, wga.design(_design)); 
    }
    
    public long getLastProcessedEventIndex() {
        return _lastProcessedEventIndex;
    }
    
    public void setLastProcessedEventIndex(long lastProcessedSsEventIndex) {
        if (!isTransient()) {
            this._lastProcessedEventIndex = lastProcessedSsEventIndex;
            _transmitToClient = true;
        }
    }
    
    public String getMode() {
        return getControllerState().get(STATEPROP_MODE).getAsString();
    }
    
    public void setMode(String mode) {
        JsonObject state = getControllerState();
        state.addProperty(STATEPROP_MODE, mode);
        if (_complianceVersion.isAtLeast(6, 0)) {
            setContextPath(TMLPortlet.PORTLETCONTEXT_NONE);
        }
        _transmitToClient = true;
    }
    
	public String getContextPath() {
	    JsonObject state = getControllerState();
		if (state.has(STATEPROP_CONTEXT)) {
		    return state.get(STATEPROP_CONTEXT).getAsString();
		}
		else {
		    return null;
		}
	}
	
	public void setContextPath(String contextPath) {
	    
	    JsonObject state = getControllerState();
		if (contextPath == null || contextPath.trim().equalsIgnoreCase(TMLPortlet.PORTLETCONTEXT_NONE)) {
			if (state.has(STATEPROP_CONTEXT)) {
			    state.remove(STATEPROP_CONTEXT);
			}
		}
		else {
		    state.addProperty(STATEPROP_CONTEXT, contextPath);
		}
		_transmitToClient = true;
	}
	
	protected void afterTransientDeserialisation(WGA wga, TMLPortletStateTransientStorage storage) throws WGException {
	    _lastProcessedEventIndex = Long.MIN_VALUE;
	    _transient = true;
	    _complianceVersion = WGA.get().design(_design.getDesignApp()).getVersionCompliance();
	    if (_scopeObjectRegistry == null) {
	        _scopeObjectRegistry = createScopeObjectRegistry();
	    }
	    else {
	        _scopeObjectRegistry.setName(getScopeObjectRegistryName());
	        _scopeObjectRegistry.setScopeObjectContextCreator(new PortletScopeObjectContextCreator(_portletKey));
	    }
        initProcessContext();
        recoverScopeObjects();
        initializeControllerState(wga);
	}

    public Object getSessionVar(String name) {
        
        if (_processId != null) {
            return getProcessContext().get(String.valueOf(name).toLowerCase());
        }
        else {
            return null;
        }
    }
    
    public Set<String> getSessionVarNames() {
        
        if (_processId != null) {
            return Collections.unmodifiableSet(getProcessContext().keySet());
        }
        else {
            return Collections.emptySet();
        }
    }
    
    public void setSessionVar(String name, Object value) {
        getProcessContext().put(String.valueOf(name).toLowerCase(), value);
    }
    
    public void removeSessionVar(String name) {
        
        getProcessContext().remove(String.valueOf(name).toLowerCase());
        _transmitToClient = true;
    }
    
    public String getPortletKey() {
        return _portletKey;
    }

    public String getProfileName() {
        return _profileName;
    }

    public boolean isTransient() {
        return _transient;
    }
    
    public boolean isDefaultState() {
        if ("view".equals(getMode()) == false) {
            return false;
        }
        
        if (getContextPath() != null) {
            return false;
        }
        
        if (_processId != null) {
            
            // If there is a process context we check if the portlet state with mode "view" is the only entry,
            // which would qualify this as default state
            TMLPortletProcessContext pc = getProcessContext();
            if (pc.size() > 1) {
                return false;
            }
            
            JsonObject controllerState = getControllerState();
            Set<Entry<String, JsonElement>> stateEntries = controllerState.entrySet();
            if (stateEntries.size() > 2) {
                return false;
            }
            
            if (stateEntries.size() > 0) {
                for (Map.Entry<String,JsonElement> elem : stateEntries) {
                    if (!elem.getKey().equals(STATEPROP_MODE)) {
                        return false;
                    }
                    if (!elem.getKey().equals(PROP_CONFIG)) {
                        return false;
                    }
                }
                    
                Entry<String,JsonElement> singleEntry = stateEntries.iterator().next();
                
            }
            
            if (pc.getScopeObjects() != null && pc.getScopeObjects().size() > 1) {
                return false;
            }
        }
        
        return true;
    }

    protected void setTransient(boolean transientState) {
        _transient = transientState;
    }

    public boolean isTransmitToClient() {
        
        if (_root) {
            return false;
        }
        
        if (!_transmitToClient) {
            return false;
        }
        
        if (!_transient) {
            return false;
        }
        
        return true;
        

    }

    public void setTransmitToClient(boolean changed) {
        _transmitToClient = changed;
    }

    public boolean isSentFromClient() {
        return _sentFromClient;
    }

    public void setSentFromClient(boolean sentFromClient) {
        _sentFromClient = sentFromClient;
    }

    public boolean isOverwritableByClient() {
        return isTransient() && !isSentFromClient();
    }

    public boolean isForceReload() {
        return _forceReload;
    }

    public void setForceReload(boolean forceReload) {
        _forceReload = forceReload;
        if (_forceReload) {
            setTransmitToClient(true);
        }
    }

    public String getProcessId() {
        return _processId;
    }
    
    public TMLPortletProcessContext getProcessContext() {
        
        if (_processContext == null) {
            initProcessContext();
        }
        
        return _processContext;
        
    }

    private void initProcessContext() {
        if (_processId == null) {
            _processId = UIDGenerator.generateUID();
            setTransmitToClient(true);
        }
        
        if (!_root) {
            ProcessContextRegistration contexts = TMLContext.getThreadMainContext().getEnvironment().getProcessContextRegistration();
            synchronized (contexts) {
                TMLPortletProcessContext pContext = (TMLPortletProcessContext) contexts.getProcessContext(_processId);
                if (pContext == null) {
                    pContext = new TMLPortletProcessContext(this, contexts);
                    contexts.setProcessContext(_processId, pContext);
                }
                
                _processContext = pContext;
            }
        }
        
        // Nonpersistent process context for root portlet
        else {
            _processContext = new TMLPortletProcessContext(this, null);
        }
    }

    public String getSessionId() {
        return _sessionId;
    }

    public String getPortletName() {
        return _portletName;
    }

    public String getParentPortletKey() {
        return _parentPortletKey;
    }

    
    public Map<String,Object> getControllerOptions(TMLContext cx) {
        
        Map<String,Object> options = new HashMap<String, Object>();
        JsonObject state = getControllerState();
        if (state.has(STATEPROP_OPTIONS)) {
            JsonObject stateOptions = (JsonObject) state.get(STATEPROP_OPTIONS);
            for (Map.Entry<String,JsonElement> entry : stateOptions.entrySet()) {
                JsonElement jsonValue = entry.getValue();
                Object value = new JsonUtils(cx).jsonToJava(jsonValue);
                options.put(entry.getKey(), value);
            }
        }
        return options;
        
    }

    public ScopeObjectRegistry getScopeObjectRegistry() {
        return _scopeObjectRegistry;
    }
    
    public void storeScopeObjects() {
        getProcessContext().setScopeObjects(_scopeObjectRegistry.getScopeObjects());
    }
    
    public void recoverScopeObjects() {
        if (getProcessContext().getScopeObjects() != null) {
            _scopeObjectRegistry.setScopeObjects(getProcessContext().getScopeObjects());
        }
        else {
            _scopeObjectRegistry.setScopeObjects(new ConcurrentHashMap<DesignResourceReference, ScopeObjectData>());
        }
    }

    public DesignResourceReference getControllerDesign() {
        return _design;
    }

    protected boolean isRoot() {
        return _root;
    }

    public Version getComplianceVersion() {
        return _complianceVersion;
    }
    
}