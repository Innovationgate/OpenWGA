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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.servlet.http.HttpSession;

import org.apache.commons.collections.map.LinkedMap;
import org.apache.commons.lang.builder.HashCodeBuilder;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.TransientObjectWrapper;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGPortletRegistry;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.Portlet;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLObject;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class TMLPortlet implements TMLObject, Portlet {

    public static final String DEFAULT_PORTLET_MODE = "view";
    public static final Pattern PORTLETNAME_PATTERN = Pattern.compile("[a-zA-Z0-9_\\.\\-\\:]+");
    public static final String REQATTRIB_FIRED_PORTLET_EVENTS = TMLPortlet.class.getName() + ".FiredPortletEvents";
    private static final int EVENTQUEUE_MAX_SIZE = 1000;
    public static final String PORTLETCONTEXT_NONE = "none";
    protected TMLUserProfile profile = null;
    protected WGPortlet reg = null;
    public TMLContext tmlContext = null;
    private TMLPortletState _state;
        
    /**
     * Queue that stores the last 1000 fired portlet events, so portlets can
     * react on them with serverside code.
     * @param session
     * @return
     */
    public static LinkedMap getFiredEventsQueue(HttpSession session) {
        
        synchronized (session) {
            @SuppressWarnings("unchecked")
            TransientObjectWrapper<LinkedMap> eventWrapper = (TransientObjectWrapper<LinkedMap>) session.getAttribute(WGACore.SESSION_FIREDPORTLETEVENTS);
            if (eventWrapper == null || eventWrapper.get() == null) {
                eventWrapper = new TransientObjectWrapper<LinkedMap>();
                eventWrapper.set(new LinkedMap());
                session.setAttribute(WGACore.SESSION_FIREDPORTLETEVENTS, eventWrapper);
            }
            return eventWrapper.get();
        }
        
    }
    
    public TMLPortlet(TMLContext tmlContext, TMLUserProfile profile, WGPortlet portlet) throws WGAPIException {
        this.tmlContext = tmlContext;
        this.profile = profile;
        this.reg = portlet;
    }

    /**
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLObject#getAPIObject()
     */
    public WGDocument getapiobject() {
    	return null;
    }

    
    @CodeCompletion
    public Object item(String name) throws WGAPIException {
        WGPortletRegistry registry = tmlContext.getPortletRegistry();
        if (registry.hasItem(reg, name)) {
            return TMLContext.flattenList(registry.getItemValue(reg, name), !getState().getComplianceVersion().isAtLeast(7,2));
        }
        else {
            return profile.getprofile().getDatabase().getNoItemBehaviour().getForTMLItem();
        }
        
        
    }

    
    @CodeCompletion
    public List<Object> itemlist(String name) throws WGAPIException {
        WGPortletRegistry registry = tmlContext.getPortletRegistry();
        if (registry.hasItem(reg, name)) {
            return TMLContext.toList(registry.getItemValue(reg, name));
        }
        else {
            return profile.getprofile().getDatabase().getNoItemBehaviour().getForTMLItemList();
        }
    }

    
    @CodeCompletion
    public boolean hasitem(String name) throws WGAPIException {
        return tmlContext.getPortletRegistry().hasItem(reg, name);
    }

    public Object meta(String name) throws WGAPIException {
    	return this.profile.meta(name);
    }

    public List<Object> metalist(String name) throws WGAPIException {
    	return this.profile.metalist(name);
    }

    @CodeCompletion
    public boolean save() throws WGAPIException {
    	return this.profile.save();
    }

    
    @CodeCompletion
    public boolean setitem(String name, Object value) throws WGAPIException {
        tmlContext.getPortletRegistry().setItemValue(reg, name, value);
        return true;
    }

    
    @CodeCompletion
    public void setvar(String name, Object value) throws WGAPIException {
        try {
            if (!isroot()) {
                WGA.get(tmlContext).tmlPage().setVar(getVarPrefix() + name, value);
            }
            else {
                throw new WGIllegalStateException("Portlet var not allowed on root scope");
            }
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }    
    }

    
    @CodeCompletion
    public Object getvar(String name) throws WGAPIException {
        if (!isroot()) {
        	try {
				return WGA.get(tmlContext).tmlPage().getVar(getVarPrefix() + name);
			} catch (WGException e) {
				return null;
			}
        }
        else {
            return null;
        }
    }

    
    @CodeCompletion
    public void removevar(String name) throws WGAPIException {
        if (!isroot()) {
            this.tmlContext.removevar(getVarPrefix() + name);
        }
    }

    
    @CodeCompletion
    public void setsessionvar(String name, Object value) throws WGAPIException {
    	setsessionvar(name, value, true);
    }

    
    @CodeCompletion
    public Object getsessionvar(String name) throws WGAPIException {
        Object value = TMLContext.flattenList(retrieveSessionVar(name), !_state.getComplianceVersion().isAtLeast(7,2));
        if (value instanceof NullPlaceHolder) {
            return null;
        }
    
        return value;
    }

    public boolean hassessionvar(String name) throws WGAPIException {
        return (retrieveSessionVar(name) != null);
    }

    public Object retrieveSessionVar(String name) throws WGAPIException {
        if (name == null) {
            return null;
        }       
    
        Object value =  getState().getSessionVar(name);
        return value;
    }

    
    @CodeCompletion
    public void setsessionvar(String name, Object value, boolean allowSerialization) throws WGAPIException {
        setsessionvar(name, value, allowSerialization, true);
    }

    @SuppressWarnings("unchecked")
    public void setsessionvar(String name, Object value, boolean allowSerialization, boolean keepList) throws WGAPIException {
        
        validateStateModification();
        
        if (value == null) {
            value = new NullPlaceHolder();
        }
        else if (value instanceof List && keepList) {
            value = new TMLContext.ListVarContainer((List<Object>) value);
        }
        
        TMLPortletState state = getState();
        state.setSessionVar(name, value);
        
    }

    @SuppressWarnings("unchecked")
    protected void wrapTransientSessionVar(String name, Object value, TMLPortletState state) {
        TransientObjectWrapper<Object> wrapper = null;
        
        Object currentValue = state.getSessionVar(name);    
        if (currentValue instanceof TransientObjectWrapper) {
           wrapper = (TransientObjectWrapper<Object>) currentValue;
        }
        else {
           wrapper = new TransientObjectWrapper<Object>();
           state.setSessionVar(name, wrapper);
        }
        wrapper.set(value);
    }

    
    @CodeCompletion
    public void removesessionvar(String name) throws WGAPIException {
        validateStateModification();
        getState().removeSessionVar(name);
    }

    
    @CodeCompletion
    public void removeitem(String name) throws WGAPIException {
        tmlContext.getPortletRegistry().removeItem(reg, name);
    }

    public TMLPortlet getportlet(String key) throws WGAPIException {
    	
    	WGPortlet portlet  = getPortletRegistration(key);
        
    	if (portlet != null) {
    		return new TMLPortlet(this.tmlContext, this.profile, portlet);
    	}
    	else {
    		return null;
    	}
    	
    }

    public WGPortlet getPortletRegistration(String key) throws WGAPIException {
        WGPortletRegistry registry = tmlContext.getPortletRegistry();
        return registry.getPortlet(registry.getApplicationId(this.tmlContext.getmaincontext().db()), key);
    }

    @CodeCompletion
    public TMLPortlet getparentportlet() throws WGAPIException {
        if (reg.getParentPortletKey() != null) {
            return getportlet(reg.getParentPortletKey());
        }
        else {
            return this;
        }
    }

    public List<String> getchildrenkeys() throws WGAPIException {
    		Iterator<WGPortlet> childRegs = tmlContext.getPortletRegistry().getChildPortlets(reg).iterator();
            List<String> childKeys = new ArrayList<String>();
            while (childRegs.hasNext()) {
                WGPortlet portlet = childRegs.next();
                childKeys.add(portlet.getKey());
            }
            return childKeys;
    }

    public List<String> getdescendantkeys() throws WGAPIException {
        Iterator<WGPortlet> childRegs = getChildRegistrations(reg, true).iterator();
        List<String> childKeys = new ArrayList<String>();
        while (childRegs.hasNext()) {
            WGPortlet reg = childRegs.next();
            childKeys.add(reg.getKey());
        }
        return childKeys;
    }

    
    @CodeCompletion
    public List<String> getchildrennames() throws WGAPIException {
            Iterator<WGPortlet> childRegs = tmlContext.getPortletRegistry().getChildPortlets(reg).iterator();
            List<String> childKeys = new ArrayList<String>();
            while (childRegs.hasNext()) {
                WGPortlet reg = childRegs.next();
                childKeys.add(reg.getName());
            }
            return childKeys;
    }

    public List<String> getdescendantnames() throws WGAPIException {
        Iterator<WGPortlet> childRegs = getChildRegistrations(reg, true).iterator();
        List<String> childKeys = new ArrayList<String>();
        while (childRegs.hasNext()) {
            WGPortlet reg = (WGPortlet) childRegs.next();
            childKeys.add(reg.getName());
        }
        return childKeys;
    }

    public String registerportlet(String tmlDb, String tmlCode, String title) throws WGAPIException {
    
    	WGPortletRegistry registry = tmlContext.getPortletRegistry();
        WGPortlet newPortlet = registry.createPortlet(registry.getApplicationId(this.tmlContext.getmaincontext().db()), reg);
    	newPortlet.setDesignDb(tmlDb);
    	newPortlet.setDesign(tmlCode);
    	newPortlet.setName(title);
    	registry.insertPortlet(newPortlet);
    	
    
    	TMLPortlet newTMLPortlet = new TMLPortlet(this.tmlContext, profile, newPortlet);
    	
        // Fetch portlet session context (so the following init event will not be overridden)
    	newTMLPortlet.getState();
        
        // Add portlet init event
        PortletEvent event = new PortletEvent("init", WGAVersion.toCsConfigVersion());
        event.setSource(newPortlet.getKey());
        event.setSourceName(title);
        event.setTargetPortletKey(newPortlet.getKey());
        addEventToQueue(event, this.tmlContext.gethttpsession());        
        
    	return newPortlet.getKey();
    }

    public String registerportlet(String tmlCode, String title) throws WGAPIException {
        return registerportlet(null, tmlCode, title);
    }

    public boolean unregisterportlet(String portletKey) throws WGAPIException {
    
        // Unregister portlet if it exists.
    	TMLPortlet portlet = getportlet(portletKey);
    	if (portlet != null) {
    	    portlet.unregister();
        }
    
        // We may have transient portlets. If so: Remove just the state
    	else {
    		TMLPortletStateStorage stateStorage = tmlContext.getPortletStateStorage();
    		if (stateStorage instanceof TMLPortletStateTransientStorage) {
        		TMLPortletStateTransientStorage transientStorage = (TMLPortletStateTransientStorage) stateStorage;
                stateStorage.disposeState(this.tmlContext.getmaincontext().db().getDbReference(), portletKey);
    		}
    	}
    	
        return true;
    }

    
    public void unregister() throws WGAPIException {
        tmlContext.getPortletRegistry().removePortlet(reg);
        clearstate();
        this.profile.save(false);
    }

    private List<WGPortlet> getChildRegistrations(WGPortlet parent, boolean descendants) throws WGAPIException {
    
    	List<WGPortlet> childRegs = new ArrayList<WGPortlet>();
    
    	Iterator<WGPortlet> iter = tmlContext.getPortletRegistry().getChildPortlets(parent).iterator();
    
    	while (iter.hasNext()) {
    		WGPortlet child = (WGPortlet) iter.next();
    		childRegs.add(child);
            if (descendants) {
                childRegs.addAll(getChildRegistrations(child, true));
            }
    	}
    	return childRegs;
    
    }

    
    @CodeCompletion
    public void cleanup() throws WGAPIException {
        
        tmlContext.getPortletRegistry().clearItems(reg);
        clearstate();
    
    }

    
    public void clearstate() throws WGAPIException {
       getStateStorage().disposeState(this.tmlContext.getmaincontext().db().getDbReference(), getportletkey());
       _state = null;
        
       // Remove portlet variables
       this.tmlContext.removePortletVariables(getVarPrefix());
    }

    
    public void forcestate() throws WGAPIException {
        getState().setSentFromClient(true);                
    }

    
    public void clearchildstate(String name) throws WGAPIException {
        
        // First try to find the portlet itself, if it is registered
        TMLPortlet child = getportletforname(name);
        if (child != null) {
            child.clearstate();
        }
        
        // Otherwise use the dedicated storage method to dispose the state of a portlet currently not registered
        else {
            getStateStorage().disposeChildState(this.tmlContext.getmaincontext().db().getDbReference(), getportletkey(), name);
        }
     }

    private TMLPortletStateStorage getStateStorage() {
        return tmlContext.getPortletStateStorage();
    }

    public void cleanup(String portletKey) throws WGAPIException {
        
        TMLPortlet portlet = getportlet(portletKey);
        if (portlet != null) {
            portlet.cleanup();
        }
        else {
            throw new WGIllegalArgumentException("Unable to cleanup portlet with key '" + portletKey + "' - no portlet with this key registered.");
    }
    
    }

    /**
     * Returns the title.
     * @return String
     */
    public String getportletkey() {
    	return reg.getKey();
    }

    public String getparentkey() {
    	return reg.getParentPortletKey();
    }

    
    @CodeCompletion
    public String gettml() {
    	return reg.getDesign();
    }

    
    @CodeCompletion
    public String gettmldb() {
        return reg.getDesignDb();
    }

    
    public List<String> getitemnames() throws WGAPIException {
        return tmlContext.getPortletRegistry().getItemNames(reg);
    }

    public String gettitle() {
    	return reg.getName();
    }

    
    @CodeCompletion
    public String getname() {
        return reg.getName();
    }

    
    @CodeCompletion
    public String getmode() throws WGAPIException {
        return getState().getMode();
    }

    public TMLPortletState getState() throws WGAPIException {
        
       if (_state == null) {
           TMLPortletStateStorage stateStorage = getStateStorage();
           if(stateStorage!=null)
        	   _state = stateStorage.getState(this);
       }
       return _state;
    }

    public Object getcontroller() throws WGAPIException, WGException{
    	ScopeObject obj = getState().fetchController(WGA.get(tmlContext));
    	if(obj!=null)
    		return obj.getObject();
    	else return null;
    }
    
    protected TMLPortletState createState(TMLPortletStateStorage storage) throws WGAPIException {
        
        // Determine the compliance version for this portlet
        
        Version complianceVersion = WGAVersion.VERSION;
        if (reg.getDesignDb() != null) {
            try {
                complianceVersion = WGA.get(tmlContext).design(reg.getDesignDb()).getVersionCompliance();
            }
            catch (Exception e) {
                tmlContext.getlog().error("Exception determining design compliance version of app '" + reg.getDesignDb() + "', e");
            }
        }
        Design controllerDesign = getControllerDesign();
        TMLPortletState state = new TMLPortletState(complianceVersion, this , tmlContext.gethttpsession().getId(), controllerDesign != null ? controllerDesign.getBaseReference() : null);
        state.initializeControllerState(WGA.get(tmlContext));
        return state;
    }

    protected static void addEventToQueue(PortletEvent event, HttpSession session) {
        
        LinkedMap events = getFiredEventsQueue(session);
        
        synchronized (events) {
            event.retrieveIndex();
            events.put(new Long(event.getIndex()), event);
            while (events.size() > EVENTQUEUE_MAX_SIZE) {
                events.remove(events.firstKey());
            }
        }
        
        
    }

    public static String getVarPrefix(String portletKey) {
        return portletKey + "_";
    }

    
    @CodeCompletion
    public void setmode(String mode) throws WGAPIException {
        validateStateModification();
    	getState().setMode(mode);
    }

    public void changeDesign(String tml) throws WGAPIException {
    	reg.setDesign(tml);
    	this.profile.save(true);
    }

    public String getDesign() {
    	return reg.getDesign();
    }

    
    @CodeCompletion
    public void fireevent(PortletEvent event) {
        // store event in hashSet and request for rendering through include tag
        @SuppressWarnings("unchecked")
        Set<PortletEvent> currentEvents = (HashSet<PortletEvent>) this.tmlContext.getrequest().getAttribute(REQATTRIB_FIRED_PORTLET_EVENTS);
        if (currentEvents == null) {
            currentEvents = new HashSet<PortletEvent>();
            this.tmlContext.getrequest().setAttribute(REQATTRIB_FIRED_PORTLET_EVENTS, currentEvents);
        }
        // set source
        event.setSource(getportletkey());
        event.setSourceName(getname());
        currentEvents.add(event);
        
        // Add to event queue
        addEventToQueue(event, this.tmlContext.gethttpsession());
    }

    
    @CodeCompletion
    public void fireevent(String eventname) {
    	fireevent(this.tmlContext.createevent(eventname));
    }

    
    @CodeCompletion
    public void fireevent(String eventName, Map<String,Object> params) {
        
        PortletEvent ev = this.tmlContext.createevent(eventName);
        for (Map.Entry<String,Object> param : params.entrySet()) {
            ev.addParameter(param.getKey(), param.getValue());
        }
        fireevent(ev);
        
    }

    @CodeCompletion
    public String registerportletforname(String name, DesignResourceReference ref, boolean overwrite) throws WGAPIException {
        
        // Verify portlet name rules
        if (!PORTLETNAME_PATTERN.matcher(name).matches()) {
            throw new WGIllegalArgumentException("Invalid portlet name '" + name + "'. The portletname may only consist of characters A-Z, a-z, 0-9, _, ., - and :");
        }
        
        // Verify portlet name not already used
        TMLPortlet portlet = getportletforname(name);
        if (portlet != null) {
            if (overwrite) {
                unregisterportletforname(name);
            }
            else {
                throw new WGIllegalArgumentException("Portlet name '" + name + "' already registered.");
            }
        }
        
        String pkey = registerportlet(ref.getDesignApp(), ref.getResourceName(), name);
        save();
        return pkey;
        
    }

    
    public String registerportletforname(String name, String appDbKey, String resourceName, boolean overwrite) throws WGException {
        
        DesignResourceReference ref = tmlContext.resolveDesignResource(appDbKey, resourceName);
        return registerportletforname(name, ref, overwrite);
        
    }

    
    @CodeCompletion
    public String registerportletforname(String name, String module, boolean overwrite) throws WGException {
        return registerportletforname(name, null, module, overwrite);
    }

    
    @CodeCompletion
    public String registerportletforname(String name, String module) throws WGException {
        return registerportletforname(name, null, module, false);
    }

    
    @CodeCompletion
    public String registerportletforname(String name, String moduleDb, String module) throws WGException {
        return registerportletforname(name, moduleDb, module, false);
    }

    @CodeCompletion
    public TMLPortlet getportletforname(String name) throws WGAPIException {
        WGPortlet portlet = tmlContext.getPortletRegistry().getPortletByName(reg.getApplicationDb(), reg, name);
        if (portlet != null) {
            return new TMLPortlet(this.tmlContext, this.profile, portlet);
        }
        else {
           return null;
        }
    }

    
    @CodeCompletion
    public void unregisterportletforname(String name) throws WGAPIException {
        
        // Remove portlet if still available. May be gone if registry is transient.
        TMLPortlet portlet = getportletforname(name);
        if (portlet != null) {
            portlet.unregister();
        }
        
    }

    
    @CodeCompletion
    public void unregisterchildportlets() throws WGAPIException {
        
        Iterator<String> keys = getchildrenkeys().iterator();
        while (keys.hasNext()) {
            String key = keys.next();
            TMLPortlet child = getportlet(key);
            if (child != null) {
                unregisterportlet(child.getportletkey());
            }
        }
        save();
        
        
    }

    public void prepareEventProcessing(Base tag) throws WGAPIException {
        
        
        TMLPortletState sessionContext = getState();
        LinkedMap list = TMLPortlet.getFiredEventsQueue(tag.getPageContext().getSession());
        
        // Look if the event queue proceeded since the last processed event
        if (list.size() > 0) {
            PortletEvent lastEvent = (PortletEvent) list.get(list.lastKey());
            if (lastEvent != null) {
                if (lastEvent.getIndex() > sessionContext.getLastProcessedEventIndex()) {
                    
                    // Find the start index for processing new events
                    Long startIndex;
                    Long lastProcessedIndex = new Long(sessionContext.getLastProcessedEventIndex());
                    if (list.containsKey(lastProcessedIndex)) {
                        startIndex = (Long) list.nextKey(lastProcessedIndex);
                    }
                    else {
                        startIndex = (Long) list.firstKey();
                    }
                    
                    // Set start index as WebTML option
                    tag.getStatus().setOption(Base.OPTION_PORTLET_EVENT_STARTINDEX, new Long(sessionContext.getLastProcessedEventIndex()),  TMLOption.SCOPE_GLOBAL);
                    
                    // Update last processed event index to be the newest event's index
                    sessionContext.setLastProcessedEventIndex(lastEvent.getIndex());
                }
            }
        }
    }

    
    public boolean isroot() {
        return (reg.isRoot());
    }

    
    @CodeCompletion
    public TMLPortlet getroot() throws WGAPIException {
        
        TMLPortlet portlet = this;
        while (!portlet.isroot()) {
            portlet = portlet.getparentportlet();
        }
        
        return portlet;
        
    }

    
    public TMLPortlet parent() throws WGAPIException {
        if (!isroot()) {
            return getparentportlet();
        }
        else {
            return null;
        }
    }

    
    public TMLPortlet child(String name) throws WGAPIException {
        return getportletforname(name);
    }

    
    @CodeCompletion
    public TMLContext getcontext() throws WGAPIException {
    	if (iscontextset()) {
    		return this.tmlContext.context(getState().getContextPath(), false);
    	} else {
    		return null;
    	}
    }

    public boolean iscontextset() throws WGAPIException {
        return getState().getContextPath() != null;
    }

    public boolean isprovisionalstate() throws WGAPIException {
        return getState().isOverwritableByClient();
    }

    
    @CodeCompletion
    public void setcontext(Context context) throws WGAPIException {
        validateStateModification();
    	if (context != null) {
    		getState().setContextPath(context.getpath());
    	} else {
    		getState().setContextPath(null);
    	}
    }

    public String getVarPrefix() {
        return getVarPrefix(reg.getKey());
    }

    public TMLPortlet() {
        super();
    }

    
    @CodeCompletion
    public TMLPortlet getsourceportlet(PortletEvent event) throws WGAPIException {
        return getportlet(event.getSource());
    }

    public WGPortlet getReg() {
        return reg;
    }

    
    public String getportletpath() throws WGAPIException {
        
        List<String> names = new ArrayList<String>();
        TMLPortlet p = this;
        while (!p.isroot()) {
            names.add(p.getname());
            p = p.getparentportlet();
        }
        
        Collections.reverse(names);
        return "/" + WGUtils.serializeCollection(names, "/");
        
    }

    public TMLUserProfile getProfile() {
        return profile;
    }

    private void validateStateModification() throws WGAPIException {
        
        if (isroot()) {
            throw new WGIllegalStateException("Cannot modify state of root portlet");
        }
        
        TMLPortletStateStorage storage = tmlContext.getPortletStateStorage();
        if (storage instanceof TMLPortletStateTransientStorage) {
            if (isroot()) {
                throw new WGIllegalStateException("Cannot modify portlet state when WebTML request is on root portlet level, only from inside another portlet");
            }
        }
        
    }

    
    public boolean equals(Object obj) {
    
        if (obj instanceof TMLPortlet) {
            TMLPortlet otherPortlet = (TMLPortlet) obj;
            return WGUtils.nullSafeEquals(otherPortlet.reg.getApplicationDb(), reg.getApplicationDb()) && otherPortlet.getportletkey().equals(getportletkey());
        }
    
        return false;
             
    }

    
    public int hashCode() {
        return (new HashCodeBuilder()).append(reg.getApplicationDb()).append(reg.getKey()).toHashCode();
    }

    public void markedited() throws WGAPIException {
        getState().setTransmitToClient(true);
    }
    
    public Design getControllerDesign() throws WGAPIException {
        
        try {
            String theDesign;
            if (isroot()) {
                theDesign = WGA.get(tmlContext).call().getOuterLayout();
            }
            else {
                theDesign = getDesign();
            }
            
            if (theDesign == null) {
                return null;
            }
            
            WGA wga = WGA.get(tmlContext);
            Design baseDesign = gettmldb() != null ? wga.design(gettmldb()) : wga.design();
            return baseDesign.resolve(getDesign() + ".controller");
        }
        catch (Exception e) {
            throw new WGBackendException("Exception getting controller design", e);
        }
    }
    

}