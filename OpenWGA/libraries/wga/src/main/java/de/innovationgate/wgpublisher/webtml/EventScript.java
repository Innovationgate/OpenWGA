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

package de.innovationgate.wgpublisher.webtml;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpSession;

import org.apache.commons.collections.map.LinkedMap;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class EventScript extends ActionBase {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String _onevent;
    
    private String _action;
    
    public static class Status extends ActionBase.Status {
        private boolean _executeEvent;
    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }

    public void tmlStartTag() throws TMLException, WGException {
        
        Status status = (Status) getStatus();
        String name = getOnevent();
        if (name == null) {
            throw new TMLException("Event name was not specified", true);
        }
        
        TMLPortlet portlet = getTMLContext().getportlet();
        if (portlet == null) {
            throw new TMLException("Personalisation is not available", true);
        }
        
        // Test if an event of that name has been issued since last run
        status._executeEvent = false;
        
        // Transient variant: Only trigger for events explicitly sent by client
        if (getTMLContext().getPortletStateStorage() instanceof TMLPortletStateTransientStorage) {
            PortletEvent event = getTMLContext().fetchRequestPortletEvent();
            if (event != null && name.equals(event.getName())) {
                getTMLContext().setvar("portletEvents", Collections.singletonList(event));
                getTMLContext().setvar("portletEvent", event);
                status._executeEvent = true;
            }
        }
        
        // Persistent variant: Find unprocessed events in the queue
        else {
            Long beginIndex = (Long) getTMLContext().option(Base.OPTION_PORTLET_EVENT_STARTINDEX);
            if (beginIndex != null) {
                List<PortletEvent> events = findEventsOfName(name, beginIndex);            
                if (events.size() > 0) {
                    getTMLContext().setvar("portletEvents", events);
                    getTMLContext().setvar("portletEvent", events.get(events.size() - 1));
                    status._executeEvent = true;
                }
            }
        }

        setEvalBody(status._executeEvent);
        
        super.tmlStartTag();
    }

    /**
     * Searches the fired events queue for events of a name, beginning at a specified index
     * @param name The event name to search for
     * @param index The start index in the queue
     * @return a list of found events
     * @throws WGAPIException 
     */
    private List<PortletEvent> findEventsOfName(String name, Long index) throws WGAPIException {

        List<PortletEvent> foundEvents = new ArrayList<PortletEvent>();
        HttpSession session = getPageContext().getSession();
        LinkedMap events = TMLPortlet.getFiredEventsQueue(session);
        
        if (events.size() == 0) {
            return foundEvents;
        }
        
        // Find the start index. This is either the index after the last processed index, or - if the last processed
        // index is not available in the queue - the first index in the queue. 
        if (events.containsKey(index)) {
            index = (Long) events.nextKey(index);
        }
        else {
            index = (Long) events.firstKey();
        }
        
        
        synchronized (events) {
            PortletEvent event;
            while (index != null) {
                event = (PortletEvent) events.get(index);
                String targetPortletKey = event.getTargetPortletKey();
                if (targetPortletKey == null || targetPortletKey.equals(getTMLContext().getportlet().getportletkey())) {
                    if (event.getName().equalsIgnoreCase(name)) {
                        foundEvents.add(event);
                    }
                }
                index = (Long) events.nextKey(index);
                
            }
        }
        
        return foundEvents;
        
    
    }

    public String getOnevent() {
        return getTagAttributeValue("onevent", _onevent, null);
    }

    public void setOnevent(String name) {
        this._onevent = name;
    }

    public void tmlEndTag() throws TMLException, WGAPIException {
        
        Status status = (Status) getStatus();
        
        // Exec event
        if (status._executeEvent) {
            executeEventScript();
            try {
                getTMLContext().removevar("portletEvent");
            }
            catch (WGAPIException e) {
                throw new TMLException("Exception removing portletEvent variable", e, true);
            }
        }
        else {
            setResult("");
        }
        
        String actionId = getAction();
        TMLAction action = getTMLContext().getActionByID(actionId, getDesignDBKey());
        if (action == null) {
            throw new TMLException("Unknown event script action: " + actionId);
        }

        
        // Register as client-side listener for this event
        StringBuffer eventReg = new StringBuffer();
        eventReg.append("<script type=\"text/javascript\">\n");
        writePortletEventRegistration(eventReg, action, getActionCallParameters(), isKeepParamsOnAJAX(), getOnevent());
        eventReg.append("</script>\n");
        setSuffix(eventReg.toString());
        
        
    }

    private void executeEventScript() {
        
        Status status = (Status) getStatus();
        de.innovationgate.webgate.api.WGContent content = this.getTMLContext().content();
        ExpressionEngine engine;
        engine = ExpressionEngineFactory.getTMLScriptEngine();
        String expr = this.getResultString(false);
        
        Map additionalObjects = new HashMap();
        
        additionalObjects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, getTagDescription());
        ExpressionResult result = engine.evaluateExpression(expr, this.getChildTagContext(), ExpressionEngine.TYPE_SCRIPT, additionalObjects);
        
        if (result.isError()) {
            addExpressionWarning(expr, result);
        }
        this.setResult(result.getResult());
        
    }

    public String getAction() {
        return getTagAttributeValue("action", _action, "$refresh");
    }

    public void setAction(String action) {
        _action = action;
    }
    
    @Override
    public String getAjax() {
        return getTagAttributeValue("ajax", ajax, "true");
    }

}
