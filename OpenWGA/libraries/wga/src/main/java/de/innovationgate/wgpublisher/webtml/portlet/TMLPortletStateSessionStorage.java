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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpSession;

import org.apache.commons.collections.map.LinkedMap;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wgpublisher.WGACore;

public class TMLPortletStateSessionStorage implements TMLPortletStateStorage {
    
    private HttpSession _session;

    
    public TMLPortletStateSessionStorage(HttpSession ses) {
        _session = ses;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLPortletStateStorage#getState(de.innovationgate.wgpublisher.webtml.utils.TMLPortlet, javax.servlet.http.HttpServletRequest)
     */
    @Override
    public TMLPortletState getState(TMLPortlet portlet) throws WGAPIException {
        
        String completeKey = getSessionContextKey(portlet);
        
        synchronized (_session) {
            
            // Get - conditionally create session context map
            Map<String, TMLPortletState> contexts = getSessionMap();
        
            // Get - conditionally create individual session context
            TMLPortletState context = (TMLPortletState) contexts.get( completeKey );
            if (context == null) {
                context = portlet.createState(this);
                
                // Set event index to current last index, so events fired before creation of this context are not executed for it
                LinkedMap list = TMLPortlet.getFiredEventsQueue(_session);
                if (!list.isEmpty()) {
                    PortletEvent event = (PortletEvent) list.get(list.lastKey());
                    context.setLastProcessedEventIndex(event.getIndex());
                }
                                
                contexts.put(completeKey, context);
            }
            return context;
        }
        
    }
    
    private String getSessionContextKey(String appKey, String portletKey) {
        return appKey + "/" + portletKey;
    }
    
    private String getSessionContextKey(TMLPortlet portlet) {
        return getSessionContextKey(portlet.getProfile().getprofile().getDatabase().getDbReference(), portlet.getportletkey());
    }
    
    private Map<String,TMLPortletState> getSessionMap() {
        
        synchronized (_session) {
            Map<String,TMLPortletState> contexts = (Map<String,TMLPortletState>) _session.getAttribute(WGACore.SESSION_PORTLETMODES);
            if (contexts == null) {
                contexts = Collections.synchronizedMap(new HashMap<String,TMLPortletState>());
                _session.setAttribute(WGACore.SESSION_PORTLETMODES, contexts);
            }
            return contexts;
        }
        
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLPortletStateStorage#disposeState(de.innovationgate.wgpublisher.webtml.utils.TMLPortlet, javax.servlet.http.HttpServletRequest)
     */
    @Override
    public void disposeState(String appDb, String portletKey) {
        // Remove the session context
        String completeKey = getSessionContextKey(appDb, portletKey);
        
        synchronized (_session) {
            Map<String,TMLPortletState> contexts = getSessionMap();
            contexts.remove( completeKey );
        }
    }
    
    @Override
    public boolean isStoreSessionVarsAtState() {
        return true;
    }
    
    @Override
    public void disposeChildState(String appDb, String portletKey, String childName) throws WGAPIException {
        throw new WGNotSupportedException("Disposing child portlet states by child name is not supported by the session-based storage");
    }


}
