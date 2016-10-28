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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpSession;
import javax.websocket.Session;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.websockets.AbstractWebSocket;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.websockets.TMLPageWebSocket;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;

public class PageScopeResolver implements ScopeResolver {

    protected static final String PARAM_WEBSOCKET = "$websocket";

    @Override
    public ScopeObjectRegistry resolveObjectRegistry(WGA wga, DesignResourceReference ref) throws WGException {
        TMLPageImpl page = (TMLPageImpl) Unlocker.unlock(wga).tmlPage();
        if (page != null && page.isAvailable()) {
             PageConnection c = page.getOrCreatePageConnection();
             if(c!=null)
            	 return	c.getScopeObjectRegistry();
        }
        
        if (wga.session().isWebSocketSessionAvailable()) {
            Session session = wga.session().getJavaWebSocketSession();
            AbstractWebSocket socket = AbstractWebSocket.getFromSession(session);
            if (socket instanceof TMLPageWebSocket) {
                return ((TMLPageWebSocket) socket).getPageConnection().getScopeObjectRegistry();
            }
        }
        
        return null;
    }

    @Override
    public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException {

        List<PageConnection> connections = new ArrayList<PageConnection>();
        Event.Scope eventScope = event.getScope();
        if (eventScope == Event.Scope.CLUSTER || eventScope == Event.Scope.SERVER) {
            for (WeakReference<PageConnection> pageConnectionRef : wga.getCore().getPageConnectionManager().getActiveConnections().values()) {
                PageConnection pageConnection = pageConnectionRef.get();
                if (pageConnection != null) {
                    connections.add(pageConnection);
                }
            }
        }
        else if (eventScope == Event.Scope.SESSION && event.getSessionId() != null) {
            HttpSession session = wga.getCore().getActiveHttpSessions().get(event.getSessionId());
            if (session != null) {
                connections.addAll(wga.getCore().getPageConnectionManager().getActiveConnectionsSessionMap(session).values());
            }
        }
        
        List<ApplicationEventReceiver> regs = new ArrayList<ApplicationEventReceiver>();
        for (PageConnection pageConnection : connections) {
            if (pageConnection.getDbKey().equals(dbKey)) {
                WGA eventWga = WGA.get(null, null, pageConnection.getSession(), wga.getCore());
                ApplicationEventReceiver receiver = new ApplicationEventReceiver(eventWga, pageConnection.getScopeObjectRegistry());
                receiver.getScopeEventParams().put(PARAM_WEBSOCKET, pageConnection.getWebSocket());
                regs.add(receiver);
            }
        }
        return regs;
        
    }

    @Override
    public boolean isApplicationEventReceiver(Event.Scope scope) {
        return true;
    }
    
    @Override
    public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException {
    }


}
