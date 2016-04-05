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

import java.util.ArrayList;
import java.util.List;

import javax.websocket.Session;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.events.Event.Scope;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.websockets.AbstractWebSocket;
import de.innovationgate.wgpublisher.websockets.IndependentWebSocket;

public class WebsocketScopeResolver implements ScopeResolver {

    @Override
    public ScopeObjectRegistry resolveObjectRegistry(WGA wga, DesignResourceReference ref) throws WGException {
        
        WGA unlocked = Unlocker.unlock(wga);
        de.innovationgate.wga.server.api.Session wgaSession = unlocked.session();
        
        if (wgaSession.isWebSocketSessionAvailable()) {
            Session session = unlocked.session().getJavaWebSocketSession();
            return AbstractWebSocket.getFromSession(session).getScopeObjectRegistry();
        }
        
        return null;
            
        
    }

    @Override
    public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException {
        if (wga.session().isHttpSessionAvailable()) {
            ObjectScope.TMLPAGE.notifyScopeObjectStateChange(scopeObject, wga, ref);
        }
    }

    @Override
    public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException {

        List<ApplicationEventReceiver> regs = new ArrayList<ApplicationEventReceiver>();
        for (IndependentWebSocket socket : wga.getCore().getIndependentWebSocketManager().getActiveSockets().values()) {
            Session s = socket.getSession();
            ScopeObjectRegistry reg = AbstractWebSocket.getFromSession(s).getScopeObjectRegistry();
            ApplicationEventReceiver receiver = new ApplicationEventReceiver(WGA.get(s), reg);
            receiver.getScopeEventParams().put(PageScopeResolver.PARAM_WEBSOCKET, socket);
            regs.add(receiver);
        }
        
        regs.addAll(ObjectScope.TMLPAGE.resolveApplicationEventReceivers(wga, dbKey, event));
        
        return regs;
        
    }

    @Override
    public boolean isApplicationEventReceiver(Scope scope) {

        if (scope == Scope.CLUSTER || scope == Scope.SERVER) {
            return true;
        }
        
        return false;
        
    }

}
