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
import java.util.Collection;
import java.util.List;

import javax.servlet.http.HttpSession;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACore.SessionLoginMap;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.events.Event.Scope;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public class SessionScopeResolver implements ScopeResolver {
    
    @Override
    public ScopeObjectRegistry resolveObjectRegistry(final WGA wga, DesignResourceReference ref) throws WGException {
        return getRegistry(Unlocker.unlock(wga).session().getJavaHttpSession(), wga.app().db().getDbReference(), true);
    }

    public static ScopeObjectRegistry getRegistry(HttpSession session, String dbKey, boolean create) throws WGException {
        String regKey = WGACore.SESSION_SCOPEOBJECTREGISTRY_BASE + dbKey;
        ScopeObjectRegistry sessionRegistry = (ScopeObjectRegistry) session.getAttribute(regKey);
        if (sessionRegistry == null && create) {
            sessionRegistry = new ScopeObjectRegistry(ObjectScope.SESSION, "Session " + session.getId() + ", Database " + dbKey, new NoopScopeObjectContextCreator());
            session.setAttribute(regKey, sessionRegistry);
            
            // Look if the user is already logged in. If so we must now trigger the event
            SessionLoginMap sessionLogins = WGACore.getSessionLogins(session);
            WGA wga = WGA.get(null, session);
            App app = wga.app(dbKey);
            if (!app.domain().auth().isAnonymous()) {
                app.createEvent("auth=login").param("userName", app.getUserName()).param("authType", app.domain().auth().getAuthenticationType()).fireOnSession();
            }
        }
        return sessionRegistry;
    }
    
    @Override
    public boolean isApplicationEventReceiver(Event.Scope scope) {
        return true;
    }
    
    @Override
    public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException {

        List<ApplicationEventReceiver> regs = new ArrayList<ApplicationEventReceiver>();
        Collection<HttpSession> sessions = new ArrayList<HttpSession>();
        Event.Scope eventScope = event.getScope();
        if (eventScope == Scope.SERVER || eventScope == Scope.CLUSTER) {
            sessions.addAll(wga.getCore().getActiveHttpSessions().values());
        }
        else if (eventScope == Scope.SESSION && event.getSessionId() != null) {
            HttpSession session = wga.getCore().getActiveHttpSessions().get(event.getSessionId());
            if (session != null) {
                sessions.add(session);
            }
        }
        
        for (HttpSession session : sessions) {
            WGA eventWga = WGA.get(null, null, session, wga.getCore());
            ScopeObjectRegistry registry = getRegistry(session, dbKey, false);
            if (registry != null) { // Sessions without registries have not yet accessed the current database
                regs.add(new ApplicationEventReceiver(eventWga, registry));
            }
        }
        return regs;
        
    }
    
    @Override
    public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException {
    }

}
