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

package de.innovationgate.wgpublisher.sessions;

import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.apache.log4j.Logger;

import com.google.common.collect.EvictingQueue;
import com.google.common.collect.Queues;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class WGAHttpSessionListener implements HttpSessionListener {
    
    protected static Logger LOG = Logger.getLogger("wga.httpsessions");

    @Override
    public void sessionCreated(HttpSessionEvent ev) {
        
        if (!isEnabledSession(ev.getSession())) {
            return;
        }
        
        LOG.debug("Session created: " + ev.getSession().getId());
        HttpSession session = ev.getSession();
        WGACore.INSTANCE.getActiveHttpSessions().put(ev.getSession().getId(), ev.getSession());
        
        // LIfecycle monitoring
        session.setAttribute(WGACore.ATTRIB_SESSION_LIFECYCLE_LISTENER, new WGAHttpSessionActivationListener());
        
        Integer sessionTimeout = (Integer) WGACore.INSTANCE.getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_SERVER_SESSIONTIMEOUT);
        
        // We only want to set it if it really differs from the default, which is enforced by web.xml
        if (sessionTimeout.intValue() != WGACore.SERVEROPTIONDEFAULT_SESSIONTIMEOUT) {
            ev.getSession().setMaxInactiveInterval(sessionTimeout.intValue() * 60);
        }
        
        initializeSessionAttributes(session);
        
    }
    
    /**
     * With activated WGA session manager check if this is really a WGA session, ignore otherwise
     */
    private boolean isEnabledSession(HttpSession session) {
        
        AbstractWGAHttpSessionManager wgaSessionManager = WGACore.INSTANCE.getHttpSessionManager();
        if (wgaSessionManager != null && !(session instanceof WGAHttpSession)) {
            return false;
        }
        
        return true;
    }

    /**
     * Initializes attributes on a session necessary for correct WebTML operations
     * This might as well be called for fake sessions, so it should not include any management that goes beyond the current call.
     */
    public static void initializeSessionAttributes(HttpSession session) {
        // Collections for page connections
        session.setAttribute(WGACore.ATTRIB_ACTIVE_PAGECONNECTIONS, new ConcurrentHashMap<String,PageConnection>());
        Queue<PageConnection> queue = Queues.<PageConnection>synchronizedQueue(EvictingQueue.<PageConnection>create(100));
        session.setAttribute(WGACore.ATTRIB_NEW_PAGE_CONNECTIONS, queue);
        
        // Other initialisations
        session.setAttribute(TMLContext.SESSIONATTRIB_PROCESSCONTEXTS, new ProcessContextRegistration());
        session.setAttribute(WGACore.ATTRIB_TMLFORM, new TMLContext.PersistentFormsMap());
        session.setAttribute(WGPDispatcher.SESSION_TEMPORARYDOWNLOADS, new WGPDispatcher.TemporaryDownloadsMap());
    }

    @Override
    public void sessionDestroyed(HttpSessionEvent ev) {
        
        if (!isEnabledSession(ev.getSession())) {
            return;
        }
        
        LOG.debug("Session destroyed: " + ev.getSession().getId());
        WGACore.INSTANCE.getActiveHttpSessions().remove(ev.getSession().getId());
        
        try {
            WGA wga = WGA.get();
            for (Map.Entry<Object,DBLoginInfo> dbLoginInfo : WGACore.getSessionLogins(ev.getSession()).entrySet()) {
                if (!WGDatabase.ANONYMOUS_USER.equals(dbLoginInfo.getValue().getUserName())) {
                    for (String dbKey : wga.domain(String.valueOf(dbLoginInfo.getKey())).getAppKeys()) {
                        WGA.get(null, null, ev.getSession(), wga.getCore()).app(dbKey).createEvent("auth=sessionClose")
                        .param("userName", dbLoginInfo.getValue().getUserName())
                        .param("sessionId", ev.getSession().getId())
                        .param("authType", dbLoginInfo.getValue().getAuthenticationType())
                        .fireOnLocalServer();
                    }
                }
            }
        }
        catch (WGException e) {
            WGACore.INSTANCE.getLog().error("Exception destroying session", e);
        }
    }

}
