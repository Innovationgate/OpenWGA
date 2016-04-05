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

import java.util.HashSet;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import de.innovationgate.wga.config.HttpSessionManagerConfiguration;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.sessions.api.WGAHttpSessionManager;

/**
 * can be subclassed to gain control over HTTPSession management for the OpenWGA servlet context
 * 
 * Note:
 * this is currently not a public API - the interface may change in future OpenWGA releases 
 */
public abstract class AbstractWGAHttpSessionManager implements WGAHttpSessionManager {   
    
    private ServletContext _context;
    private WGACore _core;
    private Set<HttpSessionListener> _sessionListeners = new HashSet<HttpSessionListener>();
    private boolean _debug = false;

   /**
    * should try to retrieve an existing and valid session with the given id
    * @param id
    * @return existing and valid session, null otherwise
    */
    @Override
    public abstract WGAHttpSession getSession(String id);
    
    /**
     * creates a new session and fires SessionCreatedEvent
     * @return
     */
    public abstract WGAHttpSession createSession();
    
    public void setContext(ServletContext context) {
        _context = context;
        _core = WGACore.retrieve(context);
    }
    
    public ServletContext getContext() {
        return _context;
    }
    
    public WGACore getCore() {
        return _core;
    }
    
    /**
     * shutdown and invalidate all sessions
     */
    public abstract void shutdown();
    
    /**
     * called during WGA core startup
     */
    public abstract void startup(HttpSessionManagerConfiguration config);
    
    /**
     * call back if an http request has been finished
     * @param request
     */
    public abstract void requestFinished(HttpServletRequest request, HttpServletResponse response);
    
    
    public void addListener(HttpSessionListener listener) {
        _sessionListeners .add(listener);
    }

    
    public void removeListener(HttpSessionListener listener) {
        _sessionListeners.remove(listener);
    }
    
    protected void fireSessionCreatedEvent(HttpSession session) {
        HttpSessionEvent event = new HttpSessionEvent(session);
        for (HttpSessionListener listener : _sessionListeners) {
            try {
                listener.sessionCreated(event);
            } catch (Throwable e) {
                _core.getLog().error("Failed to dispatch session created event.", e);
            }
        }
    }
    
    protected void fireSessionDestroyedEvent(HttpSession session) {
        HttpSessionEvent event = new HttpSessionEvent(session);
        for (HttpSessionListener listener : _sessionListeners) {
            try {
                listener.sessionDestroyed(event);
            } catch (Throwable e) {
                _core.getLog().error("Failed to dispatch session destroyed event.", e);
            }
        }
    }
    
    protected void invalidateSession(WGAHttpSession session) {
        try {
            if (!session.isInvalidated()) {
                fireSessionDestroyedEvent(session);
                session.invalidate();
            }
        } catch (Throwable e) {
            _core.getLog().error("Failed to invalidate session '" + session.getId() + "'.", e);
        }
    }
    

    protected boolean isValid(WGAHttpSession session) {
        if (session.isInvalidated()) {
            return false;
        }
        if (session.getMaxInactiveInterval() < 0) {
            return true;
        }
        return System.currentTimeMillis() - session.getLastAccessedTime() < session.getMaxInactiveInterval() * 1000;
    }

    public boolean isDebug() {
        return _debug;
    }

    public void setDebug(boolean debug) {
        _debug = debug;
    }

    public void clearListeners() {
        _sessionListeners.clear();
    }

}
