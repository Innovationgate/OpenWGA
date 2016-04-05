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

import java.util.Iterator;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.wga.config.HttpSessionManagerConfiguration;

/**
 * default implementation of a custom session manager
 * just for testing purpose
 */
public class DefaultWGAHttpSessionManager extends AbstractWGAHttpSessionManager {

    private Map<String, WGAHttpSession> _sessions = new ConcurrentHashMap<String, WGAHttpSession>();
    private Timer _sessionCleanupTimer;
    private SessionCleanupTask _sessionCleanupTask;
    
    private class SessionCleanupTask extends TimerTask {

        @Override
        public void run() {
            try {
                Iterator<WGAHttpSession> sessions = _sessions.values().iterator();
                while (sessions.hasNext()) {
                    WGAHttpSession session = sessions.next();
                    if (!isValid(session)) {
                        invalidateSession(session);
                        sessions.remove();
                    }
                }
            } catch (Throwable e) {
                getCore().getLog().error("DefaultHttpSessionManager.SessionCleanupTask failed.", e);
            }
        }
        
    }

    public DefaultWGAHttpSessionManager() {
        _sessionCleanupTask = new SessionCleanupTask();
        _sessionCleanupTimer = new Timer("DefaultHttpSessionManager.SessionCleanupTask");
        _sessionCleanupTimer.schedule(_sessionCleanupTask, 1000*120);
    }
    
    @Override
    public WGAHttpSession getSession(String id) {
        WGAHttpSession session = _sessions.get(id);
        if (session != null) {
            if (isValid(session)) {
                return session;
            } else {
                invalidateSession(session);
                _sessions.remove(id);
            }
        }
        return null;
    }

    @Override
    public WGAHttpSession createSession() {
        String id = UIDGenerator.generateUID();
        WGAHttpSession session = new WGAHttpSession(getContext(), id);
        _sessions.put(id, session);
        fireSessionCreatedEvent(session);
        return session;
    }
    
 
    @Override
    public void shutdown() {
        if (_sessionCleanupTimer != null) {
            _sessionCleanupTimer.cancel();
        }
        
        // invalidate all sessions
        for (WGAHttpSession session : _sessions.values()) {
            invalidateSession(session);
        }
        _sessions.clear();                
    }

    @Override
    public void requestFinished(HttpServletRequest request, HttpServletResponse response) {        
    }

    @Override
    public void startup(HttpSessionManagerConfiguration config) {
    }
}
