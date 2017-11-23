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

package de.innovationgate.wgpublisher.websockets;

import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpSession;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

public class PageConnectionManager {
    
    public Map<String,WeakReference<PageConnection>> _activeConnections = new ConcurrentHashMap<String, WeakReference<PageConnection>>();
    
    public PageConnection getConnection(String pageId) {
        
        WeakReference<PageConnection> conRef = _activeConnections.get(pageId);
        if (conRef != null) {
            return conRef.get();
        }
        else {
            return null;
        }
        
    }

    public PageConnection activateConnection(HttpSession session, String pageId, TMLPageWebSocket socket, String windowId) throws WGException {
        
        // A reconnect (maybe from cluster failover)
        WeakReference<PageConnection> connRef = _activeConnections.get(pageId);
        if (connRef != null) {
            PageConnection conn = connRef.get();
            if (conn != null) {
                return doActivateConnection(conn, session, socket, windowId);
            }
        }
        
        // A new connection
        Queue<PageConnection> unconfirmedConnections = getNewPageConnectionsQueue(session);
        for (PageConnection pageConnection : unconfirmedConnections) {
            if (pageConnection.getPageId().equals(pageId)) {
                return doActivateConnection(pageConnection, session, socket, windowId);
            }
        }
        
        return null;
        
    }

    private PageConnection doActivateConnection(PageConnection conn, HttpSession session, TMLPageWebSocket socket, String windowId) throws WGException {
        validateOrigin(socket, conn);
        conn.assignSocket(socket, windowId);
        putConnection(session, conn);
        
        WGA.get(session).app(conn.getDbKey()).createEvent("websocket=connect")
        	.param("windowId", windowId)
        	.param("pageId", conn.getPageId())
        	.fireOnLocalServer();
        
        return conn;
    }
    
    private void validateOrigin(TMLPageWebSocket socket, PageConnection conn) throws InvalidWebSocketSessionException {
        
        if (socket.getOrigin() == null) {
            if (!"true".equals(System.getProperty("de.innovationgate.wga.unittest"))) {
                throw new InvalidWebSocketSessionException("TMLPage WebSocket connetion had no Origin header");
            }
            else {
                return;
            }
        }
        else if (!socket.getOrigin().getHost().equals(conn.getRequestURI().getHost())) {
            throw new InvalidWebSocketSessionException("Origin '" + socket.getOrigin() + "' does not match host name of initial page connection: " + conn.getRequestURI().getHost());
        }
        
    }

    private void putConnection(HttpSession session, PageConnection con) {
        _activeConnections.put(con.getPageId(), new WeakReference<PageConnection>(con));
        if (session != null) {
            Map<String,PageConnection> activeConnections = getActiveConnectionsSessionMap(session);
            activeConnections.put(con.getPageId(), con);
        }
    }
    
    @SuppressWarnings("unchecked")
    public Map<String, PageConnection> getActiveConnectionsSessionMap(HttpSession session) {
        return (Map<String, PageConnection>) session.getAttribute(WGACore.ATTRIB_ACTIVE_PAGECONNECTIONS);
    }
    
    public void activateSessionPageConnections(HttpSession session) {
        Map<String,PageConnection> conns = getActiveConnectionsSessionMap(session);
        for (PageConnection con : conns.values()) {
            _activeConnections.put(con.getPageId(), new WeakReference<PageConnection>(con));
        }
    }

    public void removeConnection(HttpSession session, PageConnection con) {
        try {
			WGA.get(session).app(con.getDbKey()).createEvent("websocket=disconnect")
				.param("windowId", con.getWindowId())
				.param("pageId", con.getPageId())
			    .fireOnLocalServer(false);
		} catch (WGException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
        _activeConnections.remove(con.getPageId());
        if (session != null) {
        	try{
	            Map<String,PageConnection> activeConnections = getActiveConnectionsSessionMap(session);
	            activeConnections.remove(con.getPageId());
        	}
        	catch(Exception e){}
        }        
        con.close();
    }

    @SuppressWarnings("unchecked")
    public static Queue<PageConnection> getNewPageConnectionsQueue(HttpSession httpSession) {
        return (Queue<PageConnection>) httpSession.getAttribute(WGACore.ATTRIB_NEW_PAGE_CONNECTIONS);
    }

    public PageConnection newConnection(WGA wga) throws WGException {
        PageConnection pageConnection = new PageConnection(wga);
        getNewPageConnectionsQueue(wga.session().getJavaHttpSession()).add(pageConnection);
        return pageConnection;
    }

    public Map<String, WeakReference<PageConnection>> getActiveConnections() {
        return _activeConnections;
    }
    
    public void shutdown() {
        closeAllWebSockets();
    }

    public void closeAllWebSockets() {
        for (WeakReference<PageConnection> conn : _activeConnections.values()) {
            conn.get().getWebSocket().close();
        }
    }

}
