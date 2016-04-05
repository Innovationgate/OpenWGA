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

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpSession;
import javax.websocket.CloseReason;
import javax.websocket.CloseReason.CloseCodes;
import javax.websocket.EndpointConfig;
import javax.websocket.HandshakeResponse;
import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.HandshakeRequest;
import javax.websocket.server.ServerEndpoint;
import javax.websocket.server.ServerEndpointConfig;

import org.apache.log4j.Logger;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.WebSocket;
import de.innovationgate.wgpublisher.WGACore;

@ServerEndpoint(value=TMLPageWebSocket.PATH,configurator=TMLPageWebSocket.Configurator.class)
public class TMLPageWebSocket extends AbstractWebSocket implements WebSocket {
    
    public static final Logger LOG = Logger.getLogger("wga.websocket");
    
    public static final String PATH = "/services/websocket/page";
    
    public static final String PROP_INVALID = "Invalid";
    public static final String PROP_HTTPSESSION = "HttpSession";
    public static final String PROP_WINDOWID = "WindowId";
    public static final String PROP_PAGEID = "PageId";
    public static final String PROP_ORIGIN = "Origin";

    private static final String MSGTYPE_FIREPORTLETEVENT = "firePortletEvent";
    private static final String MSGTYPE_CUSTOM = "custom";
    private static final String MSGTYPE_HANDSHAKE = "handshake";
    public static class Configurator extends ServerEndpointConfig.Configurator {
        
        @Override
        public void modifyHandshake(ServerEndpointConfig sec, HandshakeRequest request, HandshakeResponse response) {
            try {
                                
                HttpSession session = null;
                boolean useSessionWorkaround = (Boolean) WGACore.INSTANCE.getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_WEBSOCKETS_SESSION_WORKAROUND);
                if (WGACore.INSTANCE.getHttpSessionManager() != null) {
                    useSessionWorkaround = true;
                }
                
                if (useSessionWorkaround) {
                    session = WGACore.INSTANCE.getActiveHttpSessions().get(request.getParameterMap().get("sessionId").get(0));
                }
                else {
                    session = (HttpSession) request.getHttpSession();
                }
                
                if (session == null) {
                    sec.getUserProperties().put(PROP_INVALID, "Web Socket request without HTTP Session");
                    return;
                }
                
                super.modifyHandshake(sec, request, response);
                sec.getUserProperties().put(PROP_HTTPSESSION, session);
                sec.getUserProperties().put(PROP_WINDOWID, request.getParameterMap().get("windowId"));
                sec.getUserProperties().put(PROP_PAGEID, request.getParameterMap().get("pageId"));
                
                if (request.getHeaders().containsKey("Origin")) {
                    sec.getUserProperties().put(PROP_ORIGIN, request.getHeaders().get("Origin").get(0));
                }
            }
            catch (Throwable t) {
                WGACore.INSTANCE.getLog().error("Exception doing web socket handshake", t);
            }
        }
        
        @Override
        public boolean checkOrigin(String originHeaderValue) {
            if (originHeaderValue == null && !"true".equals(System.getProperty("de.innovationgate.wga.unittest"))) { // HTMLUnit 2.19 does not send Origin header....
                return false;
            }
            else {
                return true; // Exact check with page connections initial URL is done on activating the connection
            }
                
        }
        
    }
    
    private HttpSession _httpSession;
    private String _windowId;
    private PageConnection _pageConnection;
    private String _pageId;
    private URI _origin;

    @SuppressWarnings("unchecked")
    @OnOpen
    public void open(Session session, EndpointConfig cfg) throws WGException, URISyntaxException {
        
        try {
            String invalidMsg = (String) cfg.getUserProperties().get(PROP_INVALID);
            if (invalidMsg != null) {
                throw new InvalidWebSocketSessionException("Web Socket request without Http Session");
            }
            
            initSession(session);
            _httpSession = (HttpSession) cfg.getUserProperties().get(PROP_HTTPSESSION);
            _windowId = ((List<String>) cfg.getUserProperties().get(PROP_WINDOWID)).get(0);
            _pageId = ((List<String>) cfg.getUserProperties().get(PROP_PAGEID)).get(0);
            if (cfg.getUserProperties().containsKey(PROP_ORIGIN)) {
                _origin = new URI((String) cfg.getUserProperties().get(PROP_ORIGIN));
            }
            _pageConnection = WGACore.INSTANCE.getPageConnectionManager().activateConnection(_httpSession, _pageId, this, _windowId);
            
            if ((Boolean) WGACore.INSTANCE.getServicesServerOptionReader().readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_WEBSOCKETS) == false) {
                throw new InvalidWebSocketSessionException("Web Sockets are disabled");
            }
            
            if (_pageConnection == null) {
                throw new InvalidWebSocketSessionException("Web Socket request without preregistered Page Connection");
            }
            
            LOG.debug("Opening socket session " + getSession().getId() + " for HTTP session " + _httpSession.getId() + ", Window " + _windowId + ", Page " + _pageId);
            
            JsonObject msg = new JsonObject();
            msg.addProperty("type", MSGTYPE_HANDSHAKE);
            doSend(msg, true);
        }
        catch (InvalidWebSocketSessionException e) {
            try {
                session.close(new CloseReason(CloseCodes.VIOLATED_POLICY, e.getMessage()));
            }
            catch (IOException e2) {
            }
            throw e;
        }
        catch (Throwable t) {
            LOG.error("Exception building websocket connection", t);
            try {
                session.close(new CloseReason(CloseCodes.CLOSED_ABNORMALLY, t.getMessage()));
            }
            catch (IOException e) {
            }
            throw t;
        }
        
    }
    
    @OnClose
    public void onClose(CloseReason reason) {
        LOG.debug("Closing socket session " + (getSession() != null ? getSession().getId() : "(unknown)") + ", Reason: " + reason.toString());
        if (_pageConnection != null) {
            WGACore.INSTANCE.getPageConnectionManager().removeConnection(_httpSession, _pageConnection);
        }
    }
    
    public void close() {
        try {
            getSession().close(new CloseReason(CloseCodes.GOING_AWAY, "Server triggered close of this socket"));
        }
        catch (IOException e) {
            WGACore.INSTANCE.getLog().error("Exception closing Web Socket session " + getSession().getId(), e);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public void firePortletEvent(String eventName, Map<String, Object> params) throws WGException {

        LOG.debug("Sending portlet event to socket session " + getSession().getId());
        
        JsonObject msg = new JsonObject();
        msg.addProperty("type", MSGTYPE_FIREPORTLETEVENT);
        
        JsonObject event = new JsonObject();
        event.addProperty("name", eventName);
        
        TMLScript tmlscript = WGA.get().tmlscript();
        if (tmlscript.isNativeObject(params)) {
            params = tmlscript.descriptify(params, Map.class);
        }
        
        event.add("params", new Gson().toJsonTree(params));
        msg.add("event", event);
        
        doSend(msg, true);
        
    }
    
    @SuppressWarnings("unchecked")
    @Override
    public void sendMessage(Map<String,Object> data) throws WGException {
        LOG.debug("Sending custom message to socket session " + getSession().getId());
        JsonObject msg = new JsonObject();
        msg.addProperty("type", MSGTYPE_CUSTOM);
        
        TMLScript tmlscript = WGA.get().tmlscript();
        if (tmlscript.isNativeObject(data)) {
            data = tmlscript.descriptify(data, Map.class);
        }
        
        msg.add("data", new Gson().toJsonTree(data));
        doSend(msg, true);
    }

    public URI getOrigin() {
        return _origin;
    }
    

    @OnMessage
    public String receiveMessage(String messageStr) {
        return handleClientTextMessage(WGA.get(getSession(), _httpSession), _pageConnection.getDbKey(), messageStr);
    }
    

    @Override
    protected List<ObjectScope> getAllowedGlobalScopes() {
        return WGUtils.list(ObjectScope.WEBSOCKET, ObjectScope.TMLPAGE);
    }

    public PageConnection getPageConnection() {
        return _pageConnection;
    }
    
    
}

