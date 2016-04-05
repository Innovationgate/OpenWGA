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
import java.util.List;
import java.util.Map;

import javax.websocket.CloseReason;
import javax.websocket.CloseReason.CloseCodes;
import javax.websocket.EndpointConfig;
import javax.websocket.HandshakeResponse;
import javax.websocket.OnClose;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.HandshakeRequest;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import javax.websocket.server.ServerEndpointConfig;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.WebSocket;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptAppGlobalRegistry;

@ServerEndpoint(value=IndependentWebSocket.PATH,configurator=IndependentWebSocket.Configurator.class)
public class IndependentWebSocket extends AbstractWebSocket implements WebSocket {
    
    public static final String PATH = "/services/websocket/independent/{dbkey}";
    
    public static final String PROP_DBKEY = "DBKey";
    public static final String PROP_AUTH = "Authentication";
    public static final String PROP_SOCKET = "WebSocket";

    private static final String MSGTYPE_FIREPORTLETEVENT = "firePortletEvent";
    private static final String MSGTYPE_CUSTOM = "custom";
    private static final String MSGTYPE_HANDSHAKE = "handshake";
    public static class Configurator extends ServerEndpointConfig.Configurator {
        
        @Override
        public void modifyHandshake(ServerEndpointConfig sec, HandshakeRequest request, HandshakeResponse response) {
            try {
                                
                super.modifyHandshake(sec, request, response);
                if (request.getHeaders().containsKey("Authorization")) {
                    DBLoginInfo loginInfo = DBLoginInfo.createFromHttpCredentials(request.getHeaders().get("Authorization").get(0));
                    sec.getUserProperties().put(PROP_AUTH, loginInfo);
                }
            }
            catch (Throwable t) {
                WGACore.INSTANCE.getLog().error("Exception doing web socket handshake", t);
            }
        }
        
    }
    
    @OnOpen
    public void open(Session session, EndpointConfig cfg, @PathParam("dbkey") String dbKey) throws WGException {
        
        initSession(session);
        
        if ((Boolean) WGACore.INSTANCE.getServicesServerOptionReader().readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_WEBSOCKETS) == false) {
            throw new InvalidWebSocketSessionException("Web Sockets are disabled");
        }
        
        DBLoginInfo dbLoginInfo = (DBLoginInfo) cfg.getUserProperties().get(PROP_AUTH);       
        try {
            WGDatabase db = WGA.get().db(dbKey, false);  
            if (db == null) {
                throw new InvalidWebSocketSessionException("No database of key " + dbKey);
            }
            
            int access;
            if (dbLoginInfo != null) {
                access = db.openSession(dbLoginInfo.getUserName(), dbLoginInfo.getPassword());
            }
            else {
                access = db.openAnonymousSession();
            }
            
            if (access <= WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
                throw new InvalidWebSocketSessionException("Invalid login");
            }
            else if (access <= WGDatabase.ACCESSLEVEL_NOACCESS) {
                throw new InvalidWebSocketSessionException("No access to database " + dbKey);
            }
            
            TMLScriptAppGlobalRegistry globalRegistry = WGACore.INSTANCE.getTmlscriptGlobalRegistry().getAppGlobalRegistry(db);
            if (!globalRegistry.getUsedManagedGlobalScopes().contains(ObjectScope.WEBSOCKET)) {
                throw new InvalidWebSocketSessionException("The app '" + dbKey + "' has no globals of WEBSOCKET scope");
            }
            
            LOG.debug("Opening independent socket session " + getSession().getId() + " to database " + dbKey + " for user " + db.getSessionContext().getUser());
            WGACore.INSTANCE.getIndependentWebSocketManager().register(this);
            Map<String, Object> userProperties = session.getUserProperties();
            userProperties.put(PROP_SOCKET, this);
            if (dbLoginInfo != null) {
                userProperties.put(PROP_AUTH, dbLoginInfo);
            }
            userProperties.put(PROP_DBKEY, dbKey);
            
            JsonObject msg = new JsonObject();
            msg.addProperty("type", MSGTYPE_HANDSHAKE);
            msg.addProperty("userName", db.getSessionContext().getUserAccess().getPrimaryName());
            msg.addProperty("accessLevel", db.getSessionContext().getAccessLevel());
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
        finally {
            WGFactory.getInstance().closeSessions();
        }
    }

    @OnClose
    public void onClose(CloseReason reason) {
        WGACore.INSTANCE.getIndependentWebSocketManager().unregister(this);
        LOG.debug("Closing socket session " + (getSession() != null ? getSession().getId() : "(unknown)") + ", Reason: " + reason.toString());
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
    

    @OnMessage
    public String receiveMessage(@PathParam("dbkey") String dbKey, String messageStr) {
        return handleClientTextMessage(WGA.get(getSession()), dbKey, messageStr);
    }

    @Override
    protected List<ObjectScope> getAllowedGlobalScopes() {
        return WGUtils.list(ObjectScope.WEBSOCKET);
    }
    
    
}

