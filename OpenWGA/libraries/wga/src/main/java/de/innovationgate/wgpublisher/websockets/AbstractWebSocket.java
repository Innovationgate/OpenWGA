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
import java.net.SocketException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.websocket.CloseReason;
import javax.websocket.OnError;
import javax.websocket.Session;

import org.apache.log4j.Logger;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.server.api.CallMethodConfig;
import de.innovationgate.wga.server.api.DescriptificationConfig;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.events.EventManager;
import de.innovationgate.wgpublisher.expressions.InvalidDirectAccessCallException;
import de.innovationgate.wgpublisher.expressions.tmlscript.ManagedTMLScriptGlobalDefinition;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.so.NoopScopeObjectContextCreator;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public abstract class AbstractWebSocket {

    public static final Logger LOG = Logger.getLogger("wga.websocket");
    public static final String SESSIONPROP_SOCKET = ("socket");
    private Session _session;
    private ScopeObjectRegistry _scopeObjectRegistry;

    private Queue<JsonObject> _bufferedMessages = new ConcurrentLinkedQueue<>();
    private ScheduledExecutorService _bufferedMessagesSendingService = Executors.newSingleThreadScheduledExecutor();
    
    public AbstractWebSocket() {
        super();
    }

    public static final AbstractWebSocket getFromSession(Session session) {
        return (AbstractWebSocket) session.getUserProperties().get(AbstractWebSocket.SESSIONPROP_SOCKET);
    }
    
    public Session getSession() {
        return _session;
    }

    public void initSession(Session session) {
        _session = session;
        _session.getUserProperties().put(SESSIONPROP_SOCKET, this);
        _scopeObjectRegistry = new ScopeObjectRegistry(ObjectScope.WEBSOCKET, "WebSocket session " + session.getId(), new NoopScopeObjectContextCreator());
    }

    protected JsonObject handleClientTextMessage(WGA wga, String dbKey, String messageStr) {
        String callId = null;
        try {
        
            JsonParser parser = new JsonParser();
            JsonObject msg = (JsonObject) parser.parse(messageStr);
            String type = msg.get("type").getAsString();
            callId = msg.has("callId") ? msg.get("callId").getAsString() : null;    
            if (type.equals("greetings")) {
                WGACore.INSTANCE.getLog().info("A socket sends its greetings: " + msg);
                return null;
            }
            else if (type.equals("callGlobal")) {
                return callGlobal(wga, callId, dbKey, msg);
            }
            else {
                return buildErrorResponse(callId, "Unknown message type: " + type);
            }
        
        }
        catch (Throwable t) {
            LOG.error("Exception handling client text message", t);
            return buildErrorResponse(callId, "Exception handling message", t);
        }
    }

    protected JsonObject buildErrorResponse(String callId, String errorMsg) {
        return buildErrorResponse(callId, errorMsg, null);
    }

    protected JsonObject buildErrorResponse(String callId, String errorMsg, Throwable t) {
        JsonObject rv = buildResponse(callId, false);
        rv.addProperty("message", errorMsg);
        if (t != null) {
            JsonArray causes = new JsonArray();
            rv.add("causes", causes);
            for (String cause : WGUtils.extractMessages(t)) {
                causes.add(cause);
            }
        }
        
        return rv;
    }

    protected JsonObject buildResponse(String callId, boolean success) {
        JsonObject rv = new JsonObject();
        rv.addProperty("type", "response");
        if (callId != null) {
            rv.addProperty("callId", callId);
        }
        if (success) {
            rv.addProperty("status", "SUCCESS");
        }
        else {
            rv.addProperty("status", "ERROR");
        }
        return rv;
    }
    
    /**
     * List of the scopes for managed globals that may be called via this websocket connection
     */
    protected abstract List<ObjectScope> getAllowedGlobalScopes();

    private JsonObject callGlobal(WGA wga, String callId, String dbKey, JsonObject msg) throws WGException {
    
        try {
            String globalName = msg.get("global").getAsString();
            String method = msg.get("method").getAsString();
            JsonObject params = msg.has("params") ? msg.get("params").getAsJsonObject() : null;
            
            if(globalName.contains(":")){
            	String[]parts = globalName.split(":");
            	dbKey = parts[0];
            	globalName = parts[1];
            }
            
            WGDatabase db = wga.db(dbKey);
            if (db==null){
            	return buildErrorResponse(callId, "Unable to find database: " + dbKey);
            }
            if(!db.isSessionOpen()) {
                return buildErrorResponse(callId, "You have no access to this database: " + dbKey);
            }
            
            TMLScriptGlobal global = WGACore.INSTANCE.getTmlscriptGlobalRegistry().getGlobal(globalName, db);
            if (global == null) {
                return buildErrorResponse(callId, "Unknown global for database '" + dbKey + "': " + globalName);
            }
            Object globalObj = global.getRef();
            if (!(globalObj instanceof ManagedTMLScriptGlobalDefinition)) {
                return buildErrorResponse(callId, "The global for database '" + dbKey + "' is no managed global: " + globalName);
            }
            
            ManagedTMLScriptGlobalDefinition managedGlobal = (ManagedTMLScriptGlobalDefinition) globalObj;
            if (!getAllowedGlobalScopes().contains(managedGlobal.getConfig().getScope())) {
                return buildErrorResponse(callId, "The global for database '" + dbKey + "' is no managed global of an allowed scope: " + globalName);
            }
            
            ScopeObject obj = managedGlobal.provideGlobal(wga, managedGlobal.getConfig().getScope().resolve(wga, managedGlobal.getDesignReference()), true);
            
            Map<String,Object> namedParams = new HashMap<String,Object>();
            namedParams.put("$websocket", this);
            if (params != null) {
                for (Map.Entry<String,JsonElement> entry : params.entrySet()) {
                    namedParams.put(entry.getKey(), wga.tmlscript().scriptify(entry.getValue(), obj.getObject()));
                }
            }
            
            EventManager.LOG.debug("Websocket session " + _session.getId() + " calling managed global '" + globalName + "' (" + System.identityHashCode(obj.getObject()) + ") method '" + method + "'");
            
            obj.beforeUsage();
            try {
            	
            	// ensure correct app context for script execution:
            	wga = WGA.get(wga.createTMLContext(db));
            	
                Object result = wga.tmlscript().callMethod(obj.getObject(), method, namedParams, null, new CallMethodConfig().setDirectAccess(true));
                if (result != null) {
                    result = wga.tmlscript().descriptify(result, Object.class, new DescriptificationConfig().convertObjectsToJSON().setForceDescriptification(true));
                    JsonObject rv = buildResponse(callId, true);
                    if (result instanceof JsonElement) {
                        rv.add("data", (JsonElement) result);
                    }
                    else {
                        rv.add("data", new Gson().toJsonTree(result));
                    }
                    return rv;
                }
                return null;
            }
            catch (InvalidDirectAccessCallException e) {
                return buildErrorResponse(callId, e.getMessage());
            }
            finally {
                obj.afterUsage();
            }
        }
        finally {
            WGFactory.getInstance().closeSessions();
        }
        
    }

    public boolean isConnected() {
        return _session.isOpen();
    }

    @OnError
    public void error(Session session, Throwable t) throws IOException {
        
        // Filter out errors that (at current servlet/websocket specs) may happen in perfectly normal workflow
        // like tabs that are closed or pages that the user navigates away from. Unfortunately there is no really
        // good way to detect them (#00004562)
        
        
        if (t instanceof SocketException || (t instanceof IOException && WGUtils.getRootCause(t) instanceof SocketException)) {
            LOG.debug("IO exception on Web Socket session " + session.getId(), t);
        }
        else if (t instanceof InvalidWebSocketSessionException) {
            LOG.error("Illegal attempt connecting Web Socket session " + session.getId() + ": " + t.getMessage() + ". Closing session.");
            if (session.isOpen()) {
                session.close(new CloseReason(CloseReason.CloseCodes.VIOLATED_POLICY, "The HTTP session belonging to the page is gone."));
            }
        }
        else {
            LOG.error("Exception on Web Socket session " + session.getId(), t);
        }
                
    }

    public synchronized void doSend(JsonObject msgObj, boolean async) {
    
        try {
            if (!_session.isOpen()) {
                return;
            }
            
            if (msgObj instanceof JsonObject) {
                JsonObject msg = (JsonObject) msgObj;
                LOG.debug("Sending JSON message on socket session " + _session.getId() + ": " + msgObj.toString());
                if (async) {
                    _session.getAsyncRemote().sendText(msg.toString());
                }
                else {
                    _session.getBasicRemote().sendText(msg.toString());   
                }
            }
            else {
                LOG.error("Invalid message type for websocket: " + msgObj.getClass().getName());
            }
        }
        catch (IllegalStateException e) {
            LOG.debug("Failed sending, buffering JSON message on socket session " + _session.getId() + ": " + msgObj.toString());
            _bufferedMessages.add(msgObj);
            _bufferedMessagesSendingService.schedule(new Runnable() {
                @Override
                public void run() {
                    sendBufferedMessages();
                }              
            }, 2, TimeUnit.SECONDS);
        }
        catch (IOException e) {
            LOG.error("Exception sending websocket message", e);
        }
        
    }

    private void sendBufferedMessages() {
        
        while (true) {
        	JsonObject msg = _bufferedMessages.poll();
            if (msg != null) {
                doSend(msg, false);
                Thread.yield();
            }
            else {
                break;
            }
        }
        
    }

    public ScopeObjectRegistry getScopeObjectRegistry() {
        return _scopeObjectRegistry;
    }

}