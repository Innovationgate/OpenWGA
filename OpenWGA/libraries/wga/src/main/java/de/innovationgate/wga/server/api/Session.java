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

package de.innovationgate.wga.server.api;

import javax.servlet.http.HttpSession;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACore.SessionLoginMap;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.websockets.IndependentWebSocket;

/**
 * Information about the end-user session for which the current environment runs.
 * This may be a HTTP session or a WebSocket session.
 * If no end-user session is available on the environment no data is returned.
 */
public class Session {
    
    /**
     * The requested application is in no authoring mode on the session
     */
    private int AUTHORINGMODE_NONE = 0;
    
    
    /**
     * The requested application is in local authoring mode on the session
     */
    private int AUTHORINGMODE_APP = 10;
    
    
    /**
     * The user session is in content manager mode 
     */
    private int AUTHORINGMODE_CONTENTMANAGER = 20;
    
    private WGA _wga;
    
    protected Session(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Returns the JavaEE HTTP session object. Only available if
     * the session is a HTTP session.
     * @throws WGException
     */
    public HttpSession getJavaHttpSession() throws WGException {
        return _wga.getHttpSession();
    }
    
    /**
     * Returns the JavaEE WebSocket session object. Only available if
     * the session is a WebSocket session.
     * @throws WGException
     */
    public javax.websocket.Session getJavaWebSocketSession() throws WGException {
        return _wga.getWebSocketSession();
    }
    
    /**
     * Returns the current authoring mode regarding the given database. 
     * @param db The database
     * @return The authoring mode as constant AUTHORINGMODE_...
     * @throws WGException
     */
    public int getAuthoringMode(Database db) throws WGException {
        return getAuthoringMode(db.getDbKey());
    }
    
    /**
     * Returns the current authoring mode regarding the given database. 
     * @param db The database
     * @return The authoring mode as constant AUTHORINGMODE_...
     * @throws WGException
     */
    public int getAuthoringMode(String dbKey) throws WGException {
        
        if (_wga.isHttpSessionAvailable()) {
            try {
                if (WGPDispatcher.isBrowserInterface(getJavaHttpSession())) {
                    return AUTHORINGMODE_CONTENTMANAGER;
                }
                
                if (WGPDispatcher.isAuthoringMode(dbKey, getJavaHttpSession())) {
                    return AUTHORINGMODE_APP;
                }
                
            }
            catch (UnavailableResourceException e) {
            }
        }
        
        return AUTHORINGMODE_NONE;
        
    }
    
    /**
     * Returns if session data is available
     */
    public boolean isAvailable() {
        return _wga.isHttpSessionAvailable() || _wga.isWebSocketSessionAvailable();
    }
    
    /**
     * Returns if the session was just created and the client does not yet know about the session ID
     * This is only informative if the session type is HTTP, returns true on WebSocket sessions.
     * @throws WGException
     */
    public boolean isNew() throws WGException {
        try {
            if (_wga.isHttpSessionAvailable()) {
                return getJavaHttpSession().isNew();
            }
            else {
                return true;
            }
        }
        catch (UnavailableResourceException e) {
            return true;
        }
    }
    
    protected SessionLoginMap getLogins() throws WGException {

        if (_wga.isHttpSessionAvailable()) {
            HttpSession session = _wga.fetchSession();
            return WGACore.getSessionLogins(session);
        }
        
        if (_wga.isWebSocketSessionAvailable()) {
            javax.websocket.Session wsSession = _wga.fetchWebSocketSession();
            DBLoginInfo loginInfo = (DBLoginInfo) wsSession.getUserProperties().get(IndependentWebSocket.PROP_AUTH);
            String dbKey = (String) wsSession.getUserProperties().get(IndependentWebSocket.PROP_DBKEY);
            if (dbKey != null && loginInfo != null) {
                WGDatabase db = _wga.db(dbKey, false);
                SessionLoginMap logins = new SessionLoginMap();
                logins.put(_wga.getCore().getDomainForDatabase(db).getName(), loginInfo);
                return logins;
            }
        }
        
        return new SessionLoginMap();
        
    }
    
    public boolean isWebSocketSessionAvailable() {
        return _wga.isWebSocketSessionAvailable();
    }
    
    public boolean isHttpSessionAvailable() {
        return _wga.isHttpSessionAvailable();
    }
    
    /**
     * Returns the id of the HTTP session, if available, else null
     */
    public String getId() throws WGException {
        if (isHttpSessionAvailable()) {
            return getJavaHttpSession().getId();
        }
        else {
            return null;
        }
    }
    
    

}
