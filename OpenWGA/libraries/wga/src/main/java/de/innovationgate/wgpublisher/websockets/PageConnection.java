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

import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.UUID;

import javax.servlet.http.HttpSession;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.so.NoopScopeObjectContextCreator;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;


public class PageConnection implements Serializable {
    
    private transient TMLPageWebSocket _webSocket = null;
    
    private String _pageId;
    private String _windowId = null;
    private boolean _clientInited = false;
    private ScopeObjectRegistry _scopeObjectRegistry = null;
    private HttpSession _session;
    private URI _requestURI;
    private String _dbKey;
    
    public String getDbKey() {
        return _dbKey;
    }

    public String getWindowId() {
        return _windowId;
    }

    public String getPageId() {
        return _pageId;
    }

    protected PageConnection(WGA wga) throws WGException {
        try {
            _session = wga.session().getJavaHttpSession();
            _pageId = UUID.randomUUID().toString();
            _requestURI = new URI(wga.call().getOriginalURL());
            _dbKey = wga.call().getMainContext().db().getDbReference();
            _scopeObjectRegistry = new ScopeObjectRegistry(ObjectScope.TMLPAGE, "Page " + _pageId, new NoopScopeObjectContextCreator());
        }
        catch (URISyntaxException e) {
            throw new RuntimeException("Invalid request URI", e);
        }
    }
    
    public boolean isConnected() {
        return _webSocket != null && _webSocket.isConnected();
    }
    
    public void assignSocket(TMLPageWebSocket socket, String windowId) {
        _windowId = windowId;
        _webSocket = socket;
    }

    public boolean isConfimed() {
        return _webSocket != null;
    }

    public boolean isClientInited() {
        return _clientInited;
    }

    public void setClientInited(boolean clientInited) {
        _clientInited = clientInited;
    }



    public ScopeObjectRegistry getScopeObjectRegistry() {
        return _scopeObjectRegistry;
    }

    public TMLPageWebSocket getWebSocket() {
        return _webSocket;
    }

    public HttpSession getSession() {
        return _session;
    }

    public void close() {
        _webSocket = null;        
    }

    public URI getRequestURI() {
        return _requestURI;
    }



}
