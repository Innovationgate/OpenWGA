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

package de.innovationgate.wgpublisher.expressions.tmlscript.wgaglobal;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.websocket.Session;

import org.apache.log4j.Logger;

import de.innovationgate.wga.server.api.WGAContext;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Legacy WGAContext used for WGA global in TMLScript
 */
public class TMLScriptWGAContext implements WGAContext {

    private TMLContext _cx;
    private boolean _isolated;
    private Session _wsSession;

    public TMLScriptWGAContext(TMLContext cx, Session wsSession, boolean isolated) {
        _cx = cx;
        _wsSession = wsSession;
        _isolated = isolated;
    }

    public WGACore getCore() {
        return _cx.getwgacore();
    }

    public TMLContext getTMLContext() {
        return _cx;
    }

    public HttpServletRequest getServletRequest() {
        return _cx.getrequest();
    }

    public HttpServletResponse getServletResponse() {
        return _cx.getresponse();
    }
    
    @Override
    public HttpSession getHttpSession() {
        return _cx.gethttpsession();
    }
    
    @Override
    public Logger getLog() {
        return _cx.getlog();
    }
    
    @Override
    public WGAContext narrow() {
        return this;
    }

    @Override
    public boolean isIsolated() {
        return _isolated;
    }

    @Override
    public Session getWebsocketSession() {
        return _wsSession;
    }

}
