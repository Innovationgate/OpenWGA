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

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.websocket.Session;

import org.apache.log4j.Logger;

import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Environment context of the {@link WGA} object, providing the available resources 
 */
public interface WGAContext {
    
    public WGACore getCore();
    
    public TMLContext getTMLContext();
    
    public HttpServletRequest getServletRequest();
    
    public HttpServletResponse getServletResponse();
    
    public HttpSession getHttpSession();
    
    public Session getWebsocketSession();
    
    public Logger getLog();
    
    /**
     * Return a narrowed down version of this context that is not dynamic to the environment
     * (in case it were it the first place. Non-dynamic contextes may just return themselves).
     */
    public WGAContext narrow();
    
    /**
     * Returns if the enclosing WGA instance should behave isolated, i.e. do not give direct access
     * to environment-dependent resources like calls, sessions etc.
     */
    public boolean isIsolated();

}
