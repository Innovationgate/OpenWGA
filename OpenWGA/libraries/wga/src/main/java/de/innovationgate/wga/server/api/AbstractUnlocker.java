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
 * Abstract base class for WGA internal functionality to access isolated resources on an WGA object
 */
public abstract class AbstractUnlocker {
    
    protected static HttpSession privateFetchSession(WGA wga) {
        return wga.fetchSession();
    }
    
    protected static HttpServletRequest privateFetchRequest(WGA wga) {
        return wga.fetchRequest();
    }
    
    protected static HttpServletResponse privateFetchResponse(WGA wga) {
        return wga.fetchResponse();
    }
    
    protected static WGA privateUnlock(WGA wga) {
        
        if (!wga.isIsolated()) {
            return wga;
        }
        
        final WGAContext originalContext = wga.getContext();
        return WGA.get(new WGAContext() {

            @Override
            public WGACore getCore() {
                return originalContext.getCore();
            }

            @Override
            public TMLContext getTMLContext() {
                return originalContext.getTMLContext();
            }

            @Override
            public HttpServletRequest getServletRequest() {
                return originalContext.getServletRequest();
            }

            @Override
            public HttpServletResponse getServletResponse() {
                return originalContext.getServletResponse();
            }

            @Override
            public HttpSession getHttpSession() {
                return originalContext.getHttpSession();
            }

            @Override
            public Logger getLog() {
                return originalContext.getLog();
            }

            @Override
            public WGAContext narrow() {
                return originalContext.narrow();
            }

            @Override
            public boolean isIsolated() {
                return false;
            }
            
            @Override
            public Session getWebsocketSession() {
                return originalContext.getWebsocketSession();
            }
            
        });
        
        
    }

}
