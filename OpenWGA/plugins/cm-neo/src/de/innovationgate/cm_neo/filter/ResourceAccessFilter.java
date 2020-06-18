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

package de.innovationgate.cm_neo.filter;

import java.io.IOException;
import java.net.HttpURLConnection;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import de.innovationgate.wgpublisher.WGACore;

public class ResourceAccessFilter implements Filter {

    private Logger log = Logger.getLogger("wga.cm-neo");
    
    public void init(FilterConfig config) throws ServletException {
    }

    public void destroy() { 
    }

	public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {        
        if (req instanceof HttpServletRequest && res instanceof HttpServletResponse) {

        	HttpServletRequest httpReq = (HttpServletRequest) req;
            HttpServletResponse httpRes = (HttpServletResponse) res;
            
            Object cmSession = httpReq.getSession().getAttribute(WGACore.ATTRIB_BROWSERINTERFACE);
            if(cmSession==null || (boolean)cmSession!=true){
                log.warn("URL '" + httpReq.getRequestURI() + "' has been requested outside CM-Session");
                httpRes.sendError(HttpURLConnection.HTTP_FORBIDDEN, "You have no valid content management session to access this resource.");
                return;
            }
            
        }
        chain.doFilter(req, res);                      
    }

}
