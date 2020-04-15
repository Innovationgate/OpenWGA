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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.filter.WGAFilterURLPatternProvider;

public class ResourceAccessFilter implements Filter {

    private static final String URL_PARAMETER_DBKEY = "dbkey";
    public static final String SESSION_ATTRIB_EDITABLE_DATABASES = "de.innovationgate.cm_neo.session.EDITABLE_DATABASES";

    private WGACore _core;
    
    public void destroy() { 
    }

    @SuppressWarnings("unchecked")
	public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {        
        if (req instanceof HttpServletRequest && res instanceof HttpServletResponse) {
            HttpServletRequest httpReq = (HttpServletRequest) req;
            HttpServletResponse httpRes = (HttpServletResponse) res;
            
            String urlDBKey = httpReq.getParameter(URL_PARAMETER_DBKEY);
                        
            if (urlDBKey != null && !urlDBKey.trim().equals("")) {
            	
            	List<String> sessionDBKeys = Collections.emptyList();
                if (httpReq.getSession().getAttribute(SESSION_ATTRIB_EDITABLE_DATABASES) != null) {
                    sessionDBKeys = (List<String>)httpReq.getSession().getAttribute(SESSION_ATTRIB_EDITABLE_DATABASES);
                }
                if (!sessionDBKeys.contains(urlDBKey.trim())) {
                     _core.getLog().warn("URL '" + httpReq.getRequestURI() + "' has been requested with an invalid database key. (Requested dbkey: '" + urlDBKey + "' != CM dbkeys: '" + sessionDBKeys + "')");
                     httpRes.sendError(HttpURLConnection.HTTP_FORBIDDEN, "You have no valid content management session to access this resource.");
                     return;
                }
            }
            
        }
        chain.doFilter(req, res);                      
    }

    public void init(FilterConfig config) throws ServletException {
        _core = WGACore.retrieve(config.getServletContext());        
    }

}
