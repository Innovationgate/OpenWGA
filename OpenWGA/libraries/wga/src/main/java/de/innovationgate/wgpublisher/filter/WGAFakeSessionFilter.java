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

package de.innovationgate.wgpublisher.filter;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionContext;

import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.sessions.WGAHttpSessionListener;

/**
 * filter implementation to remove jsession for all BotAgent requests
 */
public class WGAFakeSessionFilter implements Filter, WGAFilterURLPatternProvider {
    
    private static class RequestWrapper extends HttpServletRequestWrapper {

        private ServletContext _context;
        private HttpSession _session = null;
        
        private static class SearchEngineFakeSession implements HttpSession {
            
            private Map<String,Object> _attributes = new HashMap<String, Object>();
            private ServletContext _context = null;
            private int _maxInactiveInterval = 60 * 1000 * 60;
            private long _creationTime;
            private String _id;
            
            
            public SearchEngineFakeSession(String id, ServletContext context) {
                _id = id;
                _context = context;
                _creationTime = System.currentTimeMillis();
                
            }

            public Object getAttribute(String key) {                
                return _attributes.get(key);
            }

            public Enumeration getAttributeNames() {
                return Collections.enumeration(_attributes.keySet());
            }

            public long getCreationTime() {
                return _creationTime;
            }

            public String getId() {
                return _id;
            }

            public long getLastAccessedTime() {
                return System.currentTimeMillis();
            }

            public int getMaxInactiveInterval() {
                return _maxInactiveInterval ;
            }

            public ServletContext getServletContext() {
                return _context;
            }

            public HttpSessionContext getSessionContext() {
                return null;
            }

            public Object getValue(String key) {
                return getAttribute(key);
            }

            public String[] getValueNames() {
                return _attributes.keySet().toArray(new String[0]);
            }

            public void invalidate() {                
            }

            public boolean isNew() {
                return true;
            }

            public void putValue(String key, Object value) {
                setAttribute(key, value);
            }

            public void removeAttribute(String key) {
                _attributes.remove(key);
            }

            public void removeValue(String key) {
                _attributes.put(key, null);                
            }

            public void setAttribute(String key, Object value) {
                _attributes.put(key, value);
            }

            public void setMaxInactiveInterval(int interval) {
                _maxInactiveInterval = interval;
            }
            
        }
        
        public RequestWrapper(HttpServletRequest request, ServletContext context) {
            super(request);
            _context = context;
        }

        @Override
        public String getRequestedSessionId() {
            String agent = getHeader("User-Agent");
            if (agent == null) {
                agent = "Unknown";
            }
            String mappingKey = agent + getRemoteAddr();
            return mappingKey;
        }

        @Override
        public HttpSession getSession() {
            return getSession(true);
        }

        @Override
        public HttpSession getSession(boolean create) {     
            if (create && _session == null) {
                _session = new SearchEngineFakeSession(getRequestedSessionId(), _context);
                WGAHttpSessionListener.initializeSessionAttributes(_session);
            }
            return _session;
        }
    }
    
    
    
    private static class ResponseWrapper extends HttpServletResponseWrapper {
        public ResponseWrapper(HttpServletResponse response) {
            super(response);
        }

        @Override
        public String encodeRedirectUrl(String url) {
               return url;
        }

        @Override
        public String encodeRedirectURL(String url) {
            return url;
        }

        @Override
        public String encodeUrl(String url) {
            return url;
        }

        @Override
        public String encodeURL(String url) {
            return url;
        }
        
        @Override
        public void sendRedirect(String location) throws IOException {
            // perform redirect via 301
            ((HttpServletResponse)getResponse()).setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
            ((HttpServletResponse)getResponse()).setHeader("Location", location);
            setStatus(SC_MOVED_PERMANENTLY);
        }
    }

    private static final List<String> WHITE_LIST = new ArrayList<String>();
    static {
        WHITE_LIST.add("*");
    }

    private ServletContext _context;
    private WGACore _core;

    public void destroy() {
    }

    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain chain) throws IOException, ServletException {
        
        HttpServletRequest request = (HttpServletRequest) servletRequest;
        HttpServletResponse response = (HttpServletResponse) servletResponse;

        // wrap all requests that match the OpenWGA personalisation agent exclusions
        // remove sessionid from url and return new fake session on each request
        String agent = request.getHeader("User-Agent");
        if (agent != null && matchAgentExclusion(agent)) {
            request = new RequestWrapper(request, _context);
            response = new ResponseWrapper(response);               
        }
        
        chain.doFilter(request, response);
    }
    
    private boolean matchAgentExclusion(String agent) {
        if (_core != null && _core.getWgaConfiguration() != null && _core.getWgaConfiguration().getPersonalisationConfiguration() != null && _core.getWgaConfiguration().getPersonalisationConfiguration().getPersonalisationAgentExclusions() != null) {
           Iterator<String> exclusions =  _core.getWgaConfiguration().getPersonalisationConfiguration().getPersonalisationAgentExclusions().iterator();
           while (exclusions.hasNext()) {
               String exclusion = exclusions.next();
               try {
                   if (Pattern.matches(exclusion, agent)) {
                       return true;
                   }
               }
               catch (Exception e) {
                   _core.getLog().error("Invalid agent exclusion: '" + exclusion + "'.", e);
               }
           }
        }       
        return false;
    }

    public void init(FilterConfig config) throws ServletException {
        _context = config.getServletContext();
        _core = WGACore.retrieve(_context);
    }

    public List<String> getBlackListURLPatterns() {
        return Collections.emptyList();
    }

    public List<String> getWhiteListURLPatterns() {
        return WHITE_LIST;
    }

}
