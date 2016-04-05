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
package de.innovationgate.wgpublisher;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpUtils;
import javax.servlet.http.Part;

public class RenderServletRequest implements HttpServletRequest {
    
    private HttpServletRequest _request;
    private String _path;
    private String _query;
    
    private Hashtable _attributes = new Hashtable();
    private Hashtable _parameters = new Hashtable();
    private String _characterEncoding;
    

    public RenderServletRequest(HttpServletRequest request, String path) {
        _request = request;  
        
        _characterEncoding = request.getCharacterEncoding();
        
        // parse path
        if (path.indexOf("?") > 0) {
            _query = path.substring(path.indexOf("?")+1);
            _path = path.substring(0, path.indexOf("?"));
            // parse query string
            _parameters = HttpUtils.parseQueryString(_query);
        } else {
            _path = path;
            _query = null;
        }
               
        // reserve spec attributes (java.*, javax.*, sun.*, com.sun.*) and for websphere (com.ibm.*) 
        Enumeration names = _request.getAttributeNames();
        while (names.hasMoreElements()) {
            String attribName = (String) names.nextElement();
            if (attribName.startsWith("java.") || attribName.startsWith("javax.") || 
                attribName.startsWith("sun.") || attribName.startsWith("com.sun.") || 
                attribName.startsWith("com.ibm.")) {
                setAttribute(attribName, _request.getAttribute(attribName));
            }
        }
    }

    public String getAuthType() {
        return _request.getAuthType();
    }

    public Cookie[] getCookies() {
        return _request.getCookies();
    }

    public long getDateHeader(String arg0) {
        return _request.getDateHeader(arg0);
    }

    public String getHeader(String arg0) {
        return _request.getHeader(arg0);
    }

    public Enumeration getHeaders(String arg0) {
        return _request.getHeaders(arg0);
    }

    public Enumeration getHeaderNames() {
        return _request.getHeaderNames();
    }

    public int getIntHeader(String arg0) {
        return _request.getIntHeader(arg0);
    }

    public String getMethod() {
        // always GET in this case
        return "GET";
    }

    public String getPathInfo() {
        // WGADispatcher is the default servlet mapped with '/'
        // PathInfo is therefore null @see ServletSpecs 2.3 - 11.2
        return null;
    }

    public String getPathTranslated() {
        // @see getPathInfo()
        return null;
    }

    public String getContextPath() {
        return _request.getContextPath();
    }

    public String getQueryString() {
        return _query;
    }

    public String getRemoteUser() {
        return _request.getRemoteUser();
    }

    public boolean isUserInRole(String arg0) {
        return _request.isUserInRole(arg0);
    }

    public Principal getUserPrincipal() {
        return _request.getUserPrincipal();
    }

    public String getRequestedSessionId() {
        return _request.getRequestedSessionId();
    }
    public String getRequestURI() {
        String uri = getContextPath() + getServletPath();
        if (getPathInfo() != null) {
            uri += getPathInfo();
        }
        return uri;        
    }

    public StringBuffer getRequestURL() {
        StringBuffer url = new StringBuffer();
        url.append( _request.getScheme() + "://");
        url.append(_request.getServerName());
        url.append(":" + _request.getServerPort());
        url.append(getRequestURI());
        return url;
    }
    /*
    public String getRequestURI() {
        return _request.getContextPath() + _path;        
    }

    public StringBuffer getRequestURL() {
        StringBuffer url = new StringBuffer();
        url.append( _request.getProtocol() + "://");
        url.append(_request.getServerName());
        url.append(":" + _request.getServerPort());
        url.append(_request.getContextPath());
        url.append(_path);
        return url;
    }*/

    public String getServletPath() {
        return _path;
    }

    public HttpSession getSession(boolean arg0) {
        return _request.getSession(arg0);
    }

    public HttpSession getSession() {
        return _request.getSession();
    }

    public boolean isRequestedSessionIdValid() {
        return _request.isRequestedSessionIdValid();
    }

    public boolean isRequestedSessionIdFromCookie() {
        return _request.isRequestedSessionIdFromCookie();
    }

    public boolean isRequestedSessionIdFromURL() {
        return _request.isRequestedSessionIdFromURL();
    }

    /**
     * @deprecated
     */
    public boolean isRequestedSessionIdFromUrl() {
        return _request.isRequestedSessionIdFromUrl();
    }

    public Object getAttribute(String arg0) {
        return _attributes.get(arg0);
    }

    public Enumeration getAttributeNames() {
        return _attributes.keys();
    }

    public String getCharacterEncoding() {
        return _characterEncoding;
    }

    public void setCharacterEncoding(String arg0) throws UnsupportedEncodingException {
        _characterEncoding = arg0;        
    }

    public int getContentLength() {
        // no body in GET request - return UNKNOWN
        return -1;
    }

    public String getContentType() {
        // no body in GET request - return null
        return null;
    }

    public ServletInputStream getInputStream() throws IOException {
        // no body in GET request - return empty stream
        return new ServletInputStream() {

            public int read() throws IOException {
                return 0;
            }
            
        };       
    }

    public String getParameter(String arg0) {
        Object value = _parameters.get(arg0);
        if (value != null) {
            if (value instanceof Object[]) {
                return (String) ((Object[])value)[0];
            } else if (value instanceof String) {
                return (String) value;
            } else {
                return null;
            }
        }
        return null;
    }

    public Enumeration getParameterNames() {
        return _parameters.keys();
    }

    public String[] getParameterValues(String arg0) {
        Object value = _parameters.get(arg0);
        if (value != null) {
            if (value instanceof Object[]) {
                Object[] array = (Object[]) value;
                String[] stringArray = new String[array.length];
                for (int i = 0; i < array.length; i++) {
                    stringArray[i] = (String) array[i];
                }
                return stringArray;
            } else if (value instanceof String) {
                String[] stringArray = new String[1];
                stringArray[0] = (String) value;
                return stringArray;
            } else {
                return null;
            }
        }        
        return null;
    }

    public Map getParameterMap() {
        HashMap map = new HashMap();
        Iterator it = _parameters.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry) it.next();
            map.put(entry.getKey(), entry.getValue());
        }
        return map;
    }

    public String getProtocol() {
        // always the same as parent request - no absolute urls allowed
        return _request.getProtocol();
    }

    public String getScheme() {
        // always the same as parent request - no absolute urls allowed
        return _request.getScheme();
    }

    public String getServerName() {
        // always the same as parent request - no absolute urls allowed
        return _request.getServerName();
    }

    public int getServerPort() {
        // always the same as parent request - no absolute urls allowed
        return _request.getServerPort();
    }

    public BufferedReader getReader() throws IOException {
        // only GET requests - return reader from empty input stream
        return new BufferedReader(new InputStreamReader(getInputStream()));
    }

    public String getRemoteAddr() { 
        return _request.getRemoteAddr();
    }

    public String getRemoteHost() {
        return _request.getRemoteHost();
    }

    public void setAttribute(String arg0, Object arg1) {
        if (arg0 != null) {
            if (arg1 != null) {
                _attributes.put(arg0, arg1);
            } else {
                _attributes.remove(arg0);
            }
        }
    }

    public void removeAttribute(String arg0) {
        if (arg0 != null) {
            _attributes.remove(arg0);
        }
    }

    public Locale getLocale() {
        return _request.getLocale();
    }

    public Enumeration getLocales() {
        return _request.getLocales();
    }

    public boolean isSecure() {
        // always the same as parent request - no absolute urls allowed
        return _request.isSecure();
    }

    public RequestDispatcher getRequestDispatcher(String arg0) {
        return _request.getRequestDispatcher(arg0);
    }

    /**
     * @deprecated
     */
    public String getRealPath(String arg0) {
        return _request.getRealPath(arg0);
    }

    public String getLocalAddr() {
        return _request.getLocalAddr();
    }

    public String getLocalName() {
        return _request.getLocalName();
    }

    public int getLocalPort() {
        return _request.getLocalPort();
    }

    public int getRemotePort() {
        return _request.getRemotePort();
    }

    public boolean authenticate(HttpServletResponse arg0) throws IOException, ServletException {
        return _request.authenticate(arg0);
    }

    public AsyncContext getAsyncContext() {
        return _request.getAsyncContext();
    }

    public DispatcherType getDispatcherType() {
        return _request.getDispatcherType();
    }

    public Part getPart(String arg0) throws IOException, IllegalStateException, ServletException {
        return _request.getPart(arg0);
    }

    public Collection<Part> getParts() throws IOException, IllegalStateException, ServletException {
        return _request.getParts();
    }

    public ServletContext getServletContext() {
        return _request.getServletContext();
    }

    public boolean isAsyncStarted() {
        return _request.isAsyncStarted();
    }

    public boolean isAsyncSupported() {
        return _request.isAsyncSupported();
    }

    public void login(String arg0, String arg1) throws ServletException {
        _request.login(arg0, arg1);
    }

    public void logout() throws ServletException {
        _request.logout();
    }

    public AsyncContext startAsync() {
        return _request.startAsync();
    }

    public AsyncContext startAsync(ServletRequest arg0, ServletResponse arg1) {
        return _request.startAsync(arg0, arg1);
    }


}
