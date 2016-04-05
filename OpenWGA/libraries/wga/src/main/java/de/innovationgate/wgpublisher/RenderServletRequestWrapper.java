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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpUtils;

public class RenderServletRequestWrapper extends HttpServletRequestWrapper {
    
    private HttpServletRequest _request;
    private String _path;
    private String _query;
    
    private Hashtable _attributes = new Hashtable();
    private Hashtable _parameters = new Hashtable();
    private String _characterEncoding;
    
    public RenderServletRequestWrapper(HttpServletRequest request, String path) {
         super(request);
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

    public String getServletPath() {
        return _path;
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

    public BufferedReader getReader() throws IOException {
        // only GET requests - return reader from empty input stream
        return new BufferedReader(new InputStreamReader(getInputStream()));
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
}
