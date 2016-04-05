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

package de.innovationgate.wgpublisher.services;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;

import org.apache.commons.collections.iterators.IteratorEnumeration;

public class WGAServiceServletConfig implements ServletConfig {
    
    private Map<String,String> _initParams = new HashMap<String,String>();
    private ServletContext _servletContext;
    private String _name;
    
    public WGAServiceServletConfig(ServletContext cx, String name) {
        _servletContext = cx;
        _name = name;
    }

    public String getInitParameter(String arg0) {
        return _initParams.get(arg0);
    }

    public Enumeration getInitParameterNames() {
        return new IteratorEnumeration(_initParams.keySet().iterator());
    }

    public ServletContext getServletContext() {
        return _servletContext;
    }

    public String getServletName() {
        return _name;
    }
    
    public void setInitParameter(String key, String value) {
        _initParams.put(key, value);
    }
    
    public void removeInitParameter(String key) {
        _initParams.remove(key);
    }

}
