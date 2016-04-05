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

package de.innovationgate.wgpublisher.webtml.utils;

import java.io.Serializable;

public class TMLOption implements Serializable {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final String SCOPE_LOCAL = "local";
    public static final String SCOPE_GLOBAL = "global";
    public static final String SCOPE_PORTLET = "portlet";
    
    private String _name;
    private Object _value;
    private String _scope;
    
    public TMLOption(String name, Object value, String scope) {
        _name = name;
        _value = value;
        _scope = scope;
        
        if (_scope == null) {
            _scope = SCOPE_GLOBAL;
        }
        
        if (!_scope.equals(SCOPE_GLOBAL) && !_scope.equals(SCOPE_LOCAL) && !_scope.equals(SCOPE_PORTLET)) {
            throw new IllegalArgumentException("Invalid scope: " + _scope);
        }
    }
    
    public String getName() {
        return _name;
    }

    public String getScope() {
        return _scope;
    }

    public Object getValue() {
        return _value;
    }
    
    public boolean isLocalScope() {
        return _scope.equals(SCOPE_LOCAL);
    }
    public boolean isPortletScope() {
        return _scope.equals(SCOPE_PORTLET);
    }

}
