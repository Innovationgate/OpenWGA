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

package de.innovationgate.wgpublisher.expressions.tmlscript;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.innovationgate.wgpublisher.events.EventPath;

public class TMLScriptObjectMetadata {
    
    public static class Method {
        
        private String _name;
        public String getName() {
            return _name;
        }

        private boolean _directAccess = true;
        
        public Method(String name) {
            _name = name;
        }

        public boolean isDirectAccess() {
            return _directAccess;
        }

        public void setDirectAccess(boolean directAccess) {
            _directAccess = directAccess;
        }
        
    }
    
    public static class PortletEventListener {
        
        private String _eventName;
        private String _ajaxMode = "true";
        private String _method;
        
        public void setEventName(String eventName) {
            _eventName = eventName;
        }

        public void setAjaxMode(String ajaxMode) {
            _ajaxMode = ajaxMode;
        }

        public void setMethod(String method) {
            _method = method;
        }

        public String getEventName() {
            return _eventName;
        }

        public String getAjaxMode() {
            return _ajaxMode;
        }

        public String getMethod() {
            return _method;
        }
        
    }

    private Map<String,Method> _methods = new HashMap<String, TMLScriptObjectMetadata.Method>();
    
    public Map<String, Method> getMethods() {
        return _methods;
    }

    private Map<EventPath,String> _appEventListeners = new HashMap<EventPath,String>();
    
    private List<PortletEventListener> _portletEventListeners = new ArrayList<TMLScriptObjectMetadata.PortletEventListener>();

    public Map<EventPath, String> getAppEventListeners() {
        return _appEventListeners;
    }

    public List<PortletEventListener> getPortletEventListeners() {
        return _portletEventListeners;
    }
    

}
