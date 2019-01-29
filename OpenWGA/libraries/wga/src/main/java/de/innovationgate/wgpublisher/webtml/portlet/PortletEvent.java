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
package de.innovationgate.wgpublisher.webtml.portlet;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import net.sf.json.JSONSerializer;
import net.sf.json.util.JSONBuilder;
import net.sf.json.util.JSONUtils;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.jackrabbit.util.ISO8601;

import com.thoughtworks.xstream.io.json.JsonWriter;

import de.innovationgate.utils.ISO8601DateFormat;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.WGAVersion;

/**
 * Represents a portlet event
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE, beanMode=CodeCompletion.BEAN_MODE_ALL)
public class PortletEvent implements Serializable {
    
    private static final long serialVersionUID = 1L;

    private static long eventIndex = Long.MIN_VALUE;
    
    private static synchronized long retrieveEventIndex() {

        // The number of 2^63*2 possible event indices should be just as good as a unique name
        // (If one event is triggered every second it would take us 584 billion years to reach the end.
        // We should be able to convince our customers to mandatorily reboot their WGA instance in that period of time ;-)

        eventIndex++;
        return eventIndex;
    }
    
    private String _name;
    
    // contains the portletkey of the portlet which fired this event
    private String _source;
    
    // Contains the name of the portlet which fired this event
    private String _sourceName;
    
    // May contain a target portlet key to let this (server side) event solely effect that portlet 
    private String _targetPortletKey = null;
       
    private Map<String,Object> _parameters;

    private long _index;

    private Version _compliance;
    
    // system events
    public static final PortletEvent SESSION_IS_NEW_EVENT = new PortletEvent("de.innovationgate.wga.events.SessionIsNew", WGAVersion.toCsConfigVersion());
    public static final PortletEvent LOGIN_REQUIRED_EVENT = new PortletEvent("de.innovationgate.wga.events.LoginRequired", WGAVersion.toCsConfigVersion());
    
    /**
     * Constructor. Just for internal WGA use.
     * @param name Name of the event
     * @param compliance Compliance of the originating WGA design
     */
    public PortletEvent(String name, Version compliance) {
        if (name.indexOf("\"") != -1) {
            throw new IllegalArgumentException("Doublequotes in event names are not supported.");
        }
        _name = name;
        _parameters = new HashMap<String,Object>();
        _index = Long.MIN_VALUE;
        _compliance = compliance;
    }
    
    /**
     * Lets the event fetch an event index.
     * This must be called when the event is fired and issued to the serverside event queue
     * to guarantee that the order of indexes in this queue is small-to-large
     */
    protected void retrieveIndex() {
        _index = retrieveEventIndex();
    }
    
    /**
     * Add a parameter to the event
     * @param name parameter name
     * @param value parameter value
     */
    @CodeCompletion
    public void setParameter(String name, Object value) {
        if (name.indexOf(" ") != -1) {
            throw new IllegalArgumentException("Spaces in event parameter names are not supported.");
        } else if (name.indexOf("\"") != -1) {
            throw new IllegalArgumentException("Doubleqoutes in event parameter names are not supported.");
        } else if (name.indexOf(".") != -1) {
            throw new IllegalArgumentException("Dots in event parameter names are not supported.");
        }    
        
        if (value == null) {
            _parameters.put(name, null);
        }
        if (value instanceof String) {
            _parameters.put(name, value);
        }
        else if (_compliance.isAtLeast(6, 2) && (value instanceof Number ||
                                                    value instanceof Date ||
                                                    value instanceof Boolean)) {
            _parameters.put(name, value);
        }
        else {
            _parameters.put(name, String.valueOf(value));
        }
    }
    
    @Deprecated
    // use setParameter instead
    public void addParameter(String name, Object value) {
    	setParameter(name, value);
    }
    
    /**
     * Retrieves the value of an event parameter
     * @param name Name of the parameter
     */
    @CodeCompletion
    public Object getParameter(String name) {
        return _parameters.get(name);
    }

    /**
     * Retrieves the event parameter as a map
     * @param name Name of the parameter
     */
    @CodeCompletion
    public Map<String,Object> getParameters() {
        return _parameters;
    }

    /**
     * Returns the name of the event
     */
    @CodeCompletion
    public String getName() {
        return _name;
    }
    
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(_name);
        return builder.toHashCode();
    }
    
    /**
     * Exports the event data to Javascript code
     */
    public String toJavaScriptObject() {
        StringBuffer javaScript = new StringBuffer();        
        javaScript.append("{name:\"" + _name + "\",");
        javaScript.append("index: \"" + String.valueOf(_index) + "\",");
        javaScript.append("source:\"" + _source + "\",");
        // create script for event-parameters
        javaScript.append("params:{");
        Iterator keys = _parameters.keySet().iterator();
        while (keys.hasNext()) {
            String paramName = (String) keys.next();
            Object paramValue = _parameters.get(paramName);
            if (paramValue instanceof String) {
                javaScript.append(paramName + ":\"" + ((String) paramValue).replaceAll("\"","\\\\\"") + "\"");
            }
            else if (paramValue instanceof Number) {
                javaScript.append(paramName + ":" + JSONUtils.numberToString((Number) paramValue));
            }
            else if (paramValue instanceof Date) {
                ISO8601DateFormat df = new ISO8601DateFormat();
                javaScript.append(paramName + ":\"" + df.format((Date) paramValue) + "\"");
            }
            else if (paramValue instanceof Boolean) {
                javaScript.append(paramName + ":" + String.valueOf((Boolean) paramValue));
            }
            
            if (keys.hasNext()) {
                javaScript.append(",");
            }
        }
        javaScript.append("}}");
        return javaScript.toString();
    }
    
    /**
     * 
     * Returns the portletKey of the portlet which fired the event
     * might be 'null' on system events
     */
    public String getSource() {
        return _source;
    }

    /**
     * Sets the portlet key of the portlet that fired the event
     */
    public void setSource(String source) {
        _source = source;
    }

    /**
     * Returns the name of the portlet that fired the event
     */
    @CodeCompletion
    public String getSourceName() {
        return _sourceName;
    }

    /**
     * Sets the name of the portlet that fired the evente
     */
    public void setSourceName(String sourceName) {
        _sourceName = sourceName;
    }

    /**
     * Returns the index of the events in the user sessions event queue
     */
    public long getIndex() {
        return _index;
    }
    
    /**
     * Returns a string representation of the event index
     */
    public String getIndexStr() {
        return (new Long(_index)).toString();
    }

    /**
     * Returns a portlet key of the portlet for which this event was specifically fired. Is null if this is a global event.
     */
    @CodeCompletion
    public String getTargetPortletKey() {
        return _targetPortletKey;
    }

    /**
     * Sets a portlet key of the portlet for which this event was specifically fired.
     */
    @CodeCompletion
    public void setTargetPortletKey(String targetPortletKey) {
        _targetPortletKey = targetPortletKey;
    }

    @CodeCompletion
    public void setParameters(Map<String,Object> parameters) {
        _parameters = parameters;
    }

    

}
