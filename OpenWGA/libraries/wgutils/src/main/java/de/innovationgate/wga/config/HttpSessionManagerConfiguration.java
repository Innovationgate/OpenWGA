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

package de.innovationgate.wga.config;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

/**
 * Configuration for HTTP session management
 */
@Root(strict=false)
public class HttpSessionManagerConfiguration extends ConfigBean {

    private static final long serialVersionUID = 1L;
    
    @Attribute
    @NotNull
    private boolean enabled = false;

    @Element(required=false)
    private String implClassName;
    
    @ElementMap(entry = "option", key = "name", attribute = true, required=false)
    @NotNull
    @Options
    private Map<String, String> options = new LinkedHashMap<String,String>();

    public Map<String, String> getOptions() {
        return options;
    }

    public void setOptions(Map<String, String> options) {
        this.options = options;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof HttpSessionManagerConfiguration && obj != null) {
            HttpSessionManagerConfiguration other = (HttpSessionManagerConfiguration) obj;
            return hashCode() == other.hashCode();
        }
        return false;
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(getImplClassName());
        builder.append(isEnabled());
        Iterator<String> optionsNames = options.keySet().iterator();
        while (optionsNames.hasNext()) {
            String name = optionsNames.next();
            String value = options.get(name);
            builder.append(name);
            builder.append(value);
        }
        return builder.toHashCode();
    }

    public String getImplClassName() {
        return implClassName;
    }

    public void setImplClassName(String implClassName) {
        this.implClassName = implClassName;
    }
    

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }
}
