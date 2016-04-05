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
 * Configuration for using OpenWGA in a clustered environment
 */
@Root(strict=false)
public class ClusterConfiguration extends ConfigBean {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @Attribute(required=false)
    @NotNull
    private boolean enabled = false;
    
    @Attribute
    @NotNull
    private boolean defaultMasterNode = false;
    
    @ElementMap(entry = "option", key = "name", attribute = true, required=false)
    @NotNull
    @Options
    private Map<String, String> options = new LinkedHashMap<String,String>();
    
    @Element(required=false)
    @NotNull
    private String implClassName;
    
    @Attribute(required=false)
    private String lbRoute;

    public ClusterConfiguration() {
        implClassName = "de.innovationgate.wgpublisher.cluster.SingleNodeClusterService";
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public boolean isDefaultMasterNode() {
        return defaultMasterNode;
    }

    public void setDefaultMasterNode(boolean defaultMasterNode) {
        this.defaultMasterNode = defaultMasterNode;
    }

    public Map<String, String> getOptions() {
        return options;
    }

    public void setOptions(Map<String, String> options) {
        this.options = options;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ClusterConfiguration && obj != null) {
            ClusterConfiguration other = (ClusterConfiguration) obj;
            return hashCode() == other.hashCode();
        }
        return false;
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(isEnabled());
        builder.append(getLbRoute());
        builder.append(isDefaultMasterNode());
        builder.append(getImplClassName());
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
    

    public String getLbRoute() {
        return lbRoute;
    }

    public void setLbRoute(String lbRoute) {
        this.lbRoute = lbRoute;
    }

}
