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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

/**
 * A virtual host definition
 */
@Root(strict=false)
public class VirtualHost extends IdentifiableConfigBean {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static final String UID_ALL_DATABASES = "$all";
    
    @Attribute (required=false)
    private boolean enabled = true;

    @Attribute
    @NotNull
    private String servername;
    
    @Attribute (required=false)
    private String defaultDatabase;
    
    @ElementList
    @NotNull
    private List<String> allowedDatabases = new ArrayList<String>();
    
    @ElementList
    @NotNull
    private List<String> serverAliases = new ArrayList<String>();
    
    @ElementList
    @NotNull
    private List<VirtualResource> virtualResources = new ArrayList<VirtualResource>();
    
    @Attribute (required=false)
    private boolean hideDefaultDatabaseInURL = false;

    @Attribute (required=false)
    private boolean hideHomepageURL = false;

    public VirtualHost() {
        super();
    }
    
    public VirtualHost(String servername, String defaultDatabase) {
        this();
        setServername(servername);
        setDefaultDatabase(defaultDatabase);
    }

    public String getServername() {
        return servername;
    }

    public void setServername(String servername) {
        this.servername = servername;
    }

    public String getDefaultDatabase() {
        return defaultDatabase;
    }

    public void setDefaultDatabase(String defaultDatabase) {
        this.defaultDatabase = defaultDatabase;
    }

    public List<String> getAllowedDatabases() {
        return allowedDatabases;
    }

    public void setAllowedDatabases(List<String> allowedDatabases) {
        this.allowedDatabases = allowedDatabases;
    }

    public List<String> getServerAliases() {
        return serverAliases;
    }

    public void setServerAliases(List<String> serverAliases) {
        this.serverAliases = serverAliases;
    }

    public void setVirtualResources(List<VirtualResource> virtualResources) {
        this.virtualResources = virtualResources;
    }

    public List<VirtualResource> getVirtualResources() {
        return virtualResources;
    }
    
    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }    

    public boolean isHideDefaultDatabaseInURL() {
        return hideDefaultDatabaseInURL;
    }

    public void setHideDefaultDatabaseInURL(boolean hideDefaultDatabaseInURL) {
        this.hideDefaultDatabaseInURL = hideDefaultDatabaseInURL;
    }


    public boolean isHideHomepageURL() {
        return hideHomepageURL;
    }

    public void setHideHomepageURL(boolean proxyDefaultDatabaseInURL) {
        this.hideHomepageURL = proxyDefaultDatabaseInURL;
    }

}
