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

package de.innovationgate.wgaservices.types;


/**
 * Information about a connected plugin
 */
public class PluginInfo {
    
    private String _uniqueName;
    
    private Version _version;
    
    private boolean _active;
    
    private boolean _platformPlugin;
    
    private boolean _defaultPlugin;
    
    private Version _minimumWGAVersion;
    
    private Version _miniumJavaVersion;    
    
    private boolean _valid;
    
    private boolean _developerPlugin;
    
    private String _installationKey;
    
    private String _overlaySupport;
    
    private String _title;
    
    private String _description;
    
    private String _vendor;
    
    private String _webHomepage;

    /**
     * Returns the plugin title
     */
    public String getTitle() {
        return _title;
    }

    /**
     * Sets the plugin title
     */
    public void setTitle(String title) {
        _title = title;
    }

    /**
     * Returns the plugin description
     */
    public String getDescription() {
        return _description;
    }

    /**
     * Sets the plugin description
     */
    public void setDescription(String description) {
        _description = description;
    }

    /**
     * Returns the overlay support of the plugin
     */
    public String getOverlaySupport() {
        return _overlaySupport;
    }

    /**
     * Sets the overlay support of the plugin
     */
    public void setOverlaySupport(String overlaySupport) {
        _overlaySupport = overlaySupport;
    }

    /**
     * Returns the installation key
     */
    public String getInstallationKey() {
        return _installationKey;
    }

    /**
     * Sets the installation key
     */
    public void setInstallationKey(String installationKey) {
        _installationKey = installationKey;
    }

    /**
     * Returns if the plugin is valid
     */
    public boolean isValid() {
        return _valid;
    }

    /**
     * Sets if the plugin is valid
     */
    public void setValid(boolean valid) {
        _valid = valid;
    }

    /**
     * Returns if this is a developer plugin
     */
    public boolean isDeveloperPlugin() {
        return _developerPlugin;
    }

    /**
     * Sets if this is a developer plugin
     */
    public void setDeveloperPlugin(boolean developerPlugin) {
        _developerPlugin = developerPlugin;
    }

    /**
     * Returns the unique name of the plugin
     */
    public String getUniqueName() {
        return _uniqueName;
    }

    /**
     * Sets the unique name of the plugin
     */
    public void setUniqueName(String uniqueName) {
        _uniqueName = uniqueName;
    }

    /**
     * Returns the version of the plugin
     */
    public Version getVersion() {
        return _version;
    }

    /**
     * Sets the version of the plugin
     */
    public void setVersion(Version version) {
        _version = version;
    }

    /**
     * Returns if the plugin is active
     */
    public boolean isActive() {
        return _active;
    }

    /**
     * Sets if the plugin is active
     */
    public void setActive(boolean active) {
        _active = active;
    }

    /**
     * Sets if this is a platform plugin
     */
    public void setPlatformPlugin(boolean platformPlugin) {
        _platformPlugin = platformPlugin;
    }

    /**
     * Returns if this is a platform plugin
     */
    public boolean isPlatformPlugin() {
        return _platformPlugin;
    }

    /**
     * Sets if this is a default plugin
     */
    public void setDefaultPlugin(boolean defaultPlugin) {
        _defaultPlugin = defaultPlugin;
    }

    /**
     * Returns if this is a default plugin
     */
    public boolean isDefaultPlugin() {
        return _defaultPlugin;
    }

    /**
     * Sets the minimum OpenWGA version for running this plugin
     */
    public void setMinimumWGAVersion(Version minimumWGAVersion) {
        _minimumWGAVersion = minimumWGAVersion;
    }

    /**
     * Returns the minimum OpenWGA version for running this plugin
     */
    public Version getMinimumWGAVersion() {
        return _minimumWGAVersion;
    }

    /**
     * Returns the minimum Java version for running this plugin
     */
    public Version getMiniumJavaVersion() {
        return _miniumJavaVersion;
    }

    /**
     * Sets the minimum Java version for running this plugin
     */
    public void setMiniumJavaVersion(Version miniumJavaVersion) {
        _miniumJavaVersion = miniumJavaVersion;
    }

    /**
     * Returns the URL of the plugins web homepage
     */
    public String getWebHomepage() {
        return _webHomepage;
    }

    /**
     * Sets the URL of the plugins web homepage
     */
    public void setWebHomepage(String webHomepage) {
        _webHomepage = webHomepage;
    }

    /**
     * Returns the name of the plugin vendor
     */
    public String getVendor() {
        return _vendor;
    }

    /**
     * Sets the name of the plugin vendor
     */
    public void setVendor(String vendor) {
        _vendor = vendor;
    }
}
