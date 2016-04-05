/*******************************************************************************
 * Copyright (c) 2009, 2010 Innovation Gate GmbH.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Innovation Gate GmbH - initial API and implementation
 ******************************************************************************/

package de.innovationgate.wgaservices.pluginstore;

import java.util.Map;

import de.innovationgate.wgaservices.types.PluginInfo;
import de.innovationgate.wgaservices.types.Version;

public class PluginStorePlugin extends PluginInfo {
        
    private String _filename;
    
    private String _licenseTitle;
    private String _licenseText; 
    
    private String _category;
    
    /**
     * FLAG for dependency status
     * plugin dependencies might get one or more of the following flags here
     * 0 - OK
     * 1 - to low WGAVersion
     * 2 - to low JavaVersion
     * 4 - plugin not in store
     * 8 - invalid meta information in store (wgaversion/javaversion)
     */
    private int _status;
    
    private String _iconURL;
    private String _screenShotURL;

    private String _overlayBasePluginName;
    private Version _overlayBasePluginVersion;        
    private boolean _theme = false;

    public static PluginStorePlugin createFrom(String fileName, Map<String,Object> properties) {
        String uname = fileName.substring(0, fileName.lastIndexOf("-"));
        
        PluginStorePlugin plugin = new PluginStorePlugin();
        plugin.setUniqueName(uname);
        
        plugin.setFilename(fileName);
        
        String sVersion = (String) properties.get("version"); 
        if (sVersion != null) {
            plugin.setVersion(new Version(sVersion));
        }
        
        plugin.setLicenseTitle((String) properties.get("licensetitle"));
        plugin.setLicenseText((String) properties.get("licensetext"));
        plugin.setCategory((String) properties.get("category"));
        plugin.setTitle((String) properties.get("title"));
        
        if (properties.get("status") != null) {
            if (properties.get("status") instanceof Double) {
                plugin.setStatus(new Double((Double)properties.get("status")).intValue());    
            } else {
                plugin.setStatus(new Double(properties.get("status").toString().trim()).intValue());
            }
                
        }
        
        
        sVersion = (String)properties.get("javaversion"); 
        if (sVersion != null) {
            plugin.setMiniumJavaVersion(new Version(sVersion));
        }

        plugin.setVendor((String)properties.get("vendor"));
        plugin.setDescription((String)properties.get("description"));
        
        sVersion = (String)properties.get("wgaversion"); 
        if (sVersion != null) {
            plugin.setMinimumWGAVersion(new Version(sVersion));
        }

        String url = (String)properties.get("iconurl");
        if (url != null) {
            plugin.setIconURL(PluginStore.PLUGIN_STORE_URL + properties.get("iconurl"));
        }
        
        plugin.setWebHomepage((String)properties.get("homepage"));
        plugin.setOverlaySupport((String)properties.get("overlaysupport"));
        plugin.setOverlayBasePluginName((String)properties.get("overlay.basepluginname"));
        
        sVersion = (String)properties.get("overlay.basepluginversion");
        if (sVersion != null) {
            plugin.setOverlayBasePluginVersion(new Version(sVersion));
        }
        
        url = (String)properties.get("screenshoturl");
        if (url != null) {
            plugin.setScreenShotURL(PluginStore.PLUGIN_STORE_URL + properties.get("screenshoturl"));
        }
        
        String sTheme = (String)properties.get("overlay.theme");
        if (sTheme != null) {
            plugin.setTheme(Boolean.parseBoolean(sTheme));
        } else {
            plugin.setTheme(false);
        }
        
        return plugin;
    }

    public String getLicenseTitle() {
        return _licenseTitle;
    }
    public void setLicenseTitle(String licenseTitle) {
        _licenseTitle = licenseTitle;
    }
    public String getLicenseText() {
        return _licenseText;
    }
    public void setLicenseText(String licenseText) {
        _licenseText = licenseText;
    }
    public String getCategory() {
        return _category;
    }
    public void setCategory(String category) {
        _category = category;
    }
    
    public int getStatus() {
        return _status;
    }
    public void setStatus(int status) {
        _status = status;
    }

    public String getIconURL() {
        return _iconURL;
    }
    public void setIconURL(String iconURL) {
        _iconURL = iconURL;
    }
  
    public boolean hasOverlaySupport() {
        return getOverlaySupport() != null;
    }

    public String getOverlayBasePluginName() {
        return _overlayBasePluginName;
    }

    public void setOverlayBasePluginName(String overlayBasePluginName) {
        _overlayBasePluginName = overlayBasePluginName;
    }

    public Version getOverlayBasePluginVersion() {
        return _overlayBasePluginVersion;
    }

    public void setOverlayBasePluginVersion(Version overlayBasePluginVersion) {
        _overlayBasePluginVersion = overlayBasePluginVersion;
    }
    
    public String getFilename() {
        return _filename;
    }

    public void setFilename(String filename) {
        _filename = filename;
    }

    public void setTheme(boolean theme) {
        _theme = theme;
    }

    public boolean isTheme() {
        return _theme;
    }    
    
    public String getScreenShotURL() {
        return _screenShotURL;
    }

    public void setScreenShotURL(String screenShotURL) {
        _screenShotURL = screenShotURL;
    }
    

}
