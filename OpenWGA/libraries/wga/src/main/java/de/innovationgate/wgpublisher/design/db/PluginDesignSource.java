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

package de.innovationgate.wgpublisher.design.db;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.vfs2.FileSystemException;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.LocalizedInformation;
import de.innovationgate.wga.common.WGAXML;
import de.innovationgate.wga.common.beans.DesignConfiguration;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.model.OverlaySupport;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;
import de.innovationgate.wgpublisher.design.OverlayDesignProvider;
import de.innovationgate.wgpublisher.design.WGABaseDesignNotAvailableException;
import de.innovationgate.wgpublisher.design.WGADesign;
import de.innovationgate.wgpublisher.design.WGADesignCreationException;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.WGADesignRetrievalException;
import de.innovationgate.wgpublisher.design.WGADesignSource;
import de.innovationgate.wgpublisher.design.WGADesignConfigurationException;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;

public class PluginDesignSource implements WGADesignSource {

    private WGACore _core;
    private String _name;
    private LocalizedInformation _locInfo;

    public List<String> getDesignNames() {
        
        
        List<String> designs = new ArrayList<String>();
        Iterator<WGAPlugin> plugins = _core.getPluginSet().getPlugins().iterator();
        while (plugins.hasNext()) {
            WGAPlugin plugin = (WGAPlugin) plugins.next();
            if (plugin.isActive() && plugin.isValid() && plugin.getCsConfig().getPluginConfig().isUsageAsDesignProvider()) {
                WGDatabase db = _core.getContentdbs().get(plugin.buildDatabaseKey());
                if (db != null) {
                    designs.add(plugin.getInstallationKey());
                }
            }
        }
        
        return designs;
        
    }

    public void init(WGACore core, String name, LocalizedInformation locInfo, Map<String, String> options) {
        _core = core;
        _name = name;
        _locInfo = locInfo;
    }

    public String getName() {
        return _name;
    }

    public void applyDesign(WGADesign design, WGDatabase db, Map<String,String> options) throws WGADesignConfigurationException {
        try {
            PluginDesignProvider designProvider = createDesignProvider(design, db, options);
            
            // Determine if this is an overlay plugin. If so we must find the base and apply an overlay design
            WGAPlugin plugin = getPlugin(design);
            if (plugin.getOverlayData() != null) {
                Version basePluginVersion = new Version(plugin.getOverlayData().getBasepluginVersion());
                WGAPlugin basePlugin = _core.getPluginSet().getBestMatchingPlugin(plugin.getOverlayData().getBasepluginName(), basePluginVersion, true);
                if (basePlugin == null) {
                    throw new WGABaseDesignNotAvailableException(plugin.getOverlayData().getBasepluginName(), basePluginVersion);
                }
                WGADesign baseDesign = getDesign(basePlugin.getInstallationKey());
                OverlayDesignProvider.applyOverlayDesign(_core, db, baseDesign, new HashMap<String,String>(), design, options);
            }
            else {
                db.setDesignProvider(designProvider);
                _core.getLog().info("Web application " + db.getDbReference() + " uses design of plugin " + getPlugin(design).getPluginID().toString());
            }
            
            db.setAllowDesignModification(false);
        }
        catch (WGADesignConfigurationException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGADesignConfigurationException("Exception applying design to web application " + db.getDbReference(), e);
        }

    }

    public PluginDesignProvider createDesignProvider(WGADesign design, WGDatabase db, Map<String, String> options) throws WGADesignConfigurationException {
        
        try {
            WGAPlugin plugin = getPlugin(design);
            return new PluginDesignProvider(design.createDesignReference(), _core, db, plugin.buildDatabaseKey(), plugin.getPluginID(), options);
        }
        catch (WGADesignConfigurationException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGADesignConfigurationException("Exception creating design provider", e);
        }
        
        
    }

    private WGAPlugin getPlugin(WGADesign design) throws WGADesignConfigurationException {
        WGAPlugin plugin = _core.getPluginSet().getPluginsByInstallationKey().get(design.getName());
        if (plugin == null) {
            throw new WGADesignConfigurationException("Cannot find plugin with installation key " + design.getName());
        }
        return plugin;
    }
    
    public String getDescription(Locale locale) {
        return _locInfo.getDescription(locale);
    }
    
    public String getTitle(Locale locale) {
        return _locInfo.getTitle(locale);
    }

    public void createDesign(String designName) throws WGNotSupportedException, WGADesignCreationException {
        throw new WGNotSupportedException("Creating designs is not supported for plugin design sources");
    }

    public boolean isDesignCreatable() {
        return false;
    }

    public Class getDesignProviderClass() {
        return PluginDesignProvider.class;
    }

    public WGADesign getDesign(String name) throws WGADesignRetrievalException {
        
        WGAPlugin plugin = _core.getPluginSet().getPluginsByInstallationKey().get(name);
        if (plugin == null || !(plugin.isActive() && plugin.isValid() && plugin.getCsConfig().getPluginConfig().isUsageAsDesignProvider())) {
            return null;
        }
            
        WGDatabase db = _core.getContentdbs().get(plugin.buildDatabaseKey());
        if (db == null) {
            return null;
        }
        
        WGADesign design = new WGADesign();
        design.setSource(this);
        design.setName(plugin.getInstallationKey());
        design.setDescription(plugin.getCsConfig().getPluginConfig().getDescription());
        design.setConfig(plugin.getCsConfig());
        design.setOverlayData(plugin.getOverlayData());

        String title = plugin.getCsConfig().getPluginConfig().getTitle();
        
        // In the case of an overlay plugin: Fetch the base design and determine overlay support. When optional then we add the name of the customizable base design to the design name.
        if (plugin.getOverlayData() != null) {
            Version basePluginVersion = new Version(plugin.getOverlayData().getBasepluginVersion());
            WGAPlugin basePlugin = _core.getPluginSet().getPluginByUniqueName(plugin.getOverlayData().getBasepluginName());
            if (basePlugin == null) {
                throw new WGADesignRetrievalException("Unable to retrieve base design plugin " + plugin.getOverlayData().getBasepluginName());
            }
            
            design.setMultiLanguage(WGADesign.isMultiLanguageDesign(basePlugin.getCsConfig()));
            
            PublisherOption overlaySupport = basePlugin.getCsConfig().findPublisherOption(PublisherOption.OPTION_OVERLAY_SUPPORT);
            PublisherOption overlayTheme = plugin.getCsConfig().findPublisherOption("OverlayTheme");
            if (overlaySupport != null && OverlaySupport.OPTIONAL.equals(overlaySupport.getValue()) && overlayTheme != null && "true".equals(overlayTheme.getValue())) {
                title = basePlugin.getCsConfig().getPluginConfig().getTitle() + " & " + title;
            }
        }
        
        design.setTitle(title);
        
        return design;
        
    }
}
