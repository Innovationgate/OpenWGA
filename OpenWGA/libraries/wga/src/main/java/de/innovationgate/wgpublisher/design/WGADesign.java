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

package de.innovationgate.wgpublisher.design;

import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.config.Design;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.model.OverlaySupport;
import de.innovationgate.wgpublisher.WGACore;

public class WGADesign {
    
    
    public static boolean isMultiLanguageDesign(CSConfig config) {
        
        PublisherOption multiLangOpt = config.findPublisherOption(WGACore.DBATTRIB_MULTILANGUAGE_CONTENT);
        if (multiLangOpt != null) {
            return Boolean.parseBoolean(multiLangOpt.getValue());
        }
        else {
            return true;
        }
    }
    
    public WGADesign() {
        
    }
    
    public WGADesign(WGADesignSource collection, String name, String title, String description) {
        super();
        _source = collection;
        _name = name;
        _title = title;
        _description = description;
    }
    
    private String _name;
    private String _title;
    private String _description;
    private WGADesignSource _source;
    private CSConfig _config;
    private OverlayData _overlayData;
    private boolean _multiLanguage;
    
    public String getName() {
        return _name;
    }
    public String getTitle() {
        return _title;
    }
    public String getDescription() {
        return _description;
    }
    public WGADesignSource getSource() {
        return _source;
    }

    public void setName(String name) {
        _name = name;
    }

    public void setTitle(String title) {
        _title = title;
    }

    public void setDescription(String description) {
        _description = description;
    }

    public void setSource(WGADesignSource collection) {
        _source = collection;
    }
    
    public void applyDesign(WGDatabase db, Map<String,String> options) throws WGADesignConfigurationException {
        getSource().applyDesign(this, db, options);
    }
    
    public Design createDesignConfiguration() {
        
        Design design = new Design(getSource().getName(), getName());
        return design;
        
    }
    
    public String getReference() {
        return createDesignReference().toString();
    }
    
    public DesignReference createDesignReference() {
        return new DesignReference(getSource().getName(), getName(), null);
    }

    public CSConfig getConfig() {
        return _config;
    }

    public void setConfig(CSConfig config) {
        _config = config;
        if (config != null) {
            _multiLanguage = isMultiLanguageDesign(config);
        }
        else {
            _multiLanguage = true;
        }
    }
    
    public boolean isMultiLanguage() {
        return _multiLanguage;
    }
    
    public String getOverlaySupport() {
        
        if (_config != null) {
            PublisherOption overlayOpt = _config.findPublisherOption(PublisherOption.OPTION_OVERLAY_SUPPORT);
            if (overlayOpt != null) {
                return overlayOpt.getValue();
            }
        }
        
        return OverlaySupport.NONE;
        
    }
    
    public WGADesignProvider createDesignProvider(WGDatabase db, Map<String,String> options) throws WGADesignConfigurationException  {
        return getSource().createDesignProvider(this, db, options);
    }

    public OverlayData getOverlayData() {
        return _overlayData;
    }

    public void setOverlayData(OverlayData overlayData) {
        _overlayData = overlayData;
    }

    public void setMultiLanguage(boolean multiLanguage) {
        _multiLanguage = multiLanguage;
    }

}
