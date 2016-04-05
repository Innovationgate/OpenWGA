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

import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.common.LocalizedInformation;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.db.DBDesignProvider;

public interface WGADesignSource {
    
    public void init(WGACore core, String name, LocalizedInformation locInfo, Map<String,String> options) throws WGADesignConfigurationException;
    
    public List<String> getDesignNames() throws WGADesignRetrievalException;
    
    public WGADesign getDesign(String name) throws WGADesignRetrievalException;
    
    public String getName();
    
    public String getTitle(Locale locale);
    
    public String getDescription(Locale locale);
    
    public void applyDesign(WGADesign design, WGDatabase db, Map<String,String> options) throws WGADesignConfigurationException;
    
    public boolean isDesignCreatable();
    
    public void createDesign(String designName) throws WGNotSupportedException, WGADesignCreationException;
    
    public WGADesignProvider createDesignProvider(WGADesign design, WGDatabase db, Map<String, String> options) throws WGADesignConfigurationException;
    
    public Class getDesignProviderClass();

}
