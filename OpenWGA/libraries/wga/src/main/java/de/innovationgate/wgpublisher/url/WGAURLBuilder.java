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

package de.innovationgate.wgpublisher.url;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public interface WGAURLBuilder {

    public void newRequest(WGACore core, HttpServletRequest req);
    
    public String buildContentURL(TMLContext context, String mediaKey, String layoutKey, boolean ignoreVirtualLink) throws WGException;
    
    public String buildLayoutURL(TMLContext context, String dbKey, String mediaKey, String layoutKey) throws WGException;
    
    public String buildFileURL(TMLContext context, String dbKey, String containerName, String fileName) throws WGException;
    
    public String buildActionURL(TMLContext context, TMLAction action, Map<String,Object> namedParams, List<Object> params, String portletMode, String portletContext) throws WGException;
    
    public String buildScriptURL(TMLContext context, String dbKey, String scriptType, String scriptName) throws WGException;
    
    public String buildHomepageURL(WGDatabase db, HttpServletRequest request) throws WGException;

    public String buildHomepageURL(TMLContext context) throws WGException;

    public String buildLoginURL(WGDatabase db, HttpServletRequest request, String redirectURL) throws WGException;
    
    public String buildLogoutURL(WGDatabase db, HttpServletRequest request, String redirectURL) throws WGException;
    
    

}
