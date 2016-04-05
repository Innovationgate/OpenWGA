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

package de.innovationgate.wgpublisher.webtml.utils;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.WGACore;

/**
 * A session bound map holding WebTML variable params
 */
public class VarParamsMap extends HashMap<String,Object> {
    
    private static final long serialVersionUID = 1L;
    private String _sessionId = null;
    private String _path = null;
    @SuppressWarnings("unused")
    private Boolean _sessionBound = null; // Backward compatibility for pre WGA 6.2 var params (#00003554)
    
    public VarParamsMap(HttpSession session) {
        _sessionId = session.getId();
    }
    
    public VarParamsMap() {
    }

    public String getSessionId() {
        return _sessionId;
    }
    
    public boolean isValidRequest(HttpServletRequest request) throws MalformedURLException, WGAPIException {
    
        if (_path != null) {
            if (request == null) {
                return false;
            }
            
            URL requestURL = new URL((String) request.getAttribute(WGACore.ATTRIB_REQUESTURL));
            String path = requestURL.getPath();
            
            // Strip off path parameters which are ignored (#00002876)
            int semiPos = path.indexOf(";");
            if (semiPos != -1) {
                path = path.substring(0, semiPos);
            }
            
            if (!_path.equals(path)) {
                return false;
            }
        }
        
        if (isSessionBound() && (request.getSession() == null || !_sessionId.equals(request.getSession().getId()))) {
            return false;
        }
        
        return true;
        
    }

    public String getPath() {
        return _path;
    }

    public void setPath(String path) {
        // Strip off path parameters which are ignored (#00002876)
        int semiPos = path.indexOf(";");
        if (semiPos != -1) {
            path = path.substring(0, semiPos);
        }
        
        _path = path;
    }

    public boolean isSessionBound() {
        
        // Downward compatiblity for pre WGA 6.2 var params (#00003554)
        if (_sessionBound != null) {
            return _sessionBound.booleanValue();
        }
        
        return _sessionId != null;
    }

}
