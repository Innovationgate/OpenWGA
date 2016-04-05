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

import java.util.Map;

/**
 * contains information about an ajaxCall
 * this object is created, serialized, zipped and encrypted by tml:include
 * and posted with the ajaxCall 
 *
 */
public class AjaxInfo {
    
    private String _id;
    private String _pageId;
    private String _portletPath;
    private String _tmlmodule;    
    private String _designDB;
    private String _mediaKey;
    // B00004832
    private String _contextPath;
    private String _profileSessionId;
    private String _initialMode;
    
    
    private Map<String,TMLOption> _options;
    private boolean _saveProfileOnEnd;
    private String _superform = null;
    
    private String _queryString;



	public AjaxInfo(String id) {
        _id = id;
    }
    
    public String getQueryString() {
		return _queryString;
	}

	public void setQueryString(String queryString) {
		_queryString = queryString;
	}
    
    /**
     * no-arg constructor for XStream on JRockit VM
     */
    private AjaxInfo() {        
    }
    
    public String getId() {
        return _id;
    }
    public String getMediaKey() {
        return _mediaKey;
    }
    public void setMediaKey(String mediaKey) {
        _mediaKey = mediaKey;
    }
    public Map<String,TMLOption> getOptions() {
        return _options;
    }
    public void setOptions(Map options) {
        _options = options;
    }
    public String getTmlmodule() {
        return _tmlmodule;
    }
    public void setTmlmodule(String tmlmodule) {
        _tmlmodule = tmlmodule;
    }

    public String getDesignDB() {
        return _designDB;
    }

    public void setDesignDB(String designDB) {
        _designDB = designDB;
    }

    /**
     * @return Returns the saveProfileOnEnd.
     */
    public boolean isSaveProfileOnEnd() {
        return _saveProfileOnEnd;
    }

    /**
     * @param saveProfileOnEnd The saveProfileOnEnd to set.
     */
    public void setSaveProfileOnEnd(boolean saveProfileOnEnd) {
        _saveProfileOnEnd = saveProfileOnEnd;
    }

	public String getContextPath() {
		return _contextPath;
	}

	public void setContextPath(String contextPath) {
		_contextPath = contextPath;
	}

    public String getSuperform() {
        return _superform;
    }

    public void setSuperform(String superform) {
        _superform = superform;
    }

    public String getProfileSessionId() {
        return _profileSessionId;
    }

    public void setProfileSessionId(String profileSessionId) {
        _profileSessionId = profileSessionId;
    }

    public String getInitialMode() {
        return _initialMode;
    }

    public void setInitialMode(String initialMode) {
        _initialMode = initialMode;
    }

    public String getPortletPath() {
        return _portletPath;
    }

    public void setPortletPath(String portletPath) {
        _portletPath = portletPath;
    }

    public String getPageId() {
        return _pageId;
    }

    public void setPageId(String pageId) {
        _pageId = pageId;
    }


}
