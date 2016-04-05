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
package de.innovationgate.wgpublisher.log;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

/**
 * holds data about the current request
 *
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public class WGARequestInformation {
	
	public static final String REQUEST_ATTRIBUTENAME = "de.innovationgate.wgpublisher.log.WGARequestInformation";
	
	private long _startTime;
	private long _endTime;
	private String _mimeType;
	private WGDatabase _database;
	private WGPRequestPath _path;
	private WGContent _content;
	private TMLUserProfile _profile;
	private WGTMLModule _design;
	private boolean _loggingEnabled = true;
	private boolean _commited = false;
	private boolean _ajax = false;
	private int _statusCode = -1;
	private String _statusMessage;
	private WeakReference<Thread> _thread = null;
	
	private int _type = TYPE_UNKNOWN;
	
	public static final int TYPE_UNKNOWN = -1;
	public static final int TYPE_CONTENT = 1;
	public static final int TYPE_TML = 2;
	public static final int TYPE_SCRIPT = 3;
	public static final int TYPE_FILE = 4;
	public static final int TYPE_STATIC = 5;
	public static final int TYPE_RESOURCE = 6;
	public static final int TYPE_AJAXFORM = 7;
	
	// map for custom logging information
	private Map<String,Object> _attributes = new HashMap<String, Object>();
	
	private List<IncludeInformation> _includes = new ArrayList<IncludeInformation>();
 
	private String _classID;

    private String _profileId = null;
	
	public long getStartTime() {
		return _startTime;
	}
	public void setStartTime(long startTime) {
		_startTime = startTime;
	}
	
	public long getEndTime() {
		return _endTime;
	}
	public void setEndTime(long endTime) {
		_endTime = endTime;
	}
	
	public String getMimeType() {
		return _mimeType;
	}
	public void setMimeType(String mimeType) {
		_mimeType = mimeType;
	}
	
	public WGPRequestPath getPath() {
		return _path;
	}
	public void setPath(WGPRequestPath path) {
		_path = path;
	}
	
	public WGContent getContent() {
		return _content;
	}
	public void setContent(WGContent content) {
		_content = content;
	}
	
	public TMLUserProfile getProfile() {
		return _profile;
	}
	public void setProfile(TMLUserProfile profile) throws WGAPIException {
		_profile = profile;
		if (!_profile.getprofile().isTemporary()) {
		    _profileId = _profile.getprofile().getName();
		}
	}
	
	public WGTMLModule getDesign() {
		return _design;
	}
	public void setDesign(WGTMLModule design) {
		_design = design;
	}
	
	@CodeCompletion
	public boolean isLoggingEnabled() {
		return _loggingEnabled;
	}
	@CodeCompletion
	public void setLoggingEnabled(boolean loggingEnabled) {
		_loggingEnabled = loggingEnabled;
	}
	
	@CodeCompletion
	public Map<String, Object> getAttributes() {
		return _attributes;
	}
	public void setAttributes(Map<String, Object> attributes) {
		_attributes = attributes;
	}
	public WGDatabase getDatabase() {
		return _database;
	}
	public void setDatabase(WGDatabase database) {
		_database = database;
	}
	public boolean isCommited() {
		return _commited;
	}
	public void setCommited(boolean commited) {
		_commited = commited;
	}
	public void setAjax(boolean ajax) {
		_ajax = ajax;
	}
	public boolean isAjax() {
		return _ajax;
	}
	public List<IncludeInformation> getIncludes() {
		return _includes;
	}
	public int getStatusCode() {
		return _statusCode;
	}
	public void setStatusCode(int statusCode) {
		_statusCode = statusCode;
	}
	public String getStatusMessage() {
		return _statusMessage;
	}
	public void setStatusMessage(String statusMessage) {
		_statusMessage = statusMessage;
	}
	public int getType() {
		return _type;
	}
	public void setType(int type) {
		_type = type;
	}
	public String getClassID() {
		return _classID;
	}
	public void setClassID(String id) {
		_classID = id;
	}
	
	public void setThread(Thread t) {
        _thread = new WeakReference<Thread>(t);
	}
	
	public Thread getThread() {
	    if (_thread != null) {
	        return _thread.get();
	    }
	    else {
	        return null;
	    }
	}
    public String getProfileId() {
        return _profileId;
    }
}
