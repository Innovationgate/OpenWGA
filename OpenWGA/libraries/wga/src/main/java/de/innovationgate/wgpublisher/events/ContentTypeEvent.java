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
package de.innovationgate.wgpublisher.events;

import java.util.Map;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class ContentTypeEvent extends Event {
		

	private WGContent _document;
	private TMLUserProfile _userProfile;
	private Map<String,Object> _contextObjects;
		
	public ContentTypeEvent(WGContent document, TMLUserProfile userProfile, Map<String,Object> contextObjects) {
	    super(document.getDatabase().getRevisionObject(), null);
		_document = document;
		_userProfile = userProfile;
		_contextObjects = contextObjects;
	}
		
	/**
	 * @return
	 */
	public WGContent getdocument() {
		return _document;
	}

	/**
	 * @param document
	 */
	public void setdocument(WGContent document) {
		_document = document;
	}

	/**
	 * @return
	 */
	public TMLUserProfile getUserProfile() {
		return _userProfile;
	}
	
	public WGUserProfile getprofile() {
		if (_userProfile != null) {
			return _userProfile.getprofile();
		}
		else {
			return null;
		}
	}
	
	public WGDatabase getdatabase() {
		if (_document != null) {
			return _document.getDatabase();
		}
		else {
			return null;
		}
	}

	/**
	 * @return
	 */
	public Map<String,Object> getContextObjects() {
		return _contextObjects;
	}
	
	@Override
	public String getDatabaseKey() {
	    return _document.getDatabase().getDbReference();
	}
	

	@Override
	public Scope getScope() {
	    return Scope.SERVER;
	}
	
	@Override
	public Event createLocalDelegate() {
	    return this;
	}

}
	

