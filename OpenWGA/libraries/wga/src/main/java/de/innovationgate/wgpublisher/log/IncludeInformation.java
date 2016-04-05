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

import de.innovationgate.wgpublisher.design.DesignResourceReference;

public class IncludeInformation {
	
	private String _type;
	private DesignResourceReference _resourceRef;
	// holds the dbkey on tml includes - otherwise null
	private long _duration;
	
	
	public IncludeInformation(String type, DesignResourceReference designRef, long duration) {
		_type = type;
		_resourceRef = designRef;
		_duration = duration;
	}
	public String getType() {
		return _type;
	}
	public void setType(String type) {
		_type = type;
	}
	public String getRef() {
		return _resourceRef.getResourceName();
	}
	
	public String getDesignDb() {
	    return _resourceRef.getDesignApp();
	}
	
	public long getDuration() {
		return _duration;
	}
	public void setDuration(long duration) {
		_duration = duration;
	}
	
	
	

}
