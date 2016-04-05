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

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.servers.WGDatabaseServer;

public class AccessLogInformation {
	
	private Class<? extends WGALogger> _implementationClass;
	
	private WGDatabaseServer _dbServer;
	
	private String _location;
	
    private String _title;

	private Map<String,String> _options = new HashMap<String,String>();

	public Class<? extends WGALogger> getImplementationClass() {
		return _implementationClass;
	}

	public void setImplementationClass(Class<? extends WGALogger> implementationClass) {
		_implementationClass = implementationClass;
	}

	public WGDatabaseServer getDbServer() {
		return _dbServer;
	}

	public void setDbServer(WGDatabaseServer dbServer) {
		_dbServer = dbServer;
	}

	public Map<String, String> getOptions() {
		return _options;
	}

	public void setOptions(Map<String, String> options) {
		_options = options;
	}

	public void setLocation(String location) {
		_location = location;
	}

	public String getLocation() {
		return _location;
	}

	public void setTitle(String title) {
		_title = title;
	}

	public String getTitle() {
		return _title;
	}
}
