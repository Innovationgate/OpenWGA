/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.config;

import java.util.ArrayList;
import java.util.List;

/**
 * Object collecting the results of an OpenWGA configuration migration
 */
public class MigrationResult {
	
	private WGAConfiguration _config;
	private List<MigrationMessage> _log = new ArrayList<MigrationMessage>();
	
	public WGAConfiguration getConfig() {
		return _config;
	}
	public void setConfig(WGAConfiguration config) {
		_config = config;
	}
	public List<MigrationMessage> getLog() {
		return _log;
	}
	
	public void logWarning(String message) {
		getLog().add(new MigrationMessage(MigrationMessage.WARNING, message));
	}
	
	public void logInfo(String message) {
		getLog().add(new MigrationMessage(MigrationMessage.INFO, message));
	}
	
	public void logError(String message) {
		getLog().add(new MigrationMessage(MigrationMessage.ERROR, message));
	}
	
	public void logError(String message, Throwable e) {
		getLog().add(new MigrationMessage(MigrationMessage.ERROR, message, e));		
	}

}
