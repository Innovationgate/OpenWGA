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

/**
 * A message put out by the OpenWGA configuration migrator
 */
public class MigrationMessage {
	
	public static final int INFO = 0;
	public static final int WARNING = 1;
	public static final int ERROR = 2;
	
	private int _level;
	
	private String _message;
	
	private Throwable _throwable;
	
	public MigrationMessage(int level, String message) {
		setLevel(level);
		setMessage(message);
	}

	public MigrationMessage(int level, Throwable throwable) {
		setLevel(level);
		setMessage(throwable.getMessage());
		setThrowable(throwable);
	}
	
	public MigrationMessage(int level, String message, Throwable throwable) {
		setLevel(level);
		setMessage(message);
		setThrowable(throwable);
	}
	
	public int getLevel() {
		return _level;
	}
	
	public String getLevelName() {
		if (_level == INFO) {
			return "INFO";
		} else if (_level == WARNING) {
			return "WARNING";
		} else if (_level == ERROR) {
			return "ERROR";
		} else {
			return "unknown";
		}
	}

	public void setLevel(int level) {
		_level = level;
	}

	public String getMessage() {
		return _message;
	}

	public void setMessage(String message) {
		_message = message;
	}

	public Throwable getThrowable() {
		return _throwable;
	}

	public void setThrowable(Throwable throwable) {
		_throwable = throwable;
	}
	


}
