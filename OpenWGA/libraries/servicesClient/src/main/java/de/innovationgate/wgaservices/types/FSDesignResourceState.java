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
package de.innovationgate.wgaservices.types;


/**
 * holds the state of a file system design resource
 *
 */
public class FSDesignResourceState {
	
	public static final int TYPE_UNKNOWN = -1;
	public static final int TYPE_FOLDER = 1;
	public static final int TYPE_FILE = 2;
	
	private int _type = TYPE_UNKNOWN;
	private String _path = null;
	private long _lastmodified = 0;
	private String _md5sum = null;

	public int getType() {
		return _type;
	}

	public void setType(int type) {
		_type = type;
	}

	public String getPath() {
		return _path;
	}

	public void setPath(String path) {
		_path = path;
	}

	public long getLastmodified() {
		return _lastmodified;
	}

	public void setLastmodified(long lastmodified) {
		_lastmodified = lastmodified;
	}

	public void setMd5sum(String md5sum) {
		_md5sum = md5sum;
	}

	public String getMd5sum() {
		return _md5sum;
	}
}
