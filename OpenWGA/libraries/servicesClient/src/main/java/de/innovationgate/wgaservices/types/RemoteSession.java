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

import de.innovationgate.wgaservices.WGACoreServices;
import de.innovationgate.wgaservices.WGAServices;

/**
 * Represents a WGA Services Remote Session on the WGA Server, logged in to a single WGA domain.
// * Can be created via {@link WGACoreServices#login(String, String, String)} or {@link WGACoreServices#adminLogin(String, String)}.
 */
public class RemoteSession {

	private String _password;
	private String _username;
	private String _domain;

	/**
	 * Public constructor. Do not use from "outside".
	 * @param domain
	 * @param username
	 * @param password
	 */
	public RemoteSession (String domain, String username, String password) {
		_domain = domain;
		_username = username;
		_password = password;
	}
	
	/**
	 * Default constructor. Do not use from "outside".
	 */
	public RemoteSession() {
	}

	/**
	 * Returns the password of the login
	 */
	public String getPassword() {
		return _password;
	}

	/**
	 * Returns the username of the session
	 */
	public String getUsername() {
		return _username;
	}

	public void setPassword(String string) {
		_password = string;
	}

	public void setUsername(String string) {
		_username = string;
	}

	/**
	 * Returns the WGA domain that the user is logged in to. Is null when it represents an administrative login.
	 */
	public String getDomain() {
		return _domain;
	}

	public void setDomain(String string) {
		_domain = string;
	}

}
