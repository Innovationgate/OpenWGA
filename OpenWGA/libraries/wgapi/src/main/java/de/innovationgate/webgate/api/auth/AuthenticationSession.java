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
package de.innovationgate.webgate.api.auth;

import java.util.Set;

/**
 * Represents an authentication session of a logged in user, providing some additional information about him.
 */
public interface AuthenticationSession {

	/**
	 * Returns the distinguished, fully qualified, unique name of the user
	 */
	public String getDistinguishedName();
	/**
	 * Returns the e-mail address of the user if known
	 */
	public String getMailAddress();
	/**
	 * Returns all known name variants/aliases of the user
	 */
	public Set<String> getNames();
	/**
	 * Returns the user groups that the user is member of
	 */
	public Set<String> getGroups();
	/**
	 * Closes the authentication session making this object invalid 
	 */
	public void logout();
	/**
	 * Returns if this session object is still valid
	 */
	public boolean isValid();
	
	/**
	 * May return a session token as Single sign-on information
	 */
	public String getSessionToken();


}
