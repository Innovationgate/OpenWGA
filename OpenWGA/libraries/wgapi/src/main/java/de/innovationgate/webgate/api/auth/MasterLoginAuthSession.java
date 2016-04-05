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

import java.util.HashSet;
import java.util.Set;

import de.innovationgate.webgate.api.WGDatabase;

/**
 * Auth session object representing an anonymous login.
 */
public class MasterLoginAuthSession implements AuthenticationSession {
	
	private static MasterLoginAuthSession _inst = new MasterLoginAuthSession();

	private MasterLoginAuthSession() {
	    userNames.add(WGDatabase.MASTER_USERNAME);
	}
	
	/**
	 * Singleton retriever for this object.
	 */
	public static MasterLoginAuthSession getInstance() {
		return _inst;
	}
	
	private Set userNames = new HashSet();
	private Set userGroups = new HashSet();

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getDistinguishedName()
	 */
	public String getDistinguishedName() {
		return WGDatabase.MASTER_USERNAME;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getMailAddress()
	 */
	public String getMailAddress() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getNames()
	 */
	public Set getNames() {
		return userNames;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#getGroups()
	 */
	public Set getGroups() {
		return userGroups;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#logout()
	 */
	public void logout() {}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.auth.AuthenticationSession#isValid()
	 */
	public boolean isValid() {
		return true;
	}

    public String getSessionToken() {
        return null;
    }

}
