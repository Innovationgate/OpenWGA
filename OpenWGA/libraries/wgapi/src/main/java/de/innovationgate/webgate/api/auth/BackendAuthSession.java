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

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import de.innovationgate.webgate.api.WGDatabaseCore;

/**
 * An authentication session that needs to be authenticated in the WGAPI backend implementation.
 * This is given to {@link WGDatabaseCore#openSession(AuthenticationSession, Object, boolean)} when
 * the database has no authentication module configured.
 * The WGAPI implementation may add authentication data like groups, mail address etc. to this
 * object's data.
 */
public class BackendAuthSession implements AuthenticationSession {
    
    private String _userName;
    private boolean _valid = false;
    private Set _groups = new HashSet<String>();
    private Set _names = new HashSet<String>();
    private String _mailAddress = null;
    private String _sessionToken = null;

    public BackendAuthSession(String username, Object credentials) {
        _userName = username;
        _names.add(username);
    }

    public String getDistinguishedName() {
        return _userName;
    }

    public Set getGroups() {
        return _groups;
    }

    public String getMailAddress() {
        return _mailAddress;
    }

    public Set getNames() {
        return _names;
    }

    public String getSessionToken() {
        return _sessionToken;
    }

    public boolean isValid() {
        return _valid;
    }

    public void logout() {
    }

    /**
     * Set if this session is valid
     */
    public void setValid(boolean valid) {
        _valid = valid;
    }

    /**
     * Set the mail address of the session user
     */
    public void setMailAddress(String mailAddress) {
        _mailAddress = mailAddress;
    }

    /**
     * Set a session token of the session
     */
    public void setSessionToken(String sessionToken) {
        _sessionToken = sessionToken;
    }

}
