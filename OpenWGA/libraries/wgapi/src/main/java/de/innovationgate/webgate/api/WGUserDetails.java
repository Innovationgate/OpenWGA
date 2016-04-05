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
package de.innovationgate.webgate.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Detail information about a logged in WGA user, consolidating information from an auth session and 
 * - His primary name (which should be identical to what the WGSessionContext returns as user name)
 * - His known name aliases/variants
 * - His user groups
 * - His roles
 * - His e mail address (for workflow)
 * 
 * Not every database implementation supports creating this detail information.
 */
public class WGUserDetails extends WGUserAccess {

	private String _eMailAddress;
	private List<String> _aliases = new ArrayList<String>();
	private List<String> _groups = new ArrayList<String>();
	private List<String> _roles = new ArrayList<String>();
	private List<String> _matchingEntries = new ArrayList<String>();
	private Map<String,String> _labeledNames = new HashMap<String,String>();
    private String _authSource;

    /**
	 * Constructor. Should not be used outside WGAPI.
	 * @param primaryName
	 * @param eMailAddress
	 * @param aliases
	 * @param groups
	 * @param roles
	 */
	public WGUserDetails(int accessLevel, String primaryName, String eMailAddress, String authSource, Collection<String> aliases, Collection<String> groups, Collection<String> roles, List<String> matchingEntries, WGACLEntryFlags flags) throws WGIllegalDataException {
	    super(primaryName, accessLevel, flags);
        _authSource = authSource;
        _eMailAddress = eMailAddress;
				
		if (aliases != null) {
			_aliases.addAll(aliases);
		}
		
		if (groups != null) {
			_groups.addAll(groups);
		}
		
		if (roles != null) {
			_roles.addAll(roles);
		}
		
		if (matchingEntries != null) {
		    _matchingEntries.addAll(matchingEntries);
		}

	}

	

	/**
	 * Returns the name aliases/variants of the user
	 */
	public List<String> getAliases() {
		return _aliases;
	}

	/**
	 * Returns the e-mail address of the user, as far as it is known
	 */
	public String getEMailAddress() {
		return _eMailAddress;
	}

	/**
	 * Returns the user group that the current user is member of 
	 */
	public List<String> getGroups() {
		return _groups;
	}

	/**
	 * Returns the roles of the user.
	 */
	public List<String> getRoles() {
		return _roles;
	}



    /**
     * Returns a description of the authentication source that was used to gather the user details
     */
    public String getAuthSource() {
        return _authSource;
    }




    
    /**
     * Returns the names of ACL entries that matched the current user in authentication
     * This only works with WGA Content Store for JDBC
     */
    public List<String> getMatchingEntries() {
        return _matchingEntries;
    }



    /**
     * Returns the labeled names that are provided by the authentication session. These are additional informations about the user which are given a lookup name.
     */
    public Map<String,String> getLabeledNames() {
        return _labeledNames;
    }



    protected void setLabeledNames(Map<String,String> labeledNames) {
        _labeledNames = labeledNames;
    }
    
    @Override
    protected WGUserAccess applyUserAccessFilter(UserAccessFilter filter) throws WGAPIException {

        List<String> filteredAliases = new ArrayList<String>();
        for (String alias : _aliases) {
            if (filter.validateAlias(this, alias)) {
                filteredAliases.add(alias);
            }
        }
        
        List<String> filteredRoles = new ArrayList<String>();
        for (String role : _roles) {
            if (filter.validateRole(this, role)) {
                filteredRoles.add(role);
            }
        }
        
        List<String> filteredGroups = new ArrayList<String>();
        for (String group : _groups) {
            if (filter.validateGroup(this, group)) {
                filteredGroups.add(group);
            }
        }
        
        int maxLevel =  filter.getMaximumAccessLevel(this);
        WGUserDetails userDetails = new WGUserDetails((getAccessLevel() < maxLevel ? getAccessLevel() : maxLevel), getPrimaryName(), _eMailAddress,_authSource, filteredAliases, filteredGroups, filteredRoles, _matchingEntries, getFlags());
        userDetails.setLabeledNames(_labeledNames);
        validateFilterPrivileges(filter, userDetails, this);
        return userDetails;
        
    }

}
