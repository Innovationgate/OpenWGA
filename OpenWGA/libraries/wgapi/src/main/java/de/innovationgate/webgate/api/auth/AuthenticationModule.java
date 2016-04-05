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

import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGQueryException;

/**
 * Interface for a generic authentication module, that authenticates user logins and provides some additional information about logged in users
 * One authentication module is bound to a WGDatabase object.
 */
public interface AuthenticationModule {
    
    /**
     * Query type that queries for users and groups
     */
    public static final String QUERY_USERS_AND_GROUPS = "queryUsersAndGroups";
    
    /**
     * Query type fetching a specific user and group for a given distinguished name
     */
    public static final String QUERY_USER_DN = "userDn";
    
    /**
     * A predefined, yet optional labeled name denoting a display name for the user that is easy on the eye
     */
    public static final String USERLABEL_COMMONNAME = "commonname";

	/**
	 * Called when the Authentication module is created, providing additional information
	 * @param params Configuration parameters for the module
	 * @param db The db that this module is bound to
	 */
	public void init(Map<String,String> params, WGDatabase db) throws ConfigurationException;
	/**
	 * Called to verify a login. Returns a session object if it succeeds.
	 * @param user The user 
	 * @param credentials The users credentials, f.e. a password
	 * @return An authentication session object if the login succeeds. null otherwise.
	 * @throws AuthenticationException If the login cannot be verified
	 */
	public AuthenticationSession login(String user, Object credentials) throws AuthenticationException;
	/**
	 * Tries to retrieve the E-Mail-Address of the given user. 
	 * @param user The user
	 * @return The users E-Mail address
	 */
	public String getEMailAddress(String user);

	/**
	 * Called to notify the module to drop authentication caches (if it uses them)
	 */
	public void clearCache();
	/**
	 * Descriptive string representation of the used authentication source
	 */
	public String getAuthenticationSource();
    

    
    /**
     * Determines if this auth module can create a session token that can be used for some kind of
     * single sign-on architecture.
     */
    public boolean isGeneratesSessionToken();
    
    /**
     * Method to add a listener to authentication source events. These events may get fired by the auth module
     * when authentication data changes, so that listeners can clear their cache. Authentication modules are not
     * obliged to throw those events.
     */
    public void addAuthenticationSourceListener(AuthenticationSourceListener listener);
    
    /**
     * Removes an authentication source listener from the list of listeners
     */
    public void removeAuthenticationSourceListener(AuthenticationSourceListener listener);
    
    /**
     * Returns the classes that are allowed as credentials for this module
     */
    public Class<?>[] getAllowedCredentialClasses();
    
    /**
     * Determines, if this auth module supports queries of the given query type.
     */
    public boolean isQueryable(String queryType);
        
    /**
     * executes a query for authentication objects (users, groups etc.) and returns the result.
     * While there is no general contract for the form of the result it should consist of a {@link List} of {@link UserGroupInfo} objects,
     * so the result can be processed in a platform independent manner.
     * Use constants QUERY_... to specify query type.
     */
    public Object query(Object query, String queryType) throws WGQueryException;
    
    /**
     * Called when an auth module is no longer needed. Should return all resources
     */
    public void destroy();    
}
