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

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * This is a mixin interface for {@link WGDatabaseCore} implementations that also provide storage of user profiles for personalisation.
 * WGDatabase wraps implementations of this interface and uses them to communicate with WGA personalisation databases for user profile stores.
 * WGDatabaseCore implementations should be "thread-safe", so they should expect that they will be concurrently used by multiple threads.
 */
public interface WGPersonalisationDatabaseCore extends WGDatabaseCore {

    /**
     * Retrieves a user profile for the given name
     * @param name name of the requested user profile
     * @return The user profile of that name. Null, if there is no user profile with this name.
     * @throws WGAPIException
     */
    public WGDocumentCore getUserProfile(String name) throws WGAPIException;
    /**
     * Called to create a new user profile. 
     * If parameter name is given, this param should be used to name this profile. If another profile of this name is already present, this method is intended to do nothing and return null.
     * @param name The name, that the caller wants for the new profile. If null, the implementation should create a name.
     * @param type The type, that the caller wants to assign the profile. Possible values chosen by personalisation implementation
     * @return The created user profile. null, if a name provided was already used for an existing profile. 
     * @throws WGAPIException 
     */
    public WGDocumentCore createUserProfile(String name, int type)  throws WGAPIException;
    
    /**
     * Queries a user profile database for stored profiles, based on their data. 
     * The query syntax is up to the profile database implementation.
     * @param type Type of query. WGAPI implementations may define different profile query types. 
     * @param query Query to use for searching user profiles
     * @param params Parameters for the query.
     * @return A list of profile names, whose profile data match the query
     * @throws WGQueryException 
     * @throws WGAPIException 
     */
    public List<String> queryUserProfileNames(String type, String query, Map params) throws WGAPIException;
    
    /**
     * Returns the names of all user profiles stored inthe database
     * @throws WGAPIException
     */
    public Iterator<String> getAllUserProfileNames() throws WGAPIException;
    
    
}
