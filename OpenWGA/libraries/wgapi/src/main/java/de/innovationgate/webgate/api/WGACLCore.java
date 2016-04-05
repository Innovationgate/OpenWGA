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

import java.util.List;

/**
 * Interface that is implemented by concrete WGACL implementations.
 * 
 */
public interface WGACLCore {

	/**
	 * Returns a list of all entries of this ACL (of type WGACLEntry)
	 * @throws WGBackendException 
	 */
	List<? extends WGACLEntry> getAllEntries() throws WGAPIException;
	/**
	 * Returns an ACL entry of the given user/group/role name
	 * @param name The user/group/role to retrieve an ACL entry for
	 * @return The entry or null if none exists of that name
	 * @throws WGBackendException 
	 */
	WGACLEntry getEntry(String name) throws WGAPIException;
	/**
	 * Create a new ACL entry.
	 * @param name Name of user
	 * @param type Name of entry type, use constants WGACL.TYPE_...
	 * @param accessLevel
	 * @throws WGCreationException
	 * @throws WGAPIException 
	 */
	WGACLEntry createEntry(String name, int type, int accessLevel) throws WGAPIException;
	/**
	 * Removes an ACL entry
	 * @param entry The entry to remove
	 * @throws WGBackendException
	 */
	void remove(WGACLEntry entry) throws WGAPIException;
	/**
	 * Saves/updates an ACL entry
	 * @param entry The entry to save
	 * @throws WGBackendException
	 */
	void save(WGACLEntry entry) throws WGAPIException;
	

}
