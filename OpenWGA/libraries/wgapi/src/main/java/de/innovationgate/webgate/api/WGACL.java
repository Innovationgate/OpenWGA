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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

import de.innovationgate.webgate.api.WGDatabase.AccessLevel;

/**
 * Represents the access control list of a database, that decides, which user has which rights.
 */
public class WGACL {
    
    /**
     * String containing all characters a role name may consist of
     */
    public static final String VALID_ROLENAME_CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$#[]_-";
	
	private WGACLCore _core;
	private WGDatabase _db;
	
	protected WGACL(WGDatabase db, WGACLCore core) {
		super();
		_core = core;
		_db = db;
	}
	
	/**
	 * Returns all user/group entries of this ACL.
	 * @throws WGAPIException 
	 */
	public Map getUsers() throws WGAPIException {
		
		Map users = new HashMap();
		Iterator entries = getAllEntries().iterator();
		WGACLEntry entry;
		while (entries.hasNext()) {
			entry = (WGACLEntry) entries.next();
			if (entry.getType() == WGACLEntry.TYPE_USER) {
				users.put(entry.getName(), normalize(entry));
			}
		}
		return users;
		
		
	}
	
	/**
	 * Returns all roles in this ACL.
	 * @throws WGAPIException 
	 */
	public Map getRoles() throws WGAPIException {
		Map users = new HashMap();
		Iterator entries = getAllEntries().iterator();
		WGACLEntry entry;
		while (entries.hasNext()) {
			entry = (WGACLEntry) entries.next();
			if (entry.getType() == WGACLEntry.TYPE_ROLE) {
				users.put(entry.getName(), normalize(entry));
			}
		}
		return users;
	}
	
	/**
	 * Returns all ACL entries in this ACL.
	 * @throws WGAPIException 
	 */
	public List getAllEntries() throws WGAPIException {
		List entries = _core.getAllEntries();
        Iterator entriesIt = entries.iterator();
		while (entriesIt.hasNext()) {
			WGACLEntry entry = (WGACLEntry) entriesIt.next();
			normalize(entry);
		}
		return entries;
	} 
	
	private WGACLEntry normalize(WGACLEntry entry) throws WGIllegalDataException {
	    
	    // Remove obsolete "designer" levels (#00001420)
		if (entry != null) {
		    AccessLevel level = WGDatabase.ACCESSLEVELS.get(entry.getLevel());
		    if (level == null) {
		        throw new WGIllegalDataException("Illegal access level in ACL entry '" + entry.getName() + "': " + entry.getLevel());
		    }
		    else if (level.getCode() != entry.getLevel()) {
		        entry.setLevel(level.getCode());
		    }
		}
		return entry;
	}

	/**
	 * Returns the entry for a given name
	 * @param name Name of user or group
	 * @return The ACL enty for this user or group, null if there is none of that name
	 * @throws WGAPIException 
	 */
	public WGACLEntry getEntry(String name) throws WGAPIException {
		return normalize(_core.getEntry(name));
	}
	/**
	 * Creates a new ACL entry, that defines the access right for a user, group or role. The new entry is immediately saved when it is returned.
	 * @param name Name of the user, group or role.
	 * @param type Type of entry. User constants WGACLEntry.TYPE_.... Not used yet since there are no roles till now.
	 * @param accessLevel The Access level for the user. Use constants WGDatabase.ACCESSLEVEL_....
	 * @return The newly created ACL entry
	 * @throws WGCreationException If something went wrong in the background creation process.
	 * @throws WGAuthorisationException If you are not authorized to create entries.
	 * @throws WGAPIException 
	 */
	public WGACLEntry createEntry(String name, int type, int accessLevel) throws WGAPIException, WGAuthorisationException, WGBackendException {
		
        // Test if this entry name exists
        WGACLEntry entry = getEntry(name);
        if (entry != null) {
            throw new WGDuplicateKeyException("An entry of name '" + name + "' already exists");
        }
        
		if (_db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
			throw new WGAuthorisationException("You are no manager of this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
		}
		
		AccessLevel level = WGDatabase.ACCESSLEVELS.get(accessLevel);
		if (level == null) {
		    throw new WGIllegalArgumentException("Unknown access level: " + accessLevel);
		}
		
		if (!level.isUsageInACL()) {
		    throw new WGIllegalArgumentException("Illegal access level for ACL usage: " + accessLevel);
		}
		
        if (type == WGACLEntry.TYPE_ROLE && !isValidRoleName(name)) {
            throw new WGIllegalDataException("The name '" + name + "' is no valid role name. Valid character for role names are a-z,A-Z,0-9,$,#,[ and ].");
        }
        
		return _core.createEntry(name, type, accessLevel);
		
	}
    
    /** 
     * Creates a new role entry in the ACL
     * @param name Name of the role. See {@link #isValidRoleName} for valid role names
     * @return The newly created role entry
     * @throws WGAuthorisationException
     * @throws WGAPIException
     * @throws WGAPIException
     */
    public WGACLEntry createRoleEntry(String name) throws WGAuthorisationException, WGBackendException, WGAPIException {
        return createEntry(name, WGACLEntry.TYPE_ROLE, 0);
    }
	
	/**
	 * Creates a new ACL entry, that defines the access right for a user or group.
	 * @param name Name of user or group
	 * @param accessLevel The Access level for the user. Use constants WGDatabase.ACCESSLEVEL_....
	 * @return The newly created ACL entry
	 * @throws WGCreationException If something went wrong in the background creation process.
	 * @throws WGAuthorisationException If you are not authorized to create entries.
	 * @throws WGAPIException 
	 */
	public WGACLEntry createUserEntry(String name, int accessLevel) throws WGAPIException {
		return createEntry(name, WGACLEntry.TYPE_USER, accessLevel);
	}
	/**
	 * Removes the given ACL entry from the ACL.
	 * @param entry The entry to remove
	 * @throws WGAuthorisationException If you are not authorized to do this
	 * @throws WGAPIException 
	 */
	public void remove(WGACLEntry entry) throws WGAPIException {
		
		if (_db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
			throw new WGAuthorisationException("You are no manager of this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
		}
        
        // Test if a role is still assigned
        if (entry.getType() == WGACLEntry.TYPE_ROLE) {
            List owners = getOwnersOfRole(entry);
            if (owners.size() > 0) {
                throw new WGIllegalStateException("The role is still owned by " + owners.size() + " users and therefor cannot be removed");
            }
        }
		
		 _core.remove(entry);
		
		
	}
	/**
	 * Saves the given ACL entry.
	 * @param entry The entry to save.
	 * @throws WGAuthorisationException If you are not authorized to do this
	 * @throws WGAPIException 
	 */
	public void save(WGACLEntry entry) throws WGAPIException {

		
		if (_db.getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
			throw new WGAuthorisationException("You are no manager of this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
		}
		
		_core.save(normalize(entry));
		 		
	}
    
    /**
     * Parses the given flags and returns a WGACLEntryFlags containing the parsed flag information
     * @param entry The entry whose flags are parsed
     * @throws WGIllegalDataException 
     */
    public WGACLEntryFlags parseFlags(WGACLEntry entry) throws WGIllegalDataException {

        // Retrieve level object
        AccessLevel level = WGDatabase.ACCESSLEVELS.get(entry.getLevel());
        if (level == null) {
            throw new WGIllegalDataException("Invalid access level: " + entry.getLevel());
        }
        else if (!level.isUsageInACL()) {
            throw new WGIllegalDataException("Access level " + entry.getLevel() + " may not be used in ACL");
        }
        
        // Parse flags
        WGACLEntryFlags flags = new WGACLEntryFlags(level);
        flags.parse(entry.getFlags());
        
        return flags;
        
    }
    
    /**
     * Determines if a name is a valid role name.
     * Valid role names contain only these characters:
     * a-z,A-Z,0-9,$,#,[,]
     * 
     * @param name The role name candidate
     * @return true, if the role name is valid, false if not
     */
    public static boolean isValidRoleName(String name) {
        
        return StringUtils.containsOnly(name, VALID_ROLENAME_CHARS);
        
    }
    
    /**
     * Gets all the acl entries that are members of a specific role.
     * @param roleEntry The entry of the role whose members are searched
     * @return WGACLEntry objects of members of the role
     * @throws WGAPIException 
     */
    public List getOwnersOfRole(WGACLEntry roleEntry) throws WGAPIException {
        
        List owners = new ArrayList();
        Iterator entries = getAllEntries().iterator();
        WGACLEntry entry;
        while (entries.hasNext()) {
            entry = (WGACLEntry) entries.next();
            WGACLEntryFlags flags = parseFlags(entry);
            if (flags.getRoles().contains(roleEntry.getName())) {
                owners.add(entry);
            }
        }
        
        return owners;
        
    }

}
