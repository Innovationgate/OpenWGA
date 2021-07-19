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

import de.innovationgate.webgate.api.WGDatabase.AccessLevel;

/**
 * Represents basic user access information
 */
public class WGUserAccess {

    private static final long USERACCESS_LIFETIME = 1000 * 60 * 5;
    private int _accessLevel;
    private long _creationTime;
    private long _outdatedTime;
    private WGACLEntryFlags _flags;
    /**
     * Public constructor, taking all information
     * @param userName Distinguished name of the user
     * @param accessLevel Access level of the user
     * @param flags Optional ACL flags for the user
     * @throws WGIllegalDataException
     */
    public WGUserAccess(String userName, int accessLevel, WGACLEntryFlags flags) throws WGIllegalDataException {
        _primaryName = userName;
        _creationTime = System.currentTimeMillis();
        _outdatedTime = _creationTime + USERACCESS_LIFETIME;
        _flags = flags;
        
        AccessLevel level = WGDatabase.ACCESSLEVELS.get(accessLevel);
        if (level == null) {
            throw new WGIllegalDataException("Invalid access level: " + accessLevel);
        }
        
        _accessLevel = level.getCode();
        _allowMovingStructEntries = level.isAllowMovingStructEntriesDefault();
        _allowDeletingDocuments = level.isAllowDeletingDocumentsDefault();
        _allowDirectAccess = level.isAllowDirectAccessDefault();
        
        if (flags != null) {
            _allowMovingStructEntries = flags.isMayMoveStructs();
            _allowDeletingDocuments = flags.isMayDeleteDocs();
            _allowDirectAccess = flags.isMayAccessDirectly();
        }
    }
    
    /**
     * Public constructor for user access that is not based on an ACL entry
     * @param userName  Distinguished name of the user
     * @param accessLevel Access level of the user 
     * @throws WGIllegalDataException
     */
    public WGUserAccess(String userName, int accessLevel) throws WGIllegalDataException {
        this(userName, accessLevel, null);
    }
    
    private boolean _allowMovingStructEntries = false;
    private boolean _allowDeletingDocuments = false;
    private boolean _allowDirectAccess = false;
    private String _primaryName;
    
    /**
     * Returns if user has privilege to delete documents
     */
    public boolean mayDeleteDocuments() {
        return _allowDeletingDocuments;
    }
    
    /**
     * Returns if user has privilege to move struct entries
     */
    public boolean mayMoveStructEntries() {
        return _allowMovingStructEntries;
    }
    
    /**
     * Returns if user has privilege to access the application directly, f.e. via URL
     */
    public boolean mayAccessDirectly() {
        return _allowDirectAccess;
    }
    
    /**
     * Returns the accessLevel.
     */
    public int getAccessLevel() {
        return _accessLevel;
    }

    /**
     * Determines if this object has expired
     */
    public boolean isOutdated() {
        return System.currentTimeMillis() > _outdatedTime;
    }

    /**
     * Returns the primary name of the user, which should be full qualified and unique
     */
    public String getPrimaryName() {
    	return _primaryName;
    }
    void setPrimaryName(String name) {
    	_primaryName = name;
    }

    protected WGUserAccess applyUserAccessFilter(UserAccessFilter filter) throws WGAPIException {

        int maxLevel =  filter.getMaximumAccessLevel(this);
        WGUserAccess userAccess = new WGUserAccess(_primaryName, (_accessLevel < maxLevel ? _accessLevel : maxLevel), _flags);
        validateFilterPrivileges(filter, userAccess, this);
        return userAccess;
        
    }

    protected static void validateFilterPrivileges(UserAccessFilter filter, WGUserAccess userAccess, WGUserAccess originalUserAccess) {
        if (originalUserAccess._allowDeletingDocuments) {
            userAccess._allowDeletingDocuments = filter.validatePrivilege(originalUserAccess, Privilege.DELETE_DOCUMENTS);
        }
        if (originalUserAccess._allowDirectAccess) {
            userAccess._allowDirectAccess = filter.validatePrivilege(originalUserAccess, Privilege.DIRECT_ACCESS);
        }
        if (originalUserAccess._allowMovingStructEntries) {
            userAccess._allowMovingStructEntries = filter.validatePrivilege(originalUserAccess, Privilege.MOVE_PAGES);
        }
    }

    protected WGACLEntryFlags getFlags() {
        return _flags;
    }
    

    
}
