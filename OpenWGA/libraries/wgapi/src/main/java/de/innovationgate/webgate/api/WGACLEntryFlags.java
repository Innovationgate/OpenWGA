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
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase.AccessLevel;
import static de.innovationgate.webgate.api.Privilege.*;

/**
 * Class to parse the flags that are stored at each WGACLEntry and provide their data.
 * Flags of WGACLEntries store additional data about the entry and are stored in string form.
 * Parse via {@link #parse(String)}. Serialize to string for by {@link #toString()}.
 */
public class WGACLEntryFlags {
    
    public class Flag {
        
        private String _type;
        private String _value;

        public Flag(String flag) {
            
            int equalPos = flag.indexOf("=");
            if (equalPos != -1) {
                _type = flag.substring(0, equalPos);
                _value = flag.substring(equalPos + 1);
            }
            else {
                _type = flag;
                _value = "";
            }
            
        }
        
        public Flag(String type, String value) {
            _type = type;
            _value = value;
        }

        /**
         * @return Returns the type.
         */
        public String getType() {
            return _type;
        }

        /**
         * @return Returns the value.
         */
        public String getValue() {
            return _value;
        }

        /* (non-Javadoc)
         * @see java.lang.Object#toString()
         */
        public String toString() {
            return _type + "=" + _value;
        }
        
    }

    public static final String TYPE_ROLE = "r";
    public static final String TYPE_MOVESTRUCTS = MOVE_PAGES.toString();
    public static final String TYPE_DELETEDOCS = DELETE_DOCUMENTS.toString();
    public static final String TYPE_NOROLEINHERITANCE = NO_ROLE_INHERITANCE.toString();
    public static final String TYPE_DIRECTACCESS = DIRECT_ACCESS.toString();
    
    private List<String> _roles = new ArrayList<String>();
    private boolean _mayDeleteDocs = false;
    private boolean _mayMoveStructs = false;
    private boolean _noRoleInheritance = false;
    private boolean _mayAccessDirectly = false;


    public void parse(String flags) {
        if (flags == null || flags.trim().equals("")) {
            return;
        }
        
        try {
            Iterator flagsIt = WGUtils.deserializeCollection(flags,",", true).iterator();
            Flag flag;
            while (flagsIt.hasNext()) {
                flag = new Flag((String) flagsIt.next());
                if (flag.getType().equals(TYPE_ROLE)) {
                    _roles.add(flag.getValue());
                }
                else if (flag.getType().equals(TYPE_DELETEDOCS)) {
                    _mayDeleteDocs = Boolean.valueOf(flag.getValue()).booleanValue();
                }
                else if (flag.getType().equals(TYPE_MOVESTRUCTS)) {
                    _mayMoveStructs = Boolean.valueOf(flag.getValue()).booleanValue();
                }
                else if (flag.getType().equals(TYPE_NOROLEINHERITANCE)) {
                    _noRoleInheritance = Boolean.valueOf(flag.getValue()).booleanValue();
                }
                else if (flag.getType().equals(TYPE_DIRECTACCESS)) {
                    _mayAccessDirectly = Boolean.valueOf(flag.getValue()).booleanValue();
                }
                else {
                    WGFactory.getLogger().warn("Unknown ACL flag type: " + flag.getType());
                }
            }
        }
        catch (Exception e) {
            WGFactory.getLogger().error("Error parsing ACL flags", e);
        }
    }
    
    
    /**
     * Creates a flags object, having all settings on the default settings for the given level
     * @param level The access level whose defaults should determine the initial settings for these flags
     */
    public WGACLEntryFlags(AccessLevel level) {
        if (level != null) {
            setMayMoveStructs(level.isAllowMovingStructEntriesDefault());
            setMayDeleteDocs(level.isAllowDeletingDocumentsDefault());
            setMayAccessDirectly(level.isAllowDirectAccessDefault());
        }
    }
    




    /**
     * Returns the user roles (Strings) that are stored in this flags object. This list can be modified to add and remove roles.
     */
    public List<String> getRoles() {
        return _roles;
    }

    /**
     * Returns the flag-string representation of the flags stored in this object.
     */
    public String toString() {
        
        List flags = new ArrayList();
        Iterator roles = _roles.iterator();
        while (roles.hasNext()) {
            flags.add(new Flag(TYPE_ROLE, (String) roles.next()));
        }
        
        flags.add(new Flag(TYPE_DELETEDOCS, new Boolean(_mayDeleteDocs).toString()));
        flags.add(new Flag(TYPE_MOVESTRUCTS, new Boolean(_mayMoveStructs).toString()));
        flags.add(new Flag(TYPE_NOROLEINHERITANCE, new Boolean(_noRoleInheritance).toString()));
        flags.add(new Flag(TYPE_DIRECTACCESS, new Boolean(_mayAccessDirectly).toString()));
        
        return WGUtils.serializeCollection(flags, ",");
        
        
    }


    /**
     * Returns if privilege to delete docs is set
     */
    public boolean isMayDeleteDocs() {
        return _mayDeleteDocs;
    }


    /**
     * Returns if privilege to move struct entries is set
     */
    public boolean isMayMoveStructs() {
        return _mayMoveStructs;
    }


    /**
     * Sets privilege to delete docs
     */
    public void setMayDeleteDocs(boolean mayDeleteDocs) {
        _mayDeleteDocs = mayDeleteDocs;
    }


    /**
     * Sets privilege to move struct entries
     */
    public void setMayMoveStructs(boolean mayMoveStructs) {
        _mayMoveStructs = mayMoveStructs;
    }


    /**
     * Returns flag if a user inherits roles from less specific ACL entries
     */
    public boolean isNoRoleInheritance() {
        return _noRoleInheritance;
    }


    /**
     * Sets flag if a user inherits roles from less specific ACL entries
     */
    public void setNoRoleInheritance(boolean noRoleInheritance) {
        _noRoleInheritance = noRoleInheritance;
    }


    /**
     * Returns access privilege if a user may use the application directly
     */
    public boolean isMayAccessDirectly() {
        return _mayAccessDirectly;
    }

    /**
     * Sets access privilege if a user may use the application directly
     */
    public void setMayAccessDirectly(boolean urlAccess) {
        _mayAccessDirectly = urlAccess;
    }


    /**
     * Loads all enabled flags from the given entry flags object
     */
    public void mergeFlags(WGACLEntryFlags flags) {

        List<String> newRoles = new ArrayList<String>(flags.getRoles());
        newRoles.removeAll(_roles);
        _roles.addAll(newRoles);
        
        _mayAccessDirectly = _mayAccessDirectly || flags.isMayAccessDirectly();
        _mayDeleteDocs = _mayDeleteDocs || flags.isMayDeleteDocs();
        _mayMoveStructs = _mayMoveStructs || flags.isMayMoveStructs();
        _noRoleInheritance = _noRoleInheritance || flags.isNoRoleInheritance();
        
    }
    
}
