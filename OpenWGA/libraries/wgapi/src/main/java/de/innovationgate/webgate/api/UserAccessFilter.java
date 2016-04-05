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

import java.io.Serializable;

/**
 * A filter to reduce user access of logged in users. Implementations must be serializable.
 */
public interface UserAccessFilter extends Serializable {
    
    /**
     * Returns the maximum access level with this filter applied
     * @param userAccess The users original access rights
     */
    public int getMaximumAccessLevel(WGUserAccess userAccess);

    /**
     * Returns if the user may have this group with this filter applied
     * @param userAccess The users original access rights
     */
    public boolean validateGroup(WGUserAccess userAccess, String group);
    
    /**
     * Returns if the user may have this privilege with this filter applied
     * @param userAccess The users original access rights
     */
    public boolean validatePrivilege(WGUserAccess userAccess, Privilege privilege);
    
    /**
     * Returns if the user may have this role with this filter applied
     * @param userAccess The users original access rights
     */
    public boolean validateRole(WGUserAccess userAccess, String role);
    
    /**
     * Returns if the user may have this user name alias with this filter applied
     * @param userAccess The users original access rights
     */
    public boolean validateAlias(WGUserAccess userAccess, String alias);

}
