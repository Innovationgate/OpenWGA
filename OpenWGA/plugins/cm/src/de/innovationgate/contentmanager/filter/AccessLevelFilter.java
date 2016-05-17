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

package de.innovationgate.contentmanager.filter;

import de.innovationgate.webgate.api.Privilege;
import de.innovationgate.webgate.api.UserAccessFilter;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGUserAccess;

public class AccessLevelFilter implements UserAccessFilter {
    
    private int _level;

    public AccessLevelFilter(int level) {
        _level = level;
    }

    public int getMaximumAccessLevel(WGUserAccess userAccess) {
        return _level;
    }

    public boolean validateGroup(WGUserAccess userAccess, String group) {
        return true;
    }

    public boolean validatePrivilege(WGUserAccess userAccess, Privilege privilege) {
        return true;
    }

    public boolean validateRole(WGUserAccess userAccess, String role) {
        return true;
    }

    public boolean validateAlias(WGUserAccess userAccess, String alias) {
        return true;
    }
    
   

}
