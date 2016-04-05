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

package de.innovationgate.wgpublisher.cluster.tasks;

public class ClearFailedLoginAttemptsTask extends ClusterTask<Boolean> {

    private static final long serialVersionUID = 1L;
    
    private boolean _clearAll = false;
    
    private String _domain;
    private String _user;

    /**
     * without further parameters all logins will be cleared
     */
    public ClearFailedLoginAttemptsTask() {
        _clearAll = true;
    }
    
    /**
     * clear only specified login
     * @param domain
     * @param user
     */
    public ClearFailedLoginAttemptsTask(String domain, String user) {
        _domain = domain;
        _user = user;
    }

    @Override
    public Boolean execute() throws Exception {
        if (!_clearAll) {
            getContext().getWGACore().getBruteForceLoginBlocker().clearFailedLoginAttempts(_domain, _user, false);
        } else {
            getContext().getWGACore().getBruteForceLoginBlocker().clearAllFailedLoginAttempts(false);
        }
        return true;
    }

}
