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

import java.util.Collections;

import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

/**
 * performs a reconnect on the given database key
 */
public class ReconnectDatabaseTask extends ClusterTask<Boolean> {

    private static final long serialVersionUID = 1L;
    
    private String _dbKey = null;
    
    public ReconnectDatabaseTask(String dbkey) {
        _dbKey = dbkey;
    }

    @Override
    public Boolean execute() throws Exception {
        WGACore core = getContext().getWGACore();
        core.getLog().info("Perfoming reconnect of database '" + _dbKey + "' due to cluster request from '"  + getContext().getCaller() + "'");
        if (WGA.get(core).database(_dbKey) != null) {
            WGA.get(core).server().reconnect(Collections.singletonList(_dbKey));
            return true;
        } else {
            throw new IllegalArgumentException("Database '" + _dbKey + "' not found.");
        }
    }

}
