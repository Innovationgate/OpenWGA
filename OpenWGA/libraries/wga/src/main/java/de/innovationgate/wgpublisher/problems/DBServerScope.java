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

package de.innovationgate.wgpublisher.problems;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;

import de.innovationgate.webgate.api.modules.servers.DatabaseServerProperties;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.config.DatabaseServer;

@Element
public class DBServerScope implements ProblemScope, MessageVariableProvider {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    @Attribute(name="serverid")
    private String _serverId;
    private String _serverName;
    
    public DBServerScope(DatabaseServer serverCfg) {
        _serverId = serverCfg.getUid();
        _serverName = serverCfg.getTitle();
    }
    
    public DBServerScope(WGDatabaseServer server) {
        _serverId = server.getUid();
        _serverName = server.getTitle(Locale.getDefault());
    }
    
    public DBServerScope(DatabaseServerProperties props) {
        _serverId = props.getSingletonUID();
        _serverName = props.getSingletonTitle(Locale.getDefault());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_serverId == null) ? 0 : _serverId.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        DBServerScope other = (DBServerScope) obj;
        if (_serverId == null) {
            if (other._serverId != null)
                return false;
        }
        else if (!_serverId.equals(other._serverId))
            return false;
        return true;
    }
    
    @Override
    public String toString() {
        return "Database server: " + _serverId;
    }
    
    @Override
    public Problem.Vars getDefaultMessageVariables() {
        return Problem
                .var("serverid", _serverId)
                .var("servername", _serverName);
    }

    public String getServerId() {
        return _serverId;
    }
    

}
