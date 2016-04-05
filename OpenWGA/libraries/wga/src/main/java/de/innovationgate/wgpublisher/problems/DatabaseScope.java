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
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;

@Element
public class DatabaseScope implements ProblemScope, MessageVariableProvider {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    @Attribute(name="dbkey")
    private String _dbkey;
    
    public DatabaseScope(String dbkey) {
        _dbkey = dbkey;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_dbkey == null) ? 0 : _dbkey.hashCode());
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
        DatabaseScope other = (DatabaseScope) obj;
        if (_dbkey == null) {
            if (other._dbkey != null)
                return false;
        }
        else if (!_dbkey.equals(other._dbkey))
            return false;
        return true;
    }

    public String getDbkey() {
        return _dbkey;
    }
    
    @Override
    public String toString() {
        return "Database: " + _dbkey;
    }

    @Override
    public Problem.Vars getDefaultMessageVariables() {
        return Problem.var("dbkey", _dbkey);
    }

}
