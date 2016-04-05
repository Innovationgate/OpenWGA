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

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;

import de.innovationgate.utils.XStreamUtils;

@Element(name="problempath")
public class ProblemPath {

    @Attribute(name="type")
    private Class<? extends ProblemType> _type;
    
    private ProblemScope _scope;
    
    @Attribute(name="key")
    private String _key;
    
    public ProblemPath(Class<? extends ProblemType> type, ProblemScope scope, String key) {
        super();
        _type = type;
        _scope = scope;
        _key = key;
    }
    
    public ProblemScope getScope() {
        return _scope;
    }
    
    public Class<? extends ProblemType> getType() {
        return _type;
    }
    
    public String getKey() {
        return _key;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_key == null) ? 0 : _key.hashCode());
        result = prime * result + ((_scope == null) ? 0 : _scope.hashCode());
        result = prime * result + ((_type == null) ? 0 : _type.hashCode());
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
        ProblemPath other = (ProblemPath) obj;
        if (_key == null) {
            if (other._key != null)
                return false;
        }
        else if (!_key.equals(other._key))
            return false;
        if (_scope == null) {
            if (other._scope != null)
                return false;
        }
        else if (!_scope.equals(other._scope))
            return false;
        if (_type == null) {
            if (other._type != null)
                return false;
        }
        else if (!_type.equals(other._type))
            return false;
        return true;
    }
    

}
