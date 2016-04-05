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

@Element
public class PluginScope implements ProblemScope {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    @Attribute(name="dbkey")
    private String _pluginName;
    
    public PluginScope(String pluginName) {
        _pluginName = pluginName;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_pluginName == null) ? 0 : _pluginName.hashCode());
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
        PluginScope other = (PluginScope) obj;
        if (_pluginName == null) {
            if (other._pluginName != null)
                return false;
        }
        else if (!_pluginName.equals(other._pluginName))
            return false;
        return true;
    }
    
    @Override
    public String toString() {
        return "Plugin: " + _pluginName;
    }

    public String getPluginName() {
        return _pluginName;
    }

}
