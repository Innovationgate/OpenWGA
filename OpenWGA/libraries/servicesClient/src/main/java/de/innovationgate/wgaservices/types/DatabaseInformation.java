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

package de.innovationgate.wgaservices.types;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Information about a connected database
 */
public class DatabaseInformation {
    
    private String _implementationClass;
    
    private Map<String,String> _options = new HashMap<String, String>();

    /**
     * Sets database options
     */
    public void setOptions(Map<String,String> options) {
        _options = options;
    }

    /**
     * Returns database options
     */
    public Map<String,String> getOptions() {
        return _options;
    }

    /**
     * Sets the implementation class of the database core
     */
    public void setImplementationClass(String implementationClass) {
        _implementationClass = implementationClass;
    }

    /**
     * Returns the implementation class of the database core
     */
    public String getImplementationClass() {
        return _implementationClass;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DatabaseInformation) {
            DatabaseInformation other = (DatabaseInformation) obj;
            EqualsBuilder builder = new EqualsBuilder();
            builder.append(getImplementationClass(), other.getImplementationClass());
            builder.append(getOptions(), other.getOptions());
            return builder.isEquals();
        } else {
            return super.equals(obj);
        }
        
    }

    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(getImplementationClass());
        builder.append(getOptions());
        return builder.toHashCode();
    }

    

}
