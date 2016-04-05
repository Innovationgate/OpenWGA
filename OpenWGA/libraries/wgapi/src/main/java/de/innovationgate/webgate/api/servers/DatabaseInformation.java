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
package de.innovationgate.webgate.api.servers;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * Data about a database on a database server, used to present this database to the administrator and to connect to it
 */
public class DatabaseInformation {
    
    private Class<? extends WGDatabaseCore> _implementationClass;
    private Map<String,String> _options = new HashMap<String, String>();
    
    
    /**
     * Returns the path options needed to connect to this database
     */
    public Map<String, String> getOptions() {
        return _options;
    }

    private String _title;
    
    private String _location;
    
    /**
     * Constructor
     * @param implClass The implementation class of {@link WGDatabaseCore} that will be used to connect this database
     */
    public DatabaseInformation(Class <? extends WGDatabaseCore> implClass) {
        _implementationClass = implClass;
    }

    /**
     * Returns the implementation class of {@link WGDatabaseCore} that will be used to connect this database
     */
    public Class<? extends WGDatabaseCore> getImplementationClass() {
        return _implementationClass;
    }

    /**
     * Returns the display title
     */
    public String getTitle() {
        return _title;
    }

    /**
     * Sets the display title
     */
    public void setTitle(String title) {
        _title = title;
    }

    /**
     * Returns a descriptive location of this database to display to the administrator
     */
    public String getLocation() {
        return _location;
    }

    /**
     * Sets a descriptive location of this database to display to the administrator
     * @param location
     */
    public void setLocation(String location) {
        _location = location;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_implementationClass == null) ? 0 : _implementationClass.hashCode());
        result = prime * result + ((_options == null) ? 0 : _options.hashCode());
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
        DatabaseInformation other = (DatabaseInformation) obj;
        if (_implementationClass == null) {
            if (other._implementationClass != null)
                return false;
        }
        else if (!_implementationClass.equals(other._implementationClass))
            return false;
        if (_options == null) {
            if (other._options != null)
                return false;
        }
        else if (!_options.equals(other._options))
            return false;
        return true;
    }

}
