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
package de.innovationgate.webgate.api.auth;

import java.util.List;
import java.util.Map;

/**
 * Information about a user or group, retrieved via {@link AuthenticationModule#query(Object, String)}
 */
public interface UserGroupInfo {
    
    /**
     * Returns a list of all possible user names that may represent the current user 
     */
    public List getAliasNames();
    
    /**
     * Gets the most distinguished for of user name for the current user
     */
    public String getFullQualifiedName();
    
    /**
     * Returns if the user is a group
     */
    public boolean isGroup();
    
    /**
     * Returns if the user is a plain single-person user
     */
    public boolean isUser();
    
    /**
     * return the value of the given attribute
     * if attribute value is multiple the first value is returned
     * @param name name of the attribute
     * @return attribute value as Object - concrete class depends on the backend authentication store, might be null
     */
    public Object getAttributeValue(String name);
    
    /**
     * returns all values of the given attribute
     * @param name name of the attribute
     * @return list of attribute values - concrete class of list values depends on the backend authentication store
     */
    public List getAttributeValueList(String name);
    
    
    /**
     * returns all attributes mapped by name, value is a list of all attribute values
     * @return Map (key=attributeName, value=list of attribute values)
     */
    public Map getAttributes();
     
}
