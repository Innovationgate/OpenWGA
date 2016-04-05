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

package de.innovationgate.wga.server.api.tml;

import java.util.List;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.CodeCompletion;

/**
 *  This object represents the personalisation profile of the current browser user.
 *  This object is the same as the TMLUserProfile object in TMLScript. Documentation on this object and its methods in more detail can therefor be found on the TMLScript reference of the OpenWGA documenation library of the respective OpenWGA version for Object "TMLUserProfile".
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public interface UserProfile {

    /**
     * Offers the WGAPI object representing the personalisation profile
     */
    @CodeCompletion(preferredCase="profile",isProperty=true)
    public abstract WGUserProfile getprofile();

    /**
     * Tests if an item is available
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="hasItem")
    public abstract boolean hasitem(String name) throws WGAPIException;

    /**
     *  Returns if the profile already was allowed to be stored persistently
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="storageOptedIn",isProperty=true)
    public abstract boolean isstorageoptedin() throws WGAPIException;

    /**
     * Reads the value of a profile item as single value
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Object item(String name) throws WGAPIException;

    /**
     * Returns the value of a profile item as list value 
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="itemList")
    public abstract List<Object> itemlist(String name) throws WGAPIException;

    /**
     * Reads the value of a metadata field as single value
     * @param name Name of the field
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Object meta(String name) throws WGAPIException;

    /**
     * Reads the value of a metadata field as list value
     * @param name Name of the field
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="metaList")
    public abstract List<Object> metalist(String name) throws WGAPIException;

    /**
     * Marks this profile as being allowed for persistent storage
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="optInStorage")
    public abstract boolean optinstorage() throws WGAPIException;

    /**
     * Removes an item from the profile
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="removeItem")
    public abstract void removeitem(String name) throws WGAPIException;

    /**
     * Creates or updates a profile item
     * @param name Name of the item
     * @param value Value of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="setItem")
    public abstract boolean setitem(String name, Object value) throws WGAPIException;

}