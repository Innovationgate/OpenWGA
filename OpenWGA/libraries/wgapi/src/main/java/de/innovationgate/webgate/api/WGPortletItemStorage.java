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

package de.innovationgate.webgate.api;

import java.util.List;

/**
 * A service to store items for {@link WGTransientPortlet}s
 */
public interface WGPortletItemStorage {

    /**
     * Callback before the user profile is saved
     * @param profile
     * @throws WGAPIException
     */
    public void preSaveProfile(WGUserProfile profile) throws WGAPIException;

    /**
     * Returns all names of items stored for this portlet
     * @param portlet
     * @throws WGAPIException
     */
    public List<String> getItemNames(WGTransientPortlet portlet) throws WGAPIException;

    /**
     * Returns if an item of the given name is stored for this portlet
     * @param portlet
     * @param name
     * @throws WGAPIException
     */
    public boolean hasItem(WGTransientPortlet portlet, String name) throws WGAPIException;

    /**
     * Returns the value of a stored item for this portlet
     * @param portlet
     * @param name
     * @throws WGAPIException
     */
    public Object getItemValue(WGTransientPortlet portlet, String name) throws WGAPIException;

    /**
     * Sets stored item for this portlet
     * @param portlet
     * @param name
     * @param value
     * @throws WGAPIException
     */
    public void setItemValue(WGTransientPortlet portlet, String name, Object value) throws WGAPIException;

    /**
     * Removes a stored item from this portlet
     * @param portlet
     * @param name
     * @throws WGAPIException
     */
    public void removeItem(WGTransientPortlet portlet, String name) throws WGAPIException;

    /**
     * Callback after a new user profile is created
     * @param profile
     * @throws WGAPIException
     */
    public void prepareNewProfile(WGUserProfile profile) throws WGAPIException;

    /**
     * Pushes all persistent item storage data from all portlets to the given target storage 
     * @param targetStorage
     * @throws WGAPIException
     */
    public void pushData(WGPortletItemStorage targetStorage) throws WGAPIException;

    /**
     * Receives the item storage data for a single portlet and stores it to its own storage
     * @param p
     * @throws WGAPIException
     */
    public void receiveData(PortletItemStorageData p) throws WGAPIException;

}
