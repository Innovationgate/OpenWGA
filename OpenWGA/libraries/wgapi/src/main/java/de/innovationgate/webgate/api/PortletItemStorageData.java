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

import java.util.Set;

/**
 * Interface to access the item storage data for one portlet for migration purposes 
 */
public interface PortletItemStorageData {

    /**
     * Returns the portlet key
     */
    public abstract String getKey();

    /**
     * Returns the portlet name
     */
    public abstract String getName();

    /**
     * Returns the application of the porltet
     */
    public abstract String getApplicationDb();
    
    /**
     * Returns the names of stored items
     */
    public Set<String> getItemNames();
    
    /**
     * Returns the value of a stored item
     * @param itemName Name of the item
     * @throws WGIllegalDataException
     * @throws WGSystemException
     */
    public Object getItemValue(String itemName) throws WGAPIException;

}