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
 * Interface for all entities that may hold extension data fields
 */
public interface WGExtensionDataContainer {
    
    /** 
     * Returns the value of an extension data field.
     * @param strName Name of the requested field.
     * @return Object
     * @throws WGAPIException  
     */
    public Object getExtensionData(String strName) throws WGAPIException;
    
    /** 
     * Sets the value of an extension data field.
     * @param strName Name of the requested field.
     * @param value The value of the field
     * @throws WGAPIException  
     */
    public void writeExtensionData(String strName, Object value) throws WGAPIException;
    
    /**
     * Returns the names of all extension data fields on this entity
     * @throws WGBackendException 
     */
    public List<String> getExtensionDataNames() throws WGAPIException;
    
    /** 
     * Removes an extension data field from this entity
     * @param strName Name of the field to remove.
     * @throws WGAPIException  
     */
    public void removeExtensionData(String strName) throws WGAPIException;
    
    

}
