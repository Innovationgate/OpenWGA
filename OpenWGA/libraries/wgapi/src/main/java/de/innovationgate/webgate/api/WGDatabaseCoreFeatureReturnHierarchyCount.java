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

/**
 * Feature interface for {@link WGDatabaseCore} for performant retrieval of struct entry count (without actually retrieving the struct data) at certain hierarchy positions
 */
public interface WGDatabaseCoreFeatureReturnHierarchyCount extends WGDatabaseCore {
    
    /**
     * Retrieves the number of child entries of the given struct entry without actually retrieving them
     * @param structEntry The struct entry, whose children are to be retrieved
     * @return An iterator of all child entries, ordered by their intended default order
     * @throws WGAPIException 
     */
    public int getChildEntryCount(WGStructEntry structEntry) throws WGAPIException;
    /**
     * Retrieves the root entries (i.e. entries without parents) of the given area.
     * @param area The area, whose root entries are to be retrieved
     * @return An iterator of root entries in their intended default order
     * @throws WGAPIException 
     */
    public int getRootEntryCount(WGArea area) throws WGAPIException;

}
