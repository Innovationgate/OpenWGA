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

import java.util.TreeMap;

/**
 * Represents a Map of WGArea-Objects, where the mapping key for each area is its name.
 */
public class WGAreaMap extends TreeMap<String,WGArea> {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
	 * Constructor. Should not be called outside the WGAPI.
	 */
	public WGAreaMap(java.util.SortedMap<String,WGArea> map) {
		super(map);
		}

	/**
	 * Returns the area with the given name
	 * @param strArea Area name
	 * @return The area of this name, null if none exists under this name.
	 */
	public WGArea get(String strArea) {
		
		if (this.containsKey(strArea))
			return (WGArea) this.get(strArea);
		else
			return null;
		
	}
	
	/**
	 * Returns an iterator object, iterating over all WGArea-Objects in this map
	 * @return Iterator
	 */
	public java.util.Iterator<WGArea> iterator() {
		return this.values().iterator();
	}
}

