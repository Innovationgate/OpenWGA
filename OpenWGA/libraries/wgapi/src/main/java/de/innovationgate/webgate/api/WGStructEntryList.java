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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * A list of struct entries.
 */
public class WGStructEntryList extends LinkedHashSet<WGStructEntry> {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private WGStructEntry[] entriesByIndex = null;
	private HashMap<Number,WGStructEntry> entriesByPosition = null;
    private HashMap<Object,Integer> entryIndexByKey = null;
	
	private WGStructEntryList(List<WGStructEntry> list) {
		super(list);
	}
	
	/**
	 * Create a WGStructEntryList from an ordinary java.util.List filled with WGStructEntry objects
	 * @param list
	 */
	public static WGStructEntryList create(List<WGStructEntry> list) {
		
		return new WGStructEntryList(removeNull(list));
		
	}
	
	/**
	 * Creates an empty struct entry list
	 */
	public static WGStructEntryList create() {
	    return new WGStructEntryList(new ArrayList<WGStructEntry>());
	}
	
	private static List<WGStructEntry> removeNull(List<WGStructEntry> list) {
		
		while (list.contains(null)) {
			list.remove(null);
		}
		return list;
		
	}
	
	private void fillKeyMapAndIndexArray() throws WGAPIException {
		this.entriesByIndex = new WGStructEntry[this.size()];		
        this.entryIndexByKey = new HashMap<Object,Integer>();
		java.util.Iterator<WGStructEntry> elements = this.iterator();
		WGStructEntry entry;
		for(int idx=0; idx < this.size(); idx++) {
			entry = (WGStructEntry) elements.next();
			this.entriesByIndex[idx] = entry;
            this.entryIndexByKey.put(entry.getStructKey(), new Integer(idx));
		}
	}
	
	private void fillPositionMap() throws WGAPIException {
		this.entriesByPosition = new HashMap<Number,WGStructEntry>();
		java.util.Iterator<WGStructEntry> elements = this.iterator();
		WGStructEntry entry;
		for(int idx=0; idx < this.size(); idx++) {
			entry = (WGStructEntry) elements.next();
			Number position = entry.getPosition();
			if (this.entriesByPosition.containsKey(position) == false) {
				this.entriesByPosition.put(position, entry);
			} 
		}
	}
 	
	/**
	 * Retrieves a struct entry by it's position number.
	 * If there are multiple entries with the same position number, the first in the siblings index is returned.
	 * @param pos
	 * @throws WGAPIException 
	 */
	public WGStructEntry getByPosition(double pos) throws WGAPIException {
		if (this.entriesByPosition == null) {
			this.fillPositionMap();
		}
		return (WGStructEntry) this.entriesByPosition.get(new Double(pos));
	}
	
	/**
	 * Retrieve a struct entry by it's index.
	 * The index is the position in the siblings list, that is ordered by the position number.
	 * @param idx
	 * @throws WGAPIException 
	 */
	public WGStructEntry getByIndex(int idx) throws WGAPIException {

		if (this.entriesByIndex == null) {
			this.fillKeyMapAndIndexArray();
		}
		
		if (idx >= 0 && idx < this.entriesByIndex.length) {
			return this.entriesByIndex[idx];
		}
		else {
			return null;
		}
		
	}
	
	/**
	 * Returns the siblings index of the given entry in the siblings list.
	 * @param testEntry
	 * @return The siblings index, -1 if the entry cannot be found in the list.
	 * @throws WGAPIException 
	 */
	public int getIndexOfEntry(WGStructEntry testEntry) throws WGAPIException {
		
		if (this.entriesByIndex == null) {
			this.fillKeyMapAndIndexArray();
		}
		
        /*
		WGStructEntry entry = null;
		for (int idx=0; idx < this.entriesByIndex.length; idx++) {
			if (this.entriesByIndex[idx].getStructKey().equals(testEntry.getStructKey())) {
				return idx;
			}
		}
		return -1;*/
        
        Integer index = (Integer) entryIndexByKey.get(testEntry.getStructKey());
        if (index != null) {
            return index.intValue();
        }
        else {
            return -1;
        }
	}
	
	/**
	 * Returns a basic {@link List} containing the struct entries in their order. The list is unmodifiable.
	 * @throws WGAPIException
	 */
	public List<WGStructEntry> asList() throws WGAPIException {
       if (this.entriesByIndex == null) {
            fillKeyMapAndIndexArray();
        }
       return Collections.unmodifiableList(Arrays.asList(entriesByIndex));
	}
	

	
}

