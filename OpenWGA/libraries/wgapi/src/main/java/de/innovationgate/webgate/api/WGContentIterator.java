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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 * This class allows iterating over all contents stored in a content database.
 * It serves all {@link WGContent} objects in the following order:
 * <ul>
 *   <li>It serves the areas in the order they are offered by db.getAreas().values().
 *   <li>Inside the area it hierarchically goes through all the structentries and their content. 
 *   <li>Child struct entries are processed before the following siblings.
 * </ul>
 */
public class WGContentIterator implements Iterator<WGContent> {

    private WGDatabase _db;

    private List<WGContentKey> _contentKeys;

    private int _keyIndex = -1;

    private WGContent _next;
    
    private boolean _includeArchived;

    /**
     * Constructs an iterator for the given database
     * @param db The database 
     * @param includeArchived Choose true if you want archived documents included in the iteration, false otherwise
     * @throws WGAPIException 
     */
    public WGContentIterator(WGDatabase db, boolean includeArchived) throws WGAPIException {
        _db = db;
        _includeArchived = includeArchived;
        if (db.hasFeature(WGDatabase.FEATURE_RETRIEVE_ALL_CONTENTKEYS)) {
            _contentKeys = db.getAllContentKeys(includeArchived);
        }
        else {
            _contentKeys = fetchContentKeys(db);
        }
    }

    private List<WGContentKey> fetchContentKeys(WGDatabase db) throws WGAPIException {

        List<WGContentKey> contentKeys = new ArrayList<WGContentKey>();
        Iterator<WGArea> areas = db.getAreas().values().iterator();
        WGArea area;
        WGStructEntry entry;
        while (areas.hasNext()) {
            area = (WGArea) areas.next();
            Iterator<WGStructEntry> rootEntries = area.getRootEntries().iterator();
            while (rootEntries.hasNext()) {
                entry = (WGStructEntry) rootEntries.next();
                fetchContentKeys(entry, contentKeys);
            }
        }
        return contentKeys;

    } 

    /**
     * @param entry
     * @param contentKeys
     * @throws WGAPIException 
     */
    private void fetchContentKeys(WGStructEntry entry, List<WGContentKey> contentKeys) throws WGAPIException {

        Iterator<WGContent> contents = entry.getAllContent(_includeArchived).iterator();
        WGContent content;
        while (contents.hasNext()) {
            content = (WGContent) contents.next();
            contentKeys.add(content.getContentKey());
        }

        Iterator<WGStructEntry> children = entry.getChildEntries().iterator();
        WGStructEntry child;
        while (children.hasNext()) {
            child = (WGStructEntry) children.next();
            fetchContentKeys(child, contentKeys);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#hasNext()
     */
    public boolean hasNext() {

        try {
            return fetchNext();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Internal fetchNext() failed bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage() + ". Unable to continue iteration.");
        }
    }

    private boolean endOfListReached() {
        return (_contentKeys.size() < _keyIndex + 1);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#next()
     */
    public WGContent next() {

        try {
            if (fetchNext()) {
                WGContent next = _next;
                _next = null;
                return next;
            }
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Internal fetchNext() failed bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage() + ". Unable to continue iteration.");
        }
        
        throw new NoSuchElementException();
    }

    private boolean fetchNext() throws WGAPIException {

        while (_next == null) {
            _keyIndex++;
            if (endOfListReached()) {
                break;
            }
            _next = _db.getContentByKey((WGContentKey) _contentKeys.get(_keyIndex));
        }
        
        return (_next != null);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.util.Iterator#remove()
     */
    public void remove() {
        throw new UnsupportedOperationException();
    }

    /**
     * Returns all content keys of the contents offered by this iterator.
     */
    public List<WGContentKey> getContentKeys() {
        return Collections.unmodifiableList(_contentKeys);
    }
}
