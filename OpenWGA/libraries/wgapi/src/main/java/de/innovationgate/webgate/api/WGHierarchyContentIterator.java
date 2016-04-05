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

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;

/**
 * An iterator over all released contents of one language at a certain hierarchy position.
 */
public class WGHierarchyContentIterator extends PrefetchingIterator<WGContent> implements SkippingIterator<WGContent>, CountReportingIterator<WGContent> {
    
    private WGStructEntryIterator _structs;
    private String _language;
    private boolean _fetched = false;

    public WGHierarchyContentIterator(WGStructEntryIterator structs, String language) {
        _structs = structs;
        _language = language;
    }

    /**
     * Returns the count of pages over which this iterator iterates and retrieves contents for.
     * The actual count of returned contents may be lower as some contents may not be readable or may not exist as released version in the given language
     */
    @Override
    public int getCount() {
        return _structs.getCount();
    }

    @Override
    public int skip(int nrOfElements) {
        if (_fetched) {
            throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
        }
        return _structs.skip(nrOfElements);
    }

    @Override
    protected WGContent fetchNextValue() {

        _fetched  = true;
        
        try {
            while (_structs.hasNext()) {
                
                WGStructEntry entry = _structs.next();
                WGContent content = entry.getReleasedContent(_language);
                if (content != null) {
                    return content;
                }
                
            }
            
            return null;
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception fetching next content", e);
        }
        
    }
    
    
    /**
     * Returns the offset of the last retrieved element inside the iterated collection
     * This also includes pages that were skipped because they have no released content in the current language
     */
    public int getCurrentOffset() {
        return _structs.getCurrentOffset();
    }

}
