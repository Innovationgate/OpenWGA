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

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;

/**
 * Iterator to iterate over struct entries.
 * This iterator fetches struct entries page-wise, allowing performant usage with very many struct entries
 */
public class WGStructEntryIterator extends PrefetchingIterator<WGStructEntry> implements SkippingIterator<WGStructEntry>, CountReportingIterator<WGStructEntry> {
    
    private int _pageOffset = 0;
    private int _pageIdx = 0;
    private WGDocument _parent;
    private int _pageSize;
    private WGStructEntryList _currentPage = null;
    private boolean _endReached = false;
    private String _order;

    protected WGStructEntryIterator(WGDocument parent, int pageSize) {
        this(parent, pageSize, null);
    }
    
    protected WGStructEntryIterator(WGDocument parent, int pageSize, String order) {
        _parent= parent;
        _pageSize = pageSize;
        _order = order;
    }

    @Override
    protected WGStructEntry fetchNextValue() {

        try {
            if (_currentPage == null || _pageIdx >= _currentPage.size()) {
                if (_endReached) {
                    return null;
                }
                fetchNextPage();
            }
            WGStructEntry nextEntry = _currentPage.getByIndex(_pageIdx);
            _pageIdx++;
            return nextEntry;
            
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception fetching next struct entry", e);
        }
        
    }
    
    private int fetchNextPage() throws WGAPIException {
        
        if (_currentPage != null) {
            _pageOffset+=_pageSize;
        }

        if (_parent.getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            if (_parent instanceof WGArea) {
                if (_order != null) {
                    _currentPage = ((WGArea) _parent).getOrderedRootEntries(_pageOffset, _pageSize, _order);
                }
                else {
                    _currentPage = ((WGArea) _parent).getRootEntries(_pageOffset, _pageSize);
                }
            }
            else {
                if (_order != null) {
                    _currentPage = ((WGStructEntry) _parent).getOrderedChildEntries(_pageOffset, _pageSize, _order);
                }
                else {
                    _currentPage = ((WGStructEntry) _parent).getChildEntries(_pageOffset, _pageSize);
                }
            }
        }
        
        // If the database does not support ordered results we are better off fetching all entries (which is done anyway) at once and keep them
        else {
            if (_parent instanceof WGArea) {
                _currentPage = ((WGArea) _parent).getRootEntries();
            }
            else {
                _currentPage = ((WGStructEntry) _parent).getChildEntries();
            }
            _endReached = true;
        }
        
        _pageIdx=0;
        if (_currentPage.size() < _pageSize) {
            _endReached = true;
        }
        return _currentPage.size();
        
    }

    @Override
    public int skip(int nrOfElements) {
    
        // Skip the remainders of the current page
        if (_currentPage != null) {
            while (nrOfElements > 0 && _pageIdx < _currentPage.size()) {
                _pageIdx++;
                nrOfElements--;
            }
        }
        if (nrOfElements == 0) {
            return 0;
        }
        
        // If the database is able to fetch the total count effectively we retrieve it and are able to directly skip to the offset, unless it exceeds the count
        int count = getCount();
        if (count != -1) {
            _currentPage = null;
            _pageIdx = 0;
            
            if (count <= _pageOffset) {
                return nrOfElements;
            }
            else if (count < _pageOffset + nrOfElements) {
                nrOfElements= _pageOffset + nrOfElements - count;
                _pageOffset = count;
                return nrOfElements;
            }
            else {
                _pageOffset+=nrOfElements;
                return 0;
            }
        }
        
        // Otherwise we're better off just fetching the pages until we skipped enough
        else {
            while ( nrOfElements > 0 && hasNext()) {
                next();
                nrOfElements--;
            }
            return nrOfElements;
            
        }
        
    }

    /**
     * Returns the parent document of the iterated struct entries, either a {@link WGArea} or a {@link WGStructEntry}
     */
    public WGDocument getParent() {
        return _parent;
    }

    
    /**
     * Returns the count of all entries in this iterator, only if the database supports effective count retrieval without loading all resources. Otherwise returns -1.
     * @throws WGAPIException 
     */
    public int getCount() {
        try {
            if (_parent.getDatabase().getCore() instanceof WGDatabaseCoreFeatureReturnHierarchyCount) {
                return (_parent instanceof WGArea ? ((WGArea) _parent).getRootEntryCount() : ((WGStructEntry) _parent).getChildEntryCount());
            }
        }
        catch (WGAPIException e) {
        }
        
        return -1;
        
    }
    
    /**
     * Returns the offset of the last retrieved element inside the iterated collection
     */
    public int getCurrentOffset() {
        return _pageOffset + _pageIdx;
    }
    
}
