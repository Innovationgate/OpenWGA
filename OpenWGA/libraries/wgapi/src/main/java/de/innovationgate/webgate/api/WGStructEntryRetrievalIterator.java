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

import java.io.Closeable;
import java.io.IOException;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGStandardResultSet.FetchingIterator;

/**
 * Class to iterate over struct entries returned by hierarchy retrieeval methods of WGAPI backend. Internal use only.
 * The method {@link #close()} should be called on this iterator after using it so it can release backend resources.
 */
public class WGStructEntryRetrievalIterator extends PrefetchingIterator<WGStructEntry> implements SkippingIterator<WGStructEntry> {

    protected Iterator<WGDocumentCore> _it;
    private WGDatabase _database;
    private WGDocument _parent;
    private boolean _fetched = false;
    
    /**
     * Returns an empty version of this iterator type
     */
    public static WGStructEntryRetrievalIterator emptyIterator() {
        return new WGStructEntryRetrievalIterator(null, null, Collections.EMPTY_LIST.iterator());
    }

    protected WGStructEntry fetchStructForResult(WGDocumentCore result) throws WGAPIException {
        return getDatabase().getOrCreateStructEntryObject(result, new WGDocumentObjectFlags());
    }
    

    public WGStructEntryRetrievalIterator(WGDatabase db, WGDocument parent, Iterator<WGDocumentCore> structCores) {
        _database = db;
        _it = structCores;
        _parent = parent;
    }

    public int skip(int nrOfElements) {
        
        if (_fetched) {
            throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
        }

        while (nrOfElements > 0) {
            
            if (_currentValue != null) {
                _currentValue = null;
                nrOfElements--;
            }
            else if (_it instanceof SkippingIterator) {
                SkippingIterator skippy = (SkippingIterator) _it;
                return skippy.skip(nrOfElements);
            }
            else {
                if (_it.hasNext()) {
                    _it.next();
                }
                else {
                    return nrOfElements;
                }
                nrOfElements--;
            }
            
            
        }
        
        return 0;

    }

    @Override
    protected WGStructEntry fetchNextValue() {
        
        _fetched  = true;
        try {
            WGStructEntry currentEntry = null;
            while (_it.hasNext()) {
                WGDocumentCore result = _it.next();
                
                // Backend iterators may return null if a result is was read protected
                if (result == null) {
                    continue;
                }
                
                WGStructEntry entry = fetchStructForResult(result);
                if (entry != null) {
                    currentEntry = entry;
                    break;
                }
            }
            
            return currentEntry;
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception retrieving next content from result set", e);
        }
    }


    /**
     * Returns the database of the struct entries returned
     */
    public WGDatabase getDatabase() {
        return _database;
    }
    
    public void close() {
        if (_it instanceof Closeable) {
            try {
                ((Closeable) _it).close();
            }
            catch (IOException e) {
                WGFactory.getLogger().error("Exception closing struct entry iterator", e);
            }
        }
    }

}