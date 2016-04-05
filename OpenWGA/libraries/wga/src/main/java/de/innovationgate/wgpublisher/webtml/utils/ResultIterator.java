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

package de.innovationgate.wgpublisher.webtml.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import org.apache.log4j.Logger;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGResultSet;

/**
 * A wrapper iterator for an iterable collection, able to report result size and skip results
 */
public class ResultIterator<T> implements SkippingIterator<T> {
    
    public static interface ResultCount {
        
        public int results();
        
    }
    
    private Iterator<T> _iterator;
    private ResultCount _resultCount;
    private Integer _resultSize = null;
    private boolean _fetched = false;

    public ResultIterator(Iterator<T> it, ResultCount results) {
        _iterator = it;
        _resultCount = results;
    }
    
    public ResultIterator(Iterator<T> it, final int size) {
        _iterator = it;
        _resultCount = new ResultCount() {
            public int results() {
                return size;
            }
        };
    }
    
    public ResultIterator(Iterator<T> it, final WGResultSet resultSet) {
        _iterator = it;
        _resultCount = new ResultCount() {
            public int results() {
                try {
                    return resultSet.results();
                }
                catch (WGBackendException e) {
                    Logger.getLogger("wga").error("Exception fetching result set size", e);
                    return 0;
                }
            }
        };
    }
    
    public ResultIterator(Iterator<T> it, final ResultIterator<? extends Object> parentIterator) {
        _iterator = it;
        _resultCount = new ResultCount() {
            public int results() {
                return parentIterator.getResultSize();
            }
        };
    }
    
    
    
    
    public ResultIterator(final Collection<T> col) {
        _iterator = col.iterator();
        _resultCount = new ResultCount() {
            public int results() {
                return col.size();
            }
        };
    }

    public int getResultSize() {
        
        if (_resultSize == null) {
            _resultSize = _resultCount.results();
        }
        return _resultSize;
        
    }

    public List<Object> extractCompleteList() {
        List<Object> list = new ArrayList<Object>();
        while (_iterator.hasNext()) {
            list.add(_iterator.next());
        }
        return list;
    }

    public boolean hasNext() {
        _fetched = true;
        return _iterator.hasNext();
    }

    public T next() {
        _fetched = true;
        T value = _iterator.next();
        return value;
   }

    public void remove() {
        throw new UnsupportedOperationException();
    }
    
    public int proceedToOffset(int offset, boolean linear) {
        
        if (offset == 0) {
            return 0;
        }
        
        if (_iterator instanceof SkippingIterator && !linear) {
            @SuppressWarnings("unchecked")
            SkippingIterator<Object> skipper = (SkippingIterator<Object>) _iterator;
            return skipper.skip(offset);
        }
        else {
            while (offset > 0) {
                if (!_iterator.hasNext()) {
                    return offset;
                }
                _iterator.next();
                offset--;
            }
            return 0;
        }
        
    }

    public int skip(int nrOfElements) {
        
        if (_fetched) {
            throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
       }
        
        return proceedToOffset(nrOfElements, false);
    }

}