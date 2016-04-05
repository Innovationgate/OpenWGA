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

package de.innovationgate.utils;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * An iterator that prefetches values and is therefor able to filter them while returning valid results for {@link #hasNext()}
 */
public abstract class PrefetchingIterator<T> implements Iterator<T>{

    protected T _currentValue;

    /**
     * Method to fetch the next value. Do filtering tasks here. If you have reached the end of your iteration return null.
     */
    protected abstract T fetchNextValue();

    private boolean _endReached = false;

    public PrefetchingIterator() {
        super();
    }

    public boolean hasNext() {
        fillNextValue();
        return _currentValue != null;
    }

    public T next() {
       fillNextValue();
       if (!_endReached) {
           T valueToReturn = _currentValue;
           _currentValue = null;
           return valueToReturn;
       }
       else {
           throw new NoSuchElementException();
       }
    }

    private void fillNextValue() {
        if (_currentValue == null && !_endReached) {
            _currentValue = fetchNextValue();
            if (_currentValue == null) {
                _endReached = true;
            }
        }
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    /**
     * Returns the next value to serve without advancing the iterator. I.e. a subsequent call to {@link #next()} will (still) serve the value returned here.
     */
    public T previewNextValue() {
        fillNextValue();
        return _currentValue;
    }
    
    /**
     * Clears a prefetched current value. Returns if there actually was a prefetched value. Use this when implementing {@link SkippingIterator#skip(int)} to respect the prefetch in it 
     */
    protected boolean clearCurrentValue() {
        if (!_endReached && _currentValue != null) {
            _currentValue = null;
            return true;
        }
        else {
            return false;
        }
    }

}