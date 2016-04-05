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

/**
 * A simple skipping iterator wrapper
 * This is just a basic implementation which emulates skipping behaviour via iterating over the {@link #next()} method.
 * It is for usage at places where a SkippingIterator is needed but the iterator used may not be skipping.
 * If the wrapped iterator is itself a {@link SkippingIterator} then its own {@link #skip(int)} method is used instead of the emulation.
 */
public class SkippingIteratorWrapper<T> implements SkippingIterator<T>, IteratorWrapper<T> {
    
    private Iterator<? extends T> _it;

    public SkippingIteratorWrapper(Iterator<? extends T> it) {
        _it = it;
    }

    @Override
    public boolean hasNext() {
        return _it.hasNext();
    }

    @Override
    public T next() {
        return _it.next();
    }

    @Override
    public void remove() {
        _it.remove();
    }

    @Override
    public int skip(int nrOfElements) {
        
        if (_it instanceof SkippingIterator<?>) {
            return ((SkippingIterator<?>) _it).skip(nrOfElements);
        }

        for (int i=0; i<nrOfElements; i++) {
            if (_it.hasNext()) {
                _it.next();
            }
            else {
                return nrOfElements - i;
            }
        }
        
        return 0;
        
    }

    @Override
    public Iterator<? extends T> getWrappedIterator() {
        return _it;
    }

}
