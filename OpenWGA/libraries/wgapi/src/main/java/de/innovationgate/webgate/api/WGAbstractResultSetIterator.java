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
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.IteratorWrapper;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;

/**
 * Abstract base class for result set iterators, implementing shared functionality like prefetching which is neccessary for implicit filtering
 */
public abstract class WGAbstractResultSetIterator<T extends Object> extends PrefetchingIterator<WGContent> implements SkippingIterator<WGContent>, Closeable, IteratorWrapper<T> {

    protected Iterator<T> _it;

    protected abstract WGContent fetchContentForResult(T result) throws WGAPIException;

    public WGAbstractResultSetIterator(List<T> resultKeys) {
        _it = resultKeys.iterator();
    }
    
    public WGAbstractResultSetIterator(Iterator<T> resultKeys) {
        _it = resultKeys;
    }

    public int skip(int nrOfElements) {

       
        while (nrOfElements > 0) {
            
            if (clearCurrentValue()) {
                nrOfElements--;
            }
            else if (_it instanceof SkippingIterator) {
                SkippingIterator<?> skippy = (SkippingIterator<?>) _it;
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
    protected WGContent fetchNextValue() {
        
        try {
            WGContent currentContent = null;
            while (_it.hasNext()) {
                T result = _it.next();
                
                // Backend iterators may return null if a result is read protected
                if (result == null) {
                    continue;
                }
                
                WGContent content = fetchContentForResult(result);
                if (content != null && !content.isDummy() && passesFilter(content)) {
                    currentContent = content;
                    break;
                }
            }
            
            return currentContent;
        }
        catch (WGAPIException e) {
            throw new RuntimeException("Exception retrieving next content from result set", e);
        }
    }

    protected abstract boolean passesFilter(WGContent content);
    
    @Override
    public void close() throws IOException {
        if (_it instanceof Closeable) {
            ((Closeable) _it).close();
        }
    }
    
    
    @Override
    public Iterator<T> getWrappedIterator() {
        return _it;
    }

    

}