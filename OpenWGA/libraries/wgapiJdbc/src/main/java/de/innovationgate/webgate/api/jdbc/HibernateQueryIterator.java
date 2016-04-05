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

package de.innovationgate.webgate.api.jdbc;

import java.io.Closeable;
import java.io.IOException;
import java.util.Iterator;
import java.util.NoSuchElementException;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.ScrollMode;
import org.hibernate.ScrollableResults;

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;

public class HibernateQueryIterator implements SkippingIterator<Object>, CountReportingIterator<Object>, Closeable {

  
    private ScrollableResults _set;
    private boolean _endReached;
    private HibernateResultSet _resultSet;

    public HibernateQueryIterator(Query q, HibernateResultSet resultSet) {
        _resultSet = resultSet;
        
        long timeBefore = System.currentTimeMillis();
        _set = q.scroll(ScrollMode.SCROLL_INSENSITIVE);
        long timeAfter = System.currentTimeMillis();
        if (resultSet != null) {
            resultSet.setExecutionTime(timeAfter - timeBefore);
        }
        
        _endReached = !_set.next();
    }
    

    public int skip(int nrOfElements) {
        
        int startIdx = _set.getRowNumber();
        if (!_set.scroll(nrOfElements)) {
            _endReached = true;
            _set.last();
            int rowsScrolled = _set.getRowNumber() - startIdx; 
            return nrOfElements - rowsScrolled;
        }
        else {
            return 0;
        }
    }

    public boolean hasNext() {
        return !_endReached;
    }

    public Object next() {  
        
        if (_endReached) {
            throw new NoSuchElementException();
        }
        
        try {
            Object[] value = _set.get();
            _endReached = !_set.next();
            if (_resultSet != null) {
                return _resultSet.wrapEntity(value);
            }
            else {
                return value;
            }
        }
        catch (Exception e) {
            throw new RuntimeException("Exception fetching next query result", e);
        }
        
    }

    public void remove() {
       throw new UnsupportedOperationException();
    }


    @Override
    public int getCount() {
        try {
            if (_resultSet != null) {
                return _resultSet.results();
            }
            else {
                return -1;
            }
        }
        catch (WGBackendException e) {
            return -1;
        }
    }


    @Override
    public int getCurrentOffset() {
        return _set.getRowNumber();
    }


    @Override
    public void close() throws IOException {
        _set.close();        
    }

}
