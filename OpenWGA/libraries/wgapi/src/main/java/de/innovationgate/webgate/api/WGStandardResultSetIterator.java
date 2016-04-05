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

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGStandardResultSet.FetchingIterator;

/**
 * Utility class for iterating over content keys to return {@link WGContent} documents
 */
public class WGStandardResultSetIterator extends WGAbstractResultSetIterator implements CountReportingIterator {
    
    private WGAbstractResultSet _resultSet;
    protected boolean _enhanced = false;
    private Integer _resultLimit;
    private int _resultCount = 0;
    private int _offset = 0; 
   

    public WGStandardResultSetIterator(WGAbstractResultSet resultSet, List<Object> resultKeys, boolean enhanced, Integer resultLimit) {
        super(resultKeys);
        _resultSet = resultSet;
        _enhanced = enhanced;
        _resultLimit = resultLimit;
    }
    
    public WGStandardResultSetIterator(WGAbstractResultSet resultSet, Iterator<Object> resultKeys, boolean enhanced, Integer resultLimit) {
        super(resultKeys);
        _resultSet = resultSet;
        _enhanced = enhanced;
        _resultLimit = resultLimit;
    }


    @Override
    protected WGContent fetchContentForResult(Object result) throws WGAPIException {
        return _resultSet.fetchContentForRow(result);
    }
    
    @Override
    protected boolean passesFilter(WGContent content) {
        try {
            return (!_enhanced || content.isVisibleNow());
        }
        catch (WGAPIException e) {
            WGFactory.getLogger().error("Exception determining visibility state of " + content.getDocumentKey(), e);
            return false;
        }
    }

    @Override
    public boolean hasNext() {
        return (_resultLimit == null || _resultCount < _resultLimit) && super.hasNext();
    }

    @Override
    public Object next() {
        if (_resultLimit == null || _resultCount < _resultLimit) {
            _resultCount++;
            _offset++;
            return super.next();
        }
        else {
            throw new NoSuchElementException();
        }
    }

    @Override
    public int skip(int nrOfElements) {
        
        int limitedElements = 0;
        if (_resultLimit != null) {
            int remainingElements = _resultLimit - _resultCount;
            if (nrOfElements > remainingElements) {
                limitedElements = nrOfElements - remainingElements;
            }
        }
        
        int elementsToSkip = nrOfElements - limitedElements;
        int notSkippableElements = super.skip(elementsToSkip);
        _offset+=elementsToSkip-notSkippableElements;
        return notSkippableElements + limitedElements;
        
    }

    @Override
    public int getCount() {
        try {
            if (_it instanceof CountReportingIterator<?>) {
                return ((CountReportingIterator) _it).getCount();
            }
            else {
                return _resultSet.results();
            }
        }
        catch (WGBackendException e) {
            return -1;
        }
    }

    @Override
    public int getCurrentOffset() {

        if (_it instanceof CountReportingIterator<?>) {
            return ((CountReportingIterator) _it).getCurrentOffset();
        }
        else {
            return _offset;
        }
    }



}