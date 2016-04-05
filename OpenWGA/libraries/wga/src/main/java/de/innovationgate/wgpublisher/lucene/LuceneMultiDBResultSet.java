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
package de.innovationgate.wgpublisher.lucene;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.collections.iterators.ArrayIterator;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAbstractResultSetIterator;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

public class LuceneMultiDBResultSet extends LuceneResultSet {
    
    public class LuceneResultIterator extends WGAbstractResultSetIterator<ScoreDoc> implements CountReportingIterator<WGContent> {
        
        private int _resultCount = 0;
        private int _offset = 0;

        public LuceneResultIterator() {
            super(new ArrayIterator(_hits.scoreDocs));
        }
        
        @Override
        protected WGContent fetchContentForResult(ScoreDoc result) throws WGAPIException {
            try {
                return luceneDocToContent(result, _hits.getMaxScore());
            }
            catch (Exception e) {
                throw new WGBackendException("Exception iterating over lucene result", e);
            }
            
        }

        @Override
        protected boolean passesFilter(WGContent content) {
            try {
                return (!_enhance || content.isVisibleNow());
            }
            catch (WGAPIException e) {
                WGFactory.getLogger().error("Exception determining visibility state of " + content.getDocumentKey(), e);
                return false;
            }
        }
        
        @Override
        public boolean hasNext() {
            return (_resultLimit == -1 || _resultCount < _resultLimit) && super.hasNext();
        }
        
        @Override
        public WGContent next() {
            if (_resultLimit == -1 || _resultCount < _resultLimit) {
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
            if (_resultLimit != -1) {
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
        public int getCurrentOffset() {
            return _offset;
        }

        @Override
        public int getCount() {
            return results();
        }
        

    }

    private int _resultLimit = -1;

    public LuceneMultiDBResultSet(TopDocs hits, WGA wga, Map parameters, Query query, long executionTime) {
        super(hits, wga, parameters, query, executionTime);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGResultSet#getContentList(int, int)
     */
    public WGContentList getContentList(int start, int length) {
        try {
            WGContentList contents = new WGContentList();
            if ( _hits.scoreDocs.length > 0) {
	            for (int i=start-1; i < (start - 1 + length); i++) {
	                if (i >= _hits.scoreDocs.length) {
	                    break;
	                }                
	                ScoreDoc doc = _hits.scoreDocs[i];
	                WGContent content = luceneDocToContent(doc, _hits.getMaxScore());
	                if (content != null) {
	                    contents.add(content);
	                }
	                else {
	                    length++; // Must be increased to compensate not retrievable content
	                }
	            }
            }
            return contents;
        }
        catch (Exception e) {
            _wga.getCore().getLog().error("Error returning lucene search result", e);
            return null;
        } 
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGResultSet#getContentList()
     */
    public WGContentList getContentList() {
        return getContentList(1, results());
    }



    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.lucene.LuceneResultSet#close()
     */
    public void close() {
        super.close();
    	_query = null;
    }

    public SkippingIterator<WGContent> getResultIterator() throws WGAPIException {
        return new LuceneResultIterator();
    }

    public int results() {
        if (_resultLimit == -1) {
            return _hits.scoreDocs.length;
        }
        else {
            return _resultLimit;
        }
        
    }

    public void limitResults(int limit) {
        if (limit < _hits.scoreDocs.length) {
            _resultLimit = limit;
        }
    
    }
    
    
    @Override
    public boolean hasResults() throws WGBackendException {
        return results() > 0;
    }
    
    @Override
    public boolean isLimitingResultsInBackend() {
        return true;
    }
    
    
}

