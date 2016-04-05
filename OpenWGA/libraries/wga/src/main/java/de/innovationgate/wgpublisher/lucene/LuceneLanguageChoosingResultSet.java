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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.collections.iterators.ArrayIterator;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAbstractResultSetIterator;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

public class LuceneLanguageChoosingResultSet extends LuceneResultSet {
    
    class DBContentKey {
        
        private String _db;
        private WGContentKey _contentKey;
        private ScoreDoc _scoreDoc;
    
        public DBContentKey(String db, WGContentKey contentKey, ScoreDoc scoreDoc) {
            _db = db;
            _contentKey = contentKey;
            _scoreDoc = scoreDoc;
        }
    
        public String getDb() {
            return _db;
        }
    
        public WGContentKey getContentKey() {
            return _contentKey;
        }
    
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((_contentKey == null) ? 0 : _contentKey.hashCode());
            result = prime * result + ((_db == null) ? 0 : _db.hashCode());
            return result;
        }
    
        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            DBContentKey other = (DBContentKey) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (_contentKey == null) {
                if (other._contentKey != null)
                    return false;
            }
            else if (!_contentKey.equals(other._contentKey))
                return false;
            if (_db == null) {
                if (other._db != null)
                    return false;
            }
            else if (!_db.equals(other._db))
                return false;
            return true;
        }
    
        private LuceneLanguageChoosingResultSet getOuterType() {
            return LuceneLanguageChoosingResultSet.this;
        }

        public ScoreDoc getScoreDoc() {
            return _scoreDoc;
        }
        
    }

    public class LuceneResultIterator extends WGAbstractResultSetIterator<DBContentKey> {
        
        private int _resultCount = 0;
    
        public LuceneResultIterator() {
            super(_contents.iterator());
        }
        
        @Override
        protected WGContent fetchContentForResult(DBContentKey result) throws WGAPIException {
            try {
                return luceneDocToContent(result.getScoreDoc(), _hits.getMaxScore());
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
            
            int notSkippableElements = super.skip(nrOfElements - limitedElements);
            return notSkippableElements + limitedElements;
            
        }
        
        
    
    }

    private List<DBContentKey> _contents;
    private int _resultLimit = -1;
    
    public LuceneLanguageChoosingResultSet(TopDocs hits, WGA wga, Map parameters, BooleanQuery wholeQuery, long executionTime, List<WGLanguage> languagesPriorityList) throws WGException, CorruptIndexException, IOException, InterruptedException {
        super(hits, wga, parameters, wholeQuery, executionTime);

        // Build contents list
        _contents = buildContentsList(hits, wga.getCore());
        
        // Build maps per page
        Map<Object,Map<String, DBContentKey>> pages = new HashMap<Object,Map<String, DBContentKey>>();
        for (DBContentKey content : _contents) {
            Map<String,DBContentKey> pageMap = pages.get(content.getDb() + "/" + String.valueOf(content.getContentKey().getStructKey()));
            if (pageMap == null) {
                pageMap = new HashMap<String, DBContentKey>();
                pages.put(content.getDb() + "/" + String.valueOf(content.getContentKey().getStructKey()), pageMap);
            }
            pageMap.put(content.getContentKey().getLanguage(), content);
        }
        
        // Choose content versions to return
        List<DBContentKey> contentsToReturn = new ArrayList<DBContentKey>();
        for (Map<String, DBContentKey> pageMap : pages.values()) {
            DBContentKey chosenContent = null;
            if (pageMap.size() == 1) {
                chosenContent = pageMap.values().iterator().next();
            }
            else {
                for (WGLanguage lang : languagesPriorityList) {
                    DBContentKey conKey = pageMap.get(lang.getName());
                    if (conKey != null) {
                        chosenContent = conKey;
                        break;
                    }
                }
            }
            if (chosenContent != null) {
                contentsToReturn.add(chosenContent);
            }
        }
        
        // Keep only chosen ones in the contents list
        _contents.retainAll(contentsToReturn);
        
    }

    private List<DBContentKey> buildContentsList(TopDocs hits, WGACore core) throws CorruptIndexException, IOException, InterruptedException, WGException {

        List<DBContentKey> contents = new ArrayList<DBContentKey>();
        for (int i=0; i < hits.scoreDocs.length; i++) {
            ScoreDoc scoreDoc = hits.scoreDocs[i];
            Document doc = _wga.getCore().getLuceneManager().getDocument(scoreDoc.doc);
            String contentKey = doc.get(LuceneManager.INDEXFIELD_CONTENTKEY);
            String dbKey = doc.get(LuceneManager.INDEXFIELD_DBKEY);
            WGDatabase db = core.getContentdbs().get(dbKey);
            if (db != null) {
                contents.add(new DBContentKey(dbKey, WGContentKey.parse(contentKey, db), scoreDoc));
            }
        }
        
        return contents;

    }

    public WGContentList getContentList() throws WGAPIException {
        return getContentList(1, results());
    }

    public WGContentList getContentList(int start, int length) throws WGAPIException {
        try {
            WGContentList contents = new WGContentList();
            if (_contents.size() > 0) {
                for (int i=start-1; i < (start - 1 + length); i++) {
                    if (i >= _contents.size()) {
                        break;
                    }                
                    DBContentKey key = _contents.get(i);
                    WGContent content = luceneDocToContent(key.getScoreDoc(), _hits.getMaxScore());
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

    public SkippingIterator<WGContent> getResultIterator() throws WGAPIException {
        return new LuceneResultIterator();
    }

    public int results() {
        if (_resultLimit == -1) {
            return _contents.size();
        }
        else {
            return _resultLimit;
        }
        
    }

    public void limitResults(int limit) {
        if (limit < _contents.size()) {
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
