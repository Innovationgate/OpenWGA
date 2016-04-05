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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.collections.list.GrowthList;

/**
 * A class that caches the document keys of a given list of documents and is able to rebuild the document lists from these cached keys
 */
public class WGDocumentListCache {
    
    private GrowthList _keys = new GrowthList();
    
    /**
     * Denotes if this cache contains all documents of the backend collection
     */
    private boolean _complete = false;
    
    /**
     * Denotes if the size of {@link #_keys} is already equal to the collection size, even if not all docs were fetched 
     */
    private boolean _reachedEnd = false;
    
    public boolean isComplete() {
        return _complete;
    }

    private WGDocumentListCache() {
    }
    
    /**
     * Builds a document list cache from a collection containing {@link WGDocument} objects
     * @param col The collection
     * @param offset The offset of the given collection inside the list
     * @param size The initially requested size of the collection. If this is larger than col size it is interpreted that the remaining requested docs do not exist, i.e. the collection is smaller than requested.
     */
    public static WGDocumentListCache buildFromDocuments(List<? extends WGDocument> col, int offset, int size) {
        
        WGDocumentListCache cache = new WGDocumentListCache();
        if (col != null) {
            cache.addDocuments(col, offset, size);
        }
        return cache;
        
    }

    public static WGDocumentListCache buildFromDocuments(Collection<? extends WGDocument> col) {
        
        WGDocumentListCache cache = new WGDocumentListCache();
        if (col != null) {
            Iterator<? extends WGDocument> docs = col.iterator();
            while (docs.hasNext()) {
                WGDocument doc = docs.next();
                cache._keys.add(doc.getDocumentKeyObj());
            }
        }
        cache.setComplete(true);
        cache.setReachedEnd(true);
        return cache;
        
    }
    
   
    /**
     * Rebuilds the document list from cache
     * The method will return null if the number of documents in the list that must be gathered 
     * from backend exceeds the database configuration on {@link WGDatabase#getListCacheRebuildThreshold()}
     * @param db The database that the documents belong to
     * @return The rebuilt document list or null
     */
    public List<WGDocument> buildDocumentList(WGDatabase db) {
        
        // As this should build the complete list we cannot serve if the list is not yet completely fetched
        if (!isComplete()) {
            return null;
        }
        
        return blowUpCache(db, _keys);
    }
    

    
    /**
     * Rebuilds the document list from cache
     * The method will return null if the number of documents in the list that must be gathered 
     * from backend exceeds the database configuration on {@link WGDatabase#getListCacheRebuildThreshold()}
     * @param db The database that the documents belong to
     * @param offset The offset from which to return entries, 0 being the first one
     * @param size The number of entries to return at maximum
     * @return The rebuilt document list or null
     */
    public List buildDocumentSubList(WGDatabase db, int offset, int size) {
        
        if (offset >= _keys.size()) {
            if (isComplete()) {
                return Collections.EMPTY_LIST;
            }
            else {
                return null;
            }
        }
        
        int toIndex = offset + size;
        if (toIndex > _keys.size()) {
            
            // If the target index is not in the keys and we have not yet reached the end (meaning, the collection is only so long) we cannot use the cache
            if (!isReachedEnd()) {
                return null; 
            }
            else {
                toIndex = _keys.size();
            }
        }
        
        return blowUpCache(db, _keys.subList(offset, toIndex));
    }
    



    private List<WGDocument> blowUpCache(WGDatabase db, List<WGDocumentKey> keysToUse) {
        // First we determine how many of our documents cannot be directly served from cache
        int docsNotInCache = 0;
        List<Object> docs1 = new ArrayList<Object>();
        Iterator<WGDocumentKey> keys = keysToUse.iterator();
        while (keys.hasNext()) {
            Object cacheObj = keys.next();

            // A null cache object means that some doc at an index position has not yet been fetched. We cannot use the cache.
            if (cacheObj == null) {
                return null;
            }

            WGDocumentKey key = (WGDocumentKey) cacheObj; 
            
            try {
                WGDocument doc = db.getDocumentByDocumentKeyFromCache(key);
                if (doc != null) {
                    if (!doc.isDeleted()) {
                        docs1.add(doc);
                    }
                }
                else {
                    docsNotInCache++;
                    docs1.add(key);
                }
            }
            catch (WGAPIException e) {
                WGFactory.getLogger().error("Error retrieving document " + key.toString() + " from document list cache", e);
            }
        }
        
        // Based on the number of docs that must be gotten from backend we determine if we want to
        // really rebuild the list or if we just redo the backend operation (which is faster in many cases)
        // To trigger the latter case we just return null
        if (docsNotInCache > db.getListCacheRebuildThreshold()) {
            return null;
        }
        
        // If it is below threshold we gather the missing documents
        List<WGDocument> docs2 = new ArrayList<WGDocument>();
        Iterator<Object> docs1It = docs1.iterator();
        while (docs1It.hasNext()) {
            Object obj = docs1It.next();
            if (obj instanceof WGDocumentKey) {
                WGDocumentKey key = (WGDocumentKey) obj;
                try {
                    WGDocument doc = key.getDocument(db);
                    if (doc != null && !doc.isDeleted()) {
                        docs2.add(doc);
                    }
                    else { // Cannot retrieve a document that is in the cache list. The cache is no longer up-to-date. Exiting. (#00004330)
                        return null;
                    }
                }
                catch (WGDeletedException e) {
                    // Silently fail. The doc is no longer there.
                }
                catch (WGAPIException e) {
                    WGFactory.getLogger().error("Error retrieving document " + key.toString() + " from document list cache", e);
                }
            }
            else if (obj instanceof WGDocument) {
                docs2.add((WGDocument) obj);
            }
        }
        
        return docs2;
    }

    public void remove(WGDocument doc) {
        _keys.remove(doc.getDocumentKey());
    }
    
    public void remove(WGDocumentKey docKey) {
        _keys.remove(docKey);
    }
    
    public int size() {
        return _keys.size();
    }

    private void setComplete(boolean complete) {
        _complete = complete;
    }
    
    /**
     * Adds a collection to this list cache
     * @param col The collection
     * @param offset The offset of the given collection inside the list
     * @param size The initially requested size of the collection. If this is larger than col size it is interpreted that the remaining requested docs do not exist, i.e. the collection is smaller than requested.
     */
    private void addDocuments(List<? extends WGDocument> docs, int offset, int size) {
        int idx = offset;
        for (WGDocument doc : docs) {
            _keys.set(idx, doc.getDocumentKeyObj());
            idx++;
        }
        
        // If more documents requested than returned: We have either fetched the complete collection (when offset was 0) or at least reached its end
        if (size > docs.size()) {
            setReachedEnd(true);
            if (offset == 0) {
                setComplete(true);
            }
        }
        
    }

    public boolean isReachedEnd() {
        return _reachedEnd;
    }

    private void setReachedEnd(boolean reachedEnd) {
        _reachedEnd = reachedEnd;
    }

    /**
     * Creates a new cache with the entries of this collection and the entries of the given list merged
     * @param entries
     * @param offset
     * @param size
     * @return
     */
    public WGDocumentListCache mergeWithDocuments(List<? extends WGDocument> docs, int offset, int size) {

        WGDocumentListCache newCache = clone();
        if (docs != null) {
            newCache.addDocuments(docs, offset, size);
        }
        return newCache;
        
    }
    
    public WGDocumentListCache clone() {
        
        WGDocumentListCache newCache = new WGDocumentListCache();
        newCache._keys = new GrowthList();
        newCache._keys.addAll(_keys);
        newCache._complete = _complete;
        newCache._reachedEnd = _reachedEnd;
        return newCache;
        
    }
    
    /**
     * Returns if the given document key is currently contained in the cache.
     * @param key The key
     */
    public boolean containsKey(WGDocumentKey key) {
        return _keys.contains(key);
    }


}
