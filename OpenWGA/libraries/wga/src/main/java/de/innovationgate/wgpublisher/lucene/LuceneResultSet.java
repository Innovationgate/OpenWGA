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

import javax.servlet.http.HttpServletRequest;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;

public abstract class LuceneResultSet implements WGResultSet, Iterable<WGContent> {
    
    protected TopDocs _hits;
    protected long _executionTime;
    protected Query _query;
    boolean _enhance;
    boolean _explain = false;
    protected WGA _wga;
    public LuceneResultSet(TopDocs hits, WGA wga, Map parameters, Query query, long executionTime) {
        _hits = hits;
        _wga = wga;
        _executionTime = executionTime;
        _query = query;
        
        _enhance = !parameters.containsKey(WGDatabase.QUERYOPTION_ENHANCE) || parameters.get(WGDatabase.QUERYOPTION_ENHANCE).equals(new Boolean(true));
        if (parameters.containsKey(WGDatabase.QUERYOPTION_NATIVEOPTIONS)) {
            String nativeOptionsStr = (String) parameters.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS);
            if (nativeOptionsStr != null) {
                Iterator nativeOptions = WGUtils.deserializeCollection(nativeOptionsStr, ",", true).iterator();
                while (nativeOptions.hasNext()) {
                    String option = (String) nativeOptions.next();
                    if (option.equalsIgnoreCase("explain")) {
                        _explain = true;
                    }                 
                }
            }            
        }

    }


    
    /**
     * @param doc
     * @return  
     * @throws IOException 
     * @throws CorruptIndexException 
     * @throws InterruptedException 
     * @throws WGAccessException
     */
    protected WGContent luceneDocToContent(ScoreDoc scoreDoc, float scMax) throws CorruptIndexException, IOException, InterruptedException  {

        try {
            Document doc = _wga.getCore().getLuceneManager().getDocument(scoreDoc.doc); 
            String contentKey = doc.get(LuceneManager.INDEXFIELD_CONTENTKEY);
            String dbKey = doc.get(LuceneManager.INDEXFIELD_DBKEY);
            WGDatabase db = _wga.db(dbKey);
            if (db.isSessionOpen() == false) {
                return null;
            }
            float score = 0;
            if (scMax != 0 && !Float.isNaN(scMax) && !Float.isNaN(scoreDoc.score)) {
                score = scoreDoc.score/scMax;
            }
            
            String parentKey = doc.get(LuceneManager.VIRTUALMETA_PARENT);
            if (parentKey != null) {
                WGStructEntry parent = db.getStructEntryByKey(db.parseStructKey(parentKey));
                if (parent != null && !parent.mayReadContent()) {
                    return null;
                }
            }
            else {
                String areaName = doc.get(LuceneManager.VIRTUALMETA_AREA);
                if (areaName != null) {
                    WGArea area = db.getArea(areaName);
                    if (area != null && !area.mayReadContent()) {
                        return null;
                    }
                }
            }
            
            WGContent content = db.getContentByKey(WGContentKey.parse(contentKey, db));
            if (content != null && (!_enhance || content.isVisibleNow())) {
                
                LuceneSearchDetails  details = new LuceneSearchDetails();
                details.setScore(score);                
                if (_explain) {
                    try {
                        details.setExplanation(_wga.getCore().getLuceneManager().explain(_query, scoreDoc.doc));
                    } catch (Exception e) {                 
                    }
                }
                String luceneDocType = doc.get(LuceneManager.INDEXFIELD_DOCTYPE);
                if (luceneDocType == null) {
                    luceneDocType = LuceneManager.DOCTYPE_CONTENT;
                }
                details.setDoctype(luceneDocType);
                if (LuceneManager.DOCTYPE_ATTACHMENT.equals(luceneDocType)) {
                    details.setFilename(doc.get(LuceneManager.INDEXFIELD_FILENAME));
                    content = content.createSearchResultDuplicate(details);
                }
                else {
                    content.setSearchDetails(details);
                }
                
                return content;
            }
            else {
                return null;
            }
            
            
        }
        catch (WGException e) {
            _wga.getCore().getLog().error("Exception retrieving content for lucene document", e);
            return null;
        }

        
 
        
    }

    public long getExecutionTime() {
        return _executionTime;
    }



    public void close() {
        _wga = null;
    }
    
    public Iterator<WGContent> iterator() {
        try {
            return getResultIterator();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Exception retrieving result iterator", e);
        }
    }


}