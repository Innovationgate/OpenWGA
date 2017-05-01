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

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.TermFreqVector;

import antlr.collections.impl.Vector;
import de.innovationgate.webgate.api.SearchDetails;

public class LuceneSearchDetails extends SearchDetails {
    
    private String _doctype = null;
    private String _filename = null;
    private Integer _luceneDoc = null;
    private TermFreqVector _termFreqVector = null;
    
    public String getDoctype() {
        return _doctype;
    }
    public void setDoctype(String doctype) {
        _doctype = doctype;
    }
    public String getFilename() {
        return _filename;
    }
    public void setFilename(String filename) {
        _filename = filename;
    }
    
    public Integer getLuceneDoc(){
    	return _luceneDoc;
    }
    public void setLuceneDoc(int doc){
    	_luceneDoc = doc;
    }
    
    public void setTermFreqVector(TermFreqVector vector){
    	_termFreqVector = vector;
    }

    public LinkedHashMap<String,Integer> getTerms(){
    	
    	LinkedHashMap<String,Integer> map = new LinkedHashMap<String,Integer>();
    	
    	if(_termFreqVector==null)
    		return map;
    	Integer count = _termFreqVector.size();
    	String[] terms = _termFreqVector.getTerms();
    	
    	for(int i=0; i<count; i++){
    		if(terms[i].length()>2 && _termFreqVector.getTermFrequencies()[i]>2)
    			map.put(terms[i], _termFreqVector.getTermFrequencies()[i]);
    	}

    	LinkedList<Map.Entry<String, Integer>> list =
                new LinkedList<Map.Entry<String, Integer>>(map.entrySet());
    	Collections.sort( list, new Comparator<Map.Entry<String, Integer>>(){
            public int compare( Map.Entry<String, Integer> o1, Map.Entry<String, Integer> o2 )
            {
                return (o2.getValue()).compareTo(o1.getValue());
            }
        });
    	
    	LinkedHashMap<String,Integer> result = new LinkedHashMap<String,Integer>();
    	for (Map.Entry<String, Integer> entry : list){
            result.put(entry.getKey(), entry.getValue());
        }
    	
    	return result;
    }
    
}
