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
package de.innovationgate.wgpublisher.webtml;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wgpublisher.webtml.utils.ResultIterator;
import de.innovationgate.wgpublisher.webtml.utils.ResultSetTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class Collection extends Base {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static class ResultSetIterator implements SkippingIterator<Object> {
	    
	    private Iterator<WGResultSet> _resultSets;
	    private SkippingIterator<WGContent> _currentResultIterator = null;
	    private boolean _endReached = false;
        private boolean _fetched = false;

        public ResultSetIterator(Iterator<WGResultSet> resultSets) {
	        _resultSets = resultSets;
	    }
        
        private void fetchCurrentResultSet() throws WGAPIException {
            while (!_endReached && (_currentResultIterator == null || !_currentResultIterator.hasNext())) {
                if (!_resultSets.hasNext()) {
                    _endReached = true;
                    return;
                }
                _currentResultIterator = _resultSets.next().getResultIterator();
            }
        }

        public int skip(int nrOfElements) {
            if (_fetched) {
                throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
            }
            try {
                while (nrOfElements > 0) {
                    fetchCurrentResultSet();
                    if (_endReached) {
                        return nrOfElements;
                    }
                    nrOfElements = _currentResultIterator.skip(nrOfElements);
                }
                return 0;
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception skipping resultset results", e);
            }
        }

        public boolean hasNext() {
            _fetched  = true;
            try {
                fetchCurrentResultSet();
                return !_endReached;
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception iterating query results", e);
            }
        }

        public WGContent next() {
            _fetched = true;
            try {
                fetchCurrentResultSet();
                if (!_endReached) {
                    return _currentResultIterator.next();
                }
                else {
                    throw new NoSuchElementException();
                }
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception iterating query results", e);
            }
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }

    }

    public static final String TAGINFO_RESULTSET = "resultset";

    public static class Status extends BaseTagStatus implements ResultSetTagStatus, TMLParameterReceiver {
        
    	private java.util.List resultSets = null;
        private String resultLanguage = null;
        private Map queryParameters = null;
        
        /**
         * @see ContentListTag#getContentList()
         */
        public WGContentList getContentList() {
            
            WGContentList list = new WGContentList();
            Iterator resultSetsIt = resultSets.iterator();
            WGResultSet resultSet;
            while (resultSetsIt.hasNext()) {
                resultSet = (WGResultSet) resultSetsIt.next();
                try {
                    list.addAll(resultSet.getContentList());
                }
                catch (WGAPIException e) {
                    tmlContext.addwarning("Unable to retrieve content list from resultset. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                    log.error("Error creation collection content list", e);
                }
            }
             
            return list;
        }

        /**
         * @see ContentListTag#addContentList(WGContentList)
         */
        public void addResultSet(WGResultSet resultSet, String language) {
            resultSets.add(resultSet);
            
            if (resultLanguage == null) {
                resultLanguage = language;
            }
            else if (!resultLanguage.equals(language)) {
                resultLanguage = MULTILANGUAGE_RESULT;
            }
        }
        
        /**
         * @throws WGBackendException 
         * @see de.innovationgate.wgpublisher.webtml.utils.ResultSetTagStatus#getContentList(int, int)
         */
        public WGContentList getContentList(int start, int length) throws WGBackendException {
            
            WGContentList list = new WGContentList();
            
            Iterator sets = this.resultSets.iterator();
            WGResultSet currentSet = null;
            
            // First get to start position
            int startCtr  = start;
            int results = currentSet.results();
            while (sets.hasNext()) {
                currentSet = (WGResultSet) sets.next();
                if (results < startCtr) {
                    startCtr -= results;
                    currentSet = null;
                }
                else {
                    break;
                }
            }
            if (currentSet == null) {
                return list;
            }
            
            // The contents of this set is enough to fit the length
            if (results >= startCtr + (length - 1)) {
                try {
                    return currentSet.getContentList(startCtr, length);
                }
                catch (WGAPIException e) {
                    tmlContext.addwarning("Unable to retrieve content list from resultset. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                    return list;
                }
            }
            
            // Multiple sets are needed to fit the length
            else {
                
                // Take the first contents from this set;
                int lengthCtr = length;
                int lengthThisSet = ((int) results) - startCtr + 1;
                try {
                    list.addAll(currentSet.getContentList(startCtr, lengthThisSet));
                    lengthCtr -= lengthThisSet;
                }
                catch (WGAPIException e) {
                    tmlContext.addwarning("Unable to retrieve content list from resultset. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                }
                
                
                // Take more contents from coming sets
                while (sets.hasNext()) {
                    currentSet = (WGResultSet) sets.next();
                    
                    // This set has to be taken completely (< remaining length)
                    if (results <= lengthCtr) {
                        lengthThisSet = ((int) results);
                        try {
                            list.addAll(currentSet.getContentList(1, lengthThisSet));
                            lengthCtr -= lengthThisSet;
                        }
                        catch (WGAPIException e) {
                            tmlContext.addwarning("Unable to retrieve content list from resultset. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                        }
                        
                    }
                    
                    // This sets contents fits the remaining length
                    else {
                        lengthThisSet = lengthCtr;
                        try {
                            list.addAll(currentSet.getContentList(1, lengthThisSet));
                        }
                        catch (WGAPIException e) {
                            tmlContext.addwarning("Unable to retrieve content list from resultset. Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
                        }
                        break;
                    }
                }
            }
            
            return list;
        }
        
        public String getResultLanguage() {
            return resultLanguage;
         }

         public void addParam(String name, Object value, String type) {
             queryParameters.put(name, value);
         }

         protected Map getQueryParameters() {
             return queryParameters;
         }

         public ResultIterator getResultIterator() throws WGBackendException {
             List resultSetsCopy = new ArrayList(resultSets);
             return new ResultIterator(new ResultSetIterator(resultSetsCopy.iterator()), new ResultIterator.ResultCount() {
                 
                 @Override
                public int results() {
                     try {
                        return Status.this.results();
                    }
                    catch (WGBackendException e) {
                        WGFactory.getLogger().error("Exception retrieving query result size", e);
                        return 0;
                    }
                }
                 
             });
         }
         
         public int results() throws WGBackendException {
             Iterator resultSetIt = resultSets.iterator();
             int results = 0;
             while (resultSetIt.hasNext()) {
                 results += ((WGResultSet) resultSetIt.next()).results();
             }
             return results;

         }
         
         @Override
        public Object getTagInfo(String name) throws WGAPIException {

             if (name.equals("count")) {
                 int count = 0;
                 Iterator resultSetsIt = resultSets.iterator();
                 while (resultSetsIt.hasNext()) {
                     count += ((WGResultSet) resultSetsIt.next()).results();
                 }
                 return new Integer(count);
             }
             else if (name.equals("resultsets")) {
                 return new Integer(resultSets.size());
             }
             else if (name.equals("resultset"))  {
                 if (resultSets.size() > 0)  {
                     return resultSets.get(0);
                 }
                 else {
                     return null;
                 }
                    
             }
             
            return super.getTagInfo(name);
        }

    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
    
    public Status getStatus() {
        return (Status) super.getStatus();
    }
	/**
	 * @see Base#tmlStartTag()
	 */
	public void tmlStartTag() throws TMLException {
	    Status status = (Status) getStatus();
		status.resultSets = new ArrayList();
		status.queryParameters = new HashMap();
        status.resultLanguage = null;
	}

}

