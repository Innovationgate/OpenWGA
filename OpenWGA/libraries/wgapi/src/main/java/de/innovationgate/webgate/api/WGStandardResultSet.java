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
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import de.innovationgate.utils.SkippingIterator;

/**
 * Represents the results of a query. This result set is used for result set cores that return content keys.
 * Core implementations of this object should retrieve only the documents that are really accessed via this object, so it is a good idea to restrict access to the documents really needed to avoid overhead.
 */
public class WGStandardResultSet extends WGAbstractResultSet {

	public class FetchingIterator implements Iterator<Object> {

	    private int _nextPageOffset = 0;
	    private int _inPageOffset = 0;
	    private List _page = null;
	    private boolean _endReached = false;
	    
	    public FetchingIterator() {
	    }
	    
        private void fetchCurrentResult() throws WGAPIException {
            while (!_endReached && (_page == null || _inPageOffset >= _page.size())) {
                if (core.results() < _nextPageOffset) {
                    _endReached = true;
                    return;
                }
                _page = core.getContentList(_nextPageOffset + 1, _fetchSize);
                _nextPageOffset+=_fetchSize;
                _inPageOffset = 0;
            }
        }


        public boolean hasNext() {
            try {
                fetchCurrentResult();
                return !_endReached;
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception iterating fetch result", e);
            }
        }

        public Object next() {
            try {
                fetchCurrentResult();
                if (!_endReached) {
                    Object result = _page.get(_inPageOffset);
                    _inPageOffset++;
                    return result;
                }
                else {
                    throw new NoSuchElementException();
                }
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception iterating fetch result", e);
            }
        }

        public void remove() {
            throw new UnsupportedOperationException();
            
        }

    }

    private Integer _resultLimit = null;	private Map _parameters = null;

    private boolean _enhanced;

    private int _fetchSize = 10;

    private Integer _resultSize = null;

    private String _query;

    private Boolean _hasResults = null;

	/**
	 * Constructor. Should not be used outside WGAPI.
	 * @param db
	 * @param core
	 * @param parameters
	 */
	public WGStandardResultSet(WGDatabase db, WGResultSetCore core, Map parameters, String query) {
	    super(db, core);
		this._parameters = parameters;
		this._query = query;
		
		_enhanced = (parameters == null || 
                !parameters.containsKey(WGDatabase.QUERYOPTION_ENHANCE) ||
                (String.valueOf(parameters.get(WGDatabase.QUERYOPTION_ENHANCE))).equals("true"));
		
		if (parameters.containsKey(WGDatabase.QUERYOPTION_FETCHSIZE)) {
		    try {
                _fetchSize = Integer.parseInt((String) parameters.get(WGDatabase.QUERYOPTION_FETCHSIZE));
            }
            catch (NumberFormatException e) {
                WGFactory.getLogger().error("Unable to parse query fetch size as integer: " + parameters.get(WGDatabase.QUERYOPTION_FETCHSIZE));
            }
		}
		
		if (this.core == null) {
			this.core = new WGResultSetCore() {
			
				public int results() { return 0; };
				public List getContentList() { return new ArrayList(); };
				public List getContentList(int start, int length) { return new ArrayList(); };
                public boolean isReturnsKeys() { return false; };
                public List getColumnNames() { return null; };
                public boolean isLimitingResults() { return true; };
                public boolean isReturnsParentKeys() { return false; }
			
			};
		}
	}
		
	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSet#getContentList()
	 */
	public WGContentList getContentList() throws WGAPIException {
		
		if (_resultLimit != null) {
			return fetchContents(1, _resultLimit.intValue());
		} 
		else {
			return fetchContents(-1, -1);
		}
		
	}

	private WGContentList fetchContents(int start, int length) throws WGAPIException {

		List queryResultsList;
		
		if (start == -1 && length == -1) {
		    queryResultsList = core.getContentList();
		}
		else {
		    queryResultsList = core.getContentList(start, length);
		}
		
        if (queryResultsList == null) {
            return new WGContentList();
        }

		Iterator queryResults = queryResultsList.iterator();

		ArrayList contents = new ArrayList();
		WGContentKey contentKey;
		
		int index = 0;
        int currentLength = 0;
		while (queryResults.hasNext()) {
		    Object result = queryResults.next();
            index++;		    
            if (index < start) {
                continue;
            }
            WGContent content = fetchContentForRow(result);
			if (content == null || content.isDeleted()) {
				continue;
			}

			if (content != null && (!_enhanced || content.isVisibleNow())) {
				contents.add(content);
                currentLength++;
                if (length != -1 && currentLength >= length) {
                    break;
                }
			}
		}

		return WGContentList.create(contents);
	}

    /* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSet#getContentList(int, int)
	 */
	public WGContentList getContentList(int start, int length) throws WGAPIException {
	    
	    // Workaround for frequent misuse. Index base is 1 (like in JDBC) not 0 (like on lists/arrays)
	    if (start == 0) {
	        start = 1;
	    }
	    
		return fetchContents(start, length);
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSet#results()
	 */
	public int results() throws WGBackendException {
	    
		if (_resultLimit != null) {
			return _resultLimit;
		}
		else {
            return getResultSize();
		}
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSet#limitResults(int)
	 */
	public void limitResults(int limit) throws WGBackendException {
	    
	    if (limit == 0) {
	        _resultLimit = null;
	    }
	    else if (limit == 1) {
	        _resultLimit = (hasResults() ? 1 : 0);
	    }
	    else if (limit < getResultSize()) {
			_resultLimit = new Integer(limit);
		}
	}
	
	


    public SkippingIterator<WGContent> getResultIterator() throws WGAPIException {
        
        // The best situation where the core is able to iterate results itself
        if (core instanceof WGIteratingResultSetCore) {
            Iterator theIterator = ((WGIteratingResultSetCore) core).getResultIterator();
            return new WGStandardResultSetIterator(this, theIterator, _enhanced, _resultLimit);
        }
        
        // If the core returns only content keys we assume its sure to return them all at once
        else if (core.isReturnsKeys()){
            return new WGStandardResultSetIterator(this,  core.getContentList(), _enhanced, _resultLimit);
        }
        
        // Otherwise we need to use a fetching iterator that uses the underlying getContentList() methods
        else {
            return new WGStandardResultSetIterator(this, new FetchingIterator(), _enhanced, _resultLimit);
        }
    }
    
    public boolean hasResults() throws WGBackendException {
        
        if (_resultSize != null) {
            return _resultSize > 0;
        }
        
        if (_hasResults == null) {
            if (!(core instanceof WGCachedResultSet)) {
                this.db.verboseBackendAccess(WGOperationKey.OP_QUERY_HAS_RESULTS, _query);
            }
            if (core instanceof WGIteratingResultSetCore) {
                _hasResults = ((WGIteratingResultSetCore) core).getResultIterator().hasNext();
    
            }
            else {
                _hasResults  = core.results() > 0;
            }
        }
        
        return _hasResults;
        
    }

    public long getExecutionTime() {
        if (getCore() instanceof WGReportingResultSetCore) {
            return ((WGReportingResultSetCore) getCore()).getExecutionTime();
        }
        else {
            return 0;
        }
    }

    private Integer getResultSize() {
        
        if (_resultSize == null) {
            try {
                if (!(this.core instanceof WGCachedResultSet)) {
                    this.db.verboseBackendAccess(WGOperationKey.OP_QUERY_RESULT_COUNT, _query);
                }
                _resultSize = this.core.results();
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Exception retrieving query result size", e);
                _resultSize = 0;
            }
        }
        
        return _resultSize;
        
    }
    
}
