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

package de.innovationgate.wga.server.api;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAbstractResultSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Context;

/**
 * Represents an object carrying all resulting data of an executed query.
 * Use methods {@link #getPage(int, int)}, {@link #getFirstResult()}, the {@link #iterator()} or the backend result set via {@link #getSet()} to retrieve results.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class QueryResult extends CollectionResult {
    
    private WGResultSet _set;
    private boolean _servedFromCache;
    private String _fullQuery;
    private Map<String, Object> _outputParams;
    private String _queryType;
    private String _queryString;

    protected QueryResult(WGA wga, WGResultSet set, String fullQuery, boolean servedFromCache, Map<String,Object> outputParameters, String queryType, String queryString) {
        super(wga, null);
        _set = set;
        _fullQuery = fullQuery;
        _servedFromCache = servedFromCache;
        _outputParams = outputParameters;
        _queryType = queryType;
        _queryString = queryString;
    }

    /**
     * Returns the WGAPI result set of the query result containing the resulting contents
     */
    public WGResultSet getSet() throws WGException {
        return _set;
    }

    /**
     * Returns if the query result was served from the WGAPI query cache
     * When attribute "cache:true" was given to the query then the result may be served from the query cache. This property being true indicates that it was served like that. This property being false means that the cache did not exist or was stale and that the query had to be executed. The cache may have been filled by this execution.
     * This is the same as retrieving taginfo "cacheused" from <tml:query>.
     * 
     */
    public boolean isServedFromCache() throws WGException {
        return _servedFromCache;
    }

    /**
     * Returns the full query that was executed
     * In order to enforce additional parameters in effect for a query WebTML may further enhance the query string that was given to the {@link Database#query(String, String)} method internally. This property returns the full query like it was executed on the database backend.
     * This is the same as retrieving taginfo "fullquery" from <tml:query>.
     */
    public String getFullQuery() throws WGException {
        return _fullQuery;
    }

    /**
     * Returns all output parameters of the query
     * Special query types may have special "output parameters", i.e. parameters that are given back from the query to the caller. This Lookup table may be used to retrieve them by their names as keys.
     */
    public Map<String, Object> getOutputParams() throws WGException {
        return _outputParams;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.server.api.CollectionResult#getFirstResultContent()
     */
    @Override
    public WGContent getFirstResultContent() throws WGException {
        
        if (_set instanceof WGAbstractResultSet) {
            Iterator<WGContent> it = ((WGAbstractResultSet) _set).iterator();
            if (it.hasNext()) {
                return it.next();
            }
        }
        else {
            List<WGContent> list =_set.getContentList(1, 1);
            if (list.size() > 0) {
                return list.get(0);
            }
        }
        
        return null;
        
    }

    /**
     * Iterators over the query results in the form of WebTML context objects
     */
    @Override
    public SkippingIterator<Context> iterator() {
        
        try {
            return _wga.wrapIntoTMLContextIterator(_set.getResultIterator());
        }
        catch (WGException e) {
            throw new RuntimeException("Exception creating result set iterator", e);
        }
    }
    
    /**
     * Returns the number of contents in this query result
     */
    @Override
    public int getSize() throws WGException {
        return _set.results();
    }
    
    /**
     * This method may return the execution time of the backend query in milliseconds. Returns 0 if not supported.
     */
    public long getExecutionTime() throws WGException {
        return _set.getExecutionTime();
    }

    @Override
    protected String getDescription() {
        return "Query of type \"" + _queryType + "\": " + _queryString;
    }

}
