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

import java.util.Collection;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hibernate.HibernateException;
import org.hibernate.Query;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGReportingResultSetCore;

public abstract class HibernateResultSet implements WGReportingResultSetCore {
    
    
    private static final Pattern PATTERN_FROM_INSIDE = Pattern.compile("\\sfrom\\s");
    private static final Pattern PATTERN_FROM_AT_START = Pattern.compile("^from\\s");

    public static void injectQueryParams(Query hibQuery, Map map) throws WGAPIException {
        
        if (map == null || map.size() == 0) {
            return;
        }
        
        String[] pNames = hibQuery.getNamedParameters();
        for (int i = 0; i < pNames.length; i++) {
            String pName = pNames[i];
            Object value = map.get(pName);
            if (value != null) {
                if (value instanceof WGDocument) {
                    value = ((WGDocument) value).getNativeObject();
                }
                if (value instanceof Double) {
                    hibQuery.setDouble(pName, (Double) value);
                }
                else if (value instanceof Collection<?>) {
                    hibQuery.setParameterList(pName, (Collection<?>) value);
                }
                else {
                    hibQuery.setParameter(pName, value);
                }
            }
        }
        
    }
    
    private WGDatabaseImpl _parent;
    private Query _query;
    private long _executionTime;
    protected Map<String, Object> _parameters;
    private Integer _results = null;
    private Map<String, Object> _options;
    
    public HibernateResultSet(WGDatabaseImpl parent, Query query, Map<String, Object> queryParameters, Map<String,Object> queryOptions) {
        _parent = parent;
        _query = query;
        _parameters = queryParameters;
        _options = queryOptions;
    }
    
    protected abstract Object wrapEntity(Object entity) throws WGAPIException;

    protected WGDatabaseImpl getParent() {
        return _parent;
    }

    protected Query getQuery() {
        return _query;
    }
    
    public int results() throws WGBackendException {
        try {
            
            if (_results == null) {
                _results = fetchResultCount();
            }
            
            return _results;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving query result size", e);
        }
        
    }

    private int fetchResultCount() throws WGAPIException {
        String query = getQuery().getQueryString();
        
        String querySearchString = WGUtils.clearStrings(query, '\'', 'X').toLowerCase();
        
        int startIdx = -1;
        Matcher fromAtStartMatcher = PATTERN_FROM_AT_START.matcher(querySearchString);
        if (fromAtStartMatcher.find()) {
            startIdx = 0;
        }
        else {
            Matcher fromMatcher = PATTERN_FROM_INSIDE.matcher(querySearchString);
            if (fromMatcher.find()) {
                startIdx = fromMatcher.start();
            }
        }
        
        // Cannot create count query if we have no regular select/from query
        if (startIdx == -1) {
            return 0;
        }
        
        int orderIdx = querySearchString.indexOf("order by");
        int groupIdx = querySearchString.toLowerCase().indexOf("group by");
        
        int endIdx = query.length();
        if (orderIdx != -1) {
            endIdx = orderIdx;
        }
        if (groupIdx != -1 && groupIdx < endIdx) {
            endIdx = groupIdx;
        }

        String countQueryString = "select count(*) " + query.substring(startIdx, endIdx);
        Query countQuery = getParent().getSession().createQuery(countQueryString);
        injectQueryParams(countQuery, _parameters);
        int countResults = ((Number) countQuery.iterate().next()).intValue();
        
        // Look if we are beyond max results, if so return it (#00001818)
        if (_options.containsKey(WGDatabase.QUERYOPTION_MAXRESULTS)) {
            Integer maxResults = (Integer) _options.get(WGDatabase.QUERYOPTION_MAXRESULTS);
            if (maxResults != null && maxResults.intValue() < countResults) {
                return maxResults.intValue();
            }
            
        }
        
        return countResults;
        
    }

    public long getExecutionTime() {
        return _executionTime;
    }

    protected void setExecutionTime(long executionTime) {
        _executionTime = executionTime;
    }
    

}
