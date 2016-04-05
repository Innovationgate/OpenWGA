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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.ScrollableResults;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGIteratingResultSetCore;
import de.innovationgate.webgate.api.WGResultSetCore;

public class WGSQLResultSet extends WGStraightResultSet implements WGResultSetCore, WGIteratingResultSetCore {

	private List _contents;
	private WGDatabaseImpl _parent;
    public WGSQLResultSet(WGDatabaseImpl parent, Query query, Map<String,Object> queryParameters, Map<String,Object> queryOptions) throws WGAPIException {
	    super(parent, query, queryParameters, queryOptions);
		_parent = parent;
		//_columnNames = query.getReturnAliases(); Hibernate SQL queries do not support that
	}
	
	private void prepareContentsList() throws WGAPIException, HibernateException {
	    if (_contents == null) {
	        long timeBefore = System.currentTimeMillis();
	        _contents = wrapEntities(getQuery().iterate());
            long timeAfter = System.currentTimeMillis();
            setExecutionTime(timeAfter - timeBefore);
	    }
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList()
	 */
	public List getContentList() throws WGAPIException, HibernateException {
	    prepareContentsList();
		return _contents;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList(int, int)
	 */
	public List getContentList(int start, int length) throws WGAPIException, HibernateException {
		prepareContentsList();
		return _contents.subList(start - 1, start - 1 + length);
	}



    public boolean isReturnsKeys() {
        return false;
    }

    public Iterator<? extends Object> getResultIterator() {
        return new HibernateQueryIterator(getQuery(), this);
    }
    
    public int results() throws WGBackendException {
        try {
            String query = getQuery().getQueryString();
            
            if (query.contains("{")) {
                return getEntityQueryResults(query);
            }
            else {
                return getNormalQueryResults(query);
            }
            
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving query result size", e);
        }
        
    }

    private int getNormalQueryResults(String query) {
        ScrollableResults scroll = getQuery().scroll();
        scroll.last();
        return (scroll.getRowNumber() + 1);
    }

    private int getEntityQueryResults(String query) throws WGAPIException {
        String querySearchString = WGUtils.clearStrings(query, '\'', 'X').toLowerCase();
        
        int startIdx = querySearchString.indexOf(" from ");
        
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
        countQueryString = WGUtils.strReplace(countQueryString, "{", "", true);
        countQueryString = WGUtils.strReplace(countQueryString, "}", "", true);
        SQLQuery countQuery = getParent().getSession().createSQLQuery(countQueryString);
        injectQueryParams(countQuery, _parameters);
        return ((Number) countQuery.list().get(0)).intValue();
    }

    @Override
    public List getColumnNames() throws WGAPIException {
        return null;
    }
    
    @Override
    public boolean isLimitingResults() {
        return false;
    }
    


}
