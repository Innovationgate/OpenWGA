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

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.HibernateException;
import org.hibernate.Query;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGIteratingResultSetCore;
import de.innovationgate.webgate.api.WGResultSetCore;

public class WGStraightHQLResultSet extends WGStraightResultSet implements WGResultSetCore, WGIteratingResultSetCore {

    private List _contents = null;
    private List _columnNames;


    public WGStraightHQLResultSet(WGDatabaseImpl parent, Query query, Map<String,Object> queryParameters, Map<String,Object> queryOptions) throws WGAPIException {
        super(parent, query, queryParameters, queryOptions);
        
        String[] queryAliases = query.getReturnAliases();
        if (queryAliases != null) {
            _columnNames = Arrays.asList(queryAliases);
        }
    }

    /* (Kein Javadoc)
     * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList()
     */
    public List getContentList() throws WGAPIException, HibernateException {
        prepareContentList();
        return _contents;
    }

    /* (Kein Javadoc)
     * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList(int, int)
     */
    public List getContentList(int start, int length) throws WGAPIException, HibernateException {
        prepareContentList();
        return _contents.subList(start - 1, start - 1 + length);
    }



    public boolean isReturnsKeys() {
        return false;
    }

    public List getColumnNames() {
        return _columnNames;
    }



    private void prepareContentList() throws WGAPIException, HibernateException {
        if (_contents == null) {
            long timeBefore = System.currentTimeMillis();
            _contents = wrapEntities(getQuery().list().iterator());
            long timeAfter = System.currentTimeMillis();
            setExecutionTime(timeAfter - timeBefore);
        }    
    }

    public Iterator<? extends Object> getResultIterator() {
        return new HibernateQueryIterator(getQuery(), this);
    }
    
    @Override
    public boolean isLimitingResults() {
        return true;
    }
    
}
