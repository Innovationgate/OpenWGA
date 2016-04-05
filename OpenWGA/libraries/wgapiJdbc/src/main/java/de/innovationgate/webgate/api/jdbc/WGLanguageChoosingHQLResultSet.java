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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.HibernateException;
import org.hibernate.Query;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGIteratingResultSetCore;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGResultSetCore;

public class WGLanguageChoosingHQLResultSet extends HibernateResultSet implements WGIteratingResultSetCore {

    private List<WGContentKey> _contents;
    private List<String> _columnNames;

    public WGLanguageChoosingHQLResultSet(WGDatabaseImpl wgDatabaseImpl, Query hibQuery, Map<String,Object> queryParameters, Map<String,Object> queryOptions, List<WGLanguage> queryLanguages) throws WGBackendException{
        super(wgDatabaseImpl, hibQuery, queryParameters, queryOptions);
        try {
            
            String[] queryAliases = hibQuery.getReturnAliases();
            if (queryAliases != null) {
                _columnNames = Arrays.asList(queryAliases);
            }
            
            long timeBefore = System.currentTimeMillis();
            
            _contents = getQuery().list();
            
            // Build maps per page
            Map<Object,Map<String, WGContentKey>> pages = new HashMap<Object,Map<String, WGContentKey>>();
            for (WGContentKey key : _contents) {
                Map<String,WGContentKey> pageMap = pages.get(key.getStructKey());
                if (pageMap == null) {
                    pageMap = new HashMap<String,WGContentKey>();
                    pages.put(key.getStructKey(), pageMap);
                }
                pageMap.put(key.getLanguage(), key);
            }
            
            // Choose content versions to return
            List<WGContentKey> contentsToReturn = new ArrayList<WGContentKey>();
            for (Map<String,WGContentKey> pageMap : pages.values()) {
                WGContentKey chosenContent = null;
                if (pageMap.size() == 1) {
                    chosenContent = pageMap.values().iterator().next();
                }
                else {
                    for (WGLanguage lang : queryLanguages) {
                        WGContentKey conKey = pageMap.get(lang.getName());
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
            
            long timeAfter = System.currentTimeMillis();
            setExecutionTime(timeAfter - timeBefore);
        }
        catch (WGNotSupportedException e) {
            throw new WGBackendException("Exception creating language choosing resultset", e);
        }
        catch (WGAPIException e) {
            throw new WGBackendException("Exception creating language choosing resultset", e);
        }
        
    }

    public List getColumnNames() {
        return _columnNames;
    }

    public List getContentList(int start, int length) throws WGAPIException {
        return _contents.subList(start - 1, start - 1 + length);
    }

    public List getContentList() throws WGAPIException {
        return _contents;
    }

    public boolean isReturnsKeys() {
        return true;
    }

    public Iterator<? extends Object> getResultIterator() {
        return _contents.iterator();
    }
    
    @Override
    protected Object wrapEntity(Object entity) {
        return entity;
    }

    @Override
    public int results() {
        return _contents.size();
    }
    
    @Override
    public boolean isLimitingResults() {
        return true;
    }

}
