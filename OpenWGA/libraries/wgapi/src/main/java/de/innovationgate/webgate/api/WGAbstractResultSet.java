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

import java.io.Closeable;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

/**
 * Common base class of WGAPI query result sets, defining shared functionality
 */
public abstract class WGAbstractResultSet implements WGResultSet, Iterable<WGContent> {

    protected WGDatabase db = null;
    protected WGResultSetCore core = null;

    public WGAbstractResultSet(WGDatabase db, WGResultSetCore core) {
        this.db = db;
        this.core = core;
    }

    protected WGContent fetchContentForRow(Object result) throws WGAPIException {
        
        WGContentKey contentKey;
        WGContent content = null;
        if (result instanceof WGContentKey) {
            contentKey = (WGContentKey) result;
            content = db.getContentByKey(contentKey);
        }
        else if (result instanceof WGContentQueryResult) {
            WGContentQueryResult queryResult = (WGContentQueryResult) result;
            if (queryResult.getParentStructKey() != null) {
                WGStructEntry parent = db.getStructEntryByKey(queryResult.getParentStructKey());
                if (parent != null && !parent.mayReadContent()) {
                    return null;
                }
            }
            else if (queryResult.getAreaName() != null) {
                WGArea area = db.getArea(queryResult.getAreaName());
                if (area != null && !area.mayReadContent()) {
                    return null;
                }
            }
            
            return fetchContentForRow(queryResult.getContentKey());
        }
        
        else if (result instanceof WGDocumentCore) {
            WGDocumentCore contentCore = (WGDocumentCore) result;
            if (contentCore == null || contentCore.isDeleted()) {
                return null;
            }
            if (contentCore.isTemporary()) {
                content = this.db.createContentObject(contentCore);
            }
            else {
                content = db.getOrCreateContentObject(contentCore);
                content.setCore(contentCore);
            }
            
            if (!content.isReadableForUser()) {
                return null;
            }
        }
        
        else if (result instanceof Object[]) {
            Object[] results = (Object[]) result;
            if (results.length >= 1 && (results[0] instanceof WGContentKey || results[0] instanceof WGContentQueryResult || results[0] instanceof WGDocumentCore)) {
                content = fetchContentForRow(results[0]);
                if (content != null) {
                    pushVirtualItems(results, 1, content);
                }
            }
            else {
                content = db.getDummyContent(db.getDefaultLanguage());
                if (content != null) {
                    pushVirtualItems(results, 0, content);
                }
            }
        }
        else if (result != null) {
            content = db.getDummyContent(db.getDefaultLanguage());
            pushVirtualItems(new Object[] {result}, 0, content);
        }
        return content;
    }

    private void pushVirtualItems(Object[] results, int startIndex, WGContent content) throws WGAPIException {
        
        List columnNames = core.getColumnNames();
        if (columnNames == null) {
            columnNames = Collections.emptyList();
        }
        
        for (int i = startIndex; i < results.length; i++) {
            Object result = results[i];
            if (columnNames.size() > i) {
                String columnName = (String) columnNames.get(i);
                if (columnName != null) {
                    content.setVirtualItemValue(columnName, result);
                    continue;
                }
                
            }
            
            content.setVirtualItemValue("column" + (i+1), result);
        }
        
    }

    protected WGResultSetCore getCore() {
        return core;
    }

    @Override
    public java.util.Iterator<WGContent> iterator() {
        try {
            return getResultIterator();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Exception retrieving result iterator", e);
        }
    };
    
    @Override
    public boolean isLimitingResultsInBackend() {
        return this.core.isLimitingResults();
    }
    
    /**
     * Closes backend resources for this result set, if any. The result set will not be usable afterwards.
     */
    public void close() throws IOException {
        if (core instanceof Closeable) {
            ((Closeable) core).close();
        }
    }

}
