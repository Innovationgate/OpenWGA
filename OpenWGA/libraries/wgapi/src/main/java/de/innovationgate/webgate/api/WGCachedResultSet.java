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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.PrefetchingIterator;

/**
 * Represents a query result set that can be cached. Therefor all the content keys of the result content are extracted.
 */
public class WGCachedResultSet implements WGIteratingResultSetCore {
    
    private List<WGContentQueryResult> _resultKeys = new ArrayList<WGContentQueryResult>();
	
	/**
	 * Creates a cached result set that will cache the contents that is given as result set core
	 * @param resultSetCore The result set core, whose content result is cached
	 * @throws WGAPIException 
	 */
	public WGCachedResultSet(WGResultSetCore resultSetCore) throws WGAPIException {
	    
		if (resultSetCore == null) {
		    return;
		}
	    
		Iterator<?> contents;
		if (resultSetCore instanceof WGIteratingResultSetCore) {
		    contents = ((WGIteratingResultSetCore) resultSetCore).getResultIterator();
		}
		else {
		    contents = resultSetCore.getContentList().iterator();
		}
        
        WGDocumentCore contentCore;
        while (contents.hasNext()) {
            Object resultObject = contents.next();
            WGContentQueryResult qr = fetchQueryResult(resultObject);
            if (qr != null) {
                _resultKeys.add(qr);
            }
        }    

	}



    protected WGContentQueryResult fetchQueryResult(Object resultObject) throws WGAPIException, WGIllegalArgumentException {
        WGDocumentCore contentCore;
        WGContentQueryResult qr = null;
        if (resultObject instanceof Object[]) {
            Object[] objects = (Object[]) resultObject;
            if (objects.length >= 1) {
                qr = fetchQueryResult(objects[0]);
            }
        }
        else if (resultObject instanceof WGContentKey) {
            qr = new WGContentQueryResult((WGContentKey) resultObject);
        }
        else if (resultObject instanceof WGContentQueryResult) {
            qr = (WGContentQueryResult) resultObject;
        }
        else if (resultObject instanceof WGDocumentCore) {
            contentCore = (WGDocumentCore) resultObject;
            WGContentKey key = WGContentKey.create(contentCore, true);
            if (key.isValid()) {
                qr = new WGContentQueryResult(key);
            }
        }
        else {
            throw new WGIllegalArgumentException("Cached queries are not supported with virtual resultsets that return no actual content documents");
        }
        return qr;
    }
	

	
	public WGCachedResultSet(List<WGContentKey> keys) {
	    for (WGContentKey key : keys) {
	        _resultKeys.add(new WGContentQueryResult(key));
	    }
	}
	
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#results()
	 */
	public int results() {
	    return _resultKeys.size();
	}
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList(int, int)
	 */
	public List<? extends Object> getContentList(int start, int length) throws WGAPIException {
		return _resultKeys.subList(start - 1, start - 1 + length);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList()
	 */
	public List<? extends Object> getContentList() throws WGAPIException {
	    return _resultKeys;
	}

	public List<? extends Object> getColumnNames() {
        return Collections.emptyList();
    }

    public boolean isReturnsKeys() {
        return true;
    }

    public Iterator<WGContentQueryResult> getResultIterator() {
        return _resultKeys.iterator();
    }
    
    public boolean isLimitingResults() {
        return true;
    }
}
