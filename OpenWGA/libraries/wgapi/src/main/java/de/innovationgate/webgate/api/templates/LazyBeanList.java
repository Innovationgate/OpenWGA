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
package de.innovationgate.webgate.api.templates;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.apache.commons.collections.map.LinkedMap;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGResultSetCore;

/**
 * Implements a "lazy" list of retrieved beans.
 * This list can first be filled with the keys of beans. When beans are used, they are retrieved by their keys automatically.
 * However beans that never get used will not be retrieved, saving performance and resources. 
 */
public class LazyBeanList implements List, WGResultSetCore {
	
	private String _folder;
	private SimpleContentSource _db;
	private Map _contents;
	private List _keys;
	
	public LazyBeanList(SimpleContentSource db, String folder, Map contents) {
		
		_db = db;
		_folder = folder;
		_keys = new ArrayList(contents.keySet());
		_contents = contents;
		 
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#size()
	 */
	public int size() {
		return _keys.size();
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#isEmpty()
	 */
	public boolean isEmpty() {
		return _contents.isEmpty();
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#contains(java.lang.Object)
	 */
	public boolean contains(Object arg0) {
		try {
            fetchAll();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Unable to execute internal fetchAll() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
        }
		return _contents.containsValue(arg0);
	}

	/**
	 * @throws WGAPIException 
	 * 
	 */
	private void fetchAll() throws WGAPIException {
		Iterator keys = _keys.iterator();
		while (keys.hasNext()) {
			fetch(keys.next());
		}
		
	}

	/**
	 * @param object
	 * @throws WGAPIException 
	 */
	private Object fetch(Object key) throws WGAPIException {
	
		Object bean = _contents.get(key);
		if (bean == null) {
			bean = _db.getContent(_folder, key);
			_contents.put(key, bean);
		}
		return _db.createWrapper(new BeanKey(_folder, key), bean, true);
		
	} 

	/* (Kein Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator iterator() {
		return new LazyIterator(this);
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#toArray()
	 */
	public Object[] toArray() {
		try {
            fetchAll();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Unable to execute internal fetchAll() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
        }
		return _contents.values().toArray();
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#toArray(java.lang.Object[])
	 */
	public Object[] toArray(Object[] arg0) {
		try {
            fetchAll();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Unable to execute internal fetchAll() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
        }
		return _contents.values().toArray(arg0);
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Object arg0) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object arg0) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection arg0) {
		try {
            fetchAll();
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Unable to execute internal fetchAll() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
        }
		return _contents.values().containsAll(arg0);
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection arg0) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see java.util.List#addAll(int, java.util.Collection)
	 */
	public boolean addAll(int arg0, Collection arg1) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection arg0) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection arg0) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see java.util.Collection#clear()
	 */
	public void clear() {
	}

	/* (Kein Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Object get(int arg0) {
		try {
            return fetch(_keys.get(arg0));
        }
        catch (WGAPIException e) {
            throw new IllegalStateException("Unable to execute internal fetch() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
        }
	}

	/* (Kein Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Object set(int arg0, Object arg1) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int arg0, Object arg1) {
	}

	/* (Kein Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Object remove(int arg0) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object arg0) {
		Iterator keys = _keys.iterator();
		Object key;
		Object bean;
		while (keys.hasNext()) {
			key = keys.next();
			try {
                bean = fetch(key);
            }
            catch (WGAPIException e) {
                throw new IllegalStateException("Unable to execute internal fetch() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            }
			if (bean.equals(arg0)) {
				return _keys.indexOf(key);
			}
		}
		return -1;
	}

	/* (Kein Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object arg0) {
		ListIterator keys = _keys.listIterator();
		Object key;
		Object bean;
		while (keys.hasPrevious()) {
			key = keys.previous();
			try {
                bean = fetch(key);
            }
            catch (WGAPIException e) {
                throw new IllegalStateException("Unable to execute internal fetch() bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            }
			if (bean.equals(arg0)) {
				return _keys.indexOf(key);
			}
		}
		return -1;
	}

	/* (Kein Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator listIterator() {
		return new LazyIterator(this);
	}

	/* (Kein Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator listIterator(int arg0) {
		return new LazyIterator(this, arg0);
	}

	/* (Kein Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List subList(int arg0, int arg1) {
		
		Iterator keys = _keys.subList(arg0, arg1).iterator();
		Map subMap = new LinkedMap();
		Object key;
		while (keys.hasNext()) {
			key = keys.next();
			subMap.put(key, _contents.get(key));
		}
		
		return new LazyBeanList(_db, _folder, subMap);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#results()
	 */
	public int results() {
		return size();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList(int, int)
	 */
	public List getContentList(int start, int length) {
		int toIdx = start - 1 + length;
		if (toIdx > size()) {
		    toIdx = size();
		}
        return subList(start - 1, toIdx);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGResultSetCore#getContentList()
	 */
	public List getContentList() {
		return this;
	}

    public boolean isReturnsKeys() {
        return false;
    }

    public List getColumnNames() {
        return null;
    }
    
    public boolean isLimitingResults() {
        return false;
    }

}
