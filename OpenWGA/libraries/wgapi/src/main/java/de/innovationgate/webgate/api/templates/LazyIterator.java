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

import java.util.ListIterator;

/**
 * Implements a lazy iterator that will retrieve the next data object to server just in time.
 * Can be used on a lazy bean list.
 */
public class LazyIterator implements ListIterator {
	
	private LazyBeanList _list;
	private int _currentPos = -1;
	private int _size;
	
	/**
	 * Constructor taking a lazy bean list and a beginning index.
	 * @param list The lsit
	 * @param beginIndex The index where to begin inside the list.
	 */
	public LazyIterator(LazyBeanList list, int beginIndex) {
		if (beginIndex > 0) {
			_list = (LazyBeanList) list.subList(beginIndex, list.size());
		}
		else {
			_list = list;
		}
		_size = _list.size();
		
	}
	
	/**
	 * Constructor taking a LazyBeanList.
	 * @param list
	 */
	public LazyIterator(LazyBeanList list) {
		this(list, 0);
	}

	/* (Kein Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return _currentPos < (_size - 1);
	}

	/* (Kein Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public Object next() {
		if (hasNext()) {
			_currentPos++;
			return _list.get(_currentPos);
		}
		else {
			return null;
		}
	}

	/* (Kein Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
	}

	/* (Kein Javadoc)
	 * @see java.util.ListIterator#add(java.lang.Object)
	 */
	public void add(Object arg0) {
	}

	/* (Kein Javadoc)
	 * @see java.util.ListIterator#hasPrevious()
	 */
	public boolean hasPrevious() {
		return _currentPos > 0;
	}

	/* (Kein Javadoc)
	 * @see java.util.ListIterator#nextIndex()
	 */
	public int nextIndex() {
		if (hasNext()) {
			return _currentPos + 1;
		}
		else {
			return _size;
		}
		
	}

	/* (Kein Javadoc)
	 * @see java.util.ListIterator#previous()
	 */
	public Object previous() {
		if (hasPrevious()) {
			_currentPos--;
			return _list.get(_currentPos);
		}
		else {
			return null;
		}
	}

	/* (Kein Javadoc)
	 * @see java.util.ListIterator#previousIndex()
	 */
	public int previousIndex() {
		if (hasPrevious()) {
			return _currentPos - 1;
		}
		else {
			return -1;
		}
	}

	/* (Kein Javadoc)
	 * @see java.util.ListIterator#set(java.lang.Object)
	 */
	public void set(Object arg0) {
	}

}
