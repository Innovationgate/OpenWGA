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

import java.util.Collection;
import java.util.Vector;

/**
 * List of content objects.
 */
public class WGContentList extends Vector<WGContent> {
	


	/**
     * 
     */
    private static final long serialVersionUID = 1L;



    /**
	 * Private constructor to allow creation only via WGContentList.create methods.
	 * @param col Collection of content objects to convert into a WGContentList
	 */
	private WGContentList(Collection<WGContent> col) {
		super(col);
	}
	
	/**
	 * Constructor for empty list.
	 */
	public WGContentList() {
		super();
	}
	
	/**
	 * Creates a content list object based on a given collection of content objects.
	 * @param col Collection of content objects
	 * @return WGContentList
	 */
	public static WGContentList create(Collection<WGContent> col) {
		return new WGContentList(col);
	}
	
	/**
	 * Creates an empty content list.
	 */
	public static WGContentList create() {
	    return new WGContentList();
	}



	/**
	 * Creates a content list clone. (Like clone() was meant to be, returns a correctly casted objects though, so a little work spared...)
	 * @return WGContentList
	 */
	public WGContentList cloneContentList() {
		return new WGContentList((Vector<WGContent>) super.clone());
	}


}
