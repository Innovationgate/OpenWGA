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
package de.innovationgate.wgpublisher.webtml.utils;

import java.text.Collator;

import de.innovationgate.utils.ObjectComparator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.wgpublisher.webtml.Base;

public class ContentComparator extends ObjectComparator {
	
	private Base _tag;

	private String _name;

	private boolean _meta;

	public ContentComparator(Base tag, boolean meta, String name, Collator collator) {
        super(collator);
		_tag = tag;
		_meta = meta;
		_name = name;
	}

	/* (Kein Javadoc)
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	public int compare(Object o1, Object o2) {
		
		WGContent content1 = (WGContent) o1;
		WGContent content2 = (WGContent) o2;
		TMLContext context1;
        TMLContext context2;
        
        context1 = _tag.getTMLContextForDocument(content1);
        context2 = _tag.getTMLContextForDocument(content2);
    
		
		 
		Comparable obj1 = null;
		Comparable obj2 = null;
		
		if (_meta) {
            try {
                obj1 = (Comparable) context1.meta(_name);
                obj2 = (Comparable) context2.meta(_name);
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Unable to compare content.", e);
            }
		}
		else {
            try {
    			obj1 = (Comparable) context1.item(_name);
    			obj2 = (Comparable) context2.item(_name);
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Unable to compare content.", e);
            }
		}
        
//       If the comparables are strings, we don't want them to be sorted case-sensitive
        if (obj1 instanceof String) {
            obj1 = ((String) obj1).toLowerCase();
        }
        if (obj2 instanceof String) {
            obj2 = ((String) obj2).toLowerCase();
        }
		
        return super.compare(obj1, obj2);
		
		
		
	}
}
