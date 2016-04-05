/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * A generic {@link Comparator} for any object type.
 * The comparator compares by the following rules:
 * <ul>
 * <li> A null value is always larger than a non-null-value
 * <li> If both values are Strings they are compared by the collator given as constructor parameter
 * <li> If both values are {@link Comparable} they are compared based on this
 * <li> If they are neither strings nor comparables they are compared based on their {@link Object#toString()} strings using the collator
 * <li> If both objects are collections it compares the collection items value by value in iteration order
 * <li> If only one object is a collection the other object is converted to a collection with the object as single value
 * <li> If the collections are of unequal size and no difference could be found by the item positions
 *   that both have in common, the collection with larger size is returned as the bigger one
 * </ul>
 */
public class ObjectComparator extends Object implements Comparator<Object> {
    
    private Collator _collator;

    /**
     * Constructor. Takes a collator object for native language string comparison.
     * @param collator
     */
    public ObjectComparator(Collator collator) {
        _collator = collator;
    }

	/**
	 * @see Comparator#compare(Object, Object)
	 */
	public int compare(Object arg0, Object arg1) {

		List list0;
		List list1;

		if (WGUtils.isCollection(arg0)) {
			list0 = new ArrayList((Collection) arg0);
		}
		else {
			list0 = Collections.singletonList(arg0);
		}
		
		if (WGUtils.isCollection(arg1)) {
			list1 = new ArrayList((Collection) arg1);
		}
		else {
			list1 = Collections.singletonList(arg1);
		}
		
		Iterator values0 = list0.iterator();
		Iterator values1 = list1.iterator();
		int compareResult;
		
		while (values0.hasNext() && values1.hasNext()) {
			compareResult = this.compareObjects(values0.next(), values1.next());
			if (compareResult != 0) {
				return compareResult;
			}
		}
		
		if (values0.hasNext()) {
			return -1;
		}
		else if (values1.hasNext()) {
			return 1;
		}
		else {
			return 0;
		}

	}
	
	private int compareObjects(Object arg0, Object arg1) {
		
		if (arg0 == null && arg1 == null) {
			return 0;
		}
		
		if (arg0 == null) {
			return 1;
		}
		
		if (arg1 == null) {
			return -1;
		}
		
		if (arg0 instanceof String && arg1 instanceof String) {
            String str0 = (String) arg0;
            String str1 = (String) arg1;
            return compareStrings(str0, str1);
        }
        else if (arg0 instanceof Comparable && arg1 instanceof Comparable) {
		    
		    Comparable value0 = (Comparable) arg0;
			Comparable value1 = (Comparable) arg1;
		    
            // Use the parameter as compare base, whose class may be used for both params
			if (value1.getClass().isAssignableFrom(value0.getClass())) {
			    return (value1.compareTo(value0) * -1); 
			}
			else if (value0.getClass().isAssignableFrom(value1.getClass())) {
			    return value0.compareTo(value1);
			}
		}

		String str0 = String.valueOf(arg0);
		String str1 = String.valueOf(arg1);
		return compareStrings(str0, str1);

	}
    
    private int compareStrings(String str1, String str2) {
        return _collator.compare(str1, str2);
    }

}

