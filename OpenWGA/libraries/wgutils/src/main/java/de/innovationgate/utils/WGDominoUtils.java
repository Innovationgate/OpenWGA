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


/**
 * Some collected utilites for the using Lotus Domino APIs.
 */
public abstract class WGDominoUtils {	
	
	/**
     * Converts a collection of lotus.domino.DateTime objects to the
     * corresponding java.util.Date objects
	 * @param col List of lotus.domino.DateTime objects
	 * @return List of java.util.Date objects
	 */
	public static java.util.ArrayList toJavaDate(java.util.List col) {

		java.util.ArrayList converted = new java.util.ArrayList(col.size());
		java.util.Iterator values = col.iterator();

		try {
			while (values.hasNext()) {
				lotus.domino.DateTime dt = (lotus.domino.DateTime) values.next();
                if (!dt.getGMTTime().equals("")) {
                    converted.add((dt).toJavaDate());
                }
			}
			return converted;
		}
		catch (lotus.domino.NotesException exc) {
			System.out.println("Error converting domino date: " + exc.text);
			return null;
		}
	}


}
