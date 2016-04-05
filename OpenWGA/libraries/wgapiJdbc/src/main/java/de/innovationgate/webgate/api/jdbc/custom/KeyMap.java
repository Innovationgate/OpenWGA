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
package de.innovationgate.webgate.api.jdbc.custom;

import java.sql.Timestamp;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.commons.collections.map.ListOrderedMap;

import de.innovationgate.utils.WGUtils;

public class KeyMap extends ListOrderedMap {	

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    public static final DecimalFormat NUMBER_FORMATTER = new DecimalFormat("###0.##");

	/**
	 * @param key
	 */
	public KeyMap(JDBCSource jdbcSource, String key, String folder) throws ParseException {
		super();
		List keyColumns = (List) jdbcSource.getTables().get(folder);
		if (keyColumns == null) {
			throw new IllegalArgumentException("Table " + folder + " does not exist or has no primary key");
		}
		StringTokenizer tokens = new StringTokenizer(key, "--");
		int idx=0;
		while (tokens.hasMoreTokens()) {
			String token = tokens.nextToken();
			char dataType = token.charAt(0);
			String valueStr = token.substring(1);
			Object value = null;
			if (dataType == '#') {
				value = NUMBER_FORMATTER.parse(WGUtils.strReplace(valueStr, "D", ".", true));
			}
			else if (dataType == '*') {
				value = Timestamp.valueOf(valueStr);
			}
			else {
				value = valueStr;
			}
			put(keyColumns.get(idx), value);
			idx++;
		}
	
	}
	
	public KeyMap() {
		super();
	}

	/* (Kein Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		
		Iterator valuesIt = values().iterator();
		StringBuffer theKey = new StringBuffer();
		while (valuesIt.hasNext()) {
			Object value = valuesIt.next();
			if (value instanceof String) {
				theKey.append("$");
				theKey.append(String.valueOf(value));
			}
			else if (value instanceof Number) {
				theKey.append("#");
				theKey.append(WGUtils.strReplace(NUMBER_FORMATTER.format((Number) value), ".", "D", true));
			}
			else if (value instanceof Date) {
				theKey.append("*");
				theKey.append(String.valueOf(value));
			}
			else {
				theKey.append("~");
				theKey.append(String.valueOf(value));
			}
			
			if (valuesIt.hasNext()) {
				theKey.append("--");
			}
		}
		
		return theKey.toString();
		
	}

}
