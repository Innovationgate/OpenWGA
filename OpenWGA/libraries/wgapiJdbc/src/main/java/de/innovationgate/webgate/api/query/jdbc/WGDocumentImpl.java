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
package de.innovationgate.webgate.api.query.jdbc;


import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;

public class WGDocumentImpl extends de.innovationgate.webgate.api.fake.WGFakeDocument {
	
	protected HashMap columns = new HashMap();

	protected Object key = null;
	
	public WGDocumentImpl(WGDatabase db, ResultSet resultSet) throws WGBackendException {
		super(db, WGDocument.TYPE_CONTENT);
		
		try {
			ResultSetMetaData metaData = resultSet.getMetaData();
			int columnCount = metaData.getColumnCount();
			Object value;
			for (int columnIdx=1; columnIdx <= columnCount; columnIdx++) {
				if (metaData.getColumnType(columnIdx) == Types.OTHER) {
					value = resultSet.getString(columnIdx);
				}
				else {
					value =  resultSet.getObject(columnIdx);
				}
				
				this.columns.put(metaData.getColumnName(columnIdx).toLowerCase(), value);
                /*
				if (columnIdx == 1) {
					this.key = value;
				}*/
			}
            this.key = new Integer(resultSet.getRow());
		}
		catch (SQLException e) {
			throw new WGBackendException("Error creating document.", e);
		}
		
		
		
	}
	
	/**
	 * @throws WGIllegalArgumentException 
	 * @throws WGSystemException 
	 * @throws WGBackendException 
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getMetaData(String)
	 */

	public Object getMetaData(String name) throws WGSystemException, WGIllegalArgumentException, WGBackendException {
		
			if (name.equals(WGContent.META_STRUCTENTRY)) {
				return this.key;
			}
			else if (name.equals(WGContent.META_STATUS)) {
				return WGContent.STATUS_RELEASE;
			}
			else if (name.equals(WGContent.META_TITLE)) {
				return this.key;
			}
			else if (name.equals(WGContent.META_VISIBLE)) {
				return new Boolean(true);
			}
			else {
				return super.getMetaData(name);
			}
	} 
	
	/**
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isTemporary()
	 */
	public boolean isTemporary() {
		return true;
	}

	/**
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getItemValue(String)
	 */
	public Object getItemValue(String strName) {
		
		return this.columns.get(strName.toLowerCase());
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getItemNames()
	 */
	public List getItemNames() {
		return new ArrayList(columns.keySet());
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#hasItem(java.lang.String)
	 */
	public boolean hasItem(String strName) {
		return columns.containsKey(strName.toLowerCase());
	}

}
