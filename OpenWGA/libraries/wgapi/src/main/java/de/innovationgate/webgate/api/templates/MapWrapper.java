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
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.fake.WGFakeDocument;

/**
 * A wrapper object that takes a java.util.Map as storage object and serves it's data as document core.
 * Is used by SimpleContentSource.
 */
public class MapWrapper extends BeanWrapper {

	private Map _map;

	/**
	 * Constructor. Not to be used outside WGAPI.
	 * @param db
	 * @param key
	 * @param map
	 * @param saved
	 * @param temporary
	 */
	public MapWrapper(SimpleContentSource db, BeanKey key, Map map, boolean saved, boolean temporary) {
		super(db, key, map, saved, temporary);
		_map = map;
	}
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getItemNames()
	 */
	public List getItemNames() {
		return new ArrayList(_map.keySet());
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getItemValue(java.lang.String)
	 */
	public Object getItemValue(String strName) {
		
		if (strName != null && isLowerCaseItems()) {
			strName = strName.toLowerCase();
		}
		
		return _map.get(strName);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getMetaData(java.lang.String)
	 */
	public Object getMetaData(String type) throws WGSystemException, WGIllegalArgumentException {
		if (type.equals(WGContent.META_STRUCTENTRY)) {
			return _key;
		}
        
        // See if we should return meta properties anyway. If not always return default
        if (!_db.getSpecs().isServePropertiesAsMetas()) {
            return WGFakeDocument.getMetaDataDefault(WGDocument.TYPE_CONTENT, type, _fakeLanguage);            
        }

		// Try to retrieve meta from bean. Else get default
		String typeLC = type.toLowerCase();
		if (_map.containsKey(typeLC)) {
			return _map.get(typeLC);
		}
		else {
			return WGFakeDocument.getMetaDataDefault(WGDocument.TYPE_CONTENT, type, _fakeLanguage);
		}
	} 

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#setItemValue(java.lang.String, java.lang.Object)
	 */
	public boolean setItemValue(String strName, Object value) {
		
		if (strName != null && isLowerCaseItems()) {
			strName = strName.toLowerCase();
		}
		
		// No Item-Lists allowed for SimpleContentSources
		if (value instanceof List) {
			value = ((List) value).get(0);
		}
		
		_map.put(strName, value);
		return true;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#setMetaData(java.lang.String, java.lang.Object)
	 */
	public boolean setMetaData(String name, Object value) {
        
        // See if we should return meta properties anyway. Cancel if not.
        if (!_db.getSpecs().isServePropertiesAsMetas()) {
            return false;            
        }
        
		_map.put(name.toLowerCase(), value);
		return true;
	}

}
