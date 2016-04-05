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
package de.innovationgate.webgate.api.fake;

import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGTMLModule;

public class WGFakeTML extends WGFakeDocument {
    

	private String name;
	private String mediakey;

	public WGFakeTML(WGDatabase db, String name, String mediakey) {
		super(db, WGDocument.TYPE_TML);
		this.name = name;
		this.mediakey = mediakey;
	}

	/**
	 * @throws WGIllegalArgumentException 
	 * @throws WGSystemException 
	 * @throws WGBackendException 
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getMetaData(String)
	 */
	public Object getMetaData(String type) throws WGSystemException, WGIllegalArgumentException, WGBackendException {
	
		if (type.equals(WGTMLModule.META_DESCRIPTION)) {
		    return "This is a temporary area of name '" + name + "' that was created to maintain referential integrity because some document is linked to this name."; 
		}
		else if (type.equals(WGTMLModule.META_NAME)) {
			return name;
		}
		else if (type.equals(WGTMLModule.META_MEDIAKEY)) {
			return mediakey;
		}
		else if (type.equals(WGTMLModule.META_CODE)) {
			if (name.equals(FAKENAME_OUTERTML)) {
			    return "<tml:innerlayout/>";
			}
			else {
			    return "";
			}
		}
		else {
			return super.getMetaData(type); 
		}

	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isTemporary()
	 */
	public boolean isTemporary() {
		return true;
	}
}
