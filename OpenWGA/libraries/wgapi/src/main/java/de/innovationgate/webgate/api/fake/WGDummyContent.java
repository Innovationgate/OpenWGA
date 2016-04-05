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
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGSystemException;

public class WGDummyContent extends WGFakeDocument {

    private String _language;
    private String _structkey;

    public WGDummyContent(WGDatabase db, String language) {
        super(db, WGDocument.TYPE_CONTENT);
        _language = language;
        _structkey = "fakedoc-" + hashCode();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.fake.WGFakeDocument#getMetaData(java.lang.String)
     */
    public Object getMetaData(String name) throws WGSystemException, WGIllegalArgumentException, WGBackendException {
        
        if (name.equals(WGContent.META_LANGUAGE)) {
            return _language;
        }
        else if (name.equals(WGContent.META_STRUCTENTRY)) {
            return _structkey;
        }
        else {
            return super.getMetaData(name);
        }
        
    }
 
}
