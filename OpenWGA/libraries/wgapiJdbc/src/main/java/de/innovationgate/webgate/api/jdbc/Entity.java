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

package de.innovationgate.webgate.api.jdbc;

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGSystemException;

public abstract class Entity {
    
    private String id;
    /** collection */
    private Map<String,ExtensionData> attributes = new HashMap<String, ExtensionData>();

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Map<String, ExtensionData> getExtensionData() {
        return attributes;
    }

    public void setExtensionData(Map<String, ExtensionData> attributes) {
        this.attributes = attributes;
    }
    
    public Object readExtensionData(WGDatabaseImpl db, WGDocumentImpl doc, String name) throws WGAPIException {
        ExtensionData extData = getExtensionData().get(name);
        if (extData != null) {
            return WGDocumentImpl.readItemValue(db, doc, extData);
        }
        else {
            return null;
        }
    }
    
    public void writeExtensionData(WGDatabaseImpl db, WGDocumentImpl doc, String strName, Object value) throws WGAPIException {
        
        Map atts = getExtensionData();
        ExtensionData att = (ExtensionData) atts.get(strName);
        if (att == null) {
            ExtensionData newAtt = new ExtensionData();
            newAtt.setEntity((Entity) this);
            newAtt.setName(strName);
            ExtensionData oldItem = (ExtensionData) atts.put(newAtt.getName(), newAtt);
            att = newAtt;
        }

        WGDocumentImpl.writeItemValue(db, doc, att, value);
        
    }


}
