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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.hibernate.Hibernate;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;

/**
 * A handler for extension data on entities
 */
public class EntityExtDataHandler implements WGExtensionDataContainer {
    
    private final WGDocumentImpl _doc;
    private Entity _edEntity;
    private WGDatabaseImpl _dbCore;


    /**
     * @param wgDocumentImpl Then enclosing document of the entity. Specify null if there is none
     * @param dbCore The database core
     * @param fileEntity The entity
     */
    public EntityExtDataHandler(WGDocumentImpl wgDocumentImpl, WGDatabaseImpl dbCore, Entity entity) {
        _doc = wgDocumentImpl;
        _dbCore = dbCore;
        _edEntity = entity;
    }

    public Object getExtensionData(String strName) throws WGAPIException {
        strName = strName.toLowerCase().trim();
        ExtensionData att = _edEntity.getExtensionData().get(strName);
        
        if (att != null) {
            return WGDocumentImpl.readItemValue(_dbCore, _doc, att);
        }
        else {
            return null;
        }
    }

    public List<String> getExtensionDataNames() throws WGAPIException {
        return new ArrayList<String>(_edEntity.getExtensionData().keySet());
    }

    public void removeExtensionData(String name) throws WGAPIException {
        if (_doc != null) {
            _doc.makeEditable();
        }
        name = name.toLowerCase().trim();
        ExtensionData item = (ExtensionData) _edEntity.getExtensionData().get(name);
        if (item != null) {
            Hibernate.initialize(_edEntity.getExtensionData());
            Item oldItem = (Item) _edEntity.getExtensionData().remove(name);
        }
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        if (_doc != null) {
            _doc.makeEditable();
        }
        strName = strName.toLowerCase().trim();

        // Convert numbers to doubles
        if (value instanceof Number && !(value instanceof Double)) {
            value = new Double(((Number) value).doubleValue());
        }

        Map atts = _edEntity.getExtensionData();
        ExtensionData att = (ExtensionData) atts.get(strName);
        if (att == null) {
            ExtensionData newAtt = new ExtensionData();
            newAtt.setEntity((Entity) _edEntity);
            newAtt.setName(strName);
            ExtensionData oldItem = (ExtensionData) atts.put(newAtt.getName(), newAtt);
            att = newAtt;
        }

        WGDocumentImpl.writeItemValue(_dbCore, _doc, att, value);
    }
    
}