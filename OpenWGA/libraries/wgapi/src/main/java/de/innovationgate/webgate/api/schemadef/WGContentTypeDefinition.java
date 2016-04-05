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

package de.innovationgate.webgate.api.schemadef;

import java.util.ArrayList;
import java.util.List;

import org.simpleframework.xml.ElementList;

import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDocument;

/**
 * Definition of a content type
 */
public class WGContentTypeDefinition extends WGSchemaDocumentDefinition {
    
    @ElementList(name="contentItemDefinitions")
    private List<WGContentItemDefinition> _contentItemDefinitions = new ArrayList<WGContentItemDefinition>();
    
    @ElementList(name="contentMetaDefinitions")
    private List<WGMetaFieldDefinition> _contentMetaDefinitions = new ArrayList<WGMetaFieldDefinition>();

    /**
     * Returns definitions of items for content documents of this content type
     */
    public List<WGContentItemDefinition> getContentItemDefinitions() {
        return _contentItemDefinitions;
    }

    /**
     * Sets definitions of items for content documents of this content type
     */
    public void setContentItemDefinitions(List<WGContentItemDefinition> itemDefinitions) {
        _contentItemDefinitions = itemDefinitions;
    }
    
    /**
     * Adds a new content item definition to the schema
     */
    public void addContentItemDefinition(WGContentItemDefinition itemDef) {
        _contentItemDefinitions.add(itemDef);
    }

    @Override
    public int getDocumentType() {
        return WGDocument.TYPE_CONTENTTYPE;
    }

    /**
     * Returns predefined metadata field values for this content type
     */
    public List<WGMetaFieldDefinition> getContentMetaDefinitions() {
        return _contentMetaDefinitions;
    }

    /**
     * Sets metadata field definitions to the schema
     */
    public void setContentMetaDefinitions(List<WGMetaFieldDefinition> contentMetaDefinitions) {
        _contentMetaDefinitions = contentMetaDefinitions;
    }
    
    /**
     * Adds a new metadata field definition to the schema
     */
    public void addContentMetaDefinition(WGMetaFieldDefinition metaDef) {
        _contentMetaDefinitions.add(metaDef);
    }


}
