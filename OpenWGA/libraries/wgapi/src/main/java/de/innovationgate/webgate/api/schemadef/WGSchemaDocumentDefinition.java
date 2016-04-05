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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.Transient;

import de.innovationgate.webgate.api.WGDocumentKey;

/**
 * The definition of a schema document that is used in the OpenWGA Content Store. The document type is determined by the individual subclass of this class.
 */
@Root(strict=false)
public abstract class WGSchemaDocumentDefinition {
    
    @Attribute(name="name")
    private String _name;
    
    @Attribute(name="autoCreate")
    private boolean _autoCreate =  true;
    
    @ElementList(name="metadata")
    private List<WGMetaFieldDefinition> _metadata = new ArrayList<WGMetaFieldDefinition>();


    @Transient
    private Map<String,WGMetaFieldDefinition> _metadataCache;
    
    /**
     * Returns the type number of the document type of this definition
     */
    public abstract int getDocumentType();
    
    /**
     * Returns the document key that the document created on behalf of this definition will have
     */
    public WGDocumentKey getDocumentKey() {
        WGDocumentKey docKey = new WGDocumentKey(getDocumentType(), getName(), null);
        return docKey;
    }

    /**
     * Returns a map of the metadata field definitions of this schema, mapped by their name
     */
    public Map<String, WGMetaFieldDefinition> getMetadataCache() {
        return _metadataCache;
    }
    
    /**
     * Adds a metadata definition
     */
    public void addMetadata(WGMetaFieldDefinition metadata) {
        _metadata.add(metadata);
    }
    
    
    /**
     * Returns the metadata field definitions
     */
    public List<WGMetaFieldDefinition> getMetadata() {
        return _metadata;
    }

    /**
     * Sets the metadata field definitions
     */
    public void setMetadata(List<WGMetaFieldDefinition> metadata) {
        _metadata = metadata;
    }
    
    /**
     * Returns the name of the document to create from this schema
     */
    public String getName() {
        return _name;
    }
    
    /**
     * Sets the name of the document to create from this schemaSets the name of the schema document
     */    
    public void setName(String name) {
        _name = name;
    }
    
    /**
     * Returns if a schema document is to be automatically created based on this definition
     */
    public boolean isAutoCreate() {
        return _autoCreate;
    }
    
    /**
     * Sets if a schema document is to be automatically created based on this definition
     */
    public void setAutoCreate(boolean autoCreate) {
        _autoCreate = autoCreate;
    }

    public void init() {
        _metadataCache = new HashMap<String, WGMetaFieldDefinition>();
        for (WGMetaFieldDefinition def : _metadata) {
            _metadataCache.put(def.getName(), def);
        }
    }
}
