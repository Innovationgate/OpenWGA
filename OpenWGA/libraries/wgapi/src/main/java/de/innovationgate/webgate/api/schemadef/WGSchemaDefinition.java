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

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;
import org.simpleframework.xml.Transient;
import org.simpleframework.xml.core.Persister;

import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.MetaInfo;
import de.innovationgate.webgate.api.MetaInfoProvider;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFactory;

/**
 * The definition of a content schema for an OpenWGA content store, to be automatically enforced
 */
@Root(strict=false)
public class WGSchemaDefinition {
    
    private static Persister _serializer = new Persister();
    
    /**
     * Read a serialized schema definition from an input stream
     * @param in The data stream
     * @return Deserialized schema definition
     * @throws Exception
     */
    public static WGSchemaDefinition read(InputStream in) throws Exception {
        WGSchemaDefinition schema = _serializer.read(WGSchemaDefinition.class, in);
        schema.init();
        return schema;
    }
    
    protected void init() {
        _documentDefinitionsCache = new HashMap<String, WGSchemaDocumentDefinition>();
        for (WGSchemaDocumentDefinition def : _documentDefinitions) {
            def.init();
            _documentDefinitionsCache.put(def.getDocumentKey().toString(), def);
        }
    }
    
    /**
     * Serializes the schema definition to an output stream. The written data can again be deserialized using {@link #read(InputStream)}.
     * @param out The stream to write the schema to.
     * @throws Exception
     */
    public void write(OutputStream out) throws Exception {
        validate();
        _serializer.write(this, out);
    }
    
    /**
     * Validates the schema for logical errors
     * @throws WGSchemaValidationException If an error is found
     * @throws WGAPIException
     */
    public void validate() throws WGSchemaValidationException, WGAPIException {
        
        Set<String> docDefKeys = new HashSet<String>();
        
        for (WGSchemaDocumentDefinition docDef : _documentDefinitions) {
                                    
            if (docDef.getName() == null) {
                throw new WGSchemaValidationException("Schema definition '" + docDef.getDocumentKey() + "' contains no name.");
            }
            
            if (docDefKeys.contains(docDef.getDocumentKey().toString())) {
                throw new WGSchemaValidationException("Duplicate schema definition '" + docDef.getDocumentKey() + "'.");
            }            
            docDefKeys.add(docDef.getDocumentKey().toString());

            
            Set<String> metaNames = new HashSet<String>();
            for (WGMetaFieldDefinition metaEntry : docDef.getMetadata()) {
                
                if (metaNames.contains(metaEntry.getName())) {
                    throw new WGSchemaValidationException("Duplicate meta definition '" + metaEntry.getName() + "' on '" + docDef.getDocumentKey() + "'.");
                }            
                metaNames.add(metaEntry.getName());
                
                @SuppressWarnings("unchecked")
                MetaInfo metaInfo = WGFactory.getInstance().getMetaInfo(metaEntry.getName(), (Class<MetaInfoProvider>) WGDocument.doctypeNumberToClass(docDef.getDocumentType()));
                if (metaInfo == null) {
                    throw new WGSchemaValidationException("Unknown metadata field '" + metaEntry.getName() + "' predefined for document " + docDef.getDocumentKey());
                }
                
                if (metaEntry.getValues().size() > 0) {
                    Object singleValue = metaEntry.getValues().get(0);
                    if (singleValue != null && !metaInfo.getDataType().isAssignableFrom(singleValue.getClass())) {
                        throw new WGSchemaValidationException("Metadata field '" + metaEntry.getName() + "' predefined with value of wrong type " + singleValue.getClass().getName() + " (Should be " + metaInfo.getDataType() +  ") for document " + docDef.getDocumentKey());
                    }
                }
                
            }
            
        }
        
    }

    @Attribute(name="createDefaultLanguage")
    private boolean _createDefaultLanguage;
    
    @Transient
    private Map<String,WGSchemaDocumentDefinition> _documentDefinitionsCache; 
    
    @ElementList(name="documentDefinitions")
    private List<WGSchemaDocumentDefinition> _documentDefinitions = new ArrayList<WGSchemaDocumentDefinition>();
    
    @Attribute(name="version")
    private double _version = 5.2;

    /**
     * Determines if a language document for the platform default language should be automatically created
     */
    public boolean isCreateDefaultLanguage() {
        return _createDefaultLanguage;
    }

    /**
     * Sets if a language document for the platform default language should be automatically created
     */
    public void setCreateDefaultLanguage(boolean createDefaultLanguage) {
        _createDefaultLanguage = createDefaultLanguage;
    }

    /**
     * Returns a map of document definitions, mapped by their document keys
     */
    public Map<String, WGSchemaDocumentDefinition> getDocumentDefinitionsCache() {
        return _documentDefinitionsCache;
    }
    
    /**
     * Adds a document definition to the schema
     */
    public void addDocumentDefinition(WGSchemaDocumentDefinition docDef) {
        _documentDefinitions.add(docDef);
    }

    /**
     * Returns the document definitions of the schema
     */
    public List<WGSchemaDocumentDefinition> getDocumentDefinitions() {
        return _documentDefinitions;
    }

    /**
     * Sets the document definitions of this schema
     */
    public void setDocumentDefinitions(List<WGSchemaDocumentDefinition> documentDefinitions) {
        _documentDefinitions = documentDefinitions;
    }

    /**
     * Returns the version of the schema definition format, matching the OpenWGA version that introduced it 
     */
    public double getVersion() {
        return _version;
    }

    /**
     * Sets the version of the schema format
     */
    public void setVersion(double version) {
        _version = version;
    }
    
    public void importSchema(WGSchemaDefinition schemaDef) {
        
        if (schemaDef.isCreateDefaultLanguage() && _createDefaultLanguage == false) {
            _createDefaultLanguage = true;
        }
        
        for (WGSchemaDocumentDefinition def : schemaDef.getDocumentDefinitions()) {
            WGSchemaDocumentDefinition existingDef = _documentDefinitionsCache.get(def.getDocumentKey().toString());
            if (existingDef != null) {
                _documentDefinitions.remove(existingDef);
            }
            addDocumentDefinition((WGSchemaDocumentDefinition) XStreamUtils.clone(def));
        }
        
        init();
        
    }

}
