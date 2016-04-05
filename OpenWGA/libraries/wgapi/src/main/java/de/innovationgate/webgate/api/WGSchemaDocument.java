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

package de.innovationgate.webgate.api;

import java.util.List;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.webgate.api.schemadef.WGAreaDefinition;
import de.innovationgate.webgate.api.schemadef.WGContentTypeDefinition;
import de.innovationgate.webgate.api.schemadef.WGLanguageDefinition;
import de.innovationgate.webgate.api.schemadef.WGMetaFieldDefinition;
import de.innovationgate.webgate.api.schemadef.WGSchemaDefinition;
import de.innovationgate.webgate.api.schemadef.WGSchemaDocumentDefinition;



/**
 * Denotes documents defining the content schema of the content store and their shared functionality, explicitly their data redirection to schema definitions
 */
public abstract class WGSchemaDocument extends WGDesignDocument {
    
    public static final String EXTDATA_SCHEMADEFINITION = "SCHEMADEFINITION";

    public WGSchemaDocument(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
        super(db, doc, flags);
    }
    
    @Override
    protected Object retrieveMetaData(WGDocumentCore core, MetaInfo metaInfo, double csVersion) throws WGAPIException {

        String ucName = metaInfo.getName().toUpperCase();
        WGSchemaDocumentDefinition docDef = getSchemaDefinition();
        if (docDef != null && docDef.getMetadataCache().containsKey(ucName)) {
            WGMetaFieldDefinition metaDef = docDef.getMetadataCache().get(ucName);
            if (metaInfo != null && metaInfo.isMultiple()) {
                return metaDef.getValues();
            }
            else {
                return metaDef.getSingleValue();
            }
           
        }
        else {
            return super.retrieveMetaData(core, metaInfo, csVersion);
        }
        
    }
    
    /**
     * Returns the schema definition for this document, if available. Otherwise returns null. 
     * @throws WGAPIException
     */
    public WGSchemaDocumentDefinition getSchemaDefinition() throws WGAPIException {
        WGSchemaDefinition schemaDef = getDatabase().getSchemaDefinition();
        if (schemaDef == null) {
            return null;
        }
        String schemaKey = (String) getExtensionData(EXTDATA_SCHEMADEFINITION);
        if (schemaKey == null) {
            schemaKey = getDocumentKey();
        }
        
        
        return schemaDef.getDocumentDefinitionsCache().get(schemaKey);
        
    }
    
    /**
     * Returns if the data of the given metadata field is served from schema definition rather than from the database document
     * @param name Name of the metadata field to test
     * @return true if the data served is from the schema definition, false if it originates from the document in the OpenWGA content store
     * @throws WGAPIException
     */
    public boolean isMetaDeterminedInSchema(String name) throws WGAPIException {
        
        name = name.toUpperCase();
        WGSchemaDocumentDefinition docDef = getSchemaDefinition();
        if (docDef != null) {
            return docDef.getMetadataCache().containsKey(name);
        }
        else {
            return false;
        }
        
    }
    
    @Override
    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException {
        super.performSaveCheck();
        if (db.getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
            throw new WGAuthorisationException("You are not allowed to edit this schema document", WGAuthorisationException.ERRORCODE_OP_NEEDS_CHIEF_EDITOR_RIGHTS);
        }
    }
    
    @Override
    protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAuthorisationException, WGAPIException {
        super.performRemoveCheck(deepCheck, deletionRoot);
        if (db.getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
            throw new WGAuthorisationException("You are not allowed to remove this schema document", WGAuthorisationException.ERRORCODE_OP_NEEDS_CHIEF_EDITOR_RIGHTS);
        }
    }
    
    /**
     * Creates a schema definition predefining all the metadata currently on this document
     */
    public WGSchemaDocumentDefinition createSchemaDefinition() throws WGAPIException {
        
        WGSchemaDocumentDefinition docDef;
        if (this instanceof WGContentType) {
            docDef = new WGContentTypeDefinition();
        }
        else if (this instanceof WGArea) {
            docDef = new WGAreaDefinition();
        }
        else if (this instanceof WGLanguage) {
            docDef = new WGLanguageDefinition();   
        }
        else {
            throw new WGNotSupportedException("Schema definitions for document of type " + getClass().getName() + " are not supported");
        }
        
        docDef.setName(getName());
        
        for (String metaName : getMetaNames()) {
            MetaInfo metaInfo = getMetaInfo(metaName);
            if (!getClass().equals(metaInfo.getDefiningClass())) {
                continue;
            }
            
            Object value = getMetaData(metaName);
           
            if (!WGUtils.isEmpty(value)) {
                WGMetaFieldDefinition metaDef = new WGMetaFieldDefinition(metaName, value);
                docDef.addMetadata(metaDef);
            }
        }
        
        return docDef;
        
    }

    @Override
    public boolean setMetaData(String strName, Object value) throws WGAPIException {

        if (isMetaDeterminedInSchema(strName)) {

            if (getDatabase().getSessionContext().isMasterSession()) {
                // In case of master user we fail silently. These may be WGAPI internal initialisations.
                return false;
            }
            else {
                throw new WGIllegalStateException("Metadata field '" + strName + "' is predefined in schema definition and cannot be overwritten");
            }
        }
        
        return super.setMetaData(strName, value);
    }

}
