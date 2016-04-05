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

package de.innovationgate.webgate.api.jdbc.filehandling;

import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.hibernate.Query;

import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.HibernateQueryInputStream;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS41FileHandling implements FileHandling {

    private WGDatabaseImpl _parent;

    @Override
    public void init(WGDatabaseImpl db) {
        _parent = db;
    }

    @Override
    public FileAttachmentHandler createDocumentHandler(WGDocumentImpl doc) {

        if (doc.getType() == WGDocument.TYPE_CONTENT) {
            return new CS41FileAttachmentHandler(this, doc, new CS41ContentFileDescriptor());
        }
        else if (doc.getType() == WGDocument.TYPE_FILECONTAINER) {
            return new CS41FileAttachmentHandler(this, doc, new CS41ContainerFileDescriptor());
        }
        
        return null;
        
    }

    @Override
    public void destroy() {
    }

    public WGDatabaseImpl getParent() {
        return _parent;
    }
    
    protected InputStream createOptimizedInputStream(Query query)throws WGAPIException  {
        return new HibernateQueryInputStream(getParent().getSession(), query, 0, getParent().isOptimizedFileHandlingDisableQueryPaging());
    }
    
    public Map<String,Object> loadMdExtensionData(WGDocumentImpl doc, CS41FileAttachmentEntity metaEntity) throws WGAPIException {
        Map<String,Object> extData = new HashMap<String, Object>();
        for (String extDataName : metaEntity.getExtensionData().keySet()) {
            try {
                extData.put(extDataName, metaEntity.readExtensionData(getParent(), doc, extDataName));
            }
            catch (WGSystemException e) {
                // Ignore exception because of unsupported item type (#00004197)
            }
            catch (WGIllegalDataException e) {
                // Ignore exception because of undeserializable xstream (#00004197)
            }
        }
        return extData;
    }
    
    public void storeMdExtensionData(WGDocumentImpl doc, WGFileMetaData meta, CS41FileAttachmentEntity metaEntity) throws WGAPIException {
        
        // Get names of fields already stored
        Set<String> existingFields = new HashSet<String>();
        for (String name : metaEntity.getExtensionData().keySet()) {
            existingFields.add(name);
        }
        
        // Store fields
        for (String extDataName : meta.getExtensionDataNames()) {
            metaEntity.writeExtensionData(getParent(), doc, extDataName, meta.getExtensionData(extDataName));
            existingFields.remove(extDataName);
        }
        
        // Process removed fields
        for (String fieldName : existingFields) {
            metaEntity.getExtensionData().remove(fieldName);
        }
        
    }
    
    @Override
    public long dailyFileMaintenance(Logger log) throws WGAPIException {
        return 0;
    }
    
    @Override
    public void writeBinaryExtensionData(WGDocumentImpl doc, ExtensionData extData, Object value) throws WGAPIException {
        throw new WGNotSupportedException("Binary extension data is not supported on this content store version/patch level");
    }
    
    @Override
    public BinaryFieldData readBinaryExtensionData(WGDocumentImpl doc, ExtensionData extData) throws WGAPIException {
        throw new WGNotSupportedException("Binary extension data is not supported on this content store version/patch level");
    }


}
