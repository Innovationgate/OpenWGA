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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Blob;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.omg.CORBA._PolicyStub;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.Content;
import de.innovationgate.webgate.api.jdbc.ContentFile;
import de.innovationgate.webgate.api.jdbc.EntityContainingFiles;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.LogEntry;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS3FileAttachmentHandler implements FileAttachmentHandler {
    
    protected WGDocumentImpl _doc;
    protected CS3FileHandling _handling;
    protected EntityContainingFiles _entity;
    protected CS3FileEntityDescriptor _entityDescriptor;

    protected CS3FileAttachmentHandler(CS3FileHandling handling, WGDocumentImpl doc, CS3FileEntityDescriptor entityDescriptor) {
        _handling = handling;
        _doc = doc;
        _entity = (EntityContainingFiles) doc.getEntity();
        _entityDescriptor = entityDescriptor;
    }

    @Override
    public AttachFileOperation<?> attachFile(File file, String fileName) throws WGAPIException {

        try {
            // See if a file of that name is already attached
            if (_entity.getFileEntities().containsKey(fileName)) {
                // B0000471E
                throw new WGIllegalArgumentException("A file with the same name '" + fileName + "' is already attached on this document, please remove it first.");
            }
            
            // Load the data
            InputStream in = new BufferedInputStream(new FileInputStream(file));
            Blob blob = Hibernate.getLobCreator(_handling.getParent().getSession()).createBlob(in, file.length());

            // Create entity and attach to parent entity
            CS3FileAttachmentEntity att = _entityDescriptor.createNewFile(fileName, _entity);
            att.setData(blob);
            
            FileAttachmentEntity oldAtt = _entity.getFileEntities().put(fileName, att);
            if (oldAtt != null && oldAtt != att) {
                _handling.getParent().getSession().evict(oldAtt);
            }
            
            return new AttachFileOperation<CS3FileAttachmentEntity>(att); // We don't store/use those ops internally, so we just create the op on return 
        }
        catch (FileNotFoundException e) {
            throw new WGIllegalArgumentException("Error attaching file - not found.", e);
        }

    }

    @Override
    public void removeFile(String name) throws WGAPIException {

        CS3FileAttachmentEntity fileAtt = (CS3FileAttachmentEntity) _entity.getFileEntities().get(name);
        if (fileAtt != null) {
            Hibernate.initialize(_entity.getFileEntities());
            _entity.getFileEntities().remove(name);
        }

    }

    @Override
    public void beforeSave() throws WGAPIException {
    }

    @Override
    public void afterSave(LogEntry updateLog) throws WGAPIException {
    }

    @Override
    public void beforeRemove() throws WGAPIException {
    }

    @Override
    public void afterRemove() throws WGAPIException {
    }

    @Override
    public void pushFiles(WGDocumentImpl docCopy) throws WGAPIException {
        Content copy = (Content) docCopy.getEntity();
        copy.setFiles(cloneContentFiles(_entity.getFileEntities(), copy));
    }

    @Override
    public InputStream getFileData(String name) throws WGAPIException {

        try {
            
            Iterator it = fetchFileEntity(name);
            if (it.hasNext()) {
                CS3FileAttachmentEntity fileEntity = (CS3FileAttachmentEntity) it.next();
                _handling.getParent().getSession().refresh(fileEntity);
                return fileEntity.getData().getBinaryStream();
            }
            else {
                return null;
            }
        }
        catch (SQLException e) {
            throw new WGBackendException("Error retrieving file data", e);
        }
        
    }

    @Override
    public WGFileMetaData getFileMetaData(String name) throws WGAPIException {
        throw new WGIllegalStateException("Method getFileMetaData() is not supported on content stores of version 3");
    }

    @Override
    public int getFileSize(String name) throws WGAPIException {

        try {
            
            Iterator it = fetchFileEntity(name);
            if (it.hasNext()) {
                CS3FileAttachmentEntity fileEntity = (CS3FileAttachmentEntity) it.next();
                return (int) fileEntity.getData().length();
            }
            else {
                return -1;
            }
          
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error retrieving blob size", e);
        }
        catch (SQLException e) {
            throw new WGBackendException("Error retrieving blob size", e);
        }
        
    }

    @Override
    public boolean hasFile(String name) throws WGAPIException {
        return fetchFileEntity(name).hasNext();
    }
    
    private Iterator<?> fetchFileEntity(String strFile) throws WGAPIException, HibernateException {
        
        // We cannot query if the entity is transient
        if (_doc.getCreated() == null) {
            return Collections.emptyList().iterator();
        }
        
        String hqlQuery = "select cf from " + _entityDescriptor.getHQLFileEntity() + " cf where cf." + _entityDescriptor.getHQLParentProperty() + "=:entity and cf.name=:name";
        
        Query query = _handling.getParent().getSession().createQuery(hqlQuery);
        query.setParameter("entity", _entity);
        query.setParameter("name", strFile);
        Iterator<?> it = query.iterate();
        return it;
    }

    @Override
    public boolean isFileMetaDataAvailable() throws WGAPIException {
        return false;
    }

    @Override
    public void renameFile(String name, String newName) throws WGAPIException {
        throw new WGNotSupportedException("renameFile() is not supported on content stores of version 3");
    }
    
    /**
     * @param map
     * @return
     * @throws WGBackendException 
     */
    private Map cloneContentFiles(Map map, Content target) throws WGAPIException {

        try {
            Map fileCopies = new HashMap();
            Iterator fileKeys = map.keySet().iterator();
            Object fileKey;
            ContentFile contentFile;
            ContentFile contentFileCopy;
            while (fileKeys.hasNext()) {
                fileKey = fileKeys.next();
                contentFile = (ContentFile) map.get(fileKey);
                contentFileCopy = new ContentFile();
                contentFileCopy.setParentcontent(target);
                contentFileCopy.setName(contentFile.getName());
                Blob data = contentFile.getData();
                contentFileCopy.setData(Hibernate.getLobCreator(_handling.getParent().getSession()).createBlob(data.getBinaryStream(), data.length()));
                contentFileCopy.setExtensionData(WGDatabaseImpl.cloneExtensionData(contentFile.getExtensionData(), contentFileCopy));
                fileCopies.put(fileKey, contentFileCopy);
            }

            return fileCopies;
        }
        catch (SQLException e) {
            throw new WGBackendException("Error cloning content files.", e);
        }

    }
    
    @Override
    public List<String> getFileNames() throws WGAPIException {

     // We cannot query if the entity is transient
        if (_doc.getCreated() == null) {
            return new ArrayList();
        }
        
        String hqlQuery = "select cf.name from " + _entityDescriptor.getHQLFileEntity() + " cf where cf." + _entityDescriptor.getHQLParentProperty() + "=:entity";
        Query query = _handling.getParent().getSession().createQuery(hqlQuery);
        query.setParameter("entity", _entity);
        return query.list();
        
        
    }
    
    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String filename) throws WGAPIException {
        return null;
    }

    @Override
    public FileAttachmentEntity saveFileMetaData(WGFileMetaData md, LogEntry updateLog) throws WGAPIException {
        throw new WGIllegalStateException("Extension data is not supported on content stores of version 3");
    }
    
    @Override
    public void addBinaryExtensionData(ExtensionData extData, Object value) throws WGIllegalStateException {
        throw new WGIllegalStateException("Binary extension data is not supported on content stores of version 3");
    }

    @Override
    public void saveAdditionalObject(Object obj, LogEntry logEntry)throws WGAPIException  {
        _handling.getParent().getSession().saveOrUpdate(obj);        
    }
    

}
