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
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Blob;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.codec.binary.Hex;
import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocument.SaveAction;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.jdbc.AttachmentFilePart;
import de.innovationgate.webgate.api.jdbc.Content;
import de.innovationgate.webgate.api.jdbc.EntityContainingFiles;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.LogEntry;
import de.innovationgate.webgate.api.jdbc.MainEntity;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS41FileAttachmentHandler implements FileAttachmentHandler {
    
    public static final int ATTACHMENT_FILEPART_SIZE = 1024*64-1;
    
    protected CS41FileHandling _handling;
    protected WGDocumentImpl _doc;
    protected EntityContainingFiles _entity;
    
    /**
     * map of Content- or ContainerFileMeta objects removed on this entity during this session - mapped by fileName
     * for all these metas - file data is deleted via HQL on save()
     */
    protected Map<String,CS41FileAttachmentEntity> _removedFiles = new HashMap<String,CS41FileAttachmentEntity>();
    
    /**
     * map of Content- or ContainerFileMetas attached on this entity - mapped by fileName
     * during this session
     */
    protected Map<String,AttachFileOperation<CS41FileAttachmentEntity>> _attachedFiles = new HashMap<String,AttachFileOperation<CS41FileAttachmentEntity>>();
    
    private Map<String,WGFileMetaData> _modifiedMetaData = new HashMap<String, WGFileMetaData>();

    protected CS41FileEntityDescriptor _entityDescriptor;

    protected CS41FileAttachmentHandler(CS41FileHandling handling, WGDocumentImpl doc, CS41FileEntityDescriptor entityDescriptor) {
        _handling = handling;
        _doc = doc;
        _entity = (EntityContainingFiles) doc.getEntity();
        _entityDescriptor = entityDescriptor;
    }
    
    public AttachFileOperation<CS41FileAttachmentEntity> attachFile(File file, String fileName) throws WGAPIException {
        
        // See if a file of that name is already attached
        if (_entity.getFileEntities().containsKey(fileName) && !_removedFiles.containsKey(fileName)) {
            // B0000471E
            throw new WGIllegalArgumentException(
                    "A file with the same name '"
                            + fileName
                            + "' is already attached on this document, please remove it first.");
        }

        // Create meta entity and attach to parent entity
        Date now = new Date();
        
        CS41FileAttachmentEntity fileMeta = (CS41FileAttachmentEntity) _entity.getFileEntities().get(fileName);
        if (fileMeta == null) {
            fileMeta = _entityDescriptor.createNewFile(fileName, now, _entity);
            _entity.getFileEntities().put(fileName, fileMeta);
        }
        
        fileMeta.setSize(file.length());
        fileMeta.setLastmodified(now);
        fileMeta.setSourceFile(file);               

        AttachFileOperation<CS41FileAttachmentEntity> op = new AttachFileOperation<CS41FileAttachmentEntity>(fileMeta);
        _attachedFiles.put(fileName, op);
        _removedFiles.remove(fileName);
        
        return op;
        
        
    }
    

    public void removeFile(String name) throws WGAPIException {
        
        CS41FileAttachmentEntity fileMeta = (CS41FileAttachmentEntity) _entity.getFileEntities().get(name);
        if (fileMeta != null) {
            _removedFiles.put(name, fileMeta);
            _attachedFiles.remove(name);
            _modifiedMetaData.remove(name);
        }
        
    }
    
    public void beforeSave() throws WGAPIException {
        
        // Remove all data from removed files
        Iterator<CS41FileAttachmentEntity> removedMetas = _removedFiles.values().iterator();
        while (removedMetas.hasNext()) {
            FileAttachmentEntity removedMeta = removedMetas.next();
            processRemovedFile(removedMeta);
        }
        _removedFiles.clear();
        
        // Remove all old data from updated files (attached files whose checksum is already set)
        Iterator<AttachFileOperation<CS41FileAttachmentEntity>> attachedMetas = _attachedFiles.values().iterator();
        while (attachedMetas.hasNext()) {
            CS41FileAttachmentEntity attachedMeta = attachedMetas.next().getEntity();
            if (attachedMeta.getChecksum() != null) {
                deleteFileData(_entity, attachedMeta);
            }
        }
        
    }

    public void processRemovedFile(FileAttachmentEntity removedMeta) throws WGAPIException  {
        deleteFileData(_entity, removedMeta);
        _entity.getFileEntities().remove(((FileAttachmentEntity) removedMeta).getName());
    }
    
    public void afterSave(LogEntry updateLog) throws WGAPIException {

            // Save file data
            Session session = _handling.getParent().getSession();
            Iterator<AttachFileOperation<CS41FileAttachmentEntity>> metas = _attachedFiles.values().iterator();
            while (metas.hasNext()) {
                AttachFileOperation<CS41FileAttachmentEntity> attachOp = metas.next();
                attachOp.setUpdateLog(updateLog);
                saveFileData(attachOp);
            }
            _attachedFiles.clear();
            
            // Save file metadata (must be after file data, bc. some metadata values are influenced by it, like the update revision)
            for (WGFileMetaData meta : _modifiedMetaData.values()) {
                saveFileMetaData(meta, updateLog);
            }
            _modifiedMetaData.clear();

    }
    
    public void beforeRemove() throws WGAPIException {
        
        EntityContainingFiles filesEntity = (EntityContainingFiles) _entity;
        Iterator<String> fileNames = _doc.getFileNames().iterator();
        while (fileNames.hasNext()) {
            String filename = (String) fileNames.next();
            FileAttachmentEntity file = fetchFileAttachmentEntity(filename);
            deleteFileData(filesEntity, file);
        }
        
    }
    
    public void afterRemove() throws WGAPIException {
    }

    public void pushFiles(WGDocumentImpl docCopy) throws WGAPIException {
        
        try {
            Content copy = (Content) docCopy.getEntity();
            copy.setFiles(new HashMap<String,FileAttachmentEntity>());
            
            // detach all files from originalImpl and attach on
            // entityCopy
            Iterator<String> fileNames = _doc.getFileNames().iterator();
            while (fileNames.hasNext()) {
                String orgFileName = fileNames.next();
                pushFile(docCopy, orgFileName);
                
            }
        }
        catch (IOException e) {
            throw new WGBackendException("Exception pushing files to document copy", e);
        }
        
        
    }

    protected AttachFileOperation<?> pushFile(WGDocumentImpl docCopy, String orgFileName) throws IOException, WGAPIException {
        String fileName = WGUtils.strReplace(orgFileName, "/", "§§§", true);
        final TemporaryFile tempFile = new TemporaryFile(fileName, _doc.getFileData(orgFileName), WGFactory.getTempDir());
        if (docCopy.getDocument() != null) {
            docCopy.getDocument().afterSave(new SaveAction() {
                @Override
                public void run(WGDocument doc) throws Exception {
                    tempFile.delete();
                }
            });
        }
        else {
            tempFile.deleteOnEviction(_handling.getParent().getDb().getSessionContext());
        }
        
        File theFile = tempFile.getFile();
        AttachFileOperation<?> op = doPushFile(docCopy, orgFileName, theFile);
        return op;
    }

    protected AttachFileOperation<?> doPushFile(WGDocumentImpl docTarget, String fileName, File theFile) throws WGAPIException {
        AttachFileOperation<?> op = docTarget.getFileHandler().attachFile(theFile, fileName);
        if (op.getEntity() instanceof CS41FileAttachmentEntity) {
            CS41FileAttachmentEntity cs41Entity = (CS41FileAttachmentEntity) op.getEntity();
            cs41Entity.setCreated(getFileMetaData(fileName).getCreated());
            cs41Entity.setLastmodified(getFileMetaData(fileName).getLastmodified());
        }
        return op;
    }
    
    public InputStream getFileData(String name) throws WGAPIException {
        
        if (_removedFiles.containsKey(name)) {
            return null;
        }
        
        CS41FileAttachmentEntity fileEntity = fetchFileAttachmentEntity(name);
        if (fileEntity != null) {
            return getFileAttachmentEntityData(fileEntity);
        }
        else {
            return null;
        }
        
        
    }

    protected InputStream getFileAttachmentEntityData(CS41FileAttachmentEntity fileEntity) throws WGAPIException {
        String hqlQuery = "select cfp from " + _entityDescriptor.getHQLFileDataEntity(fileEntity) + " as cfp where cfp.meta=:metaEntity order by cfp.partnr asc";
        Query query = _handling.getParent().getSession().createQuery(hqlQuery);                                       
        query.setParameter("metaEntity", fileEntity);                 
        return _handling.createOptimizedInputStream(query);
    }
    
    
    public WGFileMetaData getFileMetaData(String name) throws WGAPIException {
        
        if (_removedFiles.containsKey(name)) {
            return null;
        }
        
        WGFileMetaData meta = _modifiedMetaData.get(name);
        if (meta != null) {
            meta.setContext(_doc.getFileMetadataContext());
            return meta;
        }
        
        CS41FileAttachmentEntity metaEntity = retrieveFileMetaEntity(name);
        if (metaEntity != null) {
            meta = readMetaData(metaEntity);
            return meta;                
        } 
        
        else {
            // file not found
            return null;
        }
    }

    protected WGFileMetaData readMetaData(CS41FileAttachmentEntity metaEntity) throws WGAPIException {
        return new WGFileMetaData(_doc.getFileMetadataContext(), metaEntity.getName(), metaEntity.getSize(), metaEntity.getCreated(), metaEntity.getLastmodified(), metaEntity.getChecksum(), null, _handling.loadMdExtensionData(_doc, metaEntity));
    }
    

    @Override
    public FileAttachmentEntity saveFileMetaData(WGFileMetaData md, LogEntry updateLog) throws WGAPIException {

        if (_removedFiles.containsKey(md.getName())) {
            throw new WGIllegalStateException("The file of name " + md.getName() + " was deleted");
        }
        
        CS41FileAttachmentEntity metaEntity = retrieveFileMetaEntity(md.getName());
        if (metaEntity == null) {
            throw new WGIllegalStateException("No file of name " + md.getName() + " on this document");
        }
        _handling.storeMdExtensionData(_doc, md, metaEntity);
        metaEntity.setLastmodified(new Date());
        return metaEntity;
      
    }
    
    public void markMetaDataModified(WGFileMetaData md) {
        _modifiedMetaData.put(md.getName(), md);
    }
    
    protected CS41FileAttachmentEntity retrieveFileMetaEntity(String filename) {
        
        if (_entity instanceof EntityContainingFiles) {
            return (CS41FileAttachmentEntity) ((EntityContainingFiles) _entity).getFileEntities().get(filename);
        }
        else {
            return null;
        }
    }
    
    public int getFileSize(String name) throws WGAPIException {
        
        CS41FileAttachmentEntity meta = (CS41FileAttachmentEntity) _entity.getFileEntities().get(name);
        if (meta != null) {
            return (int) meta.getSize();
        }
        else {
            return -1;
        }
        
    }
    
    
    
    public boolean hasFile(String name) throws WGAPIException {
        
        if (_removedFiles.containsKey(name)) {
           return false;
        }
        
        return _entity.getFileEntities().containsKey(name);
    }
    
    public boolean isFileMetaDataAvailable() throws WGAPIException {
        return true;
    }
    
    public void renameFile(String name, String newName) throws WGAPIException {
        
        if (_entity.getFileEntities().containsKey(newName)) {
            throw new WGIllegalArgumentException("A file with the same name '" + newName + "' is already attached on this document, please remove it first.");
        }
        
        Hibernate.initialize(_entity.getFileEntities());
        CS41FileAttachmentEntity meta = (CS41FileAttachmentEntity) _entity.getFileEntities().remove(name);
        if (meta != null) {
            meta.setName(newName);
            _entity.getFileEntities().put(meta.getName(), meta);
            meta.setLastmodified(new Date());
        }
        
        WGFileMetaData md = _modifiedMetaData.remove(name);
        md = readMetaData(meta);
        _modifiedMetaData.put(newName, md);
        
        
    }
    
    protected void deleteFileData(EntityContainingFiles filesEntity, FileAttachmentEntity meta) throws WGAPIException  {

        if (_handling.getParent().isSaveIsolationActive() == true && (meta == null || meta.getId() == null)) {
            return;            
        }
        
        Query query = _handling.getParent().getSession().createQuery("delete " + _entityDescriptor.getHQLFileDataEntity(meta) + " cfp where cfp.meta = :meta");
        query.setEntity("meta", meta);
        query.executeUpdate();
        
        
    }
    
    protected CS41FileAttachmentEntity fetchFileAttachmentEntity(String strFile) throws WGAPIException, HibernateException {
        
        // We cannot query if the entity is transient
        if (((MainEntity) _entity).getCreated() == null) {
            return null;
        }
        
        String hqlQuery = "select cfm from " + _entityDescriptor.getHQLFileMetaEntity() + " cfm where cfm." + _entityDescriptor.getHQLFileMetaParentProperty() + "=:entity and cfm.name=:name";
        Query query = _handling.getParent().getSession().createQuery(hqlQuery);
        query.setParameter("entity", _entity);
        query.setParameter("name", strFile);
        
        Iterator<?> it = query.iterate();
        if (it.hasNext()) {
            return (CS41FileAttachmentEntity) it.next();
        }
        else {
            return null;
        }
        
    }
    

    @Override
    public List<String> getFileNames() throws WGAPIException {
        List<String> files = new ArrayList<String>(_entity.getFileEntities().keySet());
        files.removeAll(_removedFiles.keySet());
        return files;
    }
    
    
    public void saveFileData(AttachFileOperation<CS41FileAttachmentEntity> op) throws WGAPIException {
        
        try {
            if (!op.isUpdateData()) {
                return;
            }
            
            // create digest for checksum computation
            MessageDigest digest = null;
            try {
                digest = MessageDigest.getInstance("MD5");
            } catch (NoSuchAlgorithmException e) {
                // ignore digest creation
            }
            
            Session session = _handling.getParent().getSession();
            
            CS41FileAttachmentEntity fileMeta = op.getEntity();
            InputStream in = new BufferedInputStream(new FileInputStream(fileMeta.getSourceFile()));
            // store file data
            // split up file in parts with 64K end store each part                          
            int partnr = 0;
            byte[] buffer = new byte[CS41FileAttachmentHandler.ATTACHMENT_FILEPART_SIZE];
            int len = in.read(buffer);
                            
            while (len > 0) {
                // create new file part
                AttachmentFilePart part = _entityDescriptor.createFilePart(fileMeta);
                part.setPartnr(partnr);
                
                Blob data = Hibernate.getLobCreator(session).createBlob(new ByteArrayInputStream(buffer, 0, len), len);
                part.setData(data);
                // store file part
                session.save(part);
                session.flush();
                session.evict(part);
                // update md5 digest
                if (digest != null) {
                    digest.update(buffer, 0, len);
                }
                // read next part from inputstream
                partnr++;
                len = in.read(buffer);
            }
            
            // store md5 sum as meta
            if (digest != null) {
                fileMeta.setChecksum(new String(Hex.encodeHex(digest.digest())));
                if (_handling.getParent().isSaveIsolationActive()) {
                    session.update(fileMeta);
                }
            }
        }
        catch (Exception e) {
            throw new WGBackendException("Exception storing file data", e);
        }
        
        
    }



    @Override
    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String filename) throws WGAPIException {
        return null;
    }
    
    @Override
    public void addBinaryExtensionData(ExtensionData extData, Object value) throws WGAPIException {
        throw new WGIllegalStateException("Binary extension data is not supported on this content store version");
    }
    
    @Override
    public void saveAdditionalObject(Object obj, LogEntry logEntry) throws WGAPIException {
        _handling.getParent().getSession().saveOrUpdate(obj);        
    }

}
