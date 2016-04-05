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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.hibernate.HibernateException;
import org.hibernate.LockMode;
import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.jdbc.ContentFileContent;
import de.innovationgate.webgate.api.jdbc.ContentFileDerivate;
import de.innovationgate.webgate.api.jdbc.EntityContainingFiles;
import de.innovationgate.webgate.api.jdbc.MainEntity;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS5P4FileAttachmentHandler extends CS5FileAttachmentHandler {



    protected CS5P4FileAttachmentHandler(CS5P4FileHandling handling, WGDocumentImpl doc, CS41FileEntityDescriptor entityDescriptor) {
        super(handling, doc, entityDescriptor);
    }
    
    protected AttachFileOperation<?> pushFile(WGDocumentImpl docCopy, String fileName) throws IOException, WGAPIException {
        
        AttachFileOperation<?> op;
        
        CS5P4FileAttachmentEntity sourceFileEntity = fetchFileAttachmentEntity(fileName);
        
        // If both docs from same database: Use a dummy file (the temp directory, it at least exists), mark the operation to not update the file data as it is already contained in the file contents of this database, transfer file metadata manually
        if (sourceFileEntity.getChecksumSha512() != null && _doc.getParent() == docCopy.getParent()) {
            op = doPushFile(docCopy, fileName, WGFactory.getTempDir());
            doPushAnnotations(docCopy, fileName);
           
            
            CS5P4FileAttachmentEntity targetFileEntity = (CS5P4FileAttachmentEntity) op.getEntity();
            targetFileEntity.setSize(sourceFileEntity.getSize());
            targetFileEntity.setChecksum(sourceFileEntity.getChecksum());
            targetFileEntity.setChecksumSha512(sourceFileEntity.getChecksumSha512());
            op.setUpdateData(false);
        }
        
        // Otherwise do a regular copying file push
        else {
            op = super.pushFile(docCopy, fileName);
        }
        
        return op;
    }
    


    @Override
    protected InputStream getFileAttachmentEntityData(CS41FileAttachmentEntity fileEntity) throws WGAPIException {

        Session session = _handling.getParent().getSession();
        CS5P4FileAttachmentEntity cs5p4FileAttachmentEntity = (CS5P4FileAttachmentEntity) fileEntity;
        if (cs5p4FileAttachmentEntity.getChecksumSha512() != null) {
            ContentFileContent fileContents = ((CS5P4FileHandling) _handling).fetchFileContents(cs5p4FileAttachmentEntity.getChecksumSha512());
            if (fileContents != null) {
                String hqlQuery = "select cfp from ContentFileContentPart as cfp where cfp.fileContents=:contentsEntity order by cfp.partnr asc";
                Query query = session.createQuery(hqlQuery);                                       
                query.setParameter("contentsEntity", fileContents);
                return _handling.createOptimizedInputStream(query);
            }
            else {
                return null;
            }
        }
        else {
            return super.getFileAttachmentEntityData(fileEntity);
        }
        
    }

    
    @Override
    protected void deleteFileData(EntityContainingFiles filesEntity, FileAttachmentEntity removedMeta) throws WGAPIException {
        if (((CS5P4FileAttachmentEntity) removedMeta).getChecksumSha512() == null) { // Delete legacy data only. Distinct file data is deleted by the daily maintenance when no longer referenced anywhere.
            super.deleteFileData(filesEntity, removedMeta);
        }
    }
    
    @Override
    public void saveFileData(AttachFileOperation<CS41FileAttachmentEntity> op) throws WGAPIException {
        
        try {
            CS5P4FileAttachmentEntity fileMeta = (CS5P4FileAttachmentEntity) op.getEntity();
            
            // Do not update in internal situations where it is clear that the file contents is already present in database (draft copy).
            if (!op.isUpdateData()) {
                return;
            }
            
            // Update file contents and ext data coming with the attach operation
            Session session = _handling.getParent().getSession();
            InputStream in = new BufferedInputStream(new FileInputStream(fileMeta.getSourceFile()));
            try {
                ((CS5P4FileHandling) _handling).storeFileContents(fileMeta, _entityDescriptor, in);
            }
            finally {
                in.close();
            }
        }
        catch (Exception e) {
            throw new WGBackendException("Exception saving file data", e);
        }
        
    }

    @Override
    protected CS5P4FileAttachmentEntity fetchFileAttachmentEntity(String strFile) throws WGAPIException, HibernateException {

        
        // We cannot query if the entity is transient
        if (((MainEntity) _entity).getCreated() == null) {
            return null;
        }
        
        String hqlQuery = "select cfm from " + _entityDescriptor.getHQLFileMetaEntity() + " cfm where cfm." + _entityDescriptor.getHQLFileMetaParentProperty() + "=:entity and cfm.name=:name";
        Query query = _handling.getParent().getSession().createQuery(hqlQuery);
        query.setLockMode("cfm", LockMode.PESSIMISTIC_READ);
        query.setParameter("entity", _entity);
        query.setParameter("name", strFile);
        
        Iterator<?> it = query.iterate();
        if (it.hasNext()) {
            return (CS5P4FileAttachmentEntity) it.next();
        }
        else {
            return null;
        }
        
    }
    
    public List<WGFileDerivateMetaData> getFileDerivates(String fileName) throws WGAPIException {

        CS5P4FileAttachmentEntity entity = fetchFileAttachmentEntity(fileName);
        if (entity == null || entity.getChecksumSha512() == null) {
            return null;
        }
        
        List<WGFileDerivateMetaData> metaData = new ArrayList<WGFileDerivateMetaData>();
        
        String hql = "select cfd from ContentFileDerivate as cfd where cfd.parentSha512 = :checksum";
        Query q = _handling.getParent().getSession().createQuery(hql);
        q.setParameter("checksum", entity.getChecksumSha512());
        Iterator<?> derivates = q.iterate();
        while (derivates.hasNext()) {
            ContentFileDerivate cfd = (ContentFileDerivate) derivates.next();
            WGDocumentImpl docImpl = _handling.getParent().createDocumentImpl(entity.getParentEntity());
            WGFileDerivateMetaData md = new WGFileDerivateMetaData(_doc.getParent().getDb(), WGDocument.buildDocumentKey(docImpl, _doc.getParent().getDb()), cfd.getId(), cfd.getCreator(), cfd.getName(), cfd.getCreated(), cfd.getLastmodified(), cfd.getSize(), cfd.getDerivateSha512(), cfd.getParentSha512());
            ((CS5P4FileHandling) _handling).loadMdExtensionData(md, cfd);
            metaData.add(md);
        }
        
        return metaData;
        
    }
    
    @Override
    protected WGFileMetaData readMetaData(CS41FileAttachmentEntity metaEntity) throws WGAPIException {
        CS5P4FileAttachmentEntity cs5entity = (CS5P4FileAttachmentEntity) metaEntity; 
        return new WGFileMetaData(_doc.getFileMetadataContext(), metaEntity.getName(), metaEntity.getSize(), metaEntity.getCreated(), metaEntity.getLastmodified(), metaEntity.getChecksum(), cs5entity.getChecksumSha512(), _handling.loadMdExtensionData(_doc, metaEntity));
    }
    
    
}
