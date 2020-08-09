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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.sql.Blob;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.commons.codec.binary.Hex;
import org.apache.log4j.Logger;
import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDuplicateKeyException;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.jdbc.AttachmentFilePart;
import de.innovationgate.webgate.api.jdbc.ContentFileContent;
import de.innovationgate.webgate.api.jdbc.ContentFileDerivate;
import de.innovationgate.webgate.api.jdbc.ContentFileMeta;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS5P4FileHandling extends CS5FileHandling {

    private static final String ORDINAL_NUMBER_SEQUENCE_NAME = "content_filecontents_ordinalnr";

    @Override
    public FileAttachmentHandler createDocumentHandler(WGDocumentImpl doc) {

        if (doc.getType() == WGDocument.TYPE_CONTENT) {
            return new CS5P4FileAttachmentHandler(this, doc, new CS5P4ContentFileDescriptor());
        }
        else if (doc.getType() == WGDocument.TYPE_FILECONTAINER) {
            return new CS5FileAttachmentHandler(this, doc, new CS41ContainerFileDescriptor());
        }

        return null;

    }

    public void storeFileContents(CS5P4FileAttachmentEntity fileMeta, CS41FileEntityDescriptor descriptor, InputStream in) throws IOException, NoSuchAlgorithmException,
            InstantiationException, IllegalAccessException, WGAPIException {
        
        Session session = getParent().getSession();

        // create digest for checksum computation
        MessageDigest digestMd5 = MessageDigest.getInstance("MD5");
        MessageDigest digestSha512 = MessageDigest.getInstance("SHA-512");

        ContentFileContent fileContent = storeFileContents(descriptor, in, session, digestMd5, digestSha512);

        // store digests on meta
        fileMeta.setChecksum(new String(Hex.encodeHex(digestMd5.digest())));
        fileMeta.setChecksumSha512(fileContent.getChecksumSha512());
        if (getParent().isSaveIsolationActive()) {
            session.update(fileMeta);
        }
    }
    
    public void storeFileContents(ExtensionData extData, CS41FileEntityDescriptor descriptor, InputStream in) throws IOException, NoSuchAlgorithmException,
    InstantiationException, IllegalAccessException, WGAPIException {
        
        Session session = getParent().getSession();
        
        // create digest for checksum computation
        MessageDigest digestMd5 = MessageDigest.getInstance("MD5");
        MessageDigest digestSha512 = MessageDigest.getInstance("SHA-512");
        
        ContentFileContent fileContent = storeFileContents(descriptor, in, session, digestMd5, digestSha512);
        
        // store digests on e
        extData.setBinarySha512(fileContent.getChecksumSha512());
        if (getParent().isSaveIsolationActive()) {
            session.saveOrUpdate(extData);
        }
    }

    public ContentFileContent storeFileContents(CS41FileEntityDescriptor descriptor, InputStream in, Session session, MessageDigest digestMd5, MessageDigest digestSha512) throws IOException,
            InstantiationException, IllegalAccessException, WGAPIException {
        int partnr = 0;
        byte[] buffer = new byte[CS41FileAttachmentHandler.ATTACHMENT_FILEPART_SIZE];
        int len = in.read(buffer);
        int totalLen = 0;
        
        // Create file contents entity
        ContentFileContent fileContents = new ContentFileContent();
        fileContents.setOrdinalnr(getParent().incrementSystemSequence(ORDINAL_NUMBER_SEQUENCE_NAME));
        session.save(fileContents);

        while (len > 0) {
            totalLen += len;

            // create new file part
            AttachmentFilePart part = descriptor.createFilePart(fileContents);
            part.setPartnr(partnr);

            Blob data = Hibernate.getLobCreator(session).createBlob(new ByteArrayInputStream(buffer, 0, len), len);
            part.setData(data);
            
            // store file part
            session.save(part);
            session.flush();
            session.evict(part);

            // update digests
            digestMd5.update(buffer, 0, len);
            digestSha512.update(buffer, 0, len);

            // read next part from inputstream
            partnr++;
            len = in.read(buffer);
            
        }

        String checksumSha512 = new String(Hex.encodeHex(digestSha512.digest()));

        // Store file data on contents entity
        fileContents.setChecksumSha512(checksumSha512);
        fileContents.setSize(totalLen);
        if (getParent().isSaveIsolationActive()) {
            session.update(fileContents);
        }
        return fileContents;
    }
    
    public String storeFileDerivate(WGFileMetaData originalFileMd, String creator, String derivateName, InputStream in, Map<String, Object> customMdFields) throws WGAPIException {

        if (originalFileMd.getSha512Checksum() == null) {
            throw new WGIllegalStateException("Cannot create derivate of file '" + originalFileMd.getName() + "' because it is not yet stored as distinct file content");
        }
        
        // Lookup if this derivate of the sha512/creator/name combination already exists
        String hql = "from ContentFileDerivate as cfd where cfd.parentSha512=:checksum and cfd.creator=:creator and cfd.name=:name";
        Query q = getParent().getSession().createQuery(hql);
        q.setParameter("checksum", originalFileMd.getSha512Checksum());
        q.setParameter("creator", creator);
        q.setParameter("name", derivateName);
        if (q.iterate().hasNext()) {
            throw new WGDuplicateKeyException("A file derivate '" + creator + "/" + derivateName + "' for that file content does already exist");
        }
        
        try {
            ContentFileDerivate cfd = new ContentFileDerivate();
            cfd.setParentSha512(originalFileMd.getSha512Checksum());
            cfd.setCreator(creator);
            cfd.setName(derivateName);
            cfd.setCreated(new Date());
            cfd.setLastmodified(new Date());
            
            if (customMdFields != null) {
                for (Map.Entry<String,Object> entry : customMdFields.entrySet()) {
                    cfd.writeExtensionData(getParent(), null, entry.getKey(), entry.getValue());
                }
            }
            
            // create digest for checksum computation
            MessageDigest digestMd5 = MessageDigest.getInstance("MD5");
            MessageDigest digestSha512 = MessageDigest.getInstance("SHA-512");
            
            ContentFileContent fileContent = storeFileContents(new CS5P4ContentFileDescriptor(), in, getParent().getSession(), digestMd5, digestSha512);
            cfd.setDerivateSha512(fileContent.getChecksumSha512());
            cfd.setSize(fileContent.getSize());
            
            
            if (getParent().isSaveIsolationActive()) {
                getParent().getSession().save(cfd);
            }

            /*
             * #00005586: Das müllt die historylog Tabelle voll.
             * Ist das wirklich irgendwo für notwendig ???

            getParent().createLogEntry(getParent().getSession(), WGUpdateLog.TYPE_UPDATE, WGDocument.TYPENAME_FILEDERIVATE + "/" + cfd.getId(), cfd.getId());
            */
            getParent().commitHibernateTransaction();

            return cfd.getId();
        }
        catch (NoSuchAlgorithmException e) {
            throw new WGBackendException("Exception storing file derivate", e);
        }
        catch (IllegalAccessException e) {
            throw new WGBackendException("Exception storing file derivate", e);
        }
        catch (InstantiationException e) {
            throw new WGBackendException("Exception storing file derivate", e);
        }
        catch (IOException e) {
            throw new WGBackendException("Exception storing file derivate", e);
        }
        
    }
    
    public void removeFileDerivate(String derivateId) throws WGAPIException {
        ContentFileDerivate cfd = (ContentFileDerivate) getParent().getSession().get(ContentFileDerivate.class, derivateId);
        if (cfd == null) {
            throw new WGIllegalArgumentException("No file derivate of id '" + derivateId + "' exists");
        }
        
        getParent().getSession().delete(cfd);
        /*
         * #00005586: Das müllt die historylog Tabelle voll.
         * Ist das wirklich irgendwo für notwendig ???

        getParent().createLogEntry(getParent().getSession(), WGUpdateLog.TYPE_DELETE, WGDocument.TYPENAME_FILEDERIVATE + "/" + cfd.getId(), cfd.getId());
        */
        getParent().commitHibernateTransaction();

    }

    
    public WGFileDerivateMetaData getFileDerivateMetadata(String id) throws WGAPIException {
        
        ContentFileDerivate cfd = (ContentFileDerivate) getParent().getSession().get(ContentFileDerivate.class, id);
        if (cfd == null) {
            return null;
        }
        
        FileAttachmentEntity att = getOriginalFileForDerivate(cfd);
        WGDocumentImpl docImpl = getParent().createDocumentImpl(att.getParentEntity());
        
        WGFileDerivateMetaData md = new WGFileDerivateMetaData(getParent().getDb(), WGDocument.buildDocumentKey(docImpl, getParent().getDb())  , cfd.getId(), cfd.getCreator(), cfd.getName(), cfd.getCreated(), cfd.getLastmodified(), cfd.getSize(), cfd.getDerivateSha512(), cfd.getParentSha512());
        loadMdExtensionData(md, cfd);
        return md;
        
        
    }


    
    protected FileAttachmentEntity getOriginalFileForDerivate(ContentFileDerivate cfd) throws WGAPIException {

        // On CS5P4 we can only fetch the "first best" original file that matches the checksum. There may be multiple
        String hql = "from ContentFileMeta as cfm where cfm.checksumSha512=:checksum";
        Query q = getParent().getSession().createQuery(hql);
        q.setParameter("checksum", cfd.getParentSha512());
        @SuppressWarnings("unchecked")
        Iterator<ContentFileMeta> it = q.iterate();
        if (it.hasNext()) {
            return (ContentFileMeta) it.next();
        }
        else {
            return null;
        }
        
    }

    protected void loadMdExtensionData(WGFileDerivateMetaData meta, ContentFileDerivate metaEntity) throws WGAPIException {
        for (String extDataName : metaEntity.getExtensionData().keySet()) {
            meta.writeExtensionData(extDataName, metaEntity.readExtensionData(getParent(), null, extDataName));
        }
       
    }

    public InputStream getFileDerivateData(String id) throws WGAPIException {
        
        ContentFileDerivate cfd = (ContentFileDerivate) getParent().getSession().get(ContentFileDerivate.class, id);
        if (cfd == null) {
            return null;
        }
        
        String hql = "select cfc from ContentFileContent as cfc where cfc.checksumSha512 = :checksum order by cfc.ordinalnr asc";
        Query q = getParent().getSession().createQuery(hql);
        q.setParameter("checksum", cfd.getDerivateSha512());
        @SuppressWarnings("rawtypes")
        Iterator it = q.iterate();
        try {
            if (!it.hasNext()) {
                return null;
            }
            ContentFileContent cfc = (ContentFileContent) it.next();
            
            String hqlQuery = "select cfp from ContentFileContentPart as cfp where cfp.fileContents = :contents order by cfp.partnr asc";
            Query query = getParent().getSession().createQuery(hqlQuery);                                       
            query.setParameter("contents", cfc);                 
            return createOptimizedInputStream(query);
        }
        finally {
            Hibernate.close(it);
        }

        
    }
    
    protected ContentFileContent fetchFileContents(String checksumSha512) throws WGAPIException {
        ContentFileContent fileContents = null;
        Query contentsQuery = getParent().getSession().createQuery("from ContentFileContent as cfc where cfc.checksumSha512 = :checksum order by cfc.ordinalnr asc");
        contentsQuery.setParameter("checksum", checksumSha512);
        Iterator<?> it = contentsQuery.iterate();
        try {
            if (it.hasNext()) {
                fileContents = (ContentFileContent) it.next();
            }
            return fileContents;
        }
        finally {
            Hibernate.close(it);
        }
    }
    
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException {
        
        ContentFileDerivate cfd = (ContentFileDerivate) getParent().getSession().get(ContentFileDerivate.class, md.getId());
        if (cfd == null) {
            throw new WGIllegalArgumentException("A file derivate '" + md.getId() + " does not exist");
        }
        
        storeMdExtensionData(md, cfd);
        cfd.setLastmodified(new Date());
        getParent().getSession().update(cfd);
        getParent().commitHibernateTransaction();
        
    }
    
    private void storeMdExtensionData(WGFileDerivateMetaData meta, ContentFileDerivate metaEntity) throws WGAPIException {
        
        // Get names of fields already stored
        Set<String> existingFields = new HashSet<String>();
        for (String name : metaEntity.getExtensionData().keySet()) {
            existingFields.add(name);
        }
        
        // Store fields
        for (String extDataName : meta.getExtensionDataNames()) {
            metaEntity.writeExtensionData(getParent(), null, extDataName, meta.getExtensionData(extDataName));
            existingFields.remove(extDataName);
        }
        
        // Process removed fields
        for (String fieldName : existingFields) {
            metaEntity.getExtensionData().remove(fieldName);
        }
        
    }
    
    @Override
    public long dailyFileMaintenance(Logger log) throws WGAPIException {

        Session session = getParent().getSession();
        try {
            
            session.setDefaultReadOnly(false);
            long freedMemory = 0;
            int deletedDuplicates = 0;
            
            // Remove duplicate file contents
            String hql = "select cfc.checksumSha512 as checksum from ContentFileContent as cfc group by cfc.checksumSha512 having count(*) > 1";
            @SuppressWarnings("unchecked")
            Iterator<String> duplicateChecksums = session.createQuery(hql).iterate();
            while (duplicateChecksums.hasNext()) {
                String duplicaceChecksum = duplicateChecksums.next();
                hql = "select cfc.id as id from ContentFileContent as cfc where cfc.checksumSha512 = :checksum order by cfc.ordinalnr asc";
                Query q = session.createQuery(hql);
                q.setParameter("checksum", duplicaceChecksum);
                @SuppressWarnings("unchecked")
                Iterator<String> duplicateIds = q.iterate();
                if (!duplicateIds.hasNext()) { // Just in case
                    continue;
                }
                
                // Skip the first one. That is the one that will be kept and constantly retrieved
                duplicateIds.next();
                
                // Delete the other duplicates
                while (duplicateIds.hasNext()) {
                    String id = duplicateIds.next();
                    ContentFileContent cfc = (ContentFileContent) session.get(ContentFileContent.class, id);
                    if (cfc != null) {
                        deletedDuplicates++;
                        freedMemory+=cfc.getSize();
                        session.setReadOnly(cfc, false);
                        
                        // Delete data entities via HQL to prevent loading them
                        hql = "delete ContentFileContentPart cfp where cfp.fileContents=:cfc";
                        Query deleteQ = session.createQuery(hql);
                        deleteQ.setParameter("cfc", cfc);
                        deleteQ.executeUpdate();
                        
                        session.delete(cfc);
                        getParent().commitHibernateTransaction();
                    }
                }
                Hibernate.close(duplicateIds);
            }
            Hibernate.close(duplicateChecksums);
            
            // Remove unused file contents
            long deletedUnusedFiles = 0;
            hql = "select cfc.id as id from ContentFileContent as cfc where cfc.checksumSha512 not in (select distinct cfm.checksumSha512 from ContentFileMeta as cfm where cfm.checksumSha512 is not null) and cfc.checksumSha512 not in (select distinct cfd.derivateSha512 from ContentFileDerivate as cfd)";
            @SuppressWarnings("unchecked")
            Iterator<String> obsoleteIds = session.createQuery(hql).iterate();
            while (obsoleteIds.hasNext()) {
                String id = obsoleteIds.next();
                ContentFileContent cfc = (ContentFileContent) session.get(ContentFileContent.class, id);
                if (cfc != null) {
                    deletedUnusedFiles++;
                    freedMemory+=cfc.getSize();
                    session.setReadOnly(cfc, false);
                    
                    // Delete data entities via HQL to prevent loading them
                    hql = "delete ContentFileContentPart cfp where cfp.fileContents=:cfc";
                    Query deleteQ = session.createQuery(hql);
                    deleteQ.setParameter("cfc", cfc);
                    deleteQ.executeUpdate();
                    
                    session.delete(cfc);
                    //log.info("Deleted file contents " + cfc.getChecksumSha512() + " ordinal nr " + cfc.getOrdinalnr());
                    getParent().commitHibernateTransaction();
                }
            }
            Hibernate.close(obsoleteIds);
            
            // Remove unused derivates of old CS5P4 style. Corresponding derivate data is deleted in the next run.
            hql = "select cfd.id as id from ContentFileDerivate as cfd where cfd.parentSha512 not in (select distinct cfc.checksumSha512 from ContentFileContent as cfc)";
            @SuppressWarnings("unchecked")
            Iterator<String> obsoleteDerivateIds = session.createQuery(hql).iterate();
            while (obsoleteDerivateIds.hasNext()) {
                String id = obsoleteDerivateIds.next();
                ContentFileDerivate cfd = (ContentFileDerivate) session.get(ContentFileDerivate.class, id);
                if (cfd != null) {
                    session.setReadOnly(cfd, false);
                    session.delete(cfd);
                    getParent().commitHibernateTransaction();
                }
            }
            Hibernate.close(obsoleteDerivateIds);
            
            String freedMemoryText;
            if (deletedDuplicates > 0 || deletedUnusedFiles > 0) {
                if (freedMemory > 1024 * 1024) {
                    freedMemoryText = WGUtils.DECIMALFORMAT_STANDARD.format(freedMemory / 1024 / 1024) + " MB of file storage";
                }
                else {
                    freedMemoryText = WGUtils.DECIMALFORMAT_STANDARD.format(freedMemory) + " Bytes of file storage";
                }
                log.info("Maintenance on content store of app/plugin '" + getParent().getDb().getDbReference() + "': Deleted " + WGUtils.DECIMALFORMAT_STANDARD.format(deletedDuplicates) + " duplicates and " + WGUtils.DECIMALFORMAT_STANDARD.format(deletedUnusedFiles) + " unused file contents freeing " + freedMemoryText);
            }
            return freedMemory;
            
        } 
        catch (Throwable e) {
            try {
                session.getTransaction().rollback();
            }
            catch (Exception e2) {
            }
            throw new WGBackendException("Exception running daily maintenance", e);
        }
        finally {
            session.setDefaultReadOnly(true);
        }
        
    }

}
