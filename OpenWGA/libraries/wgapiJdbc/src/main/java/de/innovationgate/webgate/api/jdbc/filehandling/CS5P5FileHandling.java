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

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;

import org.apache.log4j.Logger;
import org.hibernate.Hibernate;
import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.BinaryFieldData.Retriever;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDuplicateKeyException;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.jdbc.ContentFileContent;
import de.innovationgate.webgate.api.jdbc.ContentFileDerivate;
import de.innovationgate.webgate.api.jdbc.ContentFileMeta;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS5P5FileHandling extends CS5P4FileHandling {

    public static class BinaryExtensionDataRetriever implements Retriever {

        private String _binarySha512;
        private CS5P5FileHandling _fileHandling;

        public BinaryExtensionDataRetriever(CS5P5FileHandling fileHandling, String binarySha512) {
            _fileHandling = fileHandling;
            _binarySha512 = binarySha512;
        }

        @Override
        public InputStream retrieveInputStream() throws IOException, WGAPIException {
            ContentFileContent cfc = _fileHandling.fetchFileContents(_binarySha512);
            if (cfc != null) {
                String hqlQuery = "select cfp from ContentFileContentPart as cfp where cfp.fileContents=:cfc order by cfp.partnr asc";
                final Query query = _fileHandling.getParent().getSession().createQuery(hqlQuery);
                query.setParameter("cfc", cfc);
                return _fileHandling.createOptimizedInputStream(query);
            }
            else {
                throw new IOException("Cannot find file contents for sha512 hash: " + _binarySha512);
            }
            
        }
        
    }

    @Override
    public FileAttachmentHandler createDocumentHandler(WGDocumentImpl doc) {

        if (doc.getType() == WGDocument.TYPE_CONTENT) {
            return new CS5P5FileAttachmentHandler(this, doc, new CS5P4ContentFileDescriptor());
        }
        else if (doc.getType() == WGDocument.TYPE_FILECONTAINER) {
            return new CS5FileAttachmentHandler(this, doc, new CS41ContainerFileDescriptor());
        }

        return null;

    }

    public String storeFileDerivate(WGFileMetaData originalFileMd, String creator, String derivateName, InputStream in, Map<String, Object> customMdFields) throws WGAPIException {

        // Fetch the meta so we can bind to it
        WGDocumentImpl docCore = (WGDocumentImpl) originalFileMd.getParentDocument().getCore();
        if (docCore == null) {
            throw new WGBackendException("Cannot store file derivate for a file, as it does not belong to a document");
        }

        String hql = "from ContentFileMeta as cfm where cfm.parentcontent =:content and cfm.name=:name";
        Query q = getParent().getSession().createQuery(hql);
        q.setParameter("content", docCore.getEntity());
        q.setParameter("name", originalFileMd.getName());
        java.util.Iterator<?> results = q.iterate();
        if (!results.hasNext()) {
            throw new WGBackendException("The metadata entity of file '" + originalFileMd.getName() + "' cannot be found");
        }
        ContentFileMeta cfm = (ContentFileMeta) results.next();

        // Lookup if this derivate of the parent/creator/name combination
        // exists, if so cancel
        hql = "from ContentFileDerivate as cfd where cfd.parentMeta=:meta and cfd.creator=:creator and cfd.name=:name";
        q = getParent().getSession().createQuery(hql);
        q.setParameter("meta", cfm);
        q.setParameter("creator", creator);
        q.setParameter("name", derivateName);
        if (q.iterate().hasNext()) {
            throw new WGDuplicateKeyException("A file derivate '" + creator + "/" + derivateName + "' for that file content does already exist");
        }

        // Lookup if an old CSP4 style derivate of the sha512/creator/name
        // combination exists, if so delete it
        hql = "from ContentFileDerivate as cfd where cfd.parentMeta is null and cfd.parentSha512=:checksum and cfd.creator=:creator and cfd.name=:name";
        q = getParent().getSession().createQuery(hql);
        q.setParameter("checksum", originalFileMd.getSha512Checksum());
        q.setParameter("creator", creator);
        q.setParameter("name", derivateName);
        @SuppressWarnings("rawtypes")
        java.util.Iterator oldDerivates = q.iterate();
        while (oldDerivates.hasNext()) {
            getParent().getSession().delete(oldDerivates.next());
        }

        // Create the new derivate
        try {
            ContentFileDerivate cfd = new ContentFileDerivate();
            cfd.setParentMeta(cfm);
            cfd.setParentSha512(originalFileMd.getSha512Checksum());
            cfd.setCreator(creator);
            cfd.setName(derivateName);
            cfd.setCreated(new Date());
            cfd.setLastmodified(new Date());

            if (customMdFields != null) {
                for (Map.Entry<String, Object> entry : customMdFields.entrySet()) {
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
             * Das müllt die historylog Tabelle voll.
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

                // Skip the first one. That is the one that will be kept and
                // constantly retrieved
                duplicateIds.next();

                // Delete the other duplicates
                while (duplicateIds.hasNext()) {
                    String id = duplicateIds.next();
                    ContentFileContent cfc = (ContentFileContent) session.get(ContentFileContent.class, id);
                    if (cfc != null) {
                        deletedDuplicates++;
                        freedMemory += cfc.getSize();
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

            // Remove unused file contents, not used on attachments, derivates
            // or binary extension data
            long deletedUnusedFiles = 0;
            hql = "select cfc.id as id from ContentFileContent as cfc where cfc.checksumSha512 not in (select distinct cfm.checksumSha512 from ContentFileMeta as cfm where cfm.checksumSha512 is not null) and cfc.checksumSha512 not in (select distinct cfd.derivateSha512 from ContentFileDerivate as cfd) and cfc.checksumSha512 not in (select distinct ext.binarySha512 from ExtensionData as ext where ext.type=7)";
            @SuppressWarnings("unchecked")
            Iterator<String> obsoleteIds = session.createQuery(hql).iterate();
            while (obsoleteIds.hasNext()) {
                String id = obsoleteIds.next();
                ContentFileContent cfc = (ContentFileContent) session.get(ContentFileContent.class, id);
                if (cfc != null) {
                    deletedUnusedFiles++;
                    freedMemory += cfc.getSize();
                    session.setReadOnly(cfc, false);

                    // Delete data entities via HQL to prevent loading them
                    hql = "delete ContentFileContentPart cfp where cfp.fileContents=:cfc";
                    Query deleteQ = session.createQuery(hql);
                    deleteQ.setParameter("cfc", cfc);
                    deleteQ.executeUpdate();

                    session.delete(cfc);
                    // log.info("Deleted file contents " +
                    // cfc.getChecksumSha512() + " ordinal nr " +
                    // cfc.getOrdinalnr());
                    getParent().commitHibernateTransaction();
                }
            }
            Hibernate.close(obsoleteIds);

            // Remove unused derivates of old CS5P4 style. Corresponding
            // derivate data is deleted in the next run.
            hql = "select cfd.id as id from ContentFileDerivate as cfd where cfd.parentMeta is null and cfd.parentSha512 not in (select distinct cfc.checksumSha512 from ContentFileContent as cfc)";
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
                log.info("Maintenance on content store of app/plugin '" + getParent().getDb().getDbReference() + "': Deleted " + WGUtils.DECIMALFORMAT_STANDARD.format(deletedDuplicates) + " duplicates and "
                        + WGUtils.DECIMALFORMAT_STANDARD.format(deletedUnusedFiles) + " unused file contents freeing " + freedMemoryText);
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

    @Override
    public void writeBinaryExtensionData(WGDocumentImpl doc, ExtensionData extData, Object value) throws WGBackendException {

        try {
            // Add the data to the document
            if (doc != null) {
                doc.getFileHandler().addBinaryExtensionData(extData, value);
            }

            // With no document as "save entity" we directly write the binary
            // data
            else {
                InputStream in = BinaryFieldData.getBinaryDataInputStream(value);
                MessageDigest digestMd5 = MessageDigest.getInstance("MD5");
                MessageDigest digestSha512 = MessageDigest.getInstance("SHA-512");
                ContentFileContent fileContent = storeFileContents(new CS5P4ContentFileDescriptor(), in, getParent().getSession(), digestMd5, digestSha512);
                extData.setBinarySha512(fileContent.getChecksumSha512());
            }
        }
        catch (Exception e) {
            throw new WGBackendException("Exception storing binary extension data", e);
        }

    }

    @Override
    public BinaryFieldData readBinaryExtensionData(WGDocumentImpl doc, ExtensionData extData) throws WGAPIException {

        if (extData.getSourceData() != null) { // Unsaved ext data
            return extData.getSourceData();
        }
        
        ContentFileContent fileContents = fetchFileContents(extData.getBinarySha512());
        if (fileContents != null) {
            return new BinaryFieldData(fileContents.getSize(), new BinaryExtensionDataRetriever(this, extData.getBinarySha512()));
        }
        else {
            return null;
        }

    }
    
    @Override
    protected FileAttachmentEntity getOriginalFileForDerivate(ContentFileDerivate cfd) throws WGAPIException {
        if (cfd.getParentMeta() != null) {
            return cfd.getParentMeta();
        }
        return super.getOriginalFileForDerivate(cfd);
    }

}
