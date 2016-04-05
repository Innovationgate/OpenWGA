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
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.Query;
import org.hibernate.Session;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.jdbc.ContentFileDerivate;
import de.innovationgate.webgate.api.jdbc.ContentFileMeta;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.LogEntry;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS5P5FileAttachmentHandler extends CS5P4FileAttachmentHandler {
    
    protected Map<String,ExtensionData> _newBinExtensionData = new HashMap<String,ExtensionData>();
    protected CS5P5FileAttachmentHandler(CS5P5FileHandling handling, WGDocumentImpl doc, CS41FileEntityDescriptor entityDescriptor) {
        super(handling, doc, entityDescriptor);
    }
    
    public List<WGFileDerivateMetaData> getFileDerivates(String fileName) throws WGAPIException {

        CS5P4FileAttachmentEntity entity = fetchFileAttachmentEntity(fileName);
        if (entity == null || entity.getChecksumSha512() == null) {
            return null;
        }
        
        WGDocumentImpl docImpl = _handling.getParent().createDocumentImpl(entity.getParentEntity());
        List<WGFileDerivateMetaData> metaData = new ArrayList<WGFileDerivateMetaData>();
        
        // CS5P5 derivates, directly connected to their original attachment
        String hql = "select cfd from ContentFileDerivate as cfd where cfd.parentMeta = :meta";
        Query q = _handling.getParent().getSession().createQuery(hql);
        q.setParameter("meta", entity);
        Iterator<?> derivates = q.iterate();
        while (derivates.hasNext()) {
            ContentFileDerivate cfd = (ContentFileDerivate) derivates.next();
            WGFileDerivateMetaData md = new WGFileDerivateMetaData(_doc.getParent().getDb(), WGDocument.buildDocumentKey(docImpl, _doc.getParent().getDb()), cfd.getId(), cfd.getCreator(), cfd.getName(), cfd.getCreated(), cfd.getLastmodified(), cfd.getSize(), cfd.getDerivateSha512(), cfd.getParentSha512());
            ((CS5P4FileHandling) _handling).loadMdExtensionData(md, cfd);
            metaData.add(md);
        }
        
        // Fallback for derivates still stored CS5P4 style, connected only loosely via SHA512 checksum
        hql = "select cfd from ContentFileDerivate as cfd where cfd.parentMeta is null and cfd.parentSha512 = :checksum";
        q = _handling.getParent().getSession().createQuery(hql);
        q.setParameter("checksum", entity.getChecksumSha512());
        derivates = q.iterate();
        while (derivates.hasNext()) {
            ContentFileDerivate cfd = (ContentFileDerivate) derivates.next();
            WGFileDerivateMetaData md = new WGFileDerivateMetaData(_doc.getParent().getDb(), WGDocument.buildDocumentKey(docImpl, _doc.getParent().getDb()), cfd.getId(), cfd.getCreator(), cfd.getName(), cfd.getCreated(), cfd.getLastmodified(), cfd.getSize(), cfd.getDerivateSha512(), cfd.getParentSha512());
            ((CS5P4FileHandling) _handling).loadMdExtensionData(md, cfd);
            metaData.add(md);
        }
        
        return metaData;
        
    }
    
    @Override
    public FileAttachmentEntity saveFileMetaData(WGFileMetaData md, LogEntry updateLog) throws WGAPIException {
        md.writeExtensionData(WGFileMetaData.EXTDATA_UPDATE_REVISION, updateLog.getLog_id());
        FileAttachmentEntity att = super.saveFileMetaData(md, updateLog);
        return att;
    }
    

    
    @Override
    public void saveFileData(AttachFileOperation<CS41FileAttachmentEntity> op) throws WGAPIException {
        super.saveFileData(op);
        
        // Write the revision when the attachment was updated as extdata
        if (op.getUpdateLog() != null) {
            WGFileMetaData meta = _doc.getFileMetaData(op.getEntity().getName());
            meta.writeExtensionData(WGFileMetaData.EXTDATA_UPDATE_REVISION, op.getUpdateLog().getLog_id());
        }

    }
    
    @Override
    public void addBinaryExtensionData(ExtensionData extData, Object value) throws WGAPIException {
        try {
            extData.setSourceData(new BinaryFieldData(value));
            _newBinExtensionData.put(extData.getName(), extData);
        }
        catch (IOException e) {
            throw new WGBackendException("Exception adding binary extension data", e);
        }
    }
    
    @Override
    public void afterSave(LogEntry updateLog) throws WGAPIException {
        
        super.afterSave(updateLog);

        // Save binary extension data
        try {
            Session session = _handling.getParent().getSession();
            Iterator<ExtensionData> extDatas = _newBinExtensionData.values().iterator();
            while (extDatas.hasNext()) {
                ExtensionData extData = extDatas.next();
                InputStream in = new BufferedInputStream(extData.getSourceData().getInputStream());
                try {
                    ((CS5P4FileHandling) _handling).storeFileContents(extData, _entityDescriptor, in);
                }
                finally {
                    in.close();
                    extData.setSourceData(null);
                }
            }
            _newBinExtensionData.clear();
        }
        catch (Exception e) {
            throw new WGBackendException("Exception saving binary extension data", e);
        }
        
    }

    protected Map<String, ExtensionData> getNewBinExtensionData() {
        return _newBinExtensionData;
    }

    @Override
    protected AttachFileOperation<?> pushFile(WGDocumentImpl docCopy, String fileName) throws IOException, WGAPIException {
        AttachFileOperation<?> op = super.pushFile(docCopy, fileName);
        
        // Also push all derivates, so they are immediately available (#00003749). They may be recreated on run of the derivate update process.
        if (_doc.getParent() == docCopy.getParent()) {
            
            for (WGFileDerivateMetaData derivateMd : getFileDerivates(fileName)) {
                
                ContentFileDerivate cfd = new ContentFileDerivate();
                cfd.setName(derivateMd.getName());
                cfd.setParentMeta((ContentFileMeta) op.getEntity());
                cfd.setCreated(derivateMd.getCreated());
                cfd.setLastmodified(derivateMd.getLastModified());
                cfd.setCreator(derivateMd.getCreator());
                cfd.setDerivateSha512(derivateMd.getSha512Checksum());
                cfd.setParentSha512(derivateMd.getParentSha512Checksum());
                cfd.setSize(derivateMd.getSize());
                
                for (String name : derivateMd.getExtensionDataNames()) {
                    cfd.writeExtensionData(_handling.getParent(), docCopy, name, derivateMd.getExtensionData(name));
                }
                
                if (_handling.getParent().isSaveIsolationActive()) {
                    docCopy.addAdditionalEntityToSave(cfd);
                }
                
            }
            
        }
        
        return op;
    }
    
    @Override
    public void saveAdditionalObject(Object obj, LogEntry logEntry) throws WGAPIException {

        if (obj instanceof ContentFileDerivate) {
            ContentFileDerivate cfd = (ContentFileDerivate) obj;
            cfd.writeExtensionData(_handling.getParent(), _doc, WGFileDerivateMetaData.EXTDATA_ORIGINALREVISION, logEntry.getLog_id());
        }
        
        super.saveAdditionalObject(obj, logEntry);
    }
    
    @Override
    public void renameFile(String name, String newName) throws WGAPIException {
        super.renameFile(name, newName);
    }
    
    

}
