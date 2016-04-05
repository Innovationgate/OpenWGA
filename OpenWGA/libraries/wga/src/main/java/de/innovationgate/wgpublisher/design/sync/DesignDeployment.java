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
package de.innovationgate.wgpublisher.design.sync;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.design.fs.AbstractDesignFile;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;

public abstract class DesignDeployment extends AbstractDesignFile {

    protected long _timestampOfCodeFile = 0;
    protected long _timestampOfMetadataFile = 0;
    private transient long _codeFileSize = -1;
    private String _documentKey;
    private transient int _failures = 0;
    protected DesignSyncStatus _parent;

    public DesignDeployment(DesignSyncStatus parent, int type, String documentKey, FileObject codeFile) throws IOException, WGDesignSyncException {
        super(parent.getManager(), codeFile, type);
        _parent = parent;
        _documentKey = documentKey;
    }
    
    
    /**
     * Private No-args constructor. For serialisation only
     */
    protected DesignDeployment() {
        
    }

    public boolean isUpdated() throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {

        FileObject metadataFile = getMetadataFile();
        FileObject codeFile = getCodeFile();
        
        if (_codeFileSize == -1) {
            _codeFileSize = codeFile.getContent().getSize();
        }
        
        return ((
                metadataFile.exists() && 
                metadataFile.getContent().getLastModifiedTime() != _timestampOfMetadataFile) || 
                codeFile.getContent().getLastModifiedTime() != _timestampOfCodeFile ||
                codeFile.getContent().getSize() != _codeFileSize
        );

    }
    
    public boolean isDeleted() throws FileSystemException, WGDesignSyncException {
        return !getCodeFile().exists();
    }

    public abstract void performUpdate(WGDatabase db) throws WGException, IOException, InstantiationException, IllegalAccessException, WGDesignSyncException;



    @Override
    public FileSystemDesignManager getManager() {
        return _parent.getManager();
    }

    /**
     * @return Returns the timestampOfCodeFile.
     */
    public long getTimestampOfCodeFile() {
        return _timestampOfCodeFile;
    }

    /**
     * @return Returns the timestampOfMetadataFile.
     */
    public long getTimestampOfMetadataFile() {
        return _timestampOfMetadataFile;
    }

    public void resetUpdateInformation() throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        _timestampOfCodeFile = getCodeFile().getContent().getLastModifiedTime();
        _codeFileSize = getCodeFile().getContent().getSize();
        FileObject metadataFile = getMetadataFile();
        if (metadataFile.exists()) {
            _timestampOfMetadataFile = metadataFile.getContent().getLastModifiedTime();
        }
    }

    /**
     * @return Returns the documentKey.
     */
    public String getDocumentKey() {
        return _documentKey;
    }

    public void performDeletion(WGDatabase db) throws WGException {
        WGDocument doc = getDocument(db);
        if (doc != null) {
            doc.remove();
        }
    }
    
    @Override
    protected Logger getLog() {
        return getManager().getLog();
    }


    public WGDocument getDocument(WGDatabase db) throws WGAPIException {
        return db.getDocumentByDocumentKey(getDocumentKey());
    }
 
    /**
     * @return Returns the parent.
     */
    public DesignSyncStatus getParent() {
        return _parent;
    }

    /**
     * @param parent The parent to set.
     */
    public void setParent(DesignSyncStatus parent) {
        _parent = parent;
    }
    
    public void doAttachFile(WGDocument doc, FileObject file) throws WGDesignSyncException {
        
        try {
            if (!file.exists()) {
                throw new WGDesignSyncException("Attaching file '" + file.getName().getPath() + "' to document '" + doc.getDocumentKey() + "' failed because the file does not exist.");
            }
            
            if (!doc.attachFile(file.getContent().getInputStream(), file.getName().getBaseName())) {
                throw new WGDesignSyncException("Attaching file '" + file.getName().getPath() + "' to document '" + doc.getDocumentKey() + "' failed.");
            }
        } 
        catch (Exception e) {
            throw new WGDesignSyncException("Attaching file '" + file.getName().getPath() + "' to document '" + doc.getDocumentKey() + "' failed.", e);
        }
        
        
        
        
    }
    
    public void doSaveDocument(WGDocument doc) throws WGDesignSyncException {
        
        try {
            if (!doc.save()) {
                throw new WGDesignSyncException("Saving document '" + doc.getDocumentKey() + "' failed.");
            }
        }
        catch (WGAPIException e) {
            throw new WGDesignSyncException("Saving document '" + doc.getDocumentKey() + "' failed because of wgapi exception: " + e.getMessage(), e);
        }        
    }
    
    public void doRemoveFile(WGDocument doc, String fileName) throws WGDesignSyncException {
        try {
            if (!doc.removeFile(fileName)) {
                throw new WGDesignSyncException("Removing file '" + fileName + "' from document '" + doc.getDocumentKey() + "' failed.");
            }
        } catch (WGAPIException e) {
            throw new WGDesignSyncException("Removing file '" + fileName + "' from document '" + doc.getDocumentKey() + "' failed bc. of backend exception: " + e.getMessage(), e);
        }
        
    }
    
    protected boolean addFailure() {
        _failures++;
        if (_failures >= 5) {
            _failures = 0;
            return true;
        }
        else {
            return false;
        }
    }


    @Override
    protected void createMetadataFile(FileObject metadataFile) throws InstantiationException, IllegalAccessException, UnsupportedEncodingException, FileSystemException, IOException {
        super.createMetadataFile(metadataFile);
        _timestampOfMetadataFile = metadataFile.getContent().getLastModifiedTime();
    }
    
    private Object readResolve() {
        _codeFileSize = -1;
        return this;
    }
}
