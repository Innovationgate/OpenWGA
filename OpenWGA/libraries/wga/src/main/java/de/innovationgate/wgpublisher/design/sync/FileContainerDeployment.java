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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.wgpublisher.design.fs.FCMetadata;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;

public class FileContainerDeployment extends DesignDeployment {

    public static class FileNameComparator implements Comparator {

        public int compare(Object arg0, Object arg1) {
            
            String fileName0 = ((FileObject) arg0).getName().getBaseName();
            String fileName1 = ((FileObject) arg1).getName().getBaseName();
            return fileName0.compareTo(fileName1);
            
        }
        
    }
    
    public static class ContainerFile {

        private String _name;

        private long _timestamp;
        
        
        /**
         * Private default constructor. For serialisation only 
         */
        private ContainerFile() {
            
        }

        public ContainerFile(String name, long timestamp) {
            _name = name;
            _timestamp = timestamp;
        }

        public ContainerFile(FileObject file) throws FileSystemException {
            _name = file.getName().getBaseName().toLowerCase();
            _timestamp = file.getContent().getLastModifiedTime();
        }

        /**
         * @return Returns the name.
         */
        public String getName() {
            return _name;
        }

        /**
         * @param name
         *            The name to set.
         */
        public void setName(String name) {
            _name = name;
        }

        /**
         * @return Returns the timestamp.
         */
        public long getTimestamp() {
            return _timestamp;
        }

        /**
         * @param timestamp
         *            The timestamp to set.
         */
        public void setTimestamp(long timestamp) {
            _timestamp = timestamp;
        }

    }

    private Map<String,FileObject> _files = new HashMap<String,FileObject>();
    private transient Set<String> _warnings; 
    
    /**
     * Private Default constructor. For serialisation only
     */
    private FileContainerDeployment() {
        
    }

    public FileContainerDeployment(DesignSyncStatus parent, String documentKey, FileObject codeFile) throws IOException, WGDesignSyncException {
        super(parent, WGDocument.TYPE_FILECONTAINER, documentKey, codeFile);
    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.wgpublisher.designsync.DesignDeployment#isUpdated()
     */
    public boolean isUpdated() throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        FileObject metadataFile = getMetadataFile();
        if (metadataFile.exists() && metadataFile.getContent().getLastModifiedTime() != _timestampOfMetadataFile) {
            return true;
        }

        List<FileObject> files = getFileContainerFiles();
        Collections.sort(files, new FileNameComparator());
        
        FileObject file;
        List<String> existingFiles = new ArrayList<String>();
        
        // Check for new and updated files
        for (int i = 0; i < files.size(); i++) {
            file = (FileObject) files.get(i);
            if (isExcludedFileContainerFile(file)) {
                continue;
            }

            ContainerFile containerFile = getContainerFile(file);
            if (containerFile == null) {
                return true;
            }
            else if (existingFiles.contains(containerFile.getName().toLowerCase())) {
                
                // Possible on case-sensitive file systems. A file with same name and
                // different case has already been processed. Ignore this duplicate.
                issueWarning(file.getName().getPathDecoded());
                continue;
            }
            else {
                existingFiles.add(containerFile.getName().toLowerCase());
                if (containerFile.getTimestamp() != file.getContent().getLastModifiedTime()) {
                    return true;
                }
            }
        }
        
        // Check for deleted files
        List<String> containerFiles = new ArrayList<String>(_files.keySet());
        containerFiles.removeAll(existingFiles);
        if (containerFiles.size() > 0) {
            return true;
        }
        else {
            return false;
        }

    }

    private void issueWarning(String name) {
        
        if (_warnings == null) {
            _warnings = new HashSet<String>();
        }
        
        if (!_warnings.contains(name)) {
            Logger.getLogger(FileSystemDesignManager.LOGGER_DESIGNSYNC).warn("The file '" + name + "' seems to be present in multiple versions with differently cased names. All but the first one will be ignored.");
            _warnings.add(name);
        }
        
    }

    protected ContainerFile getContainerFile(FileObject file) {
        return (ContainerFile) _files.get(file.getName().getBaseName().toLowerCase());
    }



    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.wgpublisher.designsync.DesignDeployment#performUpdate(de.innovationgate.webgate.api.WGDocument)
     */
    public void performUpdate(WGDatabase db) throws IOException, WGException, InstantiationException, IllegalAccessException, WGDesignSyncException {

        FileObject codeFile = getCodeFile();
        // Check if file has been deleted in the meantime
        if (!codeFile.exists()) {
            return;
        }
        
        WGFileContainer con = (WGFileContainer) db.getDocumentByDocumentKey(getDocumentKey());
        if (con == null) {
            WGDocumentKey docKey = new WGDocumentKey(getDocumentKey());
            con = db.createFileContainer(docKey.getName());
            con.save();
        }

        // Find new and updated files
        FileObject[] filesArray = codeFile.getChildren();
        if (filesArray == null) {
            throw new WGDesignSyncException("Cannot collect files from directory '" + codeFile.getName().getPathDecoded() + "'. Please verify directory existence.");
        }
        
        List<FileObject> files = new ArrayList<FileObject>(Arrays.asList(filesArray));
        Collections.sort(files, new FileNameComparator());
        
        FileObject file;
        Map existingFiles = new HashMap();
        for (int i = 0; i < files.size(); i++) {
            file = (FileObject) files.get(i);
            if (isExcludedFileContainerFile(file)) {
                continue;
            }

            ContainerFile containerFile = getContainerFile(file);
            if (containerFile != null) {
                if (existingFiles.containsKey(containerFile.getName())) {
                    // Possible in case-sensitive file systems. Another file of the same name with another case is in the dir.
                    continue;                 
                }
                
                if (file.getContent().getLastModifiedTime() != containerFile.getTimestamp()) {
                   doRemoveFile(con, file.getName().getBaseName());
                   doSaveDocument(con);
                   doAttachFile(con, file);
                   doSaveDocument(con);
                   containerFile.setTimestamp(file.getContent().getLastModifiedTime());
                }
            }
            else {
                containerFile = new ContainerFile(file);
                if (con.hasFile(file.getName().getBaseName())) {
                    doRemoveFile(con, file.getName().getBaseName());
                    doSaveDocument(con);
                }
                doAttachFile(con, file);
                doSaveDocument(con);
            }
            existingFiles.put(containerFile.getName(), containerFile);
        }

        // Remove deleted files from container
        _files = existingFiles;
        List fileNamesInContainer = WGUtils.toLowerCase(con.getFileNames());
        fileNamesInContainer.removeAll(existingFiles.keySet());
        Iterator deletedFiles = fileNamesInContainer.iterator();
        while (deletedFiles.hasNext()) {
            con.removeFile((String) deletedFiles.next());
            con.save();
        }

        // Update metadata and save
        FCMetadata metaData = (FCMetadata) readMetaData();
        metaData.writeToDocument(con);
        con.save();
        FileObject metadataFile = getMetadataFile();
        if (metadataFile.exists()) {
            _timestampOfMetadataFile = metadataFile.getContent().getLastModifiedTime();
        }
        
        // We wont do this here, since this would rebuild all ContainerFiles, which is not neccessary
        // Instead we have updated just the metadata file timestamp
        // resetUpdateInformation();
    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.wgpublisher.designsync.DesignDeployment#resetTimestamps()
     */
    public void resetUpdateInformation() throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {

        
        FileObject folder = getCodeFile();
        Map files = new HashMap();
        FileObject[] folderFiles = folder.getChildren();
        FileObject folderFile;
        for (int i = 0; i < folderFiles.length; i++) {

            folderFile = folderFiles[i];
            if (isExcludedFileContainerFile(folderFile)) {
                continue;
            }

            ContainerFile containerFile = new ContainerFile(folderFile);
            files.put(containerFile.getName(), containerFile);
        }
        _files = files;
        
        
        FileObject metadataFile = getMetadataFile();
        if (metadataFile.exists()) {
            _timestampOfMetadataFile = metadataFile.getContent().getLastModifiedTime();
        }

    }

}
