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

package de.innovationgate.wgpublisher.design;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;

public class OverlayStatus {
    
    
    public enum ChangeType {
        NEW,
        CHANGED,
        CONFLICT
    }
    
    public class ChangedDocument implements Comparable<ChangedDocument> {
        
        public ChangedDocument(int resourceType, FileObject sourceFile, FileObject targetFile, FileObject categoryFolder, ChangeType changeType, Set<String> involvedFiles) throws FileSystemException {
            super();
            
            // In case of a file container the resource is the folder, not the file
            if (resourceType == WGDocument.TYPE_FILECONTAINER) {
                if (sourceFile.getType().equals(FileType.FILE)) {
                    sourceFile = sourceFile.getParent();
                    targetFile = targetFile.getParent();
                }
            }
            
            _sourceFilePath = sourceFile.getName().getPath();
            _targetFilePath = targetFile.getName().getPath();
            _changeType = changeType;
            
            String resourceName = OverlayDesignProvider.OVERLAY_PREFIX + WGUtils.strReplace(categoryFolder.getName().getRelativeName(targetFile.getName()), "/", FileSystemDesignManager.DIRECTORY_DIVIDER, true);
            
            int suffixPos = resourceName.lastIndexOf(".");
            if (suffixPos != -1) {
                resourceName = resourceName.substring(0, suffixPos);
            }
            _documentKey = new WGDocumentKey(resourceType, resourceName, (resourceType == WGDocument.TYPE_FILECONTAINER ? null : categoryFolder.getName().getBaseName()));
            _involvedFiles = involvedFiles;
            
        }
        
        private String _targetFilePath;
        private String _sourceFilePath;
        private WGDocumentKey _documentKey;
        private ChangeType _changeType;
        private Set<String> _involvedFiles;
        
        public String getTargetFilePath() {
            return _targetFilePath;
        }
        public WGDocumentKey getDocumentKey() {
            return _documentKey;
        }
        public ChangeType getChangeType() {
            return _changeType;
        }
        public String getSourceFilePath() {
            return _sourceFilePath;
        }
        @Override
        public int compareTo(ChangedDocument o) {
            return _documentKey.toString().compareTo(o._documentKey.toString());
        }
        
        @Override
        public String toString() {
            return _documentKey.toString() + " " + _changeType;
        }
        public Set<String> getInvolvedFiles() {
            return _involvedFiles;
        }
        public void setInvolvedFiles(Set<String> involvedFiles) {
            _involvedFiles = involvedFiles;
        }
        
    }
    
    private boolean _newOverlay = false;
    
    private boolean _updatedBaseDesign = false;
    
    private Map<WGDocumentKey,ChangedDocument> _changedDocuments = new HashMap<WGDocumentKey, OverlayStatus.ChangedDocument>();
    
    private List<String> _newFolders = new ArrayList<String>();
    
    private OverlayData _overlayData = null;

    private Version _currentBaseVersion;

    public boolean isUpdatedBaseDesign() {
        return _updatedBaseDesign || _changedDocuments.size() > 0;
    }

    public void setUpdatedBaseDesign(boolean basePluginUpdated) {
        _updatedBaseDesign = basePluginUpdated;
    }

    public Map<WGDocumentKey, ChangedDocument> getChangedDocuments() {
        return _changedDocuments;
    }
    
    public void addChangedResource(int resourceType, FileObject sourceFile, FileObject targetFile, FileObject categoryFolder, ChangeType changeType, String resourcePath, Set<String> involvedFiles) throws NoSuchAlgorithmException, IOException {
        
        ChangedDocument resource = new ChangedDocument(resourceType, sourceFile, targetFile, categoryFolder, changeType, involvedFiles);
        _changedDocuments.put(resource.getDocumentKey(), resource);
        
    }

    public OverlayData getOverlayData() {
        return _overlayData;
    }

    public void setOverlayData(OverlayData overlayData) {
        _overlayData = overlayData;
    }

    public List<String> getNewFolders() {
        return _newFolders;
    }

    public void setNewFolders(List<String> newFolders) {
        _newFolders = newFolders;
    }

    public boolean isNewOverlay() {
        return _newOverlay;
    }

    public void setNewOverlay(boolean newOverlay) {
        _newOverlay = newOverlay;
    }
    
    public Version getCompliantBaseVersion() {
        return new Version(_overlayData.getBasepluginVersion());
    }
    
    public Version getCurrentBaseVersion() {
        return _currentBaseVersion;
    }

    public void setCurrentBaseVersion(Version currentBaseVersion) {
        _currentBaseVersion = currentBaseVersion;
    }

    public void overlayWasUpgraded() {

        _changedDocuments.clear();
        _newFolders.clear();
        _updatedBaseDesign = false;
        _newOverlay = false;
        
    }

}
