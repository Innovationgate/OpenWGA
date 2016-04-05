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

import java.io.File;
import java.io.InputStream;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.LogEntry;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public interface FileAttachmentHandler {
    
    public AttachFileOperation<?> attachFile(File file, String fileName) throws WGAPIException;
    
    public void removeFile(String name) throws WGAPIException;
    
    public void beforeSave() throws WGAPIException;
    
    public void afterSave(LogEntry updateLog) throws WGAPIException;
    
    public void beforeRemove() throws WGAPIException;
    
    public void afterRemove() throws WGAPIException;

    public void pushFiles(WGDocumentImpl doc) throws WGAPIException;
    
    public InputStream getFileData(String name) throws WGAPIException;
    
    public WGFileMetaData getFileMetaData(String name) throws WGAPIException;
    
    public FileAttachmentEntity saveFileMetaData(WGFileMetaData md, LogEntry updateLog) throws WGAPIException;
    
    public int getFileSize(String name) throws WGAPIException;
    
    public boolean hasFile(String name) throws WGAPIException;
    
    public boolean isFileMetaDataAvailable() throws WGAPIException;
    
    public void renameFile(String name, String newName) throws WGAPIException;
    
    public List<String> getFileNames() throws WGAPIException;
    
    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String filename) throws WGAPIException;

    public void addBinaryExtensionData(ExtensionData extData, Object value) throws WGAPIException;

    public void saveAdditionalObject(Object obj, LogEntry logEntry) throws WGAPIException;

    
}
