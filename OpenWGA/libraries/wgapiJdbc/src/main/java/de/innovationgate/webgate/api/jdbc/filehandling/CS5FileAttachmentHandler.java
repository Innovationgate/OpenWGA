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
import java.io.IOException;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS5FileAttachmentHandler extends CS41FileAttachmentHandler {

    protected CS5FileAttachmentHandler(CS5FileHandling handling, WGDocumentImpl doc, CS41FileEntityDescriptor entityDescriptor) {
        super(handling, doc, entityDescriptor);
    }
    
    protected AttachFileOperation<?> pushFile(WGDocumentImpl docCopy, String fileName) throws IOException, WGAPIException {
        AttachFileOperation<?> op = super.pushFile(docCopy, fileName);
        doPushAnnotations(docCopy, fileName);
        return op;
    }

    protected void doPushAnnotations(WGDocumentImpl docCopy, String fileName) throws WGAPIException {
        WGFileMetaData sourceMeta = _doc.getFileMetaData(fileName);
        WGFileMetaData targetMeta = docCopy.getFileMetaData(fileName);
        sourceMeta.pushData(targetMeta);
    }
    
    @Override
    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String filename) throws WGAPIException {
        
        FileAttachmentEntity metaEntity = retrieveFileMetaEntity(filename);
        if (metaEntity != null) {
            return _doc.createFileExtDataHandler(_handling.getParent(), metaEntity);
        }    
    
        return null;
        
    }
    


}
