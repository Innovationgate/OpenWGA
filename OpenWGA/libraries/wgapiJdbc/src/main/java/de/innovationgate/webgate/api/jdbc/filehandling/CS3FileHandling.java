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

import java.io.InputStream;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.jdbc.ExtensionData;
import de.innovationgate.webgate.api.jdbc.Item;
import de.innovationgate.webgate.api.jdbc.WGDatabaseImpl;
import de.innovationgate.webgate.api.jdbc.WGDocumentImpl;

public class CS3FileHandling implements FileHandling {

    private WGDatabaseImpl _parent;

    @Override
    public void init(WGDatabaseImpl db) {
        _parent = db;
    }

    @Override
    public FileAttachmentHandler createDocumentHandler(WGDocumentImpl doc) {
        
        if (doc.getType() == WGDocument.TYPE_CONTENT) {
            return new CS3FileAttachmentHandler(this, doc, new CS3ContentFileDescriptor());
        }
        else if (doc.getType() == WGDocument.TYPE_FILECONTAINER) {
            return new CS3FileAttachmentHandler(this, doc, new CS3ContainerFileDescriptor());
        }
        
        return null;
    }

    @Override
    public void destroy() {
    }

    public WGDatabaseImpl getParent() {
        return _parent;
    }
    
    @Override
    public long dailyFileMaintenance(Logger log) throws WGBackendException {
        return 0;
    }
    
    @Override
    public void writeBinaryExtensionData(WGDocumentImpl doc, ExtensionData extData, Object value) throws WGBackendException {
        throw new WGNotSupportedException("Binary extension data is not supported on this content store version/patch level");
    }
    
    @Override
    public BinaryFieldData readBinaryExtensionData(WGDocumentImpl doc, ExtensionData extData) throws WGBackendException {
        throw new WGNotSupportedException("Binary extension data is not supported on this content store version/patch level");
    }


}
