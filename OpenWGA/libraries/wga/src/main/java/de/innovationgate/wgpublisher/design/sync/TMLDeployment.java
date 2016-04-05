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

import org.apache.commons.vfs2.FileObject;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wgpublisher.design.fs.DesignMetadata;
import de.innovationgate.wgpublisher.design.fs.TMLMetadata;

public class TMLDeployment extends DesignDeployment {
   
    /**
     * Private No-args constructor. For serialisation only
     */
    private TMLDeployment() {
        super();
    }
    
    public TMLDeployment(DesignSyncStatus deployments, String documentKey, FileObject codeFile) throws IOException, WGDesignSyncException {
        super(deployments, WGDocument.TYPE_TML, documentKey, codeFile);
    }

    public void performUpdate(WGDatabase db) throws WGDesignSyncException, WGException, IOException, InstantiationException, IllegalAccessException {
        
        // Check if file has been deleted in the meantime
        if (!getCodeFile().exists()) {
            return;
        }
        
        TMLMetadata metaData = (TMLMetadata) readMetaData();
        String code = readCode(metaData);
        
        
        WGTMLModule mod = (WGTMLModule) db.getDocumentByDocumentKey(getDocumentKey());
        if (mod == null) {
            WGDocumentKey key = new WGDocumentKey(getDocumentKey());
            mod = db.createTMLModule(key.getName(), key.getMediakey());
        }
        mod.setCode(code);
        metaData.writeToDocument(mod);
        doSaveDocument(mod);
        
        resetUpdateInformation();

    }

    protected DesignMetadata createDefaultMetadata() throws InstantiationException, IllegalAccessException {
        TMLMetadata md = (TMLMetadata) super.createDefaultMetadata();
        md.setDirectAccess(_parent.getManager().isDirectAccessDefault());
        return md;
    }



}
