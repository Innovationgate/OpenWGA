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

import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.design.fs.ScriptMetadata;

public class ScriptDeployment extends DesignDeployment {

    private String _codeType;
    private transient boolean _warnedAboutDuplicate = false;
    
    /**
     * Private No-args constructor. For serialisation only
     */
    private ScriptDeployment() {
        
    }

    public ScriptDeployment(DesignSyncStatus deployments, String documentKey, FileObject codeFile, String codeType) throws IOException, WGDesignSyncException {
        super(deployments, WGDocument.TYPE_CSSJS, documentKey, codeFile);
        _codeType = codeType;
    }

    public void performUpdate(WGDatabase db) throws WGDesignSyncException, IOException, WGException, InstantiationException, IllegalAccessException {
        
        // Check if file has been deleted in the meantime
        if (!getCodeFile().exists()) {
            return;
        }
        
        
        ScriptMetadata metaData = (ScriptMetadata) readMetaData();
        String code = readCode(metaData);
        
        WGCSSJSModule mod = (WGCSSJSModule) db.getDocumentByDocumentKey(getDocumentKey());
        if (mod == null) {
            WGDocumentKey key = new WGDocumentKey(getDocumentKey());
            mod = db.createCSSJSModule(key.getName(), getCodeType());
        }
        mod.setCode(code);
        metaData.writeToDocument(mod);
        doSaveDocument(mod);
        
        resetUpdateInformation();

    }

    /**
     * @return Returns the codeType.
     */
    public String getCodeType() {
        return _codeType;
    }

    protected boolean isWarnedAboutDuplicate() {
        return _warnedAboutDuplicate;
    }

    protected void setWarnedAboutDuplicate(boolean warnedAboutDuplicate) {
        _warnedAboutDuplicate = warnedAboutDuplicate;
    }



}
