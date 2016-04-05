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

package de.innovationgate.wgpublisher.design.conversion;

import java.io.File;

import org.apache.commons.vfs2.FileObject;

import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.WGA;

public class PreProcessData {
    
    private App _app;
    private WGDocumentKey _documentKey;
    private DesignReference _designReference;
    private FileObject _file;

    public FileObject getFile() {
        return _file;
    }
    public void setFile(FileObject file) {
        _file = file;
    }
    public App getApp() {
        return _app;
    }
    public void setApp(App app) {
        _app = app;
    }
    public WGDocumentKey getDocumentKey() {
        return _documentKey;
    }
    public void setDocumentKey(WGDocumentKey documentKey) {
        _documentKey = documentKey;
    }
    public DesignReference getDesignReference() {
        return _designReference;
    }
    public void setDesignReference(DesignReference designRef) {
        _designReference = designRef;
    }


}
