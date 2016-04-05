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

package de.innovationgate.wgpublisher.design.fs;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.model.ScriptMetadataInfo;

public class ScriptMetadata extends DesignMetadata {

    public ScriptMetadata(WGCSSJSModule script) throws WGAPIException {
        super(script);
        _info = new ScriptMetadataInfo();
    }
     
    public ScriptMetadata() {
        super();
        _info = new ScriptMetadataInfo();
    }

    @Override
    public void writeToDocument(WGDesignDocument doc) throws WGAPIException {
        super.writeToDocument(doc);
        if (doc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            doc.setMetaData(WGTMLModule.META_CODEOFFSET, getHeaderLines());
        }
    }
    
}
