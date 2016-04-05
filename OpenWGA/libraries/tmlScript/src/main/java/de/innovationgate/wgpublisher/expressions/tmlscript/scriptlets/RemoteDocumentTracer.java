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

package de.innovationgate.wgpublisher.expressions.tmlscript.scriptlets;

import java.util.Iterator;

import de.innovationgate.authoring.remotedoc.RemoteDocReference;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.wgpublisher.WGPDispatcher;

class RemoteDocumentTracer extends MasterSessionTask {
    
    private String _targetContentKey = null;
    private String _targetDBKey = null;
    private String _linkKey;
    private String _remoteConsumerDB;
    private WGLanguageChooser _languageChooser;
    
    public RemoteDocumentTracer(WGDatabase db, String linkKey, String targetDB, WGLanguageChooser chooser) {
        super(db);
        _linkKey = linkKey;
        _remoteConsumerDB = targetDB;
        _languageChooser = chooser;
    }
    
    public boolean isDocumentFound() {
        return (_targetContentKey != null);
    }
    
    public String getTargetContextPath() {
        
        if (_targetContentKey == null) {
            return null;
        }
        
        if (_targetDBKey != null) {
            return "db:" + _targetDBKey + "/docid:" + _targetContentKey;
        }
        else {
            return "docid:" + _targetContentKey;                
        }
    }

    protected void exec(WGDatabase db) throws Throwable {

        // Find the document that the link in remote source doc points to
        WGContent content = WGPDispatcher.getContentByAnyKey(_linkKey, db, _languageChooser, false);
        if (content == null) {
            return;
        }
        
        Iterator<?> refs = content.getItemValueList("remote_references").iterator();
        while (refs.hasNext()) {
            String refStr = (String) refs.next();
            try {
                RemoteDocReference ref = new RemoteDocReference(refStr);
                
                // If we find a reference that point to our remote consumer db we will return
                // the key of that document
                if (ref.getDbKey().equals(_remoteConsumerDB)) {
                    _targetDBKey = null;
                    _targetContentKey = ref.getContentKey();
                    return;
                }
            }
            catch (IllegalArgumentException e) {
                WGFactory.getLogger().error("Illegal remote doc reference on " + content.getDocumentKey() + " (DB " + db.getDbReference() + ")" ,e);
            }
            
        }
        
        // If we find no suitable reference we point to the remote source document
        _targetDBKey = db.getDbReference();
        _targetContentKey = content.getContentKey().toString();
        
    }

    public String getTargetContentKey() {
        return _targetContentKey;
    }

    public String getTargetDBKey() {
        return _targetDBKey;
    }
    
}