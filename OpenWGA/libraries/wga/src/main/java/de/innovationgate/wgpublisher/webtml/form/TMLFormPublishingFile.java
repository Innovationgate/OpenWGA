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

package de.innovationgate.wgpublisher.webtml.form;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wgpublisher.PublishingFile;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateCreator;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext.DocumentPCFile;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext.PCFile;

public class TMLFormPublishingFile extends PublishingFile {

    private PCFile _file;

    public TMLFormPublishingFile(WGPDispatcher wgpDispatcher, PCFile file) throws WGAPIException {
        super(wgpDispatcher, file.getName());
        _file = file;
    }

    @Override
    public String getName() {
        try {
            if (isZipped()) {
                return getZipFilePath() + " in WebTML form upload file '" + _file.getName();
            } else {
                return "WebTML form upload file " + _file.getName();
            }
        }
        catch (WGAPIException e) {
            throw new IllegalStateException(e);
        }
    }

    @Override
    public long getLastModifiedTime() throws WGAPIException {
        return _file.lastModified();
    }

    @Override
    protected InputStream getData() throws IOException {
        return _file.getData();
    }

    @Override
    protected int getUnscaledSize() throws IOException {
        return (int) _file.getSize();
    }

    @Override
    public boolean isPublishable() {
        return true;
    }

    @Override
    public String getCachingKey() {
        return null; // Never cached
    }

    @Override
    public boolean isAllowAcceptRanges() {
        return true;
    }

    @Override
    public String getSourceHint() {
        return "WebTML form upload";
    }
    
    @Override
    public boolean isAvailable() throws WGAPIException {
        return true;
    }

    @Override
    public String getETag() throws WGException {
        return String.valueOf(getLastModifiedTime());
    }
    
    @Override
    public WGDatabase getDatabase() {
        if (_file instanceof DocumentPCFile) {
            return ((DocumentPCFile) _file).getDoc().getDatabase();
        }
        else {
            return null;
        }
            
    }
    
    @Override
    public String getTextEncoding() {
        return null;
    }

}
