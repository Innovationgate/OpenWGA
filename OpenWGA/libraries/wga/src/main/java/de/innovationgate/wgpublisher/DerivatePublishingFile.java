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

package de.innovationgate.wgpublisher;

import java.io.IOException;
import java.io.InputStream;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGNotSupportedException;

public class DerivatePublishingFile extends PublishingFile {

    private boolean _publishable = true;

    private WGFileDerivateMetaData _derivateMd = null;
    
    private String _sourceHint;

    private long _lastModifiedTime;
    
    private String _eTag;

    private WGDocument _container;

    public static String getDerivateIdFromFileName(String fileName) {
        int pointPos = fileName.indexOf(".");
        if (pointPos != -1) {
            return fileName.substring(0, pointPos);
        }
        else {
            return fileName;
        }
    }

    public DerivatePublishingFile(WGPDispatcher wgpDispatcher, WGDocument container, WGFileDerivateMetaData derivateMd) throws WGAPIException {
        super(wgpDispatcher, determineFileName(derivateMd));
        _container = container;
        _sourceHint = container.getDatabase().getDbReference();
        _derivateMd = derivateMd;
        if (_derivateMd != null) {
            _lastModifiedTime = _derivateMd.getLastModified().getTime();
            _mimeType = _derivateMd.getMimeType();
            _displayWidth = _derivateMd.getDisplayWidth();
            _displayHeight = _derivateMd.getDisplayHeight();
            _eTag = _derivateMd.getSha512Checksum();
        }
    }
    
    private static String determineFileName(WGFileDerivateMetaData derivateMd) {

        String fileName = derivateMd.getId();
        String derivateSuffix = WGFactory.getMimetypeDeterminationService().determineSuffixByMimeType(derivateMd.getMimeType());
        if (derivateSuffix != null) {
            return fileName += "." + derivateSuffix;
        }
        else {
            return fileName;
        }
        
        
    }

    @Override
    public long getLastModifiedTime() throws WGAPIException {
        return _lastModifiedTime;
    }

    @Override
    protected InputStream getData() throws IOException {
        try {
            return _container.getFileDerivateData(_derivateMd.getId());
        }
        catch (WGAPIException e) {
            throw new IOException("Exception retrieving publishing file data", e);
        }
    }

    @Override
    protected int getUnscaledSize() throws IOException {
        return (int) _derivateMd.getSize();
    }

    @Override
    public boolean isPublishable() {
        return _publishable;
    }

    @Override
    public String getName() {
        return getFileName();
    }

    @Override
    public String getCachingKey() {
        
        StringBuffer key = new StringBuffer();
        key.append(_derivateMd.getId());
        
        if (_scaler != null) {
            key.append("//");
            key.append(_scaleMaxWidth + "x" + _scaleMaxHeight);
        }
        
        return key.toString();
        
    }

    public boolean isAllowAcceptRanges() {
        return true;
    }

    public String getSourceHint() {
        return _sourceHint;
    }

    @Override
    public boolean isAvailable() throws WGAPIException {
        return _derivateMd != null;
    }
    

    
    @Override
    public String getContentType() {
        if (_derivateMd != null) {
            return _mimeType;
        }
        else {
            return super.getContentType();
        }
    }

    public String getETag() {
        return _eTag;
    }

    @Override
    public WGDatabase getDatabase() {
        return _container.getDatabase();
    }
    
    @Override
    public String getTextEncoding() {
        return null;
    }

}