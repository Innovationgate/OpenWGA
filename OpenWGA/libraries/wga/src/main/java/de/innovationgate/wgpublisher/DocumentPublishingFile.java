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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.activation.DataSource;

import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.MimeTypeSpecificImageScaler;
import de.innovationgate.utils.io.IOBackendException;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wgpublisher.files.derivates.WGFailedDerivateQueryException;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.files.derivates.WGInvalidDerivateQueryException;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext.PCFile;
import de.innovationgate.wgpublisher.webtml.utils.ImageScalerFactory;

public class DocumentPublishingFile extends PublishingFile {

    private WGDocument _container;

    private boolean _publishable = true;

    private boolean _allowAcceptRanges;
    
    private String _sourceHint;

    private long _lastModifiedTime;
    
    private String _eTag;

    public DocumentPublishingFile(WGPDispatcher wgpDispatcher, WGDocument container, String fileName) throws WGAPIException {
        super(wgpDispatcher, fileName);
        _container = container;
        _allowAcceptRanges = container.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1;
        _sourceHint = container.getDatabase().getDbReference();
        _lastModifiedTime = _container.getFileLastModified(_fileName).getTime();
        if (container.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            WGFileMetaData md = container.getFileMetaData(_fileName);
            if (md != null) {
                _mimeType = md.getMimeType();
                _displayWidth = md.getDisplayWidth();
                _displayHeight = md.getDisplayHeight();
                if (md.getSha512Checksum() != null) {
                    _eTag = md.getSha512Checksum();
                }
            }
        }
        
        if (_eTag == null) {
            _eTag = String.valueOf(_lastModifiedTime);
        }
        
        // We do not want to publish files from the system file container.
        // They are the WGA pendant of J2EE's "WEB-INF" directory which is
        // also unpublished.
        if (container instanceof WGFileContainer) {
            WGFileContainer fc = (WGFileContainer) container;
            if (isSystemFileContainer(fc)) {
                _publishable = false;
            }
        }
    }

    public WGFileMetaData getFileMetaData() throws WGAPIException{
    	if (_container.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5)
    		return _container.getFileMetaData(_fileName);
    	else return null;
    }
    
    private boolean isSystemFileContainer(WGFileContainer fc) throws WGAPIException {
        if (fc.getName().equals("system") || fc.getName().startsWith("system:")) {
            return true;
        }
        
        if (fc.getName().equals("overlay:system") || fc.getName().startsWith("overlay:system:")) {
            return true;
        }
        
        return false;
    }
    
    @Override
    public long getLastModifiedTime() throws WGAPIException {
        return _lastModifiedTime;
    }

    @Override
    protected InputStream getData() throws IOException {
        try {
            return _container.getFileData(_fileName);
        }
        catch (WGAPIException e) {
            throw new IOException("Exception retrieving publishing file data", e);
        }
    }

    @Override
    protected int getUnscaledSize() throws IOException {
        try {
            return _container.getFileSize(_fileName);
        }
        catch (WGAPIException e) {
            throw new IOException("Exception retrieving document file size", e);
        }
    }

    @Override
    public boolean isPublishable() {
        return _publishable;
    }

    @Override
    public String getName() {
        if (isZipped()) {
            return getZipFilePath() + " in archive '" + getFileName() + "' on document " + _container.getDocumentKey();
        } else {
            return getFileName() + " on document " + _container.getDocumentKey();
        }
    }

    @Override
    public String getCachingKey() {
        
        StringBuffer key = new StringBuffer();
        key.append(_container.getDocumentKey());
        key.append("/");
        key.append(getFileName());
        
        if (getZipFilePath() != null) {
            key.append("/").append(getZipFilePath());
        }
        
        if (_scaler != null) {
            key.append("//");
            key.append(_scaleMaxWidth + "x" + _scaleMaxHeight);
        }
        
        return key.toString();
        
    }

    public boolean isAllowAcceptRanges() {
        return _allowAcceptRanges;
    }

    public String getSourceHint() {
        return _sourceHint;
    }

    @Override
    public boolean isAvailable() throws WGAPIException {
        return _container.hasFile(_fileName);
    }
    
    @Override
    public String getTextEncoding() {
        if (_container instanceof WGFileContainer) {
            return (String) _container.getDatabase().getAttribute(WGACore.DBATTRIB_DESIGN_ENCODING);
        }
        else {
            boolean useDesignEncodingForContentFiles = _container.getDatabase().getBooleanAttribute(WGACore.DBATTRIB_PUBLISH_CONTENTFILES_WITH_DESIGNENCODING, WGACore.DBATTRIBDEFAULT_PUBLISH_CONTENTFILES_WITH_DESIGNENCODING);
            if (useDesignEncodingForContentFiles) {
                return (String) _container.getDatabase().getAttribute(WGACore.DBATTRIB_DESIGN_ENCODING);
            }
            else {
                return null;
            }
            
        }
    }
    
    public WGFileDerivateMetaData queryDerivate(DerivateQuery derivateQuery, ClientHints clientHints) throws WGException {

        
        // Situations where no derivate can be available for a document publishing file
        boolean fallback = false;
        if (_container instanceof WGFileContainer) { // File is from design
            fallback = true;
        }
        else if (_container.getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5 || 
                (_container.getDatabase().getContentStoreVersion() == WGDatabase.CSVERSION_WGA5 && _container.getDatabase().getContentStorePatchLevel() < 4)) { // Too low CS version
            fallback = true;
        }
        else { // Essential metadata is missing which prevents derivate creation
            WGFileMetaData md = _container.getFileMetaData(_fileName);
            if (md == null || md.getSha512Checksum() == null || md.getMimeType() == null) {
                fallback = true;
            }
        }
        
        if (fallback) {
            if (isFallbackToOriginalOnDerivateQuery(derivateQuery)) {
                return null;
            }
            else {
                throw new WGNotSupportedException("File derivates are not supported for this file");
            }
        }
        
        if (_scaleMaxHeight != -1 || _scaleMaxWidth != -1) {
            throw new WGInvalidDerivateQueryException("Cannot query derivate any more after online scaling has been specified");
        }

        
        WGFileAnnotations md;
        
        // With a set device pixel ratio > 1 we do additional queries for the given ratio 
        
        if (clientHints.getDevicePixelRatio() != null && derivateQuery.getDevicePixelRatioOverride() == null) {
            float testingDevicePixelRatio = clientHints.getDevicePixelRatio();
            while (true) {
                md = _dispatcher.getCore().getFileDerivateManager().queryDerivate(_container, _fileName, derivateQuery, new ClientHints(testingDevicePixelRatio, null), true); 
                if (md != null)  {
                    _usedDevicePixelRatio = testingDevicePixelRatio;
                    break;
                }
                if (testingDevicePixelRatio == 1) {
                    break;
                }
                else if (testingDevicePixelRatio % 2 == 0) {
                    testingDevicePixelRatio = testingDevicePixelRatio / 2;
                }
                else {
                    testingDevicePixelRatio = 1f;
                }
            }
        }
        else {
            md = _dispatcher.getCore().getFileDerivateManager().queryDerivate(_container, _fileName, derivateQuery, clientHints, true);
        }
        if (md != null) {
            if (md instanceof WGFileDerivateMetaData) {
                return (WGFileDerivateMetaData) md;
            }
            else {
                return null;
            }
        }
        else {
            throw new WGFailedDerivateQueryException();        
        }
    }
    
    public WGDocument getContainer() {
        return _container;
    }

    public String getETag() {
        return _eTag;
    }
    
    @Override
    public WGDatabase getDatabase() {
        return _container.getDatabase();
    }

}