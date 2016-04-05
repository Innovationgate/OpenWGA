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
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.activation.DataSource;

import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.MimeTypeSpecificImageScaler;
import de.innovationgate.utils.WGUtils;
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
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQueryTerm;
import de.innovationgate.wgpublisher.files.derivates.TypeQueryTermProcessor;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext.PCFile;
import de.innovationgate.wgpublisher.webtml.utils.ImageScalerFactory;

/**
 * 
 */
public abstract class PublishingFile implements DataSource {

    protected final WGPDispatcher _dispatcher;
    
    protected Long _fileSize = null;

    protected boolean _zipped = false;
    protected ZipEntry _zipEntry;
    protected ZipInputStream _zipStream;

    protected boolean _streamRetrieved = false;
    
    protected int _scaleMaxHeight = -1;
    protected int _scaleMaxWidth = -1;
    protected byte[] _scaledVersion = null;

    protected String _mimeType = null;
    protected int _displayWidth = -1;
    protected int _displayHeight = -1;
    
    protected MimeTypeSpecificImageScaler _scaler = null;

    protected String _zipFilePath;

    protected String _fileName;
    protected String _dispositionFileName = null;
    
    protected Float _usedDevicePixelRatio = null;

    public PublishingFile(WGPDispatcher wgpDispatcher, String fileName) {
        _dispatcher = wgpDispatcher;
        _fileName = fileName;
        
        // If the filename contains slashes we must address a file inside a
        // zip file
        int firstSlashIndex = _fileName.indexOf("/");
        if (firstSlashIndex != -1) {
            String zipFileCandidate = _fileName.substring(0, firstSlashIndex);
            if (hasZipFileSuffix(zipFileCandidate)) {
                _zipFilePath = _fileName.substring(firstSlashIndex + 1);
                _fileName = zipFileCandidate;
                List<String> zipPathParts = WGUtils.deserializeCollection(_zipFilePath, "/");
                _dispositionFileName = zipPathParts.get(zipPathParts.size() - 1);
                _zipped = true;
            }
        }
    }
    
    public abstract long getLastModifiedTime() throws WGAPIException;

    private void retrieveZipInputStream() throws IOException {

        if (_streamRetrieved) {
            return;
        }

        InputStream in = getData();
        _zipStream = new ZipInputStream(in);
        _zipEntry = moveStreamToFile(_zipStream);
        if (_zipEntry == null) {
            try {
                _zipStream.close();
            }
            catch (IOException e) {
                _dispatcher.getCore().getLog().error("Error closing zip stream", e);
            }
            _zipStream = null;
        }
        _streamRetrieved = true;

    }

    protected abstract InputStream getData() throws IOException;

    private boolean hasZipFileSuffix(String zipFileCandidate) {

        zipFileCandidate = zipFileCandidate.toLowerCase();
        return (zipFileCandidate.endsWith(".zip") || zipFileCandidate.endsWith(".jar"));

    }



    /**
     * @return Returns the zipped.
     */
    public boolean isZipped() {
        return _zipped;
    }

    
    public long getFileSize() throws IOException {
        
        if (_fileSize == null) {
            _fileSize= determineFileSize();
        }
        return _fileSize;
        
    }
    
    
    public long determineFileSize() throws IOException {

        if (isScaled()) {
            try {
                byte[] bytes = getScaledVersion();
                return bytes.length;
            }
            catch (Exception e) {
                _dispatcher.getCore().getLog().error("Exception determining size of scaled image version. Falling back to original", e);
            }
        }
        
        if (isZipped()) {
            retrieveZipInputStream();
            if (_zipEntry != null) {
                return _zipEntry.getSize();
            }
            else {
                return -1;
            }
        }
        else {
            return getUnscaledSize();
        }

    }

    protected abstract int getUnscaledSize() throws IOException;

    public InputStream getInputStream() throws IOException {
        
        if (isScaled()) {
            try {
                byte[] scaledVersion = getScaledVersion();
                return new ByteArrayInputStream(scaledVersion);
            }
            catch (Exception e) {
                _dispatcher.getCore().getLog().error("Exception getting data of scaled image version. Falling back to original", e);
            }
        }
        
        return innerGetInputStream();
        

    }

    private InputStream innerGetInputStream() throws IOException {
        InputStream stream;
        
        if (isZipped()) {
            retrieveZipInputStream();
            stream = _zipStream;
        }
        else {
            stream = getData();
        }
        
        return stream;
    }

    private ZipEntry moveStreamToFile(ZipInputStream in) {

        try {
            ZipEntry entry;
            do {
                entry = in.getNextEntry();
            } while (entry != null && !entry.getName().equals(_zipFilePath));

            return entry;
        }
        catch (IOException e) {
            _dispatcher.getCore().getLog().error("Error reading zip file entry", e);
            return null;
        }
    }

    public abstract boolean isPublishable();



    public String getContentType() {
        if (_zipped) {
            return WGFactory.getMimetypeDeterminationService().determineByFilename(getZipFilePath());
        }
        else {
            if (_mimeType != null) {
                return _mimeType;
            }
            else {
                return WGFactory.getMimetypeDeterminationService().determineByFilename(getFileName());
            }
        }
    }

    public OutputStream getOutputStream() throws IOException {
        throw new IOException("Not supported");
    }

    public boolean isScaled() {
        return _scaler != null;
    }
    
    public abstract String getCachingKey();

    public int getScaleMaxHeight() {
        return _scaleMaxHeight;
    }

    private boolean prepareScaler() throws Exception {
        
        // Scaler already prepared?
        if (_scaler != null) {
            return false;
        }
        
        // File size below threshold?
        int scalingThreshold = (Integer) _dispatcher.getCore().getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_SERVER_SCALINGTHRESHOLD);
        long thresholdBytes = scalingThreshold * 1024 * 1024;
        long fileSize = getUnscaledSize();
        if (thresholdBytes < fileSize) {
            return false;
        }
        
        // Find out the mime type, must be an image
        if (_mimeType == null) {
            _mimeType = _dispatcher.getServletContext().getMimeType(getFileName());
        }
        if (_mimeType == null || !_mimeType.startsWith("image/")) {
            return false;
        }
        
        // File already small enough?
        if ((_scaleMaxWidth == -1 || (_displayWidth != -1 && _scaleMaxWidth >= _displayWidth)) && (_scaleMaxHeight == -1 || (_displayWidth != -1 && _scaleMaxHeight >= _displayHeight))) {
            return false;
        }
        
        // Try to allocate a scaler
        try {
             _scaler = ImageScalerFactory.createMimeTypeSpecificImageScaler(_dispatcher.getCore(), _mimeType);
             _fileSize = null;
             return true;
        }
        catch (WGNotSupportedException e) {
            // Fail silently here. This is the case when no scaler for this type is installed
            return false;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception creating scaler", e); 
        }
        
    }

    public int getScaleMaxWidth() {
        return _scaleMaxWidth;
    }

    public void setOnlineScaling(int scaleMaxWidth, int scaleMaxHeight, ClientHints clientHints) throws Exception {
        
        // Use device pixel ratio to multiply the scaling sizes
        if (clientHints.getDevicePixelRatio() != null && clientHints.getDevicePixelRatio() > 1)  {
            
            // First try with normal metrics and see if we would have scaled. If so, we need to tell the browser that we used the DPR, because we either now scale with metrics*DPR or we do not scale anymore because of the DPR.
            _scaleMaxWidth = scaleMaxWidth;
            _scaleMaxHeight = scaleMaxHeight;
            _scaler = null;
            if (prepareScaler()) {
                if (_usedDevicePixelRatio == null || clientHints.getDevicePixelRatio() > _usedDevicePixelRatio) {
                    _usedDevicePixelRatio = clientHints.getDevicePixelRatio();
                }
            }

            // Now multiply the metrics
            if (scaleMaxWidth != -1) {
                scaleMaxWidth *= clientHints.getDevicePixelRatio();
            }
            if (scaleMaxHeight != -1) {
                scaleMaxHeight *= clientHints.getDevicePixelRatio();
            }
        }
        
        _scaleMaxWidth = scaleMaxWidth;
        _scaleMaxHeight = scaleMaxHeight;
        _scaler = null;
        prepareScaler();

    }
    
    private byte[] getScaledVersion() throws IOException {
        
        try {
            if (_scaledVersion == null && _scaler != null) {
                _scaler.load(innerGetInputStream(), _mimeType);
                int maxHeight = (_scaleMaxHeight != -1 ? _scaleMaxHeight : Integer.MAX_VALUE);
                int maxWidth = (_scaleMaxWidth != -1 ? _scaleMaxWidth : Integer.MAX_VALUE);
                _scaler.shrinkToSize(maxWidth, maxHeight);
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                _scaler.writeImage(out);
                _scaledVersion = out.toByteArray();
            }
            return _scaledVersion;
        }
        catch (Throwable e) {
            _scaler = null; // Prevent usage of scaler on this file
            throw new IOException("Exception getting scaled image version", e);
        }
            
        
    }

    public abstract boolean isAllowAcceptRanges();

    public abstract String getSourceHint();
    
    public abstract String getETag() throws WGException;
    
    public abstract boolean isAvailable() throws WGAPIException;
    

    
    /**
     * @return Returns the zippedFileName.
     */
    public String getZipFilePath() {
        return _zipFilePath;
    }

    public String getFileName() {
        return _fileName;
    }
    
    protected Float getUsedDevicePixelRatio() {
        return _usedDevicePixelRatio;
    }

    /**
     * Determine if we may fallback to the original because the backend does not support derivates
     * @param derivateQuery The derivate query
     * @return true if the original should be served as fallback, false otherwise
     */
    protected boolean isFallbackToOriginalOnDerivateQuery(DerivateQuery derivateQuery) {
        String contentType = getContentType();
        
        
        // If the usage is "poster" and the original is an image (or has no content type) we may return it
        DerivateQueryTerm usage  = derivateQuery.get(DerivateQuery.QUERYTERM_USAGE);
        if (usage != null && usage.getValue().equals(WGFileDerivateMetaData.USAGE_POSTER) && (contentType == null || contentType.startsWith("image/"))) {
            return true;
        }
        
        // If the original has the same mime as request we may return it
        DerivateQueryTerm type  = derivateQuery.get(DerivateQuery.QUERYTERM_TYPE);
        if (type != null && TypeQueryTermProcessor.testMimeTypeMatch(contentType, type.getValue()) > 0) {
            return true;
        }

        // All others: Do not serve
        return false;
    }

    public String getDispositionFileName() {
        return _dispositionFileName;
    }
    
    public abstract WGDatabase getDatabase();
    
    public abstract String getTextEncoding();


}