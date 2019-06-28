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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.provider.local.LocalFile;
import org.apache.commons.vfs2.provider.zip.ZipFileSystem;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDesignResourceDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGOperationKey;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleInstantiationException;
import de.innovationgate.wga.modules.types.DesignResourceConversionModuleType;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.conversion.PreProcessData;
import de.innovationgate.wgpublisher.design.conversion.PreProcessResult;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager.ModuleFile;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;

public class DesignFileDocument extends AbstractDesignFile implements WGDocumentCore {
    
    public static final String EXTDATA_POSTPROCESSOR = "$postprocessor";

    public static class Data {
        
        private long _lastModified;
        private DesignMetadata _metadata;
        private String _code;
        private String _encoding;
        private boolean _mdFileExists;
        private String _codeFilePath;
        private String _codeFileName;
        private WGDocumentKey _documentKey;
        private Map<String, Object> _extData;
        
        
        public Data(WGDocumentKey docKey, DesignMetadata metadata, String codeFilePath, String codeFileName, String code, long lastModified, String encoding, boolean mdFileExists, Map<String,Object> extData) {
            super();
            _documentKey = docKey;
            _metadata = metadata;
            _mdFileExists = mdFileExists;
            _codeFilePath = codeFilePath;
            _codeFileName = codeFileName;
            _code = code;
            _lastModified = lastModified;
            _encoding = encoding;
            _extData = extData;
        }
        public DesignMetadata getMetadata() {
            return _metadata;
        }
        public String getCode() {
            return _code;
        }
        public long getLastModified() {
            return _lastModified;
        }
        public String getEncoding() {
            return _encoding;
        }
        public boolean isMdFileExists() {
            return _mdFileExists;
        }
        public String getCodeFilePath() {
            return _codeFilePath;
        }
        public String getCodeFileName() {
            return _codeFileName;
        }
        public WGDocumentKey getDocumentKey() {
            return _documentKey;
        }
        public Map<String, Object> getExtData() {
            return _extData;
        }
        
    }
    
    private FileSystemDesignProvider _manager;
    private WGDocument _wgdoc;
    private WGDocumentKey _docKey;
    private Data _data;
    

    protected DesignFileDocument(FileSystemDesignProvider manager, FileObject file, String name, int type, String category) throws FileSystemException, WGDesignSyncException {
        super(manager, file, type);
        _manager = manager;
        if (type == WGDocument.TYPE_TML || type == WGDocument.TYPE_CSSJS) {
            _docKey = new WGDocumentKey(type, name, category);
        }
        else {
            _docKey = new WGDocumentKey(type, name, null);
        }
    }
    

    


    protected DesignFileDocument(FileSystemDesignProvider fileSystemDesignManager, ModuleFile moduleFile) throws WGException, FileNotFoundException, IOException, InstantiationException, IllegalAccessException, ModuleDependencyException, ModuleInstantiationException {
        this(fileSystemDesignManager, moduleFile.getFile(), moduleFile.getModuleName(), moduleFile.getType(), moduleFile.getCategory());
        if (fileSystemDesignManager.isNoBackgroundChanges()) { // Load into cache if we have "no background changes" to prevent future file lookup
            getData();
        }
    }

    public DesignFileDocument(FileSystemDesignProvider fileSystemDesignProvider, Data cacheData) throws FileSystemException, WGDesignSyncException {
        super(fileSystemDesignProvider, cacheData.getCodeFilePath(), cacheData.getCodeFileName(), cacheData.getDocumentKey().getDocType());
        _manager = fileSystemDesignProvider;
        _docKey = cacheData.getDocumentKey();
        _data = cacheData;
    }




    public boolean attachFile(File file) throws WGAPIException {
        return false;
    }

    public void dispose() {
    }

    public Object evaluateExpression(String expression) throws WGExpressionException, WGBackendException {
        throw new WGExpressionException("Not supported", expression);
    }

    public Date getCreated() throws WGAPIException {
        try {
            if (getType() == WGDocument.TYPE_FILECONTAINER) {
                return getObjectCreated();
            }
            else {
                return new Date(getCodeFile().getContent().getLastModifiedTime());
            }
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving created date", e);
        }
    }

    public Object getFastAccessKey() throws WGBackendException {
        return null;
    }

    public InputStream getFileData(String strFile) throws WGAPIException {
        try {
            if (getType() != WGDocument.TYPE_FILECONTAINER) {
                return null;
            }
            FileObject file = getFileContainerFile(strFile);
            if (file == null) {
                return null;
            }
            if (file.getType().equals(FileType.FILE)) {
            	return file.getContent().getInputStream();
            } else {
            	return null;
            }
        }
        catch (Exception e) {
            throw new WGBackendException("Exception reading container file data", e);
        }
    }

    public WGFileMetaData getFileMetaData(String strFile) throws WGAPIException {
        try {
            FileObject file = getFileContainerFile(strFile);
            if (file == null) {
                return null;
            }
            
            Date lastModified = new Date(getLastModifiedTime(file));
            WGFileMetaData md = new WGFileMetaData(null, strFile.toLowerCase(), file.getContent().getSize(), lastModified, lastModified, null, null, new HashMap<String, Object>());
            return md;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving file metadata for file " + strFile + " on document " + _docKey.toString(), e);
        }
    }

    public List getFileNames() throws WGBackendException {
        try {
            if (getType() != WGDocument.TYPE_FILECONTAINER) {
                return null;
            }
            getCodeFile().refresh();
            
            Iterator<FileObject> files = getFileContainerFiles().iterator();
            List<String> fileNames = new ArrayList<String>();
            while (files.hasNext()) {
                FileObject file = files.next();
                fileNames.add(file.getName().getBaseName().toLowerCase());
            }
            return fileNames;
            
        }
        catch (Exception e) {
            throw new WGBackendException("Exception reading container file data", e);
        }
    }

    public int getFileSize(String strFile) throws WGAPIException {
        try {
            if (getType() != WGDocument.TYPE_FILECONTAINER) {
                return -1;
            }
            FileObject file = getFileContainerFile(strFile);
            if (file == null) {
                return -1;
            }
            
            return (int) file.getContent().getSize();
        }
        catch (Exception e) {
            throw new WGBackendException("Exception reading container file data", e);
        }
    }

    public List getItemNames() throws WGBackendException {
        return Collections.emptyList();
    }

    public Object getItemValue(String strName) throws WGAPIException {
        return null;
    }

    public Date getLastModified() throws WGAPIException {
        try {
            return new Date(getData().getLastModified());
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving last modified date", e);
        }
    }

    public Object getMetaData(String type) throws WGAPIException {
        
        try {
            
            if (type.equals(WGDocument.META_CREATED)) {
                return getCreated();
            }
            else if (type.equals(WGDocument.META_LASTMODIFIED)) {
                return getLastModified();
            }
            else if (type.equals(WGDocument.META_PASTAUTHORS)) {
                return Collections.EMPTY_LIST;
            }
            else if (type.equals(WGDocument.META_PASTEDITDATES)) {
                return Collections.EMPTY_LIST;
            }
            else if (type.equals(WGDocument.META_REVISION)) {
                return 1;
            }
            else if (type.equals(WGDesignDocument.META_NAME)) {
                return _docKey.getName();
            }
            else if (type.equals(WGDesignDocument.META_DESCRIPTION)) {
                return getData().getMetadata().getDescription();
            }
            else if (type.equals(WGDesignDocument.META_DESIGNREFERENCE)) {
                return new DesignReference(_manager.getDesignReference(), _docKey.toString());
            }
            
            switch (getType()) {
                
                case WGDocument.TYPE_TML:
                    
                    if (type.equals(WGTMLModule.META_CACHEABLE)) {
                        TMLMetadata metadata = (TMLMetadata) getData().getMetadata();
                        return Boolean.valueOf(metadata.isCacheable());
                    }
                    else if (type.equals(WGTMLModule.META_CODE)) {
                        return getData().getCode();
                    }
                    else if (type.equals(WGTMLModule.META_DIRECTACCESS)) {
                        TMLMetadata metadata = (TMLMetadata) getData().getMetadata();
                        return Boolean.valueOf(metadata.isDirectAccess());
                    }
                    else if (type.equals(WGTMLModule.META_PREPROCESS)) {
                        TMLMetadata metadata = (TMLMetadata) getData().getMetadata();
                        return metadata.getPreprocess();
                    }
                    else if (type.equals(WGTMLModule.META_MEDIAKEY)) {
                        return _docKey.getMediakey();
                    }
                    break;
                    
                case WGDocument.TYPE_CSSJS:
                    
                    if (type.equals(WGScriptModule.META_CODE)) {
                        return getData().getCode();
                    }
                    else if (type.equals(WGScriptModule.META_CODETYPE)) {
                        return _docKey.getMediakey();
                    }
                    break;
                    
                
                case WGDocument.TYPE_FILECONTAINER: {
                    
                    break;
                    
                }
                    
                
            }
            
            return null;
            
        }
        catch (Exception e) {
            throw new WGBackendException("Exception reading file design metadata of " + _docKey.toString(), e);
        }
        
        
    }
    
    private Data getData() throws FileNotFoundException, IOException, InstantiationException, IllegalAccessException, ModuleDependencyException, ModuleInstantiationException, WGException {

        // Look if already retrieved
        if (_data != null) {
            return _data;
        }
        
        // Load Cache
        Data cacheData = _manager.readDesignFileCache(_docKey);
        
        // Special check in "no background changes" mode. Cache existence is sufficient, no validity checks.
        if (_manager.isNoBackgroundChanges() & cacheData != null) {
           _data = cacheData;
           return cacheData;
        }
        

        // Check the cache: Last modified time must be calculated and checked against it.
        FileObject mdFile = getMetadataFile();
        long lastModified = determineLastModifiedTime(mdFile);
        if (isDesignCacheValid(cacheData, mdFile, lastModified)) {
            _data = cacheData;
            return cacheData;
        }
        
        WGOperationKey op = _manager.getDb().obtainOperationKey(WGOperationKey.OP_DESIGN_BACKEND, _docKey.toString());
        synchronized (op) {
            try {
                    op.setUsed(true);
                    
                    // Check cache once again synchronized (double checked locking)
                    cacheData = _manager.readDesignFileCache(_docKey);
                    mdFile = getMetadataFile();
                    lastModified = determineLastModifiedTime(mdFile);
                    if (isDesignCacheValid(cacheData, mdFile, lastModified)) {
                        _data = cacheData;
                        return cacheData;
                    }
                
                    // Build a new data object and put it to cache
                    DesignMetadata metadata = (DesignMetadata) readMetaData();
                    String code = readCode(metadata);
                    Map<String,Object> extData = new HashMap<String, Object>();
                    
                    code = performDesignConversionPreProcessing(code, extData);
                    
                    _data = new Data(_docKey, metadata, getCodeFilePath(), getProperCaseFileName(getCodeFile()), code, lastModified, _manager.getFileEncoding(), mdFile.exists(), extData);
                    _manager.writeDesignFileCache(_docKey, _data);
                    
                    return _data;
                
            }
            finally {
                op.setUsed(false);
            }
        }
        
        
    }





    private String getProperCaseFileName(FileObject codeFile) {

        // Try to find right case of file on case-insensitive file systems
        if (codeFile instanceof LocalFile) {
            try {
                String uri = getManager().getCore().getURLEncoder().encodePath(codeFile.getName().getURI());
                File nativeFile = new File(new URI(uri));
                return nativeFile.getCanonicalFile().getName();
            }
            catch (Exception e) {
                LOG.error("Exception determining proper name case of local file " + codeFile.getName().getPath(), e);
            }
        }
        
        return codeFile.getName().getBaseName();
        
        
    }





    private boolean isDesignCacheValid(Data cacheData, FileObject mdFile, long lastModified) throws FileSystemException {
        return cacheData != null && cacheData.getLastModified() >= lastModified && cacheData.isMdFileExists() == mdFile.exists() && cacheData.getEncoding().equals(_manager.getFileEncoding());
    }





    public String performDesignConversionPreProcessing(String code, Map<String, Object> extData) throws ModuleDependencyException, ModuleInstantiationException, WGException, FileSystemException {
        DesignResourceConversion conv = getDesignResourceConversion();
        if (conv == null) {
            return code;
        }
        
        WGA wga = WGA.get(_manager.getCore());
        PreProcessData data = new PreProcessData();
        data.setApp(wga.app(_manager.getConsumerDatabase()));
        data.setDocumentKey(_docKey);
        data.setFile(getCodeFile());
        data.setDesignReference(new DesignReference(_manager.getDesignReference(), _docKey.toString()));
        
        PreProcessResult result = conv.preProcess(wga, data, code);
        code = result.getCode();
        
        extData.putAll(result.getExtensionData());
        if (result.getPostProcessor() != null) {
            extData.put(EXTDATA_POSTPROCESSOR, result.getPostProcessor());
        }
        return code;
    }


    private DesignResourceConversion getDesignResourceConversion() throws ModuleDependencyException, ModuleInstantiationException {
        
        String key = WGDocument.doctypeNumberToName(getType()) + "/" + _docKey.getMediakey() + "/" + getSuffix();
        ModuleDefinition modDef = _manager.getCore().getModuleRegistry().getModuleDefinitionByKey(DesignResourceConversionModuleType.class, key);
        if (modDef == null) {
            return null;
        }
        modDef.testDependencies();
        return (DesignResourceConversion) _manager.getCore().getModuleRegistry().instantiate(modDef);
        
    }





    private long determineLastModifiedTime(FileObject mdFile) throws FileSystemException, WGDesignSyncException, InstantiationException, IllegalAccessException, IOException {
        long mdLastModifiedTime = Long.MIN_VALUE;
        if (mdFile.exists()) {
            mdLastModifiedTime = getLastModifiedTime(mdFile);
        }
        long codeLastModifiedTime = getLastModifiedTime(getCodeFile());
        
        long lastModified = Math.max(mdLastModifiedTime, codeLastModifiedTime);
        if (getType() == WGDocument.TYPE_FILECONTAINER && _manager.isStrictFCDateDetermination()) {
            lastModified = determineFileContainerLastModified();
        }
        return lastModified;
    }

    private long getLastModifiedTime(FileObject file) throws FileSystemException {
        
        if (file.getFileSystem() instanceof ZipFileSystem) {
            return file.getFileSystem().getParentLayer().getContent().getLastModifiedTime();
        }
        
        return file.getContent().getLastModifiedTime();
        
        
    }





    private long determineFileContainerLastModified() throws WGDesignSyncException, InstantiationException, IllegalAccessException, IOException {

        // Throw all dates that may symbolize changes to a list
        List<Long> dates = new ArrayList<Long>();
        
        // Directory date
        dates.add(getLastModifiedTime(getCodeFile()));
        
        // Metadata date
        FileObject mdFile = getMetadataFile();
        if (mdFile.exists()) {
            dates.add(mdFile.getContent().getLastModifiedTime());
        }
        
        // Container File dates
        List<FileObject> files = getFileContainerFiles();
        FileObject file;
        
        for (int i = 0; i < files.size(); i++) {
            file = (FileObject) files.get(i);
            if (isExcludedFileContainerFile(file)) {
                continue;
            }

            dates.add(file.getContent().getLastModifiedTime());
        }
        
        // Sort and take the highest
        Collections.sort(dates);
        return dates.get(dates.size() - 1);
        
        
    }





    public Object getNativeObject() throws WGBackendException {
        try {
            return getCodeFile();
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving native object of " + _docKey.toString(), e);
        }
    }

    public String getOriginDatabase() {
        return getManager().getDB().getDbReference();
    }

    public WGDocumentCore getRelation(String name) throws WGAPIException {
        return null;
    }

    public List getRelationNames() throws WGAPIException {
        return Collections.emptyList();
    }

    public boolean hasItem(String strName) throws WGBackendException {
        return false;
    }

    public boolean isDataCacheable() {
        return false;
    }

    public boolean isDeleted() throws WGAPIException {
        try {
            if (_manager.isNoBackgroundChanges()) {
                Data cacheData = _manager.readDesignFileCache(_docKey);
                if (cacheData != null) {
                    return false;
                }
            }
            
            return !getCodeFile().exists();
        }
        catch (Exception e) {
            throw new WGBackendException("Exception determining file deletion state", e);
        }
    }

    public boolean isSaved() throws WGAPIException {
        return true;
    }

    public boolean isTemporary() throws WGAPIException {
        return !_manager.isNotifying();
    }

    public WGDatabaseRevision remove() throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public boolean removeFile(String name) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public boolean removeItem(String Name) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public WGDocumentCore removeRelation(String name) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public void renameFile(String oldFileName, String newFileName) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public WGDatabaseRevision save(Date lastModified) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public boolean setItemValue(String strName, Object value) throws WGAPIException {
        return false;
    }

    public boolean setMetaData(String name, Object value) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public WGDocumentCore setRelation(String name, WGDocumentCore target) throws WGAPIException {
        throw new WGNotSupportedException("Not Supported");
    }

    public void setWGDocument(WGDocument doc) {
        _wgdoc = doc;
    }

    @Override
    protected FileSystemDesignManager getManager() {
        return _manager;
    }

    public boolean hasFileMetadata() throws WGAPIException {
        return true;
    }





    public boolean hasFile(String file) throws WGBackendException {
        try {
            return (getFileContainerFile(file) != null);
        }
        catch (Exception e) {
            throw new WGBackendException("Exception checking container file existence", e);
        }
    }





    @Override
    protected FileObject getFileContainerFile(String name) throws FileSystemException, WGDesignSyncException {
        FileObject file = super.getFileContainerFile(name);
        if (file == null) {
           getCodeFile().refresh();
        }
        return super.getFileContainerFile(name);
    }





    public WGRelationData getRelationData(String name) throws WGAPIException {
        throw new WGNotSupportedException("This operation is not supported by this database type");
    }





    public WGDocumentCore setRelation(WGRelationData relAddress) throws WGAPIException {
        throw new WGNotSupportedException("This operation is not supported by this database type");
    }





    public Object getExtensionData(String strName) throws WGAPIException {
        
        try {
            if (strName.equals(WGDocument.EXTDATA_META_PREFIX + WGTMLModule.META_DESCRIPTION)) {
                return getData().getMetadata().getDescription();
            }
            else if (strName.equals(WGDocument.EXTDATA_META_PREFIX + WGTMLModule.META_CODEOFFSET)) {
                return getData().getMetadata().getHeaderLines();
            }
            else if (strName.equals(WGDocument.EXTDATA_META_PREFIX + WGDesignResourceDocument.META_SOURCEFILENAME)) {
                return getData().getCodeFileName();
            }
            
            
            return getData().getExtData().get(strName);
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving attribute " + strName, e);
        }
    }





    public List getExtensionDataNames() throws WGAPIException {
        
        List<String> names = new ArrayList<String>();
        names.add(WGDocument.EXTDATA_META_PREFIX + WGTMLModule.META_DESCRIPTION);
        names.add(WGDocument.EXTDATA_META_PREFIX + WGTMLModule.META_CODEOFFSET);
        names.add(WGDocument.EXTDATA_META_PREFIX + WGTMLModule.META_SOURCEFILENAME);
        try {
            names.addAll(getData().getExtData().keySet());
        }
        catch (WGAPIException e) {
            throw e;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving extension data names", e);
        }
        return names;
        
    }

    public void removeExtensionData(String strName) throws WGAPIException {
        throw new WGNotSupportedException("This operation is not supported by this database type");        
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        throw new WGNotSupportedException("This operation is not supported by this database type");
    }

    public List<String> getRelationNamesOfGroup(String group, WGColumnSet order) throws WGBackendException {
        return Collections.emptyList();
    }

    @Override
    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String strFile) throws WGAPIException {
        return null;
    }

    @Override
    public List<WGFileDerivateMetaData> getFileDerivates(String strFile) throws WGAPIException {
        return null;
    }

    @Override
    public void markFileMetaDataModified(WGFileMetaData md) throws WGAPIException {
    }

    @Override
    public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String, Object> customMdFields) throws WGAPIException,
            WGNotSupportedException {
        throw new WGNotSupportedException("This operation is not supported by this database type");
    }

    @Override
    public void removeFileDerivate(String id) throws WGAPIException {
        throw new WGNotSupportedException("This operation is not supported by this database type");
    }

    @Override
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException {
        return null;
    }

    @Override
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException, WGNotSupportedException {
        // TODO Auto-generated method stub
        
    }

    @Override
    public InputStream getFileDerivateData(String id) throws WGAPIException {
        return null;
    }
    
    @Override
    public Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException {
        return Collections.<WGUpdateLog>emptyList().iterator();
    }

}
