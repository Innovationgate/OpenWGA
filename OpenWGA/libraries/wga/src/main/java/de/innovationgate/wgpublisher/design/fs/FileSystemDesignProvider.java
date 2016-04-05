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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.io.output.NullOutputStream;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.FileUtil;
import org.apache.log4j.Logger;

import de.innovationgate.utils.MD5HashingInputStream;
import de.innovationgate.utils.MD5HashingOutputStream;
import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGDocumentDoesNotExistException;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.model.WGADesignConfigurationModel;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignProviderCoreWrapper;
import de.innovationgate.wgpublisher.design.OverlayData;
import de.innovationgate.wgpublisher.design.OverlayData.ResourceData;
import de.innovationgate.wgpublisher.design.OverlayDesignProvider;
import de.innovationgate.wgpublisher.design.OverlayStatus;
import de.innovationgate.wgpublisher.design.OverlayStatus.ChangeType;
import de.innovationgate.wgpublisher.design.OverlayStatus.ChangedDocument;
import de.innovationgate.wgpublisher.design.WGADesignProvider;
import de.innovationgate.wgpublisher.design.db.PluginDesignProvider;
import de.innovationgate.wgpublisher.design.fs.DesignFileDocument.Data;
import de.innovationgate.wgpublisher.design.sync.DesignFileValidator;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;

public class FileSystemDesignProvider extends FileSystemDesignManager implements WGADesignProvider {

    private boolean _strictFCDateDetermination;

    private DesignReference _designReference;

    private String _variantSuffix;

    private boolean _notifying = false;
    
    protected boolean _noBackgroundChanges = false;

    public FileSystemDesignProvider(DesignReference ref, WGACore core, WGDatabase db, String path, Map<String, String> options) throws WGDesignSyncException, IOException, WGAPIException,
            InstantiationException, IllegalAccessException, InvalidCSConfigVersionException {
        super(core, db, path, options);
        _designReference = ref;
        _name = "Design Directory " + getBaseFolder().getURL().toString();
        _strictFCDateDetermination = true;
        _lookupVariants = WGUtils.getBooleanMapValue(options, OPTION_DESIGNVARIANTS, false);
        _variantSuffix = "." + getDB().getDbReference();
        
        // In case of the base folder being a file (an archived design) we can set notifying to true, as there will be no updates. This enables the WGAPI to cache designs.
        _notifying  = getFsResources().getBaseFolder().getType().equals(FileType.FILE);
        if (options.containsKey(OPTION_NO_BACKGROUND_CHANGES)) {
            _noBackgroundChanges = WGUtils.toBoolean(options.get(OPTION_NO_BACKGROUND_CHANGES), false);
        }
        else {
            _noBackgroundChanges = (Boolean) WGA.get(core).server().getServerOption(WGAConfiguration.SERVEROPTION_CACHE_DESIGN_NO_BACKGROUND_CHANGES);
        }

    }

    private String _name;

    public static final String OPTION_NO_BACKGROUND_CHANGES = "noBackgroundChanges";

    public static final String OPTION_SYNC = "sync";
    
    public boolean providesType(int type) {
        return _syncedDoctypes.contains(new Integer(type));
    }

    public void addDesignChangeListener(WGDesignChangeListener changeListener) {
    }

    public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAuthorisationException, WGCreationException {

        try {
            switch (type) {

                case WGDocument.TYPE_TML: {
                    FileObject newTml = getTmlFolder().resolveFile(mediaKey.toLowerCase() + "/" + name.replace(":", "/") + ".tml");
                    if (newTml.exists()) {
                        throw new WGCreationException("Document already exists: " + (new WGDocumentKey(type, name, mediaKey)).toString());
                    }
                    newTml.createFile();
                    break;
                }

                case WGDocument.TYPE_CSSJS: {
                    String suffix = DesignDirectory.getScriptInformation(mediaKey).getSuffix();
                    FileObject newScript = getScriptTypeFolder(mediaKey).resolveFile(mediaKey.toLowerCase() + "/" + name.replace(":", "/") + suffix);
                    if (newScript.exists()) {
                        throw new WGCreationException("Document already exists: " + (new WGDocumentKey(type, name, mediaKey)).toString());
                    }
                    newScript.createFile();
                    break;
                }

                case WGDocument.TYPE_FILECONTAINER: {
                    FileObject newFC = getFilesFolder().resolveFile(name.replace(":", "/"));
                    if (newFC.exists()) {
                        throw new WGCreationException("Document already exists: " + (new WGDocumentKey(type, name, mediaKey)).toString());
                    }
                    newFC.createFolder();
                    break;
                }

            }

            clearCache();
            return getDesignObject(type, name, mediaKey);
        }
        catch (Exception e) {
            throw new WGCreationException("Exception creating design document " + (new WGDocumentKey(type, name, mediaKey)).toString(), e);
        }

    }

    public void dispose() {
        close();
    }

    public WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGBackendException {

        try {
            
            WGDocumentKey docKey = new WGDocumentKey(type, name, strMediaKey);
            
            // Check the cache if we have disabled background changes
            if (isNoBackgroundChanges()) {
                DesignFileDocument.Data cacheData = readDesignFileCache(docKey);
                if (cacheData != null) {
                    return wrapVariantCore(new DesignFileDocument(this, cacheData));
                }
            }
            
            // Find the design file
            switch (type) {

                case WGDocument.TYPE_TML: {
                    ModuleFile file = getTMLModuleFile(name, strMediaKey);
                    if (file == null || !file.getFile().exists()) {
                        refreshIfUpdatable(getFsResources().getTmlFolder());
                        file = getTMLModuleFile(name, strMediaKey);
                    }

                    if (file != null) {
                        return wrapVariantCore(new DesignFileDocument(this, file));
                    }
                    break;
                }

                case WGDocument.TYPE_CSSJS: {
                    ModuleFile file = getScriptModuleFile(name, strMediaKey);
                    if (file == null || !file.getFile().exists()) {
                        refreshIfUpdatable(getFsResources().getScriptFolder());
                        file = getScriptModuleFile(name, strMediaKey);
                    }

                    if (file != null) {
                        return wrapVariantCore(new DesignFileDocument(this, file));
                    }
                    break;
                }

                case WGDocument.TYPE_FILECONTAINER: {
                    ModuleFile file = getFileContainerFile(name);
                    if (file == null || !file.getFile().exists()) {
                        refreshIfUpdatable(getFsResources().getFilesFolder());
                        file = getFileContainerFile(name);
                    }

                    if (file != null) {
                        return wrapVariantCore(new DesignFileDocument(this, file));
                    }
                    break;
                }
            }

            if (isNoBackgroundChanges()) {
                writeDesignFileCache(docKey, null);
            }
            return null;
        }
        catch (WGDocumentDoesNotExistException e) {
            return null;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving design document '" + name + "' of type " + WGDocument.doctypeNumberToName(type), e);
        }

    }

    protected Data readDesignFileCache(WGDocumentKey docKey) throws WGDocumentDoesNotExistException {
        Cache cache = getCore().getDesignFileCache();
        String cacheKey = createDesignCacheKey(docKey);
        Data cacheData = null;
        try {
             Object cacheObject = cache.read(cacheKey);
             if (cacheObject instanceof NullPlaceHolder) {
                 throw new WGDocumentDoesNotExistException();
             }
             else {
                 return (Data) cacheObject;
             }
        }
        catch (CacheException e) {
            getLog().error("Exception checking design cache", e);
            return null;
        }
    }
    
    protected void writeDesignFileCache(WGDocumentKey docKey, DesignFileDocument.Data data) {
        Cache cache = getCore().getDesignFileCache();
        String cacheKey = createDesignCacheKey(docKey);
        try {
            if (data == null) {
                cache.write(cacheKey, new NullPlaceHolder(), getDesignReference().toString());
            }
            else {
                cache.write(cacheKey, data, getDesignReference().toString());
            }
        }
        catch (CacheException e) {
            getLog().error("Exception writing design cache", e);
        }
    }
    
    @Override
    public void clearCache() throws WGException {
        try {
            Cache cache = getCore().getDesignFileCache();
            cache.flushGroup(getDesignReference().toString());
        }
        catch (CacheException e) {
            throw new WGException("Exception clearing design file cache for " + getDesignReference().toString(), e);
        }
    }

    protected String createDesignCacheKey(WGDocumentKey docKey) {
        StringBuffer cacheKey = new StringBuffer();
        
        // We need to include the reference of the current provider here bc. the dockey may not be qualification enough
        // (for example when it reflects the path inside a ZIP file)
        cacheKey.append(getDesignReference());
        
        cacheKey.append("/");
        cacheKey.append(docKey.toString());
        if (_lookupVariants) {
            cacheKey.append("//").append(_variantSuffix);
        }
        return cacheKey.toString();
    }

    public WGDocumentCore wrapVariantCore(WGDocumentCore core) throws WGAPIException {

        if (_lookupVariants) {
            String name = (String) core.getMetaData(WGDesignDocument.META_NAME);
            if (name.endsWith(_variantSuffix)) {
                return new DesignProviderCoreWrapper(core, this, true, false);
            }
            else {
                return core;
            }
        }
        else {
            return core;
        }

    }

    @Override
    protected ModuleFile getFileContainerFile(String name) throws FileSystemException, WGDesignSyncException {

        if (_lookupVariants) {
            ModuleFile file = super.getFileContainerFile(name + _variantSuffix);
            if (file != null) {
                return file;
            }
        }

        return super.getFileContainerFile(name);
    }

    @Override
    protected ModuleFile getScriptModuleFile(String name, String codetype) throws FileSystemException, WGDesignSyncException {

        if (_lookupVariants) {
            ModuleFile file = super.getScriptModuleFile(name + _variantSuffix, codetype);
            if (file != null) {
                return file;
            }
        }

        return super.getScriptModuleFile(name, codetype);
    }

    @Override
    protected ModuleFile getTMLModuleFile(String name, String strMediaKey) throws FileSystemException, WGDesignSyncException {

        if (_lookupVariants) {
            ModuleFile file = super.getTMLModuleFile(name + _variantSuffix, strMediaKey);
            if (file != null) {
                return file;
            }
        }
        return super.getTMLModuleFile(name, strMediaKey);
    }

    public List<WGDocumentCore> getDesignObjects(int type) throws WGBackendException {

        try {
            switch (type) {

                case WGDocument.TYPE_TML: {
                    refreshIfUpdatable(getFsResources().getTmlFolder());
                    List<ModuleFile> files = getTMLModuleFiles();
                    return wrapModuleFiles(files);
                }

                case WGDocument.TYPE_CSSJS: {
                    refreshIfUpdatable(getFsResources().getScriptFolder());
                    List<ModuleFile> files = getScriptModuleFiles();
                    return wrapModuleFiles(files);
                }

                case WGDocument.TYPE_FILECONTAINER: {
                    refreshIfUpdatable(getFsResources().getFilesFolder());
                    List<ModuleFile> files = getFileContainerFiles();
                    return wrapFileContainerFiles(files);
                }
            }

            return null;
        }
        catch (Exception e) {
            throw new WGBackendException("Exception retrieving design document list of type " + WGDocument.doctypeNumberToName(type), e);
        }

    }

    private void refreshIfUpdatable(FileObject tmlFolder) throws FileSystemException {
        if (getFsResources().isUpdateableFileSystem()) {
            tmlFolder.refresh();
        }
    }

    public String getName() {
        return _name;
    }

    public boolean isNotifying() {
        return _notifying;
    }

    public boolean isProviderCore(WGDocumentCore core) {
        return (core instanceof DesignFileDocument);
    }

    public void removeDesignChangeListener(WGDesignChangeListener changeListener) {
    }

    protected List<WGDocumentCore> wrapFileContainerFiles(List<ModuleFile> files) {

        List<WGDocumentCore> list = new ArrayList<WGDocumentCore>();
        Set<String> addedNames = new HashSet<String>();
        for (ModuleFile file : files) {
            try {
                WGDocumentCore core = wrapVariantCore(new DesignFileDocument(this, file));

                // Filter out base versions of variants
                if (_lookupVariants) {
                    String coreName = (String) core.getMetaData(WGDesignDocument.META_NAME);

                    // If we try to add a doc that already has been added and is
                    // NO variant, we know that this is the base version which
                    // was
                    // overwritten by a variant. So we skip it.
                    if (addedNames.contains(coreName) && !Boolean.TRUE.equals(core.getMetaData(WGDesignDocument.META_VARIANT))) {
                        continue;
                    }
                    else {
                        addedNames.add(coreName);
                    }
                }

                list.add(core);
            }
            catch (Exception e) {
                getLog().error("Exception wrapping file container file as WGAPI document: " + file.getFile().getName().getPath(), e);
            }

        }
        return list;

    }

    protected List<WGDocumentCore> wrapModuleFiles(List<ModuleFile> files) {

        Iterator<ModuleFile> it = files.iterator();
        Set<String> addedNames = new HashSet<String>();
        List<WGDocumentCore> list = new ArrayList<WGDocumentCore>();
        while (it.hasNext()) {
            ModuleFile moduleFile = (ModuleFile) it.next();
            try {
                WGDocumentCore core = wrapVariantCore(new DesignFileDocument(this, moduleFile));

                // Filter out base versions of variants
                if (_lookupVariants) {
                    String coreName = (String) core.getMetaData(WGDesignDocument.META_NAME);

                    // If we try to add a doc that already has been added and is
                    // NO variant, we know that this is the base version which
                    // was
                    // overwritten by a variant. So we skip it.
                    if (addedNames.contains(coreName) && !Boolean.TRUE.equals(core.getMetaData(WGDesignDocument.META_VARIANT))) {
                        continue;
                    }
                    else {
                        addedNames.add(coreName);
                    }
                }

                list.add(core);
            }
            catch (Exception e) {
                getLog().error("Exception wrapping module file as WGAPI document: " + moduleFile.getFile().getName().getPath(), e);
            }

        }
        return list;

    }

    protected boolean isStrictFCDateDetermination() {
        return _strictFCDateDetermination;
    }

    public WGDatabase getConsumerDatabase() {
        return getDB();
    }

    public boolean isLookupVariants() {
        return _lookupVariants;
    }

    public void closeSession() {
    }

    public void openSession(WGSessionContext context) {

        if (_lookupVariants && getFsResources().isUpdateableFileSystem()) {
            try {
                getFsResources().refreshAll();
            }
            catch (FileSystemException e) {
                // We dont want a failure of cache refreshing kill the whole
                // session, so we log it instead
                Logger.getLogger("wga.design").error("Exception refreshing file system design cache", e);
            }
        }

        try {
            updateConfiguration();
        }
        catch (Exception e) {
            Logger.getLogger("wga.design").error("Exception updating file system design configuration", e);
        }

    }

    public int designHashCode() {
        return super.designHashCode();
    }

    public DesignReference getDesignReference() {
        return _designReference;
    }

    public static OverlayStatus determineOverlayStatus(FileSystemDesignProvider sourceDesignProvider, PluginID baseId, FileObject targetDirectory, String targetEncoding, Logger log, DesignFileValidator validator) throws Exception {
        
        OverlayStatus status = new OverlayStatus();
        
        // Copy an overlay flag file to the system file container
        FileObject targetFCFolder = targetDirectory.resolveFile(DesignDirectory.FOLDERNAME_FILES);
        FileObject systemFC = targetFCFolder.resolveFile("system");
        if (!systemFC.exists()) {
            systemFC.createFolder();
        }
        
        // Import overlay data, if available
        FileObject overlayDataFile = systemFC.resolveFile(OverlayDesignProvider.OVERLAY_DATA_FILE);
        if (overlayDataFile.exists()) {
            try {
                InputStream in = new BufferedInputStream(overlayDataFile.getContent().getInputStream());
                status.setOverlayData(OverlayData.read(in));
                in.close();
            }
            catch (Exception e) {
                log.error("Exception reading overlay status. Creating new status file", e);
            }
            
            if (status.getOverlayData() != null && !status.getOverlayData().getBasepluginName().equals(baseId.getUniqueName())) {
                throw new WGDesignSyncException("The overlay folder '" + targetDirectory.getName().getPath() + "' is used with plugin '" + status.getOverlayData().getBasepluginName() 
                        + "' not '" + baseId.getUniqueName() + "'. Overlay status determination was canceled.");
            }
        }
        
        Version providerVersion = baseId.getVersion();
        if (status.getOverlayData() == null) {
            OverlayData overlayData = new OverlayData();
            overlayData.setBasepluginName(baseId.getUniqueName());
            status.setOverlayData(overlayData);
            status.setNewOverlay(true);
            overlayData.setInitialBasepluginVersion(providerVersion.toString());
        }
        
        // Test for version compatibility between base design and overlay
        
        status.setCurrentBaseVersion(providerVersion);
        if (status.getOverlayData().getBasepluginVersion() != null) {
            Version baseVersion = new Version(status.getOverlayData().getBasepluginVersion());
            
            // Base design version is different than the compliance version of the overlay. Look if it higher (=upgrade) or lower (=error)
            if (!providerVersion.equals(baseVersion) || providerVersion.getBuildVersion() != baseVersion.getBuildVersion()) {
                if (providerVersion.compareTo(baseVersion) >= 0 || (providerVersion.equals(baseVersion) && providerVersion.getBuildVersion() > baseVersion.getBuildVersion())) {
                    status.setUpdatedBaseDesign(true);
                }
                else if (providerVersion.compareTo(baseVersion) < 0) {
                    throw new WGDesignSyncException("The used base design version (" + providerVersion.toString() + ") is lower than the compliant version for the overlay (" + status.getOverlayData().getBasepluginVersion() + ").");   
                }
            }
            
        }
        
        if (status.isUpdatedBaseDesign()) {
            log.info("Used version of base design is " + providerVersion.toString() + ". Overlay currently complies with base design version " + status.getOverlayData().getBasepluginVersion() + ". The overlay can be upgraded.");
        }
        
        // Gather changed resources in base design, so we can priorize them against the overlay resources
        FileObject sourceTmlFolder = sourceDesignProvider.getTmlFolder();
        FileObject targetTmlFolder = targetDirectory.resolveFile(DesignDirectory.FOLDERNAME_TML);
        if (sourceTmlFolder.exists() && sourceTmlFolder.getType().equals(FileType.FOLDER)) {
            for (FileObject mediaKeyFolder : sourceTmlFolder.getChildren()) {
                FileObject overlayFolder = mediaKeyFolder.resolveFile(OverlayDesignProvider.OVERLAY_FOLDER);
                if (overlayFolder.exists()) {
                    FileObject targetMediaKeyFolder = targetTmlFolder.resolveFile(mediaKeyFolder.getName().getBaseName());
                    determineChangedResources(WGDocument.TYPE_TML, targetMediaKeyFolder, overlayFolder, targetMediaKeyFolder, targetDirectory, sourceDesignProvider.getFileEncoding(), targetEncoding, status, log, validator);
                }
            }
        }

        FileObject targetScriptFolder = targetDirectory.resolveFile(DesignDirectory.FOLDERNAME_SCRIPT);
        FileObject sourceScriptFolder = sourceDesignProvider.getScriptFolder();
        if (sourceScriptFolder.exists() && sourceScriptFolder.getType().equals(FileType.FOLDER)) {
            for (FileObject scriptTypeFolder : sourceScriptFolder.getChildren()) {
                FileObject overlayFolder = scriptTypeFolder.resolveFile(OverlayDesignProvider.OVERLAY_FOLDER);
                if (overlayFolder.exists()) {
                    FileObject targetScriptTypeFolder = targetScriptFolder.resolveFile(scriptTypeFolder.getName().getBaseName());
                    determineChangedResources(WGDocument.TYPE_CSSJS, targetScriptTypeFolder, overlayFolder, targetScriptTypeFolder, targetDirectory, sourceDesignProvider.getFileEncoding() ,targetEncoding, status, log, validator);
                }
            }
        }

        FileObject overlayFolder = sourceDesignProvider.getFilesFolder().resolveFile(OverlayDesignProvider.OVERLAY_FOLDER);
        if (overlayFolder.exists()) {
            determineChangedFileContainerResources(targetFCFolder, overlayFolder, targetFCFolder, targetDirectory, null, null, status, log, validator);
        }
       
        return status;
        
        
    }
    public OverlayStatus determineOverlayStatus(PluginDesignProvider originalDesignProvider) throws Exception {
        return determineOverlayStatus(originalDesignProvider.getSourceDesignProvider(), originalDesignProvider.getPluginID(), getBaseFolder(), getFileEncoding(), getLog(), _core.getDesignFileValidator());
    }
    
    
    public static void createDowngradeFiles(FileSystemDesignProvider originalDesignProvider, OverlayData data, FileObject targetFolder, String targetEncoding, Logger log) throws Exception {
        
        for (Map.Entry<String,ResourceData> resourceEntry : data.getOverlayResources().entrySet()) {
            
            boolean changed = false;
            FileObject targetFile = targetFolder.resolveFile(resourceEntry.getKey());
            if (targetFile.exists()) {
                InputStream in = new BufferedInputStream(targetFile.getContent().getInputStream(), 4096);
                MD5HashingOutputStream out = new MD5HashingOutputStream(new NullOutputStream());
                resourceInToOut(in, targetEncoding, out, targetEncoding);
                if (!out.getHash().equals(resourceEntry.getValue().getMd5Hash())) {
                    changed = true;
                }
            }
            else {
                changed = true;
            }
            
            if (changed == true) {
                String basePath = getOverlayResourcePathInBaseDesign(resourceEntry.getKey());
                FileObject sourceFile = originalDesignProvider.getBaseFolder().resolveFile(basePath);
                if (sourceFile.exists()) {
                    FileObject conflictFile = createConflictFile(targetFile);
                    log.info("Writing downgrade file of modified overlay resource " + resourceEntry.getKey() + " to conflict file: " + targetFolder.getName().getRelativeName(conflictFile.getName()));
                    InputStream in = new BufferedInputStream(sourceFile.getContent().getInputStream(), 4096);
                    OutputStream out = new BufferedOutputStream(conflictFile.getContent().getOutputStream(), 4096);
                    resourceInToOut(in, originalDesignProvider.getFileEncoding(), out, targetEncoding);
                }
                else {
                    log.warn("Overlay resource '" + resourceEntry.getKey() + "' does not exist any more in the current base design version.");
                }
            }
            
        }
        
    }
    
    private static String getOverlayResourcePathInBaseDesign(String resourcePath) {

        List<String> pathElements = WGUtils.deserializeCollection(resourcePath, "/");
        
        if (pathElements.get(0).equals("files")) {
            pathElements.add(1, "overlay");
        }
        else {
            pathElements.add(2, "overlay");
        }
        
        return WGUtils.serializeCollection(pathElements, "/");
        
    }

    public static boolean upgradeOverlay(FileSystemDesignProvider originalDesignProvider, PluginID baseId, OverlayStatus status, FileObject targetFolder, String targetEncoding, Logger log, DesignFileValidator validator) throws Exception {

        if (!status.isUpdatedBaseDesign() && !status.isNewOverlay()) {
            throw new WGDesignSyncException("Used base plugin is no higher version than overlay compliance version. Cannot perform upgrade");
        }
        
        if (status.isNewOverlay()) {
            log.info("Initializing empty overlay");
        }
        else {
            log.info("Upgrading overlay from base design version " + status.getCompliantBaseVersion() + " to " + status.getCurrentBaseVersion());
        }
        
        // Creating new folders (Done separately because there may be empty folders in the overlay which would not be created via resource changes)
        for (String folder : status.getNewFolders()) {
            FileObject targetFile = targetFolder.resolveFile(folder);
            if (!targetFile.exists()) {
                log.info("Adding new overlay folder " + targetFolder.getName().getRelativeName(targetFile.getName()));
                targetFile.createFolder();
            }
        }
        
        // Perform resource changes
        boolean conflictFileCreated = false;
        for (ChangedDocument resource : status.getChangedDocuments().values()) {
            if (performChange(resource, originalDesignProvider, status, targetEncoding, targetFolder, log, validator)) {
                conflictFileCreated = true;
            }
        }
        
        // Overwrite plugin version
        status.getOverlayData().setBasepluginVersion(baseId.getVersion().toString());

        // Write new overlay data file
        FileObject targetFCFolder = targetFolder.resolveFile(DesignDirectory.FOLDERNAME_FILES);
        FileObject systemFC = targetFCFolder.resolveFile("system");
        FileObject overlayDataFile = systemFC.resolveFile(OverlayDesignProvider.OVERLAY_DATA_FILE);
        OutputStream out = new BufferedOutputStream(overlayDataFile.getContent().getOutputStream(false));
        status.getOverlayData().write(out);
        out.flush();
        out.close();

        // Eventually update base-csconfig.xml
        FileObject baseCsConfigFile = originalDesignProvider.getFilesFolder().resolveFile("system/csconfig.xml");
        if (baseCsConfigFile.exists()) {
            String sourceHash = MD5HashingInputStream.getStreamHash(baseCsConfigFile.getContent().getInputStream());
            String targetHash = "";
            FileObject baseCsConfigFileOnOverlay = systemFC.resolveFile("base-csconfig.xml");
            if (baseCsConfigFileOnOverlay.exists()) {
                targetHash = MD5HashingInputStream.getStreamHash(baseCsConfigFileOnOverlay.getContent().getInputStream());
            }
            if (!sourceHash.equals(targetHash)) {
                baseCsConfigFileOnOverlay.delete();
                FileUtil.copyContent(baseCsConfigFile, baseCsConfigFileOnOverlay);
            }
        }
        
        // Eventually update the dependency to the base plugin on csconfig.xml's plugin config
        FileObject overlayCsConfigFile = systemFC.resolveFile("csconfig.xml");
        if (overlayCsConfigFile.exists()) {
            CSConfig overlayCsConfig = CSConfig.load(overlayCsConfigFile);
            if (overlayCsConfig.getPluginConfig() != null) {
                boolean dependencyUpdated = false;
                for (PluginID id : overlayCsConfig.getPluginConfig().getDependencies()) {
                    if (id.getUniqueName().equals(baseId.getUniqueName()) && !id.getVersion().equals(baseId.getVersion())) {
                        Version dependencyVersion = new Version(baseId.getVersion().getMajorVersion(), baseId.getVersion().getMinorVersion(), baseId.getVersion().getMaintenanceVersion(), baseId.getVersion().getPatchVersion(), 0);
                        id.setVersion(dependencyVersion);
                        dependencyUpdated = true;
                    }
                }
                
                if (dependencyUpdated) {
                    log.info("Updating dependency to base plugin in overlay plugin to new version " + baseId.getVersion());
                    overlayCsConfig.write(overlayCsConfigFile);
                }
            }
        }
        
        // Read/Write design configuration model to ensure correct storage versions of csconfig.xml (#00003634)
        FileObject designDefinitionFile = DesignDirectory.getDesignDefinitionFile(targetFolder);
        WGADesignConfigurationModel model = new WGADesignConfigurationModel(new File(designDefinitionFile.getName().getPath()));
        model.saveChanges();
        
        // Clear the overlay status
        status.overlayWasUpgraded();
        
        return conflictFileCreated;
        
    }
    
    public boolean upgradeOverlay(PluginDesignProvider originalDesignProvider, OverlayStatus status) throws Exception {
        return upgradeOverlay(originalDesignProvider.getSourceDesignProvider(), originalDesignProvider.getPluginID(), status, getBaseFolder(), getFileEncoding(), getLog(), _core.getDesignFileValidator());
    }
    
    public void createDowngradeFiles(PluginDesignProvider originalDesignProvider, OverlayData data) throws Exception {
        createDowngradeFiles(originalDesignProvider.getSourceDesignProvider(), data, getBaseFolder(), getFileEncoding(), getLog());
    }

    private static boolean performChange(ChangedDocument changedDocument, FileSystemDesignProvider originalDesignProvider, OverlayStatus status, String targetEncoding, FileObject baseFolder, Logger log, DesignFileValidator validator)
            throws FileSystemException, NoSuchAlgorithmException, UnsupportedEncodingException, IOException, WGDesignSyncException {
        
        boolean conflictFileCreated = false;
        log.info("Updating overlay resource " + changedDocument.getDocumentKey());
        
        // Find files which represent the document in source and target
        FileObject sourceDocFile = originalDesignProvider.getBaseFolder().resolveFile(changedDocument.getSourceFilePath());
        FileObject targetDocFile = baseFolder.resolveFile(changedDocument.getTargetFilePath());
        
        // Collect files to copy and delete
        Map<FileObject,FileObject> filesToCopy = new HashMap<FileObject, FileObject>();
        List<FileObject> filesToDelete = new ArrayList<FileObject>();
        
        // Collect for file containers: Must traverse container content
        if (changedDocument.getDocumentKey().getDocType() == WGDocument.TYPE_FILECONTAINER) {
            
            if (changedDocument.getChangeType() == ChangeType.NEW) {
                targetDocFile.createFolder();
            }
            
            // Copy all files in container from the source to the target 
            for (FileObject sourceFile : sourceDocFile.getChildren()) {
                if (sourceFile.getType().equals(FileType.FILE)) {
                    if (!isValidDesignFile(sourceFile, validator)) {
                        continue;
                    }
                    filesToCopy.put(sourceFile, targetDocFile.resolveFile(sourceFile.getName().getBaseName()));
                }
            }
            
            // Delete all files in target that were deployed with previous base version but are deleted in current base version 
            for (FileObject targetFile : targetDocFile.getChildren()) {
                if (targetFile.getType().equals(FileType.FILE)) {
                    if (!isValidDesignFile(targetFile, validator)) {
                        continue;
                    }
                    
                    FileObject sourceFile = sourceDocFile.resolveFile(targetFile.getName().getBaseName());
                    if (sourceFile.exists()) {
                        continue;
                    }
                    
                    // Only delete those that were deployed with previous base version and have unaltered content
                    String resourcePath = baseFolder.getName().getRelativeName(targetFile.getName());
                    ResourceData resourceData = status.getOverlayData().getOverlayResources().get(resourcePath);
                    if (resourceData != null) {
                        String hash = MD5HashingInputStream.getStreamHash(targetFile.getContent().getInputStream());
                        if (resourceData.getMd5Hash().equals(hash)) {
                            filesToDelete.add(targetFile);
                        }
                    }
                }
            }
            
        }
        
        // Collect for anything else
        else {
            filesToCopy.put(sourceDocFile, targetDocFile);
        }
        
        // Copy files
        for (Map.Entry<FileObject,FileObject> files : filesToCopy.entrySet()) {
            
            FileObject sourceFile = files.getKey();
            FileObject targetFile = files.getValue();
            String resourcePath = baseFolder.getName().getRelativeName(targetFile.getName());
            
            if (changedDocument.getChangeType() == ChangeType.CONFLICT) {
                // Do a test if the current file is conflicting. If so we write to a conflict file instead
                InputStream in = new BufferedInputStream(sourceFile.getContent().getInputStream());
                String currentHash = MD5HashingInputStream.getStreamHash(in);
                ResourceData deployedHash = status.getOverlayData().getOverlayResources().get(resourcePath);
                boolean skipConflict = false;
                
                // Conflict on file container: A single file might just be missing in the target. We can safely copy that to the target without treating as conflict (#00002440)
                if (deployedHash == null && 
                        changedDocument.getDocumentKey().getDocType() == WGDocument.TYPE_FILECONTAINER &&
                        !targetFile.exists()) {
                    skipConflict = true;
                }
                
                if (!skipConflict && (deployedHash == null || !deployedHash.getMd5Hash().equals(currentHash))) {
                    targetFile = createConflictFile(targetFile);
                    conflictFileCreated = true;
                    log.warn("Modified overlay resource " + resourcePath + " is updated in base design. We write the updated base version to conflict file for manual resolution: " + baseFolder.getName().getRelativeName(targetFile.getName()));
                }
            }
        
            // Write file
            InputStream in = new BufferedInputStream(sourceFile.getContent().getInputStream());
            MD5HashingOutputStream out = new MD5HashingOutputStream(new BufferedOutputStream(targetFile.getContent().getOutputStream(false)));
            
            // Update resource data
            resourceInToOut(in, originalDesignProvider.getFileEncoding(), out, targetEncoding);
            OverlayData.ResourceData resourceData = new OverlayData.ResourceData();
            resourceData.setMd5Hash(out.getHash());
            status.getOverlayData().setOverlayResource(resourcePath, resourceData);
            
        }
        
        // Delete files
        for (FileObject fileToDelete : filesToDelete) {
            String resourcePath = baseFolder.getName().getRelativeName(fileToDelete.getName());
            fileToDelete.delete();
            status.getOverlayData().removeOverlayResource(resourcePath);
        }
        
        return conflictFileCreated;
        
    }

    protected static FileObject createConflictFile(FileObject targetFile) throws FileSystemException {
        String conflictFileName = targetFile.getName().getBaseName() + "_ovlconflict." + targetFile.getName().getExtension();
        targetFile = targetFile.getParent().resolveFile(conflictFileName);
        if (targetFile.exists()) {
            targetFile.delete();
        }
        return targetFile;
    }

    private static void determineChangedResources(int resourceType, FileObject categoryFolder, FileObject source, FileObject target, FileObject baseFolder, String sourceEncoding, String targetEncoding, OverlayStatus status, Logger log, DesignFileValidator validator) throws WGDesignSyncException, NoSuchAlgorithmException, IOException {

        for (FileObject sourceFile : source.getChildren()) {
            if (!isValidDesignFile(sourceFile, validator)) {
                continue;
            }

            FileObject targetFile = target.resolveFile(sourceFile.getName().getBaseName());
            String resourcePath = baseFolder.getName().getRelativeName(targetFile.getName());
            
            if (sourceFile.getType().equals(FileType.FOLDER)) {
                if (!targetFile.exists()) {
                    status.getNewFolders().add(targetFile.getName().getPath());
                }
                else if (targetFile.getType().equals(FileType.FILE)) {
                    throw new WGDesignSyncException("Unable to apply overlay. Folder '" + baseFolder.getName().getRelativeName(targetFile.getName())
                            + " already exists as file. Delete it to enable overlay management again");
                }
                determineChangedResources(resourceType, categoryFolder, sourceFile, targetFile, baseFolder, sourceEncoding, targetEncoding, status, log, validator);
            }
            else if (sourceFile.getType().equals(FileType.FILE)) {
                
                // File does not exist.
                if (!targetFile.exists()) {
                    
                    // Was it once deployed?
                    ResourceData originalHash = status.getOverlayData().getOverlayResources().get(resourcePath);
                    if (originalHash == null) { // No, so it must be new in base
                        status.addChangedResource(resourceType, sourceFile, targetFile, categoryFolder, OverlayStatus.ChangeType.NEW, resourcePath, null);
                    }
                    else { // Yes, Check if the base file changed since deployment
                         String newHash = MD5HashingInputStream.getStreamHash(sourceFile.getContent().getInputStream());
                        if (newHash.equals(originalHash.getMd5Hash())) { // Nope. So this is no change. The overlay just chose not to use the file
                            continue; 
                        }
                        else { // Yes, so it is indeed a conflict
                            status.addChangedResource(resourceType, sourceFile, targetFile, categoryFolder, OverlayStatus.ChangeType.CONFLICT, resourcePath, null);                            
                        }
                    }
                    
                }
                
                // File does exist: Determine if is updated in base since the overlay file was deployed
                else {
                    
                    ResourceData originalHash = status.getOverlayData().getOverlayResources().get(resourcePath);
                    if (originalHash == null) {
                        if (!status.isUpdatedBaseDesign()) {
                            log.info("There is no information about the original deployment state of  resource " + resourcePath + ". Setting original deployment state now to the current base version.");
                            OverlayData.ResourceData resource = new OverlayData.ResourceData();
                            resource.setMd5Hash(MD5HashingInputStream.getStreamHash(sourceFile.getContent().getInputStream()));
                            status.getOverlayData().setOverlayResource(resourcePath, resource);
                        }
                        else {
                            log.info("Cannot update overlay resource " + resourcePath + " as there is no information of its original deployment state.");
                        }
                        continue;
                    }

                    // First determine if the resource really changed from what was distributed
                    String newHash = MD5HashingInputStream.getStreamHash(sourceFile.getContent().getInputStream());
                    if (newHash.equals(originalHash.getMd5Hash())) {
                        continue;
                    }
                    
                    // Determine if the target file is the same as was distributed. If not then it was user modified, so it is a conflict
                    String currentHash = MD5HashingInputStream.getStreamHash(targetFile.getContent().getInputStream());
                    if (!currentHash.equals(originalHash.getMd5Hash())) {
                        status.addChangedResource(resourceType, sourceFile, targetFile, categoryFolder, OverlayStatus.ChangeType.CONFLICT, resourcePath, null);
                    }
                    
                    // It is a normal change
                    else {
                        status.addChangedResource(resourceType, sourceFile, targetFile, categoryFolder, OverlayStatus.ChangeType.CHANGED, resourcePath, null);
                    }
                }
            }
        }

    }
    
    private static void determineChangedFileContainerResources(FileObject categoryFolder, FileObject source, FileObject target, FileObject baseFolder, String sourceEncoding, String targetEncoding, OverlayStatus status, Logger log, DesignFileValidator validator) throws WGDesignSyncException, NoSuchAlgorithmException, IOException {
        
        String targetResourcePath = baseFolder.getName().getRelativeName(target.getName());
        Set<String> involvedFiles = new HashSet<String>();
        
        // Completely new folder is a new file container
        if (!target.exists()) {
            
            status.addChangedResource(WGDocument.TYPE_FILECONTAINER, source, target, categoryFolder, ChangeType.NEW, targetResourcePath, null);
            // Als add all subfolders
            for (FileObject sourceFile : source.getChildren()) {
                if (sourceFile.getType().equals(FileType.FOLDER)) {
                    FileObject targetFile = target.resolveFile(sourceFile.getName().getBaseName());
                    determineChangedFileContainerResources(categoryFolder, sourceFile, targetFile, baseFolder, sourceEncoding, targetEncoding, status, log, validator);
                }
            }
            return;
        }
        
        
        if (target.getType().equals(FileType.FILE)) {
            throw new WGDesignSyncException("Unable to apply overlay. Folder '" + baseFolder.getName().getRelativeName(target.getName())
                    + " already exists as file. Delete it to enable overlay management again");
        }

        // Determine change type by iterating through source child files and compare with target
        boolean overlayChanged = false;
        boolean baseChanged = false;
        boolean directConflict = false;
        
        for (FileObject sourceFile : source.getChildren()) {
            if (!isValidDesignFile(sourceFile, validator)) {
                continue;
            }

            FileObject targetFile = target.resolveFile(sourceFile.getName().getBaseName());
            String fileResourcePath = baseFolder.getName().getRelativeName(targetFile.getName());
            
            if (sourceFile.getType().equals(FileType.FOLDER)) {
                // Descend onto subcontainer
                determineChangedFileContainerResources(categoryFolder, sourceFile, targetFile, baseFolder, sourceEncoding, targetEncoding, status, log, validator);
            }
            else if (sourceFile.getType().equals(FileType.FILE)) {
                
                // File does not exist. Look if it was once deployed.
                if (!targetFile.exists()) {
                    
                        ResourceData resourceData = status.getOverlayData().getOverlayResources().get(fileResourcePath);
                        if (resourceData != null) { 
                            // Did already exist. But did it have the same content?
                            String newHash = MD5HashingInputStream.getStreamHash(sourceFile.getContent().getInputStream());
                            if (newHash.equals(resourceData.getMd5Hash())) {
                                overlayChanged = true;
                                involvedFiles.add(sourceFile.getName().getBaseName() + " (removed in overlay)");
                            }
                            else {
                                baseChanged = true;
                                overlayChanged = true;
                                directConflict = true;
                                involvedFiles.add(sourceFile.getName().getBaseName() + " (removed in overlay, changed in base)");
                            }
                            
                        }
                        
                        // Did not yet exist. It is a new file in the base version.
                        else {
                            baseChanged = true;
                            involvedFiles.add(sourceFile.getName().getBaseName() + " (new in base)");
                        }
                    
                }
                
                // File does exist: Determine if is updated in base since the overlay file was deployed
                else {
                    
                    ResourceData originalHash = status.getOverlayData().getOverlayResources().get(fileResourcePath);
                    if (originalHash == null) {
                        log.warn("There is no information about the original deployment state of resource '" + fileResourcePath + "' so its change status cannot be determined.");
                        OverlayData.ResourceData resource = new OverlayData.ResourceData();
                        resource.setMd5Hash(MD5HashingInputStream.getStreamHash(sourceFile.getContent().getInputStream()));
                        status.getOverlayData().setOverlayResource(fileResourcePath, resource);
                        continue;
                    }

                    // First determine if the resource really changed from what was distributed
                    String newHash = MD5HashingInputStream.getStreamHash(sourceFile.getContent().getInputStream());
                    if (newHash.equals(originalHash.getMd5Hash())) {
                        continue;
                    }
                    
                    // Determine if the target file is the same as was distributed. If not then it was user modified, so it is a conflict
                    String currentHash = MD5HashingInputStream.getStreamHash(targetFile.getContent().getInputStream());
                    if (!currentHash.equals(originalHash.getMd5Hash())) {
                        overlayChanged = true;
                        baseChanged = true;
                        directConflict = true;
                        involvedFiles.add(sourceFile.getName().getBaseName() + " (changed in base and overlay)");
                        break;
                    }
                    
                    // It is a normal change
                    else {
                        baseChanged = true;
                        involvedFiles.add(sourceFile.getName().getBaseName() + " (changed in base)");
                    }
                }
            }
        }
        
        
        // Test the target files. Files may have been added there, or files from the previous base version may still be present that got deleted in the new base version.
        if (!baseChanged || !overlayChanged) {
            for (FileObject targetFile : target.getChildren()) {
                FileObject sourceFile = source.resolveFile(targetFile.getName().getBaseName());
                if (!sourceFile.exists()) {
                    
                    // Look if it was deployed with the previous base version.
                    String fileResourcePath = baseFolder.getName().getRelativeName(targetFile.getName());
                    ResourceData resourceData = status.getOverlayData().getOverlayResources().get(fileResourcePath);
                    if (resourceData != null) {
                        // Was deployed. But with same contents?
                        String targetHash = MD5HashingInputStream.getStreamHash(targetFile.getContent().getInputStream());
                        if (targetHash.equals(resourceData.getMd5Hash())) {
                            // This is a file that was from previous base version and got removed in the new base version.
                            involvedFiles.add(targetFile.getName().getBaseName() + " (removed from base)");
                            baseChanged = true;
                        }
                        
                        // File got removed in new base version, updated in overlay. Conflict.
                        else {
                            baseChanged = true;
                            overlayChanged = true;
                            directConflict = true;
                            involvedFiles.add(targetFile.getName().getBaseName() + " (removed from base, changed in overlay)");
                            break;
                        }
                    }
                    else {
                        involvedFiles.add(targetFile.getName().getBaseName() + " (new in overlay)");
                        overlayChanged = true;
                    }
                }
            }
        }

        // Determine change type based on the found changes
        ChangeType changeType = null;
        if (baseChanged) {
            if (overlayChanged) {
                changeType = ChangeType.CONFLICT;
            }
            else {
                changeType = ChangeType.CHANGED;
            }
        }
        
        if (changeType != null) {
            status.addChangedResource(WGDocument.TYPE_FILECONTAINER, source, target, categoryFolder, changeType, targetResourcePath, involvedFiles);
        }

    }

    private static void resourceInToOut(InputStream in, String sourceEncoding, OutputStream out, String targetEncoding) throws UnsupportedEncodingException, IOException {
        if (sourceEncoding != null && targetEncoding != null) {
            Reader inReader = new InputStreamReader(in, sourceEncoding);
            Writer outWriter = new OutputStreamWriter(out, targetEncoding);
            WGUtils.inToOut(inReader, outWriter, 4096);
            inReader.close();
            outWriter.flush();
            outWriter.close();
        }
        else {
            WGUtils.inToOut(in, out, 4096);
            in.close();
            out.flush();
            out.close();
        }
    }
    
    public boolean isReady() {
        return (getFsResources() != null);
    }
    
    @Override
    public boolean isSynchronizeAccess() {
        return false;
    }

    public boolean isNoBackgroundChanges() {
        return _noBackgroundChanges;
    }

}
