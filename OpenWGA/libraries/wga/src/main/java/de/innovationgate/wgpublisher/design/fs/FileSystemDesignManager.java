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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.logging.impl.Log4JLogger;
import org.apache.commons.transaction.file.FileResourceManager;
import org.apache.commons.vfs2.CacheStrategy;
import org.apache.commons.vfs2.Capability;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystem;
import org.apache.commons.vfs2.FileSystemConfigBuilder;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.impl.DefaultFileReplicator;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
import org.apache.log4j.Logger;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.utils.DESEncrypter;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseConnectListener;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.DesignDirectory.ScriptInformation;
import de.innovationgate.wga.common.beans.DesignDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.types.DesignResourceConversionModuleType;
import de.innovationgate.wgpublisher.ManagedDBAttribute;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversion;
import de.innovationgate.wgpublisher.design.conversion.DesignResourceConversionProperties;
import de.innovationgate.wgpublisher.design.sync.DesignFileValidator;
import de.innovationgate.wgpublisher.design.sync.DesignSyncManager;
import de.innovationgate.wgpublisher.design.sync.WGDesignSyncException;
import de.innovationgate.wgpublisher.design.sync.WGInitialDeployException;

/**
 * Common base class of {@link FileSystemDesignProvider} and {@link DesignSyncManager}
 * implementing common functionalities
 *
 */
public abstract class FileSystemDesignManager implements WGDatabaseConnectListener, ManagedDBAttribute {

    public static final String OPTION_DESIGNKEY = "designkey";

    public static final String LOGGER_DESIGNSYNC = "wga.designsync";

    public static final String SYNCSTATUS_MODULE = "designsyncstatus";

    public static final String DIRECTORY_DIVIDER = ":";

    public static Map<Integer, DesignInformation> _designTypes = new HashMap<Integer, DesignInformation>();

    protected static XStream _xstream = XStreamUtils.createXStream(new Dom4JDriver());

    private Pattern _validDesignName = Constants.PATTERN_FILENAMES;

    public static class FileSystemResources {

        private FileObject _baseFolder;

        private FileObject _filesFolder;

        private FileObject _tmlFolder;

        private FileObject _scriptFolder;
        
        private FileObject _javaFolder;

        private FileObject _designDefinitionFile;

        private boolean _updateableFileSystem = true;

        public FileSystemResources(FileObject baseFolder) throws FileSystemException {
            _baseFolder = baseFolder;
            _tmlFolder = _baseFolder.resolveFile(DesignDirectory.FOLDERNAME_TML);
            _scriptFolder = _baseFolder.resolveFile(DesignDirectory.FOLDERNAME_SCRIPT);
            _filesFolder = _baseFolder.resolveFile(DesignDirectory.FOLDERNAME_FILES);
            _javaFolder = _baseFolder.resolveFile(DesignDirectory.FOLDERNAME_JAVA);
            
            
            _designDefinitionFile = DesignDirectory.getDesignDefinitionFile(_baseFolder);
            if (_designDefinitionFile == null) {
                _designDefinitionFile = _baseFolder.resolveFile(DesignDirectory.DESIGN_DEFINITION_FILE);
            }
            
            _updateableFileSystem = !_baseFolder.getFileSystem().hasCapability(Capability.VIRTUAL);
            
        }

        protected FileObject getBaseFolder() {
            return _baseFolder;
        }

        protected FileObject getFilesFolder() {
            return _filesFolder;
        }

        protected FileObject getScriptFolder() {
            return _scriptFolder;
        }

        protected FileObject getDesignDefinitionFile() {
            return _designDefinitionFile;
        }

        protected FileObject getTmlFolder() {
            return _tmlFolder;
        }

        public FileObject getJavaFolder() {
            return _javaFolder;
        }

        protected boolean isUpdateableFileSystem() {
            return _updateableFileSystem;
        }

        public void refreshAll() throws FileSystemException {
            _baseFolder.refresh();
            _designDefinitionFile.refresh();
            _filesFolder.refresh();
            _javaFolder.refresh();
            _scriptFolder.refresh();
            _tmlFolder.refresh();
        }

    }

    public class ModuleFile {

        private FileObject _file;

        private String _prefix;

        private String _category;

        private int _type;

        public ModuleFile(FileObject file, String prefix, String category, int type) throws FileSystemException {
            _file = file;
            _prefix = prefix;
            _category = category;
            _type = type;
        }

        /**
         * @return Returns the file.
         */
        public FileObject getFile() {
            return _file;
        }

        /**
         * @return Returns the prefix.
         */
        public String getPrefix() {
            return _prefix;
        }

        public String getModuleName() {
            String fileName = _file.getName().getBaseName().toLowerCase();
            int suffixPos = fileName.lastIndexOf(".");

            String nameWithoutSuffix;
            if (suffixPos != -1) {
                nameWithoutSuffix = fileName.substring(0, suffixPos);
            }
            else {
                nameWithoutSuffix = fileName;
            }

            if (_prefix == null || _prefix.trim().equals("")) {
                return nameWithoutSuffix;
            }
            else {
                return _prefix + DIRECTORY_DIVIDER + nameWithoutSuffix;
            }
        }

        public String getCategory() {
            return _category;
        }

        public int getType() {
            return _type;
        }

    }

    public static class DesignInformation {

        private Class<? extends DesignMetadata> _metaDataClass;

        public DesignInformation(Class<? extends DesignMetadata> clazz) {
            _metaDataClass = clazz;
        }

        /**
         * @return Returns the metaDataClass.
         */
        public Class<? extends DesignMetadata> getMetaDataClass() {
            return _metaDataClass;
        }
    }

    static {
        // Map design types
        Class<ScriptMetadata> scriptClass = ScriptMetadata.class;
        _designTypes.put(new Integer(WGDocument.TYPE_CSSJS), new DesignInformation(ScriptMetadata.class));
        _designTypes.put(new Integer(WGDocument.TYPE_TML), new DesignInformation(TMLMetadata.class));
        _designTypes.put(new Integer(WGDocument.TYPE_FILECONTAINER), new DesignInformation(FCMetadata.class));
    }

    public static DesignInformation getDesignInformation(int type) {
        return _designTypes.get(new Integer(type));
    }

    protected WGDatabase _db;

    protected Logger _log;

    protected String _designKey;

    private String _fileEncoding = null;

    private DesignDefinition _syncInfo;

    protected WGACore _core;

    protected StandardFileSystemManager _fsManager;

    protected String _designPath;

    protected static final String LOGGER_DESIGNSYNC_QUIET = "wga.designsync.quiet";

    public static final String MODE_FULL = "full";

    public static final String MODE_VIRTUAL = "virtual";

    public static final String MODE_DIRECT = "direct";

    private FileSystemResources _fsResources = null;

    private DESEncrypter _cipher;

    protected boolean _directAccessDefault;

    private CSConfig _csconfig = null;

    protected Set<Integer> _syncedDoctypes = new HashSet<Integer>();

    protected Map<String, String> _designOptions;

    private String _javaClassesPath;

    private Long _designFileTimestamp = null;

    private Long _csconfigTimestamp = null;

    protected boolean _lookupVariants = false;

    public FileSystemDesignManager(WGACore core, WGDatabase db, String path, Map<String,String> options) throws WGDesignSyncException, IOException, WGAPIException, InstantiationException,
            IllegalAccessException, InvalidCSConfigVersionException {
        _core = core;

        // Init logger
        if (db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
            _log = Logger.getLogger(LOGGER_DESIGNSYNC_QUIET);
        }
        else {
            _log = Logger.getLogger(LOGGER_DESIGNSYNC);
        }

        _fsManager = new StandardFileSystemManager();
        _fsManager.setCacheStrategy(CacheStrategy.MANUAL);
        _fsManager.setLogger(new Log4JLogger(Logger.getLogger(LOGGER_DESIGNSYNC_QUIET)));
        _fsManager.setClassLoader(WGACore.getLibraryLoader());
        _fsManager.setCacheStrategy(getVFSCacheStrategy());
        _fsManager.init();

        _designPath = path;
        _designOptions = options;
        _designKey = options.get(OPTION_DESIGNKEY);

        _db = db;

        _directAccessDefault = db.getBooleanAttribute(WGACore.DBATTRIB_DIRECTACCESSDEFAULT, false);

        // Determine provided types
        String optionProviderTypes = (String) db.getCreationOptions().get(WGDatabase.COPTION_DESIGNPROVIDERTYPES);
        if (optionProviderTypes != null) {
            Iterator<String> providerTypes = WGUtils.deserializeCollection(optionProviderTypes, ",", true).iterator();
            while (providerTypes.hasNext()) {
                String providerTypeName = providerTypes.next();
                int providerType = WGDocument.doctypeNameToNumber(providerTypeName);
                if (providerType != 0) {
                    _syncedDoctypes.add(new Integer(providerType));
                }
            }
        }
        else {
            _syncedDoctypes.add(new Integer(WGDocument.TYPE_FILECONTAINER));
            _syncedDoctypes.add(new Integer(WGDocument.TYPE_TML));
            _syncedDoctypes.add(new Integer(WGDocument.TYPE_CSSJS));
        }

        fetchFileSystem(core);
        if (_db.isConnected()) {
            init();
        }
        else {
            _db.addDatabaseConnectListener(this);
        }

    }

    protected CacheStrategy getVFSCacheStrategy() {
        return CacheStrategy.ON_RESOLVE;
    }

    protected void closeFileSystem() {

        if (_fsResources != null) {
            FileSystem fs = _fsResources.getBaseFolder().getFileSystem();
            _fsResources = null;
            _fsManager.closeFileSystem(fs);
            _fsManager.freeUnusedResources();
        }

    }

    protected void fetchFileSystem(WGACore core) throws WGDesignSyncException, IOException {

        // Dont run if already fetched
        if (_fsResources != null) {
            return;
        }

        FileObject baseFolder = null;


        try {
            baseFolder = _fsManager.resolveFile(_designPath);
            if (!baseFolder.exists() || !baseFolder.getType().equals(FileType.FOLDER)) {
                throw new WGDesignSyncException("Directory location " + _designPath + " either does not exist or is not directory");
            }
        }
        catch (FileSystemException e) {
            throw new WGDesignSyncException("Unparseable VFS URI: " + _designPath, e);
        }

        _fsResources = new FileSystemResources(baseFolder);
    }

    /**
     * @param doInitSync
     * @return
     * @throws WGAPIException
     * @throws IOException
     * @throws WGDesignSyncException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws InvalidCSConfigVersionException
     */
    protected boolean init() throws WGAPIException, IOException, WGDesignSyncException, InstantiationException, IllegalAccessException, InvalidCSConfigVersionException {

        // Determine the state of the directory
        FileObject syncInfoFile = getSyncInfoFile();

        if (!syncInfoFile.exists()) {

            // A new directory. Do initial deployment
            if (isEditable()) {
                _log.info("Performing initial deployment of design to directory '" + getBaseFolder().getURL().toString() + "'");
                _fileEncoding = _core.getWgaConfiguration().getDesignConfiguration().getDefaultEncoding();
                doInitialDeployment(_designKey);
                return true;
            }
            else {
                throw new WGDesignSyncException("Cannot perform initial deploy because the file system is readonly: " + getBaseFolder().getURL().toString());
            }
        }
        else {
            // Determine if syncdata file is present. If so, use this directory
            // as sync directory.
            getOrCreateFolders();
            updateConfiguration();
            return false;

        }

    }

    public boolean isEditable() throws WGDesignSyncException {
        return getBaseFolder().getFileSystem().hasCapability(Capability.CREATE);
    }

    protected void updateConfiguration() throws IOException, WGDesignSyncException, FileSystemException, InvalidCSConfigVersionException {
        
        boolean configUpdated = false;
        
        // Import config files
        FileObject designFile = getSyncInfoFile();
        long currentDesignFileTimestamp = designFile.getContent().getLastModifiedTime();
        if (_designFileTimestamp == null || currentDesignFileTimestamp > _designFileTimestamp.longValue()) {
            _designFileTimestamp = new Long(currentDesignFileTimestamp);
            importSyncInfo(designFile);
            configUpdated = true;
        }

        FileObject csconfigFile = getFilesFolder().resolveFile("system/csconfig.xml");
        if (csconfigFile.exists()) {
            long currentCsconfigTimestamp = csconfigFile.getContent().getLastModifiedTime();
            if (_csconfigTimestamp == null || currentCsconfigTimestamp > _csconfigTimestamp.longValue()) {
                _csconfigTimestamp = new Long(currentCsconfigTimestamp);
                importCSConfig(csconfigFile);
                configUpdated = true;
            }
        }

        // Load configuration settings
        if (configUpdated) {
            determineDesignEncoding();
            determineEncryption();
            determineSyncDefaults();
        }
    }

    private void determineSyncDefaults() {

        if (_csconfig != null) {
            PublisherOption option = _csconfig.findPublisherOption(WGACore.DBATTRIB_DIRECTACCESSDEFAULT);
            if (option != null) {
                _directAccessDefault = Boolean.valueOf(option.getValue()).booleanValue();
            }
        }

    }

    private void importCSConfig(FileObject csconfigFile) throws FileSystemException, IOException, WGDesignSyncException, InvalidCSConfigVersionException {
        if (csconfigFile.exists()) {
            _csconfig = CSConfig.load(csconfigFile);
        }

    }

    private void determineEncryption() throws WGDesignSyncException {
        try {
            FileObject file = getBaseFolder().resolveFile(DesignDirectory.OBFUSCATE_FLAGFILE);
            if (file.exists()) {
                _cipher = new DESEncrypter();
                _cipher.initObfuscation();
            }
        }
        catch (Exception e) {
            throw new WGDesignSyncException("Exception inizializing deobfuscation", e);
        }

    }

    private FileObject getSyncInfoFile() throws WGDesignSyncException {
        if (_fsResources != null) {
            return _fsResources.getDesignDefinitionFile();
        }
        else {
            throw new WGDesignSyncException("The base folder was retrieved from an illegal code position");
        }
    }

    public void close() {
        closeFileSystem();
        _fsManager.close();
    }

    private void importSyncInfo(FileObject syncInfoFile) throws IOException {
        _syncInfo = DesignDefinition.load(syncInfoFile);
    }

    protected void doInitialDeployment(String designKey) throws WGAPIException, IOException, InstantiationException, IllegalAccessException, WGDesignSyncException {

        // Creat folders
        getOrCreateFolders();

        // Deploy TMLs
        Iterator modules = getDB().getTMLModules().iterator();
        WGTMLModule mod;
        while (modules.hasNext()) {
            mod = (WGTMLModule) modules.next();
            if (isValidDesignName(mod.getName())) {
                initialDeployTMLModule(mod);
            }
            else {
                _log.warn("Could not use '" + mod.getDocumentKey() + "' for design deployment since the name contains invalid characters");
            }
        }

        // Deploy file containers
        Iterator containers = getDB().getFileContainers().iterator();
        WGFileContainer con;
        while (containers.hasNext()) {
            con = (WGFileContainer) containers.next();
            if (isValidDesignName(con.getName())) {
                initialDeployFileContainer(con);
            }
            else {
                _log.warn("Could not use '" + con.getDocumentKey() + "' for design deployment since the name contains invalid characters");
            }
        }

        // Deploy script modules
        Iterator scripts = getDB().getCSSJSModules().iterator();
        WGCSSJSModule script;
        while (scripts.hasNext()) {
            script = (WGCSSJSModule) scripts.next();
            if (isValidDesignName(script.getName())) {
                initialDeployScriptModule(script);
            }
            else {
                _log.warn("Could not use '" + script.getDocumentKey() + "' for design deployment since the name contains invalid characters");
            }
        }

        // Create sync info and store it to directory
        _syncInfo = new DesignDefinition();
        _syncInfo.setDesignKey(designKey);
        if (_core.getWgaConfiguration().getDesignConfiguration().getDefaultEncoding() != null) {
            _syncInfo.setFileEncoding(_core.getWgaConfiguration().getDesignConfiguration().getDefaultEncoding());
        }
        else {
            // if encoding not specified set to platform encoding
            _syncInfo.setFileEncoding(System.getProperty("file.encoding"));
        }
        FileObject syncInfoFile = getSyncInfoFile();
        _log.info("Creating design definition file " + syncInfoFile.getName().getPath());
        syncInfoFile.createFile();
        storeSyncInfo(syncInfoFile);

        _log.info("Initial deploy finished");

    }

    private boolean isValidDesignName(String name) {

        return _validDesignName.matcher(name).matches();

    }

    private void getOrCreateFolders() throws IOException, WGDesignSyncException {
        if (isEditable()) {
            WGUtils.getOrCreateFolder(getBaseFolder(), DesignDirectory.FOLDERNAME_TML);
            WGUtils.getOrCreateFolder(getBaseFolder(), DesignDirectory.FOLDERNAME_SCRIPT);
            WGUtils.getOrCreateFolder(getBaseFolder(), DesignDirectory.FOLDERNAME_FILES);

            Iterator scriptInfos = DesignDirectory.getScriptTypes().values().iterator();
            while (scriptInfos.hasNext()) {
                ScriptInformation info = (ScriptInformation) scriptInfos.next();
                if (info.isAutoCreate()) {
                    WGUtils.getOrCreateFolder(getScriptFolder(), info.getFolder());
                }
            }
            
            if (_fsResources.getJavaFolder().exists()) {
                _javaClassesPath = _fsResources.getJavaFolder().getName().getPath();
            }
        }
    }

    protected FileObject getFilesFolder() throws WGDesignSyncException {
        if (_fsResources != null) {
            return _fsResources.getFilesFolder();
        }
        else {
            throw new WGDesignSyncException("The files folder was retrieved from an illegal code position");
        }
    }

    protected FileObject getScriptTypeFolder(String folderName) throws FileSystemException, WGDesignSyncException {
        FileObject scriptFolder = getScriptFolder();
        if (scriptFolder.exists()) {
            FileObject typeFolder = scriptFolder.resolveFile(folderName);
            if (typeFolder.exists()) {
                return typeFolder;
            }
        }
        
        return null;
    }

    protected FileObject getScriptFolder() throws WGDesignSyncException {
        if (_fsResources != null) {
            return _fsResources.getScriptFolder();
        }
        else {
            throw new WGDesignSyncException("The base folder was retrieved from an illegal code position");
        }
    }

    private void storeSyncInfo(FileObject syncInfoFile) throws IOException {
        _syncInfo.write(syncInfoFile);
    }

    private Writer createWriter(FileObject file) throws IOException {
        return new OutputStreamWriter(file.getContent().getOutputStream(), getFileEncoding());
    }

    protected FileObject initialDeployScriptModule(WGCSSJSModule script) throws IOException, InstantiationException, IllegalAccessException, WGAPIException, WGDesignSyncException {

        if (script.isMetadataModule()) {
            return null;
        }

        ScriptInformation info = DesignDirectory.getScriptInformation(script.getCodeType());
        if (info == null) {
            _log.warn("Cannot deploy unknown script code type: " + script.getCodeType());
            return null;
        }

        // Get script type folder
        FileObject scriptTypeFolder = getScriptTypeFolder(info.getFolder());

        // Eventually create intermediate directories
        List<String> path = WGUtils.deserializeCollection(script.getName(), ":", true);
        String localName = (String) path.get(path.size() - 1);
        FileObject currentDir = scriptTypeFolder;
        for (int i = 0; i < path.size() - 1; i++) {
            currentDir = currentDir.resolveFile((String) path.get(i));
            if (!currentDir.exists()) {
                _log.info("Creating script category directory" + getRelativePath(currentDir));
                try {
                    currentDir.createFolder();
                }
                catch (FileSystemException e) {
                    throw new WGInitialDeployException("Could not create script category folder '" + getRelativePath(currentDir) + "'", e);
                }
            }
            else if (!currentDir.getType().equals(FileType.FOLDER)) {
                throw new WGInitialDeployException("Cannot deploy " + script.getDocumentKey() + " to sync folder because the directory name '" + path.get(i) + "' is already used by another file");
            }
        }

        // Create code file
        FileObject codeFile = currentDir.resolveFile(localName + info.getSuffix());
        _log.info("Creating script module file " + getRelativePath(codeFile));
        try {
            codeFile.createFile();
        }
        catch (FileSystemException e) {
            throw new WGInitialDeployException("Could not create script code file '" + codeFile.getName().getPathDecoded() + "'");
        }
        Writer writer = createWriter(codeFile);
        writer.write(script.getCode());
        writer.close();

        // Create metadata file
        ScriptMetadata metaData = new ScriptMetadata(script);
        FileObject metadataDir = currentDir.resolveFile(DesignDirectory.NAME_METADATADIR);
        if (!metadataDir.exists()) {
            _log.info("Creating script metadata directory " + getRelativePath(metadataDir));
            try {
                metadataDir.createFolder();
            }
            catch (FileSystemException e) {
                throw new WGInitialDeployException("Could not create metadata folder '" + getRelativePath(metadataDir) + "'");
            }
        }
        FileObject metadataFile = metadataDir.resolveFile(localName + DesignDirectory.SUFFIX_METADATA);
        _log.info("Creating script metadata file " + getRelativePath(metadataFile));
        writer = createWriter(metadataFile);
        writer.write(_xstream.toXML(metaData.getInfo()));
        writer.close();

        return codeFile;
    }

    protected FileObject initialDeployFileContainer(WGFileContainer con) throws IOException, InstantiationException, IllegalAccessException, WGAPIException, WGDesignSyncException {

        // Create container folder
        FileObject containerFolder = getFilesFolder().resolveFile(con.getName());
        _log.info("Creating file container folder " + getRelativePath(containerFolder));
        try {
            containerFolder.createFolder();
        }
        catch (FileSystemException e) {
            throw new WGInitialDeployException("Could not create file container folder '" + containerFolder.getName().getPathDecoded() + "'", e);
        }

        // Create metadata file
        FCMetadata metaData = new FCMetadata(con);
        FileObject metadataFile = containerFolder.resolveFile(AbstractDesignFile.FILECONTAINER_METADATA_FILENAME + DesignDirectory.SUFFIX_METADATA);
        _log.info("Creating file container metadata file " + getRelativePath(metadataFile));
        Writer writer = createWriter(metadataFile);
        writer.write(_xstream.toXML(metaData.getInfo()));
        writer.close();

        // Create contained files
        Iterator fileNames = con.getFileNames().iterator();
        String fileName;
        FileObject file;
        while (fileNames.hasNext()) {
            fileName = (String) fileNames.next();
            InputStream in = con.getFileData(fileName);
            file = containerFolder.resolveFile(fileName);
            _log.info("Creating file container file " + getRelativePath(file));
            try {
                file.createFile();
            }
            catch (FileSystemException e) {
                throw new WGInitialDeployException("Could not create container file '" + getRelativePath(file) + "'", e);
            }
            OutputStream out = file.getContent().getOutputStream();
            WGUtils.inToOut(in, out, 2048);
            in.close();
            out.close();
        }

        return containerFolder;

    }

    protected FileObject initialDeployTMLModule(WGTMLModule mod) throws IOException, InstantiationException, IllegalAccessException, WGAPIException, WGDesignSyncException {

        // Find/create media key folder
        FileObject mediaKeyFolder = getTmlFolder().resolveFile(mod.getMediaKey());
        if (!mediaKeyFolder.exists()) {
            _log.info("Creating media key folder " + getRelativePath(mediaKeyFolder));
            try {
                mediaKeyFolder.createFolder();
            }
            catch (FileSystemException e) {
                throw new WGInitialDeployException("Could not create media key folder '" + mediaKeyFolder.getName().getPathDecoded() + "'");
            }
        }

        // Eventually create intermediate directories
        List<String> path = WGUtils.deserializeCollection(mod.getName(), ":", true);
        String localName = (String) path.get(path.size() - 1);
        FileObject currentDir = mediaKeyFolder;
        for (int i = 0; i < path.size() - 1; i++) {
            currentDir = currentDir.resolveFile((String) path.get(i));
            if (!currentDir.exists()) {
                _log.info("Creating tml category directory " + getRelativePath(currentDir));
                try {
                    currentDir.createFolder();
                }
                catch (FileSystemException e) {
                    throw new WGInitialDeployException("Could not create tml category folder '" + getRelativePath(currentDir) + "'", e);
                }
            }
            else if (!currentDir.getType().equals(FileType.FOLDER)) {
                throw new WGInitialDeployException("Cannot deploy " + mod.getDocumentKey() + " to sync folder because the directory name '" + path.get(i) + "' is already used by another file");

            }
        }

        // Create code file
        FileObject tmlCodeFile = currentDir.resolveFile(localName + DesignDirectory.SUFFIX_TML);
        _log.info("Creating tml module file " + getRelativePath(tmlCodeFile));
        try {
            tmlCodeFile.createFile();
        }
        catch (FileSystemException e) {
            throw new WGInitialDeployException("Could not create tml code file '" + getRelativePath(tmlCodeFile) + "'");
        }
        Writer writer = createWriter(tmlCodeFile);
        writer.write(mod.getCode());
        writer.close();

        // Create metadata file
        TMLMetadata metaData = new TMLMetadata(mod);
        FileObject metadataDir = currentDir.resolveFile(DesignDirectory.NAME_METADATADIR);
        if (!metadataDir.exists()) {
            _log.info("Creating tml metadata directory " + getRelativePath(metadataDir));
            try {
                metadataDir.createFolder();
            }
            catch (FileSystemException e) {
                throw new WGInitialDeployException("Could not create metadata folder '" + metadataDir.getName().getPathDecoded() + "'");
            }
        }

        FileObject metadataFile = metadataDir.resolveFile(localName + DesignDirectory.SUFFIX_METADATA);
        _log.info("Creating tml metadata file " + getRelativePath(metadataFile));
        writer = createWriter(metadataFile);
        writer.write(_xstream.toXML(metaData.getInfo()));
        writer.close();

        return tmlCodeFile;
    }

    protected String getRelativePath(FileObject file) throws FileSystemException, WGDesignSyncException {
        return getBaseFolder().getName().getRelativeName(file.getName());
    }

    public WGDatabase getDB() {
        return _db;
    }

    public static String determineDesignEncoding(DesignDefinition syncInfo, CSConfig csConfig) throws IOException, WGDesignSyncException, InvalidCSConfigVersionException {

        // Either encoding is set in syncinfo.xml, or syncinfo.xml points to
        // csconfig.xml
        if (syncInfo.getFileEncoding() == null) {
            return System.getProperty("file.encoding");
        }
        else if (syncInfo.getFileEncoding().equals(DesignDefinition.FILEENCODING_CSCONFIG_DEFINED)) {
            if (csConfig != null) {
                PublisherOption option = csConfig.findPublisherOption(WGACore.DBATTRIB_DESIGN_ENCODING);
                if (option != null) {
                    return option.getValue();
                }
                else {
                    return System.getProperty("file.encoding");
                }
            }
            else {
                return System.getProperty("file.encoding");
            }
        }

        else {
            return syncInfo.getFileEncoding();
        }

    }
    
    private void determineDesignEncoding() throws IOException, WGDesignSyncException, InvalidCSConfigVersionException {
        _fileEncoding = determineDesignEncoding(_syncInfo, _csconfig);
    }

    protected List<ModuleFile> getModuleFiles(int type, FileObject dir, String category) throws WGDesignSyncException, FileSystemException {
        return getModuleFiles(type, dir, "", category);

    }

    private List<ModuleFile> getModuleFiles(int type, FileObject dir, String prefix, String category) throws WGDesignSyncException, FileSystemException {

        // Find the valid suffixes for the type: Standard plus registered conversions
        Set<String> suffixes = new HashSet<String>();
        suffixes.add(getFileStandardSuffix(type, category));
        for (ModuleDefinition def : _core.getModuleRegistry().getModulesForType(DesignResourceConversionModuleType.class).values()) {
            DesignResourceConversionProperties props = (DesignResourceConversionProperties) def.getProperties();
            if (props.getDesignType() == type && (props.getCodeType() == null || props.getCodeType().equals(category))) {
                for (String sfx : props.getSuffixes()) {
                    suffixes.add("." + sfx);
                }
            }
        }
        
        List<ModuleFile> moduleFiles = new ArrayList<ModuleFile>();
        if (dir == null || !dir.exists()) {
            return moduleFiles;
        }

        FileObject[] files = dir.getChildren();
        if (files == null) {
            throw new WGDesignSyncException("Cannot collect files from directory '" + dir.getName().getPathDecoded() + "'. Please verify directory existence.");
        }

        FileObject file;
        for (int i = 0; i < files.length; i++) {
            file = files[i];
            if (!isValidDesignFile(file)) {
                continue;
            }
            if (file.getType().equals(FileType.FOLDER)) {
                if (file.getName().getBaseName().equals(DesignDirectory.NAME_METADATADIR)) {
                    continue;
                }
                String newPrefix = (prefix.trim().equals("") ? file.getName().getBaseName().toLowerCase() : prefix + DIRECTORY_DIVIDER + file.getName().getBaseName().toLowerCase());
                moduleFiles.addAll(getModuleFiles(type, file, newPrefix, category));
            }
            else {
                // Test the file suffix
                if (!suffixes.contains("." + file.getName().getExtension())) {
                    continue;
                }
                moduleFiles.add(new ModuleFile(file, prefix, category, type));
            }
        }

        return moduleFiles;

    }

    /**
     * @return Returns the tmlFolder.
     * @throws WGDesignSyncException
     */
    public FileObject getTmlFolder() throws WGDesignSyncException {
        if (_fsResources != null) {
            return _fsResources.getTmlFolder();
        }
        else {
            throw new WGDesignSyncException("The base folder was retrieved from an illegal code position");
        }
    }

    public boolean isValidDesignFile(FileObject file) {
        return isValidDesignFile(file, _core.getDesignFileValidator());
    }
    
    public static boolean isValidDesignFile(FileObject file, DesignFileValidator validator) {
        return !file.getName().getBaseName().startsWith(".") && validator.isValidFileName(file.getName().getBaseName());
    }

    public WGACore getWGACore() {
        return _core;
    }

    public DesignDefinition getSyncInfo() {
        return _syncInfo;
    }

    public String getFileEncoding() {
        return _fileEncoding;
    }

    public FileObject getBaseFolder() throws WGDesignSyncException {
        if (_fsResources != null) {
            return _fsResources.getBaseFolder();
        }
        else {
            throw new WGDesignSyncException("The base folder was retrieved from an illegal code position");
        }
    }

    protected DESEncrypter getCipher() {
        return _cipher;
    }

    public String getDesignPath() {
        return _designPath;
    }

    public Logger getLog() {
        return _log;
    }

    public boolean isDirectAccessDefault() {
        return _directAccessDefault;
    }

    public Set<Integer> getSyncedDoctypes() {
        return _syncedDoctypes;
    }

    public void databaseConnected(WGDatabaseEvent event) {

        try {
            fetchFileSystem(_core);
            try {
                init();
            }
            catch (Exception e) {
                _log.error("Error initializing file system design", e);
            }

        }
        catch (Exception e) {
            _log.error("Error fetching file system for WGA design of lazily connected db", e);
        }
    }

    public void databaseConnectionError(WGDatabaseEvent event) {
    }

    protected List<ModuleFile> getTMLModuleFiles() throws FileSystemException, WGDesignSyncException {
        // Iterate thru media key folders
        FileObject[] mediaKeyFolders = getTmlFolder().getChildren();
        if (mediaKeyFolders == null) {
            throw new WGDesignSyncException("Cannot collect modules from directory '" + getTmlFolder().getName().getPathDecoded() + "'. Please verify directory existence.");
        }

        FileObject mediaKeyFolder;
        List<ModuleFile> files = new ArrayList();
        for (int mediaKeyIdx = 0; mediaKeyIdx < mediaKeyFolders.length; mediaKeyIdx++) {
            mediaKeyFolder = mediaKeyFolders[mediaKeyIdx];
            if (!mediaKeyFolder.getType().equals(FileType.FOLDER)) {
                continue;
            }
            if (!isValidDesignFile(mediaKeyFolder)) {
                continue;
            }
            String mediaKey = mediaKeyFolder.getName().getBaseName().toLowerCase();
            files.addAll(getModuleFiles(WGDocument.TYPE_TML, mediaKeyFolder, mediaKey));
        }
        return files;
    }

    protected List<ModuleFile> getScriptModuleFiles() throws WGDesignSyncException, FileSystemException {
        List<ModuleFile> files = new ArrayList();

        // Get all files from script folders
        Iterator scriptInfos = DesignDirectory.getScriptTypes().values().iterator();
        while (scriptInfos.hasNext()) {
            ScriptInformation info = (ScriptInformation) scriptInfos.next();
            files.addAll(getModuleFiles(WGDocument.TYPE_CSSJS, getScriptTypeFolder(info.getFolder()), info.getType()));

        }
        return files;
    }

    protected List<ModuleFile> getFileContainerFiles() throws FileSystemException, WGDesignSyncException {
        FileObject filesFolder = getFilesFolder();
        List<ModuleFile> designFiles = getFileContainerFiles(filesFolder, "");
        
        return designFiles;
    }

    private List<ModuleFile> getFileContainerFiles(FileObject filesFolder, String prefix) throws FileSystemException {
        FileObject[] files = filesFolder.getChildren();
        List<ModuleFile> designFiles = new ArrayList<ModuleFile>();
        for (int i = 0; i < files.length; i++) {
            FileObject file = files[i];
            if (file.getType().equals(FileType.FOLDER) && isValidDesignFile(file)) {
                designFiles.add(new ModuleFile(file, prefix, "", WGDocument.TYPE_FILECONTAINER));
                String newPrefix = (prefix.trim().equals("") ? file.getName().getBaseName().toLowerCase() : prefix + DIRECTORY_DIVIDER + file.getName().getBaseName().toLowerCase());
                designFiles.addAll(getFileContainerFiles(file, newPrefix));
            }
        }
        return designFiles;
    }

    protected ModuleFile getFileContainerFile(String name) throws FileSystemException, WGDesignSyncException {
        
        FileObject folder = getFilesFolder();
        List<String> paths = WGUtils.deserializeCollection(name, ":");
        boolean found = false;
        
        for (String path : paths) {
            FileObject fileContainer = findMatchingChild(folder, path, Collections.singleton(""), true);
            if (fileContainer != null && fileContainer.getType().equals(FileType.FOLDER) && isValidDesignFile(fileContainer)) {
                found = true;
                folder = fileContainer;
            }
            else {
                found = false;
                break;
            }
        }
        
        if (found == true) {
            return new ModuleFile(folder, WGUtils.serializeCollection(paths.subList(0, paths.size() - 1), ":"),  "", WGDocument.TYPE_FILECONTAINER);
        }
        else {
            return null;
        }
        
        
    }

    protected ModuleFile getScriptModuleFile(String name, String type) throws FileSystemException, WGDesignSyncException {

        ScriptInformation info = DesignDirectory.getScriptInformation(type);
        if (info == null) {
            return null;
        }
        
        FileObject scriptTypeFolder = getScriptTypeFolder(info.getFolder());
        if (scriptTypeFolder == null) {
            return null;
        }
        
        return resolveModuleFile(scriptTypeFolder, name, info.getFolder(), WGDocument.TYPE_CSSJS);

    }

    protected ModuleFile getTMLModuleFile(String name, String strMediaKey) throws FileSystemException, WGDesignSyncException {
        return resolveModuleFile(getTmlFolder().resolveFile(strMediaKey.toLowerCase()), name, strMediaKey, WGDocument.TYPE_TML);

    }

    private ModuleFile resolveModuleFile(FileObject folder, String name, String category, int type) throws FileSystemException {

        if (folder == null || !folder.exists() || !folder.getType().equals(FileType.FOLDER)) {
            return null;
        }

        String prefix;
        String baseName;
        String fullName;
        String suffix = getFileStandardSuffix(type, category);
        int lastColon = name.lastIndexOf(":");
        if (lastColon != -1) {
            prefix = name.substring(0, lastColon);
            baseName = name.substring(lastColon + 1);
            fullName = baseName + suffix;
        }
        else {
            prefix = "";
            baseName = name;
            fullName = name + suffix;
        }

        // Find correct folder
        if (!prefix.equals("")) {
            List<String> path = WGUtils.deserializeCollection(prefix, ":");
            Iterator<String> elems = path.iterator();
            while (elems.hasNext()) {
                String elem = (String) elems.next();
                FileObject matchingChild = findMatchingChild(folder, elem, Collections.singleton(""), true);

                // Use matching child as next folder
                if (matchingChild != null && matchingChild.getType().equals(FileType.FOLDER)) {
                    folder = matchingChild;
                }

                // Exit if we did not find a matching folder
                else {
                    return null;
                }

            }
        }

        // Find the valid suffixes for the type: Standard plus registered conversions
        Set<String> suffixes = new HashSet<String>();
        suffixes.add(getFileStandardSuffix(type, category));
        for (ModuleDefinition def : _core.getModuleRegistry().getModulesForType(DesignResourceConversionModuleType.class).values()) {
            DesignResourceConversionProperties props = (DesignResourceConversionProperties) def.getProperties();
            if (props.getDesignType() == type && (props.getCodeType() == null || props.getCodeType().equals(category))) {
                for (String sfx : props.getSuffixes()) {
                    suffixes.add("." + sfx);
                }
            }
        }
        
        // Find the file
        FileObject file = findMatchingChild(folder, baseName, suffixes, false);
        if (file != null) {
            return new ModuleFile(file, prefix, category, type);
        }
        else {
            return null;
        }

    }

    public String getFileStandardSuffix(int type, String category) {
        return type == WGDocument.TYPE_TML ? DesignDirectory.SUFFIX_TML : DesignDirectory.getScriptInformation(category).getSuffix();
    }


    private FileObject findMatchingChild(FileObject folder, String baseName, Set<String> suffixes, boolean exactMatch) throws FileSystemException {
        FileObject matchingChild = null;
        
        // First try fast straight fetch using lowercase file
        for (String suffix : suffixes) {
            FileObject straightChild = folder.resolveFile(baseName.toLowerCase() + suffix.toLowerCase());
            if (straightChild.exists()) {
                return straightChild;
            }
        }
        
        // Secondly we iterate over all children and to a case-insensitive
        // equals
        FileObject[] children = folder.getChildren();

        for (String suffix : suffixes) {
            for (FileObject child : children) {
                if (exactMatch) {
                    if (child.getName().getBaseName().equalsIgnoreCase(baseName + suffix)) {
                        matchingChild = child;
                        break;
                    }
                }
                else {
                    String fileName = child.getName().getBaseName().toLowerCase();
                    String elementName = baseName.toLowerCase() + suffix.toLowerCase();
                    if (fileName.startsWith(elementName) && fileName.indexOf(".", elementName.length()) == -1) {
                        matchingChild = child;
                        break;
                    }
                }
            }
        }
        
        return matchingChild;
    }

    protected static String getLOGGER_DESIGNSYNC() {
        return LOGGER_DESIGNSYNC;
    }

    protected static String getSYNCSTATUS_MODULE() {
        return SYNCSTATUS_MODULE;
    }

    protected static String getDIRECTORY_DIVIDER() {
        return DIRECTORY_DIVIDER;
    }

    protected static Map<Integer, DesignInformation> getDesignTypes() {
        return _designTypes;
    }

    protected static XStream getXstream() {
        return _xstream;
    }

    protected Pattern getValidDesignName() {
        return _validDesignName;
    }

    protected WGDatabase getDb() {
        return _db;
    }

    protected String getDesignKey() {
        return _designKey;
    }

    protected WGACore getCore() {
        return _core;
    }

    protected StandardFileSystemManager getFsManager() {
        return _fsManager;
    }

    protected static String getLOGGER_DESIGNSYNC_QUIET() {
        return LOGGER_DESIGNSYNC_QUIET;
    }

    protected static String getMODE_FULL() {
        return MODE_FULL;
    }

    protected static String getMODE_VIRTUAL() {
        return MODE_VIRTUAL;
    }

    protected static String getMODE_DIRECT() {
        return MODE_DIRECT;
    }

    protected FileSystemResources getFsResources() {
        return _fsResources;
    }

    protected CSConfig getCsconfig() {
        return _csconfig;
    }

    protected Map<String, String> getDesignOptions() {
        return _designOptions;
    }

    public String getJavaClassesPath() {
        return _javaClassesPath;
    }

    public int designHashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_designPath == null) ? 0 : _designPath.hashCode());
        result = prime * result + ((_fileEncoding == null) ? 0 : _fileEncoding.hashCode());
        return result;
    }



}
