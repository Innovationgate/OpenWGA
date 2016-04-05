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
package de.innovationgate.wgpublisher.design.sync;

import java.io.IOException;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.commons.vfs2.CacheStrategy;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.log4j.Logger;

import com.thoughtworks.xstream.XStream;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.locking.Lock;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.DesignDirectory.ScriptInformation;
import de.innovationgate.wga.common.beans.DesignDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wga.model.FCMetadataInfo;
import de.innovationgate.wga.model.ScriptMetadataInfo;
import de.innovationgate.wga.model.TMLMetadataInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;

public class DesignSyncManager extends FileSystemDesignManager {

    public static final String OPTION_AUTOUPDATE = "autoupdate";

    public class PollingTask extends TimerTask {

        public void run() {
            
            synchronized (DesignSyncManager.this) {
            
                if (_inactiveLoops > 1) {
                    _inactiveLoops--;
                    return;
                }
                
                try {
                    _db.openSession();
                    _db.getSessionContext().setTask("WGA Design Synchronisation");
                    Thread.currentThread().setName("Design Synchronisation DB " + _db.getDbReference());
                    
                    // If the db is currently locked design sync would most likely fail
                    if (_db.getLockStatus() != Lock.NOT_LOCKED) {
                        return;
                    }
                    
                    // Put deployments that need to be updated in here
                    Set<DesignDeployment> deploymentsToUpdate = new HashSet<DesignDeployment>();
                    
                    // Fetch file system
                    fetchFileSystem(_core);
                    
                    // Put deployments still in file system here. Will determine deleted deployments by comparing with old deployments
                    DesignSyncStatus currentDeployments = new DesignSyncStatus(DesignSyncManager.this, getBaseFolder().getURL().toString());
                    
                    // Test files for updates
                    
                    if (_syncedDoctypes.contains(new Integer(WGDocument.TYPE_CSSJS))) {
                        checkScriptDeployments(deploymentsToUpdate, currentDeployments);
                    }
                    
                    if (_syncedDoctypes.contains(new Integer(WGDocument.TYPE_FILECONTAINER))) {
                        checkFileContainerDeployments(deploymentsToUpdate, currentDeployments);
                    }
                    
                    if (_syncedDoctypes.contains(new Integer(WGDocument.TYPE_TML))) {
                        checkTMLDeployments(deploymentsToUpdate, currentDeployments);
                    }
                    
                    // Perform updates
                    boolean somethingChanged = false;
                    Iterator<DesignDeployment> updatesIt = deploymentsToUpdate.iterator();
                    DesignDeployment deployment;
                    while (updatesIt.hasNext()) {
                        deployment = updatesIt.next();
                        _log.info("DB:" + _db.getDbReference() +  " - Updating " + deployment.getDocumentKey() + " from file system");
                        somethingChanged = true;
                        try {
                            deployment.performUpdate(_db);
                        }
                        catch (Exception e) {
                            _log.error("Error updating " + deployment.getDocumentKey() + " of db " + _db.getDbReference() + " from file system", e);
                            if (deployment.addFailure() == true) {
                                _log.error("Cancelling update of " + deployment.getDocumentKey() + " of db " + _db.getDbReference() + " from file system after 5 failures");
                                _log.error("Modify the deployment file again to re-trigger it's update: " + deployment.getCodeFile().getName().getPath());
                                deployment.resetUpdateInformation();
                            }
                        }
                    }
                    
                    // Find deleted deployments
                    Iterator deleted = _syncStatus.findDeletedDeployments(currentDeployments).iterator();
                    while (deleted.hasNext()) {
                        DesignDeployment deletedDeployment = (DesignDeployment) deleted.next();
                        _log.info("DB:" + _db.getDbReference() +  " - Deleting " + deletedDeployment.getDocumentKey() + " because it was deleted in file system");
                        somethingChanged = true;
                        try {
                            deletedDeployment.performDeletion(_db);
                        }
                        catch (RuntimeException e) {
                            _log.error("Error deleting " + deletedDeployment.getDocumentKey() + " of db " + _db.getDbReference() + " from file system", e);
                        }
                    }
                    
                    // Replace old deployments with new deployments and write then to disk
                    _syncStatus = currentDeployments;
                    if (somethingChanged) {
                        _lastModificationTime = System.currentTimeMillis();
                        storeSyncStatus();
                    }
                    
                    // Manage throttling
                    if (_core.getWgaConfiguration().getDesignConfiguration().isThrottlingEnabled()) {
                        if (somethingChanged && _throttled == true) {
                            _log.info("Stopped throttling design synchronisation of database '" + _db.getDbReference() + "' after new modification");
                            _throttled = false;
                            
                        }
                        else if (!somethingChanged && _throttled == false) {
                             if (_lastModificationTime + (1000 * 60 * _core.getWgaConfiguration().getDesignConfiguration().getThrottlingPeriodMinutes()) < System.currentTimeMillis()) {
                                 _log.info("Throttling design synchronisation of database '" + _db.getDbReference() + "' after " + _core.getWgaConfiguration().getDesignConfiguration().getThrottlingPeriodMinutes() + " minutes of inactivity");
                                 _throttled = true;
                             }
                        }
                    }
                    
                    if (_throttled) {
                        _inactiveLoops = 10;
                    }
                    _lastRun = new Date();
                }
                catch (WGDesignSyncException e) {
                    _log.error("Design sync encountered an error and will resume synchronisation on next run:");
                    if (e.getCause() != null) {
                        _log.error(e);
                    }
                    else {
                        _log.error(e.getMessage());
                    }
                }
                catch (Exception e) {
                    _log.error("Error synchronizing design for database " + _db.getDbReference(), e);
                }
                finally {
                    WGFactory.getInstance().closeSessions();
                    closeFileSystem();
                }
            }
        }
        
    }
    
    public static final String SYSPROPERTY_AUTOUPDATE_DISABLE = "de.innovationgate.wga.designsync.disable_autoupdate";
    
    public static final boolean AUTOUPDATE_GLOBALLY_DISABLED = Boolean.valueOf(System.getProperty(SYSPROPERTY_AUTOUPDATE_DISABLE)).booleanValue();
  
    static {
        // Add aliases for metadata classes to xtream
        _xstream.alias(TMLMetadataInfo.XSTREAM_ALIAS, TMLMetadataInfo.class);
        _xstream.alias(ScriptMetadataInfo.XSTREAM_ALIAS, ScriptMetadataInfo.class);
        _xstream.alias(FCMetadataInfo.XSTREAM_ALIAS, FCMetadataInfo.class);
        _xstream.alias("DesignSyncStatus", DesignSyncStatus.class);
        _xstream.alias("DesignSyncInfo", DesignDefinition.class);
        _xstream.alias("TMLDeployment", TMLDeployment.class);
        _xstream.alias("FileContainerDeployment", FileContainerDeployment.class);
        _xstream.alias("ScriptDeployment", ScriptDeployment.class);
        _xstream.alias("ContainerFile", FileContainerDeployment.ContainerFile.class);
        
        if (AUTOUPDATE_GLOBALLY_DISABLED) {
            Logger.getLogger("wga.designsync").warn("Automatic update for design synchronisation is globally disabled for this WGA runtime");
        }

    }
    
    // Status information for throtting feature
    private boolean _throttled = false;
    private int _inactiveLoops = 0;


    private Date _lastRun = new Date();
    private PollingTask _pollingTask;
    private Timer _timer;
    protected DesignSyncStatus _syncStatus;
    protected boolean _autoUpdate;
    protected long _lastModificationTime = Long.MIN_VALUE;
    private DesignReference _designReference;
    public DesignSyncManager(DesignReference ref, WGACore core, WGDatabase db, String path, Map<String,String> options) throws WGDesignSyncException, IOException, InstantiationException, IllegalAccessException, WGAPIException, InvalidCSConfigVersionException {
        super(core, db, path, options);
        _autoUpdate = WGUtils.getBooleanMapValue(_designOptions, OPTION_AUTOUPDATE, true);
        _designReference = ref;
        if (_db.isConnected()) {
            prepareSync(true);
        }
        closeFileSystem();
    }

    private void checkTMLDeployments(Set<DesignDeployment> deploymentsToUpdate, DesignSyncStatus currentDeployments) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        if (!getTmlFolder().exists()) {
            return;
        }
        
        List<ModuleFile> files = getTMLModuleFiles();
        
        Iterator<ModuleFile> filesIt = files.iterator(); 
        while (filesIt.hasNext()) {
            checkTMLDeployment(deploymentsToUpdate, currentDeployments, filesIt.next());
        }
        
        
    }

    private void checkFileContainerDeployments(Set<DesignDeployment> deploymentsToUpdate, DesignSyncStatus currentDeployments) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        if (!getFilesFolder().exists()) {
            return;
        }
        
        Iterator<ModuleFile> files = getFileContainerFiles().iterator();
        if (files == null) {
            throw new WGDesignSyncException("Cannot collect files from directory '" + getFilesFolder().getName().getPathDecoded() + "'. Please verify directory existence.");
        }
        
        
        while (files.hasNext()) {
            ModuleFile file = files.next();
            checkFCDeployment(deploymentsToUpdate, currentDeployments, file);
        }
        
    }

    private void checkScriptDeployments(Set<DesignDeployment> deploymentsToUpdate, DesignSyncStatus currentDeployments) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        if (!getScriptFolder().exists()) {
            return;
        }
        
        List<ModuleFile> files = getScriptModuleFiles();

        // Check all files
        Iterator<ModuleFile> filesIt = files.iterator();
        while(filesIt.hasNext()) {
            checkScriptDeployment(deploymentsToUpdate, currentDeployments, filesIt.next());
        }
        
        
    }

    private void checkTMLDeployment(Set<DesignDeployment> deploymentsToUpdate, DesignSyncStatus currentDeployments, ModuleFile file) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
               
        String fileName = file.getFile().getName().getBaseName().toLowerCase();
        String fileSuffix = null;
        
        // Determine suffix
        if (fileName.endsWith(DesignDirectory.SUFFIX_TML) || fileName.indexOf(DesignDirectory.SUFFIX_TML + "-") != -1) {
            fileSuffix = DesignDirectory.SUFFIX_TML;
        }
        else if (fileName.endsWith(DesignDirectory.SUFFIX_METADATA)) {
            fileSuffix = DesignDirectory.SUFFIX_METADATA;
        }
        else {
            // Won't process files of unknown suffix as simple TML modules
            return;
        }
        
        // Determine designName and see if there is an deployment
        String designDocumentKey = WGDesignDocument.buildDesignDocumentKey(WGDocument.TYPE_TML, file.getModuleName(), file.getCategory());
        DesignDeployment deployment = (DesignDeployment) _syncStatus.getTmlDeployments().get(designDocumentKey);
        if (deployment != null && !deployment.isDeleted()) {
            currentDeployments.putDeployment(designDocumentKey, deployment);
            if (deployment.isUpdated()) {
                deploymentsToUpdate.add(deployment);
            }
        }
        else {
            
            // We won't create an new deployment for a metadata file only. A code file must be present
            if (fileSuffix == DesignDirectory.SUFFIX_METADATA) {
                return;
            }
            
            // Create a new deployment
            deployment = new TMLDeployment(_syncStatus, designDocumentKey, file.getFile());
            currentDeployments.putDeployment(designDocumentKey, deployment);
            deploymentsToUpdate.add(deployment);
        }
        
    }

    private void checkFCDeployment(Set<DesignDeployment> deploymentsToUpdate, DesignSyncStatus currentDeployments, ModuleFile file) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
           
        if (!file.getFile().getType().equals(FileType.FOLDER)) {
            return;
        }
        
        // Determine designName and see if there is an deployment
        String designDocumentKey = WGDesignDocument.buildDesignDocumentKey(WGDocument.TYPE_FILECONTAINER, file.getFile().getName().getBaseName(), null);
        DesignDeployment deployment = (DesignDeployment) _syncStatus.getFileContainerDeployments().get(designDocumentKey);
        if (deployment != null && !deployment.isDeleted()) {
            currentDeployments.putDeployment(designDocumentKey, deployment);
            try {
                if (deployment.isUpdated()) {
                    deploymentsToUpdate.add(deployment);
                }
            }
            catch (FileSystemException e) {
                _log.error("Error determining update for file container deployment " + deployment.getDocumentKey(), e);
            }
        }
        else {
            // Create a new deployment
            deployment = new FileContainerDeployment(_syncStatus, designDocumentKey, file.getFile());
            currentDeployments.putDeployment(designDocumentKey, deployment);
            deploymentsToUpdate.add(deployment);
        }
        
    }

    private void checkScriptDeployment(Set<DesignDeployment> deploymentsToUpdate, DesignSyncStatus currentDeployments, ModuleFile file) throws InstantiationException, IllegalAccessException, IOException, WGDesignSyncException {
        
        String fileName = file.getFile().getName().getBaseName().toLowerCase();
        String fileSuffix = "." + file.getFile().getName().getExtension();
        
        ScriptInformation info = DesignDirectory.getScriptInformationBySuffix(fileSuffix);
        if (info == null && !fileName.endsWith(DesignDirectory.SUFFIX_METADATA)) {
            // Won't process files of unknown suffix
            return;
        }
        
        // Filter out scripts that could collide with metadata module names
        if (info != null && info.getType().equals(WGCSSJSModule.CODETYPE_XML)) {
            if (fileName.startsWith(WGCSSJSModule.METADATA_MODULE_QUALIFIER)) {
                return;
            }
        }
        
        // Determine designName and see if there is an deployment
        String designDocumentKey = WGDesignDocument.buildDesignDocumentKey(WGDocument.TYPE_CSSJS, file.getModuleName(), file.getCategory());
        ScriptDeployment deployment = (ScriptDeployment) _syncStatus.getScriptDeployments().get(designDocumentKey);
        if (deployment != null && !deployment.isDeleted()) {
            currentDeployments.putDeployment(designDocumentKey, deployment);            
            if (deployment.isUpdated()) {
                deploymentsToUpdate.add(deployment);
                deployment.setWarnedAboutDuplicate(false);
            }
            
            checkForScriptDuplicate(file, info, deployment);

        }
        else {
            
            // We won't create an new deployment for a metadata file only. A code file must be present
            if (info == null) {
                return;
            }
            
            // Create a new deployment
            deployment = new ScriptDeployment(_syncStatus, designDocumentKey, file.getFile(), info.getType());
            ScriptDeployment oldDeployment = (ScriptDeployment) currentDeployments.putDeployment(designDocumentKey, deployment);
            if (oldDeployment != null) {
                checkForScriptDuplicate(file, info, oldDeployment);
            }
            deploymentsToUpdate.add(deployment);
        }
        
    }

    private void checkForScriptDuplicate(ModuleFile file, ScriptInformation info, ScriptDeployment deployment) {
        try {
            // Test if the existing deployment eventually differs in script type - We might have a duplicate script name
            if (!deployment.getCodeType().equals(info.getType()) && getDB().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5 && !deployment.isWarnedAboutDuplicate()) {
                _log.warn("There are multiple script files named '" + file.getModuleName() + "' with script types '" + deployment.getCodeType() + "' and '" + info.getType() + "' in design of db '" + _db.getDbReference() + "'.");
                _log.warn("WGA Content Stores of versions lower than 5 cannot store multiple scripts with equal names. Please use unique names for script files or use a newer content store version (Your current is " + getDB().getContentStoreVersion() + ")!");
                deployment.setWarnedAboutDuplicate(true);
            }
        }
        catch (WGAPIException e) {
            getLog().error("Exception checking for script duplicates", e);
        }
    }

    /**
     * @return Returns the xstream.
     */
    public static XStream getXstream() {
        return _xstream;
    }
    
    public boolean isTemporary() {
        return false;
    }

    public void scanForUpdates() {
        
        Thread thread = new Thread(new PollingTask());
        thread.start();
        try {
            thread.join();
        }
        catch (InterruptedException e) {
            e.printStackTrace();
        }
        
    }

    public void waitForNextSync() {
        
        Date lastRun = _lastRun;
        while (lastRun.equals(_lastRun)) {
            
            try {
                Thread.sleep(100);
            }
            catch (InterruptedException e) {
            }
            
        }
        
    }


    @Override
    protected boolean init() throws WGAPIException, IOException, WGDesignSyncException, InstantiationException, IllegalAccessException, InvalidCSConfigVersionException {
        
        _lastModificationTime = System.currentTimeMillis();
        
        boolean initialDeploy = super.init();
        
        // Import sync status. If not present or does not belong to the same directory, initialize it
        if (!initialDeploy) {
            if (!importSyncStatus()) {
                initSyncStatus();
            }
        }
        
        return initialDeploy;
    }
    
    private void prepareSync(boolean doInitSync) throws WGAPIException, FileSystemException, IOException, WGDesignSyncException {

        // Sync mode
        String mode = getSyncMode();
        
        if (mode.equals(MODE_FULL)) {
            
            /*// Test for correct design key OBSOLETE
            if (!getSyncInfo().getDesignKey().equals(_designKey)) {
                throw new WGDesignKeyException(getBaseFolder().getURL().toString(), getSyncInfo().getDesignKey(), _designKey);
            }*/
            
            // Disable modification of the served doctypes
            Iterator<Integer> doctypes = _syncedDoctypes.iterator();
            while (doctypes.hasNext()) {
                Integer doctype = doctypes.next();
                _db.setDoctypeModifiable(doctype.intValue(), false);
            }
        }
        else if (mode.equals(MODE_VIRTUAL)) {
            // Register a virtual design provider for this database so that designs in the database do not get modified
            // Deployment information must be reset to do an inital update of the provider
            _log.info("Creating virtual design provider for database '" + _db.getDbReference() + "' to be filled with designs from file system");
            _db.setDesignProvider(new VirtualDesignProvider(_designReference, "Virtual design provider, receiving designs from directory " + getBaseFolder().getURL().toString(), _db, WGFactory.getTempDir()));
            initSyncStatus(); 
        }
        else {
            throw new WGDesignSyncException("Unknown sync mode: " + mode);
        }
        
        // Do initial sync right now and wait for it:
        // This must not be executed when init() was triggered by an event, bc. synchronisations
        // may lead to a deadlock
        if (doInitSync) {
            Thread initSyncThread = new Thread(new PollingTask());
            initSyncThread.start();
            try {
                initSyncThread.join();
            }
            catch (InterruptedException e) {
            }
        }
        
        // If autoupdate enabled start the update task
        if (_autoUpdate && !AUTOUPDATE_GLOBALLY_DISABLED) {
            _pollingTask = new PollingTask();
            _timer = new Timer();
            int pollingInterval = _core.getWgaConfiguration().getDesignConfiguration().getPollingInterval() * 1000;
            _timer.schedule(_pollingTask, pollingInterval, pollingInterval);
        }
    }

    private String getSyncMode() {
        String mode = getDesignOptions().get("syncmode");
        if (mode == null) {
            mode = MODE_FULL;
        }
        return mode;
    }

    @Override
    public void close() {
        
        if (_timer != null) {
            _timer.cancel();
        }
        
        super.close();
    }

    private void initSyncStatus() throws FileSystemException, WGDesignSyncException {
        _syncStatus = new DesignSyncStatus(this, getBaseFolder().getURL().toString());
    }

    private boolean importSyncStatus() throws IOException, WGAPIException, WGDesignSyncException {
        
        WGCSSJSModule mod = _db.getMetadataModule(SYNCSTATUS_MODULE);
        if (mod == null) {
            return false;
        }
         
        _syncStatus = (DesignSyncStatus) _xstream.fromXML(mod.getCode());
        _syncStatus.setManager(this);
        if (_syncStatus.getBasePath().equals(getBaseFolder().getURL().toString())) {
            return true;
        }
        else {
            return false;
        }
    
    }

    @Override
    protected void doInitialDeployment(String designKey) throws WGAPIException, IOException, InstantiationException, IllegalAccessException, WGDesignSyncException {
        initSyncStatus();
        super.doInitialDeployment(designKey);
        storeSyncStatus();
    }

    protected void storeSyncStatus() throws IOException, WGAPIException {
        
        if (!getSyncMode().equals(MODE_FULL)) {
            return;
        }
        
        WGCSSJSModule mod = _db.getMetadataModule(SYNCSTATUS_MODULE);
        if (mod == null) {
            mod = _db.createMetadataModule(SYNCSTATUS_MODULE);
            mod.setDescription("Module used internally by WGA to store the current status of design synchronisation.");
        }     
    
        mod.setCode(_xstream.toXML(_syncStatus));
        mod.save();
         
    }

    @Override
    protected FileObject initialDeployFileContainer(WGFileContainer con) throws IOException, InstantiationException, IllegalAccessException, WGAPIException, WGDesignSyncException {
        FileObject containerFolder = super.initialDeployFileContainer(con);
        
        // Create deployment
        if (containerFolder != null) {
            DesignDeployment deployment = new FileContainerDeployment(_syncStatus, con.getDocumentKey(), containerFolder);
            deployment.resetUpdateInformation();
            _syncStatus.putDeployment(con.getDocumentKey(), deployment);
        }
        
        return containerFolder;
    }

    @Override
    protected FileObject initialDeployScriptModule(WGCSSJSModule script) throws IOException, InstantiationException, IllegalAccessException, WGAPIException, WGDesignSyncException {
        FileObject codeFile = super.initialDeployScriptModule(script);
        
        // Create deployment object
        if (codeFile != null) {
            ScriptInformation info = DesignDirectory.getScriptInformation(script.getCodeType());
            DesignDeployment deployment = new ScriptDeployment(_syncStatus, script.getDocumentKey(), codeFile, info.getType());
            deployment.resetUpdateInformation();
            _syncStatus.putDeployment(script.getDocumentKey(), deployment);
        }
        
        return codeFile;

    }

    @Override
    protected FileObject initialDeployTMLModule(WGTMLModule mod) throws IOException, InstantiationException, IllegalAccessException, WGAPIException, WGDesignSyncException {
        FileObject tmlCodeFile = super.initialDeployTMLModule(mod);
        
        // Create deployment
        if (tmlCodeFile != null) {
            DesignDeployment deployment = new TMLDeployment(_syncStatus, mod.getDocumentKey(), tmlCodeFile);
            deployment.resetUpdateInformation();
            _syncStatus.putDeployment(mod.getDocumentKey(), deployment);
        }
        
        return tmlCodeFile;
    }

    @Override
    protected CacheStrategy getVFSCacheStrategy() {
        return CacheStrategy.MANUAL;
    }

    @Override
    public void databaseConnected(WGDatabaseEvent event) {

        try {
            fetchFileSystem(_core);
            try {
                init();
                prepareSync(false);
            }
            catch (Exception e) {
                _log.error("Error initializing design sync manager", e);
            }
            finally {
                closeFileSystem();
            }
        }
        catch (Exception e) {
            _log.error("Error fetching file system for design sync manager of lazily connected db", e);
        }
    }
}
