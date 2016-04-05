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
/**
 * Represents a plugin file and offers operations to deal with it
 */
package de.innovationgate.wgpublisher.plugins;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.log4j.Logger;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.common.beans.DesignDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.InvalidCSConfigVersionException;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wgpublisher.SystemContainerManager;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.OverlayData;
import de.innovationgate.wgpublisher.plugins.WGAPluginSet.RuntimeContext;

public class WGAPlugin {
    
    public static final int UPDATESTATUS_NEW = 0;
    public static final int UPDATESTATUS_UPDATE = 1;
    public static final int UPDATESTATUS_INSTALL_PARALLEL = 2;
    public static final int UPDATESTATUS_INSTALL_DEACTIVATED = 1;
    public static final int UPDATESTATUS_UPDATE_IDENTICAL = 3;
    
    public static Set<String> PLATFORM_PLUGINS = new HashSet<String>();
    
    static {
        PLATFORM_PLUGINS.add("de.innovationgate.admin");
        PLATFORM_PLUGINS.add("de.innovationgate.app-ui-3-1");
        PLATFORM_PLUGINS.add("de.innovationgate.contentmanager");
        PLATFORM_PLUGINS.add("de.innovationgate.csmaintenance");
        PLATFORM_PLUGINS.add("de.innovationgate.Management");
        PLATFORM_PLUGINS.add("de.innovationgate.wga-app-framework");
        PLATFORM_PLUGINS.add("de.innovationgate.wgaservices-xfire");
    }
    
    public static class Configuration {
        
        private DesignDefinition _syncInfo;
        private CSConfig _csConfig;
        private String _licenseText;
        private OverlayData _overlayData;

        // Default constructor for serialisation
        private Configuration() {
        }
        
        public Configuration(DesignDefinition syncInfo, CSConfig csConfig, String licenseText, OverlayData overlayData) {
            super();
            _syncInfo = syncInfo;
            _csConfig = csConfig;
            _licenseText = licenseText;
            _overlayData = overlayData;
        }
        public CSConfig getCsConfig() {
            return _csConfig;
        }
        public DesignDefinition getSyncInfo() {
            return _syncInfo;
        }

        public String getLicenseText() {
            return _licenseText;
        }

        public OverlayData getOverlayData() {
            return _overlayData;
        }

    }
    
    public class InstallationFault {

        public static final int ERROR_WRONG_WGA_VERSION = 1;
        public static final int ERROR_WRONG_JAVA_VERSION = 2;
        
        public static final int ERROR_DEPENDENCY_NOT_AVAILABLE = 3;
        public static final int ERROR_DEPENDENCY_WRONG_VERSION = 4;
        public static final int ERROR_DEPENDENCY_INACTIVE = 5;
        public static final int ERROR_CIRCULAR_REFERENCE = 6;
        
        public static final int ERROR_INITIALISATION = 7;
        
        private int _error;

        // Default constructor for serialisation
        private InstallationFault() {
        }
        
        public InstallationFault(int error) {
            _error = error;
        }

        public String toErrorMessage() {
            if (_error == ERROR_WRONG_JAVA_VERSION) {
                return "This plugin needs at least Java Version " + getCsConfig().getPluginConfig().getMinimumJavaVersion().toString();
            }
            else if (_error == ERROR_WRONG_WGA_VERSION) {
                return "This plugin needs at least WGA Version " + getCsConfig().getPluginConfig().getMinimumWGAVersion().toString();
            }
            else {
                return "Unknown error code " + _error;
            }
            
        }

        public int getError() {
            return _error;
        }
        
        @Override
        public String toString() {
            return toErrorMessage();
        }
        
    }
    
    public class DependencyFault extends InstallationFault {
        
        public String toErrorMessage() {
           
            if (getError() == ERROR_DEPENDENCY_NOT_AVAILABLE) {
                return "This plugin depends on another WGA Plugin " + _depPluginID.getUniqueName() + " " + _depPluginID.getVersion().toString() + " which is either not installed or not active.";
            }
            else if (getError() == ERROR_DEPENDENCY_WRONG_VERSION) {
                return "This plugin depends on another WGA Plugin " + _depPluginID.getUniqueName() + " " + _depPluginID.getVersion().toString() + " which has a too low version.";
            }
            else if (getError() == ERROR_DEPENDENCY_INACTIVE) {
                return "This plugin depends on another WGA Plugin " + _depPluginID.getUniqueName() + " " + _depPluginID.getVersion().toString() + " which is inactive.";
            }
            else if (getError() == ERROR_CIRCULAR_REFERENCE) {
                return "This plugin causes a circular reference bc. it's dependency tree contains itself. WGA therefor cannot determine a correct order to connect plugins.";
            }
            else {
                return super.toErrorMessage();
            }
            
        }

        private PluginID _depPluginID;

        // Default constructor for serialisation
        private DependencyFault() {
        }
        
        public DependencyFault(PluginID pluginID, int error) {
            super(error);
            _depPluginID = pluginID;
        }
        
    }
    
    public class InitialisationFault extends InstallationFault {

        private String _msg;

        public InitialisationFault(String msg) {
            super(InstallationFault.ERROR_INITIALISATION);
            _msg = msg;
        }
        
        // Default constructor for serialisation
        private InitialisationFault() {
        }

        public String toErrorMessage() {
           return "Unable to initialize plugin: " + _msg;
        }

    }
    

    // Persistent fields
    private WGAPluginSet _parent = null;
    private boolean _active = true;
    private boolean _defaultPlugin = false;
    private boolean _reconnectDatabase = false;
    private String _filePath;
    private String _installationKey;
    // May not be transient bc. it distinguishes the plugin in a HashSet
    private PluginID _pluginID;
    
    private transient Map _mandatoryPlugins;
    private transient Map _dependentPlugins;
    private transient boolean _valid = true;
    
    private transient WorkspaceOperation _operation;
    private transient int _updateStatus = UPDATESTATUS_NEW;

    private transient long _fileLastModified;
    private transient List<InstallationFault> _installationFaults;
    private transient Configuration _config;
    private WGAPlugin() {
    }
    
    protected WGAPlugin(WGAPluginSet parent, String path) throws WGIllegalArgumentException, IOException, InvalidPluginException {
        _parent = parent;
        _filePath = path;
        init();
    }
    
    
    public File getPluginFile() {
        File file = getParent().getCore().getWGAFile(_filePath);
        
        // Backward compatibility to WGA4: find relative to plugins folder
        if (file == null || !file.exists()) {
            file = new File(getParent().getPluginsDir(), _filePath);
        }
        
        return file;
    }


    public void init() throws FileSystemException, IOException, InvalidPluginException {

        _valid = true;
        _mandatoryPlugins = new HashMap();
        _dependentPlugins = new HashMap();
        _installationFaults = new ArrayList();
        
        try {
            validate();
        }
        catch (InvalidPluginException e) {
            _installationFaults.add(new InitialisationFault(e.getMessage()));
            throw e;
        }
        
    }

    public void validate() throws InvalidPluginException {
        try {
            File file = getPluginFile();
            if (file == null || !file.exists()) {
                if (isDefaultPlugin()) {
                    throw new MissingDefaultPluginException(this);
                }
                else {
                    throw new InvalidPluginException(this, "The plugin file/directory does not exist (any more): " + _filePath);
                }
            }
            
            try {
            loadMetadata(file);
        }
            catch (Exception e) {
                if (isDefaultPlugin()) {
                    throw new MissingDefaultPluginException(this);
                }
                else {
                    throw new InvalidPluginException(this, "The plugin file/directory does not exist (any more): " + _filePath);
                }
            }
        }
        catch (InvalidPluginException e) {
            _valid = false;
           throw e;
        }
    }



    /**
     * Load all metadata about the plugin
     * - syncinfo.xml
     * - csconfig.xml
     * - File last modified time
     * @throws InvalidPluginException
     */
    private synchronized void loadMetadata(File file) throws InvalidPluginException {
        
        try {
            _fileLastModified = file.lastModified();
            _config = loadConfiguration(file, true);
            if (_config == null) {
                throw new InvalidPluginException(this, "This plugin does not contain a mandatory file: syncinfo.xml/design.xml or csconfig.xml");
            }
            
            if (_config.getCsConfig().getPluginConfig() == null) {
                throw new InvalidPluginException(this, "The plugin is invalid because its csconfig.xml does not contain a plugin configuration");
            }
            
            _pluginID = _config.getCsConfig().getPluginConfig().getId();
        }
        catch (InvalidCSConfigVersionException e) {
            throw new InvalidPluginException(this, "This plugin was developed for a higher WGA version: " + e.getTargetVersion());
        }
        catch (Exception e) {
            throw new InvalidPluginException(this, "Cannot read plugin metadata bc. of exception ",e);
        }
        
    }

    public static Configuration loadConfiguration(File file, boolean full) throws FileNotFoundException, IOException, InvalidCSConfigVersionException {
        
        if (!file.exists()) {
            return null;
        }
        
        file = WGUtils.resolveDirLink(file);
        
        DesignDefinition syncInfo = null;
        CSConfig csConfig = null;
        OverlayData overlayData = null;
        String licenseText = null;
        
        // Normal plugin file
        if (file.isFile()) {
            ZipInputStream zipIn = new ZipInputStream(new FileInputStream(file));
            try {
                ZipEntry entry;
                while ((entry = zipIn.getNextEntry()) != null) {
                    
                    String entryName = entry.getName();
                    if (entryName.equals(DesignDirectory.DESIGN_DEFINITION_FILE) || entryName.equals(DesignDirectory.SYNCINFO_FILE)) {
                        TemporaryFile tempFile = new TemporaryFile("design", zipIn, WGFactory.getTempDir()); 
                        syncInfo = DesignDefinition.load(tempFile.getFile());
                        tempFile.delete();
                    }
                    else if (entryName.equals(SystemContainerManager.CSCONFIG_PATH)) {
                        TemporaryFile tempFile = new TemporaryFile("csconfig", zipIn, WGFactory.getTempDir()); 
                        csConfig = CSConfig.load(tempFile.getFile());
                        tempFile.delete();
                    }
                    else if (entryName.equals(SystemContainerManager.LICENSE_PATH)) {
                        licenseText = WGUtils.readString(new InputStreamReader(zipIn,  "UTF-8")).trim();
                    }
                    else if (entryName.equals(SystemContainerManager.OVERLAY_DATA_PATH)) {
                        try {
                            overlayData = OverlayData.read(zipIn);
                        }
                        catch (Exception e) {
                            Logger.getLogger("wga.plugins").error("Exception reading overlay data from plugin file " + file.getAbsolutePath(), e);
                        }
                    }
                    
                    if (syncInfo != null && csConfig != null) {
                        if (!full || overlayData != null) {
                            break;
                        }
                    }
                }
            }
            finally {
                zipIn.close();
            }
        }
        
        // Developer plugin folder
        else {
            File syncInfoFile = DesignDirectory.getDesignDefinitionFile(file);
            if (syncInfoFile.exists()) {
                syncInfo = DesignDefinition.load(syncInfoFile);
            }
            File csConfigFile = new File(file, SystemContainerManager.CSCONFIG_PATH);
            if (csConfigFile.exists()) {
                csConfig = CSConfig.load(csConfigFile);
            }
            File licenseTextFile = new File(file, SystemContainerManager.LICENSE_PATH);
            if (licenseTextFile.exists()) {
                Reader reader = new InputStreamReader(new FileInputStream(licenseTextFile) , "UTF-8");
                licenseText = WGUtils.readString(reader).trim();
                reader.close();
            }
            File overlayDataFile = new File(file, SystemContainerManager.OVERLAY_DATA_PATH);
            if (overlayDataFile.exists()) {
                try {
                    InputStream stream = new FileInputStream(overlayDataFile);
                    overlayData = OverlayData.read(stream);
                    stream.close();
                }
                catch (Exception e) {
                    Logger.getLogger("wga.plugins").error("Exception reading overlay data from plugin directory " + file.getAbsolutePath(), e);
                }
            }
            
        }
        
        if (syncInfo != null && csConfig != null && csConfig.getPluginConfig() != null) {
            return new Configuration(syncInfo, csConfig, licenseText, overlayData);
        }
        else {
            return null;
        }
        
    }
    
    public String getDesignURL() throws FileSystemException {
        if (getPluginFile().isFile()) {
            return "zip:file:///" + getPluginFile().getAbsolutePath();
        }
        else {
            return getPluginFile().getAbsolutePath();
        }
    }

    public PluginID getPluginID() {
        return _pluginID;
    }

    public boolean isActive() {
        return _active;
    }

    public void setActive(boolean active) {
        _active = active;
    }

    public void checkDependencies() {
        
        Iterator depsIt = _config.getCsConfig().getPluginConfig().getDependencies().iterator();
        while (depsIt.hasNext()) {
            PluginID depId = (PluginID) depsIt.next();
            WGAPlugin depPlugin = (WGAPlugin) _parent.getActivePluginsByUniqueName().get(depId.getUniqueName());
            if (depPlugin == null) {
                addInstallationFault(new DependencyFault(depId, DependencyFault.ERROR_DEPENDENCY_NOT_AVAILABLE));
                continue;
            }
            
            int versionCompare = depId.getVersion().compareTo(depPlugin.getPluginID().getVersion());
            if (versionCompare > 0) {
                addInstallationFault(new DependencyFault(depId, DependencyFault.ERROR_DEPENDENCY_WRONG_VERSION));
                continue;
            }
            
            if (depPlugin.isActive() == false || depPlugin.isValid() == false) {
                addInstallationFault(new DependencyFault(depId, DependencyFault.ERROR_DEPENDENCY_INACTIVE));
                continue;
            }
            
            if (detectCircularReference(depPlugin)) {
                addInstallationFault(new DependencyFault(depId, DependencyFault.ERROR_CIRCULAR_REFERENCE));
                continue;
            }
            
            depPlugin.addDependentPlugin(this);
            _mandatoryPlugins.put(depPlugin.getPluginID(), depPlugin);
            
            
        }
        
    }

    private boolean detectCircularReference(WGAPlugin depPlugin) {
        
        if (depPlugin.equals(this)) {
            return true;
        }
        
        Iterator dependencies = depPlugin.getCsConfig().getPluginConfig().getDependencies().iterator();
        while (dependencies.hasNext()) {
            PluginID id = (PluginID) dependencies.next();
            WGAPlugin plugin = getParent().getPluginByID(id);
            if (plugin != null && detectCircularReference(plugin)) {
                return true;
            }
        }
        
        
        return false; 
    }


    private void addDependentPlugin(WGAPlugin plugin) {
        _dependentPlugins.put(plugin.getPluginID(), plugin);
    }

    public Map getMandatoryPlugins() {
        return _mandatoryPlugins;
    }

    public boolean isValid() {
        return _valid;
    }

    public void setValid(boolean valid) {
        _valid = valid;
    }



    public WGAPluginSet getParent() {
        return _parent;
    }

    public String getInstallationKey() {
        return _installationKey;
    }



    public void setInstallationKey(String installationKey) {
        _installationKey = installationKey;
    }
    




    public long getFileLastModified() {
        return _fileLastModified;
    }



    public String getFilePath() {
        return FilenameUtils.normalize(getParent().getCore().getWGAFile(_filePath).getAbsolutePath());
    }



    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((_pluginID == null) ? 0 : _pluginID.hashCode());
        return result;
    }



    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final WGAPlugin other = (WGAPlugin) obj;
        if (_pluginID == null) {
            if (other._pluginID != null)
                return false;
        }
        else if (!_pluginID.equals(other._pluginID))
            return false;
        return true;
    }



    public List<InstallationFault> getInstallationFaults() {
        return _installationFaults;
    }
    
    
    public void addInstallationFault(InstallationFault fault) {
        _installationFaults.add(fault);
    }
    
    public String getIdentification() {
        return getPluginID().toString();
    }



    public WorkspaceOperation getOperation() {
        return _operation;
    }



    public void setOperation(WorkspaceOperation operation) {
        _operation = operation;
    }



    public int getUpdateStatus() {
        return _updateStatus;
    }



    public void setUpdateStatus(int updateStatus) {
        _updateStatus = updateStatus;
    }


    public String buildDatabaseKey() {
        return PluginConfig.PLUGIN_DBKEY_PREFIX + getInstallationKey();
    }


    public boolean isReconnectDatabase() {
        return getRuntimeContext().isReconnect();
    }


    public void setReconnectDatabase(boolean reconnectDatabase) {
        getRuntimeContext().setReconnect(reconnectDatabase);
    }
    
    public String getPluginHomepage() {
        
        PluginConfig pc = getCsConfig().getPluginConfig();
        
        if (!pc.isUsageAsContentStore()) {
            return null;
        }
        
        String ph = pc.getPluginHomepage();
        if (ph != null && !ph.trim().equals("")) {
            return ph;
        }
        
        PublisherOption option = getCsConfig().findPublisherOption(WGACore.DBATTRIB_HOME_PAGE);
        if (option != null) {
            return option.getValue();
        }
        else {
            return null;
        }
        
        
        
    }


    protected void setFilePath(String filePath) {
        _filePath = filePath;
    }


    public boolean isDirectory() {
       return getPluginFile().isDirectory();
    }


    public Map getDependentPlugins() {
        return _dependentPlugins;
    }

    public CSConfig getCsConfig() {
        if (_config != null) {
            return _config.getCsConfig();
        }
        else {
            throw new IllegalStateException("The plugin has no valid configuration: " + _filePath);
        }
    }
    
    public OverlayData getOverlayData() {
        if (_config != null) {
            return _config.getOverlayData();
        }
        else {
            throw new IllegalStateException("The plugin has no valid configuration: " + _filePath);
        }
    }
    
    public String getLicenseText() {
        if (_config != null) {
            return _config.getLicenseText();
        }
        else {
            return null;
        }
    }
    
    public DesignDefinition getSyncInfo() {
        return _config.getSyncInfo();
    }

    public boolean isUpdated() {
        return (getUpdateStatus() == UPDATESTATUS_UPDATE || getUpdateStatus() == UPDATESTATUS_UPDATE_IDENTICAL);
    }
    
    public RuntimeContext getRuntimeContext() {
        return _parent.getRuntimeContext(this);
    }

    public String getRegisteredFilePath() {
        return _filePath;
    }

    public boolean isDefaultPlugin() {
        return _defaultPlugin;
    }

    public void setDefaultPlugin(boolean defaultPlugin) {
        _defaultPlugin = defaultPlugin;
    }
    
    public boolean isPlatformPlugin() {
        boolean isPlatformPlugin = (_filePath != null && _filePath.contains("${wga.defaultpluginsdir}"));
        if (!isPlatformPlugin) {
            isPlatformPlugin = PLATFORM_PLUGINS.contains(getPluginID().getUniqueName());
        }
        return isPlatformPlugin;
    }
    
}
