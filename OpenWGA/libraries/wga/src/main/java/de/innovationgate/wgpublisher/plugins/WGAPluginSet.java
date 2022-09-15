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

package de.innovationgate.wgpublisher.plugins;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.vfs2.FileSystemException;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;
import com.thoughtworks.xstream.io.xml.DomDriver;

import de.innovationgate.utils.MD5HashingInputStream;
import de.innovationgate.utils.MD5HashingOutputStream;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.hsql.HsqlDatabaseServer;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACore.ConnectDatabaseProblemOccasion;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.plugins.WGAPlugin.InitialisationFault;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.MessageVariableProvider;
import de.innovationgate.wgpublisher.problems.PluginScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.servers.HsqlPluginsDatabaseServer;

public class WGAPluginSet {
    
    public static transient final int UPDATESTRATEGY_UPDATE_KEEP_DATA = 1;
    public static transient final int UPDATESTRATEGY_INSTALL_IN_PARALLEL = 2;
    
    
    
    public static class RuntimeContext {
        
        private boolean _reconnect = false;
        private boolean _updated = false;
        public boolean isReconnect() {
            return _reconnect;
        }
        public void setReconnect(boolean reconnect) {
            _reconnect = reconnect;
        }
        public boolean isUpdated() {
            return _updated;
        }
        public void setUpdated(boolean updated) {
            _updated = updated;
        }
        
    }
    
    public static class ConnectPluginProblemOccasion implements ProblemOccasion, MessageVariableProvider {
        
        private PluginScope _scope;
        private WGAPlugin _plugin;
        
        public ConnectPluginProblemOccasion(WGAPlugin plugin) {
            _plugin = plugin;
            _scope = new PluginScope(plugin.getPluginID().getUniqueName());
        }
    
        @Override
        public ProblemScope getDefaultScope() {
            return _scope;
        }
    
        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }
        
        @Override
        public Class<?> getDefaultRefClass() {
            return WGAPluginSet.class;
        }
    
        @Override
        public Problem.Vars getDefaultMessageVariables() {
            
            Problem.Vars vars = Problem
                .var("plugintitle", _plugin.getCsConfig().getPluginConfig().getTitle())
                .var("pluginname", _plugin.getPluginID().getUniqueName());
            
            if (!_plugin.isValid()) {
                vars.var("pluginfaults", WGUtils.serializeCollection(_plugin.getInstallationFaults(), "\n")); 
            }
            
            return vars;
        }
        
        @Override
        public boolean isClearedAutomatically() {
            return true;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_scope == null) ? 0 : _scope.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            ConnectPluginProblemOccasion other = (ConnectPluginProblemOccasion) obj;
            if (_scope == null) {
                if (other._scope != null)
                    return false;
            }
            else if (!_scope.equals(other._scope))
                return false;
            return true;
        }
    
    }

    private static final XStream XSTREAM = XStreamUtils.createXStream(new Dom4JDriver());
    static {
        XSTREAM.alias("WGAPluginSet", WGAPluginSet.class);
        XSTREAM.alias("WGAPlugin", WGAPlugin.class);
    }
    
    public static WGAPluginSet load(File file) throws IOException, NoSuchAlgorithmException {
        FileInputStream in = new FileInputStream(file);
        MD5HashingInputStream hashIn = new MD5HashingInputStream(in);
        WGAPluginSet set = (WGAPluginSet) XStreamUtils.loadUtf8FromInputStream(XSTREAM, hashIn);
        set._hash = hashIn.getHash();
        return set;
    }
    
    public void save(File file) throws IOException, WGIllegalStateException, NoSuchAlgorithmException {
        
        if (_workspaceSet) {
            throw new WGIllegalStateException("You cannot save a workspace plugin set");
        }
        
        // Write first to ByteArrayOutputStream and create hash to see if it changed
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        MD5HashingOutputStream hashOut = new MD5HashingOutputStream(out);
        XStreamUtils.writeUtf8ToOutputStream(this, XSTREAM, hashOut);

        // Only write if the hash differs
        String newHash = hashOut.getHash();
        if (!newHash.equals(_hash)) {
            FileOutputStream fileOut = new FileOutputStream(file);
            fileOut.write(out.toByteArray());
            fileOut.close();
            _hash = newHash;
        }
    }
    
    public void save() throws WGIllegalStateException, IOException, NoSuchAlgorithmException {
        File file = new File(_pluginsDir, "plugins.xml");
        save(file);
    }
    
    private LinkedHashSet<WGAPlugin> _plugins = new LinkedHashSet<WGAPlugin>();
    private Set<WGAPlugin> _workspacePlugins = new HashSet<WGAPlugin>();
    private transient Map<String,WGAPlugin> _activePluginsByUniqueName;
    private transient Map<String,WGAPlugin> _pluginsByInstallationKey;
    private transient File _pluginsDir;
    private transient File _pluginFilesDir;
    private transient WGACore _core;
    private transient File _pluginDBsDir;

    private transient File _pluginWorkspaceDir;
    private transient boolean _workspaceSet;
    private transient String _hash;
    private transient Map<PluginID,RuntimeContext> _runtimeContexts;
    private transient HsqlPluginsDatabaseServer _dbServer;
    
    public Map<String,WGAPlugin> getActivePluginsByUniqueName() {
        return _activePluginsByUniqueName;
    }


    
    public void validatePlugins() {
        
        boolean validityChanged;
        
        // If a plugin was determined to be invalid, we must re-evaluate everything to find
        // dependent plugins that now are invalid too. So we loop over the whole process until validity does not change anymore.
        do {
            validityChanged = false;
        
            // Validate plugins and build dependency tree
            Iterator pluginsIt = _pluginsByInstallationKey.values().iterator();
            while (pluginsIt.hasNext()) {
                
                WGAPlugin plugin = (WGAPlugin) pluginsIt.next();
                
                // If already marked invalid or inactive we end here
                if (plugin.isActive() == false || plugin.isValid() == false) {
                    continue;
                }
                
                try {
                    try {
                        plugin.validate();
                    }
                    catch (InvalidPluginException e) {
                        plugin.addInstallationFault(plugin.new InitialisationFault(e.getMessage()));
                    }
                    
                    // WGA Version
                    Version wgaVersion = WGAVersion.toCsConfigVersion();
                    int comp = wgaVersion.compareTo(plugin.getCsConfig().getPluginConfig().getMinimumWGAVersion());
                    if (comp < 0) {
                        plugin.addInstallationFault(plugin.new InstallationFault(WGAPlugin.InstallationFault.ERROR_WRONG_WGA_VERSION));
                    }
                    
                    // Java Version
                    Version javaVersion = Version.getJavaVersion();
                    comp = javaVersion.compareTo(plugin.getCsConfig().getPluginConfig().getMinimumJavaVersion());
                    if (comp < 0) {
                        plugin.addInstallationFault(plugin.new InstallationFault(WGAPlugin.InstallationFault.ERROR_WRONG_JAVA_VERSION));
                    }
                    
                    // Dependencies
                    plugin.checkDependencies();
                    
                    // Check faults
                    if (plugin.getInstallationFaults().size() > 0) {
                        plugin.setValid(false);
                        validityChanged = true;
                        if (!_workspaceSet) {
                            _core.getLog().warn("Invalid WGA Plugin " + plugin.getPluginID().getUniqueName() + ":");
                            for (Iterator iter = plugin.getInstallationFaults().iterator(); iter.hasNext();) {
                                WGAPlugin.InstallationFault fault = (WGAPlugin.InstallationFault) iter.next();
                                _core.getLog().error("- " + fault.toErrorMessage());
                            }
                        }
                        
                    }
                }
                catch (Throwable e) {
                    _core.getLog().error("Exception validating plugin " + plugin.getPluginID().toString() + ", will be marked invalid", e);
                    plugin.setValid(false);
                }
            }
        
        } while (validityChanged == true);
        
        
        
        
    }
    
    public Set<String> connectPlugins(Map domainConfig) throws WGException, FileSystemException, IOException {
        
        // Disconnect inactive/invalid plugins
        Iterator<WGAPlugin> pluginsIt = _plugins.iterator();        
        while (pluginsIt.hasNext()) {
            WGAPlugin plugin = (WGAPlugin) pluginsIt.next();
            if (plugin.getInstallationKey() != null && (!plugin.isActive() || !plugin.isValid() || plugin.isReconnectDatabase())) {
                _core.disconnectPlugin(plugin);
                plugin.setReconnectDatabase(false);
                if (!plugin.isActive()) {
                    removePluginFromMaps(plugin, false);
                    plugin.setInstallationKey(null);
                }
            }
        }
        
        // Connect active plugins
        Set<String> connectedPlugins = new HashSet<String>();
        Set<String> pluginsTreated = new HashSet<String>();
        
        pluginsIt = _pluginsByInstallationKey.values().iterator();
        while (pluginsIt.hasNext()) {
            WGAPlugin plugin = pluginsIt.next();
            ConnectPluginProblemOccasion occ = new ConnectPluginProblemOccasion(plugin);
            _core.getProblemRegistry().clearProblemOccasion(occ);
            
            
            WGDatabase db = null;
            if (plugin.isActive() && plugin.isValid()) {
                pluginsTreated.add(plugin.getPluginID().getUniqueName());    
                try {
                    db = _core.connectPlugin(plugin, domainConfig, connectedPlugins);
                }
                catch (Problem p) {
                    _core.getLog().error("Problem connecting plugin", p);
                    _core.getProblemRegistry().addProblem(p);
                }
                catch (InvalidPluginException e) {
                    String msg = "Unable to connect plugin " + plugin.getPluginID().getUniqueName();
                    if (e.getPlugin() == plugin) {
                        msg = "Unable to connect plugin " + plugin.getPluginID().getUniqueName();
                    }
                    else {
                       msg  = "Unable to connect plugin " + plugin.getPluginID().getUniqueName() + " because of dependency plugin " + e.getPlugin().getPluginID().getUniqueName();
                    }
                    _core.getLog().error(msg, e);
                    _core.getProblemRegistry().addProblem(Problem.create(occ, "pluginConnectionFailed.exception", ProblemSeverity.HIGH, new WGAServerException(msg, e)));
                }

            }          
        }
        
        // Find not yet treated invalid plugins
        pluginsIt = _plugins.iterator();
        while (pluginsIt.hasNext()) {
            WGAPlugin plugin = pluginsIt.next();
            if (pluginsTreated.contains(plugin.getPluginID().getUniqueName())) {
                continue;
            }
            
            if (plugin.isActive() && !plugin.isValid()) {
                ConnectPluginProblemOccasion occ = new ConnectPluginProblemOccasion(plugin);
                _core.getProblemRegistry().addProblem(Problem.create(occ, "pluginConnectionFailed.invalid", ProblemSeverity.HIGH));
            }
            
        }
        
        return connectedPlugins;
    }
    


    public WGACore getCore() {
        return _core;
    }


    


    private WGAPlugin createPlugin(String path) throws WGIllegalArgumentException, IOException, InvalidPluginException {
        
        File file = _core.getWGAFile(path);
        if (!file.exists()) {
            throw new WGIllegalArgumentException("File '" + file.getPath() + " does not exist or is no regular data file");
        }
        
        WGAPlugin plugin = new WGAPlugin(this, path);
        return plugin;
    }


    
    public void init(WGACore core, File pluginsDir) throws PluginSetInitException {
        init(core, pluginsDir, false);
    }
    
    
    private void init(WGACore core, File pluginsDir, boolean workspaceSet) throws PluginSetInitException {
        
        try {
        _core = core;
        _workspaceSet = workspaceSet;
        _runtimeContexts = new HashMap<PluginID, RuntimeContext>();
        
        _pluginsDir = pluginsDir;
        createPluginDirectories()
        ;
        _dbServer = new HsqlPluginsDatabaseServer(this);
        _dbServer.init(WGAConfiguration.SINGLETON_SERVER_PREFIX + _dbServer.getClass().getName(), "Plugins Database Directory", Collections.EMPTY_MAP);
        
        _pluginsByInstallationKey = new HashMap<String,WGAPlugin>();
        _activePluginsByUniqueName = new HashMap<String,WGAPlugin>();
         
        Iterator<WGAPlugin> plugins = _plugins.iterator();
        while (plugins.hasNext()) {
            
            WGAPlugin plugin = (WGAPlugin) plugins.next();
            try {
                plugin.init();
                
                if (plugin.isActive()) {
                    _activePluginsByUniqueName.put(plugin.getPluginID().getUniqueName(), plugin);
                    if (plugin.getInstallationKey() != null) {
                        _pluginsByInstallationKey.put(plugin.getInstallationKey(), plugin);
                    }
                }
            }
            catch (MissingDefaultPluginException e) {
                // Will be tolerated for now, handled later on default plugin evaluation
            }
            catch (InvalidPluginException e) {
                _core.getLog().error("Error initializing plugin " + plugin.getPluginID().toString(), e);
            }
        }
        }
        catch (PluginSetInitException e) {
            throw e;
        }
        catch (Exception e) {
            throw new PluginSetInitException("Exception initializing plugin set", e);
        }
    }

    private void createPluginDirectories() throws PluginSetInitException {
        
        // Plugin files dir: Contains installed plugins
        _pluginFilesDir = new File(_pluginsDir, "files");
        if (!_pluginFilesDir.exists()) {
            if (_workspaceSet || !_pluginFilesDir.mkdir()) {
                throw new PluginSetInitException("Could not create plugin files directory '" + _pluginFilesDir.getPath() + "'. No WGA Plugins will be connected.");        
            }
        }
        
        // Plugin databases dir: Contains HSQL databases for plugin content stores
        _pluginDBsDir = new File(_pluginsDir, "#dbs");
        
        // check for old plugin dbs dir name and rename
        File oldPluginDBsDir = new File(_pluginsDir, "dbs");
        if (oldPluginDBsDir.exists()) {
        	oldPluginDBsDir.renameTo(_pluginDBsDir);
        }
        
        if (!_pluginDBsDir.exists()) {
            if (_workspaceSet || !_pluginDBsDir.mkdir()) {
                    throw new PluginSetInitException("Could not create plugin databases directory '" + _pluginDBsDir.getPath() + "'. No WGA Plugins will be connected.");                    
            }
        }
        
        // Plugin workspace dir: Contains uploaded but not yet installed plugins
        _pluginWorkspaceDir = new File(_pluginsDir, "#workspace");
        
        File oldPluginWorkspaceDir = new File(_pluginsDir, "workspace");
        if (oldPluginWorkspaceDir.exists()) {
        	oldPluginWorkspaceDir.renameTo(_pluginWorkspaceDir);
        }
        
        if (!_pluginWorkspaceDir.exists()) {
            if (_workspaceSet || !_pluginWorkspaceDir.mkdir()) {
                throw new PluginSetInitException("Could not create plugin workspace directory '" + _pluginWorkspaceDir.getPath() + "'. No WGA Plugins will be connected.");        
            }
        }
    }

    public synchronized WGAPlugin installPlugin(File file, int updateStrategy, boolean defaultPlugin) throws PluginException {
        return installPlugin(file, updateStrategy, false, defaultPlugin, null);
    }

    
    protected synchronized WGAPlugin installPlugin(File file, int updateStrategy, boolean workspace, boolean defaultPlugin, String replaceKey) throws PluginException {

       try {

           WGAPlugin sourcePlugin = createPlugin(file.getAbsolutePath());
           
           if (!_workspaceSet) {
               _core.getLog().info("Installing plugin " + sourcePlugin.getPluginID().getUniqueName() + " Version " + sourcePlugin.getPluginID().getVersion().toString());
           }
           
           // Determine if a plugin of that id already exists
           boolean existed = false;
           
           
           WGAPlugin previousPlugin = getPluginByID(sourcePlugin.getPluginID());
           if (previousPlugin != null) {
               if (!_workspaceSet) {
                   File prevPluginFile = previousPlugin.getPluginFile();
                   if (!prevPluginFile.exists()) {
                       _core.getLog().info("Previous plugin file does no longer exist");   
                   }
                   else if (prevPluginFile.isFile()) {
                       if (!previousPlugin.isDefaultPlugin()) {
                           _core.getLog().info("Removing previous file for plugin " + previousPlugin.getPluginID().toString());
                           prevPluginFile.delete();
                       }
                       else {
                           _core.getLog().warn("You are overwriting a default plugin. The previous version, if still available, will stay and may be reinstalled automatically.");
                       }
                   }
                   else {
                       _core.getLog().info("Previous plugin file was a directory and will not be deleted");
                   }
               }
               existed = true;
           }
           
           String fileName = sourcePlugin.getPluginID().buildQualifiedFileName();
           File targetFile = new File(getPluginFilesDir(), fileName);

           // Normal mode: Copy source to target file. 
           if (!workspace && !defaultPlugin && file.isFile() && !targetFile.equals(file)) {
               InputStream in = new FileInputStream(file);
               OutputStream out = new FileOutputStream(targetFile);
               WGUtils.inToOut(in, out, 2048);
               in.close();
               out.close();
               
           }
           // Developer plugins, default plugins or reinstalled plugin from plugins folder: Just use the source file
           else {
               targetFile = file;
           }
              
           WGAPlugin targetPlugin = null;
           
           // If there already was a plugin file try to find an existing plugin definition. 
           // If so, just tell it to reconnect, change nothing else
           if (existed) {
               targetPlugin = getPluginByID(sourcePlugin.getPluginID());
               if (targetPlugin != null) {
                   targetPlugin.setFilePath(getCore().createWGAFilePath(targetFile));
                   targetPlugin.setDefaultPlugin(defaultPlugin);
                   targetPlugin.setReconnectDatabase(true);
                   targetPlugin.setUpdateStatus(WGAPlugin.UPDATESTATUS_UPDATE_IDENTICAL);
                   targetPlugin.getRuntimeContext().setUpdated(true);
                   targetPlugin.init();
                   if (!workspace) {
                       if (targetPlugin.isActive()) {
                           return activatePlugin(targetPlugin, updateStrategy, workspace, targetPlugin.getInstallationKey());
                       }
                   }
                   else {
                       // In workspace mode we must fake that the source plugin was installed, as we do not touch the target plugin
                       sourcePlugin.setInstallationKey(targetPlugin.getInstallationKey());
                       sourcePlugin.setActive(targetPlugin.isActive());
                       return sourcePlugin;
                   }
               }
           }
           
           // If plugin did not exist yet, create a new one for the target
           targetPlugin = createPlugin(getCore().createWGAFilePath(targetFile));
           targetPlugin.setDefaultPlugin(defaultPlugin);
           
           // Determine if the current installation status of this plugin is deactivated (plugins exist but are all inactive) REMOVED PER #00000867
           // boolean isPluginDeactivated = isPluginDeactivated(targetPlugin.getPluginID().getUniqueName());
           
           // Install it and activate it unless it was deactivated before
           _plugins.add(targetPlugin);
           return activatePlugin(targetPlugin, updateStrategy, workspace, replaceKey);
                      
        }
        catch (PluginException e) {
            throw e;
        }
        catch (InvalidPluginException e) {
            throw new PluginException(e.getMessage(), e.getCause());
        }
        catch (Exception e) {
            throw new PluginException("Exception installing plugin", e);
        }
        
    }

    public boolean isPluginDeactivated(String uniqueName) {
        
        List<WGAPlugin> plugins = getPluginsByUniqueName(uniqueName);
        if (plugins.size() == 0) {
            return false;
        }
        
        for (WGAPlugin plugin : plugins) {
            if (plugin.isActive()) {
                return false;
            }
        }
        
        return true;
        
        
    }

    public WGAPlugin activatePlugin(WGAPlugin plugin, int updateStrategy, boolean isWorkspacePlugin, String replaceKey) throws WGIllegalArgumentException, FileSystemException, IOException, PluginException  {

        String installationKey = null;
        if (!isWorkspacePlugin) {
            _core.getLog().info("Activating plugin " + plugin.getPluginID().getUniqueName() + " Version " + plugin.getPluginID().getVersion().toString());
        }
        plugin.setActive(true);
        boolean clearPluginDatabase = false;
        
        // Deactivate previous plugin if available
        
        // Replace key given: Check if this is a previous version of the plugin under that key, deactivate it if so        
        if (updateStrategy == UPDATESTRATEGY_UPDATE_KEEP_DATA && replaceKey != null && _pluginsByInstallationKey.get(replaceKey) != null) {

                WGAPlugin previousPlugin = (WGAPlugin) _pluginsByInstallationKey.get(replaceKey);
                if (!previousPlugin.getPluginID().getUniqueName().equals(plugin.getPluginID().getUniqueName())) {
                    throw new PluginException("The plugin under installation key '" + replaceKey + "' is no plugin of name " + plugin.getPluginID().getUniqueName());
                }
                
                installationKey = previousPlugin.getInstallationKey();
                if (previousPlugin != plugin) {
                    if (!_workspaceSet) {
                        _core.getLog().info("Deactivating WGA plugin " + previousPlugin.getPluginID().getUniqueName() + " Version " + previousPlugin.getPluginID().getVersion().toString());
                    }
                    deactivatePlugin(previousPlugin);
                }
                plugin.setUpdateStatus(WGAPlugin.UPDATESTATUS_UPDATE);
        }
        
        // No replace key given: Find previously installed version of this plugin and deactivate it
        else {
               WGAPlugin previousPlugin = (WGAPlugin) _activePluginsByUniqueName.get(plugin.getPluginID().getUniqueName());
               if (previousPlugin != null && previousPlugin != plugin && previousPlugin.isActive() == true && previousPlugin.getInstallationKey() != null) {
                   
                   // New plugin should replace old plugin, old will be deactivated
                   if (updateStrategy == UPDATESTRATEGY_UPDATE_KEEP_DATA) {
                       if (!_workspaceSet) {
                           _core.getLog().info("Deactivating WGA plugin " + previousPlugin.getPluginID().getUniqueName() + " Version " + previousPlugin.getPluginID().getVersion().toString());
                       }
                       installationKey = previousPlugin.getInstallationKey();
                       deactivatePlugin(previousPlugin);
                       plugin.setUpdateStatus(WGAPlugin.UPDATESTATUS_UPDATE);
                   }
                   else {
                       plugin.setUpdateStatus(WGAPlugin.UPDATESTATUS_INSTALL_PARALLEL);
                   }
                   
               }
               
               // Look if we have any inactive plugin of this name with an installation key which we could replace
               else {
                   for (WGAPlugin anyPlugin : _plugins) {
                       if (anyPlugin.getPluginID().getUniqueName().equals(plugin.getPluginID().getUniqueName()) && anyPlugin.isActive() == false && anyPlugin.getInstallationKey() != null) {
                           installationKey = anyPlugin.getInstallationKey();
                           break;
                       }
                   }
               }
        }
      
       // Activate plugin
        plugin.getRuntimeContext().setUpdated(true);
           if (installationKey != null) {
               plugin.setInstallationKey(installationKey);
           }
           else {
               determineInstallationKey(plugin);
           }
           
           _pluginsByInstallationKey.put(plugin.getInstallationKey(), plugin);
           _activePluginsByUniqueName.put(plugin.getPluginID().getUniqueName(), plugin);
       
       return plugin;
    }

    public void uninstallPlugin(WGAPlugin plugin, boolean workspace) {
        
        if (!workspace) {
            _core.getLog().info("Uninstalling plugin " + plugin.getIdentification());
        }
        removePluginFromMaps(plugin, true);
        
        if (!workspace && !plugin.isDefaultPlugin()) {
            _core.getLog().info("Removing plugin file " + plugin.getIdentification());
            File pluginFile = plugin.getPluginFile();
            if (pluginFile.exists() && pluginFile.isFile()) {
                pluginFile.delete();
            }
        }
        
        if (!workspace) {
            _core.getProblemRegistry().clearProblemScope(new PluginScope(plugin.getPluginID().getUniqueName()));
        }
        
    }

    private void removePluginFromMaps(WGAPlugin plugin, boolean completeRemoval) {
        
        if (completeRemoval && _plugins.contains(plugin)) {
            _plugins.remove(plugin);
        }
        
        WGAPlugin mapPlugin = (WGAPlugin) _activePluginsByUniqueName.get(plugin.getPluginID().getUniqueName());
        if (mapPlugin != null && mapPlugin.equals(plugin)) {
            _activePluginsByUniqueName.remove(plugin.getPluginID().getUniqueName());
        }
        
        if (plugin.getInstallationKey() != null) {
            mapPlugin = (WGAPlugin) _pluginsByInstallationKey.get(plugin.getInstallationKey());
            if (mapPlugin != null && mapPlugin.equals(plugin)) {
                _pluginsByInstallationKey.remove(plugin.getInstallationKey());
            }
        }
        
        
    }
    
    public void deactivatePlugin(WGAPlugin plugin) {
        
        // Only set the flag here. WGACore.connectPlugins() takes care of disconnect and cleanup operations, but needs a filled installation key to do this.
        plugin.setActive(false);
        /*
        plugin.setInstallationKey(null);
        removePluginFromMaps(plugin, false);*/
    }

    private synchronized void determineInstallationKey(WGAPlugin targetPlugin) throws WGIllegalArgumentException, FileSystemException, IOException, PluginException {
        
        String key = targetPlugin.getPluginID().buildShortName().toLowerCase();
        if (!_pluginsByInstallationKey.containsKey(key)) {
            targetPlugin.setInstallationKey(key);
            return;
        }        
        
        key = targetPlugin.getPluginID().buildShortName() + "-" + targetPlugin.getPluginID().getVersion().getMajorVersion() + "." + targetPlugin.getPluginID().getVersion().getMinorVersion();
        key = key.toLowerCase();
        if (!_pluginsByInstallationKey.containsKey(key)) {
            targetPlugin.setInstallationKey(key);
            return;
        }
        
        key = targetPlugin.getPluginID().buildShortName() + "-" + targetPlugin.getPluginID().getVersion().getMainVersionString();
        key = key.toLowerCase();
        if (!_pluginsByInstallationKey.containsKey(key)) {
            targetPlugin.setInstallationKey(key);
            return;
        }
        
        key = targetPlugin.getPluginID().getUniqueName() + "-" + targetPlugin.getPluginID().getVersion().getMainVersionString();
        key = key.toLowerCase();
        if (!_pluginsByInstallationKey.containsKey(key)) {
            targetPlugin.setInstallationKey(key);
            return;
        }
        
        throw new PluginException("Cannot determine a free installation key");
        
    }

    public File getPluginDBsDir() {
        return _pluginDBsDir;
    }

    public File getPluginFilesDir() {
        return _pluginFilesDir;
    }

    public File getPluginWorkspaceDir() {
        return _pluginWorkspaceDir;
    }

    public List<WGAPlugin> getPlugins() {
        List<WGAPlugin> plugins = new ArrayList<WGAPlugin>(_plugins);
        Collections.sort(plugins, new Comparator<WGAPlugin>() {

            public int compare(WGAPlugin p1, WGAPlugin p2) {
                int state1 = (!p1.isActive() ? 3 : !p1.isValid() ? 2 : 1);
                int state2 = (!p2.isActive() ? 3 : !p2.isValid() ? 2 : 1);
                return state1 - state2;
            }
            
        });
        return plugins;
    }

    public Map<String,WGAPlugin> getPluginsByInstallationKey() {
        return _pluginsByInstallationKey;
    }
    
    public WGAPluginSet createWorkspacePluginSet(List ops) throws PluginException, FileSystemException, PluginSetInitException {
        
        WGAPluginSet set = createClone();
        
        // Add Workspace Operations
        set.performOperations(ops, true);
        
        // Validate plugins
        set.validatePlugins();
        return set;
        
    }

    public Set<WGAPlugin> getWorkspacePlugins() {
        return _workspacePlugins;
    }
    
    public InstallPluginOperation loadPluginToWorkspace(File file) throws IOException {
        
        File targetFile = File.createTempFile("plugin", ".wgaplugin", getPluginWorkspaceDir());
        //B000048C2 - not necessary workspace dir is cleared on wga shutdown now
        // targetFile.deleteOnExit();
        WGUtils.copyFile(file, targetFile);
        
        InstallPluginOperation op = new InstallPluginOperation(targetFile, UPDATESTRATEGY_UPDATE_KEEP_DATA);
        return op;
        
        
    }
    
    public InstallPluginOperation loadPluginToWorkspace(InputStream in) throws IOException {
    	File targetFile = File.createTempFile("plugin", ".wgaplugin", getPluginWorkspaceDir());
    	WGUtils.inToOut(in, new FileOutputStream(targetFile), 1024);
    	InstallPluginOperation op = new InstallPluginOperation(targetFile, UPDATESTRATEGY_UPDATE_KEEP_DATA);
        return op;
    }

    public File getPluginsDir() {
        return _pluginsDir;
    }
    
    public List<String> getChanges(WGAPluginSet set) {
        
        List<String> changes = new ArrayList<String>();
        
        // Installed plugins that will get deactivated
        Iterator<WGAPlugin> oldPlugins = set.getPluginsByInstallationKey().values().iterator();
        while (oldPlugins.hasNext()) {
            WGAPlugin oldPlugin = (WGAPlugin) oldPlugins.next();
            WGAPlugin newPlugin = (WGAPlugin) getPluginByID(oldPlugin.getPluginID());
            WGAPlugin newInstalledPlugin = (WGAPlugin) _pluginsByInstallationKey.get(oldPlugin.getInstallationKey());
            
            if (newPlugin == null) {
                changes.add("The plugin " + oldPlugin.getIdentification() + " will be removed.");
                continue;
            }
            
            if (newInstalledPlugin != null && !newInstalledPlugin.getPluginID().equals(oldPlugin.getPluginID())) {
                changes.add("The plugin " + oldPlugin.getIdentification() + " will be replaced by " +
                        newInstalledPlugin.getIdentification() + ". The previous version will be deactivated.");
                continue;
            }
            
            if (newInstalledPlugin != null && newInstalledPlugin.getUpdateStatus() == WGAPlugin.UPDATESTATUS_UPDATE_IDENTICAL) {
                changes.add("The plugin " + oldPlugin.getIdentification() + " will be replaced by a new plugin with identical version number.");
            }
            
            if (oldPlugin.isActive() && !newPlugin.isActive()) {
                changes.add("The plugin " + newPlugin.getIdentification() + " will be deactivated.");
            }
            
            if (oldPlugin.isValid() && !newPlugin.isValid()) {
                changes.add("The plugin " + newPlugin.getIdentification() + " will be invalid.");
            }
            
            if (newPlugin.getOperation() instanceof ResetDatabaseOperation) {
                changes.add("All stored data for plugin " + newPlugin.getIdentification() + " will be erased. The plugin will initialize itself again.");
            }
        }
        
        // Workspace plugins that will not work - We don't want this information here, since we are only
        // interested in effects on existing plugins
        /*Iterator wsPlugins = _workspacePlugins.iterator();
        while (wsPlugins.hasNext()) {
            WGAPlugin wsPlugin = (WGAPlugin) wsPlugins.next();
            if (wsPlugin.isValid() == false) {
                changes.add("New plugin " + wsPlugin.getIdentification() + " will be invalid.");
            }
            else if (wsPlugin.isActive() == false) {
                changes.add("New plugin " + wsPlugin.getIdentification() + " will be inactive.");
            }
        }*/
        
        return changes;
        
    }
    
    public void removePluginFromWorkspace(File pluginFile) {
        
        if (pluginFile.exists() && pluginFile.getPath().startsWith(_pluginWorkspaceDir.getPath())) {
            pluginFile.delete();
        }
        else {
            throw new IllegalArgumentException("The plugin file '" + pluginFile.getPath() + "' is no plugin in workspace.");
        }
        
    }
    
    private void performOperations(List<WorkspaceOperation> ops, boolean workspace) throws PluginOperationException {
        
        Iterator opsIt = ops.iterator();
        while (opsIt.hasNext()) {
            WorkspaceOperation op = (WorkspaceOperation) opsIt.next();
            WGAPlugin plugin = op.perform(this, _core, workspace);
            if (plugin != null) {
                plugin.setOperation(op);
            }
            if (workspace) {
                _workspacePlugins.add(plugin);
            }
        }
        
    }
    
    public void performOperations(List<WorkspaceOperation> ops) throws PluginOperationException {
        performOperations(ops, false);
    }
    
    public WGAPlugin getPluginByID(PluginID id) {
        
        Iterator<WGAPlugin> plugins = _plugins.iterator();
        
        while (plugins.hasNext()) {
            WGAPlugin plugin = (WGAPlugin) plugins.next();
            if (plugin.getPluginID().equals(id)) {
                return plugin;
            }
        }
        
        return null;
        
    }
    

    
    public WGAPlugin getPluginByID(String idString) {
        return getPluginByID(new PluginID(idString));
    }
    
    public WGAPluginSet createClone() throws PluginSetInitException {
        
        String xml = XSTREAM.toXML(this);
        WGAPluginSet clone = (WGAPluginSet) XSTREAM.fromXML(xml);
        clone.init(_core, _pluginsDir, true);
        return clone;
        
    }
    
    public List<WGAPlugin> getPluginsByUniqueName(String name) {
        
        List<WGAPlugin> results = new ArrayList<WGAPlugin>();
        Iterator<WGAPlugin> plugins = _plugins.iterator();
        while (plugins.hasNext()) {
            WGAPlugin plugin = (WGAPlugin) plugins.next();
            if (plugin.getPluginID().getUniqueName().equals(name)) {
                results.add(plugin);
            }
        }
        Collections.sort(results, new Comparator<WGAPlugin>() {

            public int compare(WGAPlugin p1, WGAPlugin p2) {
                return p1.getPluginID().getVersion().compareTo(p2.getPluginID().getVersion());
            }
            
        });
        return results;
        
    }
    
    public WGAPlugin getPluginByUniqueName(String name) {
        
        List<WGAPlugin> plugins = getPluginsByUniqueName(name);
        if (plugins.size() > 0) {
            Iterator<WGAPlugin> pluginsIt = plugins.iterator();
            
            // Try to return an active plugin of this name
            while (pluginsIt.hasNext()) {
                WGAPlugin plugin = pluginsIt.next();
                if (plugin.isActive()) {
                    return plugin;
                }
            }
            return plugins.get(0);
        }
        else {
            return null;
        }
        
    }

    public void deletePluginDatabase(WGAPlugin plugin) {
        
        String installKey = plugin.getInstallationKey();
        
        List<String> suffixes = new ArrayList<String>();
        suffixes.add(".log");
        suffixes.add(".script");
        suffixes.add(".properties");
        suffixes.add(".data");
        suffixes.add(".lck");
        suffixes.add(".backup");
        
        for (String suffix : suffixes) {
            File dbFile  = new File(getPluginDBsDir(), installKey + suffix);
            if (dbFile.exists()) {
                dbFile.delete();
            }
        }
        
    }

    /**
     * clears temporary resources e.g. tempfiles in the workspace dir
     * called before wga shutdown
     */
    public void clearTempResources() {
    	if (_pluginWorkspaceDir != null && _pluginWorkspaceDir.exists()) {
    		WGUtils.delTree(_pluginWorkspaceDir, false);
    	}
    }
    
    /**
     * Tests if there are some invalid plugins in this plugin set
     */
    public boolean hasInvalidPlugins() {
        Iterator<WGAPlugin> plugins = _plugins.iterator();
        while (plugins.hasNext()) {
            WGAPlugin plugin = (WGAPlugin) plugins.next();
            if (!plugin.isValid()) {
                return true;
            }
        }
        
        return false;
    }
    
    public synchronized RuntimeContext getRuntimeContext(WGAPlugin plugin) {
        RuntimeContext cx = _runtimeContexts.get(plugin.getPluginID());
        if (cx == null) {
            cx = new RuntimeContext();
            _runtimeContexts.put(plugin.getPluginID(), cx);
        }
        return cx;
    }
    
    public void importRuntimeContexts(WGAPluginSet oldSet) {
        _runtimeContexts = oldSet._runtimeContexts;
    }

    public WGAPlugin getBestMatchingPlugin(String name, Version minVersion, boolean preferCloseVersion) {

        WGAPlugin bestMatch = null;
        int bestMatchDiff = 0;
        for (WGAPlugin plugin : getPluginsByUniqueName(name)) {
            
            if (!plugin.isActive() || !plugin.isValid()) {
                continue;
            }

            int diff = plugin.getPluginID().getVersion().compareTo(minVersion); 
            
            // version too low
            if (diff < 0) {
                continue;
            }
            
            
            boolean isBestMatch = false;
            if (bestMatch == null) {
                isBestMatch = true;
            }
            else {
                if (preferCloseVersion && diff < bestMatchDiff) {
                    isBestMatch = true;
                }
                else if (!preferCloseVersion && diff > bestMatchDiff) {
                    isBestMatch = true;
                }
            }
            
            if (isBestMatch) {
                bestMatch = plugin;
                bestMatchDiff = diff;
           }
            
        }
        
        return bestMatch;
    
    }

    public HsqlPluginsDatabaseServer getDbServer() {
        return _dbServer;
    }
    
    public WGAPlugin getPluginByFile(File file) {
        
        
        for (WGAPlugin plugin : _plugins) {
            if (plugin.getPluginFile().equals(file)) {
                return plugin;
            }
        }
        
        return null;
        
    }
}
