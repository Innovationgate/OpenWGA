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

package de.innovationgate.wga.server.api;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

import javax.servlet.ServletContext;

import org.apache.log4j.Logger;

import de.innovationgate.utils.DynamicClassLoadingChain;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.config.ConfigBean;
import de.innovationgate.wga.config.ConfigValidationException;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.DatabaseServer;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.model.ValidationError;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.cache.FileCache;
import de.innovationgate.wgpublisher.cache.PostprocessedResourcesCache;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.plugins.WGAPluginSet;
import de.innovationgate.wgpublisher.plugins.WorkspaceOperation;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.DomainScope;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.ProblemKeyQualificator;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemRegistry;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemType;


/**
 * This  provides information and services regarding the OpenWGA server runtime and installation
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Server {
    
    /**
     * A problem occasion created by {@link Server#startProblemOccasion(Class, Object, String, boolean)}
     */
    public static class ServerOccasion implements ProblemOccasion, ProblemKeyQualificator {
        
        private Object _scopeObject;
        private Class<?> _refClass;
        private boolean _autoClear;
        private String _baseKey;
        
        private ServerOccasion(Class<?> refClass, Object scopeObject, String baseKey, boolean autoClear) {
            _scopeObject = scopeObject;
            _refClass = refClass;
            _baseKey = baseKey;
            _autoClear = autoClear;
        }

        @Override
        public ProblemScope getDefaultScope() {
            
            try {
                if (_scopeObject instanceof Database) {
                    return new DatabaseScope(((Database) _scopeObject).getDbKey());
                }
                else if (_scopeObject instanceof WGDatabase) {
                    return new DatabaseScope(((WGDatabase) _scopeObject).getDbReference());
                }
                else if (_scopeObject instanceof Domain) {
                    return new DomainScope(((Domain) _scopeObject).getName());
                }
            }
            catch (WGException e) {
            }
            
            return GlobalScope.INSTANCE;
            
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return _refClass;
        }

        @Override
        public boolean isClearedAutomatically() {
            return _autoClear;
        }

        @Override
        public String getBaseKey() {
            return _baseKey;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_baseKey == null) ? 0 : _baseKey.hashCode());
            result = prime * result + ((_refClass == null) ? 0 : _refClass.hashCode());
            result = prime * result + ((_scopeObject == null) ? 0 : _scopeObject.hashCode());
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
            ServerOccasion other = (ServerOccasion) obj;
            if (_baseKey == null) {
                if (other._baseKey != null)
                    return false;
            }
            else if (!_baseKey.equals(other._baseKey))
                return false;
            if (_refClass == null) {
                if (other._refClass != null)
                    return false;
            }
            else if (!_refClass.equals(other._refClass))
                return false;
            if (_scopeObject == null) {
                if (other._scopeObject != null)
                    return false;
            }
            else if (!_scopeObject.equals(other._scopeObject))
                return false;
            return true;
        }


        
    }
    
    protected abstract class ServerManagementTask<ReturnValue> {
        
        private WGException _error = null;
        private ReturnValue _result = null;
        
        public ReturnValue runWithExceptions() throws WGException {

            Runnable r = new Runnable() {
                
                @Override
                public void run() {
                    try {
                        _result = execute();
                    }
                    catch (WGException e) {
                        _error = e;
                    }
                    catch (Throwable e) {
                        _error = new WGAServerException("Exception running server management task", e);
                    }
                    finally {
                        WGFactory.getInstance().closeSessions();
                    }
                }
            };
            
            try {
                Thread t = new Thread(r);
                t.start();
                t.join();
            }
            catch (InterruptedException e) {
                throw new RuntimeException("Interrupted server management task", e);
            }
            
            if (_error != null) {
                throw _error;
            }
            return _result;

        }
        
        protected abstract ReturnValue execute() throws Exception;
                
    }
    
    
    
    private ServerManagementTask<List<String>> _clearCacheTask = new ServerManagementTask<List<String>>() {
        
        @Override
        protected List<String> execute() throws Exception {
            
            List<String> errors = new ArrayList<String>();
            for (String dbKey : getDatabaseKeys()) {
                
                WGDatabase db = _wga.db(dbKey);
                
                try {
                    db.refresh();
                }
                catch (Throwable e) {
                    getLog().error("Exception clearing document and user cache of database ' " + dbKey + "'", e);
                    errors.add("Document and user cache of database '" + dbKey + "'");
                    
                }
                
                try {
                    FileCache fileCache = (FileCache) db.getAttribute(WGACore.DBATTRIB_FILECACHE);
                    fileCache.clear();
                }
                catch (Throwable e) {
                    getLog().error("Exception clearing file publishing cache of database ' " + dbKey + "'", e);
                    errors.add("File publishing cache of database '" + dbKey + "'");
                }
                
                try {
                    PostprocessedResourcesCache pprCache = (PostprocessedResourcesCache) db.getAttribute(WGACore.DBATTRIB_PPRCACHE);
                    pprCache.clear();
                }
                catch (Throwable e) {
                    getLog().error("Exception clearing PPR cache of database ' " + dbKey + "'", e);
                    errors.add("PPR cache of database '" + dbKey + "'");
                }
                
            }
            
            try {
                _wga.getCore().getWebTMLCache().clear();
            }
            catch (Throwable e) {
                getLog().error("Exception clearing WebTML cache", e);
                errors.add("WebTML cache");
            }
            
            try {
                _wga.getCore().getDesignFileCache().flushAll();
            }
            catch (Throwable e) {
                getLog().error("Exception clearing design file cache", e);
                errors.add("Design file cache");
            }
            
            try {
                ExpressionEngineFactory.getTMLScriptEngine().clearCache();
            }
            catch (Throwable e) {
                getLog().error("Exception clearing TMLScript code cache", e);
                errors.add("TMLScript code cache");
            }
            
            System.gc();
            
            return errors;
        
        }
    
    };
    private ReentrantLock _clearCacheTaskLock = new ReentrantLock();
    
    private WGA _wga;

    protected Server(WGA wga) {
        _wga = wga;
    }
    
    /**
     * Returns a List of database keys of all OpenWGA databases, including apps and data sources
     */
    public Collection<String> getDatabaseKeys() throws WGException {
        return new ArrayList<String>(_wga.getCore().getContentdbs().keySet());
    }
    
    /**
     * Returns a List of database keys of all OpenWGA applications, excluding data sources
     */
    public Collection<String> getAppKeys() throws WGException {
        
        List<String> keys = new ArrayList<String>();
        for (WGDatabase db : _wga.getCore().getContentdbs().values()) {
            if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                keys.add(db.getDbReference());
            }
        }
        return keys;
    }
    
    /**
     * Returns a List of database keys of all OpenWGA data sources, excluding full applications
     */
    public Collection<String> getDataSourceKeys() throws WGException {
        
        List<String> keys = new ArrayList<String>();
        for (WGDatabase db : _wga.getCore().getContentdbs().values()) {
            if (!db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
                keys.add(db.getDbReference());
            }
        }
        return keys;
    }
    
    /**
     * Returns the names of all OpenWGA domains
     */
    public Collection<String> getDomainNames() throws WGException {
        return new ArrayList<String>(_wga.getCore().getDomains().keySet());
    }
    
    
    /**
     * Returns the application log logger of the server
     */
    @CodeCompletion
    public Logger getLog() {
        return _wga.getCore().getLog();
    }
    

    /**
     * Returns the base URL to reach this OpenWGA runtime
     * This is the base part of every OpenWGA URL that is necessary to address the OpenWGA server itself, without any extra path information that is used to address resources within OpenWGA. 
     * @throws WGAPIException
     */
    public String getBaseURL() throws WGException {
    	return getBaseURL(true);
    }

    /**
     * Returns the base URL to reach this OpenWGA runtime
     * This is the base part of every OpenWGA URL that is necessary to address the OpenWGA server itself, without any extra path information that is used to address resources within OpenWGA. 
     * It is an absolute URL which uses an "best effort" approach to serve a matching host name (as an OpenWGA server may listen to many hostnames). When in a normal WebTML environment running on behalf of a request then this URL uses the base URL information from that request. If not then it uses the configured root URL from WGA configuration.
     * @param absolute
     * @throws WGAPIException
     */

    public String getBaseURL(boolean absolute) throws WGException {

        if (_wga.isRequestAvailable())  {
            try {
                return WGPDispatcher.getPublisherURL(_wga.getRequest(), absolute);
            }
            catch (UnavailableResourceException e) {
                // Cannot happen
            }
        }
        
        return _wga.getCore().getWgaConfiguration().getRootURL();
        
    }
    
    public String getServerName(){
    	return _wga.getCore().getWgaConfiguration().getServerName();
    }
    
    /**
     * Returns the folder in which is used for WGA configuration
     * Every OpenWGA installation uses a special folder for file data that is managed by the server itself. This contains files like the basic OpenWGA configuration file "wgaconfig.xml", the plugin management folder, folders for internally managed databases etc.
     */
    public File getConfigFolder() throws WGException {
        return _wga.getCore().getConfigFile().getParentFile();
    }
    
    /**
     * Tries to find an "OpenWGA system file" of the given file name or path in the servers file system
     * This method can be used to lookup files related to OpenWGA, no matter if they are specified as complete path or as relative path/name to OpenWGAs config folder.
     * The method will:
     * <ul>
     * <li>Test if the file name is a complete path and evaluate if a file exists at that path
     * <li>Test if the file name is the relative path of a file in OpenWGAs config folder
     * <li>Resolve path variables in the file name
     * <li>Follow OpenWGA directory links
     * </ul>
     * @param fileName Name or path of the file
     * @return The system file or null if it could not be found
     * @throws WGException
     */
    public File resolveSystemFile(String fileName) throws WGException {
        return _wga.getCore().getWGAFile(fileName);
    }
    
    /**
     * Returns the JavaEE servlet context of OpenWGA
     * This object of type javax.servlet.ServletContext is the basic context object of OpenWGA as Java Web Application.
     */
    public ServletContext getServletContext() throws WGException {
        return _wga.getCore().getServletContext();
    }
    
    /**
     * Returns OpenWGAs problem registry
     */
    public ProblemRegistry getProblemRegistry() {
        return _wga.getCore().getProblemRegistry();
    }
    
    /**
     * Returns the OpenWGA module registry
     */
    public ModuleRegistry getModuleRegistry() throws WGException {
        return _wga.getCore().getModuleRegistry();
    }
    
    /**
     * Returns the library class loader used by OpenWGA to load extension classes
     */
    public DynamicClassLoadingChain getLibraryLoader() throws WGException {
        return WGACore.getLibraryClassLoadingChain();
    }
    
    /**
     * Returns the WGA server version
     */
    public Version getVersion() throws WGException {
        return WGAVersion.toCsConfigVersion();
    }

    /**
     * Reloads the server configuration
     */
    @CodeCompletion
    public void reloadConfig() throws WGException {
        reloadConfig(null);
    }
    
    /**
     * Reloads the server configuration, reconnecting the databases belonging to the given config UIDs
     * @param uidsToReconnect UIDs of config entities. Allowed are config UIDs of domains, database servers and content databases, also database keys.
     */
    @CodeCompletion
    public List<String> reloadConfig(final List<String> uidsToReconnect) throws WGException {

        ServerManagementTask<List<String>> updateConfigTask = new ServerManagementTask<List<String>>() {
            
            @Override
            protected List<String> execute() throws Exception {
                List<String> unmatchedIds = dropDatabasesForUIDs(uidsToReconnect, false);
                _wga.getCore().updateConfig();
                return unmatchedIds;
            }
        };
        
        return updateConfigTask.runWithExceptions();
        
    }
    
    protected List<String> dropDatabasesForUIDs(List<String> uidsToReconnect, final boolean reconnect) throws Exception {
        
        if (uidsToReconnect == null) {
            return Collections.emptyList();
        }

        final Map<String,Boolean> takenActions = new HashMap<String, Boolean>();
        final List<String> contentDbkeysToReconnect = new ArrayList<String>();
        final List<String> persDomainsToReconnect = new ArrayList<String>();
        List<String> unmatchedIds = new ArrayList<String>();
        
        WGAConfiguration cfg = _wga.getCore().getWgaConfiguration();
        for (String uid : uidsToReconnect) {
            ConfigBean b = cfg.getByUid(uid);
            if (b != null) {
                if (b instanceof ContentDatabase) {
                    contentDbkeysToReconnect.add(((ContentDatabase) b).getKey());
                    takenActions.put("contentDatabases", true);
                }
                else if (b instanceof de.innovationgate.wga.config.Domain) {
                    Domain domain = _wga.domain(((de.innovationgate.wga.config.Domain) b).getName());
                    if (domain != null) {
                        contentDbkeysToReconnect.addAll(domain.getDatabaseKeys());
                        takenActions.put("contentDatabases", true);
                        if (((de.innovationgate.wga.config.Domain) b).getPersonalisation() != null) {
                            persDomainsToReconnect.add(domain.getCore().getUID());
                            takenActions.put("domainPersDatabases", true);
                        }
                    }
                }
                else if (b instanceof DatabaseServer) {
                    for (ContentDatabase db : cfg.getContentDatabases()) {
                        if (((DatabaseServer) b).getUid().equals(db.getDbServer())) {
                            contentDbkeysToReconnect.add(db.getKey());
                            takenActions.put("contentDatabases", true);
                        }
                    }
                    for (de.innovationgate.wga.config.Domain d : cfg.getDomains()) {
                        if (d.getPersonalisation() != null && ((DatabaseServer) b).getUid().equals(d.getPersonalisation().getDbServer())) {
                            persDomainsToReconnect.add(d.getUid());
                            takenActions.put("domainPersDatabases", true);
                        }
   
                    }
                }
                else {
                    unmatchedIds.add(uid);
                }
            }
            else if (_wga.getCore().getContentdbs().containsKey(uid)) {
                contentDbkeysToReconnect.add(uid);
                if (uid.startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                    takenActions.put("pluginDatabases", true);
                }
                else {
                    takenActions.put("contentDatabases", true);
                }
            }
            else {
                unmatchedIds.add(uid);
            }
        }
        
        for (String dbKey : contentDbkeysToReconnect) {
            _wga.getCore().removeContentDB(dbKey);
        }
        for (String domain : persDomainsToReconnect) {
            _wga.getCore().removePersonalisationDB(domain);
        }
        
        if (reconnect) {
            if (Boolean.TRUE.equals(takenActions.get("domainPersDatabases"))) {
                _wga.getCore().updateConfig();                    
            }
            else {
                if (Boolean.TRUE.equals(takenActions.get("pluginDatabases"))) {
                    _wga.getCore().updatePlugins();
                }
                if (Boolean.TRUE.equals(takenActions.get("contentDatabases"))) {
                    _wga.getCore().updateContentDBs();
                }
            }
        }
        
        return unmatchedIds;
        
    }

    /**
     * Updates the server configuration with the given configuration. Also dropping connections to the databases belonging to the given config UIDs.
     * If the databases are still configured after update of the configuration they will subsequently get connected again.
     * @param cfg The new configuration
     * @param uidsToReconnect UIDs of config entities. Allowed are config UIDs of domains, database servers and content databases, also database keys.
     */
    @CodeCompletion
    public void updateConfig(final WGAConfiguration cfg, final List<String> uidsToReconnect) throws WGException {
        
        ServerManagementTask<Object> updateConfigTask = new ServerManagementTask<Object>() {
            
            @Override
            protected Object execute() throws Exception {
                try {
                    dropDatabasesForUIDs(uidsToReconnect, false);
                    _wga.getCore().saveWgaConfiguration(cfg);
                }
                catch (ConfigValidationException e) {
                    
                    getLog().error("Configuration update failed because of validation errors:");
                    Iterator<ValidationError> errors = e.getValidationErrors().iterator();
                    while (errors.hasNext()) {
                        ValidationError validationError = (ValidationError) errors.next();
                        getLog().error(validationError.getMessage());    
                    }
                }
                catch (Exception e) {
                    getLog().error("Exception updating configuration", e);
                }
                return null;
            }
        };
        
        updateConfigTask.runWithExceptions();
        
    }
    
    /**
     * Updates the server configuration with the given configuration.
     * @param cfg The new configuration
     */
    @CodeCompletion
    public void updateConfig(final WGAConfiguration cfg) throws WGException {
        updateConfig(cfg, null);
    }
    
    /**
     * Reconnects database belonging to the given config UIDs
     * @param uidsToReconnect UIDs of config entities. Allowed are config UIDs of domains, database servers and content databases, also database keys.
     * @throws WGException
     */
    @CodeCompletion
    public List<String> reconnect(final List<String> uidsToReconnect) throws WGException {
        
        ServerManagementTask<List<String>> updateConfigTask = new ServerManagementTask<List<String>>() {
            @Override
            protected List<String> execute() throws Exception {
                List<String> unmatchedIds = dropDatabasesForUIDs(uidsToReconnect, true);
                return unmatchedIds;
            }
        };
        
        return updateConfigTask.runWithExceptions();
        
    }
    
    /**
     * Performs plugin operations on the current WGA plugin set
     * @param ops List of operations
     * @throws WGException
     */
    @CodeCompletion
    public void performPluginOperations(final List<WorkspaceOperation> ops) throws WGException {
        
        ServerManagementTask<Object> task = new ServerManagementTask<Object>() {
            @Override
            protected Object execute() throws Exception {
                synchronized (_wga.getCore()) {
                    WGAPluginSet pluginSet = _wga.getCore().getPluginSet();
                    pluginSet.performOperations(ops);
                    pluginSet.save();
                    _wga.getCore().updatePlugins();
                }
                return null;
            }
        };
        
        task.runWithExceptions();
        
    }

    /**
     * Starts a generic problem occasion for OpenWGA non-design-related problems 
     * @param refClass The reference class containing the problem-causing code. The problem labels will need to be put into a properties file "ClassName_problems.properties" in its package folder
     * @param scopeObject An object representing the scope of this problem. Allowed are: {@link App}, {@link DataSource}, {@link Database}, {@link WGDatabase}, {@link Domain}
     * @param baseKey The base key of this problem occasion. The labels of problem keys will start with this base key
     * @param clear Whether old problems of an equal occasion should now be cleared
     * @return The occasion object
     */
    public ServerOccasion startProblemOccasion(Class<?> refClass, Object scopeObject, String baseKey, boolean clear) {
        ServerOccasion occ = new ServerOccasion(refClass, scopeObject, baseKey, clear);
        if (clear) {
            getProblemRegistry().clearProblemOccasion(occ);
        }
        return occ;
    }

    /**
     * Starts a generic problem occasion for OpenWGA non-design-related problems. This method variant creates an occasion of global scope and automatically clears existing problems of equal occasions. 
     * @param refClass The reference class containing the problem-causing code. The problem labels will need to be put into a properties file "ClassName_problems.properties" in its package folder
     * @param baseKey The base key of this problem occasion. The labels of problem keys will start with this base key
     * @return The occasion object
     */
    public ServerOccasion startProblemOccasion(Class<?> refClass, String baseKey)  {
        return startProblemOccasion(refClass, this, baseKey, true);
    }
    
    /**
     * Starts a generic problem occasion for OpenWGA non-design-related problems. This method variant automatically clears existing problems of equal occasions. 
     * @param refClass The reference class containing the problem-causing code. The problem labels will need to be put into a properties file "ClassName_problems.properties" in its package folder
     * @param scopeObject An object representing the scope of this problem. Allowed are: {@link App}, {@link DataSource}, {@link Database}, {@link WGDatabase}, {@link Domain}
     * @param baseKey The base key of this problem occasion. The labels of problem keys will start with this base key
     * @return The occasion object
     */
    public ServerOccasion startProblemOccasion(Class<?> refClass, Object scopeObject, String baseKey)  {
        return startProblemOccasion(refClass, scopeObject, baseKey, true);
    }

    /**
     * Starts a generic problem occasion for OpenWGA non-design-related problems. This method variant creates an occasion of global scope. 
     * @param refClass The reference class containing the problem-causing code. The problem labels will need to be put into a properties file "ClassName_problems.properties" in its package folder
     * @param baseKey The base key of this problem occasion. The labels of problem keys will start with this base key
     * @param clear Whether old problems of an equal occasion should now be cleared
     * @return The occasion object
     */
    public ServerOccasion startProblemOccasion(Class<?> refClass, String baseKey, boolean clear) {
        return startProblemOccasion(refClass, this, baseKey, clear);
    }
    
    /**
     * Reads a server option
     * This method returns the server option value in its native data type, like determined in module registry.
     * If the server option is not determined for this runtime this method will return the options default value from the module registry.  
     * @param name Name of the server option
     * @return The native option value
     */
    public Object getServerOption(String name) {
        return _wga.getCore().readServerOptionOrDefault(name);
    }
    
    /**
     * Clears all clearable caches on this OpenWGA runtime including:
     * <ul>
     * <li>Data, user and file publishing caches on all web applications and data sources
     * <li>WebTML cache
     * <li>TMLScript code cache
     * <li>Design file cache
     * </ul>
     * This should be run in a master session.
     * This method will return without doing anything if another thread is already clearing caches.
     * @return Descriptions of caches whose clearing have raised errors
     * @throws WGException 
     */
    public List<String> clearAllCaches() throws WGException {
        
        if (_clearCacheTaskLock.tryLock()) {
            try {
                return _clearCacheTask.runWithExceptions();
            }
            finally {
                _clearCacheTaskLock.unlock();
            }
        }
        else {
            return Collections.emptyList();
        }
    }

    /**
     * Returns the default encoding used for HTTP communication on this server
     */
    public String getDefaultHttpEncoding() {
        return _wga.getCore().getCharacterEncoding();
    }
    
    /**
     * Returns if the server runs in development mode, so is likely started in OpenWGA developer studio
     */
    public boolean isDevelopmentMode() {
        return WGACore.isDevelopmentModeEnabled();
    }
    
    /**
     * Returns if the current server is a master node (either a standalone server or the master in a cluster)
     * which should run operations that only one node in a cluster should perform  
     */
    public boolean isMasterNode() {
        return _wga.getCore().isRunSingleNodeFunctionalities();
    }
    
    /**
     * Waits until the currently thrown app events are all processed
     */
    public void waitForAppEvents() {
        _wga.getCore().getEventManager().waitForEvents();
    }


}
