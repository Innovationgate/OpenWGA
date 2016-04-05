/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wgaservices;

import java.util.List;
import java.util.Map;

import javax.activation.DataSource;

import de.innovationgate.wgaservices.types.DatabaseInformation;
import de.innovationgate.wgaservices.types.DatabaseServerInfo;
import de.innovationgate.wgaservices.types.FSDesignResourceState;
import de.innovationgate.wgaservices.types.PluginInfo;
import de.innovationgate.wgaservices.types.RemoteSession;
import de.innovationgate.wgaservices.types.Version;

/**
 * internal wga service interface
 *
 */
public interface WGACoreServices extends WGAServices {

    /**
     * Logs in to the WGA server as WGA administrator.
     * An administrator login can do all tasks on the server under master login.
     * Usage of an administrator login is restricted to the administrative port if that is configured.
     * @param user The administrator user
     * @param pwd The administrator password
     * @return A remote session object for the administrator
     */
    public RemoteSession adminLogin(String user, String pwd) throws WGAServiceException;
	
    /**
     * Returns the design path of the given database
     * 'null' if database has no file system based design
     * @param session The session to use
     * @param dbKey The dbkey
     * @return design path of given db or 'null' 
     */
    public String getDesignPath(RemoteSession session, String dbKey) throws WGAServiceException;
	
    /**
     * installs the given list of plugin files
     * @param session
     * @param plugins
     * @throws WGAServiceException
     */
    public void installPlugins(RemoteSession session, List<DataSource> plugins) throws WGAServiceException; 

    /**
     * retrieves the file system design resource states for the given path
     * @param session
     * @param path - a '/' separated path rooted at the default FSDesignProvider root
     * @return the FSDesignResourceStates
     * @throws WGAServiceException
     */
    public List<FSDesignResourceState> retrieveFSDesignResourceState(RemoteSession session, String path) throws WGAServiceException;
    
    /**
     * retrieves the content of the given file system design resource
     * @param session
     * @param state
     * @throws WGAServiceException
     */
    public DataSource retrieveFSDesignResourceContent(RemoteSession session, FSDesignResourceState state) throws WGAServiceException;
    
    /**
     * updates/ overwrites the given design resource with the new content
     * @param session
     * @param path
     * @param content
     * @throws WGAServiceException
     */
    public void updateFSDesignResource(RemoteSession session, String path, DataSource content, long lastModified) throws WGAServiceException;
    
    /**
     * deletes the given design resource
     * @param session
     * @param path
     * @throws WGAServiceException
     */
    public void deleteFSDesignResource(RemoteSession session, String path) throws WGAServiceException;
    
    /**
     * creates the given design directory
     * @param session
     * @param path
     */
    public void mkFSDesignDir(RemoteSession session, String path) throws WGAServiceException;
    
    /**
     * retrieves the current wga configuration
     * @param session
     * @throws WGAServiceException
     */
    public DataSource getWGAConfiguration(RemoteSession session) throws WGAServiceException;
    
    /**
     * update the current wga configuration
     * @param session
     * @throws WGAServiceException
     */
    public void updateWGAConfiguration(RemoteSession session, DataSource wgaConfiguration) throws WGAServiceException;
    
    /**
     * retrieves a list of available database servers of the current OpenWGA runtime
     * which are able to provide content stores
     * @param session
     * @throws WGAServiceException
     */
    public List<DatabaseServerInfo> retrieveContentStoreDatabaseServers(RemoteSession session) throws WGAServiceException;
    
    /**
     * creates a content store on the given database server with the given name
     * this operation will only create the database and not modify the OpenWGA configuration
     * @param session
     * @param dbServerInfo
     * @param implClassName
     * @param options
     * @throws WGAServiceException
     */
    public void createDatabase(RemoteSession session, DatabaseServerInfo dbServerInfo, String implClassName, Map<String,String> options) throws WGAServiceException;
    
    /**
     * imports the given content store dump in the database specified by dbKey
     * @param session
     * @param dbKey
     * @throws WGAServiceException
     */
    public void importContentStoreDump(RemoteSession session, DataSource csDump, String dbKey, boolean includeACL) throws WGAServiceException;
    
    /**
     * creates a content store dump of the given database
     * @param session
     * @param dbKey
     * @param includeACL
     * @throws WGAServiceException
     */
    public DataSource createContentStoreDump(RemoteSession session, String dbKey, boolean includeACL) throws WGAServiceException;

    /**
     * imports the given content store dump in the database specified by dbKey
     * @param session
     * @param dbKey
     * @param includeACL
     * @param includeSystemAreas
     * @throws WGAServiceException
     */
    public void importContentStoreDump(RemoteSession session, DataSource csDump, String dbKey, boolean includeACL, boolean includeSystemAreas) throws WGAServiceException;
    
    /**
     * creates a content store dump of the given database
     * @param session
     * @param dbKey
     * @param includeACL
     * @param includeSystemAreas
     * @throws WGAServiceException
     */
    public DataSource createContentStoreDump(RemoteSession session, String dbKey, boolean includeACL, boolean includeSystemAreas) throws WGAServiceException;
    
    /**
     * retrieves a list of available databases on given server with given type
     * @param session
     * @param dbServerInfo
     * @param implClassName
     * @throws WGAServiceException
     */
    public List<DatabaseInformation> getAvailableDatabases(RemoteSession session, DatabaseServerInfo dbServerInfo, String implClassName) throws WGAServiceException;
    
    /**
     * returns information about currently installed plugins
     * @param session
     * @throws WGAServiceException
     */
    public List<PluginInfo> getPluginInformation(RemoteSession session) throws WGAServiceException;
    
    /**
     * returns the version of wga
     * @param session
     * @throws WGAServiceException
     */
    public Version getWGAVersion(RemoteSession session) throws WGAServiceException;
    
    /**
     * retrieves the plugin file of the specified plugin
     * @param session
     * @param pluginInfo
     * @throws WGAServiceException
     */
    public DataSource downloadPlugin(RemoteSession session, PluginInfo pluginInfo) throws WGAServiceException;
    
    
    /**
     * activates the specified plugin
     * @param session
     * @param pluginInfo
     * @throws WGAServiceException
     */
    public void activatePlugin(RemoteSession session, PluginInfo pluginInfo) throws WGAServiceException;
    
    
    /**
     * deactivates the specified plugin
     * fails silently if the plugin is already disabled
     * @param session
     * @param pluginInfo
     * @throws WGAServiceException
     */
    public void deactivatePlugin(RemoteSession session, PluginInfo pluginInfo) throws WGAServiceException;
    
    /**
     * returns a list of dbkeys of current connected content databases
     * @param session
     * @throws WGAServiceException
     */
    public List<String> getConnectedContentDatabases(RemoteSession session) throws WGAServiceException;
    
    /**
     * performs an overlay initialization or update on the given plugin with the given design folder
     * @param session
     * @param pluginInfo
     * @param designFolder
     * @throws WGAServiceException
     */
    public void initializeOverlay(RemoteSession session, PluginInfo pluginInfo, String designFolder) throws WGAServiceException;
    
    /**
     * enables or disables the tmlscript debugger
     * @param enabled
     * @throws WGAServiceException
     */
    public void setTMLScriptDebuggerEnabled(RemoteSession session, boolean enabled) throws WGAServiceException;
    
    /**
     * returns the state of the tmlscript debugger
     * @throws WGAServiceException
     */
    public boolean isTMLScriptDebuggerEnabled(RemoteSession session) throws WGAServiceException;
    
    /**
     * Enables or disables WebTML caching functionality globally
     * @param session
     * @param enabled
     * @throws WGAServiceException
     */
    public void setWebTMLCachingEnabled(RemoteSession session, boolean enabled) throws WGAServiceException;
    
    /**
     * Returns the state of WebTML caching
     * @param session
     * @throws WGAServiceException
     */
    public boolean isWebTMLCachingEnabled(RemoteSession session) throws WGAServiceException;
    
}
