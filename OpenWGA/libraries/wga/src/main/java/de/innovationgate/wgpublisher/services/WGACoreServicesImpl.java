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
package de.innovationgate.wgpublisher.services;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.zip.ZipInputStream;

import javax.activation.DataSource;
import javax.activation.FileDataSource;
import javax.activation.URLDataSource;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSelectInfo;
import org.apache.commons.vfs2.FileSelector;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.io.ByteArrayDataSource;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.wga.common.beans.DesignDefinition;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.common.beans.csconfig.v1.PublisherOption;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wgaservices.WGACoreServices;
import de.innovationgate.wgaservices.WGAServiceException;
import de.innovationgate.wgaservices.types.DatabaseInformation;
import de.innovationgate.wgaservices.types.DatabaseServerInfo;
import de.innovationgate.wgaservices.types.FSDesignResourceState;
import de.innovationgate.wgaservices.types.PluginInfo;
import de.innovationgate.wgaservices.types.RemoteSession;
import de.innovationgate.wgaservices.types.Version;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAVersion;
import de.innovationgate.wgpublisher.design.OverlayDesignProvider;
import de.innovationgate.wgpublisher.design.OverlayStatus;
import de.innovationgate.wgpublisher.design.WGADesign;
import de.innovationgate.wgpublisher.design.WGADesignManager;
import de.innovationgate.wgpublisher.design.WGADesignSource;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignManager;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignProvider;
import de.innovationgate.wgpublisher.design.fs.FileSystemDesignSource;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.plugins.ActivatePluginOperation;
import de.innovationgate.wgpublisher.plugins.InstallPluginOperation;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.plugins.WGAPluginSet;
import de.innovationgate.wgpublisher.plugins.WorkspaceOperation;


public class WGACoreServicesImpl extends WGAServicesImpl implements WGACoreServices {
	
	private static final Logger LOG = Logger.getLogger("wga.core-service");
	
	public WGACoreServicesImpl(WGACore core, WGAWebServicesContextProvider contextProvider) {
		super(core, contextProvider);
	}

	public RemoteSession adminLogin(String user, String pwd) throws WGAServiceException {
		HttpServletRequest request = getRequest();
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");          
        }
       
        if (!_core.isAdminLogin(user, pwd, request)) {
            throw new WGAServiceException("Administrative login is invalid");
        }
        
        return new RemoteSession(null, user, pwd);
	}



	public String getDesignPath(RemoteSession session, String dbKey) throws WGAServiceException {

        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}
		
		WGDatabase db = retrieveAndOpenDB(session, dbKey);
		if (db.isSessionOpen()) {
			WGDesignProvider provider = db.getDesignProvider();
			if (provider instanceof FileSystemDesignProvider) {
				return ((FileSystemDesignProvider)provider).getDesignPath();
			} else {
				return null;
			}
		}
		return null;
	}

	private boolean isAdminSession(RemoteSession session) {
		if (session != null) {
			return _core.isAdminLogin(session.getUsername(), session.getPassword(), getRequest());
		} else {
			return false;
		}
	}
	
	private boolean isAdminServiceEnabled() {
		return _core.isAdministrativePort(getRequest().getLocalPort());
	}

	public synchronized void installPlugins(RemoteSession session, List<DataSource> plugins) throws WGAServiceException {
		if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}
				
		try {
			WGAPluginSet pluginSet = _core.getPluginSet();
			List<WorkspaceOperation> operations = new ArrayList<WorkspaceOperation>();
			Iterator<DataSource> pluginIt = plugins.iterator();
			
			while (pluginIt.hasNext()) {
				InstallPluginOperation operation = pluginSet.loadPluginToWorkspace(pluginIt.next().getInputStream());
				operations.add(operation);
			}
			pluginSet.performOperations(operations);
			pluginSet.save();
			
			operations.clear();
			_core.updatePlugins();
		} catch (Exception e) {
			throw new WGAServiceException("Plugin installation failed.", e);
		} 

	}

	public List<FSDesignResourceState> retrieveFSDesignResourceState(RemoteSession session, String path) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}
		
		WGADesignSource source = _core.getDesignManager().getDesignSources().get(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
		if (source instanceof FileSystemDesignSource) {
			FileSystemDesignSource fsSource = (FileSystemDesignSource)source;
			try {
				fsSource.getDir().refresh();
				FileObject base = fsSource.getDir().resolveFile(path);
				if (base.exists()) {
					return createStates(base, base);					
				} 
			} catch (Exception e) {				
				throw new WGAServiceException(e);
			}			
		}
		return new ArrayList<FSDesignResourceState>();
	}

	private List<FSDesignResourceState> createStates(FileObject base, FileObject file) throws NoSuchAlgorithmException, IOException {
		List<FSDesignResourceState> states = new ArrayList<FSDesignResourceState>();
		FSDesignResourceState state = new FSDesignResourceState();
		String path = computeRelativePath(base, file);
		state.setPath(path);
		states.add(state);
		if (file.getType() ==  FileType.FOLDER) {
			state.setType(FSDesignResourceState.TYPE_FOLDER);
			long lastModified = 0; 
			for (FileObject child : file.getChildren()) {
				List<FSDesignResourceState> childStates = createStates(base, child);
				for (FSDesignResourceState childState : childStates) {
					states.add(childState);
					lastModified = Math.max(lastModified, childState.getLastmodified());
				} 
			}
			state.setLastmodified(lastModified);
		} else {			
			state.setType(FSDesignResourceState.TYPE_FILE);
			state.setLastmodified(file.getContent().getLastModifiedTime());
			state.setMd5sum(WGUtils.createMD5HEX(file.getContent().getInputStream()));
		}
		return states;
	}

	private String computeRelativePath(FileObject base, FileObject file) throws FileSystemException {
		String basePath = base.getURL().getPath();
		String currentPath = file.getURL().getPath();
		String path = currentPath.substring(basePath.length());
		if (path.startsWith("/")) {
			if (path.length() > 1) {
				path = path.substring(1);
			} else {
				path = "";
			}			
		}
		return path;
	}

	public DataSource retrieveFSDesignResourceContent(RemoteSession session, FSDesignResourceState state) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}
		
		WGADesignSource source = _core.getDesignManager().getDesignSources().get(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
		if (source instanceof FileSystemDesignSource) {
			FileSystemDesignSource fsSource = (FileSystemDesignSource)source;
			try {
				fsSource.getDir().refresh();
				FileObject resource = fsSource.getDir().resolveFile(state.getPath());
				
				String basePath = fsSource.getDir().getURL().getPath();
				String resourcePath = resource.getURL().getPath();
				if (!resourcePath.startsWith(basePath)) {
					throw new WGAServiceException(new IllegalArgumentException("Illegal design resource path '" + state.getPath() + "'."));
				}
				
				if (resource.exists()) {
					if (resource.getType().equals(FileType.FOLDER)) {
						throw new WGAServiceException(new IllegalArgumentException("Cannot retrieve content of a folder."));
					}
					else { 
						return new URLDataSource(resource.getURL());
					}
				}			
			} catch (FileSystemException e) {				
				throw new WGAServiceException("Retrieving content of FSDesignResource '" + state.getPath() + "' failed.", e);
			}			
		}
		return null;
	}

	public void deleteFSDesignResource(RemoteSession session, String path) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}

		WGADesignSource source = _core.getDesignManager().getDesignSources().get(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
		if (source instanceof FileSystemDesignSource) {
			FileSystemDesignSource fsSource = (FileSystemDesignSource)source;
			try {
				fsSource.getDir().refresh();
				FileObject resource = fsSource.getDir().resolveFile(path);
				
				String basePath = fsSource.getDir().getURL().getPath();
				String resourcePath = resource.getURL().getPath();
				if (!resourcePath.startsWith(basePath)) {
					throw new WGAServiceException(new IllegalArgumentException("Illegal design resource path '" + path + "'."));
				}
				
				if (resource.exists()) {
					resource.delete(new FileSelector() {

						public boolean includeFile(FileSelectInfo fileInfo) throws Exception {
							return true;
						}

						public boolean traverseDescendents(FileSelectInfo fileInfo) throws Exception {
							return true;
						}
						
					});
					clearDesignFileCache(fsSource, fsSource.getDir().getName().getRelativeName(resource.getName()));
				}
			} catch (FileSystemException e) {
				throw new WGAServiceException("Deleting FSDesignResource '" + path + "' failed.", e);
			}
		}
	}

	public void updateFSDesignResource(RemoteSession session, String path, DataSource content, long lastModified) throws WGAServiceException {
		
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}
		
		LOG.info("core-services: Update design resource: " + path);
		
		WGADesignSource source = _core.getDesignManager().getDesignSources().get(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
		if (source instanceof FileSystemDesignSource) {
			FileSystemDesignSource fsSource = (FileSystemDesignSource)source;
			try {
				fsSource.getDir().refresh();
				FileObject resource = fsSource.getDir().resolveFile(path);
				
				String basePath = fsSource.getDir().getURL().getPath();
				String resourcePath = resource.getURL().getPath();
				if (!resourcePath.startsWith(basePath)) {
					throw new WGAServiceException(new IllegalArgumentException("Illegal design resource path '" + path + "'."));
				}

				resource.createFile();
				OutputStream out = resource.getContent().getOutputStream();
				InputStream in = content.getInputStream();
				WGUtils.inToOut(in, out, 1024);
				out.close();
				in.close();
				resource.getContent().setLastModifiedTime(lastModified);
				
				clearDesignFileCache(fsSource, fsSource.getDir().getName().getRelativeName(resource.getName()));
				
			} catch (FileSystemException e) {
				throw new WGAServiceException("Updating FSDesignResource '" + path + "' failed.", e);
			} catch (IOException e) {
				throw new WGAServiceException("Updating FSDesignResource '" + path + "' failed.", e);
			}
		}
	}

	private void clearDesignFileCache(FileSystemDesignSource fsSource, String relativeName) throws WGAServiceException {

	    try {
            int firstPathPos = relativeName.indexOf(fsSource.getDir().getName().SEPARATOR);
            String designDir;
            if (firstPathPos != -1) {
                designDir = relativeName.substring(0, firstPathPos);
            }
            else {
                designDir = relativeName;
            }
            WGADesign design = fsSource.getDesign(designDir);
            if (design != null) {
                _core.getDesignFileCache().flushGroup(design.createDesignReference().toString());
            }
        }
        catch (Exception e) {
            throw new WGAServiceException("Exception clearing design file cache for resource " + relativeName, e);
        }
        
    }

    public void mkFSDesignDir(RemoteSession session, String path) throws WGAServiceException {
		if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
		
		if (!isAdminSession(session)) {
			throw new WGAServiceException("You need an administrative login to access this service.");
		}
		
		WGADesignSource source = _core.getDesignManager().getDesignSources().get(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
		if (source instanceof FileSystemDesignSource) {
			FileSystemDesignSource fsSource = (FileSystemDesignSource)source;
			try {
				fsSource.getDir().refresh();
				FileObject resource = fsSource.getDir().resolveFile(path);
				
				String basePath = fsSource.getDir().getURL().getPath();
				String resourcePath = resource.getURL().getPath();
				if (!resourcePath.startsWith(basePath)) {
					throw new WGAServiceException(new IllegalArgumentException("Illegal design resource path '" + path + "'."));
				}

				resource.createFolder();
			} catch (FileSystemException e) {
				throw new WGAServiceException("Creating of FSDesignDir '" + path + "' failed.", e);
			}
		}
		
	}
	
	public DataSource getWGAConfiguration(RemoteSession session) throws WGAServiceException {
	    if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        try {
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            WGAConfiguration.write(_core.getWgaConfiguration(), out);
            return new ByteArrayDataSource(out.toByteArray(), WGAConfiguration.class.getName(), "text/xml");
        }
        catch (Exception e) {
            throw new WGAServiceException("Cannot serialize wga configuration.", e);
        }
        
	}

    public List<DatabaseServerInfo> retrieveContentStoreDatabaseServers(RemoteSession session) throws WGAServiceException {
            if (!isAdminServiceEnabled()) {
                throw new WGAServiceException("Administrative services are disabled");
            }
            
            if (!isAdminSession(session)) {
                throw new WGAServiceException("You need an administrative login to access this service.");
            }
            
            List<DatabaseServerInfo> infos = new ArrayList<DatabaseServerInfo>();
            try {
                Iterator<WGDatabaseServer> servers = _core.getDatabaseServers().values().iterator();
                while (servers.hasNext()) {
                    WGDatabaseServer server = servers.next();                
                    Iterator<ModuleDefinition> csTypes = server.getContentStoreTypes().iterator();
                                        
                    if (csTypes.hasNext()) {
                        DatabaseServerInfo info = new DatabaseServerInfo();
                        info.setUid(server.getUid());
                        info.setTitle(server.getTitle(Locale.ENGLISH));
                        infos.add(info);
                        
                        while (csTypes.hasNext()) {
                            ModuleDefinition csType = csTypes.next();                        
                            if (server.isDatabaseTypeCreatable((Class<? extends WGDatabaseCore>)csType.getImplementationClass())) {
                                info.getCreateableContentStoreImplemenations().add(csType.getImplementationClass().getName());                            
                            }
                        }
                    }                    
                }
            }
            catch (Exception e) {
                throw new WGAServiceException("Cannot serialize wga configuration.", e);
            }
            return infos;
    }

    public void createDatabase(RemoteSession session, DatabaseServerInfo dbServerInfo, String implClassName, Map<String,String> options) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        Iterator<WGDatabaseServer> servers = _core.getDatabaseServers().values().iterator();
        WGDatabaseServer requestedServer = null;
        while (servers.hasNext()) {
            WGDatabaseServer server = servers.next(); 
            if (server.getUid().equals(dbServerInfo.getUid())) {
                requestedServer = server;
            }
        }
        
        if (requestedServer != null) {
            try {
                requestedServer.createDatabase((Class<? extends WGDatabaseCore>) _core.getLibraryLoader().loadClass(implClassName), options);
            }
            catch (Exception e) {
                throw new WGAServiceException(e);
            }
        }
        
    }
    
    public List<DatabaseInformation> getAvailableDatabases(RemoteSession session, DatabaseServerInfo dbServerInfo, String implClassName) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        List<DatabaseInformation> result = new ArrayList<DatabaseInformation>();
        
        Iterator<WGDatabaseServer> servers = _core.getDatabaseServers().values().iterator();
        WGDatabaseServer requestedServer = null;
        while (servers.hasNext()) {
            WGDatabaseServer server = servers.next(); 
            if (server.getUid().equals(dbServerInfo.getUid())) {
                requestedServer = server;
            }
        }
        
        if (requestedServer != null) {
            try {
                List<de.innovationgate.webgate.api.servers.DatabaseInformation> infos = requestedServer.getAvailableDatabases((Class<? extends WGDatabaseCore>) _core.getLibraryLoader().loadClass(implClassName));
                for (de.innovationgate.webgate.api.servers.DatabaseInformation info : infos) {
                    DatabaseInformation serviceInfo = new DatabaseInformation();
                    serviceInfo.setImplementationClass(info.getImplementationClass().getName());
                    serviceInfo.setOptions(info.getOptions());
                    result.add(serviceInfo);
                }
            }
            catch (Exception e) {
                throw new WGAServiceException(e);
            }
        } 
        
        return result;
    }

    public DataSource createContentStoreDump(RemoteSession session, String dbKey, final boolean includeACL) throws WGAServiceException {
        return createContentStoreDump(session, dbKey, includeACL, false);
    }
    
    public DataSource createContentStoreDump(RemoteSession session, String dbKey, final boolean includeACL, final boolean includeSystemAreas) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        try {
            final WGDatabase db = retrieveAndOpenDB(session, dbKey);
            if (db != null && db.isSessionOpen()) {
                return new DataSource() {
                    
                    public OutputStream getOutputStream() throws IOException {
                        return null;
                    }
                    
                    public String getName() {
                        return "Dump '" + db.getDbReference() + "'";
                    }
                    
                    public InputStream getInputStream() throws IOException {
                        try {
                            return _core.dumpContentStore(db, "", true, _core.getLog(), includeACL, includeSystemAreas);
                        }
                        catch (WGAPIException e) {
                            IOException ioe = new IOException(e.getMessage());
                            ioe.setStackTrace(e.getStackTrace());
                           throw ioe;
                        }
                    }
                    
                    public String getContentType() {
                        return "application/zip";
                    }
                };                
            } else {
                throw new WGAServiceException("Unable to open database '" + dbKey + "'.");
            }
        }
        catch (Exception e) {
            throw new WGAServiceException("Import of content store dump failed for database '" + dbKey + "'.", e);
        }
    }

    public void importContentStoreDump(RemoteSession session, DataSource csDump, String dbKey, boolean includeACL) throws WGAServiceException {
        importContentStoreDump(session, csDump, dbKey, includeACL, false);
    }
    
    public void importContentStoreDump(RemoteSession session, DataSource csDump, String dbKey, boolean includeACL, boolean includeSystemAreas) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        try {
            WGDatabase db = retrieveAndOpenDB(session, dbKey);
            if (db != null && db.isSessionOpen()) {
                _core.importContentStoreDump(new ZipInputStream(csDump.getInputStream()), db, _core.getLog(), includeACL, includeSystemAreas);
            } else {
                throw new WGAServiceException("Unable to open database '" + dbKey + "'.");
            }
            // reconnect cs
            _core.removeContentDB(dbKey);
            _core.updateContentDBs();
        }
        catch (Exception e) {
            throw new WGAServiceException("Import of content store dump failed for database '" + dbKey + "'.", e);
        }
    }

    public void updateWGAConfiguration(RemoteSession session, DataSource wgaConfiguration) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        try {
            WGAConfiguration configBean = WGAConfiguration.read(wgaConfiguration.getInputStream());
            _core.saveWgaConfiguration(configBean);
        }
        catch (Exception e) {
            throw new WGAServiceException("Update of wga configuration failed.", e);
        }
    }

    public List<PluginInfo> getPluginInformation(RemoteSession session) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        List<PluginInfo> result = new ArrayList<PluginInfo>();
        
        for (WGAPlugin plugin : _core.getPluginSet().getPlugins()) {
            PluginInfo info = new PluginInfo();
            info.setUniqueName(plugin.getPluginID().getUniqueName());
            info.setVersion(toServiceVersionBean(plugin.getPluginID().getVersion()));
            info.setActive(plugin.isActive());
            info.setDefaultPlugin(plugin.isDefaultPlugin());
            info.setPlatformPlugin(plugin.isPlatformPlugin());
            info.setValid(plugin.isValid());
            info.setDeveloperPlugin(plugin.isDirectory());            
            info.setMinimumWGAVersion(convertToServicesVersionBean(plugin.getCsConfig().getPluginConfig().getMinimumWGAVersion()));
            info.setMiniumJavaVersion(convertToServicesVersionBean(plugin.getCsConfig().getPluginConfig().getMinimumJavaVersion()));
            info.setInstallationKey(plugin.getInstallationKey());
            PublisherOption overlaySupport = plugin.getCsConfig().findPublisherOption(PublisherOption.OPTION_OVERLAY_SUPPORT);
            if (overlaySupport != null) {
                info.setOverlaySupport(overlaySupport.getValue());
            }            
            info.setTitle(plugin.getCsConfig().getPluginConfig().getTitle());
            info.setDescription(plugin.getCsConfig().getPluginConfig().getDescription());
            info.setVendor(plugin.getCsConfig().getPluginConfig().getVendor());
            info.setWebHomepage(plugin.getCsConfig().getPluginConfig().getWebHomepage());
            result.add(info);
        }
        
        return result;
        
    }

    public Version getWGAVersion(RemoteSession session) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        
        Version version = new Version(WGAVersion.WGAPUBLISHER_MAJOR_VERSION, WGAVersion.WGAPUBLISHER_MINOR_VERSION, 
                WGAVersion.WGAPUBLISHER_MAINTENANCE_VERSION, WGAVersion.WGAPUBLISHER_PATCH_VERSION, WGAVersion.WGAPUBLISHER_BUILD_VERSION);
        
        return version;
    }
    
    public DataSource downloadPlugin(RemoteSession session, PluginInfo pluginInfo) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
                
        try {
            WGAPluginSet pluginSet = _core.getPluginSet();
            WGAPlugin plugin = pluginSet.getPluginByUniqueName(pluginInfo.getUniqueName());
            if (plugin != null) {
                File file = plugin.getPluginFile();
                if (file != null && file.isFile()) {
                    return new FileDataSource(file);
                }
            }
        } catch (Exception e) {
            throw new WGAServiceException("Plugin download failed.", e);
        } 
        
        return null;
    }
    
    public void activatePlugin(RemoteSession session, PluginInfo pluginInfo) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
                
        try {
            WGAPluginSet pluginSet = _core.getPluginSet();
            // create plugin id from info
            PluginID id = new PluginID();
            id.setUniqueName(pluginInfo.getUniqueName());
            id.setVersion(toConfigVersionBean(pluginInfo.getVersion()));
            WGAPlugin plugin = pluginSet.getPluginByID(id);
            if (plugin != null) {
                if (!plugin.isActive()) {
                    List<WorkspaceOperation> ops = new ArrayList<WorkspaceOperation>();
                    ops.add(new ActivatePluginOperation(id, WGAPluginSet.UPDATESTRATEGY_UPDATE_KEEP_DATA));
                    pluginSet.performOperations(ops);
                }
            }
        } catch (Exception e) {
            throw new WGAServiceException("Activation of plugin '" + pluginInfo.getUniqueName() + "' failed.", e);
        } 
    }
    
    public void deactivatePlugin(RemoteSession session, PluginInfo pluginInfo) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
                
        try {
            WGAPluginSet pluginSet = _core.getPluginSet();
            // create plugin id from info
            PluginID id = new PluginID();
            id.setUniqueName(pluginInfo.getUniqueName());
            id.setVersion(toConfigVersionBean(pluginInfo.getVersion()));
            WGAPlugin plugin = pluginSet.getPluginByID(id);
            if (plugin != null) {
                if (plugin.isActive()) {
                    pluginSet.deactivatePlugin(plugin);
                }
            }
        } catch (Exception e) {
            throw new WGAServiceException("Deactivation of plugin '" + pluginInfo.getUniqueName() + "' failed.", e);
        } 
    }
    
    private static Version toServiceVersionBean(de.innovationgate.wga.common.beans.csconfig.v1.Version version) {
        Version serviceVersion = new Version();
        serviceVersion.setMajorVersion(version.getMajorVersion());
        serviceVersion.setMinorVersion(version.getMinorVersion());
        serviceVersion.setMaintenanceVersion(version.getMaintenanceVersion());
        serviceVersion.setPatchVersion(version.getPatchVersion());
        serviceVersion.setBuildVersion(version.getBuildVersion());
        return serviceVersion;
    }
    
    private static de.innovationgate.wga.common.beans.csconfig.v1.Version toConfigVersionBean(Version version) {
        de.innovationgate.wga.common.beans.csconfig.v1.Version configVersion = new de.innovationgate.wga.common.beans.csconfig.v1.Version();
        configVersion.setMajorVersion(version.getMajorVersion());
        configVersion.setMinorVersion(version.getMinorVersion());
        configVersion.setMaintenanceVersion(version.getMaintenanceVersion());
        configVersion.setPatchVersion(version.getPatchVersion());
        configVersion.setBuildVersion(version.getBuildVersion());
        return configVersion;
    }
    
    public List<String> getConnectedContentDatabases(RemoteSession session) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
                
        try {
            List<String> dbs = new ArrayList<String>();
            dbs.addAll(_core.getContentdbs().keySet());
            return dbs;
        } catch (Exception e) {
            throw new WGAServiceException("Failed to retrieve connected content dbs.", e);
        } 
    }

    public void initializeOverlay(RemoteSession session, PluginInfo pluginInfo, String designFolder) throws WGAServiceException {

        StandardFileSystemManager fsManager = new StandardFileSystemManager();
        try {
            fsManager.init();
            if (!isAdminServiceEnabled()) {
                throw new WGAServiceException("Administrative services are disabled");
            }
            
            if (!isAdminSession(session)) {
                throw new WGAServiceException("You need an administrative login to access this service.");
            }
            
            WGAPlugin basePlugin = _core.getPluginSet().getPluginsByInstallationKey().get(pluginInfo.getInstallationKey());
            if (basePlugin == null) {
                throw new WGAServiceException("No plugin is installed with installation key " + pluginInfo.getInstallationKey());
            }
            WGDatabase pluginDB = _core.getContentdbs().get(basePlugin.buildDatabaseKey());
            if (pluginDB == null)  {
                throw new WGAServiceException("Plugin database not connected for plugin " + basePlugin.getIdentification());   
            }
            
            FileSystemDesignSource fsDesignSource = (FileSystemDesignSource) _core.getDesignManager().getDesignSources().get(WGAConfiguration.UID_DESIGNSOURCE_FILESYSTEM);
            WGADesign overlayDesign = fsDesignSource.getDesign(designFolder);
            if (overlayDesign == null) {
                throw new WGAServiceException("File Directory Design '" + designFolder + "' does not exist");
            }
            
            String designFolderLocation = fsDesignSource.getDesignLocation(overlayDesign);
            
            FileObject designFolderObj = fsManager.resolveFile(designFolderLocation);
            if (!designFolderObj.exists() || !designFolderObj.getType().equals(FileType.FOLDER)) {
                throw new WGAServiceException("Design folder '" + designFolder + "' cannot be found or is no folder");
            }
            
            // Determine design encoding of folder to import to
            DesignDefinition designDef = null;
            FileObject designDefFile = designFolderObj.resolveFile("design.xml");
            if (designDefFile.exists()) {
                designDef = DesignDefinition.load(designDefFile);
            }
            
            CSConfig csConfig = null;
            FileObject csConfigFile = designFolderObj.resolveFile("files/system/csconfig.xml");
            if (csConfigFile.exists()) {
                csConfig = CSConfig.load(csConfigFile);
            }
            
            String designEncoding = FileSystemDesignManager.determineDesignEncoding(designDef, csConfig);
            
            // Trigger overlay init/uprade process
            OverlayStatus status = FileSystemDesignProvider.determineOverlayStatus((FileSystemDesignProvider) pluginDB.getDesignProvider(), basePlugin.getPluginID(), designFolderObj, designEncoding, _core.getLog(), _core.getDesignFileValidator());
            FileSystemDesignProvider.upgradeOverlay((FileSystemDesignProvider) pluginDB.getDesignProvider(), basePlugin.getPluginID(), status, designFolderObj, designEncoding, _core.getLog(), _core.getDesignFileValidator());
            
            
            // Disconnect consumer apps
            WGADesignManager designManager = _core.getDesignManager();
            for (WGDatabase consumerDb : _core.getContentdbs().values()) {
                
                WGDesignProvider provider = consumerDb.getDesignProvider();
                if (provider instanceof OverlayDesignProvider) {
                    OverlayDesignProvider overlayProvider = (OverlayDesignProvider) provider;
                    if (overlayProvider.getOverlay() instanceof FileSystemDesignProvider) {
                        FileSystemDesignProvider fsProvider = (FileSystemDesignProvider) overlayProvider.getOverlay();
                        if (fsProvider.getBaseFolder().equals(designFolderObj)) {
                            _core.removeContentDB(consumerDb.getDbReference());
                        }
                    }
                }
            }
            
            // Run update content dbs to reconnect
            _core.updateContentDBs();
            
        }
        catch (Exception e) {
        	if (e instanceof WGAServiceException) {
        		throw (WGAServiceException) e;
        	} else {
        	    throw new WGAServiceException("Overlay initialisation failed.", e);
        	}
        }
        finally {
            fsManager.close();
        }
        
        
    }

    
    private static Version convertToServicesVersionBean(de.innovationgate.wga.common.beans.csconfig.v1.Version version) {        
        return new Version(version.getMajorVersion(), version.getMinorVersion(), version.getMaintenanceVersion(), version.getPatchVersion(), version.getBuildVersion());
    }

    @Override
    public void setTMLScriptDebuggerEnabled(RemoteSession session, boolean enabled) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        if (enabled && !engine.isDebugEnabled()) {
            engine.enableDebugger();
        } else if (!enabled && engine.isDebugEnabled()) {
            engine.disableDebugger();
        }        
    }

    @Override
    public boolean isTMLScriptDebuggerEnabled(RemoteSession session) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        RhinoExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
        return engine.isDebugEnabled();
    }

    @Override
    public void setWebTMLCachingEnabled(RemoteSession session, boolean enabled) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        _core.setWebTMLCachingEnabled(enabled);
    }

    @Override
    public boolean isWebTMLCachingEnabled(RemoteSession session) throws WGAServiceException {
        if (!isAdminServiceEnabled()) {
            throw new WGAServiceException("Administrative services are disabled");
        }
        
        if (!isAdminSession(session)) {
            throw new WGAServiceException("You need an administrative login to access this service.");
        }
        return _core.isWebTMLCachingEnabled();
    }
}
