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
package de.innovationgate.webgate.api;
import java.io.File;
import java.lang.reflect.Field;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import net.sf.ehcache.CacheException;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.config.Configuration;
import net.sf.ehcache.config.DiskStoreConfiguration;

import org.apache.commons.httpclient.HttpClient;
import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.auth.AuthModuleFactory;
import de.innovationgate.webgate.api.auth.DefaultAuthModuleFactory;
import de.innovationgate.webgate.api.mail.WGMailService;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.webgate.api.workflow.WGDefaultWorkflowEngine;
import de.innovationgate.webgate.api.workflow.WGWorkflowEngine;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * Entry class to use the WGAPI in any program. Via this singleton class all databases are initially opened.
 */
public class WGFactory {
    	
	/**
	 * System property to turn off the usage of an event thread. To do that set the system property: de.innovationgate.wgapi.eventthread := false
	 */
	public static final String SYSPROPERTY_EVENTTHREAD = "de.innovationgate.wgapi.eventthread";

	private static boolean _useEventThread = true;
	private static File tempDir = new File(System.getProperty("java.io.tmpdir"));
	private static WGFactory _instance = new WGFactory();
	private static Logger _logger = Logger.getLogger("wga.api");
	private static ClassLoader _implementationLoader = Thread.currentThread().getContextClassLoader();
    private static AuthModuleFactory _authModuleFactory = new DefaultAuthModuleFactory();
    private static ModuleRegistry _moduleRegistry = new ModuleRegistry();
    
    private Map<String,UserAccessFilter> _userAccessFilters = new ConcurrentHashMap<String, UserAccessFilter>();
    private static WGMailService _mailService = null;
    private static MimetypeDeterminationService _mimetypeDeterminationService = new DefaultMimetypeDeterminationService();
    private static Class<? extends WGWorkflowEngine> _defaultWorkflowEngine = WGDefaultWorkflowEngine.class;
    
    private static HttpClientFactory _httpClientFactory = new HttpClientFactory() {

        public HttpClient createHttpClient() {
            return new HttpClient();
        }
        
    };


    /**
     * Returns the module registry to use by the WGAPI
     */
    public static ModuleRegistry getModuleRegistry() {
        return _moduleRegistry;
    }

    /**
     * Sets the module registry to use by the WGAPI
     */
    public static void setModuleRegistry(ModuleRegistry moduleRegistry) {
        _moduleRegistry = moduleRegistry;
    }
    
    /**
     * Returns a class that is able to serialize revision information of the given class.
     * @param revClass The java class in which revisions are served
     * @throws WGAPIException
     * @deprecated Since database revisions are always instances of {@link WGDatabaseRevision} now having their own serialisation/deserialisation methods
     */
    public static WGDatabaseRevisionSerializer getRevisionSerializer(Class<?> revClass) throws WGAPIException {
        return new WGDatabaseRevisionSerializer() {
            @Override
            public String toPersistentForm(Comparable<?> revision) throws Exception {
                return ((WGDatabaseRevision) revision).serialize();
            }

            @Override
            public Comparable<?> fromPersistentForm(String revisionStr) throws Exception {
                return WGDatabaseRevision.deserialize(revisionStr);
            }
        };
    }
    
    private Map<Class<? extends MetaInfoProvider>,Map<String,MetaInfo>> _metainfos_by_doctype;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgdocument;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgdesigndocument;   
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgcontent;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgstructentry;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgarea;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgcontenttype;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgcssjsmodule;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgfilecontainer;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wglanguage;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wgtmlmodule;
    
    /**
     * map for metadata-framework of  wgapi
     *   NOTE: ignore IDE Warning: "This fields are unused" --> accessed via reflection in getMetaInfo()
     */
    private Map<String,MetaInfo> _metainfos_wguserprofile;
		
	static {
		
		String eventThreadStr = (String) System.getProperty(SYSPROPERTY_EVENTTHREAD);
		if (eventThreadStr != null) {
			_useEventThread = Boolean.valueOf(eventThreadStr).booleanValue();
			getLogger().info("WGAPI: Event thread is turned " + (_useEventThread ? "ON" : "OFF"));
		}
		
	}
	
	private List<WGDatabase> _dbs = new ArrayList<WGDatabase>();
	
	private boolean _databaseBackendMaintenanceEnabled = false;
	private WGEventThread _eventThread = null;
    private CacheManager _cacheManager = null;
    private int _cacheIndex = 0;

    private Map<String, MetaInfo> _metainfos_wgfilemetadata;

    private static ThreadLocal<Boolean> _isEventThread = new ThreadLocal<Boolean>();	
	
	/**
	 * Returns true, if the current thread is the WGAPI event thread
	 */
	public static boolean isEventThread() {
		Boolean is = (Boolean) _isEventThread.get();
		if (is != null) {
			return is.booleanValue();
		}
		else {
			return false;
		}
	}	
	
	private WGFactory() {
	    Map<Class<? extends MetaInfoProvider>,Map<String,MetaInfo>> metainfos = new HashMap<Class<? extends MetaInfoProvider>, Map<String,MetaInfo>>();
	    
        // initialize metadata framework
        _metainfos_wgdocument = Collections.unmodifiableMap(gatherMetaInfos(WGDocument.class));
        metainfos.put(WGDocument.class, _metainfos_wgdocument);
        
        _metainfos_wgdesigndocument = Collections.unmodifiableMap(gatherMetaInfos(WGDesignDocument.class));
        metainfos.put(WGDesignDocument.class, _metainfos_wgdesigndocument);
        
        _metainfos_wgcontent = Collections.unmodifiableMap(gatherMetaInfos(WGContent.class));
        metainfos.put(WGContent.class, _metainfos_wgcontent);
        
        _metainfos_wgstructentry = Collections.unmodifiableMap(gatherMetaInfos(WGStructEntry.class));
        metainfos.put(WGStructEntry.class, _metainfos_wgstructentry);
        
        _metainfos_wgarea = Collections.unmodifiableMap(gatherMetaInfos(WGArea.class));
        metainfos.put(WGArea.class, _metainfos_wgarea);
        
        _metainfos_wgcontenttype = Collections.unmodifiableMap(gatherMetaInfos(WGContentType.class));
        metainfos.put(WGContentType.class, _metainfos_wgcontenttype);
        
        _metainfos_wgcssjsmodule = Collections.unmodifiableMap(gatherMetaInfos(WGCSSJSModule.class));
        metainfos.put(WGCSSJSModule.class, _metainfos_wgcssjsmodule);
        
        _metainfos_wgfilecontainer = Collections.unmodifiableMap(gatherMetaInfos(WGFileContainer.class));
        metainfos.put(WGFileContainer.class, _metainfos_wgfilecontainer);
        
        _metainfos_wglanguage = Collections.unmodifiableMap(gatherMetaInfos(WGLanguage.class));
        metainfos.put(WGLanguage.class, _metainfos_wglanguage);
        
        _metainfos_wgtmlmodule = Collections.unmodifiableMap(gatherMetaInfos(WGTMLModule.class));
        metainfos.put(WGTMLModule.class, _metainfos_wgtmlmodule);
        
        _metainfos_wguserprofile = Collections.unmodifiableMap(gatherMetaInfos(WGUserProfile.class));
        metainfos.put(WGUserProfile.class, _metainfos_wguserprofile);
        
        _metainfos_wgfilemetadata = Collections.unmodifiableMap(gatherMetaInfos(WGFileMetaData.class));
        metainfos.put(WGFileMetaData.class, _metainfos_wgfilemetadata);
        
        _metainfos_by_doctype = Collections.unmodifiableMap(metainfos);
	}
    
    
    /**
     * retrieves all metainfos of the given class
     * @param c
     * @return Map of meta infos
     */
    private Map<String,MetaInfo> gatherMetaInfos(Class<? extends MetaInfoProvider> c) {
        Map<String,MetaInfo> map = new HashMap<String, MetaInfo>();
        Field[] fields = c.getFields();
        for (int i=0; i < fields.length; i++) {
            Field field = fields[i];
            if (field.getName().startsWith("META_")) {
                try {
                    String metaName = (String) field.get(null);
                    String metaInfoFieldName = "METAINFO_" + field.getName().substring(5,field.getName().length());
                    Field metaInfoField = c.getField(metaInfoFieldName);
                    MetaInfo metaInfo = (MetaInfo) metaInfoField.get(null);
                    // check info
                    if (!metaName.equals(metaInfo.getName())) {
                        throw new RuntimeException("Unable to initialize metadata-framework. MetaInfo for metafield '" + field.getName() + "' of class ' " + c.getName() + "' is invalid. Name mismatch!.");
                    }
                    metaInfo.setDefiningClass(c);
                    map.put(metaName, metaInfo);
                    
                } catch (IllegalAccessException e) {
                    throw new RuntimeException("Unable to initialize metadata-framework." , e);
                }
                catch (SecurityException e) {
                    throw new RuntimeException("Unable to initialize metadata-framework." , e);
                }
                catch (NoSuchFieldException e) {
                    throw new RuntimeException("Unable to initialize metadata-framework. MetaInfo for metafield '" + field.getName() +"' of class '" + c.getName() + "' not found.", e);
                }
            }
        }
        return map;
    }
    
    /**
     * Returns the MetaInfo for given name of the given class
     * @param name The metadata field name
     * @param c The implementation class of the document type
     * @throws WGSystemException if the metaDataFramework cannot be accessed
     */
    public MetaInfo getMetaInfo(String name, Class<? extends MetaInfoProvider> c) throws WGSystemException {
        return getMetaInfos(c).get(name.toUpperCase());
    }
    
    /**
     * returns the MetaInfos Map for the given class
     * @param c Class 
     * @return map - MetaInfos mapped by metaName
     * @throws WGSystemException
     */
    public Map<String,MetaInfo> getMetaInfos(Class<? extends MetaInfoProvider> c) throws WGSystemException {
        
        // Redirect class to query for subclasses
        if (c.equals(WGScriptModule.class)) {
            c = WGCSSJSModule.class;
        }
        try {
            return _metainfos_by_doctype.get(c);
        }
        catch (IllegalArgumentException e) {
            throw new WGSystemException("Unable to access metadata-framework.", e);
        }
        catch (SecurityException e) {
            throw new WGSystemException("Unable to access metadata-framework.", e);
        }
    }

	
	/**
	 * Set if the current thread is the WGA event thread
	 */
	protected static void setEventThread(boolean value) {
		_isEventThread.set(new Boolean(value));
	}	
	
	/**
	 * Returns the singleton instance of the factory
	 */
	public static WGFactory getInstance() {
		return _instance;
	}
	
	/* (Kein Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	public void finalize() {
		if (this._eventThread != null) {
			this._eventThread.stop();
			this._eventThread = null;
		}
		
	}
	
	/**
	 * Opens a WGA database initially.
	 * @param server The database server that this database resides on
	 * @param strType The type of the database, i.e. the database implementation class to use
	 * @param strPath The path of the database. Varies by implementation.
	 * @param strUserName The user name to use as master login.
	 * @param strUserPwd The password for the master login.
	 * @param options Additional creation options.
     * @param prepareOnly Lets the database only be prepared (test connectivity) and connected on first session
	 * @throws WGAPIException 
	 */
	public synchronized WGDatabase openDatabase(WGDatabaseServer server, String strType, String strPath, String strUserName, String strUserPwd, Map<String,String> options, boolean prepareOnly) throws WGAPIException {

		WGDatabase db = new WGDatabase(server, strType, strPath, strUserName, strUserPwd, options, prepareOnly);
		if (db.isReady() == false) {
			return null;
		}

		List<WGDatabase> newList = new ArrayList<WGDatabase>(_dbs);
		newList.add(db);
		_dbs = newList;
		
		if (this._eventThread == null && _useEventThread) {
				this._eventThread = new WGEventThread();
				this._eventThread.start();
		}

		return db;
	}
    


    private int increaseCacheIndex() {
        _cacheIndex ++;
        return _cacheIndex;
    }

    /**
     * Opens a WGA database initially, ready for work. A database master session will already be opened.
     * @param server The database server that this database resides on
     * @param strType The type of the database, i.e. the database implementation class to use
     * @param strPath The path of the database. Varies by implementation.
     * @param strUserName The user name to use as master login.
     * @param strUserPwd The password for the master login.
     * @param options Additional creation options.
     * @throws WGAPIException 
     */
    public synchronized WGDatabase openDatabase(WGDatabaseServer server, String strType, String strPath, String strUserName, String strUserPwd, Map<String,String> options) throws WGAPIException {
        return openDatabase(server, strType, strPath, strUserName, strUserPwd, options, false);
    }
    
    /**
     * Opens a WGA database initially, but only prepares it for work. The connectivity is tested but the connection terminated afterwards.
     * No database session is opened, when the database object is returned.
     * The database core remains unconnected to save resources. It will get connected on the first session that is opened.
     * @param server The database server that this database resides on
     * @param strType The type of the database, i.e. the database implementation class to use
     * @param strPath The path of the database. Varies by implementation.
     * @param strUserName The user name to use as master login.
     * @param strUserPwd The password for the master login.
     * @param options Additional creation options.
     * @throws WGAPIException 
     */
    public synchronized WGDatabase prepareDatabase(WGDatabaseServer server, String strType, String strPath, String strUserName, String strUserPwd, Map<String,String> options) throws WGAPIException {
        return openDatabase(server, strType, strPath, strUserName, strUserPwd, options, true);
    }
    

	/**
	 * Close all open sessions on all databases. 
	 * This method performs additionally cleanup operations and should be called each time working with the WGAPI has finished for the current session.
	 */
	public void closeSessions() {

	    // Close all open databases
		for (WGDatabase db : _dbs) {
			try {
			    if (db.isSessionOpen()) {
			        db.closeSession();
			    }
            }
            catch (Throwable e) {
                WGFactory.getLogger().error("Exception closing WGAPI session on database '" + db.getDbReference() + "'", e);
            }

		}

	}
	
	/**
	 * Opens sessions on all not yet opened databases with a given login.
	 * @param user
	 * @param password
	 * @throws WGAPIException 
	 */
	public void openSessions(String user, String password) throws WGAPIException {

		for (int i=0; i < _dbs.size(); i++) {
			((WGDatabase) _dbs.get(i)).openSession(user, password);
		}

	}
	
	/**
	 * Opens sessions on all not yet opened databases with master session information.
	 * @throws WGAPIException 
	 */
	public void openSessions() throws WGAPIException {
		this.openSessions(null, null);
	}
	
	/**
	 * Returns a list of all opened database objects. 
	 * This includes all databases with closed sessions, but no databases that have been permanently closed via WGDatabase.close(). 
	 */
	public List<WGDatabase> getOpenedDatabases() {
		return Collections.unmodifiableList(this._dbs);
	}

	
	/**
	 * Permanently close all opened databases.
	 */
	public void closeAll() {

		closeSessions();

		Iterator<WGDatabase> dbList = this._dbs.iterator();        
		while (dbList.hasNext()) {
			try {
                ((WGDatabase) dbList.next()).close();
            }
            catch (WGAPIException e) {
                getLogger().error("Unable to close db.", e);
            }
		}

	}

	/**
	 * Shutdown all databases and the factory
	 */
	public void shutdown() {
		
	    // Stop the event thread
        if (this._eventThread != null) {
            this._eventThread.stop();
            this._eventThread = null;
        }
        
        closeAll();
        
		this._dbs.clear();
        
        // Clear cache of auth module factory
        getAuthModuleFactory().clearCache();
        
        // Shutdown the cache manager
        if (_cacheManager != null) {
            _cacheManager.shutdown();
            _cacheManager = null;
        }
        
	}
	/**
	 * Returns the logger object to be used by all objects inside the WGAPI
	 * @return Logger
	 */
	public static Logger getLogger() {
		return _logger;
	}
	
	protected synchronized void removeDatabase(WGDatabase db) {
		
		List<WGDatabase> newList = new ArrayList<WGDatabase>(_dbs);
		newList.remove(db);
		_dbs = newList;
		
	}

	/**
	 * Returns the class loader to load WGAPI implementations
	 */
	public static ClassLoader getImplementationLoader() {
		return _implementationLoader;
	}

	/**
	 * Sets a class loader that is used to load WGAPI implementation classes
	 */
	public static void setImplementationLoader(ClassLoader loader) {
		_implementationLoader = loader;
	}

    /**
     * Returns the directory for temporary files used by the WGAPI.
     * Defaults to the directory unter system property java.io.tmpdir.
     */
    public static File getTempDir() {
        return tempDir;
    }
    /**
     * Sets the directory for temporary files used by the WGAPI. WGA must have full access rights for this directory.
     */
    public static void setTempDir(File tempDir) {
        
        if (!tempDir.exists() || !tempDir.isDirectory()) {
            return;
        }
        
        WGFactory.tempDir = tempDir;
    }

    /**
     * Returns the Factory for Authentication Modules used by databases of this factory.
     */
    public static AuthModuleFactory getAuthModuleFactory() {
        return _authModuleFactory;
    }

    /**
     * Sets the Factory for Authentication Modules used by databases of this factory
     */
    public static void setAuthModuleFactory(AuthModuleFactory authModuleFactory) {
        _authModuleFactory = authModuleFactory;
    }
    
    /**
     * Returns the EHCache Cache Manager used by this WGFactory and its databases
     * @throws CacheException
     */
    public synchronized CacheManager getCacheManager() throws CacheException {
        
        if (_cacheManager == null) {
            Configuration config = new Configuration();
            
            DiskStoreConfiguration diskConfig = new DiskStoreConfiguration();
            diskConfig.setPath(getTempDir().getPath());
            config.addDiskStore(diskConfig);
            
            CacheConfiguration defaultConfig = new CacheConfiguration();
            defaultConfig.setMaxEntriesLocalHeap(10000);
            defaultConfig.setEternal(false);
            defaultConfig.setOverflowToDisk(false);
            defaultConfig.setTimeToIdleSeconds(120);
            defaultConfig.setTimeToLiveSeconds(120);
            defaultConfig.setDiskPersistent(false);
            defaultConfig.setDiskExpiryThreadIntervalSeconds(120);
            config.addDefaultCache(defaultConfig);
            config.setName(WGFactory.class.getName() + "#CacheManager");
            
            _cacheManager = new CacheManager(config);
        }
        return _cacheManager;
        
        
    }

   
    /**
     * Tool method for creating database servers when using WGAPI as library (rather than from inside WGA).
     * The created server will have no uid or title.
     * @param <T>
     * @param serverClass The class of the server to create
     * @param options Options for the database server
     * @return A newly created database server
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws WGAPIException
     */
    public <T extends WGDatabaseServer> T createDatabaseServer(Class<T> serverClass, Map<String, String> options) throws InstantiationException, IllegalAccessException, WGAPIException {
        T server = serverClass.newInstance();
        server.init(null, null, options);
        return server;
    }

    /**
     * Returns the default workflow engine to use
     */
    public static Class<? extends WGWorkflowEngine> getDefaultWorkflowEngine() {
        return _defaultWorkflowEngine;
    }

    /**
     * Sets the default workflow engine to use by the WGAPI
     */
    public static void setDefaultWorkflowEngine(Class<? extends WGWorkflowEngine> defaultWorkflowEngine) {
        _defaultWorkflowEngine = defaultWorkflowEngine;
    }

    /**
     * Returns the mail service that the WGAPI uses, for example for sending workflow notification mails
     */
    public static WGMailService getMailService() {
        return _mailService;
    }

    /**
     * Sets the mail service to use in WGAPI
     */
    public static void setMailService(WGMailService mailService) {
        _mailService = mailService;
    }

    /**
     * Returns a service for determining file mimetypes
     */
    public static MimetypeDeterminationService getMimetypeDeterminationService() {
        return _mimetypeDeterminationService;
    }

    /**
     * Returns a service for determining file mimetypes
     */
    public static void setMimetypeDeterminationService(MimetypeDeterminationService mimetypeDeterminationService) {
        _mimetypeDeterminationService = mimetypeDeterminationService;
    }
    
    /**
     * Returns the factory use to create HttpClient instances
     */
    public static HttpClientFactory getHttpClientFactory() {
        return _httpClientFactory;
    }

    /**
     * set the factory use to create HttpClient instances in WGA
     */
    public static void setHttpClientFactory(HttpClientFactory httpClientFactory) {
        _httpClientFactory = httpClientFactory;
    }
    
    /**
     * Registers a user access filter for use on all databases
     * @param filter The filter to add
     * @return The uid given to the filter, needed to apply it on a session
     */
    public synchronized String addUserAccessFilter(UserAccessFilter filter) {
        String uid = UUID.randomUUID().toString();
        _userAccessFilters.put(uid, filter);
        return uid;
    }
    
    /**
     * Returns a registered user access filter by its uid
     * @param filterUid The uid
     */
    public UserAccessFilter getUserAccessFilter(String filterUid) {
        return _userAccessFilters.get(filterUid);
    }

    /**
     * Returns if daily database maintenance is generally enabled
     */
    public boolean isDatabaseBackendMaintenanceEnabled() {
        return _databaseBackendMaintenanceEnabled;
    }

    /**
     * Sets if daily database maintenance is generally enabled
     */
    public void setDatabaseBackendMaintenanceEnabled(boolean databaseMaintenanceEnabled) {
        _databaseBackendMaintenanceEnabled = databaseMaintenanceEnabled;
    }
    
    /**
     * @deprecated Because of changed semantics. Use {@link #isDatabaseBackendMaintenanceEnabled()} instead.
     */
    public boolean isDatabaseMaintenanceEnabled() {
        return isDatabaseBackendMaintenanceEnabled();
    }
    
    /**
     * @deprecated Because of changed semantics. Use {@link #setDatabaseBackendMaintenanceEnabled(boolean)} instead.
     */
    public void setDatabaseMaintenanceEnabled(boolean databaseMaintenanceEnabled) {
        setDatabaseBackendMaintenanceEnabled(databaseMaintenanceEnabled);
    }

    public WGEventThread getEventThread() {
        return _eventThread;
    }

    
}
