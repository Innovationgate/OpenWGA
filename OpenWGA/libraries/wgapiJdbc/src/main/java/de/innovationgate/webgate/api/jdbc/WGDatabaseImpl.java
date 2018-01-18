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
package de.innovationgate.webgate.api.jdbc;

import java.io.File;
import java.io.InputStream;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.management.MXBean;
import javax.management.ObjectName;

import org.apache.commons.collections.Bag;
import org.apache.commons.collections.bag.HashBag;
import org.apache.commons.collections.map.LinkedMap;
import org.apache.log4j.Logger;
import org.hibernate.CacheMode;
import org.hibernate.Criteria;
import org.hibernate.FlushMode;
import org.hibernate.Hibernate;
import org.hibernate.HibernateException;
import org.hibernate.LockMode;
import org.hibernate.LockOptions;
import org.hibernate.MappingException;
import org.hibernate.ObjectDeletedException;
import org.hibernate.ObjectNotFoundException;
import org.hibernate.Query;
import org.hibernate.QueryException;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.Session.LockRequest;
import org.hibernate.SessionBuilder;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;
import org.hibernate.UnresolvableObjectException;
import org.hibernate.cfg.Configuration;
import org.hibernate.cfg.Environment;
import org.hibernate.criterion.Restrictions;
import org.hibernate.dialect.Dialect;
import org.hibernate.engine.jdbc.ColumnNameCache;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.exception.SQLGrammarException;
import org.hibernate.id.UUIDHexGenerator;
import org.hibernate.id.enhanced.TableGenerator;
import org.hibernate.jdbc.ReturningWork;
import org.hibernate.jdbc.Work;
import org.hibernate.mapping.SimpleValue;
import org.hibernate.service.ServiceRegistryBuilder;
import org.hibernate.service.jdbc.connections.spi.ConnectionProvider;
import org.hibernate.service.spi.ServiceRegistryImplementor;
import org.hibernate.service.spi.Stoppable;
import org.hibernate.stat.Statistics;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.io.xml.Dom4JDriver;

import de.innovationgate.monitoring.JmxManager;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.MetaInfo;
import de.innovationgate.webgate.api.WGACLCore;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGClosedSessionException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGDatabaseCoreFeaturePageSequences;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureReturnHierarchyCount;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureSequenceProvider;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGPageOrderSet;
import de.innovationgate.webgate.api.WGPersonalisationDatabaseCore;
import de.innovationgate.webgate.api.WGProcedureException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGResultSetCore;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.WGWrongRevisionException;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.webgate.api.auth.MasterLoginAuthSession;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionException;
import de.innovationgate.webgate.api.jdbc.custom.JDBCConnectionProvider;
import de.innovationgate.webgate.api.jdbc.filehandling.CS3FileHandling;
import de.innovationgate.webgate.api.jdbc.filehandling.CS41FileHandling;
import de.innovationgate.webgate.api.jdbc.filehandling.CS5FileHandling;
import de.innovationgate.webgate.api.jdbc.filehandling.CS5P4ContentFileDescriptor;
import de.innovationgate.webgate.api.jdbc.filehandling.CS5P4FileHandling;
import de.innovationgate.webgate.api.jdbc.filehandling.CS5P5FileHandling;
import de.innovationgate.webgate.api.jdbc.filehandling.FileHandling;
import de.innovationgate.webgate.api.jdbc.pool.DBCPConnectionProvider;
import de.innovationgate.webgate.api.jdbc.pool.DBCPReplicationConnectionProvider;
import de.innovationgate.webgate.api.mysql.GaleraClusterTableGenerator;
import de.innovationgate.webgate.api.mysql.MySqlDatabaseServer;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.config.Database;
import de.innovationgate.wga.config.DatabaseServer;


public class WGDatabaseImpl implements WGDatabaseCore, WGPersonalisationDatabaseCore, 
		WGDatabaseCoreFeatureReturnHierarchyCount, WGDatabaseCoreFeatureSequenceProvider, WGDatabaseCoreFeaturePageSequences, 
		AuthenticationSourceListener {
    
    @MXBean
    public interface StatisticsMXBean extends Statistics {
    }
    
    public static class CSVersion {
        
        private double _version;
        private int _patchLevel;
        
        public CSVersion(double version, int patchLevel) {
            _version = version;
            _patchLevel = patchLevel;
        }

        public double getVersion() {
            return _version;
        }

        public int getPatchLevel() {
            return _patchLevel;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + _patchLevel;
            long temp;
            temp = Double.doubleToLongBits(_version);
            result = prime * result + (int) (temp ^ (temp >>> 32));
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
            CSVersion other = (CSVersion) obj;
            if (_patchLevel != other._patchLevel)
                return false;
            if (Double.doubleToLongBits(_version) != Double.doubleToLongBits(other._version))
                return false;
            return true;
        }
        
    }
    
    public class DocumentDependencyComparator implements Comparator<WGUpdateLog> {

        public int compare(WGUpdateLog o1, WGUpdateLog o2) {
            
            // Different times
            int timeDiff = o1.getDate().compareTo(o2.getDate());
            if (timeDiff != 0) {
                return timeDiff;
            }
            
            // Determine sort numbers for operations/doctypes
            int sortNr1 = determineSortNumber(o1);
            int sortNr2 = determineSortNumber(o2);
            return 2 - 1;
            
            
            
        }

        private int determineSortNumber(WGUpdateLog o) {
            
            int nr = 1;
            
           WGDocumentKey docKey = new WGDocumentKey(o.getDocumentKey());
           switch (docKey.getDocType()) {
               
               case WGDocument.TYPE_CONTENT:
                   nr = 3;
                   break;

               case WGDocument.TYPE_STRUCTENTRY:
                   nr = 2;
                   break;
           }
           
           if (o.getType() == WGUpdateLog.TYPE_DELETE) {
               nr = nr * -1;
           }
           
           return nr;
           
            
        }
        
    }
    
    private static final String CUSTOM_SEQUENCE_NAMEPREFIX = "custom_";
    
    private static final String HQLQUERY_PENDING_RELEASE_DOCS_CS5 = "content.status ='" + WGContent.STATUS_REVIEW + "' AND content.extensionData['" + WGDocument.EXTDATA_META_PREFIX + WGContent.META_PENDINGRELEASE.toLowerCase() + "'].boolean = 1";
    private static final String HQLQUERY_PENDING_RELEASE_DOCS_CS3 = "content.status ='" + WGContent.STATUS_REVIEW + "' AND content.items['" + WGContent.ITEM_PENDINGRELEASE.toLowerCase() + "'].number = 1";
    
	private static final String HQLQUERY_GET_STRUCT_BY_NAME = "from StructEntry as struct where struct.uniquename=:name";
    public static final String HQL_FETCHTYPE_LAZY = "lazy";
	public static final String HQL_FETCHTYPE_STRAIGHT = "straight";
    private static final String HQLQUERY_LAZY_PARENTCHECK = 
            "select new de.innovationgate.webgate.api.WGContentQueryResult(content.structentry.key, content.language.name, content.version, parent.key, area.name) " +
            "from Content as content " + 
            "left outer join content.structentry.parententry as parent " +
            "left outer join content.structentry.area as area " +
            "where ";
    
    private static final String HQLQUERY_LAZY = "select new de.innovationgate.webgate.api.WGContentKey(content.structentry.key, content.language.name, content.version) from Content as content where ";
    
    
	// enable loadbalancing feature
    public static final String COPTION_LOADBALANCE = "loadbalance";

    // when loadbalancing is enabled - how long should the updating user stay on
    // the master after the session is closed
    public static final String COPTION_MASTERPERSISTENCE_TIMEOUT = "masterPersistenceTimeout";
    
    /**
     * Object of type {@link HotPatch} to perform a DDL hotpatch
     */
    public static final String COPTION_HOTPATCH = "HotPatch";

    public static final String DBTYPE = "jdbc/wgacontentstore";

    public static final String HIBERNATE_V3_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.hbm.xml";
    public static final String HIBERNATE_V41_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.optimizedFileHandling.hbm.xml";
    public static final String HIBERNATE_V5_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.v5.hbm.xml";
    public static final String HIBERNATE_V5_P2_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.v5p2.hbm.xml";
    public static final String HIBERNATE_V5_P3_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.v5p3.hbm.xml";
    public static final String HIBERNATE_V5_P4_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.v5p4.hbm.xml";
    public static final String HIBERNATE_V5_P5_MAPPING_FILE = "de/innovationgate/webgate/api/jdbc/Mapping.v5p5.hbm.xml";
    
    public static final String DBOPTION_MAPPINGFILE = "mapping.file";

    public static final String DBOPTION_MAPPINGRESOURCE = "mapping.resource";
    
    public static final String DBOPTION_HISTORYLOG_IDRANGE = "historylog.idrange";

    public static final String DBOPTION_USE_GALERA_OPTIMIZATIONS = "mysql.galera";

    public static final String COPTION_ANONYMOUS = "AnonymousAccessLevel";
    
    public static final String COPTION_DISTINCTFILECONTENTS = "DistinctFileContents";     
    
    public static final String BACKENDSERVICE_UPGRADE_FILE_STORAGE = WGDatabaseImpl.class.getName() + ":UpgradeFileStorage";
    
    private static final String HQLQUERY_STRAIGHT = "select content from Content as content where ";
    
    private static final String HQLQUERY_UPDATE_RELATIONS = "update from ContentRelation as relation set relation.target = :target where relation.targetstructentry = :structentry and relation.targetlanguage = :language";
    
    public static final String COPTION_HQL_FETCH_TYPE = "HQLFetchType";
    
    public static final String COPTION_HQL_LAZY_PARENTCHECK = "HQLLazyParentCheck";
    
    public static final String DBMETA_PATCH_LEVEL = WGDatabase.EXTDATA_PATCH_LEVEL;
    
    public static final Integer CURRENT_PATCH_LEVEL = 1;
    
    protected SessionFactory _sessionFactory = null;
    protected SessionBuilder _sessionBuilder = null;

    private WGDatabase _db;

    private String _path;

    private boolean _hqlLazyByDefault = true;
    private ThreadLocal<SessionStatus> _sessionStatus = new ThreadLocal<SessionStatus>();
    
    protected boolean _saveIsolationActive = false;
    private ACLImpl _aclImpl = new ACLImpl(this);

    
    public static final long DEFAULT_MASTERPERSISTENCE_TIMEOUT = 1000 * 10;
    // when loadbalancing is enabled - how long should the updating user stay on
    // the master after the session is closed
	private static long _masterPersistenceTimeout = DEFAULT_MASTERPERSISTENCE_TIMEOUT;
	

	
	private Map<String,DBUpdate> _dbUpdatesByUser = new ConcurrentHashMap<String,DBUpdate>();
	private Map<String,TableGenerator> _generatorCache = new ConcurrentHashMap<String, TableGenerator>();
	
    protected double _ddlVersion;
    /**
     * Chooses if the "query paging" feature of the optimized file handling
     * should be disabled (which it is by default)
     */
    public static final String COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING = "OptimizedFileHandling.DisableQueryPaging";
    protected static final String NATIVESQL_BOOLEAN_TRUE = "booleanTrue";
    public static final String DEFAULT_MAXOPENPREPAREDSTATEMENTS = "-1";

    
    public static class V5FastAccessKey {
        
        public V5FastAccessKey(int type, String id) {
            super();
            this.type = type;
            this.id = id;
        }
        private int type;
        private String id;
        public int getType() {
            return type;
        }
        public String getId() {
            return id;
        }
        
        @Override
        public String toString() {
            return WGDocument.doctypeNumberToName(this.type) + "/" + this.id;
        }
        
    }

	public class LogConflict {
	    
	    private List _logs = new ArrayList();
	    private WGUpdateLog _listedLog = null;

        public WGUpdateLog getListedLog() {
            return _listedLog;
        }

        public void setListedLog(WGUpdateLog listedLog) {
            _listedLog = listedLog;
        }

        public List getLogs() {
            return _logs;
        }
	    
	    
	    
	}

	public static class SessionStatus {
	    private Session _session = null;
	    private boolean _inSaveOperation = false;
	    
        public Session getSession() {
            return _session;
        }
        public void setSession(Session session) {
            _session = session;
        }
        protected boolean isInSaveOperation() {
            return _inSaveOperation;
        }
        protected void setInSaveOperation(boolean inSaveOperation) {
            _inSaveOperation = inSaveOperation;
        }
    };

	
	// bean to manage dbUpdatesByUser
	private class DBUpdate {
		private String _user;
		private Date _started;
		private boolean _sessionClosed;
		private WGSessionContext _sessionContext;

		public DBUpdate(WGSessionContext sessionContext) {
			_sessionContext = sessionContext;
			_user = _sessionContext.getUser();
			_started = new Date();
			_sessionClosed = false;
		}

		public boolean isSessionClosed() {
			return _sessionClosed;
		}
		public void setSessionClosed(boolean sessionClosed) {
			_sessionClosed = sessionClosed;
		}

		public Date getStarted() {
			return _started;
		}

		public String getUser() {
			return _user;
		}

		public boolean isInProgress() {
			if (!isSessionClosed()) {
    			return true;
            }
            else if (_user.equals(WGDatabase.ANONYMOUS_USER)) {
    			return _sessionContext == _db.getSessionContext(); 			
            }
            else if ((System.currentTimeMillis() - _masterPersistenceTimeout) >= _started.getTime()) {
    			return false;
            }
            else {
    			return true;
    		}
    	} 
	};



    private static Map<Integer, Class<? extends MainEntity>> _typeToObject = new HashMap<Integer, Class<? extends MainEntity>>();

    private static Map<Class<? extends MainEntity>, Integer> _objectToType = new HashMap<Class<? extends MainEntity>, Integer>();
    private ConnectionProvider _connProvider = null;
    protected CSVersion _csVersion;
    private FileHandling _fileHandling;

    private Configuration _conf;

    private volatile boolean _ugradeFileStorageRunning = false;

    private boolean _hqlLazyParentCheck = true;

    private JmxManager _jmxManager;

    static {
        _typeToObject.put(new Integer(WGDocument.TYPE_AREA), Area.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_CONTENT), Content.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_CONTENTTYPE), ContentType.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_CSSJS), CSSJSModule.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_FILECONTAINER), FileContainer.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_LANGUAGE), Language.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_STRUCTENTRY), StructEntry.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_TML), TMLModule.class);
        _typeToObject.put(new Integer(WGDocument.TYPE_USERPROFILE), UserProfile.class);

        _objectToType.put(Area.class, new Integer(WGDocument.TYPE_AREA));
        _objectToType.put(Content.class, new Integer(WGDocument.TYPE_CONTENT));
        _objectToType.put(ContentType.class, new Integer(WGDocument.TYPE_CONTENTTYPE));
        _objectToType.put(CSSJSModule.class, new Integer(WGDocument.TYPE_CSSJS));
        _objectToType.put(FileContainer.class, new Integer(WGDocument.TYPE_FILECONTAINER));
        _objectToType.put(Language.class, new Integer(WGDocument.TYPE_LANGUAGE));
        _objectToType.put(StructEntry.class, new Integer(WGDocument.TYPE_STRUCTENTRY));
        _objectToType.put(TMLModule.class, new Integer(WGDocument.TYPE_TML));
        _objectToType.put(UserProfile.class, new Integer(WGDocument.TYPE_USERPROFILE));
    }

    public Session getSession() throws WGAPIException {
        SessionStatus status = getSessionStatus();
        Session session = status.getSession();
        if (session != null) {
            return session;
        }
        else {
            throw new WGClosedSessionException();
        }
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#close()
     */
    public void close() throws WGAPIException {
        
        if (getSessionStatus().getSession() != null) {
            closeSession();
        }
        
        try {
            if (_sessionFactory != null) {
                _sessionFactory.close();
                _sessionFactory = null;
            }
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error closing hibernate session factory", e);
        }
        
        if (_connProvider != null) {
            if (_connProvider instanceof Stoppable) {
                ((Stoppable) _connProvider).stop();
            }
            _connProvider = null;
        }

        
        _dbUpdatesByUser.clear();
        
        if (_jmxManager != null) {
            _jmxManager.unregister();
        }
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#closeSession()
     */
    public void closeSession() throws WGAPIException {
        try {
            
            Session session = getSessionStatus().getSession();
            if (session != null) {
                Transaction trans = session.getTransaction();
                if (trans != null && trans.isActive()) {
                    trans.rollback();
                }
            }
        }
        catch (Throwable e) {
            WGFactory.getLogger().error("Exception rolling back transaction on session close", e);
        }
            
            
        try {
            // set session closed flag on dbupdate if present
            if (_db.isSessionOpen()) {
                String user = _db.getSessionContext().getUser();
                DBUpdate update = (DBUpdate) _dbUpdatesByUser.get(user);
                if (update != null) {
                	update.setSessionClosed(true);
                }
            }
        }
        catch (Throwable e) {
            WGFactory.getLogger().error("Setting session closed flag transaction on session close", e);
        }
            
        try {
            SessionStatus sessionStatus = (SessionStatus) _sessionStatus.get();
            if (sessionStatus != null) {
                Session session = sessionStatus.getSession();
                if (session != null) {
                    session.disconnect();
                    session.close();
                }
    
            }
            
            _sessionStatus.remove();
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error closing hibernate session", e);
        }
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#createContent(WGStructEntry,
     *      WGLanguage, String)
     */
    public WGDocumentCore createContent(WGStructEntry structEntry, WGLanguage language, String title, int version) throws WGAPIException {

        Content newContent = new Content();

        // Additionally set object references, so they can be requested
        // instantly
        StructEntry structEntryEntity = (StructEntry) getEntity(structEntry);
        newContent.setStructentry(structEntryEntity);
        structEntryEntity.getContent().add(newContent);

        Language languageEntity = (Language) getEntity(language);
        newContent.setLanguage(languageEntity);

        // Initialize other data
        newContent.setVersion(new Integer(version));
        newContent.setTitle(title);
        newContent.setItems(new HashMap());
        newContent.setFiles(new HashMap());
        newContent.setRelations(new HashMap());
        newContent.setIshiddenfrom(new ArrayList());
        newContent.setKeywords(new ArrayList());
        newContent.setWfhistory(new ArrayList());
        newContent.setVisible(new Boolean(true));

        return createDocumentImpl(newContent);

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#createCopy(WGDocumentCore)
     */
    public WGDocumentCore createCopy(WGDocumentCore originalCore) throws WGCreationException {

        WGDocumentImpl docOriginal = (WGDocumentImpl) originalCore;
        Object originalEntity = docOriginal.getEntity();

        try {
            WGDocumentImpl docCopy = null;

            if (originalEntity instanceof Content) {
                Content original = (Content) originalEntity;
                Content copy = new Content();
                copy.setTitle(original.getTitle());
                copy.setLanguage(original.getLanguage());
                copy.setStatus(original.getStatus());
                copy.setStructentry(original.getStructentry());
                copy.setVersion(original.getVersion());
                copy.setVisible(original.isVisible());
                docCopy = createDocumentImpl(copy);
                //entityCopy.save(new java.util.Date()); // Must save to append
                // other entities to it

                copy.setAuthor(original.getAuthor());
                if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                    copy.setContentclass(original.getContentclass());
                    copy.setOwner(original.getOwner());
                    copy.setCoauthors((List) cloneCollection(original.getCoauthors()));
                }
                copy.setDescription(original.getDescription());

                copy.setLinktarget(original.getLinktarget());

                copy.setUniquename(original.getUniquename());
                copy.setValidfrom(original.getValidfrom());
                copy.setValidto(original.getValidto());

                copy.setVirtuallink(original.getVirtuallink());
                copy.setVirtuallinktype(original.getVirtuallinktype());

                copy.setIshiddenfrom((List) cloneCollection(original.getIshiddenfrom()));
                copy.setKeywords((List) cloneCollection(original.getKeywords()));
                copy.setReaders((List) cloneCollection(original.getReaders()));
                copy.setWfhistory((List) cloneCollection(original.getWfhistory()));
                
                docOriginal.pushFiles(docCopy);
                
                copy.setItems(cloneContentItems(original.getItems(), copy));
                
                if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                    copy.setRelations(cloneContentRelations(original.getRelations(), copy));
                }

                if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                    copy.setExtensionData(cloneExtensionData(original.getExtensionData(), copy));
                }
                
            }
            else {
                throw new WGCreationException("Copying of document type " + WGDocument.doctypeNumberToName(originalCore.getType()) + " is not supported in this database");
            }

            return docCopy;

        }
        catch (Exception e) {
            throw new WGCreationException("Error creating entity copy", e);
        }
    }

    /**
     * @param map
     * @return
     */
    private Map cloneContentItems(Map map, Content target) {

        Map itemCopies = new HashMap();
        Iterator itemNames = map.keySet().iterator();
        Object itemName;
        ContentItem contentItem;
        ContentItem contentItemCopy;
        while (itemNames.hasNext()) {
            itemName = itemNames.next();
            contentItem = (ContentItem) map.get(itemName);
            contentItemCopy = new ContentItem();
            contentItemCopy.setDate(contentItem.getDate());
            contentItemCopy.setName(contentItem.getName());
            contentItemCopy.setNumber(contentItem.getNumber());
            contentItemCopy.setParentcontent(target);
            contentItemCopy.setText(contentItem.getText());
            contentItemCopy.setType(contentItem.getType());
            itemCopies.put(itemName, contentItemCopy);
        }

        return itemCopies;
    }
    
    /**
     * @param map
     * @return
     */
    private Map cloneContentRelations(Map map, Content target) {

        Map itemCopies = new HashMap();
        Iterator itemNames = map.keySet().iterator();
        Object relName;
        ContentRelation contentItem;
        ContentRelation contentItemCopy;
        while (itemNames.hasNext()) {
            relName = itemNames.next();
            contentItem = (ContentRelation) map.get(relName);
            contentItemCopy = new ContentRelation();
            contentItemCopy.setName(contentItem.getName());
            contentItemCopy.setGroup(contentItem.getGroup());
            contentItemCopy.setParentcontent(target);
            contentItemCopy.setTargetlanguage(contentItem.getTargetlanguage());
            contentItemCopy.setTargetstructentry(contentItem.getTargetstructentry());
            contentItemCopy.setTarget(contentItem.getTarget());
            itemCopies.put(relName, contentItemCopy);
        }

        return itemCopies;
    }



    public static Map<String,ExtensionData> cloneExtensionData(Map<String,ExtensionData> map, Entity target) {
        Map<String,ExtensionData> extensionDataCopies = new HashMap<String,ExtensionData>();
        Iterator<String> edNames = map.keySet().iterator();
        String edName;
        ExtensionData extensionData;
        ExtensionData exensionDataCopy;
        while (edNames.hasNext()) {
            edName = edNames.next();
            extensionData = (ExtensionData) map.get(edName);
            exensionDataCopy = new ExtensionData();
            exensionDataCopy.setEntity(target);
            exensionDataCopy.setDate(extensionData.getDate());
            exensionDataCopy.setName(extensionData.getName());
            exensionDataCopy.setNumber(extensionData.getNumber());            
            exensionDataCopy.setText(extensionData.getText());
            exensionDataCopy.setType(extensionData.getType());
            exensionDataCopy.setBinarySha512(extensionData.getBinarySha512());
            extensionDataCopies.put(edName, exensionDataCopy);
        }
        return extensionDataCopies;
    }
    
    /**
     * @param list
     * @return
     */
    private Collection cloneCollection(Collection col) {

        if (col == null) {
            return null;
        }

        if (col instanceof List) {
            List list = new ArrayList();
            list.addAll((List) col);
            return list;
        }
        else if (col instanceof Set) {
            Set set = new HashSet();
            set.addAll((Set) col);
            return set;
        }
        else if (col instanceof Bag) {
            Bag bag = new HashBag();
            bag.addAll((Bag) col);
            return bag;
        }
        else {
            throw new IllegalArgumentException("Cannot clone collections of type: " + col.getClass().getName());
        }

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#createDesignDocument(int,
     *      String, String)
     */
    public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAuthorisationException, WGCreationException {

        MainEntity newEntity = null;

        switch (type) {

        case WGDocument.TYPE_AREA:
            Area newArea = new Area();
            newArea.setName(name);
            newArea.setReaders(new ArrayList());
            newArea.setEditors(new ArrayList());
            newArea.setRootentries(new HashMap());
            newEntity = newArea;
            break;

        case WGDocument.TYPE_CONTENTTYPE:
            ContentType newContentType = new ContentType();
            newContentType.setName(name);
            newContentType.setAllowedpositions(new ArrayList());
            newEntity = newContentType;
            break;

        case WGDocument.TYPE_CSSJS:
            CSSJSModule newCSSJSModule = new CSSJSModule();
            newCSSJSModule.setName(name);
            if (mediaKey != null) {
                newCSSJSModule.setCodetype(mediaKey);
            }
            newEntity = newCSSJSModule;
            break;

        case WGDocument.TYPE_FILECONTAINER:
            FileContainer newFileContainer = new FileContainer();
            newFileContainer.setName(name);
            newFileContainer.setFiles(new HashMap());
            newEntity = newFileContainer;
            break;

        case WGDocument.TYPE_LANGUAGE:
            Language newLanguage = new Language();
            newLanguage.setName(name);
            newEntity = newLanguage;
            break;

        case WGDocument.TYPE_TML:
            TMLModule newTMLModule = new TMLModule();
            newTMLModule.setModulekey(new TMLModuleKey(name, mediaKey));
            newTMLModule.setDirectaccess(new Boolean(true));
            newTMLModule.setCacheable(new Boolean(false));
            newEntity = newTMLModule;
            break;
        }

        if (newEntity != null) {
            return createDocumentImpl(newEntity);
        }
        else {
            return null;
        }

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#createStructEntry(WGDocument,
     *      WGContentType)
     */
    public WGDocumentCore createStructEntry(Object key, WGDocument reference, WGContentType contentType) throws WGAPIException {

        StructEntry newStructEntry = new StructEntry();

        if (contentType != null) {
            ContentType contentTypeEntity = (ContentType) ((WGDocumentImpl) contentType.getCore()).getEntity();
            newStructEntry.setContenttype(contentTypeEntity);
        }

        if (key != null) {
            newStructEntry.setKey(String.valueOf(key));
        }
        else {
            UUIDHexGenerator idGenerator = new UUIDHexGenerator();
            newStructEntry.setKey(String.valueOf(idGenerator.generate((org.hibernate.engine.spi.SessionImplementor) getSession(), newStructEntry)));
        }

        if (reference instanceof WGArea) {
            Area area = (Area) ((WGDocumentImpl) reference.getCore()).getEntity();
            newStructEntry.setArea(area);
            if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                area.getRootentries().put(newStructEntry.getKey(), newStructEntry);
            }

        }
        else {
            StructEntry parentEntry = (StructEntry) getEntity(reference);
            newStructEntry.setParententry(parentEntry);
            if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                parentEntry.getChildentries().put(newStructEntry.getKey(), newStructEntry);
            }
        }

        newStructEntry.setReaders(new ArrayList());
        newStructEntry.setChildeditors(new ArrayList());
        newStructEntry.setPageeditors(new ArrayList());
        newStructEntry.setPublished(new HashMap());
        newStructEntry.setContent(new HashSet());
        newStructEntry.setChildentries(new HashMap());

        return createDocumentImpl(newStructEntry);

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#createUserProfile(String,
     *      int)
     */
    public WGDocumentCore createUserProfile(String name, int type) throws WGAPIException, WGAuthorisationException, WGCreationException {

        UserProfile newProfile = new UserProfile();

        if (name != null) {
            newProfile.setName(name);
        }
        else {
            UUIDHexGenerator idGenerator = new UUIDHexGenerator();
            newProfile.setName(String.valueOf(idGenerator.generate((SessionImplementor) getSession(), newProfile)));
        }
        newProfile.setType(new Integer(type));
        newProfile.setHits(new Integer(0));
        newProfile.setSessions(new Integer(0));
        newProfile.setLanguages(new ArrayList<String>());
        newProfile.setPortletkeys(new ArrayList<String>());
        newProfile.setItems(new HashMap<String,UserProfileItem>());
        newProfile.setPortlets(new HashMap<String,UserProfilePortlet>());

        return createDocumentImpl(newProfile);
    }

    /**
     * @throws WGAPIException 
     * @throws WGIllegalArgumentException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#fastAccess(Object)
     */
    public WGDocumentCore fastAccess(int type, Object key) throws WGAPIException, WGIllegalArgumentException {

        if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
          V5FastAccessKey v5Key = (V5FastAccessKey) key;
          if (v5Key.getId() != null) {
              Class clazz = getClassByType(v5Key.getType());
              MainEntity entity = (MainEntity) getSession().get(clazz, v5Key.getId());
              if (entity != null) {
                  return createDocumentImpl(entity);
              }
          }          
          return null;          
        }
        else {
            if (key instanceof String) {
                String cuid = (String) key;
                Content content = null;
                try {
                    content = (Content) getSession().get(Content.class, cuid);
                }
                catch (ObjectNotFoundException e) {
                    return null;
                }
                catch (HibernateException e) {
                    throw new WGBackendException("Error fast accessing content", e);
                }
    
                if (content != null) {
                    return createDocumentImpl(content);
                }
                else {
                    return null;
                }
            }
            else {
                throw new WGIllegalArgumentException("Parameter key must be an instance of String.");
            }
        }
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getAllContent(WGStructEntry)
     */
    public List getAllContent(WGStructEntry structEntry, boolean includeArchived) throws WGAPIException {
        
        try {
            StructEntry hentry = (StructEntry) ((WGDocumentImpl) structEntry.getCore()).getEntity();
            
            StringBuffer queryText = new StringBuffer();
            queryText.append("select content from Content as content where content.structentry = :entry");
            if (!includeArchived) {
                queryText.append(" and not content.status = 'a'");
            }
            Query query = getSession().createQuery(queryText.toString());
            query.setParameter("entry", hentry);
            Iterator contentIt = query.iterate();
    
            List contentList = new ArrayList();
            Content content = null;
            while (contentIt.hasNext()) {
                content = (Content) contentIt.next();
                if (includeArchived || content.getStatus() != WGContent.STATUS_ARCHIVE) {
                    contentList.add(createDocumentImpl(content));
                }
            }
            return contentList;
        }
        catch (HibernateException e) {
            throw new WGBackendException("Exception retrieving struct entry content", e);
        }

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getChildEntries(WGStructEntry)
     */
    public Iterator<WGDocumentCore> getChildEntries(WGStructEntry structEntry, WGPageOrderSet order) throws WGAPIException {
        StructEntry hentry = (StructEntry) ((WGDocumentImpl) structEntry.getCore()).getEntity();
        Map<String,Object> params = new HashMap<String, Object>(); 
        
        String orderClause;
        if (order != null && getCsVersion().getPatchLevel()>4) {
            orderClause = buildHqlPageOrderClause(order, params);
        }
        else {
        	if (order != null && getCsVersion().getPatchLevel()<=4) {
        		WGFactory.getLogger().warn("Order clause not allowed for CS PL < 5. Using default order.");
        	}
            orderClause = "struct.position asc, struct.title asc, struct.key asc";
        }
        
        Query query = getSession().createQuery("select struct from StructEntry as struct where struct.parententry = :entry order by " + orderClause);
        
        params.put("entry", hentry);
        for (Map.Entry<String,Object> param : params.entrySet()) {
            query.setParameter(param.getKey(), param.getValue());
        }
        
        return new StructEntryIterator(this, query);
        

    }

    protected String buildHqlPageOrderClause(WGPageOrderSet order, Map<String, Object> params) throws WGAPIException {

        List<String> hqlTerms = new ArrayList<String>();
        WGDocumentImpl langDoc = null;
        String contentLanguage = order.getContentLanguage();
        if (contentLanguage == null) {
            contentLanguage = _db.getDefaultLanguage();
        }
        
        langDoc = (WGDocumentImpl) getDesignObject(WGDocument.TYPE_LANGUAGE, contentLanguage, null);
        if (langDoc == null) {
            throw new WGIllegalArgumentException("Undefined language: " + contentLanguage);
        }
        
        int itemNameIdx=0;
        for (WGColumnSet.Term term : order.getTerms()) {
            
            String clause = null;
            
            if (Character.isUpperCase(term.getName().charAt(0))) {
                
                StringBuffer clauseTerm = new StringBuffer();
                
                WGColumnSet.ColumnMeta columnMeta = WGColumnSet.COLUMN_METAS.get(term.getName());
                if (columnMeta == null) {
                    throw new WGIllegalArgumentException("Unknown column meta name: " + term.getName());
                }
                for (Class<? extends WGDocument> loc : columnMeta.getLocations()) {
                    if (loc == WGStructEntry.class) {
                        clauseTerm.append("struct");
                    }
                    else if (loc == WGContent.class) {
                        params.put("langentity", langDoc.getEntity());
                        clauseTerm.append("struct.releasedcontent[:langentity]");
                    }
                    else if (loc == WGContentType.class) {
                        clauseTerm.append("struct.contenttype");
                    }
                    else if (loc == WGLanguage.class) {
                        params.put("langentity", langDoc.getEntity());
                        clauseTerm.append("struct.releasedcontent[:langentity].language");
                    }
                    else {
                        continue;
                    }
                    
                    clauseTerm.append(".").append(columnMeta.getMetaName().toLowerCase());
                        
                    // Metas with special data structures
                    if (loc == WGStructEntry.class && term.getName().equals("PAGEPUBLISHED")) {
                        params.put("lang", contentLanguage);
                        clauseTerm.append("[:lang]");
                    }
                    
                    clause = clauseTerm.toString();
                    break;
                    
                }
                
            }
            else {
                itemNameIdx++;
                params.put("itemname" + itemNameIdx, term.getName());
                String property = term.getFlags().containsKey("type") ? term.getFlags().get("type") : "text";
                params.put("langentity", langDoc.getEntity());
                clause = "struct.releasedcontent[:langentity].items[:itemname" + itemNameIdx + "]." + property;
            }
            
            if (clause != null) {
                if ("true".equals(term.getFlags().get("ci"))) {
                    clause = "lower(" + clause + ")";
                }
                if ("true".equals(term.getFlags().get("desc"))) {
                    clause += " desc";
                }
                else {
                    clause += " asc";
                }
                hqlTerms.add(clause);
            }
            
        }
        String completeClause = WGUtils.serializeCollection(hqlTerms, ", ");
        return completeClause;
        
    }

    protected String buildHqlContentOrderClause(String contentExpression, WGColumnSet order, Map<String,Object> params) throws WGAPIException {

        List<String> hqlTerms = new ArrayList<String>();
        WGDocumentImpl langDoc = null;
        
        int itemNameIdx=0;
        for (WGColumnSet.Term term : order.getTerms()) {
            
            String clause = null;
            
            if (Character.isUpperCase(term.getName().charAt(0))) {
                
                StringBuffer clauseTerm = new StringBuffer();
                
                WGColumnSet.ColumnMeta columnMeta = WGColumnSet.COLUMN_METAS.get(term.getName());
                if (columnMeta == null) {
                    throw new WGIllegalArgumentException("Unknown column meta name: " + term.getName());
                }
                for (Class<? extends WGDocument> loc : columnMeta.getLocations()) {
                    if (loc == WGStructEntry.class) {
                        clauseTerm.append(contentExpression).append(".structentry");
                    }
                    else if (loc == WGContent.class) {
                        clauseTerm.append(contentExpression);
                    }
                    else if (loc == WGContentType.class) {
                        clauseTerm.append(contentExpression).append(".structentry.contenttype");
                    }
                    else if (loc == WGLanguage.class) {
                        clauseTerm.append(contentExpression).append(".language");
                    }
                    else {
                        continue;
                    }
                    
                    clauseTerm.append(".").append(columnMeta.getMetaName().toLowerCase());
                        
                    // Metas with special data structures
                    if (loc == WGStructEntry.class && term.getName().equals(WGStructEntry.META_PUBLISHED)) {
                        clauseTerm.append("[content.language.name]");
                    }

                    clause = clauseTerm.toString();
                    break;
                    
                }
                
            }
            else {
                itemNameIdx++;
                params.put("itemname" + itemNameIdx, term.getName());
                String property = term.getFlags().containsKey("type") ? term.getFlags().get("type") : "text";
                clause = contentExpression + ".items[:itemname" + itemNameIdx + "]." + property;
                
            }
            
            if (clause != null) {
                if ("true".equals(term.getFlags().get("ci"))) {
                    clause = "lower(" + clause + ")";
                }
                if ("true".equals(term.getFlags().get("desc"))) {
                    clause += " desc";
                }
                else {
                    clause += " asc";
                }
                hqlTerms.add(clause);
            }
            
        }
        return WGUtils.serializeCollection(hqlTerms, ", ");
        
    }



    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getContentByKey(WGContentKey)
     */
    public WGDocumentCore getContentByKey(WGContentKey key) throws WGAPIException {

        try {
            
            Query contentQuery;
            if (key.getVersion() != 0) {
                contentQuery = getSession().createQuery("from Content as content where content.structentry.key =:structkey and content.language.name =:lang and content.version =:version");
                contentQuery.setInteger("version", key.getVersion());
            }
            else {
                contentQuery = getSession().createQuery("from Content as content where content.structentry.key =:structkey and content.language.name =:lang and content.status ='p'");
            }
            contentQuery.setString("structkey", String.valueOf(key.getStructKey()));
            contentQuery.setString("lang", key.getLanguage());

             
            Iterator it = contentQuery.iterate();

            if (it.hasNext()) {
                Content content = (Content) it.next();
                return createDocumentImpl(content);
            }
            else {
                return null;
            }
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error loading content by key", e);
        }

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getContentByName(String,
     *      String)
     */
    public WGDocumentCore getContentByName(String strName, String strLanguage) throws WGAPIException {

        strName = strName.toLowerCase();
        strLanguage = (strLanguage != null ? strLanguage.toLowerCase() : null);

        String queryStr = "from Content as content where content.status='p' and content.uniquename=:name";
        if (strLanguage != null) {
            queryStr += " and content.language.name=:lang";
        }

        Iterator contents;
        try {
            Query query = getSession().createQuery(queryStr);
            query.setString("name", strName);
            if (strLanguage != null) {
                query.setString("lang", strLanguage);
            }
            
            contents = query.iterate();
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error searching content by name", e);
        }

        Content content;
        if (contents.hasNext()) {
            content = (Content) contents.next();
            return createDocumentImpl(content);
         }

        return null;

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getDedicatedWorkflowEngine()
     */
    public Class getDedicatedWorkflowEngine() {
        return null;
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getDesignObject(int,
     *      String, String)
     */
    public WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException {

        Class designClass = getClassByType(type);        
        MainEntity design = null;
        try {
            if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                design = getDesignObjectV5(name, strMediaKey, designClass);
            }
            else {
                design = getDesignObjectV4(name, strMediaKey, designClass);        
            }
        }
        catch (ObjectNotFoundException e) {
            return null;
        }
        catch (ObjectDeletedException e) {
            return null;
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error retrieving design object", e);
        }

        if (design != null) {
            return createDocumentImpl(design);
        }
        else {
            return null;
        }

    }



    private MainEntity getDesignObjectV5(String name, String strMediaKey, Class designClass) throws WGAPIException {
        
        Criteria crit = getSession().createCriteria(designClass);
        crit.add(Restrictions.eq("name", name));
        if (designClass == TMLModule.class) {
            crit.add(Restrictions.eq("mediakey", strMediaKey));
        }
        else if (designClass == CSSJSModule.class) {
            crit.add(Restrictions.eq("codetype", strMediaKey));
        }
        
        List results = crit.list();
        if (results.size() > 0) {
            return (MainEntity) results.get(0);
        }
        else {
            return null;
        }
        
    }



    private MainEntity getDesignObjectV4(String name, String strMediaKey, Class designClass) throws WGAPIException {
        MainEntity design;
        if (designClass == TMLModule.class) {
            design = (MainEntity) getSession().get(designClass, new TMLModuleKey(name, strMediaKey));
        }
        else {
            design = (MainEntity) getSession().get(designClass, name);
        }
        return design;
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getDesignObjects(int)
     */
    public List getDesignObjects(int type) throws WGAPIException {

        String query = "from " + getClassByType(type).getName() + " as design";
        Iterator designs;
        try {
            designs = getSession().createQuery(query).iterate();
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error retrieving design objects.", e);           
        }

        List designList = new ArrayList();
        while (designs.hasNext()) {
            designList.add(createDocumentImpl((MainEntity) designs.next()));

        }

        return designList;

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getDummyContent(String)
     */
    public WGDocumentCore getDummyContent(String language) {
        return new de.innovationgate.webgate.api.fake.WGDummyContent(this._db, language);
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getRevision()
     */
    public Comparable getRevision() throws WGAPIException {

        try {
            List result;
            if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {                
                //Sequence seq = (Sequence) getSession().get(Sequence.class, "historylog_id");
                //if (seq != null) {
                    //return (seq.getValue() - 1);
                //}
                result = getSession().createQuery("select max(entry.id) from LogEntry as entry").list();
                if (result.size() > 0) {
                    Long id = (Long) result.get(0);
                    if (id != null) {
                        return id;
                    }
                } 
                return Long.MIN_VALUE;                
            }
            else {
                result = getSession().createQuery("select max(entry.logtime) from LogEntry as entry").list();
                Date lcDate = null;
                if (result.size() > 0) {
                    lcDate = (Date) result.get(0);
                }
    
                if (lcDate != null) {
                    return lcDate;
                }
                else {
                    return new Date(Long.MIN_VALUE);
                }
            }
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error retrieving historylog of database '" + getTitle() + "'", e);            
        }

    }

    /**
     * @throws WGIllegalArgumentException 
     * @throws WGAPIException 
     * @throws WGSystemException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getExtensionData(String)
     */
    public Object getExtensionData(String name) throws WGAPIException {
        
        name = name.toLowerCase().trim();
        ExtensionData md = retrieveExtensionData(name);
        
        if (md != null) {
            getSession().refresh(md);
            return WGDocumentImpl.readItemValue(this, null, md);
        }
        else {
            return null;
        }
        
    }


    private ExtensionData retrieveExtensionData(String name) throws WGAPIException {
        ExtensionData md = null;
        Criteria c = getSession().createCriteria(ExtensionData.class);
        c.add(Restrictions.eq("name", name));
        c.add(Restrictions.isNull("entity"));
        List<ExtensionData> results = c.list();
        if (results.size() > 0) {
            md = results.get(0);
        }
        return md;
    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getNewDesignsSince(Date)
     */
    public List getNewDesignsSince(Date date) {
        return null;
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getParentEntry(WGStructEntry)
     */
    public WGDocumentCore getParentEntry(WGStructEntry entry) throws WGAPIException {

        StructEntry hentry = (StructEntry) ((WGDocumentImpl) entry.getCore()).getEntity();
        StructEntry parent = (StructEntry) hentry.getParententry();
        if (parent != null) {
            try {
                return createDocumentImpl(parent);
            }
            catch (UnresolvableObjectException e) {
                WGFactory.getLogger().error("Struct entry with unresolveable parent entry: " + entry.getDocumentKey());
                // Apparently Hibernate also returns a parent StructEntry if it is no longer available
                // In the database. We catch this here silently, though there is a doc with non-integer
                // parent hierarchy here.
            }
        }
        
        return null;
            

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getRoles()
     */
    public List getRoles() {
        return Arrays.asList(new String[] { WGDatabase.ROLE_CONTENT, WGDatabase.ROLE_DESIGN, WGDatabase.ROLE_REPOSITORY, WGDatabase.ROLE_USERPROFILES });
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getRootEntries(WGArea)
     */
    public Iterator<WGDocumentCore> getRootEntries(WGArea area, WGPageOrderSet order) throws WGAPIException {

        Area harea = (Area) ((WGDocumentImpl) area.getCore()).getEntity();
        Map<String,Object> params = new HashMap<String, Object>(); 
        
        String orderClause;
        if (order != null) {
            orderClause = buildHqlPageOrderClause(order, params);
        }
        else {
            orderClause = "struct.position asc, struct.title asc, struct.key asc";
        }
        
        Query query = getSession().createQuery("select struct from StructEntry as struct where struct.area = :area order by " + orderClause);
        params.put("area", harea);
        for (Map.Entry<String,Object> param : params.entrySet()) {
            query.setParameter(param.getKey(), param.getValue());
        }

        return new StructEntryIterator(this, query);
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getStructEntryByKey(Object)
     */
    public WGDocumentCore getStructEntryByKey(Object key) throws WGAPIException {

        try {
            StructEntry entry = null;
            if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                Criteria crit = getSession().createCriteria(StructEntry.class);
                crit.add(Restrictions.eq("key", String.valueOf(key)));
                List results = crit.list();
                if (results.size() > 0) {
                    entry = (StructEntry) results.get(0);
                }
            }
            else {
                entry = (StructEntry) getSession().get(StructEntry.class, (String) key);
            }
            if (entry != null) {
                return createDocumentImpl(entry);
            }
            else {
                return null;
            }
        }
        catch (ObjectNotFoundException e) {
            return null;
        }

        catch (HibernateException e) {
            throw new WGBackendException("Error loading structentry by key", e);
        }

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTitle()
     */
    public String getTitle() {
        return _path;
    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getTypeName()
     */
    public String getTypeName() {
        return DBTYPE;
    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getUserProfile(String)
     */
    public WGDocumentCore getUserProfile(String name) throws WGAPIException {

        try {
            UserProfile profile = null;
            
            if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                Criteria q = getSession().createCriteria(UserProfile.class);
                q.add(Restrictions.eq("name", name));
                List results = q.list();
                if (results.size() > 0) {
                    profile = (UserProfile) results.get(0);
                }
            }
            else {
                profile = (UserProfile) getSession().get(UserProfile.class, name);
            }
            if (profile != null) {
                return createDocumentImpl(profile);
            }
            else {
                return null;
            }
        }
        catch (ObjectNotFoundException e) {
            return null;
        }

        catch (HibernateException e) {
            throw new WGBackendException("Error loading userprofile for name '" + name + "'.", e);
        }

    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#hasFeature(String)
     */
    public boolean hasFeature(String feature) {

        if (feature.equals(WGDatabase.FEATURE_EDITABLE) || feature.equals(WGDatabase.FEATURE_FULLCONTENTFEATURES) || feature.equals(WGDatabase.FEATURE_HIERARCHICAL)
                || feature.equals(WGDatabase.FEATURE_QUERYABLE) || feature.equals(WGDatabase.FEATURE_USE_OBJECTS_AS_REFERENCES) || feature.equals(WGDatabase.FEATURE_LASTCHANGED)
                || feature.equals(WGDatabase.FEATURE_GENERATES_STRUCTKEYS) || feature.equals(WGDatabase.FEATURE_ACCEPTS_STRUCTKEYS) || feature.equals(WGDatabase.FEATURE_MULTILANGUAGE)
                || feature.equals(WGDatabase.FEATURE_ACL_MANAGEABLE) || feature.equals(WGDatabase.FEATURE_COMPLEXVALUES) || feature.equals(WGDatabase.FEATURE_FIND_UPDATED_DOCS)
                || feature.equals(WGDatabase.FEATURE_RETRIEVE_ALL_CONTENTKEYS) || feature.equals(WGDatabase.FEATURE_QUERY_PROFILES) || feature.equals(WGDatabase.FEATURE_EXTERNAL_AUTHENTICATION)
                || feature.equals(WGDatabase.FEATURE_VALIDATE_ATTACHMENTS) || feature.equals(WGDatabase.FEATURE_SELF_PERSONALIZABLE) || feature.equals(WGDatabase.FEATURE_UNLIMITED_CORES)
                || feature.equals(WGDatabase.FEATURE_TRANSACTIONS) || feature.equals(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS) || feature.equals(WGDatabase.FEATURE_CONTENT_READ_PROTECTION)) {
            return true;
        } 
        else if (feature.equals(WGDatabase.FEATURE_LOADBALANCE)) {
            if (_db.getCreationOptions().get(COPTION_LOADBALANCE) != null && Boolean.valueOf((String) _db.getCreationOptions().get(COPTION_LOADBALANCE)).booleanValue()) {
            return true;
        	} 
        	else {
        		return false;
        	}
        }
        else if (feature.equals(WGDatabase.FEATURE_DIRECT_ENTITY_READDING)) {
            return _saveIsolationActive;
        }
        else if (feature.equals(WGDatabase.FEATURE_PROVIDE_PORTLETITEM_STORAGE)) {
            return _ddlVersion >= WGDatabase.CSVERSION_WGA5;
        }
        else if (feature.equals(WGDatabase.FEATURE_CONTENT_FILE_DERIVATES)) {
            return (_fileHandling instanceof CS5P4FileHandling);
        }
        else {
            return false;
        }

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#isMemberOfUserList(List)
     */
    public boolean isMemberOfUserList(List userList) throws WGAPIException {
        return _db.defaultIsMemberOfUserList(userList);
    }

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#open(WGDatabase,
     *      String, String, String, boolean)
     */
    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException {

        try {
        
            this._db = db;
            this._path = path;
            this._aclImpl = new ACLImpl(this);
    
    
            String jdbcDriver = (String) db.getCreationOptions().get("Driver");
            if (jdbcDriver == null) {
                jdbcDriver = (String) db.getCreationOptions().get("hibernate.connection.driver_class");
            }
    
            // Determine dll version
            _csVersion = determineCSVersion(db, jdbcDriver, path, user, pwd);
            _ddlVersion = _csVersion.getVersion();
            _fileHandling = createFileHandling();
            
            boolean useSharedPool = WGUtils.getBooleanMapValue(db.getCreationOptions(), WGDatabase.COPTION_SHAREDPOOL, true);
            if (useSharedPool && db.getCreationOptions().containsKey(Database.OPTION_PATH) && db.getServer() instanceof SharedPoolJDBCDatabaseServer) {
                SharedPoolJDBCDatabaseServer poolServer = (SharedPoolJDBCDatabaseServer) db.getServer();
                if (poolServer.isPoolAvailable(_csVersion)) {
                    try {
                        _connProvider = poolServer.createPoolConnectionProvider((String) db.getCreationOptions().get(Database.OPTION_PATH));
                        WGFactory.getLogger().info("Database '" + db.getDbReference() + "' uses the shared connection pool of database server '" + db.getServer().getTitle(Locale.getDefault()) + "'");
                    }
                    catch (WGInvalidDatabaseException e) {
                        throw e;
                    }
                    catch (Exception e) {
                        throw new WGInvalidDatabaseException("Exception connecting to shared database server pool", e);
                    }
                }
            }
            
            // Create regular connection provider if no shared one available/allowed
            if (_connProvider == null) {
                Properties props = new Properties();
                if (path.startsWith("jdbc:")) {
                    putDefaultConPoolProps(db, props);
                }
                if (user != null || pwd != null) {
                    props.put("hibernate.connection.username", WGUtils.getValueOrDefault(user, ""));
                    props.put("hibernate.connection.password", WGUtils.getValueOrDefault(pwd, ""));
                }
                String driverClass = (String) db.getCreationOptions().get("Driver");
           
                props.put(Environment.ISOLATION, String.valueOf(Connection.TRANSACTION_READ_COMMITTED));
           
                props.putAll(db.getCreationOptions());    
                try {
                    _connProvider = new JDBCConnectionProvider(path, driverClass, props, true);
                }
                catch (JDBCConnectionException e) {
                    throw new WGInvalidDatabaseException("Exception creating connection pool", e);
                }
            }
                
            // Build Session factory and builder
            buildSessionFactory(db, path, user, pwd, _csVersion, _connProvider);
            if ("true".equals(System.getProperty("de.innovationgate.wga.hibernate.enable_jmx"))) {
                _sessionFactory.getStatistics().setStatisticsEnabled(true);
                try {
                    Object statisticsMBean = Proxy.newProxyInstance(getClass().getClassLoader(), new Class<?>[] { StatisticsMXBean.class }, new InvocationHandler() {
                        @Override
                        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
                            return method.invoke(_sessionFactory.getStatistics(), args);
                        }
                    });
                    _jmxManager = new JmxManager(statisticsMBean, new ObjectName("de.innovationgate.WGAMonitor:name=Hibernate-Statistics,db=" + JmxManager.normalizeJmxKey(db.getDbReference())));
                }
                catch (Exception e2) {
                    WGFactory.getLogger().error("Exception enabling JMX for Hibernate statistics", e2);
                }
            }

            _sessionBuilder = _sessionFactory.withOptions();
            
            // Determine save isolation
            _saveIsolationActive = _ddlVersion >= WGDatabase.CSVERSION_WGA5;
            if (db.getCreationOptions().containsKey("SaveIsolation")) {
                _saveIsolationActive = Boolean.parseBoolean((String) db.getCreationOptions().get("SaveIsolation"));
            }
    
            // parse masterPersistenceTimeout
            if (db.getCreationOptions().containsKey(COPTION_MASTERPERSISTENCE_TIMEOUT)) {
            	_masterPersistenceTimeout = Long.parseLong((String)db.getCreationOptions().get(COPTION_MASTERPERSISTENCE_TIMEOUT));
            }
    
            // parse HQL query default type
            String hqlType = (String) db.getCreationOptions().get(COPTION_HQL_FETCH_TYPE);
            if (hqlType != null) {
                _hqlLazyByDefault = hqlType.equals(HQL_FETCHTYPE_LAZY);
            }
            
            String hqlLazyParentCheck =  (String) db.getCreationOptions().get(COPTION_HQL_LAZY_PARENTCHECK);
            if (hqlLazyParentCheck != null) {
                _hqlLazyParentCheck  = Boolean.parseBoolean(hqlLazyParentCheck);
            }
                
            
            // open session
            WGUserAccess accessLevel;
            try {
                accessLevel = openSession(MasterLoginAuthSession.getInstance(), pwd, true);
            }
            catch (WGUnavailableException e) {
                throw new WGInvalidDatabaseException("Error opening initial session", e);
            }
            catch (WGBackendException e) {
                throw new WGInvalidDatabaseException("Error opening initial session", e);
            }
            if (accessLevel.getAccessLevel() <= WGDatabase.ACCESSLEVEL_NOACCESS) {
                try {
                    close();
                }
                catch (WGBackendException e1) {
                    WGFactory.getLogger().error(e1);
                }
            }
            return accessLevel;
        
        }
        catch (WGInvalidDatabaseException e) {
            if (_connProvider != null) {
                if (_connProvider instanceof Stoppable) {
                    ((Stoppable) _connProvider).stop();
                }
                _connProvider = null;
            }
            throw e;
        }

    }





    protected FileHandling createFileHandling() {

        FileHandling fileHandling;
        if (_csVersion.getVersion() >= WGDatabase.CSVERSION_WGA5) {
            if (_csVersion.getPatchLevel() >= 5) {
                fileHandling = new CS5P5FileHandling();
            }
            else if (_csVersion.getPatchLevel() >= 4 && !"false".equals(_db.getCreationOptions().get(COPTION_DISTINCTFILECONTENTS))) {
                fileHandling = new CS5P4FileHandling();
            }
            else {
                fileHandling = new CS5FileHandling();
            }
        }
        else if (_csVersion.getVersion() >= WGDatabase.CSVERSION_WGA4_1) {
            fileHandling = new CS41FileHandling();
        }
        else {
            fileHandling = new CS3FileHandling();
        }
        
        fileHandling.init(this);
        return fileHandling;
        
    }





    private void buildSessionFactory(WGDatabase db, String path, String user, String pwd, CSVersion version, ConnectionProvider connProvider) throws WGInvalidDatabaseException {
        
        // Move old Hibernate2 packages to Hibernate3 packages 
        Iterator dbOptionsIt = db.getCreationOptions().keySet().iterator();
        while (dbOptionsIt.hasNext()) {
            Object key = dbOptionsIt.next();
            String value = (String) db.getCreationOptions().get(key);
            if (value != null && value.startsWith("net.sf.hibernate.")) {
                value = "org.hibernate." + value.substring(17);
                db.getCreationOptions().put(key, value);
            }
        }
        
        // Determine mapping file
        _conf = new Configuration();
        try {
            if (db.getCreationOptions().containsKey(DBOPTION_MAPPINGFILE)) {
                File mappingFile = new File((String) db.getCreationOptions().get(DBOPTION_MAPPINGFILE));
                if (!mappingFile.exists() || !mappingFile.isFile()) {
                    throw new WGInvalidDatabaseException("Configured mapping file '" + db.getCreationOptions().get(DBOPTION_MAPPINGFILE) + "' does not exist or is no valid file.");
                }
                _conf.addFile(mappingFile);
            }
            else if (db.getCreationOptions().containsKey(DBOPTION_MAPPINGRESOURCE)) {
                _conf.addResource((String) db.getCreationOptions().get(DBOPTION_MAPPINGRESOURCE), this.getClass().getClassLoader());
            }
            else if (_ddlVersion == WGDatabase.CSVERSION_WGA5) {
                int patchLevel = version.getPatchLevel();
                String mappingFile = getCS5PatchLevelMappingFile(patchLevel);
                _conf.addResource(mappingFile, this.getClass().getClassLoader());
            }
            else if (_ddlVersion == WGDatabase.CSVERSION_WGA4_1) {
                _conf.addResource(HIBERNATE_V41_MAPPING_FILE, this.getClass().getClassLoader());
            } else {
            	_conf.addResource(HIBERNATE_V3_MAPPING_FILE, this.getClass().getClassLoader());
            }
        }
        catch (MappingException e) {
            throw new WGInvalidDatabaseException("Exception parsing hibernate mapping", e);
        }
        
        // Some options
        _conf.buildMappings();
        if (db.getCreationOptions().containsKey(DBOPTION_HISTORYLOG_IDRANGE)) {
            Integer idRange = Integer.parseInt((String) db.getCreationOptions().get(DBOPTION_HISTORYLOG_IDRANGE)); 
            Properties generatorProps = ((SimpleValue) _conf.getClassMapping(LogEntry.class.getName()).getIdentifier()).getIdentifierGeneratorProperties();
            generatorProps.setProperty("optimizer", "pooled-lo");
            generatorProps.setProperty("increment_size", String.valueOf(idRange));
        }
   
        Properties props = new Properties();
        props.putAll(db.getCreationOptions());
        _conf.addProperties(props);
        
        ServiceRegistryBuilder registryBuilder = new ServiceRegistryBuilder();
        registryBuilder.applySettings(_conf.getProperties());
        registryBuilder.addService( ConnectionProvider.class, connProvider );
        ServiceRegistryImplementor serviceRegistry = (ServiceRegistryImplementor) registryBuilder.buildServiceRegistry();
        
        _conf.buildMappings();
        
        // Some options
        if("true".equals(db.getCreationOptions().get(DBOPTION_USE_GALERA_OPTIMIZATIONS))) {
            ((SimpleValue) _conf.getClassMapping(LogEntry.class.getName()).getIdentifier()).setIdentifierGeneratorStrategy(GaleraClusterTableGenerator.class.getName());
        }
        
        // Create session factory
        try {
            _sessionFactory = _conf.buildSessionFactory(serviceRegistry);
            
        }
        catch (HibernateException e) {
            throw new WGInvalidDatabaseException("Error creating session factory: " + e.getMessage(), e);
        }
    }



    public static String getCS5PatchLevelMappingFile(int patchLevel) {

        if (patchLevel >= 5) {
            return HIBERNATE_V5_P5_MAPPING_FILE;
        }
        else if (patchLevel >= 4) {
            return HIBERNATE_V5_P4_MAPPING_FILE;
        }
        else if (patchLevel >= 3) {
            return HIBERNATE_V5_P3_MAPPING_FILE;
        }
        else if (patchLevel >= 2) {
            return HIBERNATE_V5_P2_MAPPING_FILE;
        }
        else {
            return HIBERNATE_V5_MAPPING_FILE;
        }
        
    }





    /**
     * @param db
     * @param jdbcDriver
     * @param path
     * @param userName
     * @param password
     * @return
     * @throws WGInvalidDatabaseException
     */
    /**
     * @param db
     * @param jdbcDriver
     * @param path
     * @param userName
     * @param password
     * @return
     * @throws WGInvalidDatabaseException
     */
    private CSVersion determineCSVersion(WGDatabase db, String jdbcDriver, String path, String userName, String password) throws WGInvalidDatabaseException {
        

        try {
            JDBCConnectionProvider connProvider = createConnectionProvider(db, jdbcDriver, path, userName, password);
            try {
                
                // Determine content store version either by creation option or by inspecting backend database
                double version;
                String optionVersionStr = (String) db.getCreationOptions().get(WGDatabase.COPTION_CONTENT_STORE_VERSION);
                if (optionVersionStr != null) {
                    double optionVersion = Double.parseDouble(optionVersionStr);
                    if (optionVersion == WGDatabase.CSVERSION_WGA3 || optionVersion == WGDatabase.CSVERSION_WGA4_1 || optionVersion == WGDatabase.CSVERSION_WGA5) {
                        version = optionVersion;
                    }
                    else {
                        version = determineCSVersion(connProvider);
                    }
                }
                else {
                    version = determineCSVersion(connProvider);
                }
                 
                // Determine patch level
                int patchLevel = 0;
                if (version == WGDatabase.CSVERSION_WGA5) {
                    patchLevel = determinePatchLevel(connProvider);
                }
                
                // Optionally execute hot patch
                String hotPatchPath = (String) db.getCreationOptions().get(COPTION_HOTPATCH);
                if (hotPatchPath != null) {
                    try {
                        HotPatchCollection hotPatch = HotPatchCollection.load(new File(hotPatchPath));
                        if (hotPatch != null) {
                            for (HotPatch patch : hotPatch.getPatches()) {
                                Class<?> csType = WGFactory.getImplementationLoader().loadClass(patch.getCsType()); 
                                if (csType.isAssignableFrom(getClass()) && patch.getCsVersion() == version && patch.getCsPatchLevel() == patchLevel) {
                                    connProvider.performHotPatch(patch);
                                }
                            }
                        }
                    }
                    catch (Exception e) {
                        WGFactory.getLogger().error("Exception loading hot patch", e);
                    }
                }
                
                
                return new CSVersion(version, patchLevel);
            }
            finally {
                connProvider.stop();
            }
            
        }
        catch (JDBCConnectionException e) {
            throw new WGInvalidDatabaseException("Exception connecting to database on path " + path, e);
        }
        catch (SQLException e) {
            throw new WGInvalidDatabaseException("Exception connecting to database on path " + path, e);
        }

    }





    protected JDBCConnectionProvider createConnectionProvider(WGDatabase db, String jdbcDriver, String path, String userName, String password) throws JDBCConnectionException {
        Properties props = new Properties();
        Map creationOptions =  db.getCreationOptions();
        Iterator optsIt = creationOptions.keySet().iterator();
        while (optsIt.hasNext()) {
            String option = (String) optsIt.next();
            if (option.startsWith(("jdbc."))) {
                props.put("hibernate.connection." + option.substring(5), creationOptions.get(option));
            }
            else {
                props.put(option, creationOptions.get(option));
            }
        }
        props.put("user", WGUtils.getValueOrDefault(userName, ""));
        props.put("password", WGUtils.getValueOrDefault(password, ""));
        JDBCConnectionProvider connProvider = new JDBCConnectionProvider(path, jdbcDriver, props, false);
        return connProvider;
    }



    protected double determineCSVersion(JDBCConnectionProvider connProvider) throws SQLException {
        List<String> tables = connProvider.getDatabaseTables();
        
        if (tables.contains(getCS5IndicatorTable())) {
            return WGDatabase.CSVERSION_WGA5;
        }
        
        else if (tables.contains(getCS41IndicatorTable())) {
            return WGDatabase.CSVERSION_WGA4_1;
        }
        // This is a new HSQL content store to be created - Currently we create CS5 by default
        else if (this instanceof de.innovationgate.webgate.api.hsql.WGDatabaseImpl & tables.size() == 0) {
            return WGDatabase.CSVERSION_WGA5;
        }
        
        else {
            return WGDatabase.CSVERSION_WGA3;
        }
    }



    private int determinePatchLevel(JDBCConnectionProvider connProvider) {
        int patchLevel = 0;
        Statement stmt = null;
        ResultSet res = null;
        try {
            stmt = connProvider.getConnection().createStatement();
            res = stmt.executeQuery("SELECT datatype, numbervalue, textvalue FROM extensiondata WHERE entity_id IS NULL AND name='" + DBMETA_PATCH_LEVEL.toLowerCase() + "'");
            if (res.next()) {
                int type = res.getInt(1);
                Double numberValue = res.getDouble(2);
                String textValue = res.getString(3);
                if (type == WGDocumentImpl.ITEMTYPE_NUMBER) {
                    patchLevel = numberValue.intValue();
                }
                else if (type == WGDocumentImpl.ITEMTYPE_SERIALIZED_XSTREAM) {
                    XStream xstream = new XStream(new Dom4JDriver());
                    patchLevel = ((Number) xstream.fromXML(textValue)).intValue();
                }
            }
        }
        catch (Exception e) {
            WGFactory.getLogger().error("Exception determining CS5 patch level", e);
        }
        finally {
            try {
                if (res != null) {
                    res.close();
                }
                
                if (stmt != null) {
                    stmt.close();
                }
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Exception closing resource for CS version determination", e);
            }
        }
        
        return patchLevel;
    }



    protected String getCS5IndicatorTable() {
        return "extensiondata";
    }



    protected String getCS41IndicatorTable() {
        return "content_files_meta";
    }



    protected String getCS3MappingFile() {
        return HIBERNATE_V3_MAPPING_FILE;
    }



    protected String getCS41MappingFile() {
        return HIBERNATE_V41_MAPPING_FILE;
    }




    


    /**
     * @throws WGUnavailableException 
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#openSession(String,
     *      String)
     */
    public WGUserAccess openSession(AuthenticationSession authSession, Object pwd, boolean master) throws WGAPIException {

        try {

            // Hibernate login
            Session session = _sessionBuilder.openSession();
            // Connection conn = session.connection();
            // conn.setAutoCommit(true); //Problematic with DBCP?
            session.setFlushMode(FlushMode.COMMIT);
            if (_saveIsolationActive) {
                session.setDefaultReadOnly(true);
            }
            getSessionStatus().setSession(session);
            
            if (!session.isOpen()) {
                throw new WGUnavailableException(_db, "Unable to connect to hibernate session");
            }
            
        	// special handling if loadbalancing is enabled
            if (hasFeature(WGDatabase.FEATURE_LOADBALANCE)) {
            
                // set all connections to readonly except master sessions and if
                // update is in progress
            	final boolean readOnly = (master ? false : !isUpdateInProgress(authSession.getDistinguishedName()));            	
            	
            	try {
					session.doWork(new Work() {
                        
                        public void execute(Connection connection) throws SQLException {
                            connection.setReadOnly(readOnly);
                        }
                    });
                }
                catch (HibernateException e) {
					throw new WGBackendException("Unable to set readonly flag on connection." , e);
				}
            }

            if (getTransactionMode() != WGSessionContext.TRANSACTION_MODE_MANUAL) {
                session.beginTransaction();
            }

            if (master) {
                // Master login always has manager access
                return new WGUserAccess(WGDatabase.MASTER_USERNAME, WGDatabase.ACCESSLEVEL_MANAGER);
            }

            // Determine access
            WGUserDetails userDetails;
            try {
                userDetails = _db.defaultBuildUserDetails(authSession);
            }
            catch (WGBackendException e) {
                try {
                    closeSession();
                }
                catch (WGBackendException e1) {
                    WGFactory.getLogger().error(e1);
                }
                throw e;
            }
            if (userDetails.getAccessLevel() <= WGDatabase.ACCESSLEVEL_NOACCESS) {
                try {
                    closeSession();
                }
                catch (WGBackendException e) {
                    WGFactory.getLogger().error(e);
                }
            }

            return userDetails;
 

        }
        catch (HibernateException e) {
            try {
                closeSession();
            }
            catch (WGBackendException e1) {
                WGFactory.getLogger().error(e1);
            }
            throw new WGUnavailableException(_db, "Error opening hibernate session", e);
        }

    }



    private int getTransactionMode() {
        if (_db.getSessionContext() != null) {
            return _db.getSessionContext().getTransactionMode();
        } else {
            return WGSessionContext.TRANSACTION_MODE_DEFAULT;
        }
    }

 

    /**
     * @see de.innovationgate.webgate.api.WGDatabaseCore#parseStructKey(String)
     */
    public Object parseStructKey(String key) {

        return key;

    }

    /**
     * @throws WGAPIException 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#query(String, String,
     *      Map)
     */
    public WGResultSetCore query(String type, String query, Map params) throws WGAPIException {

        if (type == null) {
            type = "hql";
        }

        if (type.equals("native")) {
            type = "hql";
        }

        if (type.equals("hql") || type.equals("fullhql")) {
            return executeHQLQuery(type, query, params);
        }
        else if (type.equals("fulltext")) {
            return executeFulltextQuery(query, params);
        }
        else if (type.equals("sql")) {
            return executeSQLQuery(query, params);
        }
        else {
            throw new WGQueryException(query, "Unknown query type: " + type);
        }

    }

    /**
     * @param query
     * @param params
     * @return
     * @throws WGAPIException 
     */
    private WGResultSetCore executeFulltextQuery(String query, Map params) throws WGAPIException {

        // Build base query
        query = WGUtils.strReplace(query, "'", "''", true);
        params.put(WGDatabase.QUERYOPTION_RETURNQUERY, query);
        StringBuffer builtQuery = createFulltextQuery(query);

        // Additions based on params
        if (params.containsKey(WGDatabase.QUERYOPTION_ONLYRELEASED)) {
            builtQuery.append(" and content.status = 'p'");
        }

        if (params.containsKey(WGDatabase.QUERYOPTION_ONLYLANGUAGE)) {
            builtQuery.append(" and languages.name = '").append(params.get(WGDatabase.QUERYOPTION_ONLYLANGUAGE).toString()).append("'");
        }

        if (params.containsKey(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT)) {
            WGContent content = (WGContent) params.get(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT);
            if (content.getDatabase() == this._db) {
                WGDocumentCore contentCore = content.getCore();
                if (contentCore instanceof WGDocumentImpl) {
                    Content contentEntity = (Content) ((WGDocumentImpl) content.getCore()).getEntity();
                    builtQuery.append(" and not (content.cuid = '" + contentEntity.getCuid() + "')");
                }
            }
        }

        // Execute query
        String fullQuery = builtQuery.toString();
        return executeSQLQuery(fullQuery, params);

    }

    private WGResultSetCore executeSQLQuery(String fullQuery, Map queryOptions) throws WGAPIException {
        try {
            queryOptions.put(WGDatabase.QUERYOPTION_RETURNQUERY, fullQuery);
            SQLQuery hibQuery = getSession().createSQLQuery(fullQuery);
            if (fullQuery.contains("{")) {
                hibQuery.addEntity("content", Content.class);
            }
            Map queryParams = (Map) queryOptions.get(WGDatabase.QUERYOPTION_QUERY_PARAMETERS);
            HibernateResultSet.injectQueryParams(hibQuery, queryParams);
            return new WGSQLResultSet(this, hibQuery, queryParams, queryOptions);
        }
        catch (SQLGrammarException e) {
            throw new WGQueryException(fullQuery, e.getMessage(), e); 
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error executing SQL query", e);
        }
    }

    private StringBuffer createFulltextQuery(String query) {

        StringBuffer builtQuery = new StringBuffer();
        builtQuery
                .append("select {content.*} from content {content} left outer join content_items on content.cuid = content_items.cuid inner join languages on content.language = languages.name where (content.title like '%");
        builtQuery.append(query);
        builtQuery.append("%' or content.description like '%");
        builtQuery.append(query);
        builtQuery.append("%' or content_items.textvalue like '%");
        builtQuery.append(query);
        builtQuery.append("%')");
        return builtQuery;
    }

    private WGResultSetCore executeHQLQuery(String type, String query, Map queryOptions) throws WGAPIException {
        String builtQuery = null;

        Map queryParams = (Map) queryOptions.get(WGDatabase.QUERYOPTION_QUERY_PARAMETERS);
        if (queryParams == null) {
            queryParams = new HashMap();
        }
        
        List<WGLanguage> languagesPriorityList = null;
        
        // Determine fetch type. Lazy fetch will only retrieve content keys and
        // get content data in subsequent calls
        List options = WGUtils.deserializeCollection(String.valueOf(queryOptions.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS)), ",", true);
        boolean lazyFetch = _hqlLazyByDefault;
        if (options.contains(HQL_FETCHTYPE_STRAIGHT)) {
            lazyFetch = false;
        }
        else if (options.contains(HQL_FETCHTYPE_LAZY)) {
            lazyFetch = true;
        }
        
        if (type.equals("fullhql")) {
            builtQuery = query;
        }
        if (type.equals("hql")) {
            List fullQuery = new ArrayList();
            
            // Filter invisible and unreleased docs
            if (!queryOptions.containsKey(WGDatabase.QUERYOPTION_ENHANCE) || String.valueOf(queryOptions.get(WGDatabase.QUERYOPTION_ENHANCE)).equals("true")) {
                
                fullQuery.add("content.visible=" + getNativeSQLExpression(NATIVESQL_BOOLEAN_TRUE));
                fullQuery.add("(content.validfrom is null OR content.validfrom <= current_timestamp())");
                fullQuery.add("(content.validto is null OR content.validto >= current_timestamp())");
                
                String role = WGContent.DISPLAYTYPE_SEARCH;
                if (queryOptions.containsKey(WGDatabase.QUERYOPTION_ROLE)) {
                    role = (String) queryOptions.get(WGDatabase.QUERYOPTION_ROLE);
                }
                if (role != null && !role.equals(WGContent.DISPLAYTYPE_NONE)) {
                    fullQuery.add(":wgaparamRole not in elements(content.ishiddenfrom)");
                    queryParams.put("wgaparamRole", role);
                }
            }

            if (queryOptions.containsKey(WGDatabase.QUERYOPTION_ONLYRELEASED)) {
                fullQuery.add("content.status = 'p'");
            }
            
            // Filter languages
            if (queryOptions.containsKey(WGDatabase.QUERYOPTION_LANGUAGES)) {
                List<WGLanguage> langs = (List<WGLanguage>) queryOptions.get(WGDatabase.QUERYOPTION_LANGUAGES);
                if (langs.size() > 1 && lazyFetch) {
                    List languageTerms = new ArrayList();
                    for (WGLanguage lang : langs) {
                        languageTerms.add("content.language.name = '" + lang.getName() + "'");
                    }
                    fullQuery.add("(" + WGUtils.serializeCollection(languageTerms, " OR ") + ")");
                    languagesPriorityList = langs;
                }
                else if (langs.size() == 1){
                    fullQuery.add("content.language.name = :wgaparamLanguage");
                    queryParams.put("wgaparamLanguage", langs.get(0).getName());
                }
                else {
                    fullQuery.add("content.language.name = :wgaparamLanguage");
                    queryParams.put("wgaparamLanguage", getDb().getDefaultLanguage());
                }
                
            }
            else if (queryOptions.containsKey(WGDatabase.QUERYOPTION_ONLYLANGUAGE)) {
                fullQuery.add("content.language.name = '" + queryOptions.get(WGDatabase.QUERYOPTION_ONLYLANGUAGE).toString() + "'");
            }
            
            // Filter the "current document"
            if (queryOptions.containsKey(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT)) {
                WGContent content = (WGContent) queryOptions.get(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT);
                if (content.getDatabase() == this._db && !content.isDummy() && content.isSaved()) {
                    WGDocumentCore contentCore = content.getCore();
                    if (contentCore instanceof WGDocumentImpl) {
                        Content contentEntity = (Content) ((WGDocumentImpl) content.getCore()).getEntity();
                        if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                            fullQuery.add("not content.id = :wgaparamEntityId");
                            queryParams.put("wgaparamEntityId", contentEntity.getId());
                        }
                        else {
                            fullQuery.add("not content.cuid = :wgaparamEntityId");
                            queryParams.put("wgaparamEntityId", contentEntity.getCuid());
                        }
                    }
                }
            }

            // Extract ORDER BY part to get it out of the brackets
            String furtherClauses = "";
            int whereClauseEndIdx = query.toLowerCase().indexOf("group by");
            if (whereClauseEndIdx == -1) {
                whereClauseEndIdx = query.toLowerCase().indexOf("order by");
            }
            if (whereClauseEndIdx != -1) {
                furtherClauses = query.substring(whereClauseEndIdx);
                query = query.substring(0, whereClauseEndIdx);
            }

            fullQuery.add("(" + query + ")");
            
            
            builtQuery = composeHQLQuery(fullQuery, furtherClauses, lazyFetch, (languagesPriorityList != null));
        }

        if (builtQuery == null) {
            throw new WGQueryException(query, "Unknown query type: " + type);
        }
        
        builtQuery = WGUtils.strReplace(builtQuery, "\"", "'", true);

        if (queryOptions != null) {
            queryOptions.put(WGDatabase.QUERYOPTION_RETURNQUERY, builtQuery);
        }

        List results = null;
        try {
            Query hibQuery = getSession().createQuery(builtQuery);
            int maxResults = 0;
            if (queryOptions.containsKey(WGDatabase.QUERYOPTION_MAXRESULTS)) {
                 maxResults = ((Number) queryOptions.get(WGDatabase.QUERYOPTION_MAXRESULTS)).intValue();
            }
            if (maxResults > 0) {
                hibQuery.setMaxResults(maxResults);
            }
            
            HibernateResultSet.injectQueryParams(hibQuery, queryParams);
            if (lazyFetch) {
                if (languagesPriorityList != null) {
                    return new WGLanguageChoosingHQLResultSet(this, hibQuery, queryParams, queryOptions, languagesPriorityList);
                }
                else {
                    return new WGLazyHQLResultSet(this, hibQuery, queryParams, queryOptions);
                }
            }
            else {
                return new WGStraightHQLResultSet(this, hibQuery, queryParams, queryOptions);
            }
        }
        catch (QueryException e) {
            throw new WGQueryException(builtQuery, e.getMessage(), e);
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error executing HQL query", e);
        }
        
        
            
    }



    protected String getNativeSQLExpression(String expression) {
        if (NATIVESQL_BOOLEAN_TRUE.equals(expression)) {
            return "1";
        }
        else {
            return "";
        }
    }



    protected String composeHQLQuery(List fullQuery, String orderBy, boolean lazy, boolean languageChoosing) {
                
        if (lazy)  {
            if (_hqlLazyParentCheck && !languageChoosing) {
                return HQLQUERY_LAZY_PARENTCHECK + WGUtils.serializeCollection(fullQuery, " and ") + " " + orderBy;
            }
            else {
                return HQLQUERY_LAZY + WGUtils.serializeCollection(fullQuery, " and ") + " " + orderBy;
            }
        }
        else {
            return HQLQUERY_STRAIGHT + WGUtils.serializeCollection(fullQuery, " and ") + " " + orderBy;
        }
    }

    public static Class getClassByType(int type) {
        return (Class) _typeToObject.get(new Integer(type));
    }

    public static int getTypeByClass(Class aClass) {
        return ((Integer) _objectToType.get(aClass)).intValue();
    }

    public Object getEntity(WGDocument doc) throws WGAPIException {
        return ((WGDocumentImpl) doc.getCore()).getEntity();
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#beginTransaction()
     */
    public boolean beginTransaction() throws WGAPIException {
        Transaction transaction = getSession().getTransaction();
        if (transaction != null && transaction.isActive()) {
            transaction.rollback();
        }        
        transaction = getSession().beginTransaction();        
        if (transaction != null) {
            getSession().setCacheMode(CacheMode.GET);
            _db.getSessionContext().setTransactionMode(WGSessionContext.TRANSACTION_MODE_MANUAL);
            return true;
        } else {
            return false;
        }
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#commitTransaction()
     */
    public boolean commitTransaction() throws WGAPIException {
        Transaction transaction = getSession().getTransaction();
        if (transaction != null) {
            transaction.commit();
            getSession().setCacheMode(CacheMode.NORMAL);
            _db.getSessionContext().resetTransactionMode();
            getSession().beginTransaction();
            return true;
        }
    	return false;   	
    }
    
    public void beginUpdate() throws WGAPIException {    	
    	try {
    		getSession().getTransaction().rollback();
    		
    		getSession().doWork(new Work() {
                public void execute(Connection connection) throws SQLException {
                    if (connection.isReadOnly()) {
                        connection.setReadOnly(false);
                    }
                }
            });
    		
    		String user = _db.getSessionContext().getUser();
    		DBUpdate update = new DBUpdate(_db.getSessionContext());    		
    		_dbUpdatesByUser.put(user, update);
    		
			getSession().beginTransaction();
        }
        catch (HibernateException e) {
			throw new WGBackendException("unable to start transaction", e);
        }
   	
        
    }

    /** 
     * checks if an update is in progress for the current session context
     * 
     * @return
     */
    private boolean isUpdateInProgress() {
    	if (_db.getSessionContext() != null) {
    		return isUpdateInProgress(_db.getSessionContext().getUser());
        }
        else {
    		return false;
    	}
    }
    
    /**
     * checks if an update is in progress for the given user
     * 
     * @param user
     * @return
     */
    private boolean isUpdateInProgress(String user) {
    	DBUpdate update = (DBUpdate) _dbUpdatesByUser.get(user);
		if (update != null) {
			return update.isInProgress();
        }
        else {
        return false;
		}
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#rollbackTransaction()
     */
    public boolean rollbackTransaction() throws WGAPIException {
        Transaction transaction = getSession().getTransaction();
        if (transaction != null && transaction.isActive()) {
            transaction.rollback();
            getSession().setCacheMode(CacheMode.NORMAL);
            _db.getSessionContext().resetTransactionMode();
            getSession().beginTransaction();
            return true;
        }
        return false;
    }

    /*
     * (Kein Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseCore#execProcedure(java.lang.
     * String, java.util.List)
     */
    public Object execProcedure(String procName, List args) throws WGProcedureException {
        throw new WGProcedureException("Procedures are not yet supported");
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
     */
    public String getServerName() throws WGAPIException {

        try {
            DatabaseMetaData metaData = getSession().doReturningWork(new ReturningWork<DatabaseMetaData>() {
                public DatabaseMetaData execute(Connection connection) throws SQLException {
                    return connection.getMetaData();                
                }
            });
            return metaData.getDatabaseProductName() + " " + metaData.getDatabaseProductVersion();
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error retrieving server name", e);            
        }
        catch (SQLException e) {
            throw new WGBackendException("Error retrieving server name", e); 
        }
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getACL()
     */
    public WGACLCore getACL() {
        return _aclImpl;
    }
    
    public LogEntry createLogEntry(Session session, int logType, String docKey, String entityID) throws HibernateException, WGAPIException {
        LogEntry entry = new LogEntry(new Date(), logType, docKey.toString(), _db.getSessionContext().getUser());
        entry.setTarget_id(entityID);
        if (_db.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            entry.setOperation(_db.getSessionContext().getTask());
        }
        session.save(entry);
        return entry;
    }

    /*
     * (Kein Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseEventListener#databaseUpdate(
     * de.innovationgate.webgate.api.WGDatabaseEvent)
     */
    public void refresh() throws WGAPIException {
        Session session = getSession();
        session.flush();
        session.clear();
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseEventListener#isTemporary()
     */
    public boolean isTemporary() {
        return false;
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getNativeObject()
     */
    public Object getNativeObject() throws WGAPIException  {
        return getSession();
    }

    /*
     * (Kein Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseCore#resultIsFalse(java.lang.
     * Object, de.innovationgate.webgate.api.WGDocument)
     */
    public boolean resultIsFalse(Object result, WGDocument doc) {
        return false;
    }

    /*
     * (Kein Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseCore#resultIsTrue(java.lang.Object
     * , de.innovationgate.webgate.api.WGDocument)
     */
    public boolean resultIsTrue(Object result, WGDocument doc) {
        return false;
    }



    /**
     * @param props
     */
    public static void putDefaultConPoolProps(WGDatabase db, Properties props) {
        
        if (db != null) {
        	if (db.hasFeature(WGDatabase.FEATURE_LOADBALANCE)) {
        		WGUtils.setDefaultProperty(props, "hibernate.connection.provider_class", DBCPReplicationConnectionProvider.class.getName());
            }
            else {
                WGUtils.setDefaultProperty(props, "hibernate.connection.provider_class", DBCPConnectionProvider.class.getName());
        	}
        }
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxActive", "100");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.whenExhaustedAction", "1");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxWait", "120000");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxIdle", "10");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.poolPreparedStatements", "true");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxOpenPreparedStatements", DEFAULT_MAXOPENPREPAREDSTATEMENTS);
        //WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxConnLifetimeMillis", String.valueOf(MySqlDatabaseServer.DEFAULT_SHAREDPOOL_MAX_CONNECTION_LIFETIME));
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.legacyJMX", String.valueOf("true".equals(db.getCreationOptions().get(WGDatabase.COPTION_LEGACY_DBCP_MONITORING))));
        
        if (db != null && db.getDbReference().indexOf(":") == -1) {
            props.put("hibernate.dbcp.dbkey", db.getDbReference());
        }

    }
    
    public static void putDefaultServerConPoolProps(WGDatabaseServer server, Properties props) {
        
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.timeBetweenEvictionRunsMillis", String.valueOf(1000 * 60));
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.minEvictableIdleTimeMillis", String.valueOf(1000 * 60));
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.numTestsPerEvictionRun", String.valueOf(10));
        
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.whenExhaustedAction", "1");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxWait", "120000");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.poolPreparedStatements", "true");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.maxOpenPreparedStatements", DEFAULT_MAXOPENPREPAREDSTATEMENTS);
        
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.removeAbandoned", "true");
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.removeAbandonedTimeout", String.valueOf(60 * 60 * 24));
        WGUtils.setDefaultProperty(props, "hibernate.dbcp.legacyJMX", String.valueOf("true".equals(server.getOptions().get(DatabaseServer.OPTION_SHAREDPOOL_LEGACY_DBCP_MONITORING))));

    }

    /*
     * (Kein Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseCore#getUpdatedDocumentsSince
     * (java.util.Date)
     */
    public List<WGUpdateLog> getUpdateLogs(Comparable fromRevision) throws WGAPIException {

        try {
            Iterator logEntries;
            if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                Query logEntriesQ = getSession().createQuery("from de.innovationgate.webgate.api.jdbc.LogEntry as logentry where id > :start order by id asc");
                logEntriesQ.setLong("start", ((Long) fromRevision).longValue());
                logEntries = logEntriesQ.iterate();

            }
            else {
                Date cutoff = (Date) fromRevision;
                Query logEntriesQ = getSession().createQuery("from de.innovationgate.webgate.api.jdbc.LogEntry as logentry where logtime >= :start order by logtime asc");
                logEntriesQ.setTimestamp("start", new java.sql.Timestamp(cutoff.getTime()));
                logEntries = logEntriesQ.iterate();
            }
            
            List wgLogs = new ArrayList();
            LinkedMap wgLogsByTarget = new LinkedMap();
            Map conflictTargets = new HashMap();

            LogEntry entry;
            
            // First pass: Create update logs
            while (logEntries.hasNext()) {
                entry = (LogEntry) logEntries.next();
                WGUpdateLog newLog = null;
                WGUpdateLog oldLog = null;
                Date currentTime = null;
                if (entry.getTarget() != null && !entry.getTarget().equals("#UNKNOWN#")) {
                    newLog = readUpdateLog(entry);
                    wgLogs.add(newLog);
                    
                    List logsList = (List) wgLogsByTarget.get(entry.getTarget());
                    if (logsList == null) {
                        logsList = new ArrayList();
                        wgLogsByTarget.put(entry.getTarget(), logsList);
                    }
                    logsList.add(newLog);
                }
            }
            
            // Second pass for CS version < 5 to workaround some weaknesses of the CS3/4 history log
            if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                
                // Determine conflicting log entries, where update and delete is done on the same time and the same document
                Iterator wgLogsByTargetIt = wgLogsByTarget.values().iterator();
                while (wgLogsByTargetIt.hasNext()) {
                    List logs = (List) wgLogsByTargetIt.next();
                    WGUtils.sortByProperty(logs, "date");
                    Iterator logsIt = logs.iterator();
                    Date lastTime = null;
                    List<WGUpdateLog> logsAtSameTime = new ArrayList();
                    while (logsIt.hasNext()) {
                        WGUpdateLog log = (WGUpdateLog) logsIt.next();
                        if (log.getDate().equals(lastTime)) {
                            logsAtSameTime.add(log);
                        }
                        else {
                            resolveLogConflicts(wgLogs, logsAtSameTime);                
                            logsAtSameTime.clear();
                        }
                        lastTime = log.getDate();
                    }
                }
                
                // Order logentries that have the same time in an order that assures dependency documents are created before their dependent documents
                Collections.sort(wgLogs, new DocumentDependencyComparator());
                
            }
            
            
            return wgLogs;
        }
        catch (HibernateException e) {
            throw new WGBackendException("Unable to retrieve updated documents", e);
        }

    }





    protected WGUpdateLog readUpdateLog(LogEntry entry) {
        return new WGUpdateLog(entry.getType(), entry.getLogtime(), entry.getLoguser(), entry.getTarget(), entry.getOperation(), getLogRevision(entry));
    }

    private void resolveLogConflicts(List wgLogs, List conflictLogs) {

        // Reverse the sort order so we have the maximum logtimes at the beginning
        Collections.reverse(conflictLogs);
        
        // Get the operations having the maximum logtime
        WGUpdateLog maxUpdateLog = null;
        WGUpdateLog maxDeletionLog = null;
        Iterator logsIt = conflictLogs.iterator();
        Date maxDate = null;
        while (logsIt.hasNext()) {
            WGUpdateLog log = (WGUpdateLog) logsIt.next();
            
            if (maxDate == null) {
                maxDate = log.getDate();
            }
            else if (!maxDate.equals(log.getDate())) {
                break;
            }
            
            if (log.getType() == WGUpdateLog.TYPE_UPDATE) {
                maxUpdateLog = log;
            }
            else if (log.getType() == WGUpdateLog.TYPE_DELETE) {
                maxDeletionLog = log;
            }
        }
        
        // Now lets see what we have - If there is only one type of log we have no problem
        if (maxDeletionLog != null && maxUpdateLog == null) {
        }
        else if (maxUpdateLog != null && maxDeletionLog == null) {
        }
        else if (maxUpdateLog != null && maxDeletionLog != null) {
            // So we have an update and a deletion at the same time.
            // We cannot determine by log which was before the other.
            // Therefor we look if the document exists.
            // If so we use the update log, else the deletion log.
            // The nonused log is therefor removed from the log list
            WGDocument doc = null;
            WGDocumentKey documentKey = new WGDocumentKey(maxUpdateLog.getDocumentKey());
            if (documentKey.isRealDocumentKey()) {
                try {
                    doc = _db.getDocumentByKey(documentKey);
                    if (doc != null && !doc.isDeleted()) {
                        WGFactory.getLogger().info("Resolving log conflict about document " + maxUpdateLog.getDocumentKey() + ": Relevant log type determined as update");
                        wgLogs.remove(maxDeletionLog);                 
                    }
                    else {
                        WGFactory.getLogger().info("Resolving log conflict about document " + maxUpdateLog.getDocumentKey() + ": Relevant log type determined as deletion");
                        wgLogs.remove(maxUpdateLog);
                    }
                }
                catch (WGAPIException e) {
                    WGFactory.getLogger().error("Exception resolving log conflict for document " + maxUpdateLog.getDocumentKey(), e);
                }
            }
        }
        
    }

    
    public WGDocumentImpl createDocumentImpl(MainEntity entity) {

        int type;
        if (entity instanceof Content) {
            type = WGDocument.TYPE_CONTENT;
        }
        else if (entity instanceof StructEntry) {
            type = WGDocument.TYPE_STRUCTENTRY;
        }
        else if (entity instanceof Area) {
            type = WGDocument.TYPE_AREA;
        }
        else if (entity instanceof ContentType) {
            type = WGDocument.TYPE_CONTENTTYPE;
        }
        else if (entity instanceof TMLModule) {
            type = WGDocument.TYPE_TML;
        }
        else if (entity instanceof CSSJSModule) {
            type = WGDocument.TYPE_CSSJS;
        }
        else if (entity instanceof Language) {
            type = WGDocument.TYPE_LANGUAGE;
        }
        else if (entity instanceof UserProfile) {
            type = WGDocument.TYPE_USERPROFILE;
        }
        else if (entity instanceof FileContainer) {
            type = WGDocument.TYPE_FILECONTAINER;
        }
        else {
            throw new IllegalArgumentException("The given object type is no known entity: " + entity.getClass().getName());
        }

        return new WGDocumentImpl(this, entity, type);
    }

    /*
     * (non-Javadoc)
     * 
     * @seede.innovationgate.webgate.api.WGDatabaseCore#moveStructEntry(de.
     * innovationgate.webgate.api.WGStructEntry,
     *      de.innovationgate.webgate.api.WGDocument)
     */
    public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) {

        try {
            WGDocumentImpl docCore = (WGDocumentImpl) entry.getCore();
            docCore.makeEditable();
            StructEntry structEntry = (StructEntry) docCore.getEntity();

            // Remove from former parent object
            // Manual initializings are workaround for hibernate bug B00005D36
            StructEntry parentEntry = structEntry.getParententry();
            if (parentEntry != null) {
                
                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    Hibernate.initialize(parentEntry.getChildentries());
                    StructEntry oldEntry = (StructEntry) parentEntry.getChildentries().remove(structEntry.getKey());
                    if (oldEntry != null && _saveIsolationActive) {
                        getSession().evict(oldEntry);
                    }
                }
                
            }
            else {
                Area area = structEntry.getArea();
                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    Hibernate.initialize(area.getRootentries());
                    StructEntry oldEntry = (StructEntry) area.getRootentries().remove(structEntry.getKey());
                    if (oldEntry != null && _saveIsolationActive) {
                        getSession().evict(oldEntry);
                    }                
                }
            }

            // Set to new parent object
            if (newParent instanceof WGArea) {
                Area area = (Area) newParent.getNativeObject();
                structEntry.setParententry(null);
                structEntry.setArea(area);
                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    area.getRootentries().put(structEntry.getKey(), structEntry);
                }
            }
            else if (newParent instanceof WGStructEntry) {
                StructEntry parent = (StructEntry) newParent.getNativeObject();
                structEntry.setParententry(parent);
                structEntry.setArea(null);
                if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
                    parent.getChildentries().put(structEntry.getKey(), structEntry);
                }

            }
            else {
                return false;
            }
            createLogEntry(getSession(), WGUpdateLog.TYPE_STRUCTMOVE, entry.getDocumentKey(), structEntry.getId());
            entry.save();
            return true;
        }
        catch (WGAPIException e) {
            WGFactory.getLogger().error("Error moving struct entry", e);
            e.printStackTrace();
            return false;
        }

    }

    public int getContentCount(WGStructEntry structEntry) throws WGAPIException {

        try {
            Query query;
            if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
                query = getSession().createQuery("select id from Content as content where content.structentry.key=:key");
            }
            else {
                query = getSession().createQuery("select cuid from Content as content where content.structentry.key=:key");
            }
            
            query.setParameter("key", String.valueOf(structEntry.getStructKey()));
            return query.list().size();
            
            }
        catch (HibernateException e) {
            throw new WGBackendException("Error determining content presence for  " + structEntry.getDocumentKey(), e);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @seede.innovationgate.webgate.api.WGDatabaseCore#setCurrentSession(de.
     * innovationgate.webgate.api.WGSessionContext)
     */
    public void setCurrentSession(WGSessionContext context) {
    }
    
    public String convertFileNameForAttaching(String name) {
        return WGUtils.strReplace(name.toLowerCase(), "���", "/", true);
    }

    public void authenticationDataChanged() {
        
        _db.getUserCache().clear();
        
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseCore#getAllowedCredentialClasses
     * ()
     */
    public Class[] getAllowedCredentialClasses() {
        return null;
    }
    
    public List queryUserProfileNames(String type, String query, Map params) throws WGAPIException {

        if (type == null) {
            type = "hql";
        }
        
        try {
            String fullQuery;
            if (query != null) {
                if (type.equals("hql")) {
                fullQuery = "select profile.name from UserProfile profile where (" + query + ") order by profile.name asc";
            }
                else if (type.equals("fullhql")) {
                    fullQuery = query;
                }
                else {
                    throw new WGQueryException(query, "Unknown query type '" + type + "'");
                }
            }
            else {
                fullQuery = "select profile.name from UserProfile profile order by profile.name asc";
            }
            
            Query hqlQuery = getSession().createQuery(fullQuery);
            if (params.containsKey(WGDatabase.QUERYOPTION_MAXRESULTS)) {
                Number maxResults = (Number) params.get(WGDatabase.QUERYOPTION_MAXRESULTS);
                hqlQuery.setMaxResults(maxResults.intValue());
            }
            
            return hqlQuery.list();
            
        }
        catch (QueryException e) {
           throw new WGQueryException(query, e.getMessage(), e); 
        }
        catch (HibernateException e) {
           throw new WGBackendException("Error executing profile query", e);
        }
        
    }
    
    public void commitHibernateTransaction() throws WGAPIException {
        if (getTransactionMode() != WGSessionContext.TRANSACTION_MODE_MANUAL) {
            Transaction trans = getSession().getTransaction();
            if (trans != null) {
                trans.commit();
            }
            
            // hibernate might release the underlying jdbc connection after commit
            // and get a new one
            // this connection will be readonly by default - therefore we should
            // check if we have to
            // make it read/write
            if (isUpdateInProgress()) {        	
            	try {
            		getSession().doWork(new Work() {
                        public void execute(Connection connection) throws SQLException {
                            if (connection.isReadOnly()) {
                               connection.setReadOnly(false);
                            }
                        }
                    });
                }
                catch (HibernateException e) {
    				throw new WGBackendException("unable to establish a read/write connection.", e);
                }
            }
            
            getSession().beginTransaction();
        } else {
            // perform at least a flush in this mode so JDBC driver will execute changes in current JDBC transaction
            getSession().flush();
        }
    }
    
    protected void rollbackHibernateTransaction(boolean noThrows) throws WGAPIException {
        
        if (getTransactionMode() != WGSessionContext.TRANSACTION_MODE_MANUAL) {
        
            try {
                Transaction trans = getSession().getTransaction();
                if (trans != null && trans.isActive()) {
                    trans.rollback();
                }
                
                // hibernate might release the underlying jdbc connection after commit
                // and get a new one
                // this connection will be readonly by default - therefore we should
                // check if we have to
                // make it read/write
                if (isUpdateInProgress()) {        	
                	try {
                	    getSession().doWork(new Work() {
                            public void execute(Connection connection) throws SQLException {
                                if (connection.isReadOnly()) {
                                    connection.setReadOnly(false);
                                }
                            }
                        });
                		
                    }
                    catch (HibernateException e) {
        				throw new WGBackendException("unable to establish a read/write connection.", e);
                    }
                } 
                
                getSession().beginTransaction();
            }
            catch (Throwable e) {
                if (noThrows) {
                    WGFactory.getLogger().error("Exception rolling back transaction. Cause for rollback may be logged after this exception.", e);
                }
                else if (e instanceof WGBackendException) {
                    throw (WGBackendException) e;
                }
                else if (e instanceof RuntimeException) {
                    throw (RuntimeException) e;
                }
                else {
                    throw new WGBackendException("Exception rolling back transaction", e);
                }
            }
        
        }
        
    }
    
    /*
     *  (non-Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.WGDatabaseCore#getDeletions(java.util.Set)
     */
    public Set getDeletions(Set contentKeys) throws WGAPIException {
        // unsupported for this implementation
        return Collections.EMPTY_SET;
    }
    
    public List getAllContentKeys(boolean includeArchived) throws WGAPIException {
        
        try {
            String hql = "select new de.innovationgate.webgate.api.WGContentKey(content.structentry.key, content.language.name, content.version) from Content as content";
            
            if (!includeArchived) {
                hql += " where not content.status='" + WGContent.STATUS_ARCHIVE + "'";
            }
            
            Query query = getSession().createQuery(hql);
            return new ArrayList(query.list());
        }
        catch (HibernateException e) {
            throw new WGBackendException("Exception retrieving all content keys", e);            
        }
        
        
        
    }

    public WGDatabase getDb() {
        return _db;
    }
    
    protected SessionStatus getSessionStatus() {
        
        SessionStatus ss = (SessionStatus) _sessionStatus.get();
        if (ss == null) {
            ss = new SessionStatus();
            _sessionStatus.set(ss);
        }
        return ss;
        
    }
    
    public boolean isHqlLazyByDefault() {
        return _hqlLazyByDefault;
    }

    public void setHqlLazyByDefault(boolean hqlLazyByDefault) {
        _hqlLazyByDefault = hqlLazyByDefault;
    }

    public boolean useOptimizedFileHandling() throws WGAPIException {
    	return _db.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1;
    }

    protected void updateContentRelations(Content content) throws WGAPIException  {
        
        if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
            return;
        }
        
        Query query = getSession().createQuery(HQLQUERY_UPDATE_RELATIONS);
        query.setParameter("target", content);
        query.setParameter("structentry", content.getStructentry().getKey());
        query.setParameter("language", content.getLanguage().getName());
        query.executeUpdate();
    }



    public WGDocumentCore getStructEntryByName(String strName) throws WGAPIException {

        Query query = getSession().createQuery(HQLQUERY_GET_STRUCT_BY_NAME);
        query.setParameter("name", strName);
        Iterator results = query.iterate();
        if (results.hasNext()) {
            StructEntry entry = (StructEntry) results.next();
            return createDocumentImpl(entry);
        }
        else {
            return null;
        }
    
    }



    public Date getRevisionDate(Comparable lastChanged) throws WGAPIException, WGWrongRevisionException {
        
        if (_ddlVersion >= WGDatabase.CSVERSION_WGA5) {
            
            if (!(lastChanged instanceof Long)) {
                throw new WGWrongRevisionException(Long.class);
            }
            
            LogEntry entry = (LogEntry) getSession().get(LogEntry.class, (Long) lastChanged);
            if (entry != null) {
                return entry.getLogtime();
            }
            else {
            	return null;
                //return new Date(Long.MIN_VALUE);
            }
        }
        else {
            try {
                return (Date) lastChanged;
            }
            catch (ClassCastException e) {
                throw new WGWrongRevisionException(Date.class); 
            }
        }
        
    }



    public double getContentStoreVersion() throws WGAPIException {
        return _csVersion.getVersion();
    }
    
    @Override
    public int getContentStorePatchLevel() throws WGAPIException {
        return _csVersion.getPatchLevel();
    }



    public void writeExtensionData(String name, Object value) throws WGAPIException {
        
        name = name.toLowerCase().trim();
        
        // Convert numbers to doubles
        if (value instanceof Number && !(value instanceof Double)) {
            value = new Double(((Number) value).doubleValue());
        }
        
        try {
            ExtensionData md = retrieveExtensionData(name);
            if (md == null) {
                md = new ExtensionData();
                md.setName(name);
                WGDocumentImpl.writeItemValue(this, null, md, value);
                getSession().save(md);
            }
            else {
                if (_saveIsolationActive) {
                    getSession().evict(md);
                }
                WGDocumentImpl.writeItemValue(this, null, md, value);
                if (_saveIsolationActive) {
                    getSession().update(md);
                }
            }
            
            createLogEntry(getSession(), WGUpdateLog.TYPE_UPDATE, WGDocument.TYPENAME_DBMETADATA + "/" + name, md.getId());
            commitHibernateTransaction();
        }
        catch (HibernateException e) {
            throw new WGBackendException("Error setting database metadata.", e);   
        }
        
        
        
    }

    public void removeExtensionData(String name) throws WGAPIException {
        name = name.toLowerCase().trim();
        ExtensionData md = retrieveExtensionData(name);
        if (md != null) {
            String mdId = md.getId();
            getSession().delete(md);
            createLogEntry(getSession(), WGUpdateLog.TYPE_DELETE, WGDocument.TYPENAME_DBMETADATA + "/" + name, mdId);
            commitHibernateTransaction();
        }
        
    }
    
    public List<String> getExtensionDataNames() throws WGAPIException {
        return getSession().createQuery("select name from ExtensionData where entity = null").list();
    }


    public List<WGRelationData> getIncomingRelations(Object structKey, String language, String contentClass, String relName, String relGroupName, Boolean includeUnreleased, WGColumnSet order) throws WGAPIException {
        if (_ddlVersion < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        StringBuffer hql = new StringBuffer("select relation from ContentRelation as relation where relation.targetstructentry = :structkey and relation.targetlanguage = :language and relation.parentcontent.status in (:states)");
        Map<String,Object> parameters = new HashMap<String, Object>();
        parameters.put("structkey", structKey);
        parameters.put("language", language);
        
        if (relName != null) {
            hql.append(" and relation.name = :relname");
            parameters.put("relname", relName);
        }
        
        if (relGroupName != null) {
            hql.append(" and relation.group = :relgroup");
            parameters.put("relgroup", relGroupName);
        }
        
        if (contentClass != null) {
            hql.append(" and relation.parentcontent.contentclass = :sourceclass");
            parameters.put("sourceclass", contentClass);
        }
        
        if (order != null) {
            hql.append(" order by ");
            hql.append(buildHqlContentOrderClause("relation.parentcontent", order, parameters));
        }
        
        Query query = getSession().createQuery(hql.toString());
        for (Map.Entry<String, Object> param : parameters.entrySet()) {
            query.setParameter(param.getKey(), param.getValue());
        }
        
        if (includeUnreleased) {
           query.setParameterList("states", new Object[] {WGContent.STATUS_DRAFT, WGContent.STATUS_REVIEW, WGContent.STATUS_RELEASE});
        }
        else {
           query.setParameterList("states", new Object[] {WGContent.STATUS_RELEASE});
        }
        
        /*
        Criteria crit = getSession().createCriteria(ContentRelation.class);
        crit.add(Restrictions.eq("targetstructentry", structKey));
        crit.add(Restrictions.eq("targetlanguage", language));
        crit.setFetchMode("parentcontent", FetchMode.SELECT);
        
        if (includeUnreleased) {
            crit.createCriteria("parentcontent").add(Restrictions.in("status", new Object[] {WGContent.STATUS_DRAFT, WGContent.STATUS_REVIEW, WGContent.STATUS_RELEASE}));
        }
        else {
            crit.createCriteria("parentcontent").add(Restrictions.eq("status", WGContent.STATUS_RELEASE));
        }*/
        
        
        List<WGRelationData> incoming = new ArrayList();
        
        for (ContentRelation rel : (List<ContentRelation>) query.list()) {
            WGRelationData relData = createWGRelationData(rel);
            incoming.add(relData);
        }
        
        return incoming;
        
    }



    protected WGRelationData createWGRelationData(ContentRelation rel) {
        Content content = rel.getParentcontent();
        WGContentKey cKey = new WGContentKey(content.getStructentry().getKey(), content.getLanguage().getName(), content.getVersion().intValue());
        WGRelationData relData = new WGRelationData(_db, cKey, rel.getName(), rel.getTargetstructentry(), rel.getTargetlanguage(), rel.getType(), rel.getGroup());
        return relData;
    }



    /**
     * Returns if query paging for optimized file handling is enabled, see
     * {@link #COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING}
     */
    public boolean isOptimizedFileHandlingDisableQueryPaging() {
        Object disableQueryPaging = _db.getCreationOptions().get(COPTION_OPTIMIZED_FILE_HANDLING_DISABLEQUERYPAGING);
        if (disableQueryPaging != null) {
            return Boolean.valueOf((String) disableQueryPaging).booleanValue();
        }
        else {
            return true;
        }
    }



    public boolean isContentTypeUsed(WGContentType ctDoc) throws WGAPIException {
        
        // Get the entity
        WGDocumentImpl docCore = (WGDocumentImpl) ctDoc.getCore();
        ContentType ct = (ContentType) docCore.getEntity();
        
        // Do a criteria query
        Criteria crit = getSession().createCriteria(StructEntry.class);
        crit.add(Restrictions.eq("contenttype", ct));
        crit.setMaxResults(1);
        List docs = crit.list();
        
        return docs.size() > 0;
        
    }



    public boolean isLanguageUsed(WGLanguage langDoc) throws WGAPIException {

        // Get the entity
        WGDocumentImpl docCore = (WGDocumentImpl) langDoc.getCore();
        Language lang = (Language) docCore.getEntity();
        
        // Do a criteria query
        Criteria crit = getSession().createCriteria(Content.class);
        crit.add(Restrictions.eq("language", lang));
        crit.setMaxResults(1);
        List docs = crit.list();
        
        return docs.size() > 0;
        
    }



    public Iterator<String> getAllUserProfileNames() throws WGAPIException {

        try {
            String fullQuery = "select profile.name from UserProfile profile order by profile.name asc";
            
            
            Query hqlQuery = getSession().createQuery(fullQuery);
            return hqlQuery.iterate();
            
        }
        catch (QueryException e) {
           throw new WGQueryException("Querying all user profile names", e.getMessage(), e); 
        }
        catch (HibernateException e) {
           throw new WGBackendException("Error executing profile query", e);
        }
        
    }    

    public boolean isBackendServiceSupported(String serviceName) {
        
        if (WGDatabase.BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS.equals(serviceName)) {
            return true;
        }
        else if (WGDatabase.BACKENDSERVICE_DAILY_MAINTENANCE.equals(serviceName)) {
            return (_fileHandling instanceof CS5P4FileHandling);
        }
        else if (BACKENDSERVICE_UPGRADE_FILE_STORAGE.equals(serviceName)) {
            return (_fileHandling instanceof CS5P4FileHandling);
        }
        else if (WGDatabase.BACKENDSERVICE_NEW_CONTENT_VERSION.equals(serviceName)) {
            return true;
        }
        else if (WGDatabase.BACKENDSERVICE_PROBE_CONTENT.equals(serviceName)) {
            return true;
        }
        else if (WGDatabase.BACKENDSERVICE_FETCH_MULTI_CONTENT.equals(serviceName)) {
            return true;
        }
        
        return false;
    }
    
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException {
        
        if (WGDatabase.BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS.equals(serviceName)) {
            
            String query;
            if (getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
                query = HQLQUERY_PENDING_RELEASE_DOCS_CS5;
            }
            else {
                query = HQLQUERY_PENDING_RELEASE_DOCS_CS3;
            }
            
            Map queryParams = new HashMap();
            queryParams.put(WGDatabase.QUERYOPTION_ENHANCE, false);
            return _db.query("hql", query, queryParams);
        }
        
        else if (WGDatabase.BACKENDSERVICE_DAILY_MAINTENANCE.equals(serviceName)) {
            if (_ugradeFileStorageRunning) {
                return null;
            }
            dailyMaintenance(params.length >= 1 ? (Logger) params[0] : WGFactory.getLogger());
            return null;
        }
        else if (BACKENDSERVICE_UPGRADE_FILE_STORAGE.equals(serviceName)) {
            return upgradeFileStorage((Logger) params[0]);
        }
   
        else if (WGDatabase.BACKENDSERVICE_NEW_CONTENT_VERSION.equals(serviceName)) {
            return newContentVersion((WGStructEntry) params[0], (WGLanguage) params[1]);
        }
        
        else if (WGDatabase.BACKENDSERVICE_PROBE_CONTENT.equals(serviceName)) {
            return probeContent((WGStructEntry) params[0], (String) params[1], (String) params[2]);
        }
        else if (WGDatabase.BACKENDSERVICE_FETCH_MULTI_CONTENT.equals(serviceName)) {
            return fetchMultiContent((List<String>) params[0]);
        }
       
        else {
            throw new WGNotSupportedException("Backend service not supported: " + serviceName);
        }
    }

    private WGResultSetCore fetchMultiContent(List<String> keys) throws WGAPIException {

        String hql = "select content from Content as content where concat(content.structentry.key, '.', content.language.name, '.', content.version) in (:keys)";
        Map<String,Object> options = new HashMap<String, Object>();
        options.put(WGDatabase.QUERYOPTION_ENHANCE, false);
        options.put(WGDatabase.QUERYOPTION_NATIVEOPTIONS, "straight");
        options.put(WGDatabase.QUERYOPTION_MAXRESULTS, 0);
        options.put(WGDatabase.QUERYOPTION_ROLE, "none");
        
        
        Map<String,Object> params = new HashMap<String, Object>();
        params.put("keys", keys);
        options.put(WGDatabase.QUERYOPTION_QUERY_PARAMETERS, params);
        
        return query("fullhql", hql, options);

    }

    private boolean probeContent(WGStructEntry page, String language, String status) throws WGAPIException {

        StringBuffer hql = new StringBuffer();
        hql.append("select count(*) from Content as content where content.structentry.key=:page");
        if (language != null) {
            hql.append(" and content.language.name=:lang");
        }
        if (status != null) {
            hql.append(" and content.status=:status");
        }
        
        Query q = getSession().createQuery(hql.toString());
        q.setParameter("page", page.getStructKey());
        
        
        if (language != null) {
            q.setParameter("lang", language);
        }
        if (status != null) {
            q.setParameter("status", status);
        }
        
        Number count = (Number) q.uniqueResult();
        return (count != null && count.intValue() > 0);
        
    }





    private int newContentVersion(WGStructEntry structEntry, WGLanguage lang) throws WGAPIException {

        String hql = "select max(content.version) from Content as content where content.structentry.key=:key and content.language.name=:lang";
        Query q = getSession().createQuery(hql);
        q.setParameter("key", structEntry.getStructKey());
        q.setParameter("lang", lang.getName());
        Iterator it = q.iterate();
        Number maxVersion = null;
        if (it.hasNext()) {
            maxVersion = (Number) it.next();
        }
        
        if (maxVersion == null) {
            maxVersion = 0;
        }
        
        return maxVersion.intValue() + 1;
        
    }





    private Long upgradeFileStorage(Logger log) throws WGAPIException {
        
        _ugradeFileStorageRunning  = true;
        try {
            
            if (_csVersion.getVersion() < 5.0 || _csVersion.getPatchLevel() < 4) {
                log.error("This task needs a content store of version 5.0 patch level 4 or higher");
                return 0L;
            }
            
            CS5P4FileHandling fileHandling = ((CS5P4FileHandling) _fileHandling);
            long freedMemory = 0;
            
            while (true) {
                String metaHql = "from ContentFileMeta as meta where meta.checksumSha512 is null";
                Iterator oldMetas = getSession().createQuery(metaHql).iterate();
                try {
                    int counter = 0;
                    if (!oldMetas.hasNext()) {
                        break;
                    }
                    
                    while (oldMetas.hasNext() && counter < 100) {
                        ContentFileMeta meta = (ContentFileMeta) oldMetas.next();
                        getSession().setReadOnly(meta, false);
                        LockRequest lockRequest = getSession().buildLockRequest(new LockOptions(LockMode.PESSIMISTIC_WRITE));
                        lockRequest.lock(meta);
                        
                        try {
                            
                            // Just-for-sure check if this is really not yet migrated
                            getSession().refresh(meta);
                            if (meta.getChecksumSha512() != null) {
                                rollbackHibernateTransaction(false);
                                continue;
                            }
                            
                            log.info("Database: " + getDb().getDbReference()+ ": Upgrading storage of file '" + meta.getName() + "' from document '" + meta.getParentcontent().getTitle() + "' (" + meta.getParentcontent().getStructentry().getKey() + "." + meta.getParentcontent().getLanguage().getName() + "." + meta.getParentcontent().getVersion() + ")");
                            
                            // Select file parts
                            String hqlQuery = "select cfp from ContentFilePart as cfp where cfp.meta=:metaEntity order by cfp.partnr asc";
                            Query query = getSession().createQuery(hqlQuery);
                            query.setParameter("metaEntity", meta);
                            
                            // Migrate file parts to filecontents parts
                            InputStream in = new HibernateQueryInputStream(fileHandling.getParent().getSession(), query, 0, isOptimizedFileHandlingDisableQueryPaging());
                            try {
                                fileHandling.storeFileContents(meta, new CS5P4ContentFileDescriptor(), in);
                            }
                            finally {
                                in.close();
                            }
                            
                            // Delete file parts
                            Query deletionQuery = getSession().createQuery("delete ContentFilePart cfp where cfp.meta = :meta");
                            deletionQuery.setEntity("meta", meta);
                            deletionQuery.executeUpdate();
                            
                            // Commit so we can read the file afterwards
                            commitHibernateTransaction();
                            
                            /*
                            // Annotate the file
                            WGDocumentImpl doc = createDocumentImpl(meta.getParentcontent());
                            TemporaryFile tempFile = new TemporaryFile(meta.getName(), doc.getFileData(meta.getName()), WGFactory.getTempDir());
                            try {
                                WGFileMetaData md = new WGFileMetaData(new WGDocument.FakeMetaDataContext(), meta.getName(), meta.getSize(), meta.getCreated(), meta.getLastmodified(), meta.getChecksum(), meta.getChecksumSha512(), fileHandling.loadMdExtensionData(doc, meta));
                                _db.annotateMetadata(tempFile.getFile(), md, null);
                                fileHandling.storeMdExtensionData(doc, md, meta);
                                if (isSaveIsolationActive()) {
                                    getSession().update(meta); // This will not be able to store binary extension data, which however cannot be present before upgrading the file storage
                                }
                            }
                            finally {
                                tempFile.delete();
                            }
                            commitHibernateTransaction();
                            */
                        }
                        catch (Throwable e) {
                            log.error("Exception upgrading file", e);
                            rollbackHibernateTransaction(false);
                        }
                        counter++;
                    }
                
                    log.info("Clearing session cache");
                    refresh();
                    log.info("Running file storage maintenance to remove duplicate file data");
                    freedMemory+=dailyMaintenance(log);
                }
                finally {
                    Hibernate.close(oldMetas);
                }
            }
            
            log.info("Database: " + getDb().getDbReference()+ ": Upgrading file storage freed " + WGUtils.DECIMALFORMAT_STANDARD.format(freedMemory / 1024 / 1024) + " MB of file storage memory.");
            return freedMemory;
            
        }
        finally {
            _ugradeFileStorageRunning = false;
        }
        
    }

    protected long dailyMaintenance(Logger log) throws WGAPIException {
        return getFileHandling().dailyFileMaintenance(log);
    }

    @Override
    public int getChildEntryCount(WGStructEntry structEntry) throws WGAPIException {
        StructEntry hentry = (StructEntry) ((WGDocumentImpl) structEntry.getCore()).getEntity();
        Query query = getSession().createQuery("select count(*) from StructEntry as struct where struct.parententry = :entry");
        query.setParameter("entry", hentry);
        return ((Number) query.uniqueResult()).intValue();
    }





    @Override
    public int getRootEntryCount(WGArea area) throws WGAPIException {
        Area harea = (Area) ((WGDocumentImpl) area.getCore()).getEntity();
        Query query = getSession().createQuery("select count(*) from StructEntry as struct where struct.area = :area");
        query.setParameter("area", harea);
        return ((Number) query.uniqueResult()).intValue();
    }





    @Override
    public void clearSessionCache() throws WGAPIException {
        Session session = getSessionStatus().getSession();
        if (session != null) {
            session.flush();
            session.clear();
        }
        else {
            throw new WGClosedSessionException();
        }
    }





    public boolean isSaveIsolationActive() {
        return _saveIsolationActive;
    }





    protected FileHandling getFileHandling() {
        return _fileHandling;
    }





    public CSVersion getCsVersion() {
        return _csVersion;
    }


    private TableGenerator getTableGenerator(String seqName, Long startValue) throws InstantiationException, IllegalAccessException {
        
        TableGenerator tg = _generatorCache.get(seqName);
        if (tg != null && (startValue == null || startValue.equals(tg.getInitialValue()))) {
            return tg;
        }
        
        Properties props = new Properties();
        props.put(TableGenerator.TABLE_PARAM, "cs_sequences");
        props.put(TableGenerator.SEGMENT_COLUMN_PARAM, "name");
        props.put(TableGenerator.SEGMENT_VALUE_PARAM, seqName);
        props.put(TableGenerator.VALUE_COLUMN_PARAM, "value");
        props.put(TableGenerator.OPT_PARAM, "none");
        props.put(TableGenerator.IDENTIFIER_NORMALIZER, _conf.createMappings().getObjectNameNormalizer());
        
        if (startValue != null) {
            props.put(TableGenerator.INITIAL_PARAM, startValue.intValue());
        }
        
        tg = TableGenerator.class.newInstance();
        
        Dialect dialect = ((SessionFactoryImplementor) _sessionFactory).getJdbcServices().getDialect();        
        tg.configure(_sessionFactory.getTypeHelper().basic(Long.class), props, dialect);
        _generatorCache.put(seqName, tg);
        
        return tg;
        
        
    }


    @Override
    public synchronized boolean initSequence(String name, final long startValue, final boolean forceInit) throws WGAPIException {
        
        final String internalName = buildInternalSequenceName(name);
        Sequence s = (Sequence) getSession().get(Sequence.class, internalName);
        if (!forceInit && s != null) {
            return false;
        }
        
        try {
            deleteSequence(name);
            if (startValue > 1) {
                getTableGenerator(internalName, startValue-1).generate((SessionImplementor) getSession(), null);
            }
            return true;
            
        }
        catch (Throwable e) {
            throw new WGBackendException("Exception initializing sequence", e);
        }

        
    }

    public void deleteSequence(final String name) throws WGAPIException {
        final String internalName = buildInternalSequenceName(name);
        MasterSessionTask deleteSequence = new MasterSessionTask(_db) {

            @Override
            protected void exec(WGDatabase db) throws Throwable {
                Sequence s = (Sequence) getSession().get(Sequence.class, internalName);            
                if (s != null) {
                    getSession().delete(s);
                    commitHibernateTransaction();
                }
                
            }
        };
        try {
            deleteSequence.runWithExceptions();
        }
        catch (Throwable e) {
            throw new WGBackendException("Exception deleting sequence", e);
        }
    }

    @Override
    public long incrementSequence(String name) throws WGAPIException {
        final String internalName = buildInternalSequenceName(name);
        try {
            return incrementSystemSequence(internalName);
        }
        catch (Exception e) {
            throw new WGBackendException("Exception incrementing sequence", e);
        }
    }

    public long incrementSystemSequence(final String name) throws WGAPIException, InstantiationException, IllegalAccessException {
        return (Long) getTableGenerator(name, null).generate((SessionImplementor) getSession(), null);
    }

    public String buildInternalSequenceName(String name) throws WGIllegalDataException {
        
        if (!Constants.PATTERN_KEYS.matcher(name).matches()) {
            throw new WGIllegalDataException("Invalid sequence name: \"" + name + "\". Sequence names may only be alphanumeric plus the following characters: _.-:$");
        }
        
        return CUSTOM_SEQUENCE_NAMEPREFIX + String.valueOf(name);
    }
    
    @Override
    public boolean isSequenceInitialized(String name) throws WGAPIException {
        final String internalName = buildInternalSequenceName(name);
        Sequence s = (Sequence) getSession().get(Sequence.class, internalName);
        return (s != null);
    }
    
    @Override
    public long getLastSequenceIncrementValue(String name) throws WGAPIException {
        final String internalName = buildInternalSequenceName(name);
        Sequence s = (Sequence) getSession().get(Sequence.class, internalName);
        if (s != null) {
            return s.getValue();
        }
        else {
            return 0;
        }
    }
    
    @Override
    public List<String> getUsedSequenceNames() throws WGAPIException {

        Iterator i = getSession().createQuery("select seq.name from Sequence as seq").iterate();
        List<String> names = new ArrayList<String>();
        while (i.hasNext()) {
            String name = (String) i.next();
            if (name.startsWith(CUSTOM_SEQUENCE_NAMEPREFIX)) {
                names.add(name.substring(CUSTOM_SEQUENCE_NAMEPREFIX.length()));
            }
            
        }
        return names;
        
        
    }
    
    protected WGDatabaseRevision getLogRevision(LogEntry logEntry) {
        return _ddlVersion >= WGDatabase.CSVERSION_WGA5 ? WGDatabaseRevision.forValue(logEntry.getLog_id()) : WGDatabaseRevision.forValue(logEntry.getLogtime());
    }

	@Override
	public WGDocumentCore getStructEntryBySequence(long seq) throws WGAPIException {

		Query q = getSession().createQuery("select entity from ExtensionData ed where ed.name='page-sequence' and ed.number=:seq");
		q.setParameter("seq", (double)seq);
		List results = q.list();
		if (results.size() > 0) {
			Object struct = results.get(0);
			if(struct instanceof StructEntry)
				return createDocumentImpl((StructEntry)struct);
		}
		return null;
		
	}

	@Override
	public void createPageSequence(WGDocumentCore struct, boolean forceCreate) throws WGAPIException, InstantiationException, IllegalAccessException{
		if(struct instanceof WGDocumentImpl)
			((WGDocumentImpl)struct).createPageSequence(forceCreate);
	}
   
}
