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
import java.lang.ref.WeakReference;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Date;
import java.util.Deque;
import java.util.EventListener;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGDatabase.WGFakeTransaction;
import de.innovationgate.webgate.api.WGDatabase.WGRealTransaction;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.MasterLoginAuthSession;
import de.innovationgate.webgate.api.fake.WGFakeDocument;
import de.innovationgate.webgate.api.locking.LockOwner;



/**
 * Contains all information about the current database session.
 */
public class WGSessionContext implements LockOwner {
    
   
    public static final Logger LOG_COREFETCHING = Logger.getLogger("wga.api.corefetching");
    public static final Logger LOG_SESSIONS = Logger.getLogger("wga.api.sessions");
    /**
     * Predefined attribute key to be placed via {@link #setAttribute(Object, Object)} on a session to control "preemptive caching" behaviour, which automatically caches some frequently used values from a just retrieved document.
     * This is enabled by default. Store this with attribute value "false" to disable it.
     */
    public static final String SATTRIB_BYPASS_PREEMTIVE_CACHING = "BypassPreemptiveCaching";

    public class DocumentContext {
        private WGDocument document = null;
        private WGDocumentCore core = null;
        private long coreRetrievalTime = Long.MIN_VALUE;
        private boolean coreRetrieved = false;
        private boolean edited = false;
        private Object customData = null;
        private List<WGDocument.SaveAction> _afterSaveActions = new ArrayList<WGDocument.SaveAction>();
        
        
        public DocumentContext(WGDocument doc) {
            document = doc;
            
            // In case this is a temporary document with held core we directly determine if it is a new one, pending for editing
            try {
                if (document.isTemporary() && document.getTemporaryCore() != null && !document.getTemporaryCore().isSaved()) {
                    edited = true;
                }
            }
            catch (WGAPIException e) {
                WGFactory.getLogger().error("Exception determining save status of temporary document core", e);
            }

        }
        
        public WGDocumentCore getCore() {
            return core;
        }
        public void setCore(WGDocumentCore core) {
            if (core != null) {
                this.core = core;
                this.coreRetrieved = true;
                this.coreRetrievalTime = System.currentTimeMillis();
                this.edited  = false;
            }
        }
        public long getCoreRetrievalTime() {
            return coreRetrievalTime;
        }
        public boolean isCoreRetrieved() {
            return coreRetrieved;
        }
        
        public void dropCore(boolean untimelyDispose) {
            
            if (isCoreRetrieved()) {
                if (untimelyDispose) {
                    try {
                        core.dispose();
                    }
                    catch (WGAPIException e) {
                        WGFactory.getLogger().error("Exception disposing core of document " + document.getDocumentKey(), e);
                    }
                }
                core.setWGDocument(null);
                core = null;
                coreRetrievalTime = Integer.MIN_VALUE;
                coreRetrieved  = false;
                edited = false;
            }
            
        }

        public WGDocument getDocument() {
            return document;
        }

        public void setDocument(WGDocument document) {
            this.document = document;
        }

        public boolean isEdited() {
            return edited;
        }

        public void setEdited(boolean edited) {
            this.edited = edited;
        }

        public Object getCustomData() {
            return customData;
        }

        public void setCustomData(Object customData) {
            this.customData = customData;
        }

        public AuthenticationSession getAuthenticationSession() {
            return _authenticationSession;
        }

        public List<WGDocument.SaveAction> getAfterSaveActions() {
            return _afterSaveActions;
        }
    }
    
	private long _created;
	private boolean _maxCoreMessageShown = false;
	private WGUserAccess _userAccess = new WGUserAccess(WGDatabase.ANONYMOUS_USER, WGDatabase.ACCESSLEVEL_NOTLOGGEDIN, null);
	private AuthenticationSession _authenticationSession = null;
	private boolean _anonymous = false;
	private Map<Object,Object> _attributes = new HashMap<Object,Object>();
	private LinkedList<WGDocumentKey> _fetchedCores = new LinkedList<WGDocumentKey>();
	private Map<WGDocumentKey,DocumentContext> _documentContexts = new HashMap<WGDocumentKey, WGSessionContext.DocumentContext>();
	private int _totalFetchedCores = 0;
	
	private boolean _databaseUpdated = false;

	private WeakReference<WGDatabase> _db = null;
	private Object _credentials = null;
	private List<EventListener> _temporaryEventListeners = new ArrayList<EventListener>();
	
	private Deque<WGTransaction> _transactionsStack = new ArrayDeque<WGTransaction>(); 
	
	private String _user = null;
	private String _masterTenantUser = null;
	private boolean _eventThread = false;
	private boolean _masterSession = false;
	private boolean _eventsEnabled = true;
	
	private boolean _contentTypeEventsEnabled = true;
	private boolean _cascadeDeletions = true;
	private boolean _cachingEnabled = true;
	private boolean _testUniqueNames = true;
	private boolean _batchProcess = false;
	private boolean _protectedRelationsEnabled = true;
	private String _client = null;
	private int _maxDocs = Integer.MAX_VALUE;
	private String _task = "(unknown)";
    private Date _databaseChangeDateAtStart;
    private WGFakeTransaction _legacyFakeTransaction;
    
    /**
     * The default transaction mode of this database backend. Should commit changes on every save() operation.
     */
    public static final int TRANSACTION_MODE_DEFAULT = 1;
    /**
     * Manual control of transaction commit by using {@link WGDatabase#startTransaction()}
     */
    public static final int TRANSACTION_MODE_MANUAL = 2;
    
    private int _transactionMode = TRANSACTION_MODE_DEFAULT;    
    private int _previousTransactionMode = TRANSACTION_MODE_DEFAULT;
    private Comparable<?> _revisionAtStart;
    private WGUserAccess _originalUserAccess;
    private boolean _closed = false;
    private Set<WGDocument> _autoSaveDocs = new HashSet<WGDocument>();

    protected WGSessionContext(WGDatabase db, AuthenticationSession authSession, Object credentials, WGUserAccess userAccess, WGUserAccess originalUserAccess) throws WGAPIException {
		this._db = new WeakReference<WGDatabase>(db);
		
		
		this._masterSession  = authSession instanceof MasterLoginAuthSession;
		if (_masterSession) {
		    this._user = WGDatabase.MASTER_USERNAME;
		    this._credentials = WGDatabase.MASTER_USERNAME;
		}
		else {
		    this._user = authSession.getDistinguishedName();
		    this._credentials = credentials;
		}
		userAccess.setPrimaryName(this._user);
		
		this._userAccess = userAccess;
		this._originalUserAccess = originalUserAccess;
		this._authenticationSession = authSession;
		
		this._maxDocs = db.getMaxCores();
		this._created = System.currentTimeMillis();
		this._revisionAtStart = db.getRevision();
        this._databaseChangeDateAtStart = db.getLastChanged();
        this._cachingEnabled = db.isCachingEnabled();
        this._legacyFakeTransaction = db.new WGFakeTransaction();
		
		if (this._user != null && this._user.equalsIgnoreCase(WGDatabase.ANONYMOUS_USER)) {
			this._anonymous = true;
		}
		
        if (isLogSessionsDebugEnabled()) {
            LOG_SESSIONS.debug("Session " + System.identityHashCode(this) + " created, database " + db.getDbReference() + ", user " + _user + ", thread: " + Thread.currentThread().getName());
        }
	}



    private boolean isLogSessionsDebugEnabled() {
        return LOG_SESSIONS.isDebugEnabled() && !WGFactory.isEventThread();
    }

    
	
	/**
	 * Adds a fetched document core to the document cores list. 
	 * This method implements the maxdocs reduction, where the number of parallel retrieved doc cores are reduced to the maxdocs setting on the database object. 
	 * @param doc
	 */
	public void addFetchedCore(WGDocument doc, WGDocumentCore core) {
	    
		getOrCreateDocumentContext(doc).setCore(core);
		if (WGFakeDocument.isFake(core)) {
		    return;
		}
		
		_totalFetchedCores++;
		_fetchedCores.add(doc.getDocumentKeyObj());
		
		if (LOG_COREFETCHING.isDebugEnabled()) {
		    LOG_COREFETCHING.debug("Thread " + Thread.currentThread().hashCode() + " fetched document " + doc.getDocumentKey());
		}
		
		if (!doc.getDatabase().hasFeature(WGDatabase.FEATURE_UNLIMITED_CORES) && _fetchedCores.size() > _maxDocs) {
			if (!_maxCoreMessageShown) {
			    if (!isBatchProcess()) {
			        WGFactory.getLogger().warn("Database '" + _db.get().getTitle() + "' (" + _db.get().getTypeName() + " " + _db.get().getPath() + ") uses more than " + _maxDocs + " documents when performing task '" + getTask() + "'.");
			        WGFactory.getLogger().warn("This task will be low on performance. It might need to be optimized or the database option '" + WGDatabase.COPTION_MAXCORES + "' might need to be raised.");	
			    }
				_maxCoreMessageShown = true;
			}
			
			WGDocumentKey docToRemove = (WGDocumentKey) _fetchedCores.removeFirst();
	         // Do not drop if doc is temporary or the doc that was just added (which would be fatal)
			if (!doc.getDocumentKeyObj().equals(docToRemove)) {
			    DocumentContext docContext = getDocumentContext(docToRemove);
			    if (docContext != null) {
			        // Do not drop from temporary documents - They cannot be refetched
			        if (docContext.getDocument() == null || !docContext.getDocument().isTemporary()) {
			            docContext.dropCore(true);
			        }
			    }
			}
		}
	}   
	
	protected void addTemporaryEventListener(EventListener doc) {
        
        if (!_temporaryEventListeners.contains(doc)) {
            this._temporaryEventListeners.add(doc);
        }
	}

	/**
	 * The access level of the current user.
	 * @return A constant of WGDatabase.ACCESSLEVEL_...
	 */
	public int getAccessLevel() {
		return _userAccess.getAccessLevel();
	}
    
    /**
     * Returns the complete access rights information of current session
     */
    public WGUserAccess getUserAccess() {
        return _userAccess;
    }
	
	/**
	 * Retrieves a custom attribute added to this session.
	 * @param key Name of the attribute
	 * @return The attribute value
	 */
	public Object getAttribute(Object key) {
		return this._attributes.get(key);
	}

	/**
	 * The password used to open this session, if any
	 */
	public String getPassword() {
		if (_credentials instanceof String) {
		    return (String) _credentials;
        }
        else {
            return null;
        }
	}
    
    /**
     * Returns the credentials object used to open this session
     */
    public Object getCredentials() {
        return _credentials;
    }
	
	/**
	 * If the database supports feature WGDatabase.FEATURE_SESSIONTOKEN, a session token that can be used to reconnect to this database session after this WGA session has been closed. 
	 * @throws WGBackendException 
	 */
	public String getSessionToken() throws WGBackendException {
	    if (_db.get().getAuthenticationModule().isGeneratesSessionToken()) {
            return _authenticationSession.getSessionToken();
        }
        else {
            return null;
        }
	}
	
	/**
	 * The current users name.
	 * @return Returns a String
	 */
	public String getUser() {
	    if (isMasterSession() && _masterTenantUser != null) {
	        return _user + " (for " + _masterTenantUser + ")";
	    }
	    else {
	        return _user;
		}
	}

	/**
	 * Determines, if this is an anonymous session
	 */
	public boolean isAnonymous() {
		return this._anonymous;
	}

	/**
	 * Determines, if this database has been updated since last session or inside this session.
	 * @return Returns a boolean
	 */
	public boolean isDatabaseUpdated() {
		return _databaseUpdated;
	}



	/**
	 * Determine, if the current user has designer rights, i.e. the ability to edit Languages, File Containers, TML and CSS/JS modules
	 * @deprecated There are no multiple designer access levels any more. Directly test for access level "manager" instead. 
	 */
	public boolean isDesigner() {

		int level = getAccessLevel();
		if (level == WGDatabase.ACCESSLEVEL_MANAGER) {
			return true;
		}
		else {
			return false;
		}

	}
	/**
	 * Determines if a (real) manual transaction is currently in process
	 */
	public boolean isTransactionActive() {
		if (_transactionsStack.isEmpty()) {
		    return false;
		}
		
		WGTransaction trans = _transactionsStack.getLast();
		return (trans instanceof WGRealTransaction);
	}
	
	/**
	 * Sets a custom attribute to this session. These attributes will get discarded when the session ends.
	 * @param key The attribute name
	 * @param value The attribute value
	 */
	public void setAttribute(Object key, Object value) {
		this._attributes.put(key, value);
	}
	/**
	 * Sets the databaseUpdated
	 * @param databaseUpdated The databaseUpdated to set
	 */
	protected void setDatabaseUpdated(boolean databaseUpdated) {
		_databaseUpdated = databaseUpdated;
	}


	protected void addTransactionToStack(WGTransaction t) throws WGAPIException {
		_transactionsStack.push(t);	    
	}
	
   protected void removeTransactionFromStack(WGTransaction t) throws WGAPIException {
        WGTransaction otherT = _transactionsStack.pop();
        if (otherT != t) {
            throw new WGIllegalStateException("Wrong transaction cascading. Closed Transaction is not top transaction.");
        }
    }


	/**
	 * Determines if the current session runs in the WGAPI event thread
	 */
	public boolean isEventThread() {
		return _eventThread;
	}

	/**
	 * @param b
	 */
	protected void setEventThread(boolean b) {
		_eventThread = b;
	}

	/**
	 * Returns if this session is a master session (i.e. build with databases master login)
	 */
	public boolean isMasterSession() {
		return _masterSession;
	}

	/**
	 * Determines, if events are enabled in this session
	 */
	public boolean isEventsEnabled() {
		return _eventsEnabled;
	}

	/**
	 * Sets if events should be enabled for this session
	 * @param b
	 */
	public void setEventsEnabled(boolean b) {
		_eventsEnabled = b;
	}
	
	
	/**
	 * Returns details about the current user if the DB implementation supports this, otherwise null.
	 * @throws WGAPIException 
	 */
	public WGUserDetails getUserDetails() throws WGAPIException {
		
		if (!_db.get().isSessionOpen()) {
		    throw new WGClosedSessionException();
		}
		
		if (_userAccess instanceof WGUserDetails) {
		    return (WGUserDetails) _userAccess;
		}
		else {
		    return null;
		}
		 
	}

	/**
	 * Determines, if cascaded deletions are active in this session (i.e. removing a parent document automatically removes it's children)
	 */
	public boolean isCascadeDeletions() {
		return _cascadeDeletions;
	}

	/**
	 * Sets if cascaded deletions (removing a parent document automatically removes it's children) should be enabled
	 * @param b
	 */
	public void setCascadeDeletions(boolean b) {
		_cascadeDeletions = b;
	}

	/**
	 * Returns the current client software which uses the WGAPI.
	 */
	public String getClient() {
		return _client;
	}

	/**
	 * Sets the WGA client software that is currently using the WGAPI.
	 * @param string A wga client string.
	 */
	public void setClient(String string) {
		_client = string;
	}

	/**
	 * Returns the task the WGA client is executing in this session. This must be set by the WGA client externally.
	 */
	public String getTask() {
		return _task;
	}

	/**
	 * Sets the task that the current WGA client is accomplishing
	 * @param string
	 */
	public void setTask(String string) {
		_task = string;
	      if (isLogSessionsDebugEnabled()) {
	            LOG_SESSIONS.debug("Session " + System.identityHashCode(this) + ", Task set: " + string);
	        }
	}

	/**
	 * Returns the total number of fetched cores in this session (without core reduction)
	 */
	public int getTotalFetchedCores() {
		return _totalFetchedCores;
	}

	/**
	 * Determines, if the message that the maxdocs threshold has been exceeded, has already been put out to the log.
	 */
	public boolean isMaxCoreMessageShowed() {
		return _maxCoreMessageShown;
	}
	
	/**
	 * Determines if any document has been edited in the current session whose changes are not yet saved
	 */
	public boolean isAnyDocumentEdited() {
	    
	    for (DocumentContext con : _documentContexts.values()) {
	        
	        if (con.isEdited()) {
	            return true;
	        }
	        
	    }
	    
	    return false;
	    
	}

	/**
	 * Returns the creation date of this session.
	 */
	public Date getCreated() {
		return new Date(_created);
	}

	/**
	 * Determines, if caching is enabled for this session.
	 */
	public boolean isCachingEnabled() {
		return _cachingEnabled && !isTransactionActive();
	}
    
    /**
     * Returns if we currently may write to the cache.
     */
    public boolean isCacheWritingEnabled() {
        
        if (!isCachingEnabled()) {
            return false;
        }
        
        return true;
        
    }

	/**
	 * Sets, if caching is enabled for this session.
	 * @param b
	 */
	public void setCachingEnabled(boolean b) {
		_cachingEnabled = b;
	}

	/**
	 * Determines if unique content names are tested before saving content.
	 */
	public boolean isTestUniqueNames() {
		return _testUniqueNames;
	}

	/**
	 * Sets if unique content names should be tested before storing content in this session
	 * @param b
	 */
	public void setTestUniqueNames(boolean b) {
		_testUniqueNames = b;
	}

    /**
     * Returns the maximum number of backend documents that session may fetch at once.
     */
    public int getMaxDocs() {
        return _maxDocs;
    }
    /**
     * Sets the maximum number of backend documents that the current session may fetch at once.
     * If more documents are used by this session the oldest document gets disposed (and must be fetched again if it will be used again).
     * This defaults to 2000 for normal, 100 for batch processes.
     * @param maxDocs The maxDocs to set.
     */
    public void setMaxDocs(int maxDocs) {
        this._maxDocs = maxDocs;
    }
    /**
     * Returns if the current session is marked as belonging to a batch process, processing mass data.
     */
    public boolean isBatchProcess() {
        return _batchProcess;
    }
    /**
     * Sets it this session is marked as belonging to a batch process.
     * A session that is marked for batch processing will have many caches deactivated and will have getMaxDocs()=100 (if MaxDocs setting is not below that).
     */
    public void setBatchProcess(boolean batchProcess) {
        this._batchProcess = batchProcess;
        setCachingEnabled(!batchProcess);
        if (batchProcess && _db.get().getMaxCores() > 100) {
            setMaxDocs(100);
        }
        else {
            setMaxDocs(_db.get().getMaxCores());
        }
        
    }

    /**
     * Returns the last changed date that the opened database had at the beginning of this session
     */
    public Date getDatabaseChangeDateAtStart() {
        return _databaseChangeDateAtStart;
    }
    
    protected DocumentContext getOrCreateDocumentContext(WGDocument doc) {
        
        WGDocumentKey docKey = doc.getDocumentKeyObj();
        
        /*
        // For temp duplicates we use a qualifier on the key, so it has a context key distinct from the original (#00004139)
        if (doc.isTempDuplicate()) {
            docKey = docKey.withQualifier(String.valueOf(doc.hashCode()));
        }
        */
        
        DocumentContext con = getDocumentContext(docKey);
        if (con == null) {
            con = new DocumentContext(doc);

            // We wont cache document contextes for deleted documents
            // We cannot use doc.isDeleted() bc. that may recursely return to this method
            if (!doc.isDeletedFlag()) {
                _documentContexts.put(docKey, con);
            }
        }
        else {
            con.setDocument(doc);
        }
        return con;
    }
    
    /**
     * Returns a list of documents that were edited in the current session and have not yet been saved
     */
    public List<WGDocument> getEditedDocuments() {
        
        Iterator<DocumentContext> contextsIt = _documentContexts.values().iterator();
        List<WGDocument> docs = new ArrayList<WGDocument>();
        while (contextsIt.hasNext()) {
            DocumentContext context = (DocumentContext) contextsIt.next();
            try {
                if (context.isEdited() && context.getDocument().isReadableForUser()) {
                    docs.add(context.getDocument());
                }
            }
            catch (WGAPIException e) {
            }
        }
        return docs;
        
    }

    protected DocumentContext getDocumentContext(WGDocumentKey docKey) {
        DocumentContext con = (DocumentContext) _documentContexts.get(docKey);
        return con;
    }

    protected void dropAllDocumentCores(boolean untimelyDispose) {
        Iterator<DocumentContext> contexts = _documentContexts.values().iterator();
        while (contexts.hasNext()) {
            DocumentContext context = (DocumentContext) contexts.next();
            try {
                if (context.isCoreRetrieved()) {
                    context.getDocument().dropCore(untimelyDispose, true);
                }
            }
            catch (WGClosedSessionException e) {}
        }
    }

    protected void dropResources() {
        _documentContexts.clear();
        _fetchedCores.clear();
        if (_authenticationSession != null && _authenticationSession.isValid()) {
            _authenticationSession.logout();
        }

    }

    protected void remapDocumentContext(WGDocumentKey oldId, WGDocumentKey newId) {
        DocumentContext con = (DocumentContext) _documentContexts.remove(oldId);
        if (con != null) {
            _documentContexts.put(newId, con);
        }
    }
    
    protected void removeDocumentContext(WGDocumentKey id) {
        _documentContexts.remove(id);
    }

    /**
     * Returns the session on the authentication module in use for this database session
     */
    public AuthenticationSession getAuthenticationSession() {
        return _authenticationSession;
    }
    
    /**
     * Clears the session private cache including all fetched document cores.
     * Note: All unsaved document modifications will be gone after calling this method 
     * @throws WGAPIException 
     */
    public void clearCache() throws WGAPIException {
        dropAllDocumentCores(true);
        WGDatabase db = this._db.get();
        if (db != null) {
            db.getCore().clearSessionCache();
        }
        
        _documentContexts.clear();
        _fetchedCores.clear();
    }

    /**
     * Returns if protection for protected relations is enabled. Defaults to true.
     * This protection will prevent contents from being removed while they are the target of a protected relation. 
     */
    public boolean isProtectedRelationsEnabled() {
        return _protectedRelationsEnabled;
    }

    /**
     * Sets if protection for protected relations is enabled. Defaults to true.
     * This protection will prevent contents from being removed while they are the target of a protected relation. 
     */
    public void setProtectedRelationsEnabled(boolean protectedRelationsEnabled) {
        this._protectedRelationsEnabled = protectedRelationsEnabled;
    }

    /**
     * Returns if content type events are enabled for this session
     */
    public boolean isContentTypeEventsEnabled() {
        return _contentTypeEventsEnabled;
    }

    /**
     * Sets if content type events are enabled on this session
     */
    public void setContentTypeEventsEnabled(boolean contentTypeEventsEnabled) {
        this._contentTypeEventsEnabled = contentTypeEventsEnabled;
    }

    /**
     * Sets the transaction mode for this session. Never use that directly. Control transactions using {@link WGDatabase#startTransaction()} and the methods of the returned {@link WGTransaction} object
     * @param transactionMode The transaction mode. Use constants WGSessionContext.TRANSACTION_MODE...
     */
    public void setTransactionMode(int transactionMode) {
        _previousTransactionMode = _transactionMode;
        _transactionMode = transactionMode;
    }

    /**
     * Returns the current transaction mode of the session of one of the constants WGSessionContext.TRANSACTION_MODE...
     */
    public int getTransactionMode() {
        return _transactionMode;
    }
    
    /**
     * Sets the transaction mode back to the initial transaction mode of the session
     */
    public void resetTransactionMode() {
        _transactionMode = _previousTransactionMode;
    }
    
    /**
     * Removes a session attribute
     * @param name Then name of the attribute
     */
    public void removeAttribute(String name) {
        this._attributes.remove(name);
    }

    /**
     * Returns the database revision that was current when the session started
     */
    public Comparable<?> getRevisionAtStart() {
        return _revisionAtStart;
    }

    /**
     * If an {@link UserAccessFilter} is active on the session returns the original user access of the current user without the filter. If no filter is active returns null.
     */
    public WGUserAccess getOriginalUserAccess() {
        return _originalUserAccess;
    }

    /**
     * When this is a master session can hold the name of the user that triggered this master session
     */
    public String getMasterTenantUser() {
        return _masterTenantUser;
    }

    /**
     * On master sessions can be used to set the name of the user that triggered the master session. This information will be put out on the user name within this master session.
     * @param masterTenantUser
     * @throws WGAPIException
     */
    public void setMasterTenantUser(String masterTenantUser) throws WGAPIException {
        
        if (!isMasterSession()) {
            throw new WGIllegalStateException("The current session is no master session");
        }
        
        _masterTenantUser = masterTenantUser;
    }
    
    /**
     * Checks ift the given document core is attached to the current session
     * @return True if the core is still active, false if it is no longer attached and the same document is now represented by a differen core instance
     */
    public boolean isActiveCore(WGDocumentCore core) throws WGAPIException {
        
        if (isClosed()) {
            throw new WGClosedSessionException();
        }
        
        if (core.isTemporary()) {
            return true;
        }
        
        WGDocumentKey docKey = WGDocument.buildDocumentKey(core, _db.get());
        DocumentContext docContext = _documentContexts.get(docKey);
        if (docContext != null && docContext.getCore() == core) {
            return true;
        }
        else {
            return false;
        }
        
    }

    protected void close() {
        _closed = true;
        dropResources();
        if (isLogSessionsDebugEnabled()) {
            LOG_SESSIONS.debug("Session " + System.identityHashCode(this) + " closed");
        }
    }

    protected boolean isClosed() {
        return _closed;
    }
    
    public void addAfterTransactionTask(Callable<Object> r) {
        
        for (WGTransaction t  :_transactionsStack) {
            if (t instanceof WGRealTransaction) {
                ((WGRealTransaction) t).addAfterCommitTask(r);
                return;
            }
        }
        
        try {
            r.call();
        }
        catch (Exception e) {
            WGFactory.getLogger().error("Exception executing after-transaction operation inline", e);
        }
        
    }

    protected WGFakeTransaction getLegacyFakeTransaction() {
        return _legacyFakeTransaction;
    }

    protected Deque<WGTransaction> getTransactionsStack() {
        return _transactionsStack;
    }



    protected void preClose() {

        for (WGDocument doc : _autoSaveDocs) {
            try {
                if (doc.isEdited()) {
                    doc.save();
                }
            }
            catch (WGAPIException e) {
                WGFactory.getLogger().error("Exception autosaving document " + doc.getDocumentKey(), e);
            }
        }
        
    }
    
    protected void addAutoSaveDoc(WGDocument doc) {
        _autoSaveDocs.add(doc);
    }
    
}

