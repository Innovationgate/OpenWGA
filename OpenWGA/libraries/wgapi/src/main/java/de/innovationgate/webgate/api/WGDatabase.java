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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.security.cert.X509CRL;
import java.security.cert.X509Certificate;
import java.text.Collator;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Semaphore;

import javax.activation.DataSource;
import javax.servlet.http.HttpServletRequest;

import org.apache.commons.collections.SortedBag;
import org.apache.commons.collections.bag.TreeBag;
import org.apache.commons.collections.map.ListOrderedMap;

import com.google.common.collect.MapMaker;

import de.innovationgate.utils.CertificateValidationUtils;
import de.innovationgate.utils.ConcurrentWeakHashMap;
import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.UserHashMap;
import de.innovationgate.utils.UserHashMapGroup;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheDisposalListener;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.webgate.api.PageRightsFilter.Right;
import de.innovationgate.webgate.api.auth.AnonymousAuthSession;
import de.innovationgate.webgate.api.auth.AnonymousAwareAuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthSessionWithUserCacheQualifier;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.webgate.api.auth.BackendAuthSession;
import de.innovationgate.webgate.api.auth.CertAuthCapableAuthModule;
import de.innovationgate.webgate.api.auth.LabeledNamesProvider;
import de.innovationgate.webgate.api.auth.MasterLoginAuthSession;
import de.innovationgate.webgate.api.auth.RedirectionAuthModule;
import de.innovationgate.webgate.api.auth.RequestAwareAuthenticationModule;
import de.innovationgate.webgate.api.fake.WGFakeDatabase;
import de.innovationgate.webgate.api.fake.WGFakeDocument;
import de.innovationgate.webgate.api.fake.WGFakeLanguage;
import de.innovationgate.webgate.api.locking.Lock;
import de.innovationgate.webgate.api.locking.LockException;
import de.innovationgate.webgate.api.locking.LockManager;
import de.innovationgate.webgate.api.locking.LockOwner;
import de.innovationgate.webgate.api.locking.Lockable;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.webgate.api.schemadef.WGAreaDefinition;
import de.innovationgate.webgate.api.schemadef.WGContentItemDefinition;
import de.innovationgate.webgate.api.schemadef.WGContentTypeDefinition;
import de.innovationgate.webgate.api.schemadef.WGLanguageDefinition;
import de.innovationgate.webgate.api.schemadef.WGMetaFieldDefinition;
import de.innovationgate.webgate.api.schemadef.WGSchemaDefinition;
import de.innovationgate.webgate.api.schemadef.WGSchemaDocumentDefinition;
import de.innovationgate.webgate.api.servers.WGDatabaseServer;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.webgate.api.workflow.WGDefaultWorkflowEngine;
import de.innovationgate.webgate.api.workflow.WGWorkflowEngine;
import de.innovationgate.webgate.api.workflow.WGWorkflowEvent;
import de.innovationgate.webgate.api.workflow.WGWorkflowEventListener;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wga.model.VersionCompliance;
import de.innovationgate.wga.modules.options.JSONListOptionType;
import de.innovationgate.wga.modules.options.OptionConversionException;

/**
 * <p>
 * Represents a WGA Database. Can be a content database, design database,
 * repository database, user profile database or a mixture of it. The Roles of a
 * database give info about it's contents. Can be retrieved via getRoles().
 * </p>
 * 
 * <h4>Usage patterns:</h4>
 * 
 * <p>
 * <b>Simple use: </b>
 * <p/>
 * <p>
 * This is recommended, when there are is only one stream of operation to be
 * done and the used databases will not be opened again for a time.
 * </p>
 * 
 * <code>
 * WGDatabase db = WGFactory.getInstance().openDatabase(dbtype, dbpath, username, password, options);<br/>
 * ... db actions ...<br/>
 * db.close();<br/>
 * </code>
 * <p>
 * After db.close() the database object must not be used further.
 * </p>
 * 
 * 
 * 
 * <p>
 * <b>Recurring and multithreaded use: </b>
 * </p>
 * <p>
 * This is recommended when the opened databases should be reused at later time
 * and/or will be used by multiple threads. This mode introduces the "database
 * session", that will open a WGDatabase object for usage and can be closed
 * without completely closing the database object and opened again later.
 * </p>
 * <code>
 * // Initial opening - Opens the database and implicitly a session too<br/>
 *  WGDatabase db = WGFactory.getInstance().openDatabase(dbtype, dbpath, username, password, options);<br/>
 *  ... db actions ...<br/>
 *  db.closeSession();<br/>
 * <br/>
 *  ...<br/>
 * <br/>
 *  // Recurring opening - maybe in another thread using the same db object<br/>
 *  db.openSession(username, password)<br/>
 *  ... db actions ...<br/>
 *  db.closeSession();<br/>
 * <br/>
 *  ...<br/>
 * <br/>
 *  // Final closing at the end of the program - No further WGA processing after that<br/>
 *  db.close();<br/>
 * </code>
 * <p>
 * This mode not only allows the WGAPI to keep database connections but also
 * allows for caching of field values.
 * </p>
 * 
 */
public class WGDatabase implements Lockable, WGDesignChangeListener, PageHierarchyNode, AuthenticationSourceListener, WGExtensionDataContainer {
    
    public static final String EXTDATA_DATABASE_UUID = "database_uuid";
    public static final String EXTDATA_PATCH_LEVEL = "ddlpatchlevel";
    public static final String EXTDATA_DERIVATE_CREATORS = "filederivates_creators";
    public static final String EXTDATA_DERIVATE_REVISION = "filederivates_lastupdaterevision";
    public static final String EXTDATA_CS_SYNC_RUNNING = "contentstore_syncing";
    
    /**
     * Extension data fields on the database which are no regular data and do not represent any database state and should be protected from data deletion operations 
     */
    public static final List<String> PROTECTED_DATABASE_EXTDATA = Arrays.asList(new String[] {EXTDATA_DATABASE_UUID, EXTDATA_PATCH_LEVEL}); 
    
    public static final int DOCUMENTCACHE_SIZE_DEFAULT = 5000;
    
    public static final int NAMECACHE_SIZE_DEFAULT = 5000;
    
    public static final int QUERYCACHE_SIZE_DEFAULT = 1000;

    /** Content store version identifying a database that is no content store at all */
    public static final double CSVERSION_NO_CONTENTSTORE = 0;
    
    /** Content store version identifying a content store used as of WGA version 3.0 */
    public static final double CSVERSION_WGA3 = 3;
    
    /** Content store version identifying a content store used as of WGA version 4.1, including optimized file handling */
    public static final double CSVERSION_WGA4_1 = 4.1;

    /** Content store version identifying a content store used as of WGA version 5 */
    public static final double CSVERSION_WGA5 = 5;

    /**
     * Name of the implicit "authenticated" group holding all users that were able to authenticate themselves
     */
    public static final String AUTHENTICATED_GROUP = "authenticated";
    
    public class ReconfigurationAccessor {
        
        public void reconfigureCore(boolean prepareOnly, WGUserAccess userAccess) throws WGAPIException {
            WGDatabase.this.reconfigureCore(prepareOnly, userAccess);
        }
        
    }
    
    public class WGRealTransaction implements WGTransaction {
        
        private boolean _open = true;
        private List<Callable<Object>> _afterCommitTasks = new ArrayList<Callable<Object>>();

        @Override
        public boolean commit() throws WGAPIException {
            
            getSessionContext().removeTransactionFromStack(this);
            _open = false;
            if (!doCommitTransaction()) {
                throw new WGBackendException("Transaction commit failed");
            }
            for (Callable<Object> r : _afterCommitTasks) {
                try {
                    r.call();
                }
                catch (Exception e) {
                    WGFactory.getLogger().error("Exception executing after-transaction operation", e);
                }
            }
            return true;
            
        }

        @Override
        public void rollback() {
            _open = false;
            try {
                if (!doRollbackTransaction()) {
                    throw new WGAPIException("Transaction rollback failed because of unknown cause");
                }
            }
            catch (Throwable t) {
                WGFactory.getLogger().error("Transaction rollback failed", t);
            }
            finally {
                try {
                    getSessionContext().removeTransactionFromStack(this);
                }
                catch (Throwable t) {
                    WGFactory.getLogger().error("Exception removing transaction from stack", t);
                }
            }
        }
        
        @Override
        public boolean isOpen() {
            return _open;
        }
        
        public void addAfterCommitTask(Callable r) {
            _afterCommitTasks.add(r);
        }
        
    }
    
    public class WGCascadedTransaction implements WGTransaction {
        
        private boolean _open = true;
        
        @Override
        public boolean commit() throws WGAPIException {
            _open = false;
            verboseCacheManagement("Committing cascaded transaction, no backup changes catchup. Stack: " + WGUtils.serializeCollection(getSessionContext().getTransactionsStack(), " <- "));
            getSessionContext().removeTransactionFromStack(this);
            return false;
        }

        @Override
        public void rollback() {
            _open = false;
            try {
                if (!doRollbackTransaction()) {
                    throw new WGAPIException("Transaction rollback failed because of unknown cause");
                }
            }
            catch (Throwable t) {
                WGFactory.getLogger().error("Transaction rollback failed", t);
            }
            finally {
                try {
                    getSessionContext().removeTransactionFromStack(this);
                }
                catch (Throwable t) {
                    WGFactory.getLogger().error("Exception removing transaction from stack", t);
                }
            }
        }
        
        @Override
        public boolean isOpen() {
            return _open;
        }
        
    }
    
    public class WGFakeTransaction implements WGTransaction {
        
        private boolean _open = true;
        
        @Override
        public boolean commit() throws WGAPIException {
            _open = false;
            getSessionContext().removeTransactionFromStack(this);
            verboseCacheManagement("Committing fake transaction. No cache maintenance");
            return false;
        }

        @Override
        public void rollback() {
            _open = false;
            try {
                getSessionContext().removeTransactionFromStack(this);
            }
            catch (Throwable t) {
                WGFactory.getLogger().error("Exception removing transaction from stack", t);
            }
            
        }
        
        public boolean isOpen() {
            return _open;
        };
        
    }
    
    /**
     * Represents an ACL privilege for an ACL level
     */
    public static class AccessLevelPrivilege {
        
        public AccessLevelPrivilege(boolean defaultValue, boolean available) {
            super();
            this._defaultValue = defaultValue;
            this._available = available;
        }
        private boolean _defaultValue;
        private boolean _available;
        /**
         * Returns if the privilege is default
         */
        public boolean isDefaultValue() {
            return _defaultValue;
        }
        /**
         * Returns if the privilege is available
         */
        public boolean isAvailable() {
            return _available;
        }

                
    }
    
    
    /**
     * Represents an ACL Access Level
     */
    public static class AccessLevel {
        
        public static final String ACCESSLEVEL_GROUP_PREFIX = "accesslevel.";
        
        private int _code;
        private String _name;
        private AccessLevelPrivilege _allowMovingStructEntries;
        private AccessLevelPrivilege _allowDeletingDocuments;
        private AccessLevelPrivilege _allowDirectAccess;
        private boolean _usageInACL;
        
        private AccessLevel(int code, String name, boolean usageInACL, AccessLevelPrivilege allowMovingStructEntries, AccessLevelPrivilege allowDeletingDocuments, AccessLevelPrivilege allowUrlAccess) {
            _code = code;
            _name = name;
            
            _usageInACL = usageInACL;
            
            _allowMovingStructEntries = allowMovingStructEntries;
            _allowDeletingDocuments = allowDeletingDocuments;
            _allowDirectAccess = allowUrlAccess;
        }
        
        
        /**
         * Returns if users of this level have the privilege to move pages by default
         */
        public boolean isAllowMovingStructEntriesDefault() {
            return _allowMovingStructEntries.isDefaultValue();
        }


        /**
         * Returns if users of this level have the privilege to delete documents by default
         */
        public boolean isAllowDeletingDocumentsDefault() {
            return _allowDeletingDocuments.isDefaultValue();
        }
        
        /**
         * Returns if users of this level have the privilege for direct access by default
         */
        public boolean isAllowDirectAccessDefault() {
            return _allowDirectAccess.isDefaultValue();
        }
        
        /**
         * Returns if users of this level may have the privilege to move pages
         */
        public boolean isAllowMovingStructEntriesAvailable() {
            return _allowMovingStructEntries.isAvailable();
        }


        /**
         * Returns if users of this level may have the privilege to delete documents
         */
        public boolean isAllowDeletingDocumentsAvailable() {
            return _allowDeletingDocuments.isAvailable();
        }
        
        /**
         * Returns if users of this level may have the privilege for direct access
         */
        public boolean isAllowDirectAccessAvailable() {
            return _allowDirectAccess.isAvailable();
        }


        /**
         * Returns the numeric code
         */
        public int getCode() {
            return _code;
        }
        /**
         * Returns the name
         */
        public String getName() {
            return _name;
        }
        
        /**
         * Returns the name of the group containing all users having at least this access level
         */
        public String getGroupName() {
            
            return ACCESSLEVEL_GROUP_PREFIX + getName().toLowerCase();
            
        }


        /**
         * Returns if this level may be used for ACL entries
         */
        public boolean isUsageInACL() {
            return _usageInACL;
        }



        
        
    }
    
    private class DocumentCacheDisposalListener implements CacheDisposalListener {
        

        
        public void disposeAll() {
            
            // Just remove all document cores from session. Other cache cleanups will be done in WGDatabase.refresh(), which triggers this
            if (isSessionOpen()) {
                getSessionContext().dropAllDocumentCores(true);
            }
        }

        public void dispose(String key, Object value) {
            if (value instanceof WGDocument) {
                try {
                    ((WGDocument) value).dispose();
                }
                catch (WGAPIException e) {
                    WGFactory.getLogger().error("Exception disposing document object", e);
                }
            }
            
        }
    }

    public class DocumentCollectionHierarchy implements PageHierarchyNode {
        
        private int _type;

        public DocumentCollectionHierarchy(int type) {
            _type = type;
        }

        public Class<? extends WGDocument> getChildNodeType() {
            switch (_type) {
                case WGDocument.TYPE_AREA:
                    return WGArea.class;
                    
                case WGDocument.TYPE_LANGUAGE:
                    return WGLanguage.class;
                    
                case WGDocument.TYPE_CONTENTTYPE:
                    return WGContentType.class;
                    
                case WGDocument.TYPE_CSSJS:
                    return WGScriptModule.class;
                    
                case WGDocument.TYPE_TML:
                    return WGTMLModule.class;
                    
                case WGDocument.TYPE_FILECONTAINER:
                    return WGFileContainer.class;
                    
                default:
                    throw new IllegalStateException("Unknown document type " + _type);
                    
            }
        }

        public List<? extends PageHierarchyNode> getChildNodes() throws WGAPIException {
           return getDesignObjects(_type);
        }
        
        @Override
        public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
            return new SkippingIteratorWrapper<PageHierarchyNode>((Iterator<PageHierarchyNode>) getChildNodes().iterator());
        }

        public String getNodeKey() throws WGAPIException {
            return WGDocument.doctypeNumberToName(_type);
        }

        public String getNodeTitle(String language) throws WGAPIException {
            
            switch (_type) {
                
                case WGDocument.TYPE_AREA: return "Areas";
                
                case WGDocument.TYPE_CONTENTTYPE: return "Content types";
                
                case WGDocument.TYPE_LANGUAGE: return "Languages";
                
                case WGDocument.TYPE_FILECONTAINER: return "File containers";
                
                case WGDocument.TYPE_TML: return "WebTML modules";
                
                case WGDocument.TYPE_CSSJS: return "Scripts";
                
                
            }
            
            return WGDocument.doctypeNumberToName(_type);
        }

        public PageHierarchyNode getParentNode() throws WGAPIException {
            return WGDatabase.this;
        }
        
    }
    
    public class AllDocumentsHierarchy {
        
        private ListOrderedMap _docCollections = new ListOrderedMap();
        
        public AllDocumentsHierarchy() {
            _docCollections.put(WGDocument.TYPE_AREA, new DocumentCollectionHierarchy(WGDocument.TYPE_AREA));
            _docCollections.put(WGDocument.TYPE_CONTENTTYPE, new DocumentCollectionHierarchy(WGDocument.TYPE_CONTENTTYPE));
            _docCollections.put(WGDocument.TYPE_LANGUAGE, new DocumentCollectionHierarchy(WGDocument.TYPE_LANGUAGE));
            _docCollections.put(WGDocument.TYPE_CSSJS, new DocumentCollectionHierarchy(WGDocument.TYPE_CSSJS));
            _docCollections.put(WGDocument.TYPE_TML, new DocumentCollectionHierarchy(WGDocument.TYPE_TML));
            _docCollections.put(WGDocument.TYPE_FILECONTAINER, new DocumentCollectionHierarchy(WGDocument.TYPE_FILECONTAINER));
        }
        
        public DocumentCollectionHierarchy getCollectionForType(int type) {
            return (DocumentCollectionHierarchy) _docCollections.get(new Integer(type));
        }

        @SuppressWarnings("unchecked")
        public List<PageHierarchyNode> getChildNodes() throws WGAPIException {
            return _docCollections.valueList();
        }

    }
    
private AllDocumentsHierarchy _allDocumentsHierarchy = new AllDocumentsHierarchy();
    /**
     * An interface defining an action that can be execute on certain database events.
     */
    public interface DatabaseAction {
        
        /**
         * The action to be ran
         * @param db The connected database.
         * @throws Exception
         */
        public void run(WGDatabase db) throws Exception;
    }
    
    
    /**
     * @Deprecated Use {@link DatabaseAction}. Just there for downward compatibility.
     */
    public interface ConnectAction extends DatabaseAction {
        
    }

    /**
     * Defines a behaviour for WGDatabases how they should respond to requests
     * for nonexistent items
     */
    public class NoItemBehaviour extends Object {

        private String behaviour;

        private Object forGetItemValue;

        private String forGetItemText;

        private List<Object> forGetItemValueList;

        private Object forTMLItem;

        private List<Object> forTMLItemList;

        private Object forTMLFormField;

        private List<Object> forTMLFormFieldList;

        private Object forTMLFormEmptyField;

        private List<Object> forTMLFormEmptyFieldList;

        /**
         * Default contructor defining the "straight" behaviour which is default
         * since WGA 4
         */
        public NoItemBehaviour() {
            straight();
        }

        /**
         * Sets the behaviour complicance to the given behaviour string. Uses
         * Constants {@link CSConfig#VERSIONCOMPLIANCE_WGA3} or
         * {@link CSConfig#VERSIONCOMPLIANCE_WGA4}
         */
        public void compliantTo(String compliance) {

            if (!compliance.equals(behaviour)) {

                if (compliance.equals(CSConfig.VERSIONCOMPLIANCE_WGA3)) {
                    compatibleForDBImplementation(getCore());
                }
                else {
                    straight();
                }
            }

        }

        public void straight() {

            behaviour = CSConfig.VERSIONCOMPLIANCE_WGA51;
            forGetItemValue = null;
            forGetItemText = null;
            forGetItemValueList = new ArrayList<Object>();
            forTMLItem = null;
            forTMLItemList = new ArrayList<Object>();
            forTMLFormField = null;
            forTMLFormFieldList = new ArrayList<Object>();
            forTMLFormEmptyField = null;
            forTMLFormEmptyFieldList = new ArrayList<Object>();
        }

        /**
         * Assures WGA 3.3 compatibility for the given database type
         * 
         * @param core
         */
        public void compatibleForDBImplementation(WGDatabaseCore core) {

            behaviour = CSConfig.VERSIONCOMPLIANCE_WGA3;

            List<Object> listWithEmptyString = new ArrayList<Object>();
            listWithEmptyString.add("");

            // Implementation independent in WGA 3.3
            forTMLItem = "";
            forTMLFormField = null;
            forTMLFormFieldList = null;
            forTMLFormEmptyField = "";
            // B00004716
            forTMLFormEmptyFieldList = new ArrayList<Object>();

            // Implementation dependent in WGA 3.3
            if (core.getTypeName().startsWith("domino/")) {
                forGetItemValue = "";
                forGetItemValueList = listWithEmptyString;
                forGetItemText = "";
                forTMLItemList = listWithEmptyString;
            }
            else if (core.getTypeName().startsWith("jdbc/")) {
                forGetItemValue = new ArrayList<Object>();
                forGetItemValueList = new ArrayList<Object>();
                forGetItemText = null;
                forTMLItemList = new ArrayList<Object>();
            }

        }

        /**
         * Behaviour for method
         * {@link de.innovationgate.webgate.api.WGDocument#getItemValue}
         */
        public Object getForGetItemValue() {
            return clone(forGetItemValue);
        }

        public void setForGetItemValue(Object forGetItemValue) {
            this.forGetItemValue = forGetItemValue;
        }

        /**
         * Behaviour for method {@link WGDocument#getItemValueList}
         */
        @SuppressWarnings("unchecked")
        public List<Object> getForGetItemValueList() {
            return (List<Object>) clone(forGetItemValueList);
        }

        private Object clone(Object obj) {

            if (obj != null && obj instanceof List) {
                return new ArrayList<Object>((List<?>) obj);
            }
            else {
                return obj;
            }

        }

        public void setForGetItemValueList(List<Object> forGetItemValueList) {
            this.forGetItemValueList = forGetItemValueList;
        }

        /**
         * Behaviour for method TMLForm.field(), when field exists but is empty
         */
        public Object getForTMLFormEmptyField() {
            return clone(forTMLFormEmptyField);
        }

        public void setForTMLFormEmptyField(Object forTMLFormEmptyField) {
            this.forTMLFormEmptyField = forTMLFormEmptyField;
        }

        /**
         * Behaviour for method TMLForm.fieldList, when field exists but is
         * empty
         */
        @SuppressWarnings("unchecked")
        public List<Object> getForTMLFormEmptyFieldList() {
            return (List<Object>) clone(forTMLFormEmptyFieldList);
        }

        public void setForTMLFormEmptyFieldList(List<Object> forTMLFormEmptyFieldList) {
            this.forTMLFormEmptyFieldList = forTMLFormEmptyFieldList;
        }

        /**
         * Behaviour for method TMLForm.field()
         */
        public Object getForTMLFormField() {
            return clone(forTMLFormField);
        }

        public void setForTMLFormField(Object forTMLFormField) {
            this.forTMLFormField = forTMLFormField;
        }

        /**
         * Behaviour for method TMLForm.fieldList()
         */
        @SuppressWarnings("unchecked")
        public List<Object> getForTMLFormFieldList() {
            return (List<Object>) clone(forTMLFormFieldList);
        }

        public void setForTMLFormFieldList(List<Object> forTMLFormFieldList) {
            this.forTMLFormFieldList = forTMLFormFieldList;
        }

        /**
         * Behaviour for method TMLContext.item()
         */
        public Object getForTMLItem() {
            return clone(forTMLItem);
        }

        public void setForTMLItem(Object forTMLItem) {
            this.forTMLItem = forTMLItem;
        }

        /**
         * Behaviour for method TMLContext.itemList()
         */
        @SuppressWarnings("unchecked")
        public List<Object> getForTMLItemList() {
            return (List<Object>) clone(forTMLItemList);
        }

        public void setForTMLItemList(List<Object> forTMLItemList) {
            this.forTMLItemList = forTMLItemList;
        }

        /**
         * Behaviour for method {@link WGDocument#getItemText}
         */
        public String getForGetItemText() {
            return (String) clone(forGetItemText);
        }

        public void setForGetItemText(String forGetItemText) {
            this.forGetItemText = forGetItemText;
        }

        public String getCurrentBehaviour() {
            return behaviour;
        }

    }

    /**
     * Cache of user authorisation details
     */
    private static String USERCACHE_USERDETAILS = "userDetails";
    
    /**
     * Cache of decisions whether the user is member of a user list or not
     */
    private static String USERCACHE_MEMBERDECISIONS = "memberDecisions";

    /**
     * Predefined creation option, determining how many cache entries the WGAPI
     * cache is allowed to hold in memory. Defaults to 2000.
     * @deprecated
     */
    public static final String COPTION_MEMORYCACHESIZE = "MemoryCacheSize";

    /**
     * Predefined creation option, controlling of the WGAPI Cache is allowed to
     * use disk overflow functionality, as far as the implementation supports
     * this. Defaults to false.
     * @deprecated
     */
    public static final String COPTION_CACHEOVERFLOW = "CacheOverflowOnDisk";

    /**
     * Predefined creation option, controlling if the WGA workflow should
     * automatically approve a document in workflow if it's publisher is also
     * the approver
     */
    public static final String COPTION_AUTOAPPROVE = "AutoApprove";
    
    /**
     * Predefined creation option, controlling if hierarchical reader fields on areas and
     * pages are in effect, defaulting to true. Setting this to false may improve
     * performance but will let those feld be ignored.
     */
    public static final String COPTION_PAGEREADERS_ENABLED = "PageReadersEnabled";
    
    
    /**
     * Controls usage of user caches, which will cache user authorisation results for some time.
     * Disable this if your user may have differing aliases, groups and roles with each request.
     */
    public static final String COPTION_USERCACHES_ENABLED = "UserCachesEnabled";
    
    /**
     * Class name of a type that implements {@link PageRightsFilter} and is to be used as this
     */
    public static final String COPTION_PAGERIGHTSFILTER = "PageRightsFilter";

    /**
     * Java system property for turning on verbose cachemanagement in applog
     */
    public static final String SYSPROPERTY_VERBOSE_CACHEMANAGEMENT = "de.innovationgate.wga.cachemanagement.verbose";

    /**
     * Java system property for turning on verbose output when a mutable object
     * needs to be cloned in WGAPI
     */
    public static final String SYSPROPERTY_VERBOSE_MUTABLECLONING = "de.innovationgate.wga.mutablecloning.verbose";

    private static final boolean VERBOSE_CACHE_MANAGEMENT = Boolean.valueOf(System.getProperty(SYSPROPERTY_VERBOSE_CACHEMANAGEMENT)).booleanValue();

    private static final boolean VERBOSE_MUTABLE_CLONING = Boolean.valueOf(System.getProperty(SYSPROPERTY_VERBOSE_MUTABLECLONING)).booleanValue();

    private static final String SYSPROPERTY_VERBOSE_DOCUMENTINSTANTIATION = "de.innovationgate.wga.documentinstantiation.verbose";

    protected static final boolean VERBOSE_DOCUMENT_INSTANTIATION = Boolean.valueOf(System.getProperty(SYSPROPERTY_VERBOSE_DOCUMENTINSTANTIATION)).booleanValue();

    
    /**
     * Java system property to enable/disable automatic operation key maintaining
     */
    public static final String SYSPROPERTY_MAINTAIN_OPERATIONKEYS = "de.innovationgate.wga.maintain-operationkeys";
    
    
    /**
     * Sysproperty to enable verbose backend access
     */
    public static final String SYSPROPERTY_VERBOSE_BACKENDACCESS = "de.innovationgate.wga.backend.verbose";
    
    
    /**
     * Sysproperty to configure the doctypes for verbose backend access, as commaseparated typename list
     */
    public static final String SYSPROPERTY_VERBOSE_BACKENDACCESS_DOCTYPES = "de.innovationgate.wga.backend.verbose.doctypes";

    private static final boolean VERBOSE_BACKEND_ACCESS = Boolean.valueOf(System.getProperty(SYSPROPERTY_VERBOSE_BACKENDACCESS)).booleanValue();
    
    private static final Set<Integer> VERBOSE_BACKEND_ACCESS_DOCTYPES = new HashSet<Integer>();
    static {
        if (System.getProperty(SYSPROPERTY_VERBOSE_BACKENDACCESS_DOCTYPES) != null) {
            for (String typeName : WGUtils.deserializeCollection(System.getProperty(SYSPROPERTY_VERBOSE_BACKENDACCESS_DOCTYPES), ",", true)) {
                int docType = WGDocument.doctypeNameToNumber(typeName);
                if (docType > 0) {
                    VERBOSE_BACKEND_ACCESS_DOCTYPES.add(docType);
                }
            }
        }
    }

    /**
     * Predefined creation option, determining how many seconds the update
     * monitoring should pause after an update has been processed. Defaults to 5
     * seconds.
     */
    public static final String COPTION_UPDATETIMEOUT = "UpdateTimeout";

    /**
     * Sets a db reference directly at creation time, so the WGAPI might use it
     * from the start
     */
    public static final String COPTION_DBREFERENCE = "DBReference";

    private int updateTimeoutSeconds = 5;

    /**
     * Represents an Entry in Query cache with all neccessary info
     */
    public class QueryCacheEntry {

        private WGCachedResultSet _resultSet;

        private String _fullQuery;

        public QueryCacheEntry(WGCachedResultSet resultSet, String fullQuery) {
            _resultSet = resultSet;
            _fullQuery = fullQuery;
        }

        /**
         * @return Returns the fullQuery.
         */
        protected String getFullQuery() {
            return _fullQuery;
        }

        /**
         * @return Returns the resultSet.
         */
        protected WGCachedResultSet getResultSet() {
            return _resultSet;
        }

    }

    /**
     * Represents the key of a query cache entry, containing query type, query
     * string and parameter
     */
    public class QueryCacheKey implements Serializable {

        private String _type;

        private String _query;

        private Map _parameters;

        public QueryCacheKey(String type, String query, Map parameters) {
            _type = type;
            _query = query;
            _parameters = new HashMap();

            // Convert parameter entries to comparable objects
            Iterator entries = parameters.entrySet().iterator();
            while (entries.hasNext()) {
                Map.Entry entry = (Map.Entry) entries.next();
                Object entryValue = entry.getValue();
                if (entryValue instanceof Comparable) {
                    _parameters.put(entry.getKey(), entryValue);
                }
                else if (entryValue instanceof WGDocument) {
                    _parameters.put(entry.getKey(), ((WGDocument) entryValue).getDocumentKey());
                }
                else {
                    _parameters.put(entry.getKey(), String.valueOf(entryValue));
                }
            }

        }

        public int hashCode() {
            final int PRIME = 31;
            int result = 1;
            result = PRIME * result + ((_parameters == null) ? 0 : _parameters.hashCode());
            result = PRIME * result + ((_query == null) ? 0 : _query.hashCode());
            result = PRIME * result + ((_type == null) ? 0 : _type.hashCode());
            return result;
        }

        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            final QueryCacheKey other = (QueryCacheKey) obj;
            if (_parameters == null) {
                if (other._parameters != null)
                    return false;
            }
            else if (!_parameters.equals(other._parameters))
                return false;
            if (_query == null) {
                if (other._query != null)
                    return false;
            }
            else if (!_query.equals(other._query))
                return false;
            if (_type == null) {
                if (other._type != null)
                    return false;
            }
            else if (!_type.equals(other._type))
                return false;
            return true;
        }

    }

    /**
     * Contains statistics about a WGA session. Session statistics are initially
     * collected in WGSessionContext objects but thrown away with these objects
     * when the session exists. The SessionStatistic object can be used to
     * persist certain intersting statistics beyond the session lifetime.
     */
    public class SessionStatistic implements Comparable {

        private boolean _maxDocsExceeded;

        private String _task;

        private Date _created;

        private int _totalDocsRetrieved;

        /**
         * Constructor that will extract statistics from a session context.
         * 
         * @param context
         *            The session context that is used for information retrieval
         */
        public SessionStatistic(WGSessionContext context) {
            _totalDocsRetrieved = context.getTotalFetchedCores();
            _created = context.getCreated();
            _task = context.getTask();
            _maxDocsExceeded = context.isMaxCoreMessageShowed();
        }

        /**
         * Returns the creation date of the session
         */
        public Date getCreated() {
            return _created;
        }

        /**
         * Returns if the "maxdocs"-Property of maximum documents per session
         * was exceeded by this session. If so, the session did drop it's first
         * retrieved backend documents while processing.
         */
        public boolean isMaxDocsExceeded() {
            return _maxDocsExceeded;
        }

        /**
         * Returns the task that the session did accomplish
         */
        public String getTask() {
            return _task;
        }

        /**
         * Returns, how many docs the session did retrieve from backend while it
         * was running
         */
        public int getTotalDocsRetrieved() {
            return _totalDocsRetrieved;
        }

        /*
         * (Kein Javadoc)
         * 
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        public int compareTo(Object o) {
            SessionStatistic otherStatistic = (SessionStatistic) o;
            return (getTotalDocsRetrieved() - otherStatistic.getTotalDocsRetrieved());
        }

    }

    /**
     * Shows, if the database monitors the last change date of the backend
     * database to maintain caches.
     */
    public boolean monitorLastChange() {
        return this.monitorLastChange;
    }

    private String title = null;

    /**
     * Accesslevel-Name for ACCESSLEVEL_NOTLOGGEDIN
     */
    public static final String ALNAME_NOTLOGGEDIN = "(not logged in)";
    
    /**
     * Accesslevel-Name for ACCESSLEVEL_READER
     */
    public static final String ALNAME_READER = "READER";

    /**
     * Accesslevel-Name for ACCESSLEVEL_NOACCESS
     */
    public static final String ALNAME_NOACCESS = "NOACCESS";

    /**
     * Accesslevel-Name for ACCESSLEVEL_MANAGER
     */
    public static final String ALNAME_MANAGER = "MANAGER";

    /**
     * Accesslevel-Name for ACCESSLEVEL_EDITOR
     */
    public static final String ALNAME_EDITOR = "EDITOR";
    
    /**
     * Accesslevel-Name for ACCESSLEVEL_CHIEF_EDITOR
     */
    public static final String ALNAME_CHIEF_EDITOR = "CHIEFEDITOR";

    /**
     * Accesslevel-Name for ACCESSLEVEL_AUTHOR
     */
    public static final String ALNAME_AUTHOR = "AUTHOR";
    
    /**
     * Map of all access levels. This also includes access levels not allowed for usage in ACL entries, like NOTLOGGEDIN, and duplicate mappings because of obsolete access levels
     */
    public static final Map<Integer,AccessLevel> ACCESSLEVELS = new HashMap<Integer, WGDatabase.AccessLevel>();
    
    
    /**
     * Map of "real" access levels, which are available for new ACLs entries
     */
    public static final Map<Integer,AccessLevel> REAL_ACCESSLEVELS = new LinkedHashMap<Integer, WGDatabase.AccessLevel>();
    static {
        AccessLevel al;

       
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_NOTLOGGEDIN, ALNAME_NOTLOGGEDIN, false,
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(false, false));
        ACCESSLEVELS.put(al.getCode(), al);
        
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_NOACCESS, ALNAME_NOACCESS, true, 
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(false, false));
        REAL_ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(al.getCode(), al);
        
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_READER, ALNAME_READER, true,                 
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(true, true));
        REAL_ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(15, al); // Transitionary mapping for old "/designer" level
        
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_AUTHOR, ALNAME_AUTHOR, true,
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(false, false),
                new AccessLevelPrivilege(true, true));
        REAL_ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(25, al); // Transitionary mapping for old "/designer" level
        
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_EDITOR, ALNAME_EDITOR, true, 
                new AccessLevelPrivilege(true, true),
                new AccessLevelPrivilege(true, true),
                new AccessLevelPrivilege(true, true));
        REAL_ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(35, al); // Transitionary mapping for old "/designer" level
        
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_CHIEF_EDITOR, ALNAME_CHIEF_EDITOR, true, 
                new AccessLevelPrivilege(true, true),
                new AccessLevelPrivilege(true, true),
                new AccessLevelPrivilege(true, true));
        REAL_ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(al.getCode(), al);
        
        al = new AccessLevel(WGDatabase.ACCESSLEVEL_MANAGER, ALNAME_MANAGER, true,
                new AccessLevelPrivilege(true, true),
                new AccessLevelPrivilege(true, true),
                new AccessLevelPrivilege(true, true));
        REAL_ACCESSLEVELS.put(al.getCode(), al);
        ACCESSLEVELS.put(al.getCode(), al);
        
    }
    
    private Map<String,WGOperationKey> operations = (new MapMaker()).weakValues().makeMap();

    /**
     * Converts access level access level names WGDatabase.ALNAME_... to codes
     * WGDatabase.ACCESSLEVEL_...
     * 
     * @param levelCode
     *            The access level code to convert
     * @return The access level name. If code is unknown returns "(unknown)"
     */
    public static String accessLevelText(int levelCode) {

        AccessLevel level = ACCESSLEVELS.get(levelCode);
        if (level != null) {
            return level.getName();
        }
        else {
            return "(unknown)";
        }

    }

    /**
     * Converts access level names WGDatabase.ALNAME_... to codes
     * WGDatabase.ACCESSLEVEL_...
     * 
     * @param levelName
     *            The access level name to convert
     * @return The code. If the name is unknown returns
     *         WGDatabase.ACCESSLEVEL_NOTLOGGEDIN
     */
    public static int accessLevelCode(String levelName) {

        for (AccessLevel level : ACCESSLEVELS.values()) {
            if (level.getName().equals(levelName)) {
                return level.getCode();
            }
        }
        
        return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
    }
    
    /**
     * Returns access level information for the given access level code
     * @param levelCode
     */
    public static AccessLevel accessLevel(int levelCode) {
        return ACCESSLEVELS.get(levelCode);
    }

    /**
     * Collects various statistics about this database.
     */
    public class WGDatabaseStatistics {

        /**
         * Returns the number of documents that currently reside in the cache
         * cache
         */
        public long getDocumentCount() {
            try {
                return masterDocumentsByKey.getSize();
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception retrieving master document cache size", e);
                return 0;
            }
        }
        
        public long getDocumentCacheMaxSize() {
            try {
                return masterDocumentsByKey.getMaxSize();
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception retrieving master document cache max size", e);
                return 0;
            }
        }
        
        public int getDocumentCacheUtilisation() {
            try {
                return masterDocumentsByKey.getUtilisation();
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception retrieving master document cache utilisation", e);
                return 0;
            }
        }

    }
    
    private int CONCURRENCY_WGOPERATION_KEY = 65536;
    private final Semaphore _wgOperationKeySemaphore = new Semaphore(CONCURRENCY_WGOPERATION_KEY, true);
    
    /**
     * Util class for finding the next free content version
     */
    public class FreeContentVersionFinder {

        private WGStructEntry _entry;

        private WGLanguage _language;

        public FreeContentVersionFinder(WGStructEntry entry, WGLanguage language) {
            _entry = entry;
            _language = language;
        }

        public int findNewVersion() throws WGAPIException {
            
            int newVersion;
            
            // Optimized backend method
            if (isBackendServiceSupported(BACKENDSERVICE_NEW_CONTENT_VERSION)) {
                newVersion = (Integer) callBackendService(BACKENDSERVICE_NEW_CONTENT_VERSION, new Object[] {_entry, _language});
            }
            
            // WGAPI method
            else {
                newVersion = 0;
                Iterator<WGDocumentCore> contentList = getCore().getAllContent(_entry, true).iterator();
                WGDocumentCore content;
                while (contentList.hasNext()) {
                    content = contentList.next();
                    String language = (String) content.getMetaData(WGContent.META_LANGUAGE);
                    int version = ((Number) content.getMetaData(WGContent.META_VERSION)).intValue();
                    if (language.equals(_language.getName()) && version > newVersion) {
                        newVersion = version;
                    }
                }
                newVersion++;
            }
            
            return newVersion;
        }

    }

    private UserHashMapGroup userHashMapGroup;

    // Special user names
    /**
     * Anonymous user name
     */
    public static final String ANONYMOUS_USER = Constants.ANONYMOUS_USER;

    /**
     * Dummy user name that is used in openSession-Method to insert a session
     * token. The token then should be entered as password.
     */
    public static final String SESSIONTOKEN_USER = "###SESSIONTOKEN###";

    /**
     * Dummy user name that is used on request based authmodules if
     * request.getRemoteUser() returns 'null'
     */
    public static final String UNKNOWN_REMOTE_USER = "###UNKNOWN_REMOTE_USER###";

    /**
     * The default media key
     */
    public static final String DEFAULT_MEDIAKEY = "html";

    /**
     * User hasn't logged in yet or login failed due to incorrect login
     * information
     */
    // Access levels
    /**
     * User nas no valid login
     */
    public static final int ACCESSLEVEL_NOTLOGGEDIN = -1;

    /**
     * User has not access to the database.
     */
    public static final int ACCESSLEVEL_NOACCESS = 0;

    /**
     * User can read documents but cannot create or edit them.
     */
    public static final int ACCESSLEVEL_READER = 10;

    /**
     * User can create content documents but has no access to content documents,
     * not created by him
     */
    public static final int ACCESSLEVEL_AUTHOR = 20;

    /**
     * User can create new content documents, and edit existing ones that he
     * didn't create.
     */
    public static final int ACCESSLEVEL_EDITOR = 30;

    /**
     * Full functional rights, restricted technical rights
     */
    public static final int ACCESSLEVEL_CHIEF_EDITOR = 40;
    
    /**
     * Full access, creation and administration rights.
     */
    public static final int ACCESSLEVEL_MANAGER = 90;

    /**
     * Enhance the query with default settings (e.g. not to retrieve documents
     * that are set invisible). Set to Boolean(true/false).
     */
    public static final String QUERYOPTION_ENHANCE = "enhance";

    /**
     * Set to Boolean(true), to reduce query results to only released content
     */
    public static final String QUERYOPTION_ONLYRELEASED = "onlyReleased";

    /**
     * Set to a specific language code to reduce query results to content of
     * this language
     */
    public static final String QUERYOPTION_ONLYLANGUAGE = "onlyLanguage";

    /**
     * When set, excludes the WGContent object given as option value from the
     * query result
     */
    public static final String QUERYOPTION_EXCLUDEDOCUMENT = "excludeDocument";

    /**
     * This is an output option set by the implementation, that carries the
     * complete query that was executed, as it was modified by the
     * implementation.
     */
    public static final String QUERYOPTION_RETURNQUERY = "returnQuery";

    /**
     * Controls the maximum results of the query. Specify as new Integer(value). A value of 0 means unlimited results.
     */
    public static final String QUERYOPTION_MAXRESULTS = "maxResults";

    /**
     * Cache the results of queries.
     */
    public static final String QUERYOPTION_CACHERESULT = "cacheResult";

    /**
     * Takes native options for the database implementation as commaseparated
     * key=value-Pairs. E.g. option1=value1,option2=value2...
     */
    public static final String QUERYOPTION_NATIVEOPTIONS = "nativeOptions";

    /**
     * Set to the role, that this query should act as, filtering contents that
     * should be invisible to that role. Default is
     * WGContent.DISPLAYTYPE_SEARCH. Compare WGContent.isHiddenFrom(). Use
     * Constants WGContent.DISPLAYTYPE_... as values.
     */
    public static final String QUERYOPTION_ROLE = "role";

    /**
     * The occurence of this queryoption in the query parameters after the
     * execution of the query shows, that the result set was retrieved from
     * query cache
     */
    public static final String QUERYOPTION_USEDCACHE = "usedCache";
    
    /**
     * Injects a priority list of languages to return in the query. Should override {@link #QUERYOPTION_ONLYLANGUAGE} if available.
     */
    public static final String QUERYOPTION_LANGUAGES = "languages";

    /**
     * This query option contains a map with query parameters, whose values
     * should be used by the underlying query mechanism to fill parameter parts
     * in the query text.
     */
    public static final String QUERYOPTION_QUERY_PARAMETERS = "queryParameters";;
    
    
    /**
     * Query type for {@link #queryUserProfileNames(String, String, Map)} that retrieves all existent user profiles
     */
    public static final String PROFILEQUERY_TYPE_ALL = "all";

    // Roles
    /**
     * Database serves content and supplementary docs like content type,
     * language if it is a full content store
     */
    public static final String ROLE_CONTENT = "content";

    /**
     * Database servers design documents (WebTML, CSS/JS)
     */
    public static final String ROLE_DESIGN = "design";

    /**
     * Database serves file containers
     */
    public static final String ROLE_REPOSITORY = "repository";

    /**
     * Database servers user profiles
     */
    public static final String ROLE_USERPROFILES = "userprofiles";

    /**
     * Content supports query languages.
     */
    public static final String FEATURE_QUERYABLE = "queryable";
    
    /**
     * The content store is able to also store user profiles and be its own "personalisation database"
     */
    public static final String FEATURE_SELF_PERSONALIZABLE = "selfPersonalizable";

    /**
     * DB has a native expression language
     */
    public static final String FEATURE_NATIVEEXPRESSIONS = "nativeExpressions";

    /**
     * Database is editable i.e. can modify data of it's backend
     */
    public static final String FEATURE_EDITABLE = "editable";

    /**
     * Content can be navigated hierarchical / is hierarchically ordered
     */
    public static final String FEATURE_HIERARCHICAL = "hierarchical";

    /**
     * Content database maintains a last changed property, that should be
     * monitored to track database changes.
     */
    public static final String FEATURE_LASTCHANGED = "lastChanged";

    /**
     * Content database can store complex values beyond string, number an dates
     * and performs type checking itself
     */
    public static final String FEATURE_COMPLEXVALUES = "complexValues";

    /**
     * Content database needs temporary storage of webtml variables in content
     * documents (e.g. to allow native expression languages to access them)
     */
    public static final String FEATURE_STORESVARS = "storesVars";

    /**
     * Content database has content types, struct entries, is multilingual and
     * maintains content versioning, so content document creation must receive
     * and generate all this info.
     */
    public static final String FEATURE_FULLCONTENTFEATURES = "fullContentFeatures";

    /**
     * Controls the behaviour when setting object relations for newly created
     * objects. If feature present, the objects themselves will be passed as
     * metadata. If not, only the name/key will be passed
     */
    public static final String FEATURE_USE_OBJECTS_AS_REFERENCES = "useObjectsAsReferences";

    /**
     * Controls, if this implementations's newly created struct entries can
     * generate their own keys
     */
    public static final String FEATURE_GENERATES_STRUCTKEYS = "generatesStructkeys";

    /**
     * Controls if this implementation's newly created struct entries can accept
     * keys from the creator
     */
    public static final String FEATURE_ACCEPTS_STRUCTKEYS = "acceptsStructkeys";
    
    /**
     * Feature that will prevent the WGAPI from limiting the fetched cores for a session
     * Should be set for DBs where limiting on WGAPI level makes no sense
     */
    public static final String FEATURE_UNLIMITED_CORES = "unlimitedCores";
    
    /**
     * Controls if this implementation is able to perform a backend login, i.e. the core itself
     * logs in with the user's login data. If this feature is activated and the database has
     * no authentication module configured, the login information is given to {@link WGDatabaseCore#openSession(AuthenticationSession, Object, boolean)}
     * as {@link BackendAuthSession}.
     */
    public static final String FEATURE_PERFORMS_BACKEND_LOGIN = "performsBackendLogin";

    /**
     * Shows if this database relies on authentication modules for external
     * authentication.
     */
    public static final String FEATURE_EXTERNAL_AUTHENTICATION = "externalAuthentication";

    /**
     * Controls if this WGAPI implementation implements the getAllContentKeys()
     * method
     */
    public static final String FEATURE_RETRIEVE_ALL_CONTENTKEYS = "retrieveAllContentKeys";

    /**
     * Shows, if this implementation supports querying user profiles by method
     * queryUserProfileNames
     */
    public static final String FEATURE_QUERY_PROFILES = "queryProfiles";

    /**
     * Automatically cascades deletions (i.e. deletes all dependent documents),
     * so that the WGAPI doesn't need to do that
     */
    public static final String FEATURE_AUTOCASCADES_DELETIONS = "autocascadesDeletions";

    /**
     * Controls if this implementation can take session tokens for a login
     */
    public static final String FEATURE_SESSIONTOKEN = "sessionToken";

    /**
     * Controls, if this implementation holds content in multiple languages
     */
    public static final String FEATURE_MULTILANGUAGE = "multilanguage";

    /**
     * Content database supports transactions
     */
    public static final String FEATURE_TRANSACTIONS = "transactions";

    /**
     * This feature instructs the WGAPI to reduce callbacks to the database on
     * some occassions where it is unlikely necessary to connect (Should
     * increase performance on implementations with slow database connections)
     */
    public static final String FEATURE_REDUCE_CALLBACKS = "reduceCallbacks";

    /**
     * ACL of this database is manageable via WGAPI
     */
    public static final String FEATURE_ACL_MANAGEABLE = "aclManageable";

    /**
     * Find updated documents since a cutoff date by core-method
     * getUpdateDocumentsSince
     */
    public static final String FEATURE_FIND_UPDATED_DOCS = "findUpdatedDocs";

    /**
     * Use the WGAPI to create a new database of this type (Method
     * WGFactory.createDatabase())
     */
    public static final String FEATURE_CREATEABLE = "createable";

    /**
     * determines that the underlying core has the feature to loadbalance
     * connections to multiple db servers, in this case connections are by
     * default readonly and each db updates have to be initialized by calling
     * beginUpdate()
     */
    public static final String FEATURE_LOADBALANCE = "loadbalance";

    /**
     * determines if documents in this database has the ability to validate file
     * attachments by calling doc.validateAttachments();
     */
    public static final String FEATURE_VALIDATE_ATTACHMENTS = "validateAttachments";

    /**
     * determines if the database supports real content relations
     */
    public static final String FEATURE_CONTENT_RELATIONS = "contentRelations";


    /**
     * determines if the database implementation has the ability to return document changes and modified dates on millisecond precision
     */
    public static final String FEATURE_MILLISECOND_PRECISION = "millisecondPrecision";
    
    /**
     * Enabling this feature will disable the lowercasing of metadata field values for fields that normally enforce this.
     */
    public static final String FEATURE_DISABLE_META_LOWERCASING = "disableMetaLowercasing";
    
    /**
     * Determines if the database supports removing and re-adding a subentity (item, relation, file attachments) without intermittent save 
     */
    public static final String FEATURE_DIRECT_ENTITY_READDING = "directEntityReadding";
    
    /**
     * Determines if a database returns ordered results on hierarchy navigation methods {@link WGDatabaseCore#getRootEntries(WGArea, WGPageOrderSet)} and {@link WGDatabaseCore#getChildEntries(WGStructEntry, WGPageOrderSet)}, allowing the fetching of partial results  
     */
    public static final String FEATURE_ORDERED_NAVIGATION_RESULTS = "orderedNavigationResults";
    
    /**
     * Determines if a database is able to return meta {@link WGUserProfile#META_PORTLETITEMSTORAGE}, a storage service for items of transient portlets
     */
    public static final String FEATURE_PROVIDE_PORTLETITEM_STORAGE = "providePortletItemStorage";
    
    /**
     * Indicates if the database knows any kind of user-specific content read protection, so that content caches must be individual for every user
     */
    public static final String FEATURE_CONTENT_READ_PROTECTION = "contentReadProtection";
    
    /**
     * Determines if the database is able to store derivates to content file attachments
     */
    public static final String FEATURE_CONTENT_FILE_DERIVATES = "contentFileDerivates";
    
    /**
     * Marks databases that should not be patched, f.e. because they are read-only
     */
    public static final String FEATURE_UNPATCHEABLE = "unpatcheable";
    
  
    /**
     * Predefined creation option to specify configuration XML.
     * @deprecated
     */
    public static final String COPTION_XML = "XML";

    /**
     * Defines if caching is globally enabled for this database, defaults to
     * true. If false, the database will cache no document data. In this state
     * the WGAPI locking functions are not effective.
     */
    public static final String COPTION_CACHING_ENABLED = "CachingEnabled";
    
    /**
     * Predefined creation option, that defines how many "cores" (i.e. backend
     * documents) a session may retrieve at once. If more are retrieved, the
     * first cores will be dropped to stay under this threshold.
     */
    public static final String COPTION_MAXCORES = "MaxDocs";

    /**
     * Predefined creation option, that determines if the LastChange-Date of
     * this database should be monitored to drop cache when it changes.
     */
    public static final String COPTION_MONITORLASTCHANGE = "MonitorLastChange";

    /**
     * Specifies the document types that a design provider for the current db
     * should provide, taking a commaseparated list of typenames of design
     * documents. This option must be enforced by the provider itself.
     */
    public static final String COPTION_DESIGNPROVIDERTYPES = "DesignProviderDoctypes";

    /**
     * Predefined creation option, that determines the class name of a workflow
     * engine to use. If this is not set, the default workflow engine of the
     * implementation is used
     */
    public static final String COPTION_WORKFLOWENGINE = "WorkflowEngine";

    /**
     * Determines the behaviour of this database and functionalitites that use
     * this database regarding the retrieval of nonexistent items. Allowed
     * values: - "straight" - The default. Always return null. - "compatible" -
     * Compatibility with WGA 3.3. Use the values used there which differ by DB
     * implementation and functionality
     */
    public static final String COPTION_NOITEMBEHAVIOUR = "NoItemBehaviour";

    /**
     * Determines the latency of user specific caches that expire after the
     * given time (in minutes). If 0 the caches never expire.
     */
    public static final String COPTION_USERCACHELATENCY = "UserCacheLatency";
    
    /**
     * Determines the latency of regular caches that expire after the given time (in minutes).
     * If 0 the caches will never expire. 
     */
    public static final String COPTION_CACHELATENCY = "CacheLatency";
    
    /**
     * Determines if this database may use a shared connection pool on servers
     * which have one available. Databases implicitly use a shared pool when
     * one is available, unless this setting is disabled. 
     */
    public static final String COPTION_SHAREDPOOL = "SharedPool";

    /**
     * Determines if users at access level "READER" are allowed to create user
     * profiles in this database
     */
    public static final String COPTION_READERPROFILECREATION = "ReaderProfileCreation";
    
    
    /**
     * Option to directly determine the content store version of this database 
     */
    public static final String COPTION_CONTENT_STORE_VERSION = "ContentStoreVersion";
    
    /**
     * Option to define users/groups/roles that should be able to read all documents no matter the contents of reader fields
     */
    public static final String COPTION_MANDATORY_READERS = "MandatoryReaders";

    /**
     * Special DB attribute that is to be set while a batch process (e.g.
     * synchronization) is currently updating the database.
     */
    public static final String ATTRIB_UPDATED = "$updated";

    /**
     * CA loaded via publisher option POPTION_CA
     */
    private X509Certificate _currentCA;

    private long _currentCALastModified;

    /**
     * CRL loaded via publisher option POPTION_CRL
     */
    private X509CRL _currentCRL;

    private long _currentCRLLastModified;

    // Core object
    private WGDatabaseCore core = null;

    // Storing of information
    private volatile boolean ready = false;

    private boolean monitorLastChange = false;

    protected boolean cachingEnabled = true;

    /**
     * The revision indicator of data in this database. Actually the state
     * of the database that is represented by the current cache state.
     */
    private WGDatabaseRevision _revision = null;
    
    /**
     * The date that the current revision corresponds to
     */
    private Date _revisionDate = null;

    /**
     * Time of the last cache maintenance. This is currently only used for {@link #getLastCacheMaintenance()}.
     * The readonly cache functionality is now driven by #lastCacheMaintenanceNanos.
     */
    private Date lastCacheMaintenance = null;
    

    
    

    private String masterLoginInputName = null;

    private String masterLoginName = null;

    private String masterLoginPassword = null;

    private String dbReference = "(none)";

    private String path = null;

    private boolean contentRole = false;

    private boolean designRole = false;

    private boolean repositoryRole = false;

    private boolean userProfilesRole = false;

    private int maxCores = 1000;

    private Map<String,Object> creationOptions = new ConcurrentHashMap<String, Object>();

    private Map<String,Object> customAttributes = new ConcurrentHashMap<String, Object>();

    private Map features = new ConcurrentHashMap();

    private SortedBag sessionStatistics = new TreeBag();

    // Session objects
    protected ThreadLocal sessionContext = new ThreadLocal();

    

    protected Cache masterDocumentsByKey;
    protected Cache masterDocumentByName;
    

    protected UserHashMap _userCache = null;

    protected Cache masterQueryCache;
    
    protected ConcurrentWeakHashMap<WGDocumentKey,WGDocument.Cache> masterDocumentCaches = new ConcurrentWeakHashMap<>();

    protected Map<Integer,WGDocumentListCache> designDocumentLists = new ConcurrentHashMap<Integer,WGDocumentListCache>();

    // Event listener registration
    private List<WGDatabaseEventListener> databaseEventListeners = new CopyOnWriteArrayList<WGDatabaseEventListener>();
    
    private List<WGDatabaseConnectListener> databaseConnectListeners = new CopyOnWriteArrayList<WGDatabaseConnectListener>();

    private List<WGDesignChangeListener> databaseDesignChangeListeners = new CopyOnWriteArrayList<WGDesignChangeListener>();

    private List<WGBackendChangeListener> backendChangeListeners = new CopyOnWriteArrayList<WGBackendChangeListener>();

    private List<WGContentEventListener> contentEventListeners = new CopyOnWriteArrayList<WGContentEventListener>();

    private List<WGDocumentEventListener> documentEventListeners = new CopyOnWriteArrayList<WGDocumentEventListener>();

    private List<WGWorkflowEventListener> workflowEventListeners = new CopyOnWriteArrayList<WGWorkflowEventListener>();
    
    private List<WGFileAnnotator> fileAnnotators = new CopyOnWriteArrayList<WGFileAnnotator>();

    private WGFileConverter fileConverter = null;
    
    private WGDesignProvider designProvider = null;
    
    private WGSchemaDefinition schemaDefinition = null;

    protected WGWorkflowEngine workflowEngine = new WGDefaultWorkflowEngine();

    // Utility objects
    protected ThreadLocal<Boolean> recursiveEventSemaphore = new ThreadLocal<Boolean>();
    
    private PageRightsFilter _pageRightsFilter = new DefaultPageRightsFilter();


    private long updateTimeout = Long.MIN_VALUE;

    private boolean allowCacheMaintenance = true;

    private Set unmodifiableDoctypes = new HashSet();

    private boolean readerProfileCreation = true;

    private boolean projectMode = false;

    /**
     * Defines, if incremental cache maintenance is allowed for this database.
     * Defaults to true (if the database type supports it). If set to false, the
     * database will completely clear all caches once the data is manipulated in
     * the background
     */
    public static final String COPTION_ALLOWCACHEMAINTENANCE = "AllowCacheMaintenance";

    /**
     * Defines if an online deletion check for documents should be enabled which
     * costs performance. This is disabled by default.
     */
    public static final String COPTION_DELETIONCHECK = "DeletionCheck";

    /**
     * Enables/Disables project mode. In project mode no versioning and
     * workflows are used.
     */
    public static final String COPTION_PROJECTMODE = "ProjectMode";
   
    /**
     * Portlet registry mode "transient", where registrations are only kept for each individual request.
     */
    public static final String PORTLETREGISTRYMODE_TRANSIENT = "transient";
    
    /**
     * Portlet registry mode "persistent" where registrations are stored on the user profile and persisted.
     */
    public static final String PORTLETREGISTRYMODE_PERSISTENT = "persistent";

    /**
     * The user name that is returned for the user of a WGA master session
     */
    public static final String MASTER_USERNAME = "Master Session";

    /**
     * Number of documents in a cache list that may need to be retrieved to rebuild the cache. If more docs need to be retrieved the cache will not be used.
     */
    public static final String COPTION_LIST_CACHE_REBUILD_THRESHOLD = "ListCacheRebuildThreshold";

    // lockMangager for locks of objects in this db
    private LockManager _lockManager = new LockManager();
    
    private AuthenticationModule _authenticationModule = null;

    /**
     * Injected default language by outer code
     */
    private volatile String _defaultLanguage = null;

    private volatile boolean connected = false;

    private String type;

    private String typeName;

    private int userCacheLatency = 30;

    private Collator _defaultLanguageCollator = null;

    private NoItemBehaviour noItemBehaviour;

    private boolean deletionCheck = false;

    private List<DatabaseAction> _connectActions = new ArrayList<DatabaseAction>();
    private List<DatabaseAction> _openActions = new ArrayList<DatabaseAction>();

    private boolean autoApprove = true;
    private boolean pageReadersEnabled = true;

    private boolean userCachesEnabled = true;
    
    private int listCacheRebuildThreshold = 10;

    private WGDatabaseServer server;

    private boolean _defaultLanguageAutoDetermined = false;

    private List<String> _mandatoryReaders = Collections.emptyList();

    private boolean _maintainOperationKeys;

    private Version _complianceVersion = CSConfig.getComplianceVersion(VersionCompliance.VERSIONCOMPLIANCE_DEFAULT);
    private Version _overlayComplianceVersion = CSConfig.getComplianceVersion(VersionCompliance.VERSIONCOMPLIANCE_DEFAULT);
    
    private String _uuid = null;
    private double _csVersion;
    private int _csPatchLevel;
    private int _cacheLatency;
    private Object _cacheMaintenanceMonitor = new Object();

    /**
     * Option to disable certificate authentication even when the auth module to use has certauth enabled
     */
    public static final String COPTION_DISABLE_CERTAUTH = "DisableCertAuth";
    
    /**
     * Option to fake certificate authentication. Database will enable certauth, no matter the auth module, and accept all X509 certificates verification
     */
    public static final String COPTION_FAKE_CERTAUTH = "FakeCertAuth";

    /**
     * A query option used on non-iterating query types, determining the size of a results fetch page to retrieve from the backend
     */
    public static final String QUERYOPTION_FETCHSIZE = "fetchsize";

    /**
     * Option to configure the maximum number of document objects to be kept in document cache
     */
    public static final String COPTION_DOCUMENTCACHESIZE = "DocumentCacheSize";
    
    /**
     * Option to configure the maximum number of document names to be kept in name cache
     */
    public static final String COPTION_NAMECACHESIZE = "NameCacheSize";

    /**
     * Option to configure the maximum number of query results to be kept in query cache
     */
    public static final String COPTION_QUERYCACHESIZE = "QueryCacheSize";
    
    /**
     * Option to configure if this database is clustered if used in an OpenWGA server cluster
     */
    public static final String COPTION_CLUSTERED = "Clustered";
    
    /**
     * Option to enable legacy DBCP 1 JMX monitoring, provided by WGA-own beans (actually only for databases using DBCP, ergo JDBC databases)
     */
    public static final String COPTION_LEGACY_DBCP_MONITORING = "JDBC.LegacyDBCPMonitoring";
    
    /**
     * Service to clear all user profiles of the personalisation database in one fast batch operation.
     * As this operation invalidates all existing profiles attached to sessions it should not be executed on productive environments.
     * This operation will leave no trace in historylog.
     */
    public static final String BACKENDSERVICE_CLEAR_USERPROFILES = "clearUserProfiles";
    
    /**
     * Service to effectively determine the version number for a new content document
     */
    public static final String BACKENDSERVICE_NEW_CONTENT_VERSION = "newContentVersion";
    
    /**
     * Service to clear all areas, struct entries and content documents from the content store in one fast batch operation.
     * This operation will leave no trace in historylog.
     * Schema from design may not be enforced after this service. Enforce manually or reopen the database to enforce again.
     */
    public static final String BACKENDSERVICE_CLEAR_CONTENT = "clearContent";
    
    /**
     * Service to clear all content (areas, structs and contents) and schema (contenttypes and language definitions) from the content store in one fast batch operation.
     * This operation will leave no trace in historylog.
     * Schema from design may not be enforced after this service. Enforce manually or reopen the database to enforce again.
     */
    public static final String BACKENDSERVICE_CLEAR_CONTENT_AND_SCHEMA = "clearContentAndSchema";
    
    /**
     * Completely clears all data from the database, including all documents, the ACL, database metadata, historylog. Also resets system and custom sequences.
     * This operation will leave no trace in historylog (well, besides clearing it)
     * Schema from design may not be enforced after this service. Enforce manually or reopen the database to enforce again.
     */
    public static final String BACKENDSERVICE_CLEAR_DATABASE = "clearDatabase";
    
    /**
     * A backend service defining some daily maintenance operation, to execute on the first event thread run each day
     */
    public static final String BACKENDSERVICE_DAILY_MAINTENANCE = "dailyMaintenance";
    
    /**
     * Selects all documents that are pending release. Returns a {@link WGAbstractResultSet}.
     */
    public static final String BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS = "selectPendingReleaseDocuments";
    
    /**
     * Checks if a content document for a certain page exists in a given language and status
     */
    public static final String BACKENDSERVICE_PROBE_CONTENT = "probeContent";
    
    /**
     * Fetches multiple contents in one backend call by their content key
     */
    public static final String BACKENDSERVICE_FETCH_MULTI_CONTENT = "fetchMultiContent";
    
    /**
     * Backend services that are allowed to be called under user session
     */
    public static final Set<String> USER_BACKENDSERVICES = new HashSet<String>(Arrays.asList(new String[] {
            BACKENDSERVICE_PROBE_CONTENT,
            BACKENDSERVICE_NEW_CONTENT_VERSION
    }));
    

    private static final String USERCACHE_FILTEREDACCESS_PREFIX = "FilteredUserAccess:";
    private static final int CACHELATENCY_DEFAULT_NOT_UPDATEABLE_DBS = 1;

    /**
     * Protected constructor. Construction from outside only via WGFactory
     * object.
     * 
     * @param server
     *            The database server that this database resides on
     * @param strType
     *            Type of database implementation (i.e. full qualified name of
     *            the database implementation class)
     * @param strPath
     *            Path to database. Interpreted by the implementation and
     *            therefor varying by implementation
     * @param strUserName
     *            Login user name
     * @param strUserPwd
     *            Password of user
     * @param options
     *            Map of implementation specific options.
     * @throws WGAPIException
     */
    protected WGDatabase(WGDatabaseServer server, String strType, String strPath, String strUserName, String strUserPwd, Map<String,String> options, boolean prepareOnly) throws WGAPIException {

        // Initialize basic members
        this.server = server;
        this.type = strType;
        this.path = strPath;
        this.masterLoginInputName = strUserName;
        this.masterLoginName = strUserName;
        this.masterLoginPassword = strUserPwd;

        this.userHashMapGroup = new UserHashMapGroup();
        this._userCache = userHashMapGroup.newUserHashMap("dbCacheUserCache");

        // Setting creation options
        if (options != null) {
            WGUtils.putAllNonNullValues(options, this.creationOptions);
        }

        if (this.creationOptions.containsKey(COPTION_DBREFERENCE)) {
            setDbReference(String.valueOf(this.creationOptions.get(COPTION_DBREFERENCE)));
        }
        else {
            setDbReference(getPath());
        }
        
        // Try to get implementation class and open database
        connectDBCore(prepareOnly);
        Map<String,Object> genericCacheParams = new HashMap<String,Object>();
        genericCacheParams.put(Cache.PARAM_TIME_TO_LIVE_SECONDS, _cacheLatency * 60);
        
        // Create master document cache
        Map<String,Object> params = new HashMap<String,Object>(genericCacheParams);
        params.put(Cache.PARAM_DISPOSAL_LISTENER, new DocumentCacheDisposalListener());
        int docCacheCapacity = DOCUMENTCACHE_SIZE_DEFAULT;
        if (options.containsKey(WGDatabase.COPTION_DOCUMENTCACHESIZE)) {
            docCacheCapacity = Integer.parseInt((String) options.get(WGDatabase.COPTION_DOCUMENTCACHESIZE));
        }
        try {
            this.masterDocumentsByKey = CacheFactory.createCache("WGAPIDocumentByKeyCache_" + getDbReference(), docCacheCapacity, params);
        }
        catch (CacheException e) {
            throw new WGSystemException("Exception creating master document cache", e);
        }
        
        // Create master name cache
        params = new HashMap<String,Object>(genericCacheParams);
        int nameCacheCapacity = NAMECACHE_SIZE_DEFAULT;
        if (options.containsKey(WGDatabase.COPTION_NAMECACHESIZE)) {
            nameCacheCapacity = Integer.parseInt((String) options.get(WGDatabase.COPTION_NAMECACHESIZE));
        }
        try {
            this.masterDocumentByName = CacheFactory.createCache("WGAPIContentByNameCache_" + getDbReference(), nameCacheCapacity, params);
        }
        catch (CacheException e) {
            throw new WGSystemException("Exception creating master name cache", e);
        }

        // Create master query cache
        params = new HashMap<String,Object>(genericCacheParams);
        int queryCacheCapacity = QUERYCACHE_SIZE_DEFAULT;
        if (options.containsKey(WGDatabase.COPTION_QUERYCACHESIZE)) {
            queryCacheCapacity = Integer.parseInt((String) options.get(WGDatabase.COPTION_QUERYCACHESIZE));
        }
        try {
            this.masterQueryCache = CacheFactory.createCache("WGAPIQueryCache_" + getDbReference(), queryCacheCapacity, params);
        }
        catch (CacheException e) {
            throw new WGSystemException("Exception creating master query cache", e);
        }

        this.ready = true;
        
        // Execute connect actions that may have been registered while the db got connected 
        if (isConnected()) {
            notifyDatabaseActions(_connectActions);
        }
        
        _maintainOperationKeys = Boolean.parseBoolean(System.getProperty(SYSPROPERTY_MAINTAIN_OPERATIONKEYS, "false"));

    }

    private void createDBCoreObject(String strType) throws WGInvalidDatabaseException {
        Class dbClass;
        try {
            dbClass = Class.forName(strType, true, WGFactory.getImplementationLoader());

            if (!WGDatabaseCore.class.isAssignableFrom(dbClass)) {
                throw new WGInvalidDatabaseException("Cannot allocate db type " + strType + ": is no implementation of " + WGDatabaseCore.class.getName());
            }
            this.core = (WGDatabaseCore) dbClass.newInstance();
        }
        catch (ClassNotFoundException exc) {
            throw new WGInvalidDatabaseException("Database implementation class or dependent class not found. Check classpath: " + exc.getMessage());
        }
        catch (NoClassDefFoundError err) {
            throw new WGInvalidDatabaseException("Database implementation class or dependent class not found. Check classpath: " + err.getMessage());
        }
        catch (IllegalAccessException exc) {
            throw new WGInvalidDatabaseException("Database implementation class or dependent class not accessible: " + exc.getMessage());
        }
        catch (InstantiationException exc) {
            throw new WGInvalidDatabaseException("Could not instantiate implementation class: " + exc.getMessage());
        }
    }

    /**
     * Creates the connection to the database backend, instantiates and opens
     * the db core
     * 
     * @param prepareOnly
     *            Specify false to only prepare the database for connection, but
     *            leave it unconnected after this method, standing by for the
     *            first session to be opened.
     * @return true, if the method connected the core, false if not (when the
     *         core already was connected)
     * @throws WGAPIException
     */
    /**
     * @param prepareOnly
     * @return true if the db has actually been connected, false if it has just been prepared
     * @throws WGAPIException
     */
    private synchronized boolean connectDBCore(boolean prepareOnly) throws WGAPIException {
        WGUserAccess userAccess;

        // Prevent running when already connected
        if (connected) {
            return false;
        }

        _connectActions.clear();
        _openActions.clear();
        
        // / Create core object
        createDBCoreObject(this.type);
        
        // Inject reconfiguration accessor
        if (this.core instanceof WGReconfigurableDatabaseCore) {
            ((WGReconfigurableDatabaseCore) this.core).injectReconfigurationAccessor(new ReconfigurationAccessor());
        }

        // Open database
        userAccess = this.core.open(this, path, masterLoginName, masterLoginPassword, prepareOnly);
        if (userAccess.getAccessLevel() <= WGDatabase.ACCESSLEVEL_NOACCESS) {
            throw new WGInvalidDatabaseException("Master login " + masterLoginName + " either has wrong password or no access to database");
        }

        reconfigureCore(prepareOnly, userAccess);
        
        return true;
    }




    private void reconfigureCore(boolean prepareOnly, WGUserAccess userAccess) throws WGAPIException {
        // Initialize data from core
        if (this.title == null) {
            this.title = this.core.getTitle();
        }

        this.typeName = this.core.getTypeName();

        // Initialize CS roles
        List<?> roles = this.core.getRoles();
        if (roles.contains(WGDatabase.ROLE_CONTENT)) {
            this.contentRole = true;
        }
        if (roles.contains(WGDatabase.ROLE_DESIGN)) {
            this.designRole = true;
        }
        if (roles.contains(WGDatabase.ROLE_REPOSITORY)) {
            this.repositoryRole = true;
        }
        if (roles.contains(WGDatabase.ROLE_USERPROFILES)) {
            this.userProfilesRole = true;
        }

        // Initialize max cores setting
        if (creationOptions.containsKey(COPTION_MAXCORES)) {
            try {
                maxCores = Integer.parseInt((String) creationOptions.get(COPTION_MAXCORES));
            }
            catch (NumberFormatException e) {
                WGFactory.getLogger().error("Cannot interpret creation option " + COPTION_MAXCORES + " as an integer. Defaulting to " + maxCores);
            }
            if (maxCores == 0) {
                maxCores = Integer.MAX_VALUE;
            }
        }

        // Initialize background monitoring for database changes
        if (!prepareOnly) {
            this.monitorLastChange = this.core.hasFeature(FEATURE_LASTCHANGED);

            if (creationOptions.containsKey(COPTION_MONITORLASTCHANGE)) {
                monitorLastChange = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_MONITORLASTCHANGE))).booleanValue();
            }
        }

        // Initialize caching enablement
        if (creationOptions.containsKey(COPTION_CACHING_ENABLED)) {
            cachingEnabled = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_CACHING_ENABLED))).booleanValue();
        }

        // Initialize cache maintenance
        if (creationOptions.containsKey(COPTION_ALLOWCACHEMAINTENANCE)) {
            allowCacheMaintenance = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_ALLOWCACHEMAINTENANCE))).booleanValue();
        }

        // Online deletion check for documents
        if (creationOptions.containsKey(COPTION_DELETIONCHECK)) {
            deletionCheck = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_DELETIONCHECK))).booleanValue();
        }

        // Project mode
        if (creationOptions.containsKey(COPTION_PROJECTMODE)) {
            projectMode = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_PROJECTMODE))).booleanValue();
            WGFactory.getLogger().info("Enabled project mode for database " + getDbReference());
        }

        // Initialize reader profile creation
        if (creationOptions.containsKey(COPTION_READERPROFILECREATION)) {
            readerProfileCreation = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_READERPROFILECREATION))).booleanValue();
        }
        
        // Initialize page rights filter
        if (creationOptions.containsKey(COPTION_PAGERIGHTSFILTER)) {
            String filterName = (String) creationOptions.get(COPTION_PAGERIGHTSFILTER);
            try {
                @SuppressWarnings("unchecked")
                Class<? extends PageRightsFilter> filterClass = (Class<? extends PageRightsFilter>) WGFactory.getImplementationLoader().loadClass(filterName);
                PageRightsFilter filter = filterClass.newInstance();
                _pageRightsFilter=filter;
            }
            catch (Exception e) {
                throw new WGIllegalArgumentException("Exception instantiating page rights filter: " + filterName, e);
            }
        }
        onConnect(new DatabaseAction() {
            @Override
            public void run(WGDatabase db) throws Exception {
                getPageRightsFilter().init(db);
            }
            
        });
        
        if (creationOptions.containsKey(COPTION_AUTOAPPROVE)) {
            autoApprove = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_AUTOAPPROVE))).booleanValue();
        }
        
        pageReadersEnabled = this.core.hasFeature(FEATURE_FULLCONTENTFEATURES) && this.core.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5;
        if (creationOptions.containsKey(COPTION_PAGEREADERS_ENABLED)) {
            pageReadersEnabled = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_PAGEREADERS_ENABLED))).booleanValue();
        }
        if (creationOptions.containsKey(COPTION_USERCACHES_ENABLED)) {
            userCachesEnabled  = Boolean.valueOf(String.valueOf(creationOptions.get(COPTION_USERCACHES_ENABLED))).booleanValue();
        }

        // Initialize update timeout for background changes
        if (creationOptions.containsKey(COPTION_UPDATETIMEOUT)) {
            try {
                updateTimeoutSeconds = Integer.parseInt((String) creationOptions.get(COPTION_UPDATETIMEOUT));
                WGFactory.getLogger().info("Update timeout is set to " + updateTimeoutSeconds + " seconds.");
            }
            catch (NumberFormatException e) {
                WGFactory.getLogger().error("Cannot parse db option " + COPTION_UPDATETIMEOUT + " value " + creationOptions.get(COPTION_UPDATETIMEOUT));
            }
        }

        // Initialize user cache latency
        if (creationOptions.containsKey(COPTION_USERCACHELATENCY)) {
            String latencyStr = (String) creationOptions.get(COPTION_USERCACHELATENCY);
            try {
                userCacheLatency = Integer.parseInt(latencyStr);
            }
            catch (NumberFormatException e) {
                WGFactory.getLogger().error("Cannot parse db option " + COPTION_USERCACHELATENCY + " value " + creationOptions.get(COPTION_USERCACHELATENCY));
            }
        }
        
        // Initialize other caches latency
        _cacheLatency = 0;
        if (!hasFeature(FEATURE_FIND_UPDATED_DOCS)) {
            _cacheLatency = CACHELATENCY_DEFAULT_NOT_UPDATEABLE_DBS;
        }
        if (creationOptions.containsKey(COPTION_CACHELATENCY)) {
            String latencyStr = (String) creationOptions.get(COPTION_CACHELATENCY);
            try {
                _cacheLatency = Integer.parseInt(latencyStr);
            }
            catch (NumberFormatException e) {
                WGFactory.getLogger().error("Cannot parse db option " + COPTION_CACHELATENCY + " value " + creationOptions.get(COPTION_CACHELATENCY));
            }
        }
        
        // Setup user hashmaps
        if (!hasFeature(FEATURE_CONTENT_READ_PROTECTION)) {
            this.userHashMapGroup.setSingleUserMode(true);
        }
        else if (userCacheLatency != 0) {
            _userCache.setMapLatency(userCacheLatency * 1000 * 60);
        }

        // Initialize list cache rebuild threshold
        if (creationOptions.containsKey(COPTION_LIST_CACHE_REBUILD_THRESHOLD)) {
            String thresholdStr = (String) creationOptions.get(COPTION_LIST_CACHE_REBUILD_THRESHOLD);
            try {
                listCacheRebuildThreshold = Integer.parseInt(thresholdStr);
                WGFactory.getLogger().info("User list cache rebuild threshold is set to " + listCacheRebuildThreshold);
            }
            catch (NumberFormatException e) {
                WGFactory.getLogger().error("Cannot parse db option " + COPTION_LIST_CACHE_REBUILD_THRESHOLD + " value " + thresholdStr);
            }
        }
        
        // Init mandatory readers
        if (creationOptions.containsKey(COPTION_MANDATORY_READERS)) {
            try {
                _mandatoryReaders  = (List<String>) JSONListOptionType.INSTANCE.unconvert((String) creationOptions.get(COPTION_MANDATORY_READERS));
            }
            catch (OptionConversionException e) {
                WGFactory.getLogger().error("Cannot parse db option " + COPTION_MANDATORY_READERS + " value " + creationOptions.get(COPTION_MANDATORY_READERS));
            }
        }
        
        // Init content store version
        _csVersion = this.core.getContentStoreVersion();
        _csPatchLevel = this.core.getContentStorePatchLevel();

        // Init last modified date
        updateRevision(WGDatabaseRevision.forValue(this.core.getRevision()));

        // Init value of nonexistent items
        noItemBehaviour = new NoItemBehaviour();
        if (creationOptions.containsKey(COPTION_NOITEMBEHAVIOUR)) {
            String noItemValueProp = (String) creationOptions.get(COPTION_NOITEMBEHAVIOUR);
            if (noItemValueProp.equalsIgnoreCase("compatible")) {
                noItemBehaviour.compatibleForDBImplementation(core);
            }
        }
        
     // If the db should only get prepared, close the core again
        if (prepareOnly) {
            core.close();
            // core = null; Not safe. Many processes rely on an existing core
            connected = false;
        }

        // Else open a master session
        else {
            // Open a session context
            WGSessionContext sessionContext = new WGSessionContext(this, MasterLoginAuthSession.getInstance(), null, userAccess, null);
            this.setSessionContext(sessionContext);
            
            // Initialize default language collator
            try {
                _defaultLanguageCollator = Collator.getInstance(WGLanguage.languageNameToLocale(getDefaultLanguage()));
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error determining default language collator. Using default platform collator", e);
                _defaultLanguageCollator = Collator.getInstance();
            }
            
            // Initialize custom workflow engine
            initWorkflowEngine();

            // Set database UUID if not yet present
            if (getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && !getTypeName().equals(WGFakeDatabase.DBTYPE)) {
                _uuid  = (String) getExtensionData(EXTDATA_DATABASE_UUID);
                if (_uuid == null) {
                    _uuid = UUID.randomUUID().toString();
                    writeExtensionData(EXTDATA_DATABASE_UUID, _uuid);
                }
            }
            connected = true;
        }
        
    }

    private void initWorkflowEngine() throws WGAPIException {
        
        Class engineClass = null;
        
        // Custom workflow engine configured by creation option
        if (getCreationOptions().containsKey(COPTION_WORKFLOWENGINE)) {
            String engineClassName = (String) this.getCreationOptions().get(COPTION_WORKFLOWENGINE);
            try {
                engineClass = WGFactory.getImplementationLoader().loadClass(engineClassName);
            }
            catch (ClassNotFoundException e) {
                throw new WGConfigurationException("Cannot load Workflow engine class " + engineClassName, e);
            }
        }
        
        // Dedicated workflow engine for the current db implementation 
        else if (getCore().getDedicatedWorkflowEngine() != null) {
            engineClass = getCore().getDedicatedWorkflowEngine();
        }
        
        // Default workflow engine
        else {
            engineClass = WGFactory.getDefaultWorkflowEngine();
        }
        
        if (!WGWorkflowEngine.class.isAssignableFrom(engineClass)) {
            throw new WGConfigurationException("Workflow engine class " + engineClass.getName() + " does not implement " + WGWorkflowEngine.class.getName());
        }
        
        try {
            WGWorkflowEngine engine;
            if (WGFactory.getModuleRegistry() != null) {
                engine = (WGWorkflowEngine) WGFactory.getModuleRegistry().instantiate(engineClass);
            }
            else {
                engine = (WGWorkflowEngine) engineClass.newInstance();
            }
            engine.init(this);
            workflowEngine = engine;
        }
        catch (WGAPIException e) {
            throw e;
        }
        catch (Throwable e) {
            throw new WGBackendException("Exception initializing workflow engine", e);
        }
        
        
    }

    /**
     * Triggers checking for database updates.
     * @param force Whether to force the check. Specifying false will respect the update timeout configured.
     * @return true if updates were detected and processed
     * @throws WGAPIException
     */
    protected boolean checkDatabaseUpdates(boolean force) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (this.monitorLastChange == false) {
            return false;
        }

        // No updates while in transaction
        if (getSessionContext().isTransactionActive()) {
            return false;
        }

        // Update timeout?
        if (!force && updateTimeout != 0 && updateTimeout >= System.currentTimeMillis()) {
            return false;
        }
        updateTimeout = 0;

        // This method is already called higher in the call stack - so do
        // nothing
        if (this.recursiveEventSemaphore.get() != null) {
            return false;
        }
        this.recursiveEventSemaphore.set(new Boolean(true));
        try {
            boolean processed = false;
            
            // Outer test, if updating is needed. Faster, bc. there is no
            // synchronisation
            if (isDatabaseUpdatedInBackground()) {

                synchronized (_cacheMaintenanceMonitor) {
                
                    // When in synchronized block, test again, bc. another
                    // thread may already have performed update while
                    // this task was waiting for the sync lock to be released
                    WGDatabaseRevision newLastChanged = WGDatabaseRevision.forValue(getCore().getRevision());
                    if (isDatabaseUpdatedInBackground()) {
    
                        verboseCacheManagement("Cache management starting. Database date changed from " + _revision + " to " + newLastChanged);
    
                        // No other task performing update at the moment - so
                        // refresh right here right now
                        if (hasFeature(FEATURE_FIND_UPDATED_DOCS) && allowCacheMaintenance == true) {
                            List<WGUpdateLog> changeLogs = getCore().getUpdateLogs(_revision.getRevisionValue());
                            processChanges(changeLogs);
                        }
                        else {
                            refresh();
                        }
    
                        // Update some flags, clear some (not maintainable)
                        // caches and fire event
                        
                        this.getSessionContext().setDatabaseUpdated(true);
                        
                        // No more neccessary? Is done in processChanges() or refresh()
                        // this.fireDatabaseEvent(new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_UPDATE, null));
                        
    
                        this.getCore().refresh();
                        updateTimeout = System.currentTimeMillis() + (1000 * updateTimeoutSeconds);
                        verboseCacheManagement("Cache management ended");
                        processed = true;
    
                    }
                    updateRevision(newLastChanged);
                
                }
                
            }

            return processed;

        }
        finally {
            this.recursiveEventSemaphore.remove();
        }
    }

    /**
     * Returns if the database has been updated in background and caches may not
     * reflect the most up-to-date state, because cache management has not yet
     * processed these updates
     */
    public boolean isDatabaseUpdatedInBackground() throws WGAPIException {
        Comparable revisionValue = getCore().getRevision();
        if (revisionValue == null) {
            return false;
        }
        
        WGDatabaseRevision newRevision = WGDatabaseRevision.forValue(revisionValue);
        WGDatabaseRevision cacheRevision = _revision;
        if (newRevision.isProbablyNewerThan(cacheRevision)) {
            if (newRevision.isUniqueValue()) {
                return true;
            }
            else if (newRevision.equals(cacheRevision)) {
                if (cacheRevision.isLaterEqualRevisionValuePossible()) {
                    return true;
                }
                else {
                    return false;
                }
            }
            else {
                return true;
            }
        }
        else {
            return false;
        }
    }

    /**
     * Processes a list of updates, that occured in the background. Caches will
     * be maintained and events will be fired.
     * 
     * @param updatedDocuments
     *            List of WGUpdateLog objects, representing database updates
     * @throws WGAPIException
     */
    private void processChanges(List<WGUpdateLog> updatedDocuments) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        verboseCacheManagement("Performing incremental cache update");

        if (getSessionContext().isTransactionActive() || !hasFeature(FEATURE_FIND_UPDATED_DOCS)) {
            return;
        }

        if (updatedDocuments == null) {
            WGFactory.getLogger().error("No updated documents provided for change processing");
            return;
        }

        // Iterate through update events
        Iterator<WGUpdateLog> changes = updatedDocuments.iterator();
        while (changes.hasNext()) {
            Thread.yield();
            WGUpdateLog log = changes.next();
            verboseCacheManagement("Processing change log " + log.getRevision().getRevisionValue().toString() + ", Type " + log.getType() + ", Target " + log.getDocumentKey() + ", Transactional: " + getSessionContext().isTransactionActive());
            
            // Call backend change listeners
            for (WGBackendChangeListener listener : this.backendChangeListeners) {
                listener.processChange(this, log);
            }

            // Filter out rows that do not refer to document updates (like ACL
            // entries)
            if (log.getDocumentKey().startsWith("$")) {
                verboseCacheManagement("Non-document change log. Skipped");
                continue;
            }

            WGDocumentKey docKey = new WGDocumentKey(log.getDocumentKey());

            // Perform all neccessary operation for the given type of update
            if (log.getType() == WGUpdateLog.TYPE_UPDATE) {
                try {
                    clearNonexistentDocIndicatorForDocument(log.getDocumentKey());
                    WGDocument doc = getDocumentByKey(log.getDocumentKey());
                    if (doc!= null) {
                        WGDatabaseRevision cacheRevision = doc.getCacheRevision();
                        if (cacheRevision != null && log.getRevision() != null && cacheRevision.isUniqueValue() && cacheRevision.compareTo(log.getRevision()) >= 0)  {
                            verboseCacheManagement("Document already on cache revision. Skipped");
                            continue;
                        }
                    }
                    
                    verboseCacheManagement("Performing creation/update of doc " + log.getDocumentKey());
                    documentSaved(doc, docKey, true, false, log.getRevision());
    
                    // If metadata module was updated in background we drop the complete cache
                    // as it might symbolize the end of a batch update process (F00004402)
                    if (log.getDocumentKey().startsWith(WGCSSJSModule.METADATA_MODULE_QUALIFIER)) {
                        verboseCacheManagement("Update of metadata module " + log.getDocumentKey() + ". Complete cache refresh");
                        refresh();
                    }
                }
                catch (WGDeletedException e) {
                    // Silently ignore here. Most likely this update log refers a backend entity that has already been deleted in the meantime.
                    // Continue processing to pickup deletion log (#00002163)
                }

            }
            else if (log.getType() == WGUpdateLog.TYPE_DELETE) {
                try {
                    WGDocument doc = getDocumentByDocumentKeyFromCache(docKey, false);
                    if (doc != null) {
                        WGDatabaseRevision cacheRevision = doc.getCacheRevision();
                        if (cacheRevision != null && log.getRevision() != null && cacheRevision.isUniqueValue() && cacheRevision.compareTo(log.getRevision()) >= 0)  {
                            verboseCacheManagement("Document already on cache revision. Skipped");
                            continue;
                        }
                    }
                    
                    verboseCacheManagement("Performing deletion of doc " + log.getDocumentKey());
                    documentRemoved(doc, docKey, false, (docKey.getDocType() == WGDocument.TYPE_CONTENT), log.getRevision());
                }
                catch (WGDocumentDoesNotExistException e) {
                    // NOOP: Was deleted and already marked as nonexistent
                }
            }
            else if (log.getType() == WGUpdateLog.TYPE_STRUCTMOVE) {
                try {
                    WGStructEntry struct = (WGStructEntry) getDocumentByKey(log.getDocumentKey());
                    if (struct != null) {
                        verboseCacheManagement("Performing struct movage of doc " + log.getDocumentKey());
                        processMovedDocuments(struct, false);
                    }
                }
                catch (WGDeletedException e) {
                    // Silently ignore here. Most likely this update log refers a backend entity that has already been deleted in the meantime.
                    // Continue processing to pickup deletion log (#00002163)
                }
            }
            else {
                WGFactory.getLogger().warn("Unknown log type: " + log.getType());
            }
            
            getSessionContext().clearCache();
        }

        // Throw global update event so event listeners may update
        this.fireDatabaseEvent(new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_UPDATE));

    }

    /**
     * Removes a nodoc placeholder for the given document to be able to load
     * this document when it just has been created
     * 
     * @param documentKey
     *            The document key of the document
     */
    private void clearNonexistentDocIndicatorForDocument(String documentKey) {
        WGDocumentKey key = new WGDocumentKey(documentKey);
        clearNonexistentDocIndicator(masterDocumentsByKey, String.valueOf(key));
    }

    private void clearNonexistentDocIndicator(Cache cache, String cacheKey) {
        try {
            Object entry = cache.read(cacheKey);
            if (entry instanceof NullPlaceHolder) {
                cache.flush(cacheKey);
            }
        }
        catch (CacheException e) {
            WGFactory.getLogger().error("Exception clearing NDI master document cache", e);
        }
    }

    private void verboseCacheManagement(String msg) {
        if (VERBOSE_CACHE_MANAGEMENT) {
            WGFactory.getLogger().info("Cache Management DB:" + getDbReference() + " - " + msg);
        }
    }

    protected void verboseMutableCloning(Object obj) {
        if (VERBOSE_MUTABLE_CLONING) {
            WGFactory.getLogger().info("Mutable cloning DB:" + getDbReference() + " - Object of type " + obj.getClass().getName());
        }
    }

    protected void verboseBackendAccess(int operation, Object key) {
        if (VERBOSE_BACKEND_ACCESS) {
            
            int docType = determineBackendAccessDocType(operation, key);
            if (docType == 0) { // Means: Is no backend access
                return;
            }
            
            if (docType == -1 || VERBOSE_BACKEND_ACCESS_DOCTYPES.size() == 0 || VERBOSE_BACKEND_ACCESS_DOCTYPES.contains(docType)) {
                WGFactory.getLogger().info(
                        "Backend Access DB:" + getDbReference() + " - Operation: " + WGOperationKey.getOperationName(operation) + " - Key: " + WGUtils.strReplace(String.valueOf(key), "\n", "", true) + " - Cache: "
                                + (!getSessionContext().isCachingEnabled() ? "disabled" : !getSessionContext().isCacheWritingEnabled() ? "readonly" : "enabled") + " - Session: "
                                + getSessionContext().hashCode() + " - Task: " + getSessionContext().getTask());
            }
        }
    }

    private int determineBackendAccessDocType(int operation, Object key) {

        switch (operation) {
            
            case WGOperationKey.OP_CONTENT_BY_NAME:
            case WGOperationKey.OP_QUERY:
            case WGOperationKey.OP_QUERY_RESULT_COUNT:
            case WGOperationKey.OP_QUERY_HAS_RESULTS:
            case WGOperationKey.OP_STRUCT_CONTENTS:
            case WGOperationKey.OP_STRUCT_CONTENTS_INCLUDING_ARCHIVED:
                return WGDocument.TYPE_CONTENT;
                
            case WGOperationKey.OP_DESIGN_LIST:
                if (key instanceof Number) {
                    return ((Number) key).intValue();
                }
                else {
                    return -1;
                }
                
            case WGOperationKey.OP_DOCUMENT_BY_KEY:
            case WGOperationKey.OP_DOCUMENT_CORE:
            case WGOperationKey.OP_DOCUMENT_CORE_FASTACCESS:
            case WGOperationKey.OP_SAVE:
            case WGOperationKey.OP_DELETE:
                if (key instanceof WGDocumentKey) {
                    return ((WGDocumentKey) key).getDocType();
                }
                else if (key instanceof String) {
                    
                }
                else {
                    return -1;
                }
                
            case WGOperationKey.OP_HDB_CREATE_STORAGE:
            case WGOperationKey.OP_HDB_GET_OR_CREATE_STORAGE:
                return 0;
            
            case WGOperationKey.OP_STRUCT_PARENT:
            case WGOperationKey.OP_STRUCT_ROOTS:
            case WGOperationKey.OP_STRUCT_CHILDREN:
                return WGDocument.TYPE_STRUCTENTRY;
                
            
            default:
                return -1;
        }
        
        
    }
    

    /**
     * Refreshes the data in database, testing for changes in the backend and
     * clearing all caches.
     * 
     * @throws WGAPIException
     */
    public void refresh() throws WGAPIException {

        synchronized (_cacheMaintenanceMonitor) {
            if (!isSessionOpen()) {
                throw new WGClosedSessionException();
            }
    
            verboseCacheManagement("Performing complete cache refresh");
    
            if (getSessionContext().isTransactionActive()) {
                //clear session cache            
                getSessionContext().clearCache();
                return;
            }
    
            // obtain a dblock - to ensure no documents are locked
            this.lock();
    
            this.getSessionContext().setDatabaseUpdated(true);
            this.clearDocumentMappings();
            this.getCore().refresh();
            clearUserCaches();
            getSessionContext().clearCache();
            this.fireDatabaseEvent(new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_UPDATE));
    
            // unlock db
            this.unlock();
        }
    }

    /**
     * Closes the database session. The database object and all child objects.
     * 
     * @throws WGAPIException
     */
    public void closeSession() throws WGAPIException {
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        
        WGSessionContext oldContext = getSessionContext();
        oldContext.preClose();

        // Notify design provider
        if (designProvider != null) {
            designProvider.closeSession();
        }
        
        try {
            getCore().closeSession();
        }
        finally {
            
            oldContext.close();
            if (oldContext != null && oldContext.isMaxCoreMessageShowed()) {
                addToStatistics(oldContext);
            }

            // release obtained locks of sessioncontext
            getLockManager().releaseAllLocks(oldContext);

            setSessionContext(null);
        }
    }

    /**
     * Called to drop all retrieved document cores for this session at once.
     * 
     * @param untimelyDispose
     */
    public void dropAllDocumentCores(boolean untimelyDispose) {

        getSessionContext().dropAllDocumentCores(untimelyDispose);

    }

    /**
     * Adds a session statistic about the current session to collected session
     * statistics.
     * 
     * @param oldContext
     */
    private void addToStatistics(WGSessionContext oldContext) {

        sessionStatistics.add(new SessionStatistic(oldContext));
        if (sessionStatistics.size() > 100) {
            sessionStatistics.remove(sessionStatistics.first());
        }

    }

    /**
     * Closes and reopens the session with the given user information
     * 
     * @param username
     * @param credentials
     * @return the access level of the new session
     * @throws WGAPIException
     */
    public int reopenSession(String username, Object credentials) throws WGAPIException {
        return reopenSession(username, credentials, null);
    }
    
    public int reopenSession(String username, Object credentials, String filter) throws WGAPIException {
    	return reopenSession(username, credentials, filter, null);
    }
    
    /**
     * Closes and reopens the session with the given user information
     * 
     * @param username
     * @param credentials
     * @param filter ID of the user access filter to apply. Specify null for none.
     * @return the access level of the new session
     * @throws WGAPIException
     */
    public int reopenSession(String username, Object credentials, String filter, HttpServletRequest request) throws WGAPIException {

        if (isSessionOpen()) {
            closeSession();
        }

        int accessLevel = openSession(username, credentials, filter, request);
        if (accessLevel > WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
            // Do an "out of the order" cache maintenance now, to ensure the new session is on the most current state
            // as reopenSession() is often used to catch-up with changes done on another thread
            catchupBackendChanges();            
        }
        
        return accessLevel;

    }

    /**
     * Closes and reopens the session with the current logged in user. If no
     * session is open, opens a master session.
     * 
     * @return The access level in the new session
     * @throws WGAPIException
     */
    public int reopenSession() throws WGAPIException {

        if (!isSessionOpen() || getSessionContext().isMasterSession()) {
            return reopenSession(null, null);
        }
        else {
            return reopenSession(getSessionContext().getUser(), getSessionContext().getCredentials());
        }

    }

    /**
     * Removed a document and does all neccessary operations after that. -
     * Maintain caches - Throw events - Set states Either parameter document or
     * log must be present
     * 
     * @param document
     *            The document that is/was removed. Can be null if it was
     *            deleted in background and not in cache
     * @param docKey
     *            The document key of the document removed
     * @param removedByWGAPI
     *            true if the removing is triggered by WGAPI and should be
     *            executed in this method.
     * @return True if deletion succeeded
     * @throws WGAPIException
     */
    protected boolean remove(WGDocument document) throws WGAPIException {

        // Prepare deletion
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        // Only neccessary if document was in cache = document is present
        boolean dropQueryCache = false;
        
        
        // check document lock
        if (document.getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Unable to remove document. Document is locked.");
        }

        // check child locks of document for current session context
        if (getLockManager().foreignChildLocksExists(document, getSessionContext())) {
            throw new ResourceIsLockedException("Unable to remove document. Child objects of the object you want to remove are locked.");
        }

        // remove all relations pointing to me
        // + look if we need to clear the query cache
        if (document instanceof WGContent) {
            WGContent content = (WGContent) document;

            /*
             * We can't remove relations here because we need to update relation source contents and may not have sufficient rights for that.
             */
            //content.removeAllIncomingRelations();
            
            if (content.getRetrievalStatus().equals(WGContent.STATUS_RELEASE)) {
                dropQueryCache = true;
            }

        }
        
        // Ensure the document knows its cache parents
        if (document instanceof WGStructEntry) {
            WGStructEntry struct = (WGStructEntry) document;
            struct.getParentEntry();
            struct.getArea();
        }

        // Delete
        verboseBackendAccess(WGOperationKey.OP_DELETE, document.getDocumentKeyObj());
        WGDatabaseRevision revision = document.getCore().remove();
        
    
        documentRemoved(document, document.getDocumentKeyObj(), true, dropQueryCache, revision);
        return true;

    }

    private void documentRemoved(WGDocument document, WGDocumentKey docKey, boolean removedByWGAPI, boolean dropQueryCache, WGDatabaseRevision revision) throws WGAPIException {

        if (getSessionContext().isTransactionActive()) {
            return;
        }
        
        // Clear the document cache
        masterDocumentCaches.remove(docKey);

        // Clear the caches on the cached document object(s) and its cache parents, mark them deleted
        if (document != null) {
            document.setEdited(false);
            document.setDeleted(true);
            for (WGDocumentKey cacheParentKey : document.getCacheParents()) {
                WGDocument cacheParent = getDocumentByDocumentKeyFromCache(cacheParentKey);
                if (cacheParent != null) {
                    cacheParent.dropCache();
                }
            }

        }
            
        WGDocument cachedDocument = unmapDocumentObject(docKey);
        if (cachedDocument != null && cachedDocument != document) {
            cachedDocument.setEdited(false);
            cachedDocument.setDeleted(true);
            for (WGDocumentKey cacheParentKey : cachedDocument.getCacheParents()) {
                WGDocument cacheParent = getDocumentByDocumentKeyFromCache(cacheParentKey);
                if (cacheParent != null) {
                    cacheParent.dropCache();
                }
            }
        }
        
        // Clear indirect caches
        if (document == null && isDesignDocumentType(docKey.getDocType())) {
            designDocumentLists.clear();
        }

        _userCache.clear();
        if (dropQueryCache) {
            try {
                verboseCacheManagement("Flushing query cache on behalf of removed document " + docKey.toString());
                masterQueryCache.flushAll();
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception flushing query cache on database '" + getDbReference() + "'", e);
            }
        }

        // Fire content has been deleted event
        if (docKey.getTypename().equals(WGDocument.TYPENAME_CONTENT)) {
            WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_HASBEENDELETED, docKey.toString(), null, this);
            if (document != null) {
                event.setContent((WGContent) document);
            }
            fireContentEvent(event);
        }

        // Fire design change event
        if (docKey.getTypename().equals(WGDocument.TYPENAME_FILECONTAINER) || docKey.getTypename().equals(WGDocument.TYPENAME_TML) || docKey.getTypename().equals(WGDocument.TYPENAME_CSSJS)) {
            // Only fire when no design provider present. Otherwise the
            // listeners are registered at the provider and it is his
            // responsibility to throw the event
            if (getDesignProvider() == null) {
                List<WGUpdateLog> logs = new ArrayList<WGUpdateLog>();
                logs.add(new WGUpdateLog(WGUpdateLog.TYPE_DELETE, new Date(), getSessionContext().getUser(), docKey.toString(), null, revision));
                fireDatabaseDesignChangedEvent(new WGDesignChangeEvent(null, this, logs));
            }
        }

        // Remove the session context, so we won't refetch it by the
        // cached document there
        getSessionContext().removeDocumentContext(docKey);

        // Fire database event if the remove has been done by WGAPI
        // When this is done in background change processing, the event
        // will get fired after all changes by checkDatabaseUpdates
        updateCacheMaintenanceData();
        if (removedByWGAPI) {
            this.getSessionContext().setDatabaseUpdated(true);
            fireDatabaseEvent(new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_UPDATE, docKey));
        }
        
        if (document != null && revision != null && !getSessionContext().isTransactionActive()) {
            document.setCacheRevision(revision);
        }
    }

    /**
     * Returns the area with the given name
     * 
     * @param strName
     *            Name of the area
     * @return WGArea
     * @throws WGAPIException
     */
    public WGArea getArea(String strName) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (WGArea) getDesignObject(WGDocument.TYPE_AREA, strName, null);

    }

    /**
     * Returns a map of all areas of this database, mapped by their area name.
     * 
     * @return WGAreaMap
     * @throws WGAPIException
     */
    public WGAreaMap getAreas() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        List areasList = getDesignObjects(WGDocument.TYPE_AREA);
        if (areasList == null) {
            return null;
        }

        Iterator areas = areasList.iterator();
        TreeMap areasMap = new TreeMap();

        WGArea area;
        while (areas.hasNext()) {
            area = (WGArea) areas.next();
            areasMap.put(area.getName(), area);
        }

        return new WGAreaMap(areasMap);
    }

    /**
     * Returns a struct entry by it's page sequence
     * 
     * @param seq				The page sequence number
     * @return WGStructEntry 	The found struct entry, null if there is none with that key.
     * @throws WGAPIException
     */    
    public WGStructEntry getStructEntryBySequence(long seq) throws WGAPIException{
    	
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        if(!(getCore() instanceof WGDatabaseCoreFeaturePageSequences))
        	throw new WGNotSupportedException("Page sequences are not supported for this database");
        
        WGDocumentCore struct = ((WGDatabaseCoreFeaturePageSequences)getCore()).getStructEntryBySequence(seq);
        if (struct != null && !struct.isDeleted()) {
            return this.getOrCreateStructEntryObject(struct, new WGDocumentObjectFlags());
        }

        return null;

    }
    
    /**
     * Returns a struct entry by it's struct key
     * 
     * @param structKey			The struct key to find a struct entry for.
     * @return WGStructEntry 	The found struct entry, null if there is none with that key.
     * @throws WGAPIException
     */
    public WGStructEntry getStructEntryByKey(Object structKey) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (WGStructEntry) getDocumentByKey(new WGDocumentKey(WGDocument.TYPE_STRUCTENTRY, String.valueOf(structKey), null));
    }

    private WGStructEntry retrieveStructEntry(Object structKey) throws WGAPIException {

        WGStructEntry entry = null;

        WGDocumentCore entryCore = this.getCore().getStructEntryByKey(structKey);
        if (entryCore != null && !entryCore.isDeleted()) {
            entry = this.getOrCreateStructEntryObject(entryCore, new WGDocumentObjectFlags());
        }
        else {
            mapNonExistentDocIndicator(masterDocumentsByKey, String.valueOf(new WGDocumentKey(WGDocument.TYPE_STRUCTENTRY, String.valueOf(structKey), null)), WGDocument.TYPE_STRUCTENTRY);
        }

        return entry;
    }

    /**
     * Wraps a struct entry core into a WGStructEntry object
     * 
     * @param doc
     *            core to wrap
     * @return WGStructEntry
     * @throws WGAPIException
     */
    protected WGStructEntry getOrCreateStructEntryObject(WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {

        WGDocumentKey key = WGDocument.buildDocumentKey(doc, this);
        WGStructEntry entry;
        try {
            entry = (WGStructEntry) getDocumentByDocumentKeyFromCache(key, false);
        }
        catch (WGDocumentDoesNotExistException e) {
            entry = null;
        }

        if (entry == null) {
            entry = new WGStructEntry(this, doc, flags);
            if (entry.getStructKey() != null) {

                // Directly fill some very relevant caches once we have the core
                if (!entry.isTemporary() && !entry.isDummy() && !"true".equals(getSessionContext().getAttribute(WGSessionContext.SATTRIB_BYPASS_PREEMTIVE_CACHING))) {
                    entry.getMetaData(WGStructEntry.META_KEY);
                    entry.getMetaData(WGStructEntry.META_AREA);
                    entry.getMetaData(WGStructEntry.META_POSITION);
                    entry.getMetaData(WGStructEntry.META_CONTENTTYPE);
                }

                mapDocumentObject(entry);
            }
        }
        else {
            if (!entry.isEdited()) {
                entry.setCore(doc);
            }
        }
        return entry;

    }

    /**
     * Returns the child entries of a given struct entry
     * 
     * @param structEntry
     *            struct entry to retrieve child entries for
     * @return WGAPIException
     */
    protected WGStructEntryRetrievalIterator getChildEntries(WGStructEntry structEntry, WGPageOrderSet pageOrder) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (structEntry == null || structEntry.isDeleted() || structEntry.isDummy() || (structEntry.getCore() instanceof WGFakeDocument && structEntry.getCore().isDeleted())) {
            return WGStructEntryRetrievalIterator.emptyIterator();
        }

        return new WGStructEntryRetrievalIterator(this, structEntry, this.getCore().getChildEntries(structEntry, pageOrder));

    }

    /**
     * Returns all content for a struct entry
     * 
     * @param structEntry
     *            Struct entry, whose content is to be retrieved
     * @return WGContentList
     * @throws WGAPIException
     */
    protected WGContentList getAllContent(WGStructEntry structEntry, boolean includeArchived) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        try {
        if (structEntry == null || structEntry.isDeleted() || structEntry.isDummy() || (structEntry.getCore() instanceof WGFakeDocument && structEntry.getCore().isDeleted())) {
            return WGContentList.create();
        }
        }
        catch (WGDeletedException e) {
            return WGContentList.create();
        }

        ArrayList contents = new ArrayList();
        List contentCoresList = this.getCore().getAllContent(structEntry, includeArchived);
        if (contentCoresList != null) {
            Iterator contentCores = contentCoresList.iterator();

            WGDocumentCore contentCore;
            WGContent content;

            while (contentCores.hasNext()) {
                contentCore = (WGDocumentCore) contentCores.next();
                content = getOrCreateContentObject(contentCore);

                if (content != null) {
                    contents.add(content);
                }
            }
        }

        return WGContentList.create(contents);

    }
    
   /**
    * Returns the released content of the given struct entry and language without retrieving anything other
    * 
    * @param entry The struct entry
    * @param strLanguage The language to retrieve.
    * @throws WGAPIException 
    */
   protected WGContent getReleasedContent(WGStructEntry entry, String strLanguage) throws WGAPIException {
       
       if (strLanguage == null) {
           strLanguage = getDefaultLanguage();
       }
       
       WGContentKey relContentKey = new WGContentKey(entry.getStructKey(), strLanguage, 0);
       return retrieveContentByKey(relContentKey);
       
       
   }
    

    /**
     * Returns on operation key for syncing of backend access. Should not be
     * used outside the WGAPI.
     * 
     * @param operation
     * @param key
     * @return The key for the given operation. Can be a new created one or an
     *         existing key for this operation, if there was one.
     */
    public WGOperationKey obtainOperationKey(int operation, Object key) {

        if (_maintainOperationKeys) {
            try {
                _wgOperationKeySemaphore.acquireUninterruptibly();
                return performObtainOperationKey(operation, String.valueOf(key));
            } finally {
                _wgOperationKeySemaphore.release();
            }
        } else {
            return performObtainOperationKey(operation, String.valueOf(key));
        }
    }
    
    private WGOperationKey performObtainOperationKey(int operation, String key) {
        String operationKeyStr = WGOperationKey.createString(operation, key);
        WGOperationKey opKey = (WGOperationKey) this.operations.get(operationKeyStr);
        if (opKey != null) {
            return opKey;
        }
        else {
            opKey = new WGOperationKey(this, operation, key);
            this.operations.put(operationKeyStr, opKey);
            return opKey;
        }
    }

    /**
     * Returns all content for a struct entry
     * 
     * @param structEntry
     *            Struct entry, whose content is to be retrieved
     * @return WGContentList
     * @throws WGAPIException
     */
    protected WGContentList getAllContent(WGStructEntry structEntry) throws WGAPIException {
        return this.getAllContent(structEntry, false);
    }

    /**
     * Wraps a design document core into a matching WGDesignDocument object
     * 
     * @param doc
     *            core to wrap
     * @return WGDesignDocument
     * @throws WGAPIException
     */
    private WGDesignDocument createDesignDocumentObject(WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {

        int type = doc.getType();
        WGDesignDocument design = null;

        if (type == WGDocument.TYPE_AREA) {
            design = new WGArea(this, doc, flags);
        }
        else if (type == WGDocument.TYPE_CONTENTTYPE) {
            design = new WGContentType(this, doc, flags);
        }
        else if (type == WGDocument.TYPE_TML) {
            design = new WGTMLModule(this, doc, flags);
        }
        else if (type == WGDocument.TYPE_FILECONTAINER) {
            design = new WGFileContainer(this, doc, flags);
        }
        else if (type == WGDocument.TYPE_LANGUAGE) {
            design = new WGLanguage(this, doc, flags);
        }
        else if (type == WGDocument.TYPE_CSSJS) {
            design = new WGScriptModule(this, doc, flags);
        }

        mapDocumentObject(design);
        return design;
    }
    


    /**
     * Returns a design object with the given information
     * 
     * @param type
     *            document type choosing, which kind of design object to
     *            retrieve. Use constants WGDocument.TYPE_...
     * @param strName
     *            Name of the design object
     * @param mediaKey
     *            media key of the design object. If design object has no media
     *            key, provide null.
     * @return WGDesignDocument
     * @throws WGAPIException
     */
    public WGDesignDocument getDesignObject(int type, String strName, String mediaKey) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (strName == null) {
            return null;
        }

        return (WGDesignDocument) getDocumentByKey(new WGDocumentKey(type, strName, mediaKey));
    }

    private WGDesignDocument retrieveDesignDocument(int type, String strName, String mediaKey) throws WGAPIException {

        WGDesignDocument design = null;
        WGDocumentCore designCore = getDesignObjectCore(type, strName, mediaKey);
        if (designCore != null && !designCore.isDeleted()) {
            design = this.getOrCreateDesignDocumentObject(designCore, new WGDocumentObjectFlags());
        }
        else {
            mapNonExistentDocIndicator(masterDocumentsByKey, (new WGDocumentKey(type, strName, mediaKey)).toString(), type);
        }

        return design;

    }

    protected WGDocumentCore getDesignObjectCore(int type, String strName, String mediaKey) throws WGAPIException {
        String mediaKeyParam = (mediaKey != null ? mediaKey.toLowerCase() : null);
        String designNameParam = toLowerCaseMeta(strName);

        if (designProvider != null && designProvider.providesType(type) && !isMetadataModule(type, strName)) {
            return designProvider.getDesignObject(type, designNameParam, mediaKeyParam);
        }
        else {
            return this.getCore().getDesignObject(type, designNameParam, mediaKeyParam);
        }
    }

    private boolean isMetadataModule(int type, String strName) {

        return (type == WGDocument.TYPE_CSSJS && strName.startsWith(WGCSSJSModule.METADATA_MODULE_QUALIFIER));

    }

    /**
     * Returns a design object with the given information. In this method the
     * media key is omitted. Therefor it is not suitable to retrieve WebTML
     * Modules
     * 
     * @param type
     *            document type choosing, which kind of design object to
     *            retrieve. Use constants WGDocument.TYPE_...
     * @param strName
     *            Name of the design object
     * @return WGDesignDocument
     * @throws WGAPIException
     */
    public WGDesignDocument getDesignObject(int type, String strName) throws WGAPIException {
        return this.getDesignObject(type, strName, null);
    }

    /**
     * Returns the root entries to an area.
     * 
     * @param area
     *            The area to fetch root entries for
     * @return WGStructEntryList
     * @throws WGAPIException
     * @throws WGAPIException
     */
    protected WGStructEntryRetrievalIterator getRootEntries(WGArea area, WGPageOrderSet order) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (area == null || area.isDeleted() || area.isDummy() || (area.getCore() instanceof WGFakeDocument && area.getCore().isDeleted())) {
            return WGStructEntryRetrievalIterator.emptyIterator();
        }

        return new WGStructEntryRetrievalIterator(this, area,  this.getCore().getRootEntries(area, order));
        
    }

    /**
     * Returns the title of this database (if there is one).
     * 
     * @return String
     */
    public String getTitle() {

        return title;

    }

    /**
     * Registers a listerer for database events.
     * 
     * @param listener
     *            The listener to register.
     */
    public void addDatabaseEventListener(WGDatabaseEventListener listener) {

        if (listener.isTemporary()) {
            this.getSessionContext().addTemporaryEventListener(listener);
        }
        else {
            if (!this.databaseEventListeners.contains(listener)) {
                this.databaseEventListeners.add(listener);
            }
        }

    }

    /**
     * Adds a listener for the connection of this database to its backend.
     * 
     * @param listener
     */
    public void addDatabaseConnectListener(WGDatabaseConnectListener listener) {

        if (!this.databaseConnectListeners.contains(listener)) {
            this.databaseConnectListeners.add(listener);
        }

    }

    /**
     * Removes a listener for the connection of this database to its backend
     * 
     * @param listener
     */
    public void removeDatabaseConnectListener(WGDatabaseConnectListener listener) {
        this.databaseConnectListeners.remove(listener);
    }

    /**
     * Adds a listener for content events to the database.
     * 
     * @param listener
     */
    public void addContentEventListener(WGContentEventListener listener) {

        if (!contentEventListeners.contains(listener)) {
            this.contentEventListeners.add(listener);
        }

    }

    /**
     * Adds a listener for content events to the database.
     * 
     * @param listener
     */
    public void addDocumentEventListener(WGDocumentEventListener listener) {

        if (!documentEventListeners.contains(listener)) {
            this.documentEventListeners.add(listener);
        }

    }

    /**
     * Adds a listener for workflow events to the database
     * 
     * @param listener
     */
    public void addWorkflowEventListener(WGWorkflowEventListener listener) {

        if (!this.workflowEventListeners.contains(listener)) {
            this.workflowEventListeners.add(listener);
        }

    }

    /**
     * Removes a database event listener.
     */
    public void removeDatabaseEventListener(WGDatabaseEventListener listener) {

        if (listener.isTemporary()) {
            return;
        }

        this.databaseEventListeners.remove(listener);

    }

    /**
     * Removes a content event listener.
     */
    public void removeContentEventListener(WGContentEventListener listener) {
        this.contentEventListeners.remove(listener);
    }

    /**
     * Removes a content event listener.
     */
    public void removeDocumentEventListener(WGDocumentEventListener listener) {
        this.documentEventListeners.remove(listener);
    }

    /**
     * Removes a workflow event listener.
     */
    public void removeWorkflowEventListener(WGWorkflowEventListener listener) {

        this.workflowEventListeners.remove(listener);

    }

    /**
     * Fires a database event and notifies all listeners
     * 
     * @param event
     *            The event to throw
     */
    private void fireDatabaseEvent(WGDatabaseEvent event) {

        if (!isSessionOpen()) {
            return;
        }

        if (!getSessionContext().isEventsEnabled()) {
            return;
        }

        
        Iterator listeners = this.databaseEventListeners.iterator();
        while (listeners.hasNext()) {
            WGDatabaseEventListener listener = (WGDatabaseEventListener) listeners.next();
            listener.databaseUpdate(event);
        }
        

    }

    /**
     * Fires a event, that signals that this database has been connected to its
     * backend or that this process resulted in an error.
     * 
     * @param event
     *            The event to fire
     */
    private void fireDatabaseConnectEvent(WGDatabaseEvent event) {

        if (!isSessionOpen()) {
            if (event.getType() != WGDatabaseEvent.TYPE_CONNECTION_ERROR) {
                return;
            }
        }
        else if (!getSessionContext().isEventsEnabled()) {
            return;
        }

        
        Iterator listeners = this.databaseConnectListeners.iterator();
        while (listeners.hasNext()) {
            WGDatabaseConnectListener listener = (WGDatabaseConnectListener) listeners.next();
            if (event.getType() == WGDatabaseEvent.TYPE_CONNECTED) {
                listener.databaseConnected(event);
            }
            else if (event.getType() == WGDatabaseEvent.TYPE_CONNECTION_ERROR) {
                listener.databaseConnectionError(event);
            }
        }
        
    }

    protected void fireDocumentEvent(WGDocumentEvent event) throws WGAPIException {

    	if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (!getSessionContext().isEventsEnabled()) {
            return;
        }
    	
        for(WGDocumentEventListener listener: documentEventListeners){
        	listener.handleEvent(event);
        }
    }

    protected boolean fireContentEvent(WGContentEvent event) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (!getSessionContext().isEventsEnabled()) {
            return true;
        }
        
        Iterator listeners = this.contentEventListeners.iterator();
        while (listeners.hasNext()) {
            WGContentEventListener listener = (WGContentEventListener) listeners.next();
            if (event.getType() == WGContentEvent.TYPE_CREATED) {
                listener.contentCreated(event);
            }
            else if (event.getType() == WGContentEvent.TYPE_SAVED) {
                if (listener.contentSaved(event) == false) {
                    return false;
                }
            }
            else if (event.getType() == WGContentEvent.TYPE_HASBEENSAVED) {
                listener.contentHasBeenSaved(event);
            }
            else if (event.getType() == WGContentEvent.TYPE_HASBEENDELETED) {
                listener.contentHasBeenDeleted(event);
            }
            else if (event.getType() == WGContentEvent.TYPE_HASBEENMOVED) {
                listener.contentHasBeenMoved(event);
            }
            else if (event.getType() == WGContentEvent.TYPE_STATUSCHANGED) {
                listener.contentStatusChanged(event);
            }
        }
        
        return true;
    }

    /**
     * Fires a workflow event. Not to be called outside the WGAPI.
     * 
     * @param event
     * @return A list of results that were returned by the event recipients
     */
    public List fireWorkflowEvent(WGWorkflowEvent event) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (!getSessionContext().isEventsEnabled()) {
            return new ArrayList();
        }

        List resultList = new ArrayList();
        
        Iterator listeners = this.workflowEventListeners.iterator();
        while (listeners.hasNext()) {
            WGWorkflowEventListener listener = (WGWorkflowEventListener) listeners.next();
            listener.workflowMail(event);
        }
        
        return resultList;
    }

    /**
     * Opens a new session.
     * 
     * @param user
     *            The username for the new session. Specify null for master
     *            login.
     * @param credentials
     *            The password to the username. Specify null for master login.
     * @return The access level to the database, as constant
     *         WGDatabase.ACCESSLEVEL_...
     * @throws WGAPIException
     */
    public int openSession(String user, Object credentials) throws WGAPIException {
        return innerOpenSession(user, credentials, null, null);
    }
    
    /**
     * Opens a session as anonymous user
     * @throws WGAPIException
     */
    public int openAnonymousSession() throws WGAPIException {
        return openSession(WGDatabase.ANONYMOUS_USER, null);
    }
    
    /**
     * Opens a session as anonymous user, including an access filter
     * @param accessFilter The filter id
     * @throws WGAPIException
     */
    public int openAnonymousSession(String accessFilter) throws WGAPIException {
        return openSession(WGDatabase.ANONYMOUS_USER, null, accessFilter);
    }

    
    /**
     * Opens a new session.
     * 
     * @param user
     *            The username for the new session. Specify null for master
     *            login.
     * @param credentials
     *            The password to the username. Specify null for master login.
     * @param filter
     *            A user access filter to reduce the user rights
     * @return The access level to the database, as constant
     *         WGDatabase.ACCESSLEVEL_...
     * @throws WGAPIException
     */
    public int openSession(String user, Object credentials, String filter, HttpServletRequest request) throws WGAPIException {
        return innerOpenSession(user, credentials, filter, request);
    }
    public int openSession(String user, Object credentials, String filter) throws WGAPIException {
        return innerOpenSession(user, credentials, filter, null);
    }
    
    private int innerOpenSession(String user, Object credentials, String accessFilterUid, HttpServletRequest request) throws WGAPIException {

        if (!isReady()) {
            throw new WGUnavailableException(this, "The database is currently not ready for operation");
        }
        
        if (this.isSessionOpen()) {
            if (user == null && getSessionContext().isMasterSession()) {
                return this.getSessionContext().getAccessLevel();
            }            
            else if (isMemberOfUserList(Collections.singletonList(user))) {
                return this.getSessionContext().getAccessLevel();
            }
            else {
                closeSession();
            }
        }

        // Determine if this database yet has to be connected to the backend
        if (!isConnected()) {
            synchronized (this) {
                if (!isConnected()) {
                    boolean wasConnected = false;
                    try {
                        wasConnected = connectDBCore(false);
                    }
                    catch (WGInvalidDatabaseException e) {
                        WGDatabaseEvent event = new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_CONNECTION_ERROR);
                        fireDatabaseConnectEvent(event);
                        throw new WGUnavailableException(this, "Unable to connect to database core", e);
                    }
        
                    // Fire the connect event in case connectDBCore has connected the
                    // core
                    // If this is false, the current thread most likely was blocked on
                    // connectDBCore() by some other
                    // thread that built the core connection.So this thread entered the
                    // method when the core was
                    // already connected.
                    if (wasConnected == true) {
                        WGDatabaseEvent event = new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_CONNECTED);
                        fireDatabaseConnectEvent(event);
                        notifyDatabaseActions(_connectActions);
                        // We close the (master) session that resulted from connecting
                        // the core
                        // We will open the real user session later
                        closeSession();
                    }
                }
            }
        }

        WGFactory.getLogger().debug("WGDatabase open session " + path);

        // If both params are null, the master login (from initial open command)
        // will be used
        boolean masterLogin = false;
        
        // Create authentication session
        AuthenticationSession authSession = null;
        AuthenticationModule authModule = getAuthenticationModule();

        // Master login
        if (user == null && credentials == null) {
            user = this.masterLoginInputName;
            credentials = this.masterLoginPassword;
            masterLogin = true;
            authSession = MasterLoginAuthSession.getInstance();
        }
        
        
        // Anonymous login
        else if (user.equals(WGDatabase.ANONYMOUS_USER)) {
        	if(authModule!=null && authModule instanceof AnonymousAwareAuthenticationModule && request!=null)
        		authSession = ((AnonymousAwareAuthenticationModule)authModule).anonymousLogin(request);
        	else authSession = AnonymousAuthSession.getInstance();
        }
        
        // Regular login against authentication module
        else if (authModule != null) {
            if (certAuthEnabled() && (credentials instanceof X509Certificate)) {
                authSession = ((CertAuthCapableAuthModule) authModule).login((X509Certificate) credentials);
            }
            else {
            	if(authModule instanceof RequestAwareAuthenticationModule)
            		authSession = ((RequestAwareAuthenticationModule)authModule).login(user, credentials, request);
            	else authSession = authModule.login(user, credentials);
            }
            
            if (authSession == null) {
                return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
            }
        }
        
        // Backend login
        else if (hasFeature(FEATURE_PERFORMS_BACKEND_LOGIN)) { 
            authSession = new BackendAuthSession(user, credentials);
        }
        
        // If no auth module and database does not accept backend logins we use an anonymous session
        else {
            authSession = AnonymousAuthSession.getInstance();
        }

        // Open core session
        WGUserAccess userAccess = this.core.openSession(authSession, credentials, masterLogin);
        if (userAccess.getAccessLevel() <= ACCESSLEVEL_NOACCESS) {
            return userAccess.getAccessLevel();
        }
        
        WGUserAccess originalUserAccess = null;
        String userCacheKey = getUserCacheKey(authSession);
        if (accessFilterUid != null) {
            UserAccessFilter filter = WGFactory.getInstance().getUserAccessFilter(accessFilterUid);
            if (filter != null) {
                originalUserAccess = userAccess;
                Map userCache = getUserCache().getMapForUser(userCacheKey);
                userAccess = (WGUserAccess) userCache.get(USERCACHE_FILTEREDACCESS_PREFIX + accessFilterUid);
                if (userAccess == null || userAccess.isOutdated()) {
                    userAccess = originalUserAccess.applyUserAccessFilter(filter);
                    userCache.put(USERCACHE_FILTEREDACCESS_PREFIX + accessFilterUid, userAccess);
                }
                userCacheKey = accessFilterUid + "////" + userCacheKey;
            }
        }
        
        userHashMapGroup.fetchAllMapsForUser(userCacheKey);
        WGSessionContext sessionContext = new WGSessionContext(this, authSession, credentials, userAccess, originalUserAccess);
        sessionContext.setCachingEnabled(cachingEnabled);
        this.setSessionContext(sessionContext);
        core.setCurrentSession(sessionContext);
        
        // Notify design provider
        if (designProvider != null) {
            designProvider.openSession(sessionContext);
        }
       
        return userAccess.getAccessLevel();

    }
    
    /**
     * Opens a new session using a certificate for login.
     * To use certificate authentication you must use a {@link CertAuthCapableAuthModule} with enabled certificate authentication
     * @param cert
     *            The certificate used to login.
     * @param filter   
     *            A user access filter to reduce user rights
     * @return The access level to the database, as constant
     *         WGDatabase.ACCESSLEVEL_...
     * @throws WGAPIException
     */
    public int openSession(X509Certificate cert, String filter) throws WGAPIException {
        
        if (!"true".equals(getCreationOptions().get(COPTION_FAKE_CERTAUTH))) {
        
            if (!certAuthEnabled()) {
                WGFactory.getLogger().warn("WGAPI: Tried to access database " + getDbReference() + " via certificate auth although it is not configured for it");
                return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
            }
            
            if (cert == null) {
                return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
            }
        
            CertAuthCapableAuthModule auth = (CertAuthCapableAuthModule) getAuthenticationModule();
            
            X509Certificate caCert = auth.getCA();
            X509CRL crl = auth.getCRL();
            
            // verify if clientCert is issued by given dbCA
            if (!CertificateValidationUtils.verify(cert, caCert)) {
                String message = "Failed login for '" + cert.getSubjectDN().getName() + "': Certificate was signed by another CA (Certificate authentication)";            
                WGFactory.getLogger().warn(message);
                return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
            }
            
            // check if client certificate is revoked
            if (CertificateValidationUtils.isCertRevoked(crl, cert)) {
                String message = "Failed login for '" + cert.getSubjectDN().getName() + "': Certificate was revoked by CRL (Certificate authentication)";
                WGFactory.getLogger().info(message);
                return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
            }
        }
        
        String user = cert.getSubjectDN().toString();
        return innerOpenSession(user, cert, filter, null);
        
    }

    private synchronized void notifyDatabaseActions(List<DatabaseAction> actions) {

        Iterator<DatabaseAction> actionsIt = actions.iterator();
        while (actionsIt.hasNext()) {
            DatabaseAction action = actionsIt.next();
            try {
                action.run(this);
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error executing connect action", e);
            }
        }
        actions.clear();

    }

    /**
     * Opens a new session with master login.
     * 
     * @throws WGAPIException
     */
    public int openSession() throws WGAPIException {
        return this.openSession(null, null, null);
    }

    private List wrapDesignCores(List designCores) throws WGAPIException {

        if (designCores == null) {
            return null;
        }

        if (designCores.size() == 0) {
            designCores = new ArrayList();
        }

        List designs = new ArrayList();
        Iterator objectCores = designCores.iterator();

        WGDocumentCore designCore;
        WGDesignDocument design;
        String name;
        String mediaKey;

        while (objectCores.hasNext()) {
            designCore = (WGDocumentCore) objectCores.next();
            if (designCore == null || designCore.isDeleted()) {
                continue;
            }

            name = (String) designCore.getMetaData(WGDesignDocument.META_NAME);

            if (designCore.getType() == WGDocument.TYPE_TML) {
                mediaKey = (String) designCore.getMetaData(WGTMLModule.META_MEDIAKEY);
            }
            else {
                mediaKey = null;
            }

            design = getOrCreateDesignDocumentObject(designCore, new WGDocumentObjectFlags());
            if (design != null) {
                designs.add(design);
            }
            else {
                if (WGFactory.getLogger().isDebugEnabled()) {

                    WGFactory.getLogger().error(
                            "Could not retrieve design object " + WGDesignDocument.doctypeNumberToName(designCore.getType()) + "/" + name + "/" + mediaKey + ". Check name validity");
                }
            }
        }
        return designs;
    }

    /**
     * Returns the imlementation type (i.e. the class name of the database core
     * implementation)
     */
    public String getType() {

        return this.type;

    }

    /**
     * Returns a descriptive type name of the database implementation used.
     */
    public String getTypeName() {
        return typeName;
    }

    /**
     * Returns all design objects of a specific type.
     * 
     * @param type
     *            The type of design objects to retrieve. Use constants
     *            WGDocument.TYPE_...
     * @return ArrayList List of WGDesignObject objects
     * @throws WGAPIException
     */
    public List<? extends WGDesignDocument> getDesignObjects(int type) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        // Double checked cache (un-synchronized and synchronized)
        Integer typeObj = new Integer(type);
        List designs = fetchDocumentListCache((WGDocumentListCache) this.designDocumentLists.get(typeObj), type);
        if (!getSessionContext().isCachingEnabled() || designs == null) {
            WGOperationKey op = obtainOperationKey(WGOperationKey.OP_DESIGN_LIST, type);
            synchronized (op) {
                try {
                    op.setUsed(true);
                    designs = fetchDocumentListCache((WGDocumentListCache) this.designDocumentLists.get(typeObj), type);
                    if (!getSessionContext().isCachingEnabled() || designs == null) {
                        designs = wrapDesignCores(getDesignObjectCores(type));
                        if (getSessionContext().isCacheWritingEnabled()) {
                            this.designDocumentLists.put(typeObj, WGDocumentListCache.buildFromDocuments(designs));
                        }
                    }
                }
                finally {
                    op.setUsed(false);
                }
            }
        }

        if (designs != null) {
            return designs;
        }
        else {
            return null;
        }

    }

    protected List<? extends WGDocument> fetchDocumentListCache(WGDocumentListCache listCache, int docType) {

        if (!isDoctypeCacheable(docType)) {
            return null;
        }

        if (listCache != null && isCachingEnabled()) {
            return listCache.buildDocumentList(this);
        }
        else {
            return null;
        }
    }
    
    protected List fetchDocumentListCache(WGDocumentListCache listCache, int docType, int offset, int size) {

        if (!isDoctypeCacheable(docType)) {
            return null;
        }

        if (listCache != null && isCachingEnabled()) {
            return listCache.buildDocumentSubList(this, offset, size);
        }
        else {
            return null;
        }
    }

    protected boolean isDoctypeCacheable(int docType) {
        boolean docTypeCacheable = true;
        if (getDesignProvider() != null) {
            if (!getDesignProvider().isNotifying() && getDesignProvider().providesType(docType)) {
                docTypeCacheable = false;
            }
        }
        return docTypeCacheable;
    }

    private List getDesignObjectCores(int type) throws WGAPIException {

        if (designProvider != null && designProvider.providesType(type)) {
            List designCores = designProvider.getDesignObjects(type);
            if (designCores != null && type == WGDocument.TYPE_CSSJS) {
                designCores = removeMetadataModules(designCores);
            }
            return designCores;
        }
        else {
            return this.getCore().getDesignObjects(type);
        }

    }

    private List removeMetadataModules(List designCores) throws WGAPIException {

        List results = new ArrayList();
        Iterator cores = designCores.iterator();
        WGDocumentCore core;
        while (cores.hasNext()) {
            core = (WGDocumentCore) cores.next();
            if (!isMetadataModule(core.getType(), (String) core.getMetaData(WGDesignDocument.META_NAME))) {
                results.add(core);
            }
        }
        return results;

    }

    /**
     * Retrieves a content by it's unique name.
     * 
     * @param strName
     *            Unique name of the content
     * @param strLanguage
     *            Language of the content
     * @return WGContent Content matching unique name and language, null if none
     *         exists.
     * @throws WGAPIException
     */
    public WGContent getContentByName(String strName, String strLanguage) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (strName == null || strName.trim().equals("")) {
            return null;
        }
        strName = strName.toLowerCase();

        String strCacheLanguage = strLanguage;
        if (strLanguage == null) {
            strCacheLanguage = getDefaultLanguage();
            if (strCacheLanguage == null) {
                strCacheLanguage = "de";
            }
        }

        strCacheLanguage = strCacheLanguage.toLowerCase();

        // Double checked cache (un-synchronized and synchronized)
        try {
            String byNameKey = buildUniqueNameCacheKey(strName, strCacheLanguage);
            WGContent content = (WGContent) fetchDocumentFromCache(this.masterDocumentByName, byNameKey);
            if (content == null || !isReleasedDocumentOfName(content, strName)) {
                WGOperationKey op = obtainOperationKey(WGOperationKey.OP_CONTENT_BY_NAME, strName);
                synchronized (op) {
                    try {
                        op.setUsed(true);
                        content = (WGContent) fetchDocumentFromCache(this.masterDocumentByName, byNameKey);
                        if (content == null || !isReleasedDocumentOfName(content, strName)) {
                            if (content != null) {
                                unmapUniqueName(strName);
                            }
                            content = retrieveContentByName(strName, strLanguage, byNameKey);
                        }
                    }
                    finally {
                        op.setUsed(false);
                    }
                }
            }

            return content;
            
        }
        catch (WGDocumentDoesNotExistException e) {
            return null;
        }

    }

    private String buildUniqueNameCacheKey(String strName, String strCacheLanguage) {
        return strName.toLowerCase().concat("|").concat(strCacheLanguage.toLowerCase());
    }

    private boolean isReleasedDocumentOfName(WGDocument doc, String strName) throws WGAPIException {
        

        if (doc instanceof WGContent) {
            WGContent content = (WGContent) doc;
            
            if (!WGContent.STATUS_RELEASE.equals(content.getStatus())) {
                return false;
            }

            
            if (strName.equals(content.getUniqueName())) {
                return true;
            }
            
            if (strName.equals(content.getStructEntry().getUniqueName())) {
                return true;
            }
        }
        else if (doc instanceof WGStructEntry) {
            WGStructEntry struct = (WGStructEntry) doc;
            
            if (strName.equals(struct.getUniqueName())) {
                return true;
            }
            
        }
        
        return false;
               
        
    }

    private WGContent retrieveContentByName(String strName, String strLanguage, String byNameKey) throws WGAPIException {
        WGContent content = null;
        
        // First try to find a struct entry of that name. If a struct of that name exists, look if it has released contents.
        WGStructEntry struct = getStructEntryByName(strName);
        if (struct != null) {
            content = struct.getReleasedContent(strLanguage);
            if (content != null) {
                return content;
            }
            else {
                return null;
            }
        }
        
        // Then try to find a content with the uname/lang combination
        WGDocumentCore contentCore = getCore().getContentByName(strName, strLanguage);
        if (contentCore != null && !contentCore.isDeleted()) {
            content = getOrCreateContentObject(contentCore);
            if (content != null) {
                if (content.isReadableForUser()) {
                    return content;
                }
                else {
                    return null;
                }
            }
        }
        
        // No content to find, cache the "non-existence" then return null
        mapNonExistentDocIndicator(this.masterDocumentByName, byNameKey, WGDocument.TYPE_CONTENT);
        return null;
        
        
    }

    /**
     * Retrieves a struct entry which has the given unique name
     * This method will only return struct entries if they themselves own the unique name, not if any content of them owns it.
     * @param strName Unique name
     * @return A struct entry of this name or null if the name is not given to any struct entry.
     * @throws WGAPIException
     */
    public WGStructEntry getStructEntryByName(String strName) throws WGAPIException {
        
        if (getContentStoreVersion() < CSVERSION_WGA5) {
            return null;
        }
        
        // Double checked cache (un-synchronized and synchronized)
        try {
            WGStructEntry struct = (WGStructEntry) fetchDocumentFromCache(this.masterDocumentByName, strName);
            if (struct == null || !isReleasedDocumentOfName(struct, strName)) {
                WGOperationKey op = obtainOperationKey(WGOperationKey.OP_STRUCT_BY_NAME, strName);
                synchronized (op) {
                    try {
                        op.setUsed(true);
                        struct = (WGStructEntry) fetchDocumentFromCache(this.masterDocumentByName, strName);
                        if (struct == null || !isReleasedDocumentOfName(struct, strName)) {
                            if (struct != null) {
                                unmapUniqueName(strName);
                            }
                            struct = retrieveStructEntryByName(strName);
                        }
                    }
                    finally {
                        op.setUsed(false);
                    }
                }
            }

            return struct;
            
        }
        catch (WGDocumentDoesNotExistException e) {
            return null;
        }
        
    }
    
    protected WGStructEntry retrieveStructEntryByName(String strName) throws WGAPIException {
        
        WGStructEntry struct = null;
        WGDocumentCore structCore = getCore().getStructEntryByName(strName);
        if (structCore != null) {
            struct = getOrCreateStructEntryObject(structCore, new WGDocumentObjectFlags());
        }
        else {
            mapNonExistentDocIndicator(this.masterDocumentByName, strName, WGDocument.TYPE_STRUCTENTRY);
        }
        return struct;
        
    }

    /**
     * Retrieves a content by it's unique name in the database's default
     * language.
     * 
     * @param name
     *            Unique name of the content
     * @return WGContent Content matching unique name, null if none exists.
     * @throws WGAPIException
     */
    public WGContent getContentByName(String name) throws WGAPIException {
        return getContentByName(name, getDefaultLanguage());
    }

    /**
     * Sets the sessionContext
     * 
     * @param sessionContext
     *            The sessionContext to set
     */
    private void setSessionContext(WGSessionContext sessionContext) {
        
        if (sessionContext == null) {
            this.sessionContext.remove();
        }
        else {
            this.sessionContext.set(sessionContext);
        } 
    }

    /**
     * Sets a custom attribute to the database. These attributes remain at the
     * database object forever and can even be retrieved when no session is
     * open. The attributes can later be retrieved via getAttribute().
     * 
     * @param name
     *            The custom attribute name
     * @param obj
     *            The attribute value.
     */
    public void setAttribute(String name, Object obj) {
        if (obj != null) {
            this.customAttributes.put(name, obj);
        }
        else {
            this.customAttributes.remove(name);
        }
    }

    /**
     * Removes a database attribute.
     * 
     * @param name
     *            Name of the attribute to remove
     * @return The removed attribute value, if there was one, null otherwise
     */
    public Object removeAttribute(String name) {
        return this.customAttributes.remove(name);
    }

    /**
     * Returns a custom attribute set to this database.
     * 
     * @param name
     *            Name of the attribute
     * @return Object Value of the attribute
     */
    public Object getAttribute(String name) {
        return this.customAttributes.get(name);
    }

    /**
     * Tests if the database contains an attribute of the given name
     * 
     * @param name
     *            Attribute name
     * @return true, if attribute exists, false (suprise) if not
     */
    public boolean hasAttribute(String name) {
        return this.customAttributes.containsKey(name);
    }

    /**
     * Returns the boolean value of an attribute, accepting boolean objects and
     * those string representations that are converted by
     * WGUtils.stringToBoolean().
     * 
     * @param name
     *            The name of the Attribute
     * @param defaultValue
     *            The default value to return, if the attribute is not
     *            interpretable as boolean
     * @return The boolean value of the attribute, or the default value if the
     *         attribute was not present or it's boolean value was not
     *         determinable
     */
    public boolean getBooleanAttribute(String name, boolean defaultValue) {
        return WGUtils.getBooleanMapValue(customAttributes, name, defaultValue);
    }

    /**
     * Executes a query in the database giving a result set of content documents
     * in return.
     * 
     * @param type
     *            The type of query. Available types vary by database
     *            implementation.
     * @param query
     *            The query. Format depends on query type.
     * @param parameters
     *            Additional parameters for the query. Use Constants
     *            WGDatabase.QUERYOPTION_... as map keys.
     * @return A result set containing the contents matching the query
     * @throws WGAPIException
     */
    public WGAbstractResultSet query(String type, String query, Map parameters) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (parameters == null) {
            parameters = new HashMap();
        }

        Boolean cachedResultSet = (Boolean) parameters.get(WGDatabase.QUERYOPTION_CACHERESULT);
        if (cachedResultSet == null) {
            cachedResultSet = new Boolean(false);
        }

        QueryCacheKey queryCacheKey = new QueryCacheKey(type, query, new HashMap(parameters));
        if (cachedResultSet.booleanValue() == true) {
            QueryCacheEntry cacheEntry = null;;
            try {
                cacheEntry = (QueryCacheEntry) masterQueryCache.read(queryCacheKey);
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception reading query cache on database '" + getDbReference() + "'", e);
            }
            if (cacheEntry != null) {
                parameters.put(WGDatabase.QUERYOPTION_RETURNQUERY, cacheEntry.getFullQuery());
                parameters.put(WGDatabase.QUERYOPTION_USEDCACHE, new Boolean(true));
                return new WGStandardResultSet(this, cacheEntry.getResultSet(), parameters, query);
            }
        }

        verboseBackendAccess(WGOperationKey.OP_QUERY, query);
        WGResultSetCore resultSetCore = this.core.query(type, query, parameters);

        if (!parameters.containsKey(WGDatabase.QUERYOPTION_RETURNQUERY)) {
            parameters.put(WGDatabase.QUERYOPTION_RETURNQUERY, query);
        }

        if (cachedResultSet.booleanValue() == true && getSessionContext().isCacheWritingEnabled()) {
            WGCachedResultSet resultSet = new WGCachedResultSet(resultSetCore);
            try {
                masterQueryCache.write(queryCacheKey, new QueryCacheEntry(resultSet, (String) parameters.get(WGDatabase.QUERYOPTION_RETURNQUERY)));
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception writing query cache on database '" + getDbReference() + "'", e);
            }
            return new WGStandardResultSet(this, resultSet, parameters, query);
        }
        else {
            return new WGStandardResultSet(this, resultSetCore, parameters, query);
        }

    }

    /**
     * Queries the database for specific features.
     * 
     * @param featureName
     *            The feature to query. Use Constants WGDatabase.FEATURE_...
     * @return true if the database implementation supports this feature, false
     *         otherwise
     */
    public boolean hasFeature(String featureName) {

        Boolean feature = (Boolean) this.features.get(featureName);
        if (feature == null) {
            
            if (featureName.equals(WGDatabase.FEATURE_SESSIONTOKEN)) {
                feature = (_authenticationModule != null && _authenticationModule.isGeneratesSessionToken());
            }
            else {
                feature = new Boolean(this.getCore().hasFeature(featureName));
            }
            this.features.put(featureName, feature);
        }

        return feature.booleanValue();
    }

    /**
     * Returns the roles of this db implementation, describing what documents
     * can be found in it.
     * 
     * @return List List of constants WGDatabase.ROLE_... <br/>
     *         WGDatabase.ROLE_CONTENT: Database contains content documents and
     *         related (WGContent, WGStructEntry, WGArea) <br/>
     *         WGDatabase.ROLE_DESIGN: Database contains design documents
     *         (WGTMLModule, WGCSSJSLibrary) <br/>
     *         WGDatabase.ROLE_REPOSITORY: Database contains file containers
     *         (WGFileContainer) <br/>
     *         WGDatabase.ROLE_USERPROFILES: Database contains user profiles
     *         (WGUserProfile) <br/>
     */
    public List getRoles() {
        return this.getCore().getRoles();
    }

    /**
     * Wraps a content core into a WGContent object
     * 
     * @param doc
     *            content core to wrap
     * @return WGContent
     * @throws WGAPIException
     */
    protected WGContent createContentObject(WGDocumentCore doc) throws WGAPIException {

        WGContent content = new WGContent(this, doc);
        mapDocumentObject(content);
        return content;

    }

    /**
     * Maps the document object on diverse document caches at the database For
     * content objects this method should only be called for new and modified
     * documents as it will remove the content from all user's private caches
     * 
     * @param doc
     * @throws WGAPIException
     */
    private void mapDocumentObject(WGDocument doc) throws WGAPIException {

        if (doc == null || doc.isTemporary() || doc.isDummy()) {
            return;
        }
        
        if (!getSessionContext().isCacheWritingEnabled()) {
            return;
        }
        
        if (!isDoctypeCacheable(doc.getType())) {
            return;
        }
        
        if (doc instanceof WGUserProfile) {
            return;
        }

        WGDocumentKey documentKey = doc.getDocumentKeyObj();
        updateCache(masterDocumentsByKey, String.valueOf(documentKey), doc);

        if (doc instanceof WGContent) {
            WGContent content = (WGContent) doc;            
            
            // Map uname from content
            String uniqueName = content.getUniqueName(); 
            if (!WGUtils.isEmpty(uniqueName)) {
                mapUniqueName(uniqueName, content);
            }
            
            // Map uname from struct
            uniqueName = content.getStructEntry().getUniqueName(); 
            if (!WGUtils.isEmpty(uniqueName)) {
                mapUniqueName(uniqueName, content);
            }
        }
        else if (doc instanceof WGStructEntry) {
            WGStructEntry struct = (WGStructEntry) doc;
            String uname = struct.getUniqueName();
            if (!WGUtils.isEmpty(uname)) {
                unmapUniqueName(uname);
            }
        }

    }

    /**
     * Adds a listener for backend changes to the database
     * @param listener
     */
    public void addBackendChangeListener(WGBackendChangeListener listener) {
        if (!this.backendChangeListeners.contains(listener)) {
            this.backendChangeListeners.add(listener);
        }
    }
    
    /**
     * Removes a listener for backend changes from the database
     * @param listener
     */
    public void removeBackendChangeListener(WGBackendChangeListener listener) {
        this.backendChangeListeners.remove(listener);
    }

    private void mapUniqueName(String uniqueName, WGContent content) throws WGAPIException {
        if (content.getStatus().equals(WGContent.STATUS_RELEASE)) {
            String uniqueNameKey = buildUniqueNameCacheKey(uniqueName, content.getLanguage().getName());
            updateCache(this.masterDocumentByName, uniqueNameKey, content.getDocumentKeyObj());
        }
    }
    
    private void unmapUniqueName(String uniqueName) throws WGAPIException {
        
        if (!getSessionContext().isCacheWritingEnabled()) {
            return;
        }
        
        try {
            this.masterDocumentByName.flush(uniqueName);
            for (WGLanguage lang : getLanguages().values()) {
                String uniqueNameKey = buildUniqueNameCacheKey(uniqueName, lang.getName());
                this.masterDocumentByName.flush(uniqueNameKey);
            }
        }
        catch (CacheException e) {
            throw new WGBackendException("Exception in cache maintenance", e);
        }

   }

    /**
     * Method to update cache maps with values. This method takes
     * {@link WGSessionContext#isCacheWritingEnabled()} into credit and refuses
     * to put the value in the cache if cache writing is disabled. In that case
     * it just clears the cache entry if the previous entry is not the same as
     * the entry to set.
     * 
     * @param map
     * @param key
     * @param value
     */
    private void updateCacheMap(Map map, Object key, Object value) {

        // In case the correct object is already in cache we do nothing
        if (map.get(key) == value) {
            return;
        }

        if (getSessionContext().isCacheWritingEnabled()) {
            map.put(key, value);
        }
        else {
            map.remove(key);
        }

    }
    
    private void updateCache(Cache cache, String key, Object value) {

        try {
            if (!getSessionContext().isCacheWritingEnabled()) {
                return;
            }
            
            // In case the correct object is already in cache we do nothing
            if (WGUtils.nullSafeEquals(cache.read(key), value)) {
                return;
            }
            
            cache.write(key, value);
        }
        catch (CacheException e) {
            WGFactory.getLogger().error("Exception updating master document cache", e);
        }

    }
    
    

    protected WGDocument unmapDocumentObject(WGDocumentKey docKey) throws WGAPIException {

        WGDocument oldDoc = null;
        try {
            oldDoc = (WGDocument) masterDocumentsByKey.read(docKey.toString());
            masterDocumentsByKey.flush(docKey.toString());
        }
        catch (CacheException e) {
            WGFactory.getLogger().error("Exception flushing entry on master document cache", e);
        }

        int docType = docKey.getDocType();
        if (isDesignDocumentType(docType)) {
            WGDocumentListCache designs = (WGDocumentListCache) this.designDocumentLists.get(docType);
            if (designs != null) {
                designs.remove(docKey);
            }
        }
        
        return oldDoc;
    }

    /**
     * Creates a new content document. This version can determine the version to
     * use and is mainly for cloning content.
     * 
     * @param entry
     *            struct Entry, that this new content document will be attached
     *            to
     * @param language
     *            Language of the new content
     * @param title
     *            Title of the new content
     * @param Integer
     *            version The needed version for the created document
     * @throws WGAPIException
     */
    protected WGContent createContent(WGStructEntry entry, WGLanguage language, String title, Integer version) throws WGAPIException {

        if (!this.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && (entry == null || language == null || title == null)) {
            throw new WGIllegalArgumentException("Mandatory parameter (Struct entry, language or title) is empty");
        }

        performContentCreationCheck(entry, language);

        // Evaluate new version number
        int newVersion = 0;
        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            if (version != null) {
                newVersion = version.intValue();
            }
            else {
                newVersion = findNewVersionNumber(entry, language);
            }

        }

        // Create core and pre-initialize to be a valid WGContent object
        WGDocumentCore docCore = this.getCore().createContent(entry, language, title, newVersion);
        if (docCore == null) {
            throw new WGCreationException("Unable to create content. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of content");
        }

        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            docCore.setMetaData(WGContent.META_STATUS, WGContent.STATUS_DRAFT);
        }

        // Create WGContent object
        WGContent newContent = this.createContentObject(docCore);

        // Initialize
        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            newContent.setVisible(true);
        }
        newContent.setTitle(title);
        newContent.setMetaData(WGContent.META_AUTHOR, getSessionContext().getUser());
        if (getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            newContent.setMetaData(WGContent.META_OWNER, getSessionContext().getUser());
        }
        



        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            
            // Create predefined item/meta fields based on schema definition
            WGContentTypeDefinition schemaDef = (WGContentTypeDefinition) entry.getContentType().getSchemaDefinition();
            if (schemaDef != null) {
                for (WGContentItemDefinition itemDef : schemaDef.getContentItemDefinitions()) {
                    if (itemDef.hasInitialValue()) {
                        newContent.setItemValue(itemDef.getName(), itemDef.getInitialValue());
                    }
                }
                for (WGMetaFieldDefinition metaDef : schemaDef.getContentMetaDefinitions()) {
                    newContent.setMetaData(metaDef.getName(), metaDef.getValues());
                }
            }

        }


        // Event contentCreated
        String contentType = null;
        if (entry != null && entry.getContentType() != null) {
            contentType = entry.getContentType().getName();
        }
        WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_CREATED, newContent.getDocumentKey(), contentType, this);
        event.setContent(newContent);
        fireContentEvent(event);

        // return
        return newContent;

    }

    /**
     * Controls if the current user may create a content document with the given data. If so the method exists normally. If not an exception is thrown.
     * @param entry The struct entry under which to create the content. If given null all struct entry data checks are bypassed.
     * @param language The language in which to create it
     * @throws WGAPIException If the user may not create the document. The exception informs about the reason.
     */
    public void performContentCreationCheck(WGStructEntry entry, WGLanguage language) throws WGAPIException, ResourceIsLockedException, WGAuthorisationException, WGIllegalStateException {
        // check entry lock
        if (entry != null && entry.getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Unable to create content. Given structentry is locked.");
        }

        if (getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_AUTHOR) {
            throw new WGAuthorisationException("You are not authorized to create content in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
        }

        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && entry != null && !entry.isSaved()) {
            throw new WGIllegalStateException("Struct entry for this content has not yet been saved");
        }

        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && !language.isSaved()) {
            throw new WGIllegalStateException("Language for this content has not yet been saved");
        }
       
        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && language.getDatabase() != this) {
            throw new WGIllegalDataException("The language definition is from a different database");
        }
       
        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && entry != null) {

            // Test if draft content allready exists for this language code in
            // status DRAFT
        	if (!getSessionContext().isMasterSession() && entry != null && entry.hasContent(language.getName(), WGContent.STATUS_DRAFT)) {
                throw new WGIllegalStateException("There is already an existing draft copy in language " + language.getTitle() + ".", WGIllegalStateException.ERRORCODE_CONTENT_DRAFT_EXISTS);
            }

        	// Ask PageRightsFilter first end stop other checks if ALLOWED_SKIP_DEFAULT_CHECKS
    		Right right = getPageRightsFilter().mayEditContent(entry, getSessionContext().getUserAccess(), language);
    		if (right == Right.ALLOWED_SKIP_DEFAULT_CHECKS)
    			return;				// cancel all other tests
    		else if (right == Right.DENIED)
    			throw new WGAuthorisationException("PageRightsFilter denies to edit content in this language", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGERIGHTSFILTER, language);            

        	// does contenttype allow usage:
        	WGContentType contentType = entry.getContentType();
            if (contentType != null && !contentType.mayCreateContent()) {
                throw new WGAuthorisationException("User is not allowed to use this content type", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, contentType);
            }
            else if (contentType == null && getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
                throw new WGIllegalDataException("The page has no content type");
            }

            if (!language.mayCreateContent()) {
                throw new WGAuthorisationException("User is not allowed to create content in this language", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_LANGUAGE, language);
            }
            
        	WGDocument restrictionDoc= entry.testEditPageHierarchyRights();
            if (restrictionDoc != null) {
                String code = (restrictionDoc instanceof WGArea ? WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA : WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE); 
                throw new WGAuthorisationException("User is not allowed to create content under this entry", code, restrictionDoc);
            }        	
       	
        }

    }

    private int findNewVersionNumber(WGStructEntry entry, WGLanguage language) throws WGAPIException {

        FreeContentVersionFinder finder = new FreeContentVersionFinder(entry, language);
        return finder.findNewVersion();


    }

    /**
     * Creates a new content document. This method flavor is meant for
     * "complete" content stores with struct entry hierarchy.
     * 
     * @param entry
     *            The struct entry for the new content
     * @param language
     *            The language of the new content
     * @param title
     *            The title of the new content
     * @return The newly created content document.
     * @throws WGAPIException
     */
    public WGContent createContent(WGStructEntry entry, WGLanguage language, String title) throws WGAPIException {
        return createContent(entry, language, title, null);
    }

    /**
     * Creates a simple content document with the content key given. This method
     * flavor is meant for content stores without struct entry hierarchy, like
     * those derived from the SimpleContentSource-Class
     * 
     * @param area
     *            The area to create the content in.
     * @param key
     *            The key of the new content.
     * @param title
     *            The title of the new content.
     * @return The newly created content.
     * @throws WGAPIException
     */
    public WGContent createContent(WGArea area, Object key, String title) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            throw new WGNotSupportedException("This creation method is not available with this database implementation, because this implementation needs more information to create a content.");
        }

        if (area == null) {
            throw new WGIllegalArgumentException("There was no area provided");
        }

        WGStructEntry entry = createStructEntry(key, area, null, title);
        entry.save();

        return this.createContent(entry, null, title);

    }

    /**
     * Equivalent to calling createStructKey(null, parent, contentType, title).
     * 
     * @param parent
     * @param contentType
     * @param title
     * @throws WGAPIException
     */
    public WGStructEntry createStructEntry(WGDocument parent, WGContentType contentType, String title) throws WGAPIException {
        return createStructEntry(null, parent, contentType, title);
    }

    /**
     * Creates a new struct entry
     * 
     * @param key
     *            The struct key of the new entry. Can be specified when
     *            database supports feature
     *            WGDatabase.FEATURE_ACCEPTS_STRUCTKEYS. Can be left out with
     *            null, when the database supports feature
     *            WGDatabase.FEATURE_GENERATES_STRUCTKEYS.
     * @param parent
     *            Parent object. Can be a WGArea object (new entry will be root)
     *            or another WGStructEntry object (new entry will be child)
     * @param contentType
     *            Content type for contents of this struct entry
     * @param title
     *            Title of this struct entry
     * @return WGStructEntry
     * @throws WGAPIException
     */
    public WGStructEntry createStructEntry(Object key, WGDocument parent, WGContentType contentType, String title) throws WGAPIException {

        if (!this.isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (parent == null || title == null) {
            throw new WGIllegalArgumentException("Mandatory parameter parent or title is empty");
        }

        performStructCreationCheck(key, parent, contentType);

        WGDocumentCore entryCore = this.getCore().createStructEntry(key, parent, contentType);
        if (entryCore == null) {
            throw new WGCreationException("Unable to create struct entry. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of struct entries");
        }
        WGStructEntry newEntry = this.getOrCreateStructEntryObject(entryCore, new WGDocumentObjectFlags());

        // Initialize
        newEntry.setMetaData(WGStructEntry.META_TITLE, title);

        // DISABLED: Area is set inside DatabaseCore.createStructEntry
        // WGArea area = (parent instanceof WGArea ? (WGArea) parent :
        // ((WGStructEntry) parent).getArea());
        // newEntry.setMetaData(WGStructEntry.META_AREA,
        // (hasFeature(FEATURE_USE_OBJECTS_AS_REFERENCES) ? area.getCore() :
        // (Object) area.getName()));

        newEntry.setMetaData(WGStructEntry.META_POSITION, new Integer(0));
        if (contentType != null) {
            newEntry.setMetaData(WGStructEntry.META_CONTENTTYPE, (hasFeature(FEATURE_USE_OBJECTS_AS_REFERENCES) ? contentType.getCore() : (Object) contentType.getName()));
        }

        // Save and return
        // newEntry.save();
        return newEntry;

    }

    /**
     * Controls if the current user may create a struct entry with the given data. If so the method exists normally. If not an exception is thrown.
     * @param key A struct key that is to be determined for the new struct entry. Give null if you want the WGAPI to generate the key.
     * @param parent The parent of the new struct entry. May be an {@link WGArea} or another {@link WGStructEntry}.
     * @param contentType The content type of the new struct entry.
     * @throws WGAPIException If the user may not create the document. The exception informs about the reason.
     */
    public void performStructCreationCheck(Object key, WGDocument parent, WGContentType contentType) throws WGAPIException {

        
        if (parent == null) {
            throw new WGIllegalDataException("No parent document was given");
        }
        
        if (parent.getDatabase() != this) {
            throw new WGIllegalDataException("The parent document is from a different database");
        }
        
        // check parent lock
        if (parent.getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Unable to create structentry. Parent (structentry or area) is locked.");
        }

        if (!parent.isSaved()) {
            throw new WGIllegalStateException("Parent object of this struct entry (either struct entry or area) has not yet been saved");
        }

        if (hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && contentType == null) {
            throw new WGIllegalArgumentException("You did not provide a content type which is mandatory for this database type");
        }
        
        if (!(parent instanceof WGArea || parent instanceof WGStructEntry)) {
            throw new WGIllegalArgumentException("parent parameter of invalid document type: " + (parent != null ? parent.getClass().getName() : "null"));
        }
        
        WGArea targetArea = (parent instanceof WGArea ? (WGArea) parent : ((WGStructEntry) parent).getArea());
        if (contentType != null) {
           
            if (!contentType.isSaved()) {
                throw new WGIllegalStateException("Content type for this struct entry has not yet been saved");
            }
            if (contentType.getDatabase() != this) {
                throw new WGIllegalDataException("The content type is from a different database");
            }
            
            if (!targetArea.isSystemArea() && !contentType.mayCreateChildEntry(parent)) {
                throw new WGAuthorisationException("User is not allowed to use this content type at this position", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, contentType);
            }
                
        }

        if (getSessionContext().getAccessLevel() < ACCESSLEVEL_AUTHOR) {
            throw new WGAuthorisationException("You are not authorized to create struct entries in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
        }

        boolean isRoot = (parent instanceof WGArea);
        if (isRoot) {
            WGArea parentArea = (WGArea) parent;
            if (!parentArea.mayEditPages()) {
                throw new WGAuthorisationException("User is not allowed to edit entries in this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, parent);
            }
            if (contentType != null) {
                parentArea.testRootPageRestrictions(contentType);
            }
        }
        else {
            WGStructEntry parentStruct = ((WGStructEntry) parent);
            WGDocument restrictingDoc = parentStruct.testEditChildPagesHierarchyRights();
            if (restrictingDoc != null) {
                String errorCode = (restrictingDoc instanceof WGArea ? WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA : WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE);
                throw new WGAuthorisationException("User is not allowed to edit child entries under this parent entry", errorCode, (WGStructEntry) restrictingDoc);
            }
            if (contentType != null) {
                parentStruct.testChildPageRestrictions(contentType);
            }
        }



        if (!hasFeature(FEATURE_ACCEPTS_STRUCTKEYS)) {
            if (key != null) {
                throw new WGCreationException("You provided a struct key, but this implementation does not accept struct keys from the creator");
            }
        }

        if (!hasFeature(FEATURE_GENERATES_STRUCTKEYS)) {
            if (key == null) {
                throw new WGCreationException("You did not provide a struct key, but this implementation cannot generate struct keys itself");
            }

            WGStructEntry otherEntry = getStructEntryByKey(key);
            if (otherEntry != null) {
                throw new WGDuplicateKeyException("There is already a struct entry under the given key '" + String.valueOf(key) + "'");
            }
        }
    }

    /**
     * Returns a WGContent object for the given content core by either
     * retrieving an existing or creating one
     * 
     * @param doc
     *            The content core
     * @return WGContent
     * @throws WGAPIException
     */
    protected WGContent getOrCreateContentObject(WGDocumentCore doc) throws WGAPIException {

        if (doc == null) {
            return null;
        }

        WGDocumentKey key = WGDocument.buildDocumentKey(doc, this);
        WGContent content;
        try {
            content = (WGContent) getDocumentByDocumentKeyFromCache(key, false);
        }
        catch (WGDocumentDoesNotExistException e) {
            content = null;
        }

        if (content == null) {
            content = this.createContentObject(doc);
        }
        else if (!content.isTemporary()) {
            if (!content.isEdited()) {
                content.setCore(doc);
            }
        }
        return content;

    }

    protected WGDesignDocument getOrCreateDesignDocumentObject(WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {

        WGDocumentKey key = WGDocument.buildDocumentKey(doc, this);
        WGDesignDocument design;
        try {
            design = (WGDesignDocument) getDocumentByDocumentKeyFromCache(key, false);
        }
        catch (WGDocumentDoesNotExistException e) {
            design = null;
        }

        if (design == null) {
            design = createDesignDocumentObject(doc, flags);
        }
        else {
            if (!design.isEdited()) {
                design.setCore(doc);
            }
        }
        return design;

    }

    /**
     * Wraps a user profile core into a WGUserProfile object.
     * 
     * @param doc
     *            core to wrap
     * @return WGUserProfile
     * @throws WGAPIException
     */
    private WGUserProfile createUserProfileObject(WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {

        WGUserProfile profile = new WGUserProfile(this, doc, flags);
        return profile;
    }

    /**
     * Returns date, where data in the database was last changed. This function
     * may not be supported by some implementations (Feature
     * WGDatabase.FEATURE_LASTCHANGED) and return new Date(Long.MIN_VALUE).
     * 
     * @return Date
     * @throws WGAPIException 
     */
    public Date getLastChanged() {
        return _revisionDate;
    }
    
    /**
     * Returns the revision state of the database. The return type depends on the backend database, but is normally either {@link Long} or {@link Date}.
     * The revision here is the state that the database and all its caches currently safely represents. The actual revision on the backend may be newer.
     * and can be retrieved using {@link #getBackendRevision()}.
     * You can use {@link #getRevisionObject()} instead which provides a high-level object easier to compare to other revisions and to serialize/deserialize.
     * To get dates corresponding to the given revision use {@link #getRevisionDate(Comparable)}.
     * To directly retrieve concrete dates use {@link #getLastChanged()} instead.
     */
    public Comparable getRevision() {
        return _revision.getRevisionValue();
    }
    
    /**
     * Returns the revision state of the database backend. This is the most up-to-date revision including all most recent changes, but may not yet be reflected by all
     * caches of the WGAPI.
     * @throws WGAPIException
     */
    public Comparable getBackendRevision() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getCore().getRevision();
        
    }
    
    /**
     * Returns the revision state of the database as an high level object, usable for revision comparison and storage independently from the concrete backend type of revision value
     * Just like {@link #getRevision()} this returns the revision state of the database that is safely represented by all caches.
     */
    public WGDatabaseRevision getRevisionObject() {
        return _revision;
    }
    
    /**
     * Retrieve the datetime where a given database revision was done.
     * The indicators given as parameter must have been retrieved by {@link #getRevision()}.
     * If the revision is of wrong datatype or is no revision known in this database the method throws a WGWrongRevisionExcception.
     * @param lastModified The revision indicator
     * @return The date that the given revision was done
     * @throws WGAPIException
     * @throws WGWrongRevisionException if the given revision is no revision of the current database
     */
    public Date getRevisionDate(Comparable lastModified) throws WGAPIException, WGWrongRevisionException {
        if (lastModified instanceof WGDatabaseRevision) {
            return getCore().getRevisionDate(((WGDatabaseRevision) lastModified).getRevisionValue());
        }
        else {
            return getCore().getRevisionDate(lastModified);
        }
    }

    /**
     * Returns the map of creation options provided to this database when it was
     * initially opened
     * 
     * @return Map
     */
    public Map getCreationOptions() {

        return this.creationOptions;

    }

    /**
     * Returns a map of WGLanguage objects, mapped by their language name (code)
     * 
     * @return Map
     * @throws WGAPIException
     */
    public Map<String,WGLanguage> getLanguages() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        List languageList = this.getDesignObjects(WGDocument.TYPE_LANGUAGE);
        if (languageList == null) {
            return null;
        }

        Iterator languageIterator = languageList.iterator();

        HashMap languages = new HashMap<String,WGLanguage>();
        WGLanguage language = null;

        while (languageIterator.hasNext()) {
            language = (WGLanguage) languageIterator.next();
            languages.put(language.getName(), language);
        }

        return languages;

    }

    /**
     * Returns the session context, providing all data about the currently
     * opened database session. Null if no session is open.
     * 
     * @return WGSessionContext
     */
    public WGSessionContext getSessionContext() {

        return (WGSessionContext) this.sessionContext.get();
    }

    /**
     * Clears all document caches.
     */
    private synchronized void clearDocumentMappings() {

        try {
            this.masterDocumentsByKey.flushAll();
            this.masterDocumentByName.flushAll();
            this.masterQueryCache.flushAll();
            this.masterDocumentCaches.clear();
        }
        catch (CacheException e) {
            WGFactory.getLogger().error("Exception clearing master document cache", e);
        }
        this.designDocumentLists.clear();

    }

    /**
     * Completely closes this database object. This object and all child objects
     * must not be used after calling this.
     * 
     * @throws WGAPIException
     */
    public void close() throws WGAPIException {

        WGFactory.getInstance().removeDatabase(this);
        ready = false;

        if (isSessionOpen()) {
            this.closeSession();
        }
        
        clearDocumentMappings();
        try {
            this.masterDocumentsByKey.destroy();
            this.masterDocumentByName.destroy();
            this.masterQueryCache.destroy();
        }
        catch (CacheException e) {
            WGFactory.getLogger().error("Exception closing master document cache");
        }

        if (this.designProvider != null) {
            this.designProvider.removeDesignChangeListener(this);
            this.designProvider.dispose();
            this.designProvider = null;
        }

        if (isConnected()) {
            core.close();
            core = null;
        }
        
        closeAuthenticationModule();

        // Cannot release db listeners here, bc. the closing might be issued by
        // a fired event (e.g. connection error), and this would result in a
        // concurrent
        // modification.
        // databaseConnectListeners.clear();

    }
    
    private void closeAuthenticationModule() {
        if (_authenticationModule != null) {
            _authenticationModule.removeAuthenticationSourceListener(this);
            _authenticationModule.destroy();
        }
    }

    /**
     * Retrieves a dummy content object. Should support the native expression
     * language of the database, if there is one.
     * 
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent getDummyContent(String language) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (language == null) {
            language = getDefaultLanguage();
        }

        if (hasFeature(FEATURE_MULTILANGUAGE)) {
            WGLanguage lang = getLanguage(language);
            if (lang == null) {
                throw new WGIllegalArgumentException("Language '" + language + "' is not defined");
            }
        }

        return new WGContent(this, this.getCore().getDummyContent(language), new WGDocumentObjectFlags().setDummy(true));
    }

    /**
     * Returns a dummy profile object that does not store information.
     * 
     * @param name
     *            Profile name that the dummy profile should return
     * @throws WGAPIException
     */
    public WGUserProfile getDummyProfile(String name) throws WGAPIException {
        return createUserProfile(name, Constants.PERSMODE_SESSION, true, new WGDocumentObjectFlags().setDummy(true));
    }

    /**
     * Parses the string representation of a struct key (e.g. used in a URL) to
     * the real internal struct key format
     * 
     * @param key
     * @return The struct key object. Type varies by database implementation.
     * @throws WGAPIException
     */
    public Object parseStructKey(String key) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return this.getCore().parseStructKey(key);
    }

    /**
     * Return a content document by content key
     * 
     * @param key
     *            Key to retrieve content for
     * @return WGContent the found content, if none found null
     * @throws WGAPIException
     */
    public WGContent getContentByKey(WGContentKey key) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (key == null) {
            return null;
        }

        WGContent content = null;

        // A released version is needed. Find via struct entry
        if (key.getVersion() == 0) {
            WGStructEntry entry = getStructEntryByKey(key.getStructKey());
            if (entry == null) {
                return null;
            }

            return entry.getReleasedContent(key.getLanguage());
        }

        // A certain version is needed. Find via document key
        else {
            return (WGContent) getDocumentByKey(key.toDocumentKey());
        }
    }

    private WGContent retrieveContentByKey(WGContentKey key) throws WGAPIException {

        WGContent content = null;

        WGDocumentCore contentCore = this.getCore().getContentByKey(key);
        if (contentCore != null && !contentCore.isDeleted()) {
            content = this.getOrCreateContentObject(contentCore);
        }
        else {
            mapNonExistentDocIndicator(this.masterDocumentsByKey, key.toDocumentKey().toString(), WGDocument.TYPE_CONTENT);
        }

        return content;

    }

    protected void mapNonExistentDocIndicator(Map cacheMap, Object cacheKey, int docType) {

        if (getSessionContext().isCacheWritingEnabled() && isDoctypeCacheable(docType)) {
            cacheMap.put(cacheKey, new NullPlaceHolder());
        }

    }
    
    protected void mapNonExistentDocIndicator(Cache cache, String cacheKey, int docType) {

        if (getSessionContext().isCacheWritingEnabled() && isDoctypeCacheable(docType)) {
            try {
                cache.writeEntry(cacheKey, new NullPlaceHolder());
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception mapping NDI to master document cache", e);
            }
        }

    }

    /**
     * Return a content document by content key string.
     * 
     * @param keyStr
     *            The content key in its string representation
     * @return The content with that content key or null if it does not exist
     * @throws WGAPIException
     */
    public WGContent getContentByKey(String keyStr) throws WGAPIException {
        WGContentKey contentKey = WGContentKey.parse(keyStr, this);
        if (contentKey != null) {
            return getContentByKey(contentKey);
        }
        else {
            throw new WGIllegalArgumentException("Invalid content key string: " + keyStr);
        }
    }

    /**
     * Fetches a document from any WGDatabase document cache. If the cache entry
     * contains an object that is no document this is taken as indicator that
     * the document does not exist (It already was tried to retrieve it but that
     * failed). To let the calling code differ between empty cache and
     * nonexistent document the method throws a WGDocumentDoesNotExistException
     * in that case.
     * This variant will not return results while session caching is disabled.
     * 
     * @param cache
     *            The cache map to use
     * @param key
     *            The cache key
     * @return The cached document. null if the cache entry is empty (not yet
     *         cached)
     * @throws WGAPIException 
     * @throws WGDocumentDoesNotExistException if the document is marked in cache as nonexistent
     * @throws WGAPIException
     */
    protected WGDocument fetchDocumentFromCache(Object cache, Object key) throws WGAPIException {
        return fetchDocumentFromCache(cache, key, false);
    }

    /**
     * Fetches a document from any WGDatabase document cache. If the cache entry
     * contains an object that is no document this is taken as indicator that
     * the document does not exist (It already was tried to retrieve it but that
     * failed). To let the calling code differ between empty cache and
     * nonexistent document the method throws a WGDocumentDoesNotExistException
     * in that case.
     * 
     * @param cache
     *            The cache map to use
     * @param key
     *            The cache key
     * @param internal call Specify true for internal API calls where the session caching setting should have no effect.
     * @return The cached document. null if the cache entry is empty (not yet
     *         cached)
     * @throws WGAPIException 
     * @throws WGDocumentDoesNotExistException if the document is marked in cache as nonexistent
     * @throws WGAPIException
         */
    protected WGDocument fetchDocumentFromCache(Object cache, Object key, boolean internalCall) throws WGAPIException {


        if (!internalCall && !getSessionContext().isCachingEnabled()) {
            return null; 
        }

        Object cacheContent = null;
        if (cache instanceof Map) {
            cacheContent = ((Map) cache).get(key);
        }
        else if (cache instanceof Cache) {
            try {
                cacheContent = ((Cache) cache).read(String.valueOf(key));
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception reading master document cache", e);
            }
        }
        else {
            throw new WGIllegalArgumentException("Illegal cache object type " + cache.getClass().getName());
        }

        // Direct cache hit (normally only masterDocumentsByKey): Return doc if not read protected now
        if (cacheContent instanceof WGDocument) {
            WGDocument doc = (WGDocument) cacheContent;
            
            try {
                if (!doc.isReadableForUser()) {
                    throw new WGDocumentDoesNotExistException();
                }
            }
            catch (WGDeletedException e) {
                if (internalCall) {
                    return doc;
                }
                else {
                    return null;
                }
            }
            
            return doc;
        }

        // Indirect cache hit: Lookup document by its document key in master
        // cache
        else if (cacheContent instanceof WGDocumentKey) {
            return fetchDocumentFromCache(masterDocumentsByKey, (WGDocumentKey) cacheContent);
        }

        // Cache indicating document does not exist: Throw exception
        else if (cacheContent instanceof NullPlaceHolder) {
            throw new WGDocumentDoesNotExistException();
        }

        // Cache indication document is unknown. We must retrieve it from
        // backend.
        else {
            return null;
        }

    }

    /**
     * Returns a document by the given document key.
     * 
     * @param key
     *            The document key for the document in its string representation
     * @return The document of that key, null if there is none
     * @throws WGAPIException
     * @deprecated Use {@link #getDocumentByKey(String)} instead
     */
    public WGDocument getDocumentByDocumentKey(String key) throws WGAPIException {
        WGDocumentKey documentKey = new WGDocumentKey(key);
        return getDocumentByKey(documentKey);
    }

    /**
     * Returns a document by the given document key.
     * 
     * @param documentKey
     *            The document key for the document.
     * @return The document of that key, null if there is none
     * @throws WGAPIException
     */
    public WGDocument getDocumentByKey(WGDocumentKey documentKey) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (!documentKey.isRealDocumentKey()) {
            throw new IllegalArgumentException("The document key '" + documentKey + "' does not belong to a real WGAPI document");
        }
        
        if (hasFeature(FEATURE_DISABLE_META_LOWERCASING)) {
            documentKey.useProperCaseNames();
        }

        try {
            // Check cache once unsynchronized, then synchronized
            WGDocument doc = getDocumentByDocumentKeyFromCache(documentKey, true);
            if (doc == null) {
                
                // Double-checked locking (some doctypes may choose not to)
                if (isSynchronizeAccessForDoctype(documentKey.getDocType())) {
                    WGOperationKey op = obtainOperationKey(WGOperationKey.OP_DOCUMENT_BY_KEY, documentKey);
                    synchronized (op) {
                        try {
                            op.setUsed(true);
                            doc = getDocumentByDocumentKeyFromCache(documentKey, true);
    
                            if (doc == null) {
                                verboseBackendAccess(WGOperationKey.OP_DOCUMENT_BY_KEY, documentKey);
                                doc = retrieveDocumentByKey(documentKey);
                            }
                        }
                        finally {
                            op.setUsed(false);
                        }
                    }
                    
                }
                else {
                    verboseBackendAccess(WGOperationKey.OP_DOCUMENT_BY_KEY, documentKey);
                    doc = retrieveDocumentByKey(documentKey);
                }
            }
            
            return doc;
        }
        catch (WGDocumentDoesNotExistException e) {
            return null;
        }

    }

    private boolean isSynchronizeAccessForDoctype(int docType) {

        WGDesignProvider designProvider = getDesignProvider();
        if (designProvider == null) {
            return true;
        }
        
        if (designProvider.providesType(docType)) {
            return designProvider.isSynchronizeAccess();
        }
        
        return true;
        
    }

    private WGDocument retrieveDocumentByKey(WGDocumentKey documentKey) throws WGAPIException {
        WGDocument doc;
        int docType = documentKey.getDocType();
        switch (docType) {
            case WGDocument.TYPE_CONTENT:
                WGContentKey contentKey = WGContentKey.parse(documentKey.getName(), this);
                doc = retrieveContentByKey(contentKey);
                break;

            case WGDocument.TYPE_USERPROFILE:
                doc = retrieveUserProfile(documentKey.getName());
                break;

            case WGDocument.TYPE_STRUCTENTRY:
                Object structKey = parseStructKey(documentKey.getName());
                doc = retrieveStructEntry(structKey);
                break;

            default:
                doc = retrieveDesignDocument(docType, documentKey.getName(), documentKey.getMediakey());
                break;
        }
        
        if (doc != null && doc.isReadableForUser()) {
            return doc;
        }
        else {
            throw new WGDocumentDoesNotExistException();
        }
        
    }
    
    /**
     * Returns a document by the given document key.
     * 
     * @param key
     *            The document key for the document in its string representation
     * @return The document of that key, null if there is none
     * @throws WGAPIException
     */
    public WGDocument getDocumentByKey(String key) throws WGAPIException {
        WGDocumentKey documentKey = new WGDocumentKey(key);
        return getDocumentByKey(documentKey);
    }

    /**
     * Returns the parent entry to the given struct entry
     * 
     * @param entry
     *            The entry, whose parent is searched
     * @return WGStructEntry
     * @throws WGAPIException
     */
    public WGStructEntry getParentEntry(WGStructEntry entry) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (entry == null || entry.isDeleted() || entry.isDummy()) {
            return null;
        }

        WGDocumentCore parentCore = this.getCore().getParentEntry(entry);
        if (parentCore != null && !parentCore.isDeleted()) {
            return getOrCreateStructEntryObject(parentCore, new WGDocumentObjectFlags());
        }
        else {
            return null;
        }

    }

    /**
     * Retrieves the value of an extension data field stored in this database.
     * Extension data fields are custom data that are persistently stored to an entity, in this case the database itself, and which may have custom purpose.
     * This feature is only available in WGA Content Stores of Version 5 or higher.
     * @param name The name of the field.
     * @return The value of the requested field for this database core. 
     * @throws WGAPIException 
     */

    public Object getExtensionData(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException("Database metadata", WGDatabase.CSVERSION_WGA5);
        }

        return getCore().getExtensionData(name);
    }
    
    /**
     * Returns the names of stored extension data fields on this database
     * This feature is only available in WGA Content Stores of Version 5 or higher.
     * @return List of field mnames
     * @throws WGAPIException
     */
    public List<String> getExtensionDataNames() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException("Database metadata", WGDatabase.CSVERSION_WGA5);
        }

        return getCore().getExtensionDataNames();
    }
    
    /**
     * Writes a extension data field. The data is immediately stored.
     * Extension data fields are custom data that are persistently stored to an entity, in this case the database itself, and which may have custom purpose.
     * This feature is only available in WGA Content Stores of Version 5 or higher.
     * @param name Name of the field
     * @param value The value to store
     * @throws WGAPIException
     */
    public void writeExtensionData(String name, Object value) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException("Database metadata", WGDatabase.CSVERSION_WGA5);
        }
        
        if (getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
            throw new WGAuthorisationException("Only managers and chief editors can modify database metadata", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
        }

        getCore().writeExtensionData(name, value);
    }
    
    /**
     * Removes a extension data field from the database. The removal is immediately commited.
     * This feature is only available in WGA Content Stores of Version 5 or higher.
     * @param name Name of the field to remove
     * @throws WGAPIException
     */
    public void removeExtensionData(String name) throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException("Database metadata", WGDatabase.CSVERSION_WGA5);
        }
        
        if (getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
            throw new WGAuthorisationException("Only managers and chief editors can modify database metadata", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
        }


        getCore().removeExtensionData(name);
        
    }

    /**
     * Returns the default language of this database
     * 
     * @return String
     * @throws WGAPIException
     */
    public String getDefaultLanguage() throws WGAPIException {
        
        if (_defaultLanguage == null) {
            _defaultLanguage = Locale.getDefault().getLanguage();
        }
        
        return _defaultLanguage;
    }

    /**
     * Trigger the determination of a default language.
     * This method tries to find a reasonable safe language to use as default language with limited effort.
     * It does not take into account which language is used the most but will take a language with root contents available.
     * @throws WGAPIException
     */
    public synchronized void determineDefaultLanguage() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        // Look for root content languages 
        WGContent content = null;
        Map langs = getLanguages();
        Iterator langsIt = getLanguages().values().iterator();
        while (langsIt.hasNext()) {
            WGLanguage lang = (WGLanguage) langsIt.next();
            content = getFirstReleasedContent(lang.getName(), true);
            if (content != null) {
                break;
            }
        }
        
        // If we found a root content in some language we take it as default language
        if (content != null) {
            _defaultLanguage = content.getLanguage().getName();
            return;
        }
        
        // If langs defined: Use the one matching the platform default
        // or the "first best" when no definition matches
        if (langs.size() > 0) {
            WGLanguage lang = getLanguageForLocale(Locale.getDefault());
            if (lang == null) {
                lang = (WGLanguage) langs.values().iterator().next();
            }
            _defaultLanguage = lang.getName();
            return;
        }

        // No langs defined: Use platform default language
        else {
            _defaultLanguage = Locale.getDefault().getLanguage();
        }
        
        _defaultLanguageAutoDetermined  = true;
    }

    /**
     * Returns the name of the database server (if there is one).
     * 
     * @return String
     * @throws WGAPIException
     */
    public String getServerName() throws WGAPIException {
        return getCore().getServerName();
    }

    /**
     * Returns, if the database is ready for work.
     * 
     * @return Returns a boolean
     */
    public boolean isReady() {
        return ready;
    }

    /**
     * Returns the core implementation object of this database.
     * 
     * @return WGDatabaseCore
     */
    public WGDatabaseCore getCore() {
        return core;
    }
    
    /**
     * Returns the personalisation core implementation of this database, if it is a personalisation database.
     * @throws WGNotSupportedException 
     * @throws WGNotSupportedException If the database is no personalisation database
     */
    public WGPersonalisationDatabaseCore getPersonalisationCore() throws WGNotSupportedException {
        
        WGDatabaseCore theCore = getCore();
        if (theCore instanceof WGPersonalisationDatabaseCore) {
            return (WGPersonalisationDatabaseCore) theCore;
        }
        else {
            throw new WGNotSupportedException("This database is no personalisation database");
        }
        
        
    }

    /**
     * Indicates if a database session is open
     */
    public boolean isSessionOpen() {
        return (this.getSessionContext() != null);
    }

    /**
     * Returns the database path, given as parameter of the method
     * WGDatabase.open()
     * 
     * @return String
     */
    public String getPath() {
        return path;
    }

    /**
     * Returns the first released content document of this database in the
     * specified language. It is up to the db implementation to decide, which
     * document is regarded "the first" of the database.
     * 
     * @param language
     *            The language of the content
     * @param onlyRoots
     *            Specify true, if you want to get only root documents
     * @return WGContent The content, null if there is no content of that
     *         language.
     * @throws WGAPIException
     */
    public WGContent getFirstReleasedContent(final String language, boolean onlyRoots) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        WGLanguageChooser chooser = new WGLanguageChooser() {

            public WGContent selectContentForName(WGDatabase db, String name, boolean isBI) throws WGAPIException {
                return db.getContentByName(name, language);
            }

            public WGContent selectContentForPage(WGStructEntry page, boolean isBI) throws WGAPIException {
                return page.getReleasedContent(language);
            }

            public WGLanguage selectDatabaseLanguage(WGDatabase db) throws WGAPIException {
                return db.getLanguage(language);
            }

            public WGContentKey chooseQueryContent(WGDatabase db, Map<String,WGContentKey> contents) throws WGAPIException {
                return contents.get(language);
            }

            public List<WGLanguage> getQueryLanguages(WGDatabase db) throws WGAPIException {
                return Collections.singletonList(db.getLanguage(language));
            }
            
            @Override
            public List<WGContent> selectContentPriorityOrder(WGStructEntry page, boolean isBI) throws WGAPIException {
                return null; // Not used
            }
            
        };
        
        Iterator areas = this.getAreas().iterator();
        while (areas.hasNext()) {
            WGArea area = (WGArea) areas.next();
            if (area.isSystemArea()) {
                continue;
            }
            
            Iterator rootEntries = area.getRootEntryIterator(10);
            while (rootEntries.hasNext()) {
                WGContent content = this.getFirstReleasedContent((WGStructEntry) rootEntries.next(), chooser, onlyRoots);
                if (content != null) {
                    return content;
                }
            }
        }

        return null;

    }
    
    /**
     * Returns the first released content document of this database. 
     * It is up to the db implementation to decide, which document is regarded "the first" of the database.
     * 
     * @param chooser
     *            A language chooser object determining what language versions may be used
     * @param onlyRoots
     *            Specify true, if you want to get only root documents
     * @return WGContent The content, null if there is no content of that
     *         language.
     * @throws WGAPIException
     */
    public WGContent getFirstReleasedContent(WGLanguageChooser chooser, boolean onlyRoots) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        Iterator areas = this.getAreas().iterator();
        while (areas.hasNext()) {
            WGArea area = (WGArea) areas.next();
            if (area.isSystemArea()) {
                continue;
            }
            
            Iterator rootEntries = area.getRootEntryIterator(10);
            while (rootEntries.hasNext()) {
                WGContent content = getFirstReleasedContent((WGStructEntry) rootEntries.next(), chooser, onlyRoots);
                if (content != null) {
                    return content;
                }
            }
        }

        return null;

    }

    /**
     * Returns the first released content of the given language to be found in
     * this database.
     * 
     * @param language
     * @throws WGAPIException
     */
    public WGContent getFirstReleasedContent(String language) throws WGAPIException {
        return getFirstReleasedContent(language, false);
    }

    /**
     * Returns the first released content in a given language, that is connected
     * to the given struct entry or to his descendant entries.
     * 
     * @param entry
     *            The reference entry. Content will be attached to this one or
     *            to a descendant entry of it.
     * @param language
     *            Language of the needed content
     * @param onlyRoots
     *            Specify true, if you only want root documents returned
     * @return WGContent The content, null if there was no content of this
     *         language found.
     * @throws WGAPIException
     */
    private WGContent getFirstReleasedContent(WGStructEntry entry, WGLanguageChooser chooser, boolean onlyRoots) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (entry == null || entry.isDeleted() || entry.isDummy()) {
            return null;
        }

        WGContent content = chooser.selectContentForPage(entry, false);
        if (content != null) {
            return content;
        }
        else if (!onlyRoots) {
            Iterator childEntries = entry.getChildEntryIterator(10);
            while (childEntries.hasNext()) {
                content = this.getFirstReleasedContent((WGStructEntry) childEntries.next(), chooser, onlyRoots);
                if (content != null) {
                    return content;
                }
            }
            return null;
        }
        else {
            return null;
        }
    }

    /**
     * Retrieves a user profile for the given profile name.
     * 
     * @param name
     * @throws WGAPIException
     */
    public WGUserProfile getUserProfile(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (name == null) {
            return null;
        }

        return (WGUserProfile) getDocumentByKey(new WGDocumentKey(WGDocument.TYPE_USERPROFILE, name, null));

    }

    private WGUserProfile retrieveUserProfile(String name) throws WGAPIException {

        WGUserProfile profile = null;
        WGDocumentCore profileCore = getPersonalisationCore().getUserProfile(name);
        if (profileCore != null && !profileCore.isDeleted()) {
            profile = this.createUserProfileObject(profileCore, new WGDocumentObjectFlags());
        }

        return profile;
    }

    /**
     * Creates a new user profile.
     * 
     * @param userNameWish
     *            Name of the new user profile. May be null to let the
     *            personalisation database generate a name.
     * @param type
     *            Type of the user profile. Can be any integer chosen by the
     *            personalisation implementation to differ profile types.
     * @param forceNew
     *            Forces the creation of a new profile. If there is already an
     *            existing profile with the given name then a new profile with
     *            a different name is created and returned.
     * @return The new user profile.
     * @throws WGAPIException
     *             instance of WGCreationException if a profile with this name
     *             already exists or the profile creation fails otherwise
     *             WGAPIExceptions on BackendErrors
     */
    public WGUserProfile createUserProfile(String userNameWish, int type, boolean forceNew) throws WGAPIException {
        return createUserProfile(userNameWish, type, forceNew, new WGDocumentObjectFlags());
    }

    protected WGUserProfile createUserProfile(String userNameWish, int type, boolean forceNew, WGDocumentObjectFlags flags) throws WGAPIException {
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        // check db lock
        if (getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Unable to create userprofile. Database is locked.");
        }

        if (getSessionContext().getAccessLevel() < ACCESSLEVEL_AUTHOR) {
            if (!(readerProfileCreation && getSessionContext().getAccessLevel() >= ACCESSLEVEL_READER)) {
                throw new WGAuthorisationException("User is not allowed to create user profiles in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
            }
        }

        if (userNameWish != null) {
            
            WGUserProfile profile = retrieveUserProfile(userNameWish);
            
            // The profile may have been created simultaneously in another thread. We do not fail here, just return the profile.
            if (profile != null) {
                if (forceNew) {
                    userNameWish = null;
                }
                else {
                    return profile;
                }
                //throw new WGDuplicateKeyException("There is already a user profile with id '" + userNameWish + "'");
            }
            
        }
            
        WGDocumentCore profileCore = getPersonalisationCore().createUserProfile(userNameWish, type);
        if (profileCore == null) {
            throw new WGCreationException("Unable to create user profile. Maybe databases of type " + getCore().getClass().getName() + " do not support creating user profiles.");
        }
        
        WGUserProfile profile = this.createUserProfileObject(profileCore, flags);
        return profile;

    }
    
    /**
     * Creates a new user profile.
     * 
     * @param userNameWish
     *            Name of the new user profile. May be null to let the
     *            personalisation database generate a name.
     * @param type
     *            Type of the user profile. Can be any integer chosen by the
     *            personalisation implementation to differ profile types.
     * @return The new user profile.
     * @throws WGAPIException
     *             instance of WGCreationException if a profile with this name
     *             already exists or the profile creation fails otherwise
     *             WGAPIExceptions on BackendErrors
     */
    public WGUserProfile createUserProfile(String userNameWish, int type) throws WGAPIException {
        return createUserProfile(userNameWish, type, false, new WGDocumentObjectFlags());
    }

    /**
     * Returns, if the database contains content
     * 
     * @return boolean
     */
    public boolean isContentRole() {
        return contentRole;
    }

    /**
     * Returns if the database contains design documents
     * 
     * @return boolean
     */
    public boolean isDesignRole() {
        return designRole;
    }

    /**
     * Returns if if the database contains file containers.
     * 
     * @return boolean
     */
    public boolean isRepositoryRole() {
        return repositoryRole;
    }

    /**
     * Indicates if the database contains user profiles
     */

    public boolean isUserProfilesRole() {
        return userProfilesRole;
    }

    /**
     * Determines if this database is completely empty of (content store)
     * documents. This does not work on personalisation databases.
     * 
     * @throws WGAPIException
     */
    public boolean isEmpty() throws WGAPIException {

        boolean contentEmpty = true;
        boolean designEmpty = true;

        if (isContentRole()) {
            contentEmpty = isContentEmpty();
        }

        if (isDesignRole() && getDesignProvider() == null) {
            designEmpty = (isDesignEmpty());
        }

        return (contentEmpty && designEmpty);

    }

    /**
     * Returns if this database has no design documents (file containers, script
     * modules, WebTML modules) and can be regarded "design-empty".
     * 
     * @throws WGAPIException
     */
    public boolean isDesignEmpty() throws WGAPIException {
        return getFileContainers().size() == 0 && getCSSJSModules().size() == 0 && getTMLModules().size() == 0;
    }

    /**
     * Returns if this database has neither content hierarchy (areas, struct
     * entries, contents) nor content schema documents (content types,
     * languages) and can be regarded "content-empty"
     * 
     * @throws WGAPIException
     */
    public boolean isContentEmpty() throws WGAPIException {
        return (getAreas().size() == 0 && getContentTypes().size() == 0 && getLanguages().size() == 0);
    }



    /**
     * Gets the name of the master login, which is the login user name used with
     * the initial call to WGDatabase.open().
     * 
     * @return String
     */
    public String getMasterLoginName() {
        return masterLoginName;
    }

    /**
     * Gets the workflow engine object for this database.
     * 
     * @return WGWorkflowEngine
     * @throws WGWorkflowException
     * @throws WGBackendException
     */
    public WGWorkflowEngine getWorkflowEngine() throws WGAPIException {

        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return workflowEngine;

    }

    /**
     * Returns an object containing internal statistics about the database
     * 
     * @return WGDatabaseStatistics
     */
    public WGDatabaseStatistics getStatistics() {

        return new WGDatabaseStatistics();
    }

    /**
     * Tests, if the current user can be associated with any entry in the list.
     * This is up to the database implementation to decide since name/group/role
     * mechanims may vary widely in different databases.
     * 
     * @param list
     * @throws WGAPIException
     */
    public boolean isMemberOfUserList(List list) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        // Lookup the member decisions cache
        Boolean decision;
        Map<Integer,Boolean> memberDecisions = null;
        int listHash = 0;
        if (this.userCachesEnabled) {
            memberDecisions = (Map<Integer, Boolean>) getUserCache().get(USERCACHE_MEMBERDECISIONS);
            if (memberDecisions == null) {
                memberDecisions = new ConcurrentHashMap<Integer, Boolean>();
                getUserCache().put(USERCACHE_MEMBERDECISIONS, memberDecisions);
            }
            listHash = list.hashCode();
            decision = memberDecisions.get(listHash);
            if (decision != null) {
                return decision;
            }
        }

        // Determine membership 
        decision =  this.getCore().isMemberOfUserList(list);
        
        // Write to cache
        if (this.userCachesEnabled) {
            memberDecisions.put(listHash, decision);
        }
        return decision;
    }

    /**
     * Creates a simple content document. This method can only be used, when the
     * underlying database implementation doesn't support full content features
     * like struct entries, content types etc.
     * 
     * @param title
     *            Title of the new content document.
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent createContent(String title) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (!hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            return this.createContent((WGStructEntry) null, null, title);
        }
        else {
            throw new WGNotSupportedException("This creation method is not available with this database implementation, because this implementation needs more information to create a content.");
        }
    }

    /**
     * Creates a simple content document. This variant can be used in
     * implementations that don't have struct entries or content types and
     * generate their own keys. (e.g. some descendants of SimpleContentSource
     * like JDBCSource).
     * 
     * @return The newly created content document.
     * @throws WGAPIException
     */
    public WGContent createContent() throws WGAPIException {
        return createContent("");
    }

    /**
     * Returns a list with all content types stored in this database
     * 
     * @return List List of WGContentType objects
     * @throws WGAPIException
     */
    public List<WGContentType> getContentTypes() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (List<WGContentType>) getDesignObjects(WGDocument.TYPE_CONTENTTYPE);
    }

    /**
     * Returns a content type document of the given name
     * 
     * @param name
     *            The name of the content type
     * @return WGContentType The content type if one with that name is present,
     *         null otherwise
     * @throws WGAPIException
     */
    public WGContentType getContentType(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (WGContentType) getDesignObject(WGDocument.TYPE_CONTENTTYPE, name);
    }

    /**
     * Returns the list of TML modules in this database.
     * 
     * @throws WGAPIException
     */
    public List<WGTMLModule> getTMLModules() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (List<WGTMLModule>) getDesignObjects(WGDocument.TYPE_TML);
    }

    /**
     * Returns the TML module for the given name and media key
     * 
     * @param name
     * @param mediaKey
     * @throws WGAPIException
     */
    public WGTMLModule getTMLModule(String name, String mediaKey) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return (WGTMLModule) getDesignObject(WGDocument.TYPE_TML, name, mediaKey);
    }

    /**
     * Returns the language of that name (code) if it exists. If the language
     * does not exist a dummy language definition is returned to prevent
     * NullPointers bc. of nonexistent language definitions. You can test for a
     * dummy language definition by calling language.isDummy().
     * 
     * @param name
     *            The language name
     * @return WGLanguage
     * @throws WGAPIException
     */
    public WGLanguage getLanguage(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (name == null) {
            return null;
        }

        WGLanguage lang = (WGLanguage) getDesignObject(WGDocument.TYPE_LANGUAGE, name);
        if (lang == null) {
            lang = (WGLanguage) createDesignDocumentObject(new WGFakeLanguage(this, name, "(Not defined)"), new WGDocumentObjectFlags().setDummy(true));
        }
        return lang;
    }

    /**
     * Retrieves the best matching language definition for the given locale.
     * Language definitions are searched in descending details order:
     * language_COUNTRY_VARIANT language_COUNTRY language
     * 
     * @param locale
     *            The locale to match
     * @return The language that matches the locale best, null if none could be
     *         found
     * @throws WGAPIException
     */
    public WGLanguage getLanguageForLocale(Locale locale) throws WGAPIException {

        if (locale == null) {
            return getLanguage(getDefaultLanguage());
        }

        String langKey;
        WGLanguage lang;
        if (!WGUtils.isEmpty(locale.getCountry()) && !WGUtils.isEmpty(locale.getVariant())) {
            langKey = locale.getLanguage() + "_" + locale.getCountry() + "_" + locale.getVariant();
            lang = getLanguage(langKey);
            if (lang != null && !lang.isDummy()) {
                return lang;
            }
        }

        if (!WGUtils.isEmpty(locale.getCountry())) {
            langKey = locale.getLanguage() + "_" + locale.getCountry();
            lang = getLanguage(langKey);
            if (lang != null && !lang.isDummy()) {
                return lang;
            }
        }

        langKey = locale.getLanguage();
        lang = getLanguage(langKey);
        if (lang != null && !lang.isDummy()) {
            return lang;
        }

        return null;

    }

    /**
     * Creates a new area definition.
     * 
     * @param name
     *            The Name of the new area
     * @return The newly created area object.
     * @throws WGAPIException
     */
    public WGArea createArea(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        performDesignCreationCheck(WGDocument.TYPE_AREA, name, null);

        name = name.toLowerCase();

        WGArea area = this.getArea(name);
        if (area != null) {
            throw new WGDuplicateKeyException("There is already an area of name '" + name + "'");
        }

        WGDocumentCore areaCore = this.getCore().createDesignDocument(WGDocument.TYPE_AREA, name, null);
        if (areaCore == null) {
            throw new WGCreationException("Unable to create area. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of areas");
        }

        area = (WGArea) createDesignDocumentObject(areaCore,  new WGDocumentObjectFlags());
        return area;
    }

    /**
     * Creates a new content type.
     * 
     * @param name
     *            Name of the new content type.
     * @return The newly created content type object.
     * @throws WGAPIException
     */
    public WGContentType createContentType(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        performDesignCreationCheck(WGDocument.TYPE_CONTENTTYPE, name, null);

        WGContentType contentType = this.getContentType(name);
        if (contentType != null) {
            throw new WGDuplicateKeyException("There is already a content type of name '" + name + "'");
        }

        name = name.toLowerCase();

        WGDocumentCore ctCore = createDesignDocumentCore(WGDocument.TYPE_CONTENTTYPE, name, null);
        if (ctCore == null) {
            throw new WGCreationException("Unable to create content type. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of content types");
        }

        contentType = (WGContentType) createDesignDocumentObject(ctCore,  new WGDocumentObjectFlags());
        contentType.setPositioning(WGContentType.POSITIONING_EVERYWHERE);
        // contentType.save();
        return contentType;

    }

    private WGDocumentCore createDesignDocumentCore(int type, String name, String mediaKey) throws WGAPIException {

        // check db lock
        if (getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Unable to create designobject with name '" + name + "'. Database is locked.");
        }

        if (designProvider != null && designProvider.providesType(type) && !isMetadataModule(type, name)) {
            return designProvider.createDesignDocument(type, name, mediaKey);
        }
        else {
            return getCore().createDesignDocument(type, name, mediaKey);
        }

    }

    /**
     * Creates a new TML module.
     * 
     * @param name
     *            name of the new TML module.
     * @param mediaKey
     *            Media key of the new TML module.
     * @return The newly created TML module.
     * @throws WGAPIException
     */
    public WGTMLModule createTMLModule(String name, String mediaKey) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        performDesignCreationCheck(WGDocument.TYPE_TML, name, mediaKey);

        name = name.toLowerCase();
        mediaKey = mediaKey.toLowerCase();

        WGTMLModule tmlModule = this.getTMLModule(name, mediaKey);
        if (tmlModule != null) {
            throw new WGDuplicateKeyException("There is already a tml module of name '" + name + "' and media key '" + mediaKey + "'");
        }

        WGDocumentCore tmlCore = createDesignDocumentCore(WGDocument.TYPE_TML, name, mediaKey);
        if (tmlCore == null) {
            throw new WGCreationException("Unable to create TML module. Maybe databases of type " + getCore().getClass().getName() + " do not support creating TML modules.");
        }

        tmlModule = (WGTMLModule) createDesignDocumentObject(tmlCore, new WGDocumentObjectFlags());
        tmlModule.setDirectAccessAllowed(true);
        // tmlModule.save();
        return tmlModule;
    }

    /**
     * Moves a struct entry to a new position.
     * 
     * @param entry
     *            The entry to move
     * @param newParent
     *            The new parent of the entry. If it is an WGArea object the
     *            struct entry will become a root document in this area. If it
     *            is an WGStructEntry it will become a child entry of this
     *            entry.
     * @return true, if the operation succeeded, false otherwise
     * @throws WGAPIException
     */
    public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) throws WGAPIException {

        if (isSessionOpen() == false) {
            throw new WGClosedSessionException();
        }

        performStructMoveCheck(entry, newParent);

        WGDocument oldParent = (entry.isRoot() ? (WGDocument) entry.getArea() : entry.getParentEntry());
        boolean result = getCore().moveStructEntry(entry, newParent);
        if (result == false) {
            return false;
        }

        oldParent.dropCache();
        newParent.dropCache();
        entry.dropCache();

        // Must ensure that these object versions are the ones in cache
        mapDocumentObject(oldParent);
        mapDocumentObject(newParent);
        mapDocumentObject(entry);
        
        // Fire event "content has been moved" for each and every influenced content
        processMovedDocuments(entry, true);        

        updateRevision(WGDatabaseRevision.forValue(getCore().getRevision()));

        WGDocumentEvent event = new WGDocumentEvent(WGDocumentEvent.TYPE_MOVED, entry.getDocumentKeyObj());
        fireDocumentEvent(event);

        return true;

    }

    private void processMovedDocuments(WGStructEntry entry, final boolean doMoveModifications) throws WGAPIException {
        
        // Clear cache on old parent (if still in cache #00004198)
        try {
            for (Serializable keyStr :  this.masterDocumentsByKey.getEntryKeys()) {
                WGDocumentKey key = new WGDocumentKey((String) keyStr);
                if (key.getDocType() == WGDocument.TYPE_STRUCTENTRY) {
                    Object structObj = this.masterDocumentsByKey.read(keyStr);
                    if (structObj instanceof WGStructEntry) { // May still be a NullPlaceHolder
                        WGStructEntry struct = (WGStructEntry) structObj;
                        if (struct.isContainedInChildEntryCache(entry)) {
                            struct.dropCache();
                        }
                    }
                }
                if (key.getDocType() == WGDocument.TYPE_AREA) {
                    Object areaObj = this.masterDocumentsByKey.read(keyStr);
                    if (areaObj instanceof WGArea) {
                        WGArea area = (WGArea)  areaObj;
                        if (area.isContainedInRootEntryCache(entry)) {
                            area.dropCache();
                        }
                    }
                }
            }
        }
        catch (CacheException e1) {
            WGFactory.getLogger().error("Exception processing moved page " + entry.getStructKey(), e1);
        }
        
        // Clear caches on moved tree
        entry.visit(new WGPageVisitor() {
            public void visit(WGArea area) {};
            public void visit(WGStructEntry entry) {
                try {
                    entry.dropCache();
                }
                catch (WGAPIException e) {
                    WGFactory.getLogger().error("Exception firing event 'contentHasBeenMoved'", e);
                }
            };
            public void visit(WGContent content) {
                try {
                    content.dropCache();
                    
                    if (doMoveModifications) {
                        boolean shouldBeVisible = !(content.hasCompleteRelationships() && content.getStructEntry().getArea().isSystemArea());
                        if (content.isVisible() != shouldBeVisible) {
                            content.setVisible(shouldBeVisible);
                            content.save(content.getLastModified());
                        }
                    }
                    
                    WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_HASBEENMOVED, content.getDocumentKey(), content.getStructEntry().getContentType().getName(), content.getDatabase());
                    event.setContent(content);
                    fireContentEvent(event);
                }
                catch (WGAPIException e) {
                    WGFactory.getLogger().error("Exception firing event 'contentHasBeenMoved'", e);
                }
            }
        });
    }

    /**
     * Controls if the current user may move a struct entry to the given new parent. If so the method exists normally. If not an exception is thrown.
     * @param entry The entry to move
     * @param newParent The new parent of the entry. May be an {@link WGArea} or another {@link WGStructEntry}.
     * @throws WGAPIException If the user may not move the document. The exception informs about the reason.
     */
    public void performStructMoveCheck(WGStructEntry entry, WGDocument newParent) throws WGIllegalArgumentException, WGAPIException, WGAuthorisationException, ResourceIsLockedException {
        if (entry == null) {
            throw new WGIllegalArgumentException("Entry to move is null");
        }

        if (newParent == null) {
            throw new WGIllegalArgumentException("New parent is null");
        }

        if (newParent.getDocumentKey().equals(entry.getDocumentKey())) {
            throw new WGIllegalArgumentException("You are trying to make a structentry the child entry of itself!");
        }

        if (newParent instanceof WGStructEntry && ((WGStructEntry) newParent).isDescendantOf(entry)) {
            throw new WGIllegalArgumentException("You are trying to move a structentry into it's own child hierarchy!");
        }

        if (entry.getDatabase() != newParent.getDatabase()) {
            throw new WGIllegalArgumentException("The databases of the entry to move and the new parent document are different");
        }

        WGDocument restrictingDoc = entry.testEditPageHierarchyRights();
        if (restrictingDoc != null) {
            String errorCode = (restrictingDoc instanceof WGArea ? WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA : WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE);
            throw new WGAuthorisationException("You are not allowed to edit this struct entry because document '" + restrictingDoc.getDocumentKey() + "' disallows it.", errorCode, restrictingDoc);
        }

        if (!getSessionContext().getUserAccess().mayMoveStructEntries()) {
            throw new WGAuthorisationException("You are not allowed to move struct entries in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_MOVEPAGE_RIGHTS);
        }

        if (newParent instanceof WGArea) {
            WGArea area = (WGArea) newParent;
            if (!area.mayEditPages()) {
                throw new WGAuthorisationException("You are not allowed to move structs to this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA);
            }
            if (entry.getContentType() != null) {
                area.testRootPageRestrictions(entry.getContentType());
            }
        }
        else if (newParent instanceof WGStructEntry) {
            WGStructEntry parentEntry = (WGStructEntry) newParent;
            restrictingDoc = parentEntry.testEditChildPagesHierarchyRights();
            if (restrictingDoc != null) {
                String errorCode = (restrictingDoc instanceof WGArea ? WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA : WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE);
                throw new WGAuthorisationException("You are not allowed to move a struct entry below this parent entry because document '" + restrictingDoc.getDocumentKey() + "' disallows it.", errorCode, restrictingDoc);
            }
            if (entry.getContentType() != null) {
                parentEntry.testChildPageRestrictions(entry.getContentType());
            }
        }

        WGArea targetArea = (newParent instanceof WGArea ? (WGArea) newParent : ((WGStructEntry) newParent).getArea());
        if (!targetArea.isSystemArea() && !entry.getContentType().mayCreateChildEntry(newParent)) {
            throw new WGAuthorisationException("The content type of the struct cannot be used at the target position", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, entry.getContentType());
        }

        // check lock status of entry to move
        if (entry.getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Cannot move structentry. Given entry is locked.");
        }

        // check lock status of newParent
        if (newParent.getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Cannot move structentry. New parentEntry is locked.");
        }
        
        // Check edit rights for all documents in moved page hierarchy
        entry.performSubtreeModificationCheck();
        
    }

    /**
     * Creates a new Script module
     * 
     * @param name
     *            Name of the new module
     * @param type
     *            Type of the module. A constant WGCSSJSModule.CODETYPE_...
     * @throws WGAPIException
     */
    public WGCSSJSModule createCSSJSModule(String name, String type) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        performDesignCreationCheck(WGDocument.TYPE_CSSJS, name, type);

        name = name.toLowerCase();

        WGCSSJSModule cssLib = this.getCSSJSModule(name, type);
        if (cssLib != null) {
            throw new WGDuplicateKeyException("There is already a css/js library of name '" + name + "' and type '"+ type + "'");
        }

        WGDocumentCore cssCore = createDesignDocumentCore(WGDocument.TYPE_CSSJS, name, type);
        if (cssCore == null) {
            throw new WGCreationException("Unable to create css/js library. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of css/js libraries");
        }

        cssLib = (WGCSSJSModule) createDesignDocumentObject(cssCore, new WGDocumentObjectFlags());
        cssLib.setMetaData(WGCSSJSModule.META_CODETYPE, type);
        // cssLib.save();
        return cssLib;

    }

    /**
     * Controls if the current user may create a design document with the given data. If so the method exists normally. If not an exception is thrown.
     * @param type The type of design document. Use constants WGDocument.TYPE_...
     * @param name The name of design document
     * @param mediaKey For WebTML modules specify media key, for script modules specify code type (Constants WGScriptModule.CODETYPE_...). For other types specify null.
     * @throws WGAPIException If the creation for the given data would fail
     */
    public void performDesignCreationCheck(int type, String name, String mediaKey) throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("Unable to create design document. Database is locked.");
        }
        
        if (type == WGDocument.TYPE_AREA || type == WGDocument.TYPE_CONTENTTYPE || type == WGDocument.TYPE_LANGUAGE) {
            if (getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
                throw new WGAuthorisationException("You are not allowed to create schema documents", WGAuthorisationException.ERRORCODE_OP_NEEDS_CHIEF_EDITOR_RIGHTS);
            }
        }
        else {
            if (getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
                throw new WGAuthorisationException("You are not allowed to create design documents", WGAuthorisationException.ERRORCODE_OP_NEEDS_DESIGNER_RIGHTS);
            }
        }

        if (type == WGDocument.TYPE_CSSJS && isMetadataModule(WGDocument.TYPE_CSSJS, name) && !WGCSSJSModule.CODETYPE_XML.equals(mediaKey)) {
            throw new WGIllegalArgumentException("Metadata modules must be of codetype XML");
        }
        
        if (!isDoctypeModifiable(type)) {
            throw new WGIllegalStateException("Updating this type of design document via WGAPI is not permitted in this database: " + WGDocument.doctypeNumberToName(type), WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
    }

    /**
     * Returns the script modules in this database.
     * 
     * @throws WGAPIException
     */
    public List getCSSJSModules() throws WGAPIException {
        return getDesignObjects(WGDocument.TYPE_CSSJS);
    }

    /**
     * Returns the script module of the given name.
     * 
     * @param name
     * @throws WGAPIException
     */
    public WGScriptModule getCSSJSModule(String name) throws WGAPIException {
        
        // Changed backend semantics with WGA5:
        // When no codetype given we iterate over the available codetypes and request the separately
        
        Iterator types = WGScriptModule.METAINFO_CODETYPE.getAllowedValues().iterator();
        while (types.hasNext()) {
            String type = (String) types.next();
            WGScriptModule mod = getCSSJSModule(name, type);
            if (mod != null) {
                return mod;
            }
        }
        
        return null;
    }
    
    /**
     * Returns the script module of the given name and type
     * 
     * @param name
     * @throws WGAPIException
     */
    public WGScriptModule getCSSJSModule(String name, String type) throws WGAPIException {
        return (WGScriptModule) getDesignObject(WGDocument.TYPE_CSSJS, name, type);
    }

    /**
     * Returns the list of file containers in this database
     * 
     * @throws WGAPIException
     */
    public List<WGFileContainer> getFileContainers() throws WGAPIException {
        return (List<WGFileContainer>) getDesignObjects(WGDocument.TYPE_FILECONTAINER);
    }

    /**
     * Return the file container of the given name
     * 
     * @param name
     * @throws WGAPIException
     */
    public WGFileContainer getFileContainer(String name) throws WGAPIException {
        return (WGFileContainer) getDesignObject(WGDocument.TYPE_FILECONTAINER, name);
    }

    /**
     * Creates a new file container.
     * 
     * @param name
     *            The name of the file container.
     * @return The newly created file container.
     * @throws WGAPIException
     */
    public WGFileContainer createFileContainer(String name) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        performDesignCreationCheck(WGDocument.TYPE_FILECONTAINER, name, null);

        name = name.toLowerCase();

        WGFileContainer file = this.getFileContainer(name);
        if (file != null) {
            throw new WGDuplicateKeyException("There is already a file container of name '" + name + "'");
        }

        WGDocumentCore fileCore = createDesignDocumentCore(WGDocument.TYPE_FILECONTAINER, name, null);
        if (fileCore == null) {
            throw new WGCreationException("Unable to create file container. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of file containers");
        }

        file = (WGFileContainer) createDesignDocumentObject(fileCore, new WGDocumentObjectFlags());
        // file.save();
        return file;

    }

    /**
     * Creates a new language definition only by name, which is also used as title
     * 
     * @param name
     *            The name (i.e. language code) of the new language.
     *            language
     * @return The newly created language definition.
     * @throws WGAPIException
     */
    public WGLanguage createLanguage(String name) throws WGAPIException {
        return createLanguage(name, name);
    }
    
    /**
     * Creates a new language definition.
     * 
     * @param name
     *            The name (i.e. language code) of the new language.
     * @return The newly created language definition.
     * @throws WGAPIException
     */
    public WGLanguage createLanguage(String name, String title) throws WGClosedSessionException, WGAPIException, WGDuplicateKeyException, WGCreationException {
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        performDesignCreationCheck(WGDocument.TYPE_LANGUAGE, name, null);

        name = name.toLowerCase();

        WGLanguage language = this.getLanguage(name);
        if (language != null && language.isDummy() == false) {
            throw new WGDuplicateKeyException("There is already a language of name '" + name + "'");
        }

        WGDocumentCore langCore = this.getCore().createDesignDocument(WGDocument.TYPE_LANGUAGE, name, null);
        if (langCore == null) {
            throw new WGCreationException("Unable to create language. Maybe databases of type " + getCore().getClass().getName() + " do not support creation of languages");
        }

        language = (WGLanguage) createDesignDocumentObject(langCore, new WGDocumentObjectFlags());
        language.setTitle(title);
        return language;
    }

    /**
     * Creates a draft copy of the given content document, keeping it's language
     * 
     * @param content
     * @return The draft copy of the content.
     * @throws WGAPIException
     */
    public WGContent createDraftCopy(WGContent content) throws WGAPIException {

        return createDraftCopy(content, null);
    }

    /**
     * Creates a draft copy of the given content, using the language given as
     * parameter.
     * 
     * @param content
     *            The content to copy
     * @param language
     *            The language for the draft copy. Specify null to keep the
     *            language of the original.
     * @return The draft copy. This document is already stored when it is
     *         returned.
     * @throws WGAPIException
     */
    public WGContent createDraftCopy(WGContent content, WGLanguage language) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (language == null) {
            language = content.getLanguage();
        }

        if (!content.isSaved()) {
            throw new WGIllegalStateException("Source content has not yet been saved");
        }

        // check if structentry is saved
        if (!content.getStructEntry().isSaved()) {
            throw new WGIllegalStateException("StructEntry of source content has no yet been saved.");
        }

        if (!language.isSaved()) {
            throw new WGIllegalStateException("Language for this content has not yet been saved");
        }

        // Test for validity and access rights
        if (content.getStatus().equals(WGContent.STATUS_RELEASE) == false && content.getStatus().equals(WGContent.STATUS_ARCHIVE) == false) {
            throw new WGIllegalStateException("This content is not in status RELEASE");
        }

        if (getSessionContext().getAccessLevel() < ACCESSLEVEL_AUTHOR) {
            throw new WGAuthorisationException("User is not allowed to create content in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
        }

        if (getSessionContext().getAccessLevel() < ACCESSLEVEL_EDITOR) {
            if (!content.isAuthorOrOwner()) {
                throw new WGAuthorisationException("You are not allowed to create a draft copy of this content bc. you are not the author of the published version.", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
            }
        }

        /*
        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && content.getStructEntry().testEditPageHierarchyRights() != null) {
            throw new WGAuthorisationException("User is not allowed to create content under this struct entry", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, content.getStructEntry());
        }
        */
        if (hasFeature(FEATURE_FULLCONTENTFEATURES) && !content.mayEditContent()) {
            throw new WGAuthorisationException("User is not allowed to create content under this struct entry", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, content.getStructEntry());
        }

        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            WGContentType contentType = content.getStructEntry().getContentType();
            if (contentType != null && !contentType.mayCreateContent()) {
                throw new WGAuthorisationException("User is not allowed to use this content type", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, contentType);
            }
            else if (contentType == null && getSessionContext().getAccessLevel() != WGDatabase.ACCESSLEVEL_MANAGER) {
                throw new WGIllegalArgumentException("You cannot create struct entries without a content type");
            }
        }

        WGContent newContent = createNewDraftVersion(content, language);

        // Initialize
        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            newContent.setStatus(WGContent.STATUS_DRAFT);
            newContent.setMetaData(WGContent.META_AUTHOR, getSessionContext().getUser());
            newContent.setValidity(null, null);
        }

        /*
        // Initialize by workflow engine
        WGWorkflow workflow = getWorkflowEngine().getWorkflow(newContent);
        workflow.initialize();
        */
        
        if (!projectMode) {
            newContent.addWorkflowHistoryEntry("Draft copy created");
        }
        else {
            newContent.addWorkflowHistoryEntry("Changed to draft status in project mode");
        }
        
        WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_STATUSCHANGED, newContent.getDocumentKey(), content.getStructEntry().getContentType().getName(), this);
        event.setContent(newContent);
        fireContentEvent(event);

        // Save and return
        newContent.save();
        return newContent;

    }

    private synchronized WGContent createNewDraftVersion(WGContent content, WGLanguage language) throws WGAPIException, WGCreationException {
        // Evaluate new version number
        int maxVersion = findNewVersionNumber(content.getStructEntry(), language);

        // We will update the last changed date only, when there are no
        // pending changes in background
        boolean updateLastChanged = !isDatabaseUpdatedInBackground();

        // Create core and pre-initialize to be a valid WGContent object
        WGDocumentCore docCore = getCore().createCopy(content.getCore());
        if (docCore == null) {
            throw new WGCreationException("Unable to create draft copy. Maybe databases of type " + getCore().getClass().getName() + " do not support copying content");
        }

        if (hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            docCore.setMetaData(WGContent.META_VERSION, new Integer(maxVersion));
            docCore.setMetaData(WGContent.META_LANGUAGE, language.getName());
        }

        // Create WGContent object
        WGContent newContent = createContentObject(docCore);
        return newContent;
    }



    /**
     * Indicates, if document caching is enabled globally for the database
     */
    public boolean isCachingEnabled() {
        return cachingEnabled;
    }

    /**
     * Controls if the document caching is enabled for this database. It is
     * enabled by default. Turning it off may speed up writing operations but
     * will mean significant overhead to recurring reading operations.
     */
    public void setCachingEnabled(boolean b) {
        cachingEnabled = b;
    }

    /**
     * Starts a database transaction. The database must support this, which is
     * indicated by the feature WGDatabase.FEATURE_TRANSACTIONS.
     * <p>
     * NOTE: This transaction functionality is for WGAPI internal use only!  Use {@link WGDatabase#startTransaction()} for external use.
     * </p>
     * @return If the starting of the transaction succeeds
     * @deprecated Use {@link #startTransaction()}
     */
    public boolean beginTransaction() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        boolean result = getCore().beginTransaction();
        if (result == true) {
            getSessionContext().addTransactionToStack(getSessionContext().getLegacyFakeTransaction());
            return true;
        }
        else {
            return false;
        }
    }
    
    /**
     * Starts a transaction and returns a transaction object.
     * Changes done while the transaction runs will not be written to backend until {@link WGTransaction#commit()} is called.
     * Calling {@link WGTransaction#rollback()} will revert all changes done in the transaction.
     * If this is called while already in a transaction it returns a cascaded transaction. The commit of the cascaded transaction will be a no-op. Rolling back will roll back the real transaction.
     * If this is called on a database backend which does not support transaction it returns a fake transaction with no-op operations.
     * @return The transaction object
     * @throws WGAPIException
     */
    public WGTransaction startTransaction() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        WGTransaction trans = null;
        if (!hasFeature(FEATURE_TRANSACTIONS)) {
            trans = new WGFakeTransaction();
        }
        else if (!getSessionContext().getTransactionsStack().isEmpty()) {
            trans = new WGCascadedTransaction();
        }
        else {
            boolean result = getCore().beginTransaction();
            if (result == true) {
                trans = new WGRealTransaction();
            }
            else {
                throw new WGBackendException("Starting transaction failed");
            }
        }
        
        getSessionContext().addTransactionToStack(trans);
        return trans;
        
    }

    /**
     * Commits a database transaction, executing all data changes since the
     * start of the transaction. A transaction must be started with
     * beginTransaction() before calling this. The database must support
     * transactions, which is indicated by the feature
     * WGDatabase.FEATURE_TRANSACTIONS.
     * <p>
     * NOTE: This transaction functionality is for WGAPI internal use only!  Use {@link WGDatabase#startTransaction()} for external use.
     * </p>
     * @return If the committing of the transaction succeeds
     * @deprecated Use {@link WGTransaction#commit()}
     */
    public boolean commitTransaction() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        boolean result = getCore().commitTransaction();
        if (result == true) {
            getSessionContext().removeTransactionFromStack(getSessionContext().getLegacyFakeTransaction());
            MasterSessionTask performDBUpdates = new MasterSessionTask(this) {
                
                @Override
                protected void exec(WGDatabase db) throws Throwable {
                    db.checkDatabaseUpdates(true);                    
                }
            };
            try {
                performDBUpdates.runWithExceptions();
            }
            catch (Throwable e) {
                throw new WGBackendException("Unable to perform db update on transaction commit.", e);
            }
            return true;
        }
        else {
            return false;
        }
    }
    
    protected boolean doCommitTransaction() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        boolean result = getCore().commitTransaction();
        if (result == true) {
            verboseCacheManagement("Committing transaction. Doing backup changes catchup");
            catchupBackendChanges();
            return true;
        }
        else {
            throw new WGBackendException("Transaction commit failed");
        }
        
    }

    /**
     * Rolls a database transaction back, canceling all data changes done since
     * the start of the transaction. A transaction must be started with
     * beginTransaction() prior to doing this. The database must support
     * transactions, which is indicated by the feature
     * WGDatabase.FEATURE_TRANSACTIONS.
     * <p>
     * NOTE: This transaction functionality is for WGAPI internal use only!  Use {@link WGDatabase#startTransaction()} for external use.
     * </p>
     * @return If the rolling back of the transaction succeeds
     * @deprecated use {@link WGTransaction#rollback()}
     */
    public boolean rollbackTransaction() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        boolean result = getCore().rollbackTransaction();
        if (result == true) {
            getSessionContext().removeTransactionFromStack(getSessionContext().getLegacyFakeTransaction());
            return true;
        }
        else {
            return false;
        }
    }
    
    protected boolean doRollbackTransaction() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        boolean result = getCore().rollbackTransaction();
        if (result == true) {
            getSessionContext().clearCache();
            return true;
        }
        else {
            return false;
        }
        
    }

    /**
     * Retrieves a WGACL object which manages the access control to this
     * database. Is only supported by databases with feature
     * WGDatabase.FEATURE_ACL_MANAGEABLE
     */
    public WGACL getACL() {

        WGACLCore core = getCore().getACL();
        if (core != null) {
            return new WGACL(this, core);
        }
        else {
            return null;
        }
    }

    /**
     * Returns a HashMap that will store userdefined data for the currently
     * logged in user. Each user has his own hash map. This information persists
     * sessions and can be fetched again in later sessions.
     */
    public UserHashMap getUserCache() {
        return _userCache;
    }

    /**
     * Returns the group of UserHashMaps for this database. Should not be used
     * outside the WGAPI.
     */
    public UserHashMapGroup getUserHashMapGroup() {
        return userHashMapGroup;
    }

    /**
     * Returns the native backend object for this database if there is one.
     * 
     * @throws WGBackendException
     */
    public Object getNativeObject() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return getCore().getNativeObject();

    }

    /**
     * Performs all the neccessary operations after a document has been saved: -
     * Update caches - fire events - set states Either param document or param
     * log must be filled to indicate the document
     * 
     * @param document
     *            The document that has been saved, if it is available, else
     *            null
     * @param log
     *            An Update log that triggered this document saved operation
     * @param isNewDoc
     *            Determines if this is a newly created doc that has been saved
     *            for the first date
     * @param savedViaWGA
     *            Determines if the document has been saved via WGA (and not in
     *            background)
     * @throws WGAPIException
     */
    protected void documentSaved(WGDocument document, WGDocumentKey docKey, boolean isNewDoc, boolean savedByWGAPI, WGDatabaseRevision revision) throws WGAPIException {

        if (getSessionContext().isTransactionActive()) {
            return;
        }
        
        int docType = docKey.getDocType();
        
        // In case of contents and struct entries we must always load the document to do effective cache management
        if (document == null && (docType == WGDocument.TYPE_CONTENT || docType == WGDocument.TYPE_STRUCTENTRY)) {
            document = getDocumentByKey(docKey);
        }
        
        // Operations only neccessary if the document is currently in cache =
        // document is present
        if (document != null) {

        	/*
        	 * In case the document has been deleted and then is saved (recreated):
        	 * We have to reset the deleted flag.
        	 */
        	document.setDeleted(false);
            document.dropCache();

            // Must be done to ensure that the most up-to-date object is mapped
            mapDocumentObject(document);

            // Clear related caches
            if (document instanceof WGContent) {
                WGContent content = (WGContent) document;

                if (!content.getStatus().equals(WGContent.STATUS_DRAFT) && !content.getStatus().equals(WGContent.STATUS_REVIEW)) {
                    
                    // Flush the query cache
                    try {
                        verboseCacheManagement("Flushing query cache on behalf of saved document " + docKey.toString());
                        masterQueryCache.flushAll();
                    }
                    catch (CacheException e) {
                        WGFactory.getLogger().error("Exception flushing query cache on database '" + getDbReference() + "'", e);
                    }
                    content.getStructEntry().dropCache();
                }
                
                if (isNewDoc) {
                    content.getStructEntry().dropCache();
                }
            }
            else if (document instanceof WGStructEntry) {
                WGStructEntry structEntry = (WGStructEntry) document;
                if (structEntry.isRoot()) {
                    structEntry.getArea().dropCache();
                }
                else {
                    structEntry.getParentEntry().dropCache();
                }
            }
        }

        // Clear indirect caches
        if (isDesignDocumentType(docType)) {
            designDocumentLists.remove(new Integer(docType));
        }
        
        _userCache.clear();

        // Fire content has been saved event
        if (docType == WGDocument.TYPE_CONTENT) {
            String contentType = null;
            if (document != null) {
                WGContent content = (WGContent) document;
                if (content.hasCompleteRelationships()) {
                    contentType = content.getStructEntry().getContentType().getName();
                }
            }
            WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_HASBEENSAVED, docKey.toString(), contentType, this);
            if (document != null) {
                event.setContent((WGContent) document);
            }
            fireContentEvent(event);
        }

        // Fire design change event
        if (docType == WGDocument.TYPE_FILECONTAINER || docType == WGDocument.TYPE_TML || docType == WGDocument.TYPE_CSSJS) {
            // Only fire when no design provider present. Otherwise the
            // listeners are registered at the provider and it is his
            // responsibility to throw the event
            if (getDesignProvider() == null) {
                List logs = new ArrayList();
                logs.add(new WGUpdateLog(WGUpdateLog.TYPE_UPDATE, new Date(), getSessionContext().getUser(), docKey.toString(), null, revision));
                fireDatabaseDesignChangedEvent(new WGDesignChangeEvent(null, this, logs));
            }
        }
        
        updateCacheMaintenanceData();

        if (savedByWGAPI) {
            this.getSessionContext().setDatabaseUpdated(true);
            // Fire database event if the remove has been done by WGAPI
            // When this is done in background change processing, the event will
            // get fired after all changes by checkDatabaseUpdates
            fireDatabaseEvent(new WGDatabaseEvent(this, WGDatabaseEvent.TYPE_UPDATE, document));
            
        }
        
        if (document != null && revision != null) {
            document.setCacheRevision(revision);
        }
        
    }

    private void updateRevision(WGDatabaseRevision revision) throws WGAPIException {
        if (getSessionContext() != null && getSessionContext().isTransactionActive()) {
            return;
        }
        _revision = revision;
        
        if (_revision == null) {
            _revision = WGDatabaseRevision.forValue(new Date(Long.MIN_VALUE));
            _revisionDate = new Date(Long.MIN_VALUE);
        }
        else {
            if (_revision.getRevisionValue() instanceof Date) {
                _revisionDate = (Date) _revision.getRevisionValue();
            }
            else {
                _revisionDate = getCore().getRevisionDate(_revision.getRevisionValue());
            }
        }
        
    }

    private void updateCacheMaintenanceData() {
        lastCacheMaintenance = new Date();
    }

    /**
     * Tests if the given document type is the type of a design document, i.e.
     * an area, contenttype, script module, file container, language definition
     * or tml module
     * 
     * @param docType
     * @return true, if the doctype is a design document type, false if not
     */
    public static boolean isDesignDocumentType(int docType) {

        return (docType == WGDocument.TYPE_AREA || docType == WGDocument.TYPE_CONTENTTYPE || docType == WGDocument.TYPE_CSSJS || docType == WGDocument.TYPE_FILECONTAINER
                || docType == WGDocument.TYPE_LANGUAGE || docType == WGDocument.TYPE_TML);

    }

    /**
     * Returns the authentication module used to authenticate sessions, if this database supports and uses a auth module.
     * If this database uses a {@link RedirectionAuthModule} this method returns the real backend module. So this method should be used
     * to access the module if some special, non-standard, capabilities are to be used
     */
    public AuthenticationModule getAuthenticationModule() {
        
        AuthenticationModule backendModule = _authenticationModule;
        while (backendModule instanceof RedirectionAuthModule) {
            backendModule = ((RedirectionAuthModule) backendModule).getBackendModule();
        }
        
        return backendModule;
    }

    /**
     * Returns the password of the master login used to open this database.
     */
    public String getMasterLoginPassword() {
        return masterLoginPassword;
    }

    /**
     * Returns the maximum cores (i.e. backend document) that a session is
     * allowed to hold at once. If more are retrieved, the oldest cores will be
     * dropped to remain under this threshold.
     */
    public int getMaxCores() {
        return maxCores;
    }

    /**
     * Returns collected statistics of sessions that exceeded the maxdocs
     * threshold. A maximum of 100 statistics is collected. After that the
     * statistics of the top 100 statistics with the most docs retrieved are
     * kept.
     */
    public SortedBag getSessionStatistics() {
        return sessionStatistics;
    }

    /**
     * Sets the title of the database. This is not stored in the backend
     * database but only served via "getTitle()" until the database object is
     * closed.
     * 
     * @param string
     *            The title to set
     */
    public void setTitle(String string) {
        title = string;
    }

    /**
     * Retrieves logs of documents that were modified since a cutoff revision. This
     * is only available in databases with feature WGDatabase.FEATURE_FIND_UPDATED_DOCS. 
     * The returned list does not reflect all changes done to the documents but returns
     * a "condensed" list of operations reflecting the current state of documents
     * in the smallest possible way:
     * <ul>
     * <li>If a document has been modified multiple times in a row there will only be one update log, no matter how much updates have been done
     * <li>If a document was moved multiple times in a row and still exists there will be one move log, no matter how much updates have been done
     * </ul>
     * 
     * @param cutoff
     *            The cutoff revision. Documents modified earlier will not be
     *            included in the result
     * @return List containing the log objects of type WGUpdateLog
     * @throws WGAPIException
     */
    public List<WGUpdateLog> getUpdatedDocumentsSince(Comparable cutoff) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (cutoff instanceof WGDatabaseRevision) {
            cutoff = ((WGDatabaseRevision) cutoff).getRevisionValue();
        }

        List<WGUpdateLog> logs = getCore().getUpdateLogs(cutoff);
        List<WGUpdateLog> condensedLogs = new ArrayList<WGUpdateLog>();
        
        // Filter logs to get the "condensed" list described in javadoc
        Map<String,Integer> lastOperation = new HashMap<String,Integer>();
        
        for (WGUpdateLog log : logs) {
            
            Integer op = lastOperation.get(log.getDocumentKey());
            if (op == null || op != log.getType()) {
                condensedLogs.add(log);
                lastOperation.put(log.getDocumentKey(), log.getType());
            }
        }
        
        return condensedLogs;

    }

    /**
     * Returns if the given struct entry contains a content document. This
     * method is capable of regarding content documents that the current user is
     * not allowed to see.
     * 
     * @param entry
     *            The entry to test
     * @return true, if there exists a content (even those that the user may not
     *         see), false otherwise
     * @throws WGAPIException
     */
    public boolean hasContents(WGStructEntry entry) throws WGAPIException {

        if (isSessionOpen() == false) {
            throw new WGClosedSessionException();
        }

        if (entry == null || entry.isDeleted() || entry.isDummy()) {
            return false;
        }

        return (entry.getContentCount() > 0);

    }


    /**
     * Returns a document by it's document key, only if it is contained in
     * cache.
     * 
     * @param documentKeyStr
     *            The document key in its string representation
     * @return The document if it already is cached, null otherwise
     * @throws WGAPIException
     */
    public WGDocument getDocumentByDocumentKeyFromCache(String documentKeyStr) throws WGAPIException {

        WGDocumentKey documentKey = new WGDocumentKey(documentKeyStr);

        return getDocumentByDocumentKeyFromCache(documentKey);

    }

    /**
     * Returns a document by it's document key, only if it is contained in
     * cache.
     * 
     * @param documentKey
     *            The document key
     * @return The document if it already is cached, null otherwise
     * @throws WGAPIException
     */
    public WGDocument getDocumentByDocumentKeyFromCache(WGDocumentKey documentKey) throws WGAPIException {
        try {
        return getDocumentByDocumentKeyFromCache(documentKey, true);
    }
        catch (WGDocumentDoesNotExistException e) {
            return null;
        }
    }

    /**
     * Retrieves a document from document cache.
     * @param documentKey The key of the document.
     * @param userMode true means that this is a fetch for an individual user and his access rights and session caching setting have to take effect. false is "internal mode" where the API tries to retrieve a document object without any checks.
     * @return Fetched document or null
     * @throws WGAPIException
     */
    private WGDocument getDocumentByDocumentKeyFromCache(WGDocumentKey documentKey, boolean userMode) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        // Test session private cache
        WGSessionContext.DocumentContext docContext = getSessionContext().getDocumentContext(documentKey);
        if (docContext != null && docContext.getDocument() != null && !docContext.getDocument().isDeleted()) {
            if (docContext.getDocument().isReadableForUser()) {
                return docContext.getDocument();
            }
            else {
                throw new WGDocumentDoesNotExistException();   
            }
        }

        Object cache;
        Object cacheKey;
        int docType = documentKey.getDocType();
        if (!isDoctypeCacheable(docType)) {
            return null;
        }

        // From here user calls may only proceed if caching is enabled on the session
        if (userMode && !getSessionContext().isCachingEnabled()) {
            return null;
        }

        // Test public cache
        cache = masterDocumentsByKey;
        cacheKey = documentKey;
        WGDocument doc = fetchDocumentFromCache(cache, cacheKey, !userMode);
        if (doc != null && !doc.isDeleted()) {
            return doc;
        }
        else {
            return null;
        }
    }

    /**
     * Given a list of user/group names from an access control field (e.g.
     * META_READERS in Content) tests if anyone is allowed the operation.
     * 
     * @param users
     *            The list of user/group names
     * @param emptyMeansYes
     *            Determines if an empty field (size = 0; first element null or
     *            empty string) should return true
     * @return true, if regarding this user/group list anyone is allowed
     */
    public static boolean anyoneAllowed(List users, boolean emptyMeansYes) {

        if (users == null) {
            return emptyMeansYes;
        }

        if (users.size() == 0 || (users.size() == 1 && (users.get(0) == null || users.get(0).toString().trim().equals("")))) {
            return emptyMeansYes;
        }

        if (users.contains("*")) {
            return true;
        }

        return false;

    }

    /**
     * Given a list of user/group names from an access control field (e.g.
     * META_READERS in Content) tests if anyone is allowed the operation. In
     * this version of the method, an empty field (size = 0 or first element
     * null or empty string) is always treated as true.
     * 
     * @param users
     *            The list of user/group names
     * @return true, if regarding this user/group list anyone is allowed
     */
    public static boolean anyoneAllowed(List users) {
        return anyoneAllowed(users, true);
    }

    /**
     * Returns an Iterator iterating over all content documents in this database
     * does not include archived documents
     * 
     * @throws WGAPIException
     */
    public Iterator<WGContent> getAllContent() throws WGAPIException {
        return getAllContent(false);
    }

    /**
     * Returns an Iterator iterating over all content documents in this database
     * 
     * @param includeArchived
     *            Should archived documents been included? true/false
     * @throws WGAPIException
     */
    public Iterator<WGContent> getAllContent(boolean includeArchived) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return new WGContentIterator(this, includeArchived);
    }





    /**
     * Returns a database reference to be used throughout the application (in
     * WGA: the database key)
     */
    public String getDbReference() {
        return dbReference;
    }

    /**
     * Sets a reference for this database to use throughout the application.
     */
    public void setDbReference(String dbReference) {
        this.dbReference = dbReference;
    }

    /**
     * Default implementation for building user details for an authentication
     * session. This one can be used by DB implementations that implement
     * FEATURE_ACL_MANAGEABLE. This method is not for direct usage outside of
     * the WGAPI.
     * 
     * @param authSession
     *            The authentication session to create details for
     * @return User details object
     * @throws WGBackendException
     */
    public WGUserDetails defaultBuildUserDetails(AuthenticationSession authSession) throws WGAPIException {

        // First try to retrieve from user cache
        WGUserDetails userDetails = null;
        Map<Object,Object> userCache = null;
        if (this.userCachesEnabled) {
            userCache = getUserCache().getMapForUser(getUserCacheKey(authSession));
            userDetails = (WGUserDetails) userCache.get(USERCACHE_USERDETAILS);
            if (userDetails != null && !userDetails.isOutdated()) {
                return userDetails;
            }
        }

        WGACL acl = getACL();
        Map users = acl.getUsers();
        Set entries = users.keySet();
        Set roles = new HashSet();
        List<String> allMatchingEntries = new ArrayList<String>();
        List<String> bestMatchingEntries = new ArrayList<String>();
        boolean inheritRoles = true;

        // Try to find matching username (always has highest priority for
        // accessLevelEntry)
        String matchingEntryName = (String) WGUtils.containsAny(entries, authSession.getNames());
        if (matchingEntryName != null) {
            WGACLEntry accessLevelEntry = acl.getEntry(matchingEntryName);
            bestMatchingEntries.add(accessLevelEntry.getName());
            allMatchingEntries.add(accessLevelEntry.getName());
            WGACLEntryFlags parsedFlags = acl.parseFlags(accessLevelEntry);
            roles.addAll(parsedFlags.getRoles());
            if (parsedFlags.isNoRoleInheritance()) {
                inheritRoles = false;
            }
        }

        // Try to find matching groups (largest access level will be used as
        // accessLevelEntry)

        Set groups = new HashSet(entries);
        groups.retainAll(authSession.getGroups());
        boolean addToBestMatching = bestMatchingEntries.size() == 0;        
        if (groups.size() > 0) {
            Iterator groupsIt = groups.iterator();
            boolean stopInheritingRoles = false;
            while (groupsIt.hasNext()) {
                WGACLEntry currentEntry = (WGACLEntry) acl.getEntry((String) groupsIt.next());
                if (addToBestMatching) {
                    bestMatchingEntries.add(currentEntry.getName());
                }
                allMatchingEntries.add(currentEntry.getName());
                WGACLEntryFlags parsedFlags = acl.parseFlags(currentEntry);
                if (inheritRoles) {
                    roles.addAll(parsedFlags.getRoles());
                }
                if (parsedFlags.isNoRoleInheritance()) {
                    stopInheritingRoles = true;
                }

            }

            if (stopInheritingRoles) {
                inheritRoles = false;
            }

        }

        // If User != anonymous he is member of the predefined group
        // "authenticated"
        if (!WGDatabase.ANONYMOUS_USER.equals(authSession.getDistinguishedName())) {
            WGACLEntry authenticatedEntry = acl.getEntry(AUTHENTICATED_GROUP);
            if (authenticatedEntry != null) {
                allMatchingEntries.add(authenticatedEntry.getName());
                WGACLEntryFlags parsedFlags = acl.parseFlags(authenticatedEntry);

                if (inheritRoles) {
                    roles.addAll(parsedFlags.getRoles());
                }

                if (bestMatchingEntries.size() == 0) {
                    bestMatchingEntries.add(authenticatedEntry.getName());
                }

                if (parsedFlags.isNoRoleInheritance()) {
                    inheritRoles = false;
                }
            }
        }

        // Try to find default entry.
        WGACLEntry defaultEntry = acl.getEntry("*");
        if (defaultEntry != null) {
            allMatchingEntries.add(defaultEntry.getName());
            if (inheritRoles) {
                roles.addAll(acl.parseFlags(defaultEntry).getRoles());
            }

            if (bestMatchingEntries.size() == 0) {
                bestMatchingEntries.add(defaultEntry.getName());
            }
        }

        // Of all best matching entries: Find the highest access level, collect all flags (we will only use privileges since roles are collected separately)
        int accessLevel = WGDatabase.ACCESSLEVEL_NOACCESS;
        WGACLEntryFlags flags = null;
        for (String entryName : bestMatchingEntries) {
            WGACLEntry entry = acl.getEntry(entryName);
            if (entry.getLevel() > accessLevel) {
                accessLevel = entry.getLevel();
            }
            WGACLEntryFlags entryFlags = acl.parseFlags(entry);
            if (flags == null) {
                flags = entryFlags;
            }
            else {
                flags.mergeFlags(entryFlags);
            }
        }

        // Feed cache
        String authSource = "(none)";
        if (_authenticationModule != null) {
            authSource = _authenticationModule.getAuthenticationSource();
        }
        userDetails = new WGUserDetails(accessLevel, authSession.getDistinguishedName(), authSession.getMailAddress(), authSource, authSession.getNames(), authSession.getGroups(), roles,
                allMatchingEntries, flags);

        if (authSession instanceof LabeledNamesProvider) {
            userDetails.setLabeledNames(((LabeledNamesProvider) authSession).getLabeledNames());
        }

        if (userCache != null) {
            userCache.put(USERCACHE_USERDETAILS, userDetails);
        }
        return userDetails;

    }

    protected String getUserCacheKey(AuthenticationSession authSession) {
        
        StringBuffer key = new StringBuffer();
        
        if (authSession instanceof AuthSessionWithUserCacheQualifier) {
            String cacheQualifier = ((AuthSessionWithUserCacheQualifier) authSession).getCacheQualifier();
            if (cacheQualifier != null) {
                key.append(cacheQualifier).append("/////");
            }    
        }
        
        key.append(authSession.getDistinguishedName());
        return key.toString();
        
    }

    /**
     * Default implementation for determining user list membership. This may be
     * used by DB-Implementations that return {@link WGUserDetails} to specify
     * detailed user information. This method is not intended for direct usage
     * outside of the WGAPI.
     * 
     * @param userList
     *            A list of user/group/rolenames to determine membership for.
     * @return true, if the current user is member, false if not.
     * @throws WGAPIException
     */
    public boolean defaultIsMemberOfUserList(List userList) throws WGAPIException {

        if (getSessionContext().isMasterSession()) {
            return true;
        }
        
        WGUserDetails userDetails = getSessionContext().getUserDetails();
        if (userDetails == null) { // Only possible when no authentication configured
            return true;
        }

        return defaultIsMemberOfUserList(userList, userDetails);

    }

    /**
     * Default implementation for determining user list membership against any custom user details object.
     * @param userListOrig A list of user/group/rolenames to determine membership for. 
     * @param userDetails The user details representing the users authorisations
     * @return true, if the current user is member, false if not.
     */
    public static boolean defaultIsMemberOfUserList(List userListOrig, WGUserDetails userDetails) {
        
        if (userListOrig.size() == 0) {
            return false;
        }

        List userList = WGUtils.toLowerCase(userListOrig);
        
        if (userList.contains(userDetails.getPrimaryName().toLowerCase())) {
            return true;
        }

        if (WGUtils.containsAny(userList, WGUtils.toLowerCase(userDetails.getAliases())) != null) {
            return true;
        }

        // Test groups
        if (WGUtils.containsAny(userList, WGUtils.toLowerCase(userDetails.getGroups())) != null) {
            return true;
        }

        // Test roles
        if (WGUtils.containsAny(userList, WGUtils.toLowerCase(userDetails.getRoles())) != null) {
            return true;
        }

        // Test predefined groups
        if (!userDetails.getPrimaryName().equalsIgnoreCase(WGDatabase.ANONYMOUS_USER) && userList.contains(WGDatabase.AUTHENTICATED_GROUP)) {
            return true;
        }
        
        // Test access level groups
        for (AccessLevel level : REAL_ACCESSLEVELS.values()) {
            if (userDetails.getAccessLevel() >= level.getCode()) {
                if (userList.contains(level.getGroupName())) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Returns if this database allows modification of design documents for the
     * current session This method returs false if any of the doctypes file
     * container, script or tml module are not modifiable. For getting more
     * detailed information the method {@link #isDoctypeModifiable(int)} should
     * be used.
     */
    public boolean isAllowDesignModification() {

        if (getSessionContext() != null && getSessionContext().isMasterSession()) {
            return true;
        }

        return isDoctypeModifiable(WGDocument.TYPE_FILECONTAINER) || isDoctypeModifiable(WGDocument.TYPE_CSSJS) || isDoctypeModifiable(WGDocument.TYPE_TML);

    }

    /**
     * Sets if this database should allow modification of design documents This
     * call equals calling {@link #setDoctypeModifiable(int, boolean)} for the
     * doctypes file container, script and tml module to false.
     */
    public void setAllowDesignModification(boolean allowDesignUpdates) {
        setDoctypeModifiable(WGDocument.TYPE_TML, false);
        setDoctypeModifiable(WGDocument.TYPE_CSSJS, false);
        setDoctypeModifiable(WGDocument.TYPE_FILECONTAINER, false);
    }

    /**
     * Returns the names of all database attributes as a Set
     */
    public Set getAttributeNames() {
        return Collections.unmodifiableSet(customAttributes.keySet());
    }

    /**
     * Returns an external design provider that was set to this database, if
     * available
     */
    public WGDesignProvider getDesignProvider() {
        return designProvider;
    }

    /**
     * Sets an external design provider from which this database should retrieve
     * design documents. Which design document types are provided is up to the
     * provider.
     * 
     * @param designProvider
     *            The design provider to set.
     * @throws LockException
     */
    public void setDesignProvider(WGDesignProvider designProvider) throws WGAPIException {

        if (this.designProvider != null) {
            this.designProvider.removeDesignChangeListener(this);
        }

        this.designProvider = designProvider;

        if (this.designProvider.isNotifying()) {
            this.designProvider.addDesignChangeListener(this);
        }

        if (isConnected()) {
            refresh();
        }
    }

    /**
     * Converts the name of a file to attach to a format that is accepted by the
     * database backend
     * 
     * @param name
     *            file name
     * @return converted file name
     */
    protected String convertFileNameForAttaching(String name) {
        return getCore().convertFileNameForAttaching(name);
    }

    /**
     * Determines if certificate authentication is enabled for this database
     */
    public boolean certAuthEnabled() {
        
        // Overriding option COPTION_DISABLE_CERTAUTH has priority
        String disableCertAuth = (String) this.getCreationOptions().get(COPTION_DISABLE_CERTAUTH);
        if (disableCertAuth != null && disableCertAuth.equalsIgnoreCase("true")) {
            return false;
        }
        
        // Faked cert auth
        String fakeCertAuth = (String) this.getCreationOptions().get(COPTION_FAKE_CERTAUTH);
        if ("true".equals(fakeCertAuth)) {
            return true;
        }
        
        // If auth module is cert auth capable we return its status
        AuthenticationModule authModule = getAuthenticationModule();
        if (authModule instanceof CertAuthCapableAuthModule) {
            CertAuthCapableAuthModule certMod = (CertAuthCapableAuthModule) authModule;
            return certMod.isCertAuthEnabled();
        }
        
        // No cert auth
        return false;
    }

    



    



    /**
     * Sets an option to a map if it is not already set
     * 
     * @param options
     *            the map to use
     * @param key
     *            the key to set to the map
     * @param value
     *            the value to write to the map
     */
    public static void putDefaultOption(Map options, String key, Object value) {

        if (!options.containsKey(key)) {
            options.put(key, value);
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.locking.Lockable#lock(de.innovationgate
     * .webgate.api.locking.LockOwner)
     */
    public void lock(LockOwner owner) throws WGAPIException {
        getLockManager().obtainLock(this, owner);
    }

    /**
     * locks the database for the current sessioncontext
     * 
     * @throws WGAPIException
     */
    public void lock() throws WGAPIException {
        WGSessionContext sessionContext = getSessionContext();
        lock((LockOwner) sessionContext);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * de.innovationgate.webgate.api.locking.Lockable#unlock(de.innovationgate
     * .webgate.api.locking.LockOwner)
     */
    public void unlock(LockOwner owner) {
        getLockManager().releaseLock(this, owner);
    }

    /**
     * unlocks the database for the current sessioncontext
     */
    public void unlock() {
        WGSessionContext sessionContext = getSessionContext();
        unlock(sessionContext);
    }

    /*
     * (non-Javadoc)
     * 
     * @seede.innovationgate.webgate.api.locking.Lockable#getLockStatus(de.
     * innovationgate.webgate.api.locking.LockOwner)
     */
    public int getLockStatus(LockOwner owner) throws WGAPIException {
        return getLockManager().getLockStatus(this, owner);
    }

    /**
     * gets the lockstatus for the current sessioncontext
     * 
     * @return A value indication the lock status. See constants on
     *         de.innovationgate.webgate.api.locking.Lock.
     * @throws WGAPIException
     */
    public int getLockStatus() throws WGAPIException {
        return getLockStatus(getSessionContext());
    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.locking.Lockable#getParentLockable()
     */
    public Lockable getParentLockable() {
        return null; // db has no parent
    }

    protected LockManager getLockManager() {
        return _lockManager;
    }

    public void designChanged(final WGDesignChangeEvent event) {

        // If this is not yet connected there is no need for cache maintenance
        if (!isConnected()) {
            return;
        }

        WGDesignProvider designProvider = event.getDesignProvider();

        // If the source is a virtual design provider, then
        // cache management will happen automatically by WGDocument.save()
        if (designProvider instanceof WGVirtualDesignProvider) {
            return;
        }

        MasterSessionTask task = new MasterSessionTask(this) {
            protected void exec(WGDatabase db) throws Throwable {
                processChanges(event.getUpdateLogs());
            }
        };

        if (!task.run()) {
            WGFactory.getLogger().error("Exception on design change processing", task.getThrowable());
        }
    }

    /**
     * Queries a user profile database for stored profiles, based on their data.
     * The query syntax is up to the profile database implementation.
     * 
     * @param type The type of the query
     * @param query Query to use for searching user profiles
     * @param params Parameters given to the query
     * @return A list of names of user profiles whose profile data match the query
     * @throws WGQueryException
     */
    public List queryUserProfileNames(String type, String query, Map params) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (params == null) {
            params = new HashMap();
        }

        return getPersonalisationCore().queryUserProfileNames(type, query, params);

    }
    
    /**
     * Returns the names of all user profiles that are stored in the personalisation database
     * @throws WGNotSupportedException If the database is not personalisation database
     * @throws WGAPIException
     */
    public Iterator getAllUserProfileNames() throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getPersonalisationCore().getAllUserProfileNames();
        
    }
 
    /**
     * Queries a user profile database for stored profiles, based on their data.
     * The query syntax is up to the profile database implementation.
     * This variant uses the default query language for profile queries without parameters
     * 
     * @param query Query to use for searching user profiles
     * @return A list of names of user profiles whose profile data match the query
     * @throws WGQueryException
     */
 
    public List queryUserProfileNames(String query) throws WGAPIException {
        return queryUserProfileNames(null, query, null);
    }

    /**
     * Returns if this database is already connected to its backend. This may be
     * false, when the database was opened with WGFactory.prepareDatabase() and
     * not session has been opened since.
     * 
     */
    public boolean isConnected() {
        return connected;
    }

    /**
     * A metadata module is a special version of a script module that is used to
     * store metadata about the state of the database. It has some special
     * behaviours: - It is never stored or retrieved from a design provider but
     * always from the original db (even if the design provider target hosts
     * metadata modules) - It is not synchronized or migrated along with other
     * data - It is always of code type XML Metadata modules are stored
     * technically as normal script modules with a prefix qualifier, retrieved
     * from constant WGDatabase.METADATA_MODULE_QUALIFIER.
     * 
     * @param name
     *            The name of the metadata module
     * @return A metadata module
     * @throws WGAPIException
     */
    public WGScriptModule getMetadataModule(String name) throws WGAPIException {
        return getScriptModule(WGCSSJSModule.METADATA_MODULE_QUALIFIER + name, WGScriptModule.CODETYPE_XML);
    }

    /**
     * Creates a new metadata module. For details about metadata modules see
     * method getMetadataModule.
     * 
     * @param name
     *            Name of the module to create
     * @return The created module
     * @throws LockException
     * @throws WGCreationException
     * @throws WGAuthorisationException
     */
    public WGCSSJSModule createMetadataModule(String name) throws WGAPIException {
        return createCSSJSModule(WGCSSJSModule.METADATA_MODULE_QUALIFIER + name, WGCSSJSModule.CODETYPE_XML);
    }

    /**
     * Returns the user cache latency in minutes. This is the time after which
     * all user specific caches get discarded automatically to allow changes in
     * the authentication backend to take effect. This setting can be specified
     * via creation option COPTION_USERCACHELATENCY. If it is 0 the user caches
     * are kept eternal unless the database data changes.
     */
    public int getUserCacheLatency() {
        return userCacheLatency;
    }

    /**
     * Clears all user-specific caches. These caches normally contain either
     * cached authentication/authorisation information or links to content
     * documents that these users are allowed to see.
     */
    public void clearUserCaches() {
        userHashMapGroup.clearAllMaps();
    }

    /**
     * Returns the currently configured behaviour of this database regarding the
     * request of nonexistent items
     */
    public NoItemBehaviour getNoItemBehaviour() {
        return noItemBehaviour;
    }

    protected Collator getDefaultLanguageCollator() {
        if (_defaultLanguageCollator == null) {
            try {
                _defaultLanguageCollator = Collator.getInstance(WGLanguage.languageNameToLocale(getDefaultLanguage()));
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error determining default language collator. Using default platform collator", e);
                _defaultLanguageCollator = Collator.getInstance();
            }
        }
        return _defaultLanguageCollator;
    }

    /**
     * Returns if users with reader access to this profile database may create
     * profile documents. This setting is effective when WGA Content Stores are
     * used directly as personalisation databases, where the Content Store ACL
     * also is effective for profile creation (while normal personalisation
     * databases are always opened via master login).
     */
    public boolean isReaderProfileCreation() {
        return readerProfileCreation;
    }

    /**
     * Returns a list of all content keys in the current database. This method
     * may use optimized backend operations for this task, which do not need to
     * traverse through WGAPI documents.
     * 
     * @param includeArchived
     *            Specify true, to also retrieve content keys for archived
     *            contents
     * @return List of content key objects {@link WGContentKey}
     * @throws WGAPIException
     */
    public List getAllContentKeys(boolean includeArchived) throws WGAPIException {

        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        if (!hasFeature(FEATURE_RETRIEVE_ALL_CONTENTKEYS)) {
            throw new WGNotSupportedException("This operation is not supported by this WGAPI implementation");
        }

        return getCore().getAllContentKeys(includeArchived);

    }

    /**
     * Returns the system time where the last cache maintenance happened
     */
    public Date getLastCacheMaintenance() {
        if (lastCacheMaintenance != null) {
            return lastCacheMaintenance;
        }
        else {
            return new Date(Long.MIN_VALUE);
        }
    }

    /**
     * Notify database of the beginning of update operations via this
     * connection. This is only neccessary in cluster environments when the
     * underlying database connections are kept "readOnly" until a real update
     * happens.
     */
    public void beginUpdate() throws WGAPIException {
        getCore().beginUpdate();
    }

    /**
     * Returns if backend deletion checks are enabled, see
     * {@link #COPTION_DELETIONCHECK}
     */
    public boolean isDeletionCheck() {
        return deletionCheck;
    }

    public Class getChildNodeType() {
        return DocumentCollectionHierarchy.class;
    }

    public List getChildNodes() throws WGAPIException {
        return _allDocumentsHierarchy.getChildNodes();
    }
    
    @Override
    public SkippingIterator<PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
        return new SkippingIteratorWrapper<PageHierarchyNode>(_allDocumentsHierarchy.getChildNodes().iterator());
    }

    public String getNodeKey() {
        return "db/" + getDbReference();
    }

    public String getNodeTitle(String language) throws WGAPIException {
        return getTitle();
    }

    public PageHierarchyNode getParentNode() {
        return null;
    }

    /**
     * Returns if project mode is enabled, see {@link #COPTION_PROJECTMODE}
     */
    public boolean isProjectMode() {
        return projectMode;
    }

    /**
     * Registers an action that should be executed when the database is
     * connected. If it already is connected the action is executed at once and
     * the method returns true. If it is not yet connected but only prepared for
     * connecting the method returns false. In that case the action is executed
     * at the time the database will be connected. This action is always
     * executed under master session rights. Exceptions occurring in the action
     * will be logged but never thrown to the outside.
     * 
     * @param action
     *            The action to execute.
     * @return true if the action was executed immediately, false if it is not.
     */
    public synchronized boolean onConnect(DatabaseAction action) {

        if (isConnected()) {
            try {
                action.run(this);
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error executing connect action", e);
            }
            return true;
        }
        else {
            _connectActions.add(action);
            return false;
        }

    }
    
    /**
     * @deprecated Use {@link #onConnect(DatabaseAction)}
     */
    public synchronized boolean onConnect(ConnectAction action) {
        return onConnect((DatabaseAction) action);
    }
    
    protected synchronized boolean onOpen(DatabaseAction action) {

        if (isConnected()) {
            try {
                action.run(this);
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error executing connect action", e);
            }
            return true;
        }
        else {
            _openActions.add(action);
            return false;
        }

    }

    /**
     * Returns if automatic approval is enabled, see
     * {@link #COPTION_AUTOAPPROVE}.
     */
    public boolean isAutoApprove() {
        return autoApprove;
    }

    /**
     * Sets if automatic approval is enabled
     */
    public void setAutoApprove(boolean autoApprove) {
        this.autoApprove = autoApprove;
    }

    /**
     * Sets if documents of a given doctype are modifiable for non-master
     * sessions Functionalities that feed the design of a database with external
     * data might want to disable modifiability of those types that are served.
     * This functionality is only effective for design and schema documents,
     * those that are retrievable via
     * {@link #getDesignObject(int, String, String)}.
     * 
     * @param type
     *            The doctype to set. Use WGDocument.TYPE_... constants
     * @param modifiable
     *            Whether to set the doctype modifiable or unmodifiable
     */
    public void setDoctypeModifiable(int type, boolean modifiable) {

        if (modifiable) {
            this.unmodifiableDoctypes.remove(new Integer(type));
        }
        else {
            this.unmodifiableDoctypes.add(new Integer(type));
        }
    }

    /**
     * Tests if documents of a given doctypes are modifiable for the current
     * session
     * 
     * @param type
     *            The type to test
     * @return true, if docs of that type are modifiable
     */
    public boolean isDoctypeModifiable(int type) {

        // Master session may modify anything
        if (getSessionContext() != null && getSessionContext().isMasterSession()) {
            return true;
        }

        // If the doctype is provided by a design provider, it is not modifiable
        // by users
        WGDesignProvider provider = getDesignProvider();
        if (provider != null) {
            boolean providesType = provider.providesType(type);
            if (providesType == true) {
                return false;
            }
        }

        // Check for manually set unmodifiability
        return !this.unmodifiableDoctypes.contains(new Integer(type));
    }

    /**
     * Returns a content with the given name in any language. Prefers the
     * default language of the database. If no content with that name is found
     * it tries every language defined in the database
     * 
     * @param name
     *            The name to search
     * @return The content if any was found or null
     * @throws WGAPIException
     */
    public WGContent getAnyContentByName(String name) throws WGAPIException {

        // Try default language first
        WGContent content = getContentByName(name);
        if (content != null) {
            return content;
        }

        // Iterate over languages and return the first match
        Iterator langs = getLanguages().values().iterator();
        while (langs.hasNext()) {
            WGLanguage lang = (WGLanguage) langs.next();
            content = getContentByName(name, lang.getName());
            if (content != null) {
                return content;
            }
        }

        return null;

    }

    /**
     * Manually set a default language for this database
     * 
     * @param defaultLanguage
     *            The default language
     */
    public void setDefaultLanguage(String defaultLanguage) {
        _defaultLanguage = defaultLanguage;
        _defaultLanguageCollator = Collator.getInstance(WGLanguage.languageNameToLocale(_defaultLanguage));
    }

    /**
     * Adds a design change listener to this database. If the database uses a
     * design provider it is registered with the provider. Otherwise it is
     * registered as normal WGDatabaseEventListener. Not all design change
     * events will carry information about the document being updated.
     * 
     * @param changeListener
     */
    public void addDesignChangeListener(WGDesignChangeListener changeListener) {
        if (getDesignProvider() != null) {
            getDesignProvider().addDesignChangeListener(changeListener);
        }
        else {
            this.databaseDesignChangeListeners.add(changeListener);
        }
    }

    /**
     * Removes a design change listener to this database. If the database uses a
     * design provider it is removed from the provider. Otherwise it is removed
     * as normal WGDatabaseEventListener.
     * 
     * @param changeListener
     */
    public void removeDesignChangeListener(WGDesignChangeListener changeListener) {
        if (getDesignProvider() != null) {
            getDesignProvider().removeDesignChangeListener(changeListener);
        }
        else {
            this.databaseDesignChangeListeners.remove(changeListener);
        }
    }

    private void fireDatabaseDesignChangedEvent(WGDesignChangeEvent e) {

        if (!isSessionOpen()) {
            return;
        }

        if (!getSessionContext().isEventsEnabled()) {
            return;
        }

        if (getDesignProvider() != null) {
            return;
        }
        
        Iterator listeners = this.databaseDesignChangeListeners.iterator();
        while (listeners.hasNext()) {
            WGDesignChangeListener listener = (WGDesignChangeListener) listeners.next();
            listener.designChanged(e);
        }
        
    }

    /**
     * Returns the script module of the given name and codetype
     * 
     * @param name
     * @param codetype
     * @throws WGAPIException
     */
    public WGScriptModule getScriptModule(String name, String codetype) throws WGAPIException {
        return getCSSJSModule(name, codetype);
    }
    
    /**
     * Returns the script module of the given name and any codetype
     * 
     * @param name
     * @throws WGAPIException
     */
    public WGCSSJSModule getScriptModule(String name) throws WGAPIException {
        return getCSSJSModule(name);
    }

    /**
     * Returns the script modules in this database.
     * 
     * @throws WGAPIException
     */
    public List<WGScriptModule> getScriptModules() throws WGAPIException {
        return getCSSJSModules();
    }

    /**
     * Create a new script module. Same as {@link #createCSSJSModule(String, String)} but better named.
     * @param name Name of the script module
     * @param type Code type of the module. Use constants WGScriptModule.CODETYPE_...
     * @return The newly created script module
     * @throws WGAPIException
     */
    public WGScriptModule createScriptModule(String name, String type) throws WGAPIException {
        return (WGScriptModule) createCSSJSModule(name, type);
    }

    protected int getListCacheRebuildThreshold() {
        return listCacheRebuildThreshold;
    }

    /**
     * Returns the operation keys of backend operations that are currently running
     * @return List of {@link WGOperationKey} objects
     */
    public List getCurrentBackendOperations() {

        List currentOps = new ArrayList();
        Iterator ops = operations.values().iterator();
        while (ops.hasNext()) {
            WGOperationKey op = (WGOperationKey) ops.next();
            if (op.isUsed()) {
                currentOps.add(op);
            }
        }
        return currentOps;

    }
    
    /**
     * Returns a map of operation keys currently or recently in usage
     */
    public Map getOperationKeys() {
        return Collections.unmodifiableMap(this.operations);
    }
    

    /**
     * Returns the operation keys of backend operations that have run and are not yet cleant up
     * @return List of {@link WGOperationKey} objects
     */
    public List getRecentBackendOperations() {
        List currentOps = new ArrayList(operations.values());
        return currentOps;
    }

    /**
     * Injects an authentication module to the database that should be used to authenticate users.
     * @param authenticationModule The module
     * @throws WGIllegalArgumentException
     */
    public void setAuthenticationModule(AuthenticationModule authenticationModule) throws WGIllegalArgumentException {
        
        closeAuthenticationModule();
        
        // Unpack the auth module to see its real capabilities
        AuthenticationModule backendModule = authenticationModule;
        while (backendModule instanceof RedirectionAuthModule) {
            backendModule = ((RedirectionAuthModule) backendModule).getBackendModule();
        }
        
        if (certAuthEnabled() && (!(backendModule instanceof CertAuthCapableAuthModule))) {
            throw new WGIllegalArgumentException("The given authentication module is not capable for certificate authentication, which is used by database '" + getDbReference() + "':" + authenticationModule.getAuthenticationSource());
        }
        
        _authenticationModule = authenticationModule;
        _authenticationModule.addAuthenticationSourceListener(this);
    }

    public void authenticationDataChanged() {
        getUserCache().clear();
    }
    
    /**
     * Returns the version of content store that this database represents, i.e the version of its storage format.
     * The content store version equals the (Open)WGA feature version which introduced this storage format. Returns constants CSVERSION_...
     * @throws WGAPIException
     */
    public double getContentStoreVersion() throws WGAPIException {
        return _csVersion;
    }
    
    /**
     * Returns the patch level of this content store. The patch level indicates automatic updates to the storage format of the content store which were applied.
     * Generally the patch level complements the content store version, fetchable via {@link #getContentStoreVersion()}, to indicate the feature set of the content store.
     * This is the patch level that is currently active on the connected database. If the backend patch level changed since the last reconnect this will not be reflected. 
     * @throws WGAPIException
     */
    public int getContentStorePatchLevel() throws WGAPIException {
        return _csPatchLevel;
    }

    protected AllDocumentsHierarchy getAllDocumentsHierarchy() {
        return _allDocumentsHierarchy;
    }
    
    /**
     * Implements the visitor pattern on the page hierarchy. The visitor visits all areas, all of their struct entries and all of their (non-archived) contents.
     * @param visitor
     * @throws WGAPIException
     */
    public void visit(WGPageVisitor visitor) throws WGAPIException {
        
        Iterator<WGArea> areas = getAreas().values().iterator();
        while (areas.hasNext()) {
            WGArea area = areas.next();
            area.visit(visitor);
            
        }
        
    }

    /**
     * Returns the server used to connect to this database
     */
    public WGDatabaseServer getServer() {
        return server;
    }
    
    
    /**
     * Completely clears this document of all content related documents, which is all contents, struct entries, areas, content types and languages
     * WARNING (in case this has not yet become clear): This single method call will delete ALL documents of the given types in the database!
     * @throws WGAPIException 
     */
    public void clearContentData() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_MANAGER) {
            throw new WGAuthorisationException("This operation only may be done by a manager", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
        }
        
        Iterator areas = getAreas().values().iterator();
        while (areas.hasNext()) {
            WGArea area = (WGArea) areas.next();
            area.remove();
        }
        
        Iterator cts = getContentTypes().iterator();
        while (cts.hasNext()) {
            WGContentType ct = (WGContentType) cts.next();
            ct.remove();
        }

        Iterator langs = getLanguages().values().iterator();
        while (langs.hasNext()) {
            WGContentType ct = (WGContentType) langs.next();
            ct.remove();
        }
        
    }


    
    /**
     * Returns if the database can be accessed by anonymous users
     * @throws WGBackendException
     */
    public boolean isAnonymousAccessible() throws WGAPIException {
        // check if file is anonymous accessible
        
        WGACL acl = getACL();
        if(acl==null)
        	return true; 
        
        boolean isAnonymousAccessible = false;
        // first check anonymous db access
        WGACLEntry anonymousEntry = acl.getEntry(WGDatabase.ANONYMOUS_USER);
        if (anonymousEntry != null && anonymousEntry.getLevel() >= WGDatabase.ACCESSLEVEL_READER) {
            isAnonymousAccessible = true;
        } else if (anonymousEntry == null) {
            WGACLEntry defaultEntry = acl.getEntry("*");
            if (defaultEntry != null && defaultEntry.getLevel() >= WGDatabase.ACCESSLEVEL_READER) {
                isAnonymousAccessible = true;    
            }
        }
        return isAnonymousAccessible;
    }
    
    /**
     * Enforces the given schema definition on the database
     * @param schemaDef
     * @throws WGAPIException
     */
    public void enforceSchema(WGSchemaDefinition schemaDef) throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (!getSessionContext().isMasterSession()) {
            throw new WGIllegalStateException("Enforcing schema is only possible under master session");
        }
        
        if (!hasFeature(FEATURE_FULLCONTENTFEATURES)) {
            throw new WGIllegalStateException("Enforcing schema is only possible on full WGA Content Stores");
        }
        
        this.schemaDefinition = schemaDef;
        boolean languageCreated = false;
        
        for (WGSchemaDocumentDefinition docDef : this.schemaDefinition.getDocumentDefinitionsCache().values()) {
            if (docDef.isAutoCreate()) {
                WGDocument newDoc = null;
                if (docDef instanceof WGContentTypeDefinition) {
                    WGDocument ct = getContentType(docDef.getName());
                    if (ct == null) {
                        WGFactory.getLogger().info("Creating content type '" + docDef.getName() + "' from schema definition");
                        newDoc = createContentType(docDef.getName());
                    }
                }
                else if (docDef instanceof WGAreaDefinition) {
                    WGArea area = getArea(docDef.getName());
                    if (area == null) {
                        WGFactory.getLogger().info("Creating area '" + docDef.getName() + "' from schema definition");
                        newDoc = createArea(docDef.getName());
                    }
                }
                else if (docDef instanceof WGLanguageDefinition) {
                    WGLanguage lang = getLanguage(docDef.getName());
                    if (lang == null || lang.isDummy()) {
                        WGFactory.getLogger().info("Creating language '" + docDef.getName() + "' from schema definition");
                        newDoc = createLanguage(docDef.getName());
                        languageCreated = true;
                    }
                }
                
                if (newDoc != null) {
                    newDoc.save();
                } 
            }
        }
        
        if (this.schemaDefinition.isCreateDefaultLanguage()) {
            if (getLanguages().size() == 0) {
                WGFactory.getLogger().info("Creating default language from schema definition");
                WGLanguage lang = createLanguage(getDefaultLanguage());
                lang.save();
            }
        }
        
        // If a language was created and the default language of this db was automatically determined we now redetermine it
        if (languageCreated && _defaultLanguageAutoDetermined) {
            determineDefaultLanguage();
        }
        
        
    }

    /**
     * Returns the schema definition in effect on this database
     */
    public WGSchemaDefinition getSchemaDefinition() {
        return schemaDefinition;
    }
    
    /**
     * Creates a schema definition containing all content types and areas in this database with their current metadata.
     * @throws WGAPIException 
     */
    public WGSchemaDefinition createSchemaDefinition() throws WGAPIException {
        
        WGSchemaDefinition schemaDef = new WGSchemaDefinition();
        for (WGContentType ct : getContentTypes()) {
            schemaDef.addDocumentDefinition(ct.createSchemaDefinition());
        }
        for (WGArea area : getAreas().values()) {
            schemaDef.addDocumentDefinition(area.createSchemaDefinition());
        }
        for (WGLanguage lang : getLanguages().values()) {
            schemaDef.addDocumentDefinition(lang.createSchemaDefinition());
        }
        
        return schemaDef;
        
    }
    
    /**
     * Lowercases a metadata field value for this database if metadata lowercasing is not disabled
     * @param meta The value to conditionally lowercase
     */
    public String toLowerCaseMeta(String meta) {
        
        if (!hasFeature(WGDatabase.FEATURE_DISABLE_META_LOWERCASING)) {
            return meta.toLowerCase();
        }
        else {
            return meta;
        }
        
    }

    /**
     * Returns if visibility constraints of the hierarchical reader fields on areas and pages are enabled
     */
    public boolean isPageReadersEnabled() {
        return pageReadersEnabled;
    }

    /**
     * Returns a list of readers that automatically have read access to all read-protected documents  
     */
    public List<String> getMandatoryReaders() {
        return _mandatoryReaders;
    }
    
    /**
     * Returns if a backend service is supported
     * @param serviceName Name of the service
     */
    public boolean isBackendServiceSupported(String serviceName) throws WGAPIException {
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getCore().isBackendServiceSupported(serviceName);
    }
    
    /**
     * Calls a backend service whose support is optional. Should throw WGNotSupportedException if the backend service is not supported.
     * Calling backend services is only allowed on master sessions.
     * @param serviceName The service name. Use WGDatabase.BACKENDSERVICE_*
     * @param params The matching parameters for the called service
     * @return An optional return value of the service
     */
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException {
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (!USER_BACKENDSERVICES.contains(serviceName) && !getSessionContext().isMasterSession()) {
            throw new WGAuthorisationException("This backend service may only get called on master sessions");
        }
        
        if (params == null) {
            params = new Object[0];
        }
        
        Object result = getCore().callBackendService(serviceName, params);
        
        // Subsequent cleanup operations per service
        if (BACKENDSERVICE_CLEAR_CONTENT.equals(serviceName) || BACKENDSERVICE_CLEAR_CONTENT_AND_SCHEMA.equals(serviceName)) {
            refresh();
        }
        else if (BACKENDSERVICE_CLEAR_DATABASE.equals(serviceName)) {
            refresh();
            updateRevision(WGDatabaseRevision.forValue(getCore().getRevision()));
        }
        else if (BACKENDSERVICE_CLEAR_USERPROFILES.equals(serviceName)) {
        }
        else if (BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS.equals(serviceName)) {
        }
        
        return result;
    }
    
    /**
     * Fetches all content documents pending release and releases those that are ready to be released
     * @throws WGAPIException 
     */
    public void releasePendingContents() throws WGAPIException {
        
        if (!isBackendServiceSupported(WGDatabase.BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS)) {
            return;
        }
        
        WGAbstractResultSet resultSet = (WGAbstractResultSet) callBackendService(WGDatabase.BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS, null);
        for (WGContent content : resultSet) {
            if (content.getValidFrom() == null) {
                content.release("Released by pending release publishing task on " + WGUtils.DATEFORMAT_STANDARD.format(new Date()), content.getWorkflow(), "Released");
                content.save();
                WGFactory.getLogger().info("Releasing content '" + content.getTitle() + "' (" + getDbReference() + "/" + content.getContentKey().toString() + ") which was pending without defined validity period");
            }
            else if (!content.getValidFrom().after(new Date())) {
                content.release("Released by pending release publishing task on " + WGUtils.DATEFORMAT_STANDARD.format(new Date()), content.getWorkflow(), "Released");
                content.save();
                WGFactory.getLogger().info("Releasing content '" + content.getTitle() + "' (" + getDbReference() + "/" + content.getContentKey().toString() + ") because its validity period (from " + (new SimpleDateFormat("dd.MM.yyyy HH:mm")).format(content.getValidFrom()) + ") has begun.");
            }
        }
        
        
    }
    
    protected Semaphore getWgOperationKeySemaphore() {
        return _wgOperationKeySemaphore;
    }

    protected boolean isMaintainOperationKeys() {
        return _maintainOperationKeys;
    }
    
    /**
     * Enforces the given version compliance behaviour on this database
     * @param compliance Compliance string from CSConfig.VERSIONCOMPLIANCE_*
     * @param enforceNoItemBehaviour If the behaviour regarding nonexistent items for the compliance should be enforced. May be false if this was already set by a higher priority configuration.
     */
    public void enforceVersionCompliance(String compliance, boolean enforceNoItemBehaviour) {
        if (enforceNoItemBehaviour) {
            getNoItemBehaviour().compliantTo(compliance);
        }
        _complianceVersion = CSConfig.getComplianceVersion(compliance);        
    }

    /**
     * Returns the WGA version whose behaviour is enforced on this database via {@link #enforceVersionCompliance(String, boolean)}
     */
    public Version getComplianceVersion() {
        return _complianceVersion;
    }

    public void setOverlayComplianceVersion(String compliance) {
        _overlayComplianceVersion = CSConfig.getComplianceVersion(compliance);        
    }

    /**
     * Returns the WGA version whose behaviour is enforced on this database via {@link #enforceVersionCompliance(String, boolean)}
     */
    public Version getOverlayComplianceVersion() {
        return _overlayComplianceVersion;
    }

    /**
     * Returns a universal ID for the current database.
     * This ID is stored on the data backend as database extension data field and therefor really identifies the backend databases data set rather than the WGAPI database instance.
     * It is initialized on the first connection that is done via WGAPI to this database. Only OpenWGA Content Stores of version 5 or higher own a UUID. On other databases this returns null.
     * @throws WGAPIException 
     */
    public String getUUID() {
        return _uuid;
    }
    
    /**
     * Returns an WGAPI sequence object for the given sequence name. The sequence does not need to exist yet.
     * This feature is only available on WGA Content Stores of version 5 or higher.
     * @param sequenceName Name of the sequence
     * @throws WGAPIException
     */
    public WGSequence getSequence(String sequenceName) throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException("Sequences", WGDatabase.CSVERSION_WGA5);
        }
        
        return new WGSequence(this, sequenceName);
    }
    
    /**
     * Returns the names of sequences that are initialized and/or in use
     * @throws WGAPIException
     */
    public List<String> getUsedSequenceNames() throws WGAPIException {
        
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException("Sequences", WGDatabase.CSVERSION_WGA5);
        }
        
        return ((WGDatabaseCoreFeatureSequenceProvider) getCore()).getUsedSequenceNames();
        
    }

    
    /**
     * checks for backend changes
     * @return revision
     * @throws WGAPIException
     */
    public Comparable catchupBackendChanges() throws WGAPIException {
        if (isDatabaseUpdatedInBackground()) {
            MasterSessionTask masterTask = new MasterSessionTask(this) {            
                @Override
                protected void exec(WGDatabase db) throws Throwable {
                    db.getSessionContext().setTask("CatchUp Backend Changes");
                    db.getSessionContext().setBatchProcess(true);
                    
                    // Since we manage the cache here, we need caching although having a batch process
                    db.getSessionContext().setCachingEnabled(true);
                    
                    db.checkDatabaseUpdates(true);                   
                }
            };
            try {
                masterTask.runWithExceptions();
            } catch (WGAPIException e) {
                throw e;
            }
            catch (Throwable e) {
               throw new WGAPIException("Error retrieving backend changes", e);
            }                    
        }
        return getRevision();
    }
    
    
   
    /**
     *Sets annotators which are called when files are attached to this database and is able to annotate the file with additional information
     * @param annotators The annotators
     */
    public void setFileAnnotators(List<WGFileAnnotator> annotators) {
       List<WGFileAnnotator> newAnnotators = new ArrayList(annotators);
       Collections.sort(newAnnotators, new Comparator<WGFileAnnotator>() {

            @Override
            public int compare(WGFileAnnotator o1, WGFileAnnotator o2) {
                return new Integer(o1.getOrderNr()).compareTo(o2.getOrderNr());
            }
            
       });
        
       this.fileAnnotators = newAnnotators;
    }
    
    /**
     * Returns the currently active file annotators on this database
     */
    public List<WGFileAnnotator> getFileAnnotators() {
        return Collections.unmodifiableList(this.fileAnnotators);
    }

    /**
     * Returns the list of content event listeners for this database. This list
     * is the actual event listener list used by this WGDatabase. Code using
     * iterators of this list should synchronize against the list
     */
    public List getContentEventListeners() {
        return contentEventListeners;
    }
    
    /**
     * Tool method for annotating a metadata object based on the given file, using the default and optionally given additional annotators
     * @param file The file data
     * @param meta The metadata object to annotate
     * @param additionalAnnotators Additional annotators to run.
     * @throws WGAPIException
     */
    public void annotateMetadata(final File file, WGFileMetaData meta, List<WGFileAnnotator> additionalAnnotators) throws WGAPIException {
        
        DataSource ds = new DataSource() {

            @Override
            public String getContentType() {
                return "application/octet-stream";
            }

            @Override
            public InputStream getInputStream() throws IOException {
                return new BufferedInputStream(new FileInputStream(file));
            }

            @Override
            public String getName() {
                return file.getName();
            }

            @Override
            public OutputStream getOutputStream() throws IOException {
                throw new IOException("This data source does not provide an output stream");
            }
            
        };
        
        annotateMetadata(ds, meta, additionalAnnotators);
    }

    /**
     * Tool method for annotating a metadata object based on the given file, using the default and optionally given additional annotators
     * @param ds The file data
     * @param meta The metadata object to annotate
     * @param additionalAnnotators Additional annotators to run.
     * @throws WGAPIException
     */
    protected void annotateMetadata(DataSource ds, WGFileMetaData meta, List<WGFileAnnotator> additionalAnnotators) throws WGAPIException {
        List<WGFileAnnotator> annotators = new ArrayList<WGFileAnnotator>(getFileAnnotators());
        if (additionalAnnotators != null) {
            annotators.addAll(additionalAnnotators);
        }
        
        for (WGFileAnnotator annotator : annotators) {
            try {
                annotator.annotateFile(ds, meta);
            }
            catch (Throwable e) {
                WGFactory.getLogger().error("Exception running annotator " + annotator.getClass().getName() + " on attaching file '" + ds.getName() + "'", e);
            }
        }
        
    }

    protected WGDocument.Cache getDocumentCache(WGDocument wgDocument) {
        
        WGDocument.Cache cache = this.masterDocumentCaches.get(wgDocument.getDocumentKeyObj());
        if (cache == null) {
            cache = wgDocument.createDocumentCache();
            this.masterDocumentCaches.put(wgDocument.getDocumentKeyObj(), cache);
        }
       
        return cache;
    }
    
    public void maintenance(boolean fullMaintenance) throws WGException {
        
        getUserHashMapGroup().maintenance(fullMaintenance);
        
        if (fullMaintenance) {
            
            masterDocumentCaches.maintenance();
            
            if (isBackendServiceSupported(WGDatabase.BACKENDSERVICE_DAILY_MAINTENANCE) && WGFactory.getInstance().isDatabaseBackendMaintenanceEnabled()) {
                callBackendService(WGDatabase.BACKENDSERVICE_DAILY_MAINTENANCE, null);
            }
        }
        
    }
    

    public PageRightsFilter getPageRightsFilter() {
        return _pageRightsFilter;
    }

    public void setPageRightsFilter(PageRightsFilter pageRightsFilter) throws WGAPIException {
        _pageRightsFilter = pageRightsFilter;
        refresh();
    }

    public void setFileConverter(WGFileConverter conv){
    	this.fileConverter = conv;
    }
    public WGFileConverter getFileConverter(){
    	return this.fileConverter;
    }

	public void createPageSequence(WGStructEntry struct, boolean forceCreate) throws WGAPIException, InstantiationException, IllegalAccessException {
		
        if (!isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        if(!(getCore() instanceof WGDatabaseCoreFeaturePageSequences))
        	throw new WGNotSupportedException("Page sequences are not supported for this database");

        if (getSessionContext().getAccessLevel() < ACCESSLEVEL_MANAGER) {
            throw new WGAuthorisationException("You are not authorized to create page sequences in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
        }

        ((WGDatabaseCoreFeaturePageSequences)getCore()).createPageSequence(struct.getCore(), forceCreate);
		struct.save();
	}
    
}
