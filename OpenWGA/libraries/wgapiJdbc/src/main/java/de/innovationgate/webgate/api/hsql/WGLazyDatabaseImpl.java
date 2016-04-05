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

package de.innovationgate.webgate.api.hsql;

import java.io.File;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGACL;
import de.innovationgate.webgate.api.WGACLCore;
import de.innovationgate.webgate.api.WGACLEntry;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabase.ReconfigurationAccessor;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureReturnHierarchyCount;
import de.innovationgate.webgate.api.WGDatabaseCoreFeatureSequenceProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGPageOrderSet;
import de.innovationgate.webgate.api.WGPersonalisationDatabaseCore;
import de.innovationgate.webgate.api.WGReconfigurableDatabaseCore;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGResultSetCore;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.WGWrongRevisionException;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.webgate.api.fake.WGFakeContentStore;
import de.innovationgate.webgate.api.fake.WGFakeContentStore.ACLEntryImpl;
import de.innovationgate.webgate.api.utils.MasterSessionTask;

/**
 * A version of the HSQLDB database store that does not connect any database if the
 * database is not yet present until data is actually stored.
 */
public class WGLazyDatabaseImpl implements WGDatabaseCore, WGPersonalisationDatabaseCore, WGDatabaseCoreFeatureReturnHierarchyCount, WGDatabaseCoreFeatureSequenceProvider, AuthenticationSourceListener, WGReconfigurableDatabaseCore {
    
    private volatile WGDatabaseCore _core;
    private volatile boolean _lazy = false;
    private WGDatabase _db;
    private String _path;
    private String _user;
    private String _pwd;
    private boolean _prepareOnly;
    private ReentrantReadWriteLock _theLock = new ReentrantReadWriteLock();
    private ReconfigurationAccessor _reconfigurationAccessor;
    
    public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException {
        
        _db = db;
        _path = path;
        _user = user;
        _pwd = pwd;
        _prepareOnly = prepareOnly;
        
        File propsFile = WGDatabaseImpl.getHsqlBaseFile(path + ".properties");
        WGUserAccess userAccess = connectSubCore(db, path, user, pwd, prepareOnly, propsFile);
        
        // If we have a database but it is empty we delete it and use the fake CS instead, connect again
        if (!_lazy && isEmpty()) {
            
            _core.close();
            WGFactory.getLogger().info("Deleting database files of '" + _db.getDbReference() + "' as the database is empty. Will use lazy database mode until data is stored.");
            String propsName = propsFile.getName().substring(0, propsFile.getName().lastIndexOf("."));
            for (File f : propsFile.getParentFile().listFiles()) {
                int dotPos = f.getName().lastIndexOf(".");
                if (dotPos != -1 && f.getName().substring(0, dotPos).equals(propsName)) {
                    f.delete();
                }
            }
            
            userAccess = connectSubCore(db, path, user, pwd, prepareOnly, propsFile);
        }
        
        return userAccess;
        
    }

    private WGUserAccess connectSubCore(WGDatabase db, String path, String user, String pwd, boolean prepareOnly, File propsFile) throws WGAPIException {
        if (propsFile.exists()) {
            _core = new WGDatabaseImpl();
        }
        else {
            _core = new WGFakeContentStore();
            _lazy = true;
        }

        WGUserAccess userAccess = _core.open(db, path, user, pwd, prepareOnly);
        return userAccess;
    }
    
    private boolean isEmpty() throws WGAPIException {
        return _core.getDesignObjects(WGDocument.TYPE_AREA).size() == 0 &&
                _core.getDesignObjects(WGDocument.TYPE_CONTENTTYPE).size() == 0 &&
                _core.getDesignObjects(WGDocument.TYPE_LANGUAGE).size() == 0 &&
                !((WGPersonalisationDatabaseCore) _core).getAllUserProfileNames().hasNext();
                
    }

    public WGUserAccess openSession(AuthenticationSession authSession, Object credentials, boolean master) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.openSession(authSession, credentials, master);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void close() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            _core.close();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void closeSession() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            _core.closeSession();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public String getTitle() throws WGBackendException {
        _theLock.readLock().lock();
        try {    
            return _core.getTitle();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public String getTypeName() {
        _theLock.readLock().lock();
        try {
            return _core.getTypeName();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Comparable getRevision() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getRevision();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Date getRevisionDate(Comparable lastChanged) throws WGAPIException, WGWrongRevisionException {
        _theLock.readLock().lock();
        try {
            return _core.getRevisionDate(lastChanged);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Object getExtensionData(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {    
            return _core.getExtensionData(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List<String> getExtensionDataNames() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getExtensionDataNames();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void writeExtensionData(String name, Object value) throws WGAPIException {
        switchToDatabaseCore();
        _theLock.readLock().lock();
        try {
            _core.writeExtensionData(name, value);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void removeExtensionData(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            _core.removeExtensionData(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List getRoles() {
        _theLock.readLock().lock();
        try {
            return _core.getRoles();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean hasFeature(String feature) {
        _theLock.readLock().lock();
        try {
            return _core.hasFeature(feature);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Iterator<WGDocumentCore> getChildEntries(WGStructEntry structEntry, WGPageOrderSet order) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getChildEntries(structEntry, order);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Iterator<WGDocumentCore> getRootEntries(WGArea area, WGPageOrderSet pageOrder) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getRootEntries(area, pageOrder);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getStructEntryByKey(Object key) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getStructEntryByKey(key);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getParentEntry(WGStructEntry entry) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getParentEntry(entry);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List<WGDocumentCore> getAllContent(WGStructEntry structEntry, boolean includeArchived) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getAllContent(structEntry, includeArchived);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getContentByKey(WGContentKey key) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getContentByKey(key);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getContentByName(String strName, String strLanguage) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getContentByName(strName, strLanguage);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getStructEntryByName(String strName) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getStructEntryByName(strName);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getDummyContent(String language) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getDummyContent(language);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGResultSetCore query(String type, String query, Map parameters) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.query(type, query, parameters);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore fastAccess(int type, Object key) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.fastAccess(type, key);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Object parseStructKey(String key) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.parseStructKey(key);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public synchronized WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAPIException {
        switchToDatabaseCore(); 
        _theLock.readLock().lock();
        try {
            return _core.createDesignDocument(type, name, mediaKey);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    private void switchToDatabaseCore() throws WGAPIException {

        if (!_lazy) {
            return;
        }
        
        _theLock.writeLock().lock();
        try {
            if (!_lazy) {
                return;
            }
            
            _db.closeSession();
            WGDatabaseCore oldCore = _core;
            _core = new WGDatabaseImpl();
            WGUserAccess userAccess = _core.open(_db, _path, _user, _pwd, _prepareOnly);
            _lazy = false;
            _reconfigurationAccessor.reconfigureCore(false, userAccess);
            
            // Migrate the ACL created in fake mode
            WGACL acl = _db.getACL();
            for (WGACLEntry entry : oldCore.getACL().getAllEntries()) {
                WGACLEntry newEntry = acl.createEntry(entry.getName(), entry.getType(), entry.getLevel());
                newEntry.setFlags(entry.getFlags());
                acl.save(newEntry);
            }
            
            _db.refresh();
            _core.closeSession();
            
            _core.openSession(_db.getSessionContext().getAuthenticationSession(), _db.getSessionContext().getCredentials(), _db.getSessionContext().isMasterSession());
            
            
            
        }
        finally {
            _theLock.writeLock().unlock();
        }
        
        
    }

    public WGDocumentCore createStructEntry(Object key, WGDocument reference, WGContentType contentType) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.createStructEntry(key, reference, contentType);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore createContent(WGStructEntry structEntry, WGLanguage language, String title, int version) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.createContent(structEntry, language, title, version);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Class getDedicatedWorkflowEngine() {
        _theLock.readLock().lock();
        try {
            return _core.getDedicatedWorkflowEngine();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean isMemberOfUserList(List userList) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.isMemberOfUserList(userList);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore createCopy(WGDocumentCore original) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.createCopy(original);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean beginTransaction() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.beginTransaction();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean rollbackTransaction() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.rollbackTransaction();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean commitTransaction() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.commitTransaction();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public String getServerName() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getServerName();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public double getContentStoreVersion() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getContentStoreVersion();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public int getContentStorePatchLevel() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getContentStorePatchLevel();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGACLCore getACL() {
        _theLock.readLock().lock();
        try {
            return _core.getACL();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public Object getNativeObject() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getNativeObject();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean resultIsTrue(Object result, WGDocument doc) {
        _theLock.readLock().lock();
        try {
            return _core.resultIsTrue(result, doc);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean resultIsFalse(Object result, WGDocument doc) {
        _theLock.readLock().lock();
        try {
            return _core.resultIsFalse(result, doc);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void refresh() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            _core.refresh();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void clearSessionCache() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            _core.clearSessionCache();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.moveStructEntry(entry, newParent);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public int getContentCount(WGStructEntry entry) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getContentCount(entry);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void setCurrentSession(WGSessionContext context) {
        _theLock.readLock().lock();
        try {
            _core.setCurrentSession(context);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List getDesignObjects(int type) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getDesignObjects(type);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getDesignObject(type, name, strMediaKey);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public String convertFileNameForAttaching(String name) {
        _theLock.readLock().lock();
        try {
            return _core.convertFileNameForAttaching(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List getAllContentKeys(boolean includeArchived) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getAllContentKeys(includeArchived);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public void beginUpdate() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            _core.beginUpdate();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List<WGUpdateLog> getUpdateLogs(Comparable cutoff) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getUpdateLogs(cutoff);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public List<WGRelationData> getIncomingRelations(Object structKey, String language, String sourceContentClass, String relName, String relGroupName, Boolean includeUnreleased, WGColumnSet order)
            throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.getIncomingRelations(structKey, language, sourceContentClass, relName, relGroupName, includeUnreleased, order);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean isContentTypeUsed(WGContentType ct) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.isContentTypeUsed(ct);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean isLanguageUsed(WGLanguage lang) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.isLanguageUsed(lang);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    public boolean isBackendServiceSupported(String serviceName) {
        _theLock.readLock().lock();
        try {
            return _core.isBackendServiceSupported(serviceName);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    @Override
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return _core.callBackendService(serviceName, params);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public void authenticationDataChanged() {
        _theLock.readLock().lock();
        try {
            ((AuthenticationSourceListener) _core).authenticationDataChanged();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public boolean initSequence(String name, long startValue, boolean forceInit) throws WGAPIException {
        switchToDatabaseCore();
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureSequenceProvider) _core).initSequence(name, startValue, forceInit);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public long incrementSequence(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureSequenceProvider) _core).incrementSequence(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public boolean isSequenceInitialized(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureSequenceProvider) _core).isSequenceInitialized(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public long getLastSequenceIncrementValue(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureSequenceProvider) _core).getLastSequenceIncrementValue(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public void deleteSequence(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            ((WGDatabaseCoreFeatureSequenceProvider) _core).deleteSequence(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public List<String> getUsedSequenceNames() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureSequenceProvider) _core).getUsedSequenceNames();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public int getChildEntryCount(WGStructEntry structEntry) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureReturnHierarchyCount) _core).getChildEntryCount(structEntry);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public int getRootEntryCount(WGArea area) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return ((WGDatabaseCoreFeatureReturnHierarchyCount) _core).getRootEntryCount(area);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public WGDocumentCore getUserProfile(String name) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return getPersoCore().getUserProfile(name);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public WGDocumentCore createUserProfile(String name, int type) throws WGAPIException {
        switchToDatabaseCore();
        _theLock.readLock().lock();
        try {
            return getPersoCore().createUserProfile(name, type);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public List<String> queryUserProfileNames(String type, String query, Map params) throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return getPersoCore().queryUserProfileNames(type, query, params);
        }
        finally {
            _theLock.readLock().unlock();
        }
    }

    @Override
    public Iterator<String> getAllUserProfileNames() throws WGAPIException {
        _theLock.readLock().lock();
        try {
            return getPersoCore().getAllUserProfileNames();
        }
        finally {
            _theLock.readLock().unlock();
        }
    }
    
    private WGPersonalisationDatabaseCore getPersoCore() {
        return (WGPersonalisationDatabaseCore) _core;
    }

    @Override
    public void injectReconfigurationAccessor(ReconfigurationAccessor accessor) {
        _reconfigurationAccessor = accessor;
    }

}
