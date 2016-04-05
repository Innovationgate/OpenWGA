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

package de.innovationgate.wgpublisher.design.db;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGConfigurableTypeDesignProvider;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDesignProvider;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.utils.MasterSessionTask;
import de.innovationgate.wga.common.WGAXML;
import de.innovationgate.wga.common.beans.DesignConfiguration;
import de.innovationgate.wga.config.DesignReference;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;
import de.innovationgate.wgpublisher.design.DesignProviderCoreWrapper;
import de.innovationgate.wgpublisher.design.WGADesignProvider;

public class DBDesignProvider extends WGConfigurableTypeDesignProvider implements WGADesignProvider, WGDatabaseEventListener, WGDesignChangeListener, WGACoreEventListener {

    public static final String OPTION_CROSSLOGINMODE = "crossloginmode";
    protected WGDatabase _designDB = null;
    private String _designDBKey;
    private WGDatabase _slaveDB;
    private WGACore _core;
    private boolean _cacheFailedLookups = true;
    private Set _failedLookups = new HashSet();
    private Set _designChangeListeners = new HashSet();
    private Comparable _designDBLastChanged;
    private String _crossLoginMode;
    private boolean _notifying = false;
    

    private boolean _lookupVariants;
    private DesignReference _designReference;
    private String _variantSuffix;
    private boolean _syncAccess = true;


    public DBDesignProvider(DesignReference ref, WGACore core, WGDatabase slaveDB, String dbkey, Map<String,String> options) {
        
        _designReference = ref;
        
        List<Integer> providerTypesList = null;
        String optionProviderTypes = (String) slaveDB.getCreationOptions().get(WGDatabase.COPTION_DESIGNPROVIDERTYPES);
        if (optionProviderTypes != null) {
            providerTypesList = new ArrayList<Integer>();
            Iterator<String> providerTypes = WGUtils.deserializeCollection(optionProviderTypes, ",", true).iterator();
            while (providerTypes.hasNext()) {
                String providerTypeName = (String) providerTypes.next();
                int providerType = WGDocument.doctypeNameToNumber(providerTypeName);
                if (providerType != 0) {
                    providerTypesList.add(new Integer(providerType));
                }
            }
        }
        initProviderTypes(providerTypesList);
        
        _core = core;
        _slaveDB = slaveDB;
        _designDBKey = dbkey;
        _crossLoginMode = WGUtils.getValueOrDefault(options.get(OPTION_CROSSLOGINMODE), WGAXML.DESIGNSHARING_MODE_ANONYMOUS);
        _lookupVariants = WGUtils.getBooleanMapValue(options, OPTION_DESIGNVARIANTS, false);
        _variantSuffix = "." + slaveDB.getDbReference();
        _core.addEventListener(this);
        
        // Try to register with design db immediately
        try {
            if (!registerDesignDB()) {
                core.getLog().warn("Unable to register design consumer '" + slaveDB.getDbReference() + "' with design provider '" + _designDBKey + "' immediately. Disabling design caching because of unknown design provider type.");
            }
        }
        catch (WGException e) {
            core.getLog().error("Exception registering design consumer '" + slaveDB.getDbReference() + "' with design provider '" + _designDBKey + "'", e);
        }
    }
    
    public List<WGDocumentCore> getDesignObjects(int type) {
        try {
            WGDatabase db = getDesignDB();
            if (db.getDesignProvider() != null) {
                return wrapCores(db.getDesignProvider().getDesignObjects(type));
            }
            else {
                return wrapCores(db.getCore().getDesignObjects(type));
            }
        }
        catch (WGException e) {
            WGFactory.getLogger().error("Error retrieving design from design database '" + _designDBKey + "'", e);
            return null;
        }
    }

    private List<WGDocumentCore> wrapCores(List designObjects) throws WGAPIException {
        
        if (designObjects == null) {
            return null;
        }
        
        Iterator designsIt = designObjects.iterator();
        Set<String> addedNames = new HashSet<String>();
        WGDocumentCore core;
        List<WGDocumentCore> newList = new ArrayList<WGDocumentCore>();
        while (designsIt.hasNext()) {
            core = wrapVariantCore((WGDocumentCore) designsIt.next());
            
            // Filter out base versions of variants
            if (_lookupVariants) {
                String coreName = (String) core.getMetaData(WGDesignDocument.META_NAME);
                
                // If we try to add a doc that already has been added and is NO variant, we know that this is the base version which was
                // overwritten by a variant. So we skip it.
                if (addedNames.contains(coreName) && !Boolean.TRUE.equals(core.getMetaData(WGDesignDocument.META_VARIANT))) {
                    continue;
                }
                else {
                    addedNames.add(coreName);
                }
            }
            
            
            newList.add(core);
        }
        
        return newList;
    }

    public WGDocumentCore getDesignObject(int type, String name, String mediaKey) {
        try {
            WGDatabase db = getDesignDB();
            
            if (_lookupVariants) {
                WGDocumentCore core = fetchDesignObject(db, type, name + "." + _slaveDB.getDbReference(), mediaKey);
                if (core != null) {
                    return wrapVariantCore(core);
                }
            }
            
            return fetchDesignObject(db, type, name, mediaKey);
        }
        catch (WGUnavailableException e) {
            WGFactory.getLogger().error("Design database is currently unavailable", e);
            return null;
        }
        catch (WGException e) {
            WGFactory.getLogger().error("Error retrieving design from design database '" + _designDBKey + "'", e);
            return null;
        }
    }

    private WGDocumentCore fetchDesignObject(WGDatabase db, int type, String name, String mediaKey) throws WGAPIException {
        
        String failedKey = new WGDocumentKey(type, name, mediaKey).toString();
        if (_cacheFailedLookups && _failedLookups.contains(failedKey)) {
            return null;
        }
        
        WGDocumentCore core = null;
        if (db.getDesignProvider() != null && db.getDesignProvider().providesType(type)) {
            core = db.getDesignProvider().getDesignObject(type, name, mediaKey);
        }
        else {
            core = db.getCore().getDesignObject(type, name, mediaKey);
        }
        
        if (core == null) {
            if (_cacheFailedLookups) {
                _failedLookups.add(failedKey);
            }
        }
        else {
            core = new DesignProviderCoreWrapper(core, this, false, false);
        }
        
        return core;
    }



    public String getName() {
        return "DB design '" + _designDBKey + "'";
    }

    public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGCreationException {
        throw new WGCreationException("This database uses the design of database '" + _designDBKey + "'");
    }
    
    private boolean registerDesignDB() throws WGUnavailableException, WGAuthorisationException, WGInvalidDatabaseException {
        
           WGDatabase currentDesignDB = (WGDatabase) _core.getContentdbs().get(_designDBKey);
           if (currentDesignDB != null && currentDesignDB != _designDB) {
               _designDB = currentDesignDB;
               
               // Since lazy connections on a design provider DB won't work well, we issue a warning here
               // We also open a session to connect the database and init its potential design provider
               if (!_designDB.isConnected() && _designDB.getDesignProvider() != null) {
                   _core.getLog().warn("You configured design provider db '" + _designDB.getDbReference() + "', which itself has a design provider, as lazily connected.");
                   _core.getLog().warn("This is not supported since this may lead to delays in design serving, resulting in design documents not being immediately ready for the design consumer db.");
               }
                
               // Register as listener for the database itself or its own design provider
               if (_designDB.getDesignProvider() != null) {
                   _notifying = _designDB.getDesignProvider().isNotifying();
                   if (_designDB.getDesignProvider() instanceof WGADesignProvider) {
                       _syncAccess  = ((WGADesignProvider) _designDB.getDesignProvider()).isSynchronizeAccess();
                   }
                   if (_designDB.getDesignProvider().isNotifying()) {
                       _designDB.getDesignProvider().addDesignChangeListener(this);
                   }
                   else {
                       _cacheFailedLookups = false;
                   }
               }
               else {
                   _designDBLastChanged = _designDB.getRevision();
                   _designDB.addDatabaseEventListener(this);
                   _notifying = true;
               }
               
               // Clear the cache of the slave db
               if (_slaveDB != null && _slaveDB.isConnected()) {
                   MasterSessionTask task = new MasterSessionTask(_slaveDB) {
                       protected void exec(WGDatabase db) throws Throwable {
                           fireUpdateChangeEvent(new WGDesignChangeEvent(DBDesignProvider.this, db, new ArrayList()));
                           db.refresh();
                       }
                   };
                   task.run();
               }
               return true;
     
           }
           else {
               return false;
           }
        
    }

    /**
     * @return Returns the designDBKey.
     */
    public String getDesignDBKey() {
        return _designDBKey;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseEventListener#databaseUpdate(de.innovationgate.webgate.api.WGDatabaseEvent)
     */
    public synchronized void databaseUpdate(WGDatabaseEvent event) {
        
        try {
			_failedLookups.clear();
    	    WGDatabase db = event.getDatabase();
        	Comparable currentLastChanged = db.getRevision();
	        if (currentLastChanged.compareTo(_designDBLastChanged) > 0) {
    	        List updateLogs = db.getUpdatedDocumentsSince(_designDBLastChanged);
        	    updateLogs = filterLogs(updateLogs);
            	if (updateLogs.size() == 0) {
	                return;
    	        }
        	    WGDesignChangeEvent designEvent = new WGDesignChangeEvent(this, _slaveDB, updateLogs);
            	fireUpdateChangeEvent(designEvent);
	            _designDBLastChanged = currentLastChanged;
    	    }
        } 
        catch (WGAPIException e) {
            _core.getLog().error("Unable to process databaseUpdate event.", e);
        }
        
    }

    private List filterLogs(List updateLogs) {
       
        List newLogs = new ArrayList();
        Iterator logs = updateLogs.iterator();
        while (logs.hasNext()) {
            WGUpdateLog log = (WGUpdateLog) logs.next();
            if (log.getDocumentKey().startsWith("$")) {
                continue;
            }
            
            WGDocumentKey docKey = new WGDocumentKey(log.getDocumentKey());
            if (docKey.getTypename().equals(WGDocument.TYPENAME_TML) ||
                docKey.getTypename().equals(WGDocument.TYPENAME_CSSJS) ||
                docKey.getTypename().equals(WGDocument.TYPENAME_FILECONTAINER)) {
                
                // If lookup variants have changed (and feature is active) we need to change the log to use the base name
                // So the correct caches are cleared
                if (_lookupVariants == true && _slaveDB != null) { 
                    String suffix = "." + _slaveDB.getDbReference();
                    if (docKey.getName().endsWith(suffix)) {
                        String newName = DesignProviderCoreWrapper.cutoffVariantSuffix(docKey.getName(), suffix);
                        WGDocumentKey newKey = new WGDocumentKey(docKey.getTypename(), newName, docKey.getMediakey());
                        log = new WGUpdateLog(log.getType(), log.getDate(), log.getUser(), newKey.toString(), null, null);
                    }
                }
                
                newLogs.add(log);
            }
        }
        return newLogs;
        
    }

    private synchronized void fireUpdateChangeEvent(WGDesignChangeEvent designEvent) {
        Iterator listeners = _designChangeListeners.iterator();
        while (listeners.hasNext()) {
            WGDesignChangeListener listener = (WGDesignChangeListener) listeners.next();
            listener.designChanged(designEvent);
        }
    }

    private void crossLogin(WGDatabase sourceDB, WGDatabase targetDB) throws WGAPIException {
        
        if (targetDB.isSessionOpen()) {
            return;
        }
        
        String user = WGDatabase.ANONYMOUS_USER;
        String pwd = null;
        if (_crossLoginMode.equals(WGAXML.DESIGNSHARING_MODE_PARALLEL)) {
            if (!sourceDB.isSessionOpen()) {
                throw new WGAuthorisationException("Unable to do cross login in parallel mode while both databases have no session open");
            }
            if (sourceDB.getSessionContext().isMasterSession()) {
                user = null;
                pwd = null;
            }
            else {
                user = sourceDB.getSessionContext().getUser();
                pwd = sourceDB.getSessionContext().getPassword();
            }
        }
               
        int accLevel = targetDB.openSession(user, pwd);
         
        if (accLevel <= WGDatabase.ACCESSLEVEL_NOACCESS) {
            if (_crossLoginMode.equals(WGAXML.DESIGNSHARING_MODE_PARALLEL)) {
                throw new WGAuthorisationException("Design sharing databases do not support neccessary logins. Database '" + targetDB.getDbReference() + "' denies access for username '" + user + "' which is accepted by database '" + sourceDB.getDbReference() + "' (or has a different password).");
            }
            else {
                throw new WGAuthorisationException("Design sharing databases do not support neccessary logins. Database '" + targetDB.getDbReference() + "' does not accept anonymous access.");
            }
        }
        
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseEventListener#isTemporary()
     */
    public boolean isTemporary() {
        return false;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignProvider#addDesignChangeListener(de.innovationgate.webgate.api.WGDesignChangeListener)
     */
    public void addDesignChangeListener(WGDesignChangeListener changeListener) {
        _designChangeListeners.add(changeListener);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignProvider#removeDesignChangeListener(de.innovationgate.webgate.api.WGDesignChangeListener)
     */
    public void removeDesignChangeListener(WGDesignChangeListener changeListener) {
         _designChangeListeners.remove(changeListener);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignChangeListener#designChanged(de.innovationgate.webgate.api.WGDesignChangeEvent)
     */
    public void designChanged(WGDesignChangeEvent event) {

        _failedLookups.clear();
        WGDesignChangeEvent designEvent = new WGDesignChangeEvent(this, _slaveDB, filterLogs(event.getUpdateLogs()));
        fireUpdateChangeEvent(designEvent);
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDesignProvider#dispose()
     */
    public void dispose() {
        _designChangeListeners.clear();
        _core.removeEventListener(this);
        _core = null;
        _slaveDB = null;        
    }

    public void contentStoreConnected(WGACoreEvent event) {
        try {
            registerDesignDB();
        }
        catch (WGException e) {
        }
    }

    public void contentStoreDisconnected(WGACoreEvent event) {
    }

    public WGDatabase getDesignDB() throws WGAPIException {
        
            if (_designDB == null) {
                throw new WGInvalidDatabaseException("The configured design database '" + _designDBKey + "' is not connected");
            }
        
            if (!_designDB.isSessionOpen()) {
                crossLogin(_slaveDB, _designDB);
            }
                       
            return _designDB;
        
    }

    public boolean isLookupVariants() {
        return _lookupVariants;
    }

    public WGDatabase getSlaveDB() {
        return _slaveDB;
    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {
    }

    public void startupPreConnect(WGACoreEvent event) {
    }
    
    public boolean isProviderCore(WGDocumentCore core) {
        return (core instanceof DesignProviderCoreWrapper);
    }

    public boolean isNotifying() {
        return _notifying;
    }

    public WGDatabase getConsumerDatabase() {
        return getSlaveDB();
    }

    public void closeSession() {
    }

    public void openSession(WGSessionContext context) {
    }

    public int designHashCode() {
        return _designDB.hashCode();
    }

    public WGDocumentCore wrapVariantCore(WGDocumentCore core) throws WGAPIException {
        
        if (_lookupVariants) {
            String name = (String) core.getMetaData(WGDesignDocument.META_NAME);
            if (name.endsWith(_variantSuffix)) {
                return new DesignProviderCoreWrapper(core, this, true, false);
            }
            else {
                return new DesignProviderCoreWrapper(core, this, false, false);
            }
        }
        else {
            return new DesignProviderCoreWrapper(core, this, false, false);
        }
        
    }
    
    public boolean isReady() {

        try {
            WGDatabase designDB = getDesignDB();
            return (designDB != null && designDB.isReady());
        }
        catch (WGAPIException e) {
            return false;
        }
        
    }
    
    public boolean isSynchronizeAccess() {
        return _syncAccess;
    }

    public DesignReference getDesignReference() {
        return _designReference;
    };
    
    @Override
    public void clearCache() throws WGException {
        WGDesignProvider designProvider = _designDB.getDesignProvider();
        if (designProvider != null) {
            if (designProvider instanceof WGADesignProvider) {
                ((WGADesignProvider) designProvider).clearCache();
            }
        }
        else {
            _designDB.refresh();
        }
    }
    
    @Override
    public String getFileEncoding() {
        return null;
    }
}


