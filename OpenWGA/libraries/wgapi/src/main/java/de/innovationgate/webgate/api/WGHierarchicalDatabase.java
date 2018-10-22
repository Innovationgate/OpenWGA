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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.transaction.Transaction;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.webgate.api.utils.MasterSessionTask;

/**
 * <pre>
 * wraps a wga content store to use it as hierarchical database
 * simplifies creating content documents for data storage use
 * creation of ContentTypes, Languages, Areas is encapsulated
 *
 * WGHierarchicalDatabase automatical create the following areas:
 *  - HDB-Content (used for content)
 *  - HDB-System (used for meta information about the db)
 *
 * WGHierarchicalDatabase automatical create the following content types:
 *  - HDB-ContentType with outer-layout HDB-Layout
 * 
 * The hdb structure contains WGContents. Each WGContent is of type HDB-ContentType 
 * and created in the default language of the database. All neccessary WGStructEntries are handled and created 
 * by the hdb automatically. 
 * 
 * A WGContent in the hdb can either be used as a storage or as a content. A storage has a special struct-entry 
 * which is by default write/edit-protected with the role ($HDBAdmin). Therefore normal users cannot delete a 
 * whole hdb structure. A storage contains contents and might contains substorages. When a storage is created the user
 * should give an id for it. This id is used to create a unique id for the storage, based on the hierarchical storage
 * structure. The unique id of a storage contains all parent storage ids concated by "."
 * for e.g.: storageA
 *                  |- storageB
 *                            |- storageC   
 * 
 * unique id of A is: "storageA"
 * unique id of B is: "storageA.storageB"
 * unique id of C is: "storageA.storageB.storageC"
 * 
 * The unique id of a storage is set as unique name for the WGContent object, so the unique storage id can be used
 * directly for e.g. in context expressions.
 * 
 * A startup script can be defined with the publisher option "de.innovationgate.wgpublisher.WGACore.DBATTRIB_HDBInit" 
 * This can be either a class name of a class which implements 
 * {@link de.innovationgate.webgate.api.WGHierarchicalDatabaseStartup} or a name of a tmlsscript module within this db. 
 * The startup script is executed when an hdb instance gets connected. 
 * It can be used to create storages and define the initial db-structure. 
 * </pre>
 */
public class WGHierarchicalDatabase {
    
    /**
     * Name under which the HDB event parameter is stored as attribute of the {@link WGSessionContext}
     */
    public static final String SESSIONPARAM_HDBPARAMETER = "hdbparameter";

    private static Map<String, WGHierarchicalDatabase> _instances = new HashMap<String, WGHierarchicalDatabase>();
    
    private static Set<WGHierarchicalDatabaseCoreListener> _coreListeners = new HashSet<WGHierarchicalDatabaseCoreListener>();

	private static WGHierarchicalDatabaseListenerFactory _defaultListenerFactory;
	private static WGHierarchicalDatabaseStartup _defaultStartupImpl;
    
    /**
     * Name of the area in which HDB content is to be stored
     */
    public static final String AREA_CONTENT = "HDB-Content";
    private static final String AREA_SYSTEM = "HDB-System";
    /**
     * Name of the content type which HDB content pages belong to
     */
    public static final String CONTENT_TYPE = "hdb-contenttype";
    private static final String LAYOUT = "HDB-Layout";
    
    /**
     * ACL Role name which is granted administrative HDB rights, like editing rights to HDB storage documents. 
     */
    public static final String STORAGE_ADMIN_ROLE = "$HDBAdmin";
    
    /**
     * Attribute to control if HDB should use versioning when updating contents. Defaults to false. Set to true to enable this.
     */
    public static final String DBATTRIB_HDB_USE_VERSIONING = "HDBUseVersioning";
    
    
    /**
     * Item name in which the name of a  HDB listener class or module for a content is stored
     */
    public static final String ITEMNAME_LISTENER_CLASS_OR_MODULE = "$HDBListener";
    
    /**
     * Character by which the name parts of HDB storage ids are divided
     */
    public static final String STORAGE_DELIMITER = ".";
            
    private WGDatabase _database;
    private WGHierarchicalDatabaseStartup _startup;
    private WGHierarchicalDatabaseListenerFactory _listenerFactory;
    
    private boolean _useVersioning = false;
    private boolean _hdbModelMode = false;
    
    /**
     * Behaviour regarding the post update event
     */
    public enum PostUpdateEventBehaviour {
        /**
         * Run the post update event before saving the content
         */
        RUN_BEFORE_SAVE,
        /**
         * Run the post update event before after the content
         */
        RUN_AFTER_SAVE
    }
    
    private PostUpdateEventBehaviour _postUpdateEventBehaviour = PostUpdateEventBehaviour.RUN_BEFORE_SAVE;
    
    
    /**
     * creates a new hdb instance for the given WGDatabase
     * @param database WGDatabase to use as backendstorage
     * @param startupImpl StartupScript to execute, can be 'null'
     * @throws WGAPIException
     */
    public static void createInstance(WGDatabase database, WGHierarchicalDatabaseStartup startupImpl) throws WGAPIException {
    	WGHierarchicalDatabase hdb = null;
    	synchronized (_instances) {
    		hdb = new WGHierarchicalDatabase(database, startupImpl);
            _instances.put(database.getDbReference().toLowerCase(), hdb);                        
            hdb.prepare();	                        	
		}        
    	for (WGHierarchicalDatabaseCoreListener listener : _coreListeners)  {
    		try {
    			listener.databaseCreated(hdb);
    		} catch (Throwable e) {    			
    		}
    	}
    } 
    
    /**
     * creates a new hdb instance for the given WGDatabase
     * @param database WGDatabase to use as backendstorage
     * @param startupImpl StartupScript to execute, can be 'null'
     * @param listenerFactory WGHierarchicalDatabaseListenerFactory implementation to use for listener creation
     * @throws WGAPIException
     */
    public static void createInstance(WGDatabase database, WGHierarchicalDatabaseStartup startupImpl, WGHierarchicalDatabaseListenerFactory listenerFactory) throws WGAPIException {
    	WGHierarchicalDatabase hdb = null;
    	synchronized (_instances) {
    		hdb = new WGHierarchicalDatabase(database, startupImpl, listenerFactory);        
            _instances.put(database.getDbReference().toLowerCase(), hdb);           
            hdb.prepare();
		}
    	for (WGHierarchicalDatabaseCoreListener listener : _coreListeners)  {
    		try {
    			listener.databaseCreated(hdb);
    		} catch (Throwable e) {    			
    		}
    	}
    }
        
    /**
     * removes an created instance
     * @param dbKey
     */
    public static void removeInstance(String dbKey) {
        WGHierarchicalDatabase hdb = _instances.remove(dbKey.toLowerCase());
        for (WGHierarchicalDatabaseCoreListener listener : _coreListeners)  {
    		try {
    			listener.databaseRemoved(hdb);
    		} catch (Throwable e) {    			
    		}
    	}        
    }
    
    /**
     * returns the requested hdb for the given dbKey, instance have to be create prior by using createInstance(...)
     * @param dbKey
     * @return WGHierarchicalDatabase
     * @throws WGAPIException
     */
    public static WGHierarchicalDatabase getInstance(String dbKey) throws WGAPIException {
        return (WGHierarchicalDatabase) _instances.get(dbKey.toLowerCase());
    }
    
    
    /**
     * returns the requested hdb for the given db, instance is created if not existing using the default startup and listenerFactory implementations
     * @param db WGDatabase to use as backendstorage
     * @throws WGAPIException
     */
    public static WGHierarchicalDatabase getOrCreateInstance(WGDatabase db) throws WGAPIException {
        if (!_instances.containsKey(db.getDbReference().toLowerCase())) {
        	if (_defaultListenerFactory != null) {
        		createInstance(db, _defaultStartupImpl , _defaultListenerFactory);
        	} else {
        		createInstance(db, _defaultStartupImpl);
        	}
        }
        WGHierarchicalDatabase instance = (WGHierarchicalDatabase) _instances.get(db.getDbReference().toLowerCase());
        return instance;
    }
    
    private class PrepareTask extends MasterSessionTask {
        private WGHierarchicalDatabase _hdb;
        
        public PrepareTask(WGDatabase db, WGHierarchicalDatabase hdb) {
            super(db, true);
            _hdb = hdb;
        }

        protected void exec(WGDatabase db) throws WGAPIException {
            // create content area
            WGArea contentArea = db.getArea(AREA_CONTENT);
            if (contentArea == null) {
                contentArea = db.createArea(AREA_CONTENT);
                contentArea.save();
            }
            
            // create system area
            WGArea systemArea = db.getArea(AREA_SYSTEM);
            if (systemArea == null) {
                systemArea = db.createArea(AREA_SYSTEM);
                if (db.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
                    systemArea.setSystemArea(true);
                }
                systemArea.save();
            }
            else if (db.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && !systemArea.isSystemArea()) {
                systemArea.setSystemArea(true);
                systemArea.save();
            }
            
            // create content type
            WGContentType contentType = db.getContentType(CONTENT_TYPE);
            if (contentType == null) {
                contentType = db.createContentType(CONTENT_TYPE);
                contentType.setOuterLayoutName(LAYOUT);
                contentType.setAuthoring(false);
                contentType.save();
            } 
            
            // create default language if not exists
            WGLanguage language = db.getLanguage(_database.getDefaultLanguage());
            if (language.isDummy()) {
                language = db.createLanguage(_database.getDefaultLanguage(), _database.getDefaultLanguage());
                language.save();
            }            
            
            if (_startup != null) {
                try {
                    _startup.execute(_hdb);
                }
                catch (Exception e) {
                    throw new WGAPIException("Unable to execute startup script.", e);
                }
            }
            
            db.refresh();            
        }
    }
    
    
    private class ProtectTask extends MasterSessionTask {
		private WGStructEntry _structentry;
        
        public ProtectTask(WGDatabase db, WGStructEntry structentry) {
            super(db);
            _structentry = structentry;
        }

        protected void exec(WGDatabase db) throws WGAPIException {
        	_structentry.setPageEditors(Collections.singletonList(STORAGE_ADMIN_ROLE));
            _structentry.save();
            db.refresh();            
        }
    }
    
    private void prepare() throws WGAPIException {
    	String useVersioning = (String) _database.getAttribute(DBATTRIB_HDB_USE_VERSIONING);
    	if (useVersioning != null) {
    		_useVersioning = Boolean.valueOf(useVersioning).booleanValue();
    	}
    	
    	PrepareTask prepareTask = new PrepareTask(_database, this);
        try {
            prepareTask.runWithExceptions();
        } catch (WGAPIException e) {
            throw e;
        } catch (Throwable e) {
            throw new WGAPIException(e.getMessage(), e);
        }

        
        // Reopen the session on this database so changes done in prepare task are reflected 
        _database.reopenSession();
    }
    
    
    /**
     * protects the given struct entry
     * if dbsession is no master session protection is done in a seperate master session task
     * @param entry
     * @throws WGAPIException
     */
    private void protect(WGStructEntry entry) throws WGAPIException {
    	if (_database.getSessionContext().isMasterSession()) {
    		entry.setPageEditors(Collections.singletonList(STORAGE_ADMIN_ROLE));
        	entry.save();        	
    	} else {	    	
	        ProtectTask protectTask = new ProtectTask(_database, entry);
	        try {
	            protectTask.runWithExceptions();
	        } catch (WGAPIException e) {
	            throw e;
	        } catch (Throwable e) {
	            throw new WGAPIException(e.getMessage(), e);
	        }
    	}
    }

    private WGHierarchicalDatabase(WGDatabase database, WGHierarchicalDatabaseStartup startup) {
        _database = database;
        _startup = startup;
        _listenerFactory = new WGHierarchicalDatabaseJavaListenerFactory();
    }
    
    private WGHierarchicalDatabase(WGDatabase database, WGHierarchicalDatabaseStartup startup, WGHierarchicalDatabaseListenerFactory factory) {
        _database = database;
        _startup = startup;
        _listenerFactory = factory;
    }
    
    /**
     * Determines if the given content document is a HDB storage
     * @param content The content document
     * @throws WGAPIException
     */
    public boolean isStorage(WGContent content) throws WGAPIException {
        if (content.hasItem("isStorageEntry")) {
            return ((Boolean)content.getItemValue("isStorageEntry")).booleanValue();
        } else {
            return false;
        }
    }

    /**
     * Retrieves the WGAPI Content Type that this HDB uses to construct contents
     * @throws WGAPIException
     */
    public WGContentType getContentType() throws WGAPIException {
        return _database.getContentType(CONTENT_TYPE);
    }
    
    /**
     * Returns the language that the HDB uses to create content
     * @throws WGAPIException
     */
    public WGLanguage getLanguage() throws WGAPIException {
        return _database.getLanguage(_database.getDefaultLanguage());
    }
    
    /**
     * Returns a dummy content object for the current database in the language that the HDB uses
     * @throws WGAPIException
     */
    public WGContent getDummyContent() throws WGAPIException {
        return _database.getDummyContent(_database.getDefaultLanguage());
    }
    
    /**
     * Returns the {@link WGDatabase} object wrapped by this hdb
     */
    public WGDatabase getWrappedDB() {
        return _database;
    }    
    
    /**
     * creates a child content of the given parent with the given title,
     * parameter is delivered within the contentCreatedEvent
     * @param parent either a storage or a content
     * @param title if 'null' a title is generated
     * @param parameter can be retrieved in the event listener by event.getParameter()
     * @return WGContent - in versioning mode this content is not published yet
     * @throws WGAPIException
     * @throws WGHierarchicalDatabaseEventException - if a listener cancel the process
     */
    public WGContent createContent(WGContent parent, String title, Object parameter) throws WGAPIException, WGHierarchicalDatabaseEventException {
        
        if (parent == null) {
            throw new WGIllegalArgumentException("Parent content may not be null");
        }
        
        WGHierarchicalDatabaseEvent event = 
            new WGHierarchicalDatabaseEvent(WGHierarchicalDatabaseEvent.TYPE_PRE_CREATE_CONTENT, this);        
        event.setParentContent(parent);
        event.setParameter(parameter);
        WGTransaction trans = _database.startTransaction();
        
        try {
            fireEvent(parent, event);
            WGStructEntry entry = createStructEntry(parent.getStructEntry(), title);
            WGContent content = innerCreateContent(entry, title, parameter);
    
            event.setType(WGHierarchicalDatabaseEvent.TYPE_POST_CREATE_CONTENT);
            event.setContent(content);
            
            fireEvent(parent, event);
            trans.commit();
            return content;
        }
        finally {
            if (trans.isOpen()) {
                trans.rollback();
            }
        }
        
    }
    
    /**
     * creates a child content of the given parent with the given title,
     * @param parent either a storage or a content
     * @param title if 'null' a title is generated
     * @return WGContent
     * @throws WGAPIException
     * @throws WGHierarchicalDatabaseEventException - if a listener cancel the process
     */
    public WGContent createContent(WGContent parent, String title) throws WGAPIException, WGHierarchicalDatabaseEventException {
        return createContent(parent, title, null);
    }
    
    /**
     * creates a child content of the given parent with a generated title
     * @param parent either a storage or a content
     * @return WGContent
     * @throws WGAPIException
     * @throws WGHierarchicalDatabaseEventException - if a listener cancel the process
     */
    public WGContent createContent(WGContent parent) throws WGAPIException, WGHierarchicalDatabaseEventException {
        return createContent(parent, null, null);
    }
        
    /**
     * deletes the given content
     * @param content WGContent
     * @param parameter can be retrieved in the event listener by event.getParameter()
     * @throws WGAPIException
     * @throws WGHierarchicalDatabaseEventException - if a listener cancel the process
     */
    public void deleteContent(WGContent content, Object parameter) throws WGAPIException, WGHierarchicalDatabaseEventException {
        WGHierarchicalDatabaseEvent event = new WGHierarchicalDatabaseEvent(WGHierarchicalDatabaseEvent.TYPE_PRE_DELETE_CONTENT, this);
        event.setContent(content);
        WGContent parent = getParentContent(content);
        event.setParentContent(parent);
        event.setParameter(parameter);
        
        WGTransaction trans = _database.startTransaction();
        try {
            if (parent != null) {
                fireEvent(parent, event);
            }
            
            content.getStructEntry().remove();
            
            event.setType(WGHierarchicalDatabaseEvent.TYPE_POST_DELETE_CONTENT);
            event.setContent(null);
            
            if (parent != null) {
                fireEvent(parent, event);
            }
            trans.commit();
        }
        finally {
            if (trans.isOpen()) {
                trans.rollback();
            }
        }
    }
    
    /**
     * deletes the given content
     * @param content WGContent
     * @throws WGAPIException
     * @throws WGHierarchicalDatabaseEventException
     */
    public void deleteContent(WGContent content) throws WGAPIException, WGHierarchicalDatabaseEventException {
        deleteContent(content, null);
    }
    
    /**
     * updates the title on the given content and optional further fields within the listener using parameter
     * if versioning is enabled the post_update_event is executed on a draft copy
     * @param content the content to update
     * @param title the new value for the title, 'null' will leave the title unchanged
     * @param parameter can be retrieved in the event listener by event.getParameter()
     * @return WGContent a saved and published content object, if versioning is enabled the previous content has been archived
     * @throws WGAPIException 
     * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
     */
     public WGContent updateContent(WGContent content, String title, Object parameter) throws WGAPIException, WGHierarchicalDatabaseEventException {
         WGHierarchicalDatabaseEvent event = new WGHierarchicalDatabaseEvent(WGHierarchicalDatabaseEvent.TYPE_PRE_UPDATE_CONTENT, this);
         event.setContent(content);
         WGContent parent = getParentContent(content);
         event.setParentContent(parent);

         event.setParameter(parameter);
         
         WGTransaction trans = _database.startTransaction();
         try {
         
             if (parent != null) {
                 fireEvent(parent, event);
             }
             
             if (_useVersioning && content.getStatus() != null && !(content.getStatus().equals(WGContent.STATUS_DRAFT))) {
            	 content = _database.createDraftCopy(content);
             }
             
             if (title != null && !title.equals(content.getStructEntry().getTitle()) && content.getStructEntry().maySave()) {
                 content.getStructEntry().setTitle(title);
                 content.getStructEntry().save();
                 content.setTitle(title);
             }
             
             event.setType(WGHierarchicalDatabaseEvent.TYPE_POST_UPDATE_CONTENT);
             event.setContent(content);
             
             if (parent != null && _postUpdateEventBehaviour == PostUpdateEventBehaviour.RUN_BEFORE_SAVE) {
                 fireEvent(parent, event);
             }
             
             content.getDatabase().getSessionContext().setAttribute(SESSIONPARAM_HDBPARAMETER, parameter);
        	 try{
                 if (content.getStatus() != null && content.getStatus().equals(WGContent.STATUS_DRAFT)) {
                	 content.publish("");
                 } else {
                	 content.save();
                 }
        	 }
             finally {
                 content.getDatabase().getSessionContext().removeAttribute(SESSIONPARAM_HDBPARAMETER);
             }
             
             if (parent != null && _postUpdateEventBehaviour == PostUpdateEventBehaviour.RUN_AFTER_SAVE) {
                 fireEvent(parent, event);
             }
             
             trans.commit();
             return content;
             
         }
         finally {
             if (trans.isOpen()) {
                 trans.rollback();
             }
         }
     }
         
     
     /**
      * moves the given content under the given parent content
      * pre- and post-move events are first fired on the old hierarchy and then on the new hierarchy
      * if target parent is null the content will be moved to the db root
      * @param content the content to move
      * @param newParent the new parent to move the content to
      * @param parameter can be retrieved in the event listener by event.getParameter()
      * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
      * @throws WGAPIException
      */
     public void moveContent(WGContent content, WGContent newParent, Object parameter) throws WGHierarchicalDatabaseEventException, WGAPIException {
    	 if (isStorage(content)) {
    		 throw new WGIllegalArgumentException("Moving a storage entry is not permitted.");
    	 }
    	 
    	 WGHierarchicalDatabaseEvent event = new WGHierarchicalDatabaseEvent(WGHierarchicalDatabaseEvent.TYPE_PRE_MOVE_CONTENT_FROM, this);
         event.setContent(content);
         WGContent parent = getParentContent(content);
         event.setParentContent(parent);         
         event.setTargetParentContent(newParent);         
         event.setParameter(parameter);
         
         if (parent != null) {
        	 fireEvent(parent, event);
         }
         
         if (newParent != null) {
        	 event.setType(WGHierarchicalDatabaseEvent.TYPE_PRE_MOVE_CONTENT_TO);
        	 fireEvent(newParent, event);
         }
    	 
         if (newParent != null) {
        	 _database.moveStructEntry(content.getStructEntry(), newParent.getStructEntry());
         } else {
        	 _database.moveStructEntry(content.getStructEntry(), _database.getArea(AREA_CONTENT));
         }
    	 
    	 
    	 
    	 if (parent != null) {
    		 event.setType(WGHierarchicalDatabaseEvent.TYPE_POST_MOVE_CONTENT_FROM);
    		 fireEvent(parent, event);
    	 }
    	 if (newParent != null) {
    		 event.setType(WGHierarchicalDatabaseEvent.TYPE_POST_MOVE_CONTENT_TO);
        	 fireEvent(newParent, event);
         }
     }
     
     /**
      * moves the given content under the given parent content
      * pre- and post-move events are first fired on the old hierarchy and then on the new hierarchy
      * if target parent is null the content will be moved to the db root
      * @param content the content to move
      * @param newParent the new parent to move the content to
      * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
      * @throws WGAPIException
      */
     public void moveContent(WGContent content, WGContent newParent) throws WGHierarchicalDatabaseEventException, WGAPIException {
    	 moveContent(content, newParent, null);
     }
     
     /**
      * updates the given content within the listener, parameter will be null, leaves the title unchanged
      * @param content the content to update
      * @throws WGAPIException
      * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
      */
     public void updateContent(WGContent content) throws WGAPIException, WGHierarchicalDatabaseEventException {
         updateContent(content, null, null);
     }
     
     /**
      * updates the given content within the listener using parameter, leaves the title unchanged
      * @param content the content to update
      * @param parameter can be retrieved in the event listener by event.getParameter()
      * @throws WGAPIException
      * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
      */
     public void updateContent(WGContent content, Object parameter) throws WGAPIException, WGHierarchicalDatabaseEventException {
         updateContent(content, null, parameter);
     }
    
    /**
     * deletes the content determined by the given key 
     * @param key determined by getKey(WGContent)
     * @throws WGAPIException
     * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
     */
    public void deleteContent(String key) throws WGAPIException, WGHierarchicalDatabaseEventException {
        deleteContent(getContent(key), null);
    }
    
    /**
     * deletes the content determined by the given key
     * @param key determined by getKey(WGContent)
     * @param parameter can be retrieved in the event listener by event.getParameter() 
     * @throws WGHierarchicalDatabaseEventException if a listener has canceled the process
     * @throws WGAPIException 
     */
    public void deleteContent(String key, Object parameter) throws WGAPIException, WGHierarchicalDatabaseEventException {
        deleteContent(getContent(key), parameter);
    }
    
    /**
     * returns currently published content object for the given key
     * @param key determined by getKey(WGContent)
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent getContent(String key) throws WGAPIException {
        return _database.getStructEntryByKey(key).getReleasedContent(getLanguage().getName());
    }
    
    
    /**
     * returns a db wide unique key for the given content
     * @param content
     * @return key
     * @throws WGAPIException
     */
    public String getKey(WGContent content) throws WGAPIException {
        return content.getStructEntry().getStructKey().toString();
    }
    
    /**
     * sets the given module (tmlscriptmodule) or classname (implementation of WGHierarchicalContentListener) 
     * on the given storage or content
     * @param content
     * @param moduleOrClassname
     * @throws WGAPIException
     */
    public void setListener(WGContent content, String moduleOrClassname) throws WGAPIException {
        // check if listener is up to date to prevent unneccessary updates
        if (content.hasItem(ITEMNAME_LISTENER_CLASS_OR_MODULE)) {
            String currentListener = (String) content.getItemValue(ITEMNAME_LISTENER_CLASS_OR_MODULE);
            if (moduleOrClassname.equals(currentListener)) {
                // listener is up to date
                return;
            }
        }
        
        // update or set listener
        content.setItemValue(ITEMNAME_LISTENER_CLASS_OR_MODULE, moduleOrClassname);
        content.save();
    }
    
    /**
     * removes the stored listener reference from the given storage or content
     * @param content
     * @throws WGAPIException 
     */
    public void removeListener(WGContent content) throws WGAPIException {
        if (content.hasItem(ITEMNAME_LISTENER_CLASS_OR_MODULE)) {
            content.removeItem(ITEMNAME_LISTENER_CLASS_OR_MODULE);
            content.save();
        }
    }
    
    private WGHierarchicalDatabaseListener getListener(WGContent content) throws WGAPIException {
        String listenerImplName = (String) content.getItemValue(ITEMNAME_LISTENER_CLASS_OR_MODULE);
        if (listenerImplName != null) {
            try {
                return _listenerFactory.createListenerInstance(listenerImplName);
            }
            catch (Throwable e) {
                throw new WGAPIException("Unable to create listener instance '" + listenerImplName + "'.", e);
            }
        } else {
            return null;
        }
    }
    
    private void fireEvent(WGContent content, WGHierarchicalDatabaseEvent event) throws WGAPIException, WGHierarchicalDatabaseEventException {
                        
        String downTopEvent = _hdbModelMode ? "pre" : "post";
        if (event.getType().toLowerCase().indexOf(downTopEvent) != -1) {
            if (content != null) {
                WGContent parent = getParentContent(content);
                if (parent != null) {
                    fireEvent(parent, event);
                }
            }
        }
        
        // determine direct child
        String parentKeyOfContentToFireEventOn = null;        
        if (event.getParentContent() != null) {
            parentKeyOfContentToFireEventOn = getKey(event.getParentContent());
        }
        String keyOfContentToFireEventOn = getKey(content);        
        if (parentKeyOfContentToFireEventOn != null && keyOfContentToFireEventOn.equals(parentKeyOfContentToFireEventOn)) {
            event.setDirectChild(true);
        } else {
            event.setDirectChild(false);
        }
        
        WGHierarchicalDatabaseListener listener = getListener(content);        
        if (listener != null) {
            // set listener content on event
            event.setListenerContent(content);
            try {
                if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_CREATE_CONTENT)) {
                    listener.preCreateContent(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_CREATE_CONTENT)) {
                    listener.postCreateContent(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_UPDATE_CONTENT)) {
                    listener.preUpdateContent(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_UPDATE_CONTENT)) {
                    listener.postUpdateContent(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_DELETE_CONTENT)) {
                    listener.preDeleteContent(event);                    
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_DELETE_CONTENT)) {
                    listener.postDeleteContent(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_MOVE_CONTENT_TO) && listener instanceof WGHierarchicalDatabaseListenerV2) {
                	((WGHierarchicalDatabaseListenerV2)listener).preMoveContentTo(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_MOVE_CONTENT_TO) && listener instanceof WGHierarchicalDatabaseListenerV2) {
                	((WGHierarchicalDatabaseListenerV2)listener).postMoveContentTo(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_MOVE_CONTENT_FROM) && listener instanceof WGHierarchicalDatabaseListenerV2) {
                	((WGHierarchicalDatabaseListenerV2)listener).preMoveContentFrom(event);
                } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_MOVE_CONTENT_FROM) && listener instanceof WGHierarchicalDatabaseListenerV2) {
                	((WGHierarchicalDatabaseListenerV2)listener).postMoveContentFrom(event);
                } 
            }
            catch (WGHierarchicalDatabaseEventException e) {
                throw e;
            }
            catch (Throwable e) {
            	if (listener instanceof WGHierarchicalDatabaseListenerV2) {
            		throw new WGAPIException("Execution of event listener '" + ((WGHierarchicalDatabaseListenerV2)listener).getName() + "' failed bc. of exception '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'", e);
            	} else {
            		throw new WGAPIException("Execution of event listener failed bc. of exception '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'", e);
            	}
            }                        
        }       
        
        String topDownEvent = _hdbModelMode ? "post" : "pre";
        if (event.getType().toLowerCase().indexOf(topDownEvent) != -1) {
            if (content != null) {
                WGContent parent = getParentContent(content);
                if (parent != null) {
                    fireEvent(parent, event);
                }        
            }
        }
        
    }
    
    private WGContent getParentContent(WGContent content) throws WGAPIException {
        WGStructEntry entry = content.getStructEntry();
        if (entry.isRoot()) {
            return null;
        } else {
            return entry.getParentEntry().getReleasedContent(getLanguage().getName());
        }
    }


    private WGStructEntry createStructEntry(WGStructEntry parent, String title) throws WGAPIException {
        WGStructEntry newEntry = null;
        if (title == null) {
            title = UIDGenerator.generateUID();
        }
        
        if (parent == null) {
            newEntry = _database.getArea(AREA_CONTENT).createRootEntry(getContentType(), title);
        } else {
            newEntry = _database.createStructEntry(parent, getContentType(), title);            
        }              
        newEntry.save();
        return newEntry;
    }
         
    
    private WGContent innerCreateContent(WGStructEntry entry, String title, Object parameter) throws WGAPIException {
        if (title == null) {
            title = (String) entry.getStructKey();
        }
        
        entry.getDatabase().getSessionContext().setAttribute(SESSIONPARAM_HDBPARAMETER, parameter);
        try { 
            WGContent content = entry.createContent(getLanguage(), title);
            if (!_useVersioning) {
            	content.publish();
            }
            else {
                content.save();
            }
            return content;
        }
        finally {
            entry.getDatabase().getSessionContext().removeAttribute(SESSIONPARAM_HDBPARAMETER);
        }
        
    }
    
    /**
     * creates a storage with the given id
     * @param id
     * @param parameter
     * @return WGContent
     * @throws WGAPIException
     * @throws WGDuplicateKeyException if storage already exists
     */
    public WGContent createStorage(String id, Object parameter) throws WGAPIException {
        return createStorage(null, id, parameter);
    }
    
    /**
     * creates a storage with the given id
     * @param id
     * @return WGContent
     * @throws WGAPIException
     * @throws WGDuplicateKeyException if storage already exists
     */
    public WGContent createStorage(String id) throws WGAPIException {
        return createStorage(id, null);
    }
    
    /**
     * creates a storage with the given id as child of the given parent storage
     * @param parent - parent storage
     * @param id
     * @param parameter
     * @return WGContent
     * @throws WGAPIException
     * @throws WGDuplicateKeyException if storage already exists
     */
    public WGContent createStorage(WGContent parent, String id, Object parameter) throws WGAPIException {
    	WGOperationKey lock = null; 
    	if (parent != null) {
    		lock = _database.obtainOperationKey(WGOperationKey.OP_HDB_CREATE_STORAGE, parent.getDocumentKey() + id);
    	} else {
    		lock = _database.obtainOperationKey(WGOperationKey.OP_HDB_CREATE_STORAGE, id);
    	}
    	synchronized (lock) {
    	    try {
        	    lock.setUsed(true);
                String uid = createStorageUID(parent, id);
                // check if storage exists
                if (getStorage(uid) != null) {
                    throw new WGDuplicateKeyException("Storage '" + uid + "' already exists.");
                }
                
                WGTransaction trans = _database.startTransaction();
                try {
                
                    WGStructEntry entry = null;
                    if (parent != null) {
                        // check if parent is a storage
                        if (!isStorage(parent)) {
                            throw new WGIllegalArgumentException("Unable to create storage with id '" + id + "' - '" + parent.getTitle() + "' is not a valid parent storage.");
                        }
                        entry = createStructEntry(parent.getStructEntry(), id.toLowerCase());    
                    } else {
                        entry = createStructEntry(null, id);
                    }        
                    
                    WGContent storage = innerCreateContent(entry, id.toLowerCase(), parameter);
                    storage.setItemValue("isStorageEntry", Boolean.TRUE);
                    storage.setUniqueName(uid);
                    storage.save();
                    if (_useVersioning) {
                    	storage.publish("");
                    }
                                   
                    // protect storage
                    protect(entry);
                    trans.commit();
                    return storage;
                
                }
                finally {
                    if (trans.isOpen()) {
                        trans.rollback();
                    }
                }
                
    	    } finally {
    	        lock.setUsed(false);
    	    }
		}
    }
    
    /**
     * creates a storage with the given id as child of the given parent storage
     * @param parent - parent storage
     * @param id
     * @return WGContent
     * @throws WGAPIException
     * @throws WGDuplicateKeyException if storage already exists
     */
    public WGContent createStorage(WGContent parent, String id) throws WGAPIException {
        return createStorage(parent, id, null);
    }
    
    /**
     * returns the storage with the given uid
     * @param uid
     * @return WGContent 'null' if storage does not exist
     * @throws WGAPIException
     */
    public WGContent getStorage(String uid) throws WGAPIException {
        return _database.getContentByName(uid.toLowerCase());
    }
    
    /**
     * creates the storage with the given id if not exists
     * @param id
     * @param parameter
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent getOrCreateStorage(String id, Object parameter) throws WGAPIException {
    	WGOperationKey lock = _database.obtainOperationKey(WGOperationKey.OP_HDB_GET_OR_CREATE_STORAGE, id);
    	synchronized (lock) {
    	    try {
    	        lock.setUsed(true);
    	        if (getStorage(id) == null) {
    	            return createStorage(id, parameter);
    	        } else {
    	            return getStorage(id);
    	        }
    	    } finally {
    	        lock.setUsed(false);
    	    }
		}
    }
    
    /**
     * creates the storage with the given id if not exists
     * @param id
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent getOrCreateStorage(String id) throws WGAPIException {
        return getOrCreateStorage(id, null);
    }
    
    /**
     * creates the storage with the given id as child of the given parent storage
     * @param parent the parent storage
     * @param id
     * @param parameter
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent getOrCreateStorage(WGContent parent, String id, Object parameter) throws WGAPIException {    	
    	WGOperationKey lock = _database.obtainOperationKey(WGOperationKey.OP_HDB_GET_OR_CREATE_STORAGE, parent.getDocumentKey() + id);
    	synchronized (lock) {
    	    try {
    	        lock.setUsed(true);
                String uid = createStorageUID(parent, id);
                if (getStorage(uid) == null) {
                    return createStorage(parent, id, parameter);
                } else {
                    return getStorage(uid);
                }
    	    } finally {
    	        lock.setUsed(false);
    	    }
		}
    }
    
    
    /**
     * creates the storage with the given id as child of the given parent storage
     * @param parent the parent storage
     * @param id
     * @return WGContent
     * @throws WGAPIException
     */
    public WGContent getOrCreateStorage(WGContent parent, String id) throws WGAPIException {
        return getOrCreateStorage(parent, id, null);
    }
    
    /**
     * creates the content with the given uid as child of the given parent document, if not yet existent
     * @param parent the parent document
     * @param id The uid of the content
     * @param parameter An optional parameter for HDB listeners
     * @return WGContent The newly created or existent content with the given id
     * @throws WGAPIException
     */
    public WGContent getOrCreateUIDContent(WGContent parent, String id, Object parameter) throws WGAPIException {        
                           
            String uid = createStorageUID(parent, id);
            WGContent storage = getStorage(uid);
            if (storage == null) {
                
                WGTransaction trans = _database.startTransaction();
                try {
                    storage = createContent(parent, id.toLowerCase(), parameter);
                    assignContentUID(storage, id);
                    storage.save();
                    trans.commit();
                }
                finally {
                    if (trans.isOpen()) {
                        trans.rollback();
                    }
                }
            }
            return storage;

    }
    
    /**
     * creates the content with the given uid as child of the given parent document, if not yet existent
     * @param parent the parent document
     * @param id The uid of the content
     * @return WGContent The newly created or existent content with the given id
     * @throws WGAPIException
     */
    public WGContent getOrCreateUIDContent(WGContent parent, String id) throws WGAPIException {
        return getOrCreateUIDContent(parent, id, null);
    }
    
    /**
     * deletes the storage with the given id 
     * @param uid
     * @throws WGAPIException
     */
    public void deleteStorage(String uid) throws WGAPIException {
        WGContent storage = getStorage(uid);
        if (storage != null) {
            storage.getStructEntry().remove();
        }
    }
    
    public String createStorageUID(WGContent parent, String id) throws WGAPIException {
        if (id == null) {
            throw new WGIllegalArgumentException("Cannot create storage with id 'null'.");
        }
        if (id.indexOf(STORAGE_DELIMITER) != -1) {
            throw new WGIllegalArgumentException("Character '" + STORAGE_DELIMITER + "' is invalid for storage id '" + id + "'.");
        }
        String uid = null;
        if (parent != null) {
            uid = getStorageID(parent) + STORAGE_DELIMITER + id.toLowerCase();
        } else {
            uid = id.toLowerCase();
        }
        return uid;
    }
    
    /**
     * Issues an UID to a content document.
     * This makes the uid functionality of storages available for contents.
     * Use this method after creating but before saving a content to issue it a UID. 
     * The UID is set as unique name of the content.
     * @param con The content that should be issued a UID
     * @param id The local id of this content
     * @return The UID that was issued to this content.
     * @throws WGAPIException
     */
    public String assignContentUID(WGContent con, String id) throws WGAPIException {
        String uid = createStorageUID(con.getParentContent(), id);
        con.setUniqueName(uid);
        return uid;
    }
    
    private String getStorageID(WGContent storage) throws WGAPIException {
        String uname = storage.getUniqueName();
        if (uname != null) {
            return uname.toLowerCase();
        }
        else {
            throw new WGIllegalDataException("Corrupted HDB data. The storage '" + storage.getStructKey() + "' does not have an unique name");
            
        }
    }
    
    /**
     * Returns all root documents in this Database
     * @throws WGAPIException 
     */
    public List getRootDocuments() throws WGAPIException {
        WGArea contentArea = _database.getArea(AREA_CONTENT);
        Iterator rootEntries = contentArea.getRootEntries().iterator();
        List docs = new ArrayList();
        while (rootEntries.hasNext()) {
            WGStructEntry rootEntry = (WGStructEntry) rootEntries.next();
            WGContent rootContent = rootEntry.getReleasedContent(getLanguage().getName());
            if (rootContent != null) {
                docs.add(rootContent);
            }
        }
        return docs;
    }

	/**
	 * Returns the default listener factory for all HDB instances
	 */
	public static WGHierarchicalDatabaseListenerFactory getDefaultListenerFactory() {
		return _defaultListenerFactory;
	}

	/**
     * Sets the default listener factory for all HDB instances
     */
	public static void setDefaultListenerFactory(WGHierarchicalDatabaseListenerFactory listenerFactory) {
		_defaultListenerFactory = listenerFactory;
	}

	/**
	 * Returns the default startup implementation for all HDB instances
	 */
	public static WGHierarchicalDatabaseStartup getDefaultStartupImpl() {
		return _defaultStartupImpl;
	}

	/**
     * Sets the default startup implementation for all HDB instances
     */
	public static void setDefaultStartupImpl(WGHierarchicalDatabaseStartup startupImpl) {
		_defaultStartupImpl = startupImpl;
	}
	
	/**
	 * Adds a HDB core listener
	 */
	public static void addCoreListener(WGHierarchicalDatabaseCoreListener listener) {
		_coreListeners.add(listener);		
	}
	
	/**
     * Removes a HDB core listener
     */
	public static void removeCoreListener(WGHierarchicalDatabaseCoreListener listener) {
		_coreListeners.remove(listener);		
	}

    /**
     * Returns the behaviour of this HDB instance regarding post update events
     */
    public PostUpdateEventBehaviour getPostUpdateEventBehaviour() {
        return _postUpdateEventBehaviour;
    }

    /**
     * Sets a behaviour for post update events
     */
    public void setPostUpdateEventBehaviour(PostUpdateEventBehaviour postUpdateEventBehaviour) {
        _postUpdateEventBehaviour = postUpdateEventBehaviour;
    }

    /**
     * Determines if this HDB instance uses versioning. If true then new documents will begin in draft state, updates to documents will be performed in new versions
     */
    public boolean isUseVersioning() {
        return _useVersioning;
    }

    /**
     * Sets if this HDB instance should use versioning. If true then new documents will begin in draft state, updates to documents will be performed in new versions.
     */
    public void setUseVersioning(boolean useVersioning) {
        _useVersioning = useVersioning;
    }

    /**
     * Returns if this HDB instance is in HDBModel model. This influences the order in which events are executed.
     */
    public boolean isHdbModelMode() {
        return _hdbModelMode;
    }

    /**
     * Sets if this HDB instance is in HDBModel mode. This will influence the order of executed events.
     */
    public void setHdbModelMode(boolean hdbModelMode) {
        _hdbModelMode = hdbModelMode;
    }
    

}
