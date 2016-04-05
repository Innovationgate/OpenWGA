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

import java.io.InputStream;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.BackendAuthSession;

/**
 * Represents the core of a WGA database.
 * WGDatabase wraps implementations of this interface and uses them to communicate with WGA content stores, design stores, data repositories or user profile stores.
 * WGDatabaseCore implementations should be "thread-safe", so they should expect that they will be concurrently used by multiple threads.
 */
public interface WGDatabaseCore {

	/**
	 * Opens a database initially. It is ensured, that this method is to be called before any work on this database core is done. 
 	 * The implementation should also open a database session implicitly.
	 * @param db The WGDatabase object, that will wrap this database core.
	 * @param path The path of the database, that should be opened. Interpretation of the path is up to the specific database core
	 * @param user The user name for opening the initial session.
	 * @param pwd The password for the given username
	 * @param prepareOnly Flag if the database should only be prepared for opening
	 * @return The access level for the opened database under the current user session. Use constants WGDatabase.ACCESSLEVEL_.... as return. Return WGDatabase.ACCESSLEVEL_NOTLOGGEDIN, if the opening failed due to wrong path information or login.
	 * @throws WGAPIException 
	 */
	public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGAPIException;
    
	/**
	 * Opens a session for the given user/thread. Sessions are specific to single java threads and cannot be used across threads. 
	 * It is ensured, that this method is to be called after each call of closeSession, before work on this database core starts again in the current thread.
	 * @param authSession The authentication session of the current user. If there is no authentication module configured this is of type {@link BackendAuthSession}
	 * @param credentials The credentials that the user gave for authenticating
	 * @param master States, that the used login is the master login
	 * @return The access object, containing the access level of the current user session. Use constants WGDatabase.ACCESSLEVEL_.... as return. Returns WGDatabase.ACCESSLEVEL_NOTLOGGEDIN, if the opening failed due to wrong login.
	 * @throws WGAPIException 
	 */
	public WGUserAccess openSession(AuthenticationSession authSession, Object credentials, boolean master) throws WGAPIException;
	/**
	 * Called when a database implementation is completely closed and all cached data of it is dropped. Should release all resources.
	 * @throws WGAPIException
	 */
	public void close() throws WGAPIException;
	/**
	 * Called when a database session is closed, i.e. the current thread/user stops working with this database for the logged in user.
	 * @throws WGAPIException 
	 */
	public void closeSession() throws WGAPIException;
	
	/**
	 * Retrieves a descriptive database title.
	 * @return String
	 * @throws WGBackendException 
	 */
	public String getTitle() throws WGBackendException;
	/**
	 * Returns a description of this core implementation type. Normally in format "dbplatform/dbdesign/furtherInfos/..."
	 * @return The implementation description
	 */
	public abstract String getTypeName();

	/**
	 * Returns an revision indicator of the last change done to this database
	 * This may be a anything that indicates the revision of the current database, like some java.util.Date or a sequence number. 
	 * If the database implementation does not maintain such a date, it should return the creation date of the database. In that case the change dates of the documents are used to control cache dropping.
	 * @return Date
	 * @throws WGAPIException 
	 */
	public Comparable getRevision() throws WGAPIException;
	
	/**
	 * Returns a date object corresponding to the given revision indicator
	 * Use this method to get concrete dates for the revisions returned by  {@link #getRevision()}.
	 * This method is obliged to throw a {@link WGWrongRevisionException} if the revision type does not match this database
	 * of the given revision is not known.
	 * @param lastChanged A revision indicator returned by {@link #getRevision()}
	 * @return A date
	 * @throws WGAPIException
	 * @throws WGWrongRevisionException if the given revision is no revision of the current database
	 */
	public Date getRevisionDate(Comparable lastChanged) throws WGAPIException, WGWrongRevisionException;
	
	/**
	 * Retrieves the value of an extension data field stored in this database.
	 * This feature is only available in WGA Content Stores of Version 5 or higher.
	 * @param name The name of the field.
	 * @return The value of the requested field for this database core. 
	 * @throws WGAPIException 
	 */
	public Object getExtensionData(String name) throws WGAPIException;
	

    /**
     * Retrieves names of all extension data fields stored in this database.
     * This feature is only available in WGA Content Stores of Version 5 or higer.
     * @return List of field names 
     * @throws WGAPIException 
     */
    public List<String> getExtensionDataNames() throws WGAPIException;
	
	/**
     * Writes an extension data field to in this database.
     * This writes a custom data value to the database. The value is immediately stored.
     * This feature is only available in WGA Content Stores of Version 5 or higher.
     * @param name The name of the field.
     * @param value The value to store
     * @throws WGAPIException 
     */
    public void writeExtensionData(String name, Object value) throws WGAPIException;
    
    
    /**
     * Removes an extension data field from this database
     * @param name The name of the field to remove
     * @throws WGAPIException
     */
    public void removeExtensionData(String name) throws WGAPIException;
	
	/**
	 * Returns the roles that this database complies.
	 * Based on this information, the WGAPI decides, which methods can be called on this database core.
	 * @return A list containing the constants WGDatabase.ROLE_.... for the roles, that this database complies.
	 */
	public List getRoles();
	/**
	 * Returns information on specific features, that this database implements.
	 * @param feature The requested feature. The constants WGDatabase.FEATURE_... are used here.
	 * @return true, if this feature is implemented
	 */
	public boolean hasFeature(String feature);

	/**
	 * Retrieves all child struct entries for the provided struct entry.
	 * @param structEntry The struct entry, whose children are to be retrieved
	 * @param order Instructions about the desired order
	 * @return An iterator of all child entries, ordered by their intended default order
	 * @throws WGAPIException 
	 */
	public Iterator<WGDocumentCore> getChildEntries(WGStructEntry structEntry, WGPageOrderSet order) throws WGAPIException;
	/**
	 * Retrieves the root entries (i.e. entries without parents) of the given area.
	 * @param area The area, whose root entries are to be retrieved
	 * @param pageOrder Order set denoting the order in which entries should be served, null for default order (position, title, both ascending)
	 * @return An iterator of root entries in their intended default order
	 * @throws WGAPIException 
	 */
	public Iterator<WGDocumentCore> getRootEntries(WGArea area, WGPageOrderSet pageOrder) throws WGAPIException;
	
	/**
	 * Retrieves a struct entry for the given struct key
	 * @param key
	 * @return The struct entry of that key. Null if no struct entry has this key.
	 * @throws WGAPIException
	 */
	public WGDocumentCore getStructEntryByKey(Object key) throws WGAPIException;
	/**
	 * Retrieves the parent struct enty for the given entry.
	 * @param entry The struct entry, whose parent is to be retrieved
	 * @return The parent entry. Null if the parameter entry is a root entry and therefor has no parent.
	 * @throws WGAPIException 
	 */
	public WGDocumentCore getParentEntry(WGStructEntry entry) throws WGAPIException;
	
	/**
	 * Retrieves a list of all content unter the specified struct entry.
	 * @param structEntry The struct entry, whose content is to be retrieved.
	 * @param includeArchived Decides, if archived documents should be also retrieved
	 * @return An unordered list of all content for this struct entry. An empty list, if there is no content for this struct entry.
	 * @throws WGAPIException 
	 */
	public List<WGDocumentCore> getAllContent(WGStructEntry structEntry, boolean includeArchived) throws WGAPIException;
	/**
	 * Retrieves the content for the provided content key.
	 * If the version of the content key is 0 then the released content of the given struct and language must be retrieved.
	 * @param key The content key
	 * @return The retrieved content. Null, if there is no content for this key.
	 * @throws WGAPIException
	 */
	public WGDocumentCore getContentByKey(WGContentKey key) throws WGAPIException;
	/**
	 * Retrieves a content by it's unique name for the given language. If language is null, the default language of the content store is used.
	 * @param strName The unique name
	 * @param strLanguage The language name (i.e. short code). If null, the default language of the content store is to be used.
	 * @return The retrieved content for this name. Null if there is no content with this unique name. 
	 * @throws WGAPIException 
	 */
	public WGDocumentCore getContentByName(String strName, String strLanguage) throws WGAPIException;
	
	/**
	 * Returns a struct entry by it's unique name
	 * @param strName The unique name
	 * @return A struct entry of that name or null
	 * @throws WGAPIException
	 */
	public WGDocumentCore getStructEntryByName(String strName) throws WGAPIException;
	
	/**
	 * Creates a "dummy content" object, that is used, when rendering WebTML without a content context. 
	 * May return the "fake" document core implementation, if no native expression language is needed.
	 * @return WGDocumentCore
	 * @throws WGAPIException 
	 */
	public WGDocumentCore getDummyContent(String language) throws WGAPIException;
	/**
	 * Queries the database for content.
     * For full content store implementations:
     * The native query implementation should itself enforce the query option WGDatabase.QUERYOPTION_ENHANCE, 
     * if set as parameter, regarding the following field:
     * - "ishiddenfrom" regarding the role given as query option WGDatabase.QUERYOPTION_ROLE
     * It might choose to enforce more visibility fields to optimize performance, but is not obliged to do so.
	 * @param type Type of query. Interpretation is up to the specific core implementation.
	 * @param query The query
	 * @param parameters Query options as Map influencing the query result. See WGDatabase.QUERYOPTION_... for options keys.
	 * @return WGResultSetCore The result set of this query as a specific WGResultSetCore implementation containing content.
	 * @throws WGAPIException 
	 */
	public WGResultSetCore query(String type, String query, java.util.Map parameters) throws WGAPIException;

	/**
	 * Called for fast re-retrieval of a document core, if that core on first instantiation specified a fast access key.
	 * @param key The fast access key, that the document core provided, when it was instantiated the first time.
	 * @return Another instance of the document core for this fast access key. Null, if none could be retrieved.
	 * @throws WGAPIException  
	 */
	public WGDocumentCore fastAccess(int type, Object key) throws WGAPIException;
	
	/**
	 * Parses the string representation of a struct key (as used in URLs) to the implementation specific format.
	 * @param key The string representation of the struct key
	 * @return The struct key object to be used with this implementation.
	 * @throws WGAPIException 
	 */
	public Object parseStructKey(String key) throws WGAPIException;
	

	
	/**
	 * Create a design document of the given type and keys.
	 * @param type Design document type. Use constants WGDocument.TYPE_...
	 * @param name Unique name of design object. Use only A-Z,a-z,0-9 and _
	 * @param mediaKey Media key, if the document type is a TML module
	 * @return The new created design document
	 * @throws WGAPIException 
	 */
	public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) throws WGAPIException;
	/**
	 * Creates a struct entry.
	 * @param key Key of the struct entry if the implementation supports using explicit keys. Leave null otherwise.
	 * @param reference Reference document, which can be an WGArea (struct will be root in this area) or another WGStructEntry (struct will be child of that)
	 * @param contentType Content type for this struct entry
	 * @return The newly created struct entry
	 * @throws WGAPIException 
	 */
	public WGDocumentCore createStructEntry(Object key, WGDocument reference, WGContentType contentType) throws WGAPIException;
	/**
	 * Called to create a content document. Usage of parameters may vary according to the subset of WGA features implemented by this database core
	 * @param structEntry The structentry for the content
	 * @param language The language of the content
	 * @param title The title of the content
	 * @param version The version of the content if this should explicitly be set (only in clone creation!). Leave null for normal creation.
	 * @return The newly created content object.
	 * @throws WGAPIException 
	 */
	public WGDocumentCore createContent(WGStructEntry structEntry, WGLanguage language, String title, int version) throws WGAPIException;
	
	/**
	 * Retrieves the class of a dedicated workflow engine for this implementation. Return null to use the default engine of the runtime.
	 */
	public Class getDedicatedWorkflowEngine();
	
	/**
	 * Determines if the currently logged in user is a member of the given user/group/role list.
	 * @param userList
	 * @return true if so, false otherwise.
	 * @throws WGAPIException 
	 */
	public boolean isMemberOfUserList(List userList) throws WGAPIException;
	
	/**
	 * Create a copy of the given doc core. Optional feature needed for methods WGDatabase.createDraftCopy and WGDatabase.createCopy
	 * @param original
	 * @return The copy.
	 * @throws WGAPIException 
	 */
	public WGDocumentCore createCopy(WGDocumentCore original) throws WGAPIException;
	
	/**
	 * Called when a transaction should begin.
	 * Needed when database supports feature WGDatabase.FEATURE_TRANSACTIONS.
	 * @return true, if the operation succeeded, false otherwise
	 */
	public boolean beginTransaction() throws WGAPIException;
	/**
	 * Called when a started transaction should be rolled back.
	 * Needed when database supports feature WGDatabase.FEATURE_TRANSACTIONS.
	 */
	public boolean rollbackTransaction() throws WGAPIException ;

	/**
	 * Called when a running transaction should get submitted
	 * Needed when database supports feature WGDatabase.FEATURE_TRANSACTIONS.
	 * @return true, if the operation succeeded, false otherwise
	 */
	public boolean commitTransaction() throws WGAPIException ;
	
	/**
	 * Returns the name of the server for the backend database (if there is one)
	 * @throws WGAPIException 
	 */
	public String getServerName() throws WGAPIException;

	
	
	/**
	 * Returns the version of WGA Content Store that this backend represents.
	 * The features available for this database are dependent on the content store version.
	 * This method returns values of constants WGDatabase.CSVERSION_...
	 * @throws WGAPIException
	 */
	public double getContentStoreVersion() throws WGAPIException;
	
	/**
	 * Returns the patch level of the storage format of this WGA Content Store.
	 * Databases not representing content stores and not offering automatic patching should return 0.
	 * @throws WGAPIException
	 */
	public int getContentStorePatchLevel() throws WGAPIException;
	
	/**
	 * Retrieves the ACL if implementation supports WGDatabase.FEATURE_ACL_MANAGEABLE
	 */
	public WGACLCore getACL();
	
	/**
	 * Retrieves the native backend object for this database if there is any.
	 * @throws WGBackendException 
	 */
	public Object getNativeObject() throws WGAPIException;

	/**
	 * Tests, if an native expression result equals true in the native expression language.
	 * @param result The expression result, retrieved by a call to evaluateExpression
	 * @param doc The document, against which the expression was evaluated
	 * @return boolean
	 */
	public boolean resultIsTrue(Object result, WGDocument doc);	
	/**
	 * Tests, if an native expression result equals false in the native expression language.
	 * @param result The expression result, retrieved by a call to evaluateExpression
	 * @param doc The document, against which the expression was evaluated
	 * @return boolean
	 */
	public boolean resultIsFalse(Object result, WGDocument doc);
	
	

	
	/**
	 * Is called when a database refresh is performed
	 * The core should perform necessary cleanups of cached data and held resources
	 * If the database supports WGAPI transactions it should reduce the impact of this method to the current session while in a transaction
	 */
	public void refresh() throws WGAPIException;
	
	/**
	 * Is called when the cache of the current WGAPI session is cleared.
	 * Should therefor clear all cached data and held resources for the current session. This may throw away make unsaved changes to backend classes.
	 * @throws WGClosedSessionException If currently no session is open
	 * @throws WGAPIException
	 */
	public void clearSessionCache() throws WGAPIException;
	
	/**
	 * Moves a struct entry to a new parent document, either another struct entry of an area. May return false if this operation is not supported.
	 * @param entry The entry to move
	 * @param newParent The new parent document, which can be either an WGStructEntry or a WGArea object
	 * @return true if the operation succeeded, false if it is not supported by this WGAPI implementation.
	 * @throws WGAPIException 
	 */
	public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) throws WGAPIException;
	
	/**
	 * Returns the number of total contents on this struct entry, including those that are not visible to the user.
	 * This is helpful to determine if there are contents unknown to the current user, which may prevent some operations like deletions 
	 * @param entry The entry to test
	 * @return true if there are contents, false otherwise
	 * @throws WGNotSupportedException If the database implementation does not support this operation 
	 * @throws WGBackendException On all other backend related errors
	 */
	public int getContentCount(WGStructEntry entry) throws WGAPIException;
	
	
	/**
	 * Method to inject the current session context to the core. This gives the core the ability to associate internal
	 * threadlocal resources to the corresponding session context.
	 * @param context
	 */
	public void setCurrentSession(WGSessionContext context);
    
    /**
     * Retrieves all design objects of the given doc class.
     * @param type The doc class. See constants WGDocument.FDC_...
     * @return An unordered list of all design objects for this doc class. An empty list if there are no design objects of that class.
     * @throws WGAPIException 
     */
    public abstract List getDesignObjects(int type) throws WGAPIException;

    /**
     * Retrieves a design object by it's doc class and unique name.
     * @param type The doc class of the design object. See Constants under WGDocument.FDC_...
     * @param name The unique name of the design
     * @param strMediaKey For media key specific design objects (e.g. WebTML-Modules), the media key of the design object to be retrieved
     * @return The retrieved design object.
     * @throws WGAPIException
     */
    public abstract WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException;
    /**
     * Returns a file name like it would be converted when a file of this name got attached to a document.
     * WGAPI implementations are allowed to convert the name of an attached file in some ways, e.g. convert
     * the name to lowercase or replace some unallowed characters. This method is used to convert names of 
     * files to attach before they are attached. It can also be used to test attaching behaviour.
     * Implementations that do not support file attaching should return null here.
     * @param name
     */
    public String convertFileNameForAttaching(String name);

    /**
     * Retrieves all content keys of all contents in this database as list.
     * Optional feature. Implementations that do not implement this method shoud return false on hasFeature(WGDatabase.FEATURE_RETRIEVE_ALL_CONTENTKEYS).
     * @param includeArchived Specifies if archived content should be included in the list
     * @return A list of objects #{@link WGContentKey}
     * @throws WGAPIException
     */
    public List getAllContentKeys(boolean includeArchived) throws WGAPIException;

	/**
	 * Notifies the backend database that the session will update data from now on.
     * This is only neccessary in cluster environments when the underlying database connections
     * are kept "readOnly" until a real update happens.

	 */
	public void beginUpdate() throws WGAPIException;

    /**
     * Retrieves list of update logs - i.e. the operations done on documents - since a cutoff revision in ascending order of operation times.
     * Needed if database supports WGDatabase.FEATURE_FIND_UPDATED_DOCS.
     * @param cutoff
     * @return List of objects of type {@link de.innovationgate.webgate.api.WGUpdateLog WGUpdateLog} 
     * @throws WGAPIException 
     */
    public abstract List<WGUpdateLog> getUpdateLogs(Comparable cutoff) throws WGAPIException;
    
    /**
     * Must Return the relations that point to the given released content document
     * @param structKey The structkey of the content
     * @param language The language of the content
     * @param sourceContentClass content class of the content owning the relation. null for contents of all content classes
     * @param relName Name of the relation coming in. null should retrieve relations of all names
     * @param relGroupName Group name of the relation coming in. null should retrieve relations of all groups
     * @param includeUnreleased Specify true to also retrieve documents in draft or approval state. False will retrieve only published documents.
     * @param order Optionally order in which to return relations, evaluated against the relation source documents, null for no order
     * @return List of incoming relations in no particular order
     * @throws WGAPIException
     */
    public List<WGRelationData> getIncomingRelations(Object structKey, String language, String sourceContentClass, String relName, String relGroupName, Boolean includeUnreleased, WGColumnSet order) throws WGAPIException;
    
    /**
     * Tests if the given content type is still in usage, i.e. if there are struct entries referencing it
     * @param ct The content type to test
     * @return true if the content type is in usage, false if not
     * @throws WGAPIException
     */
    public boolean isContentTypeUsed(WGContentType ct) throws WGAPIException;

    /**
     * Tests if the given language is still in usage, i.e. if there are contents referencing it
     * @param lang The language to test
     * @return true if the language is in usage, false if not
     * @throws WGAPIException
     */
    public boolean isLanguageUsed(WGLanguage lang) throws WGAPIException;
    
    /**
     * Returns if a backend service is supported
     * @param serviceName Name of the service
     */
    public boolean isBackendServiceSupported(String serviceName);
    
    /**
     * Calls a backend service whose support is optional. Should throw WGNotSupportedException if the backend service is not supported.
     * @param serviceName The service name. Use WGDatabase.BACKENDSERVICE_*
     * @param params The matching parameters for the called service
     * @return An optional return value of the service
     */
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException;
    
}
