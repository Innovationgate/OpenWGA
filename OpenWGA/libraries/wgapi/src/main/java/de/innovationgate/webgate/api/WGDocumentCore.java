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
import java.io.InputStream;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;



/**
 * Represents the core of a WGA document. WGA Documents can be contents, struct entries, areas, doctypes, WebTML-Layouts, CSS/JS-Resources, language definitions, file containers and user profiles.
 * All these diferent document classes are accessed via this single interface, though the implementing classes can be different for each doc class.
 * <p>
 * A WGDocumentCore "lives" inside a WGDocument-Object and is directly accessed only by this object.
 * While many threads access the same WGDocument object, each WGDocument object holds multiple WGDocumentCore objects, one for each thread, all representing the same background data. 
 * Background resources held by the core implementation should be managed with that fact in mind.
 * After each closed session, all document cores are dropped and will be re-retrieved by their next calling.
 * Therefor WGDocumentCores themselves need not to be thread safe, cause they are specific to one thread and one user session. 
 */
public interface WGDocumentCore extends WGExtensionDataContainer {
	
	/**
	 * Returns the type of this document. 
	 * @return The type. Use Constants WGDocument.TYPE_...
	 */
	public int getType();
	/**
	 * Can provide an fast access key, that the corresponding database core implementation can use to re-retrieve an instance of this core. 
	 * Provide null, if this feature is not implemented for this core.
	 * @return Object The fast access key, that will be used in the method fastAccess of the corresponding database core implementation.
	 * @throws WGBackendException 
	 */
	public Object getFastAccessKey() throws WGBackendException;
	
	/**
	 * Tests, if this document is marked as deleted.
	 * @return boolean
	 * @throws WGAPIException
	 */
	// Info about this WGDocumentCore
	public boolean isDeleted() throws WGAPIException;
	/**
	 * Should return true, if this document core cannot be re-retrieved by it's individual key. In result, the WGAPI won't cache the wrapper object of this document core.
	 * @return boolean
	 * @throws WGAPIException 
	 */
	public boolean isTemporary() throws WGAPIException;
	
	/**
	 * Tests, if the content contains an item of the given name.
	 * @param strName
	 * @return boolean
	 * @throws WGBackendException 
	 */
	// Methods to retrieve doc items
	public boolean hasItem(String strName) throws WGAPIException;
	/** 
	 * Returns the value of an content item. Applies only to contents.
	 * @param strName Name of the requested content item.
	 * @return Object
	 * @throws WGAPIException  
	 */
	public Object getItemValue(String strName) throws WGAPIException;
	
	/**
	 * Retrieves various meta data for the document core.
	 * The method should answer all meta data names, that are provided as constants META_... for the specific WGDocument subclass. Further documentation of the specific data expected for each meta data name will be provided in the wgapi implementation handbook.
	 * @param type The type of the meta data. See constants META_ of the specific WGDocument subclass for this doc class.
	 * @return The value of the requested meta data for this document core.
	 * @throws WGAPIException
	 */
	public Object getMetaData(String type) throws WGAPIException;
	/**
	 * Returns a list of the names of embedded files. Applies only to contents and file containers.
	 * @return ArrayList
	 * @throws WGBackendException 
	 */
	public List<String> getFileNames() throws WGAPIException;
	
	/**
	 * Tests if a file of the given name is attached to the document
	 * @throws WGBackendException
	 */
	public boolean hasFile(String file) throws WGAPIException;
	/**
	 * Retrieves an input stream to the data of an embedded file. Applies only to content and file containers.
	 * @param strFile The name of the file
	 * @return InputStream
	 * @throws WGAPIException
	 */
	public java.io.InputStream getFileData(String strFile) throws WGAPIException;
	
	/**
	 * Returns metadata about the created derivates for the given file. May return null if derivates are not supported.
	 * @param strFile The file name
	 * @return List of derivate metadata
	 * @throws WGAPIException
	 */
	public List<WGFileDerivateMetaData> getFileDerivates(String strFile) throws WGAPIException;
	
	/**
	 * Returns the size of an embedded file in bytes. Applies only to contents and file containers.
	 * @param strFile The name of the file, whose size is requested.
	 * @return int
	 * @throws WGAPIException 
	 */
	public int getFileSize(String strFile) throws WGAPIException;
	/**
	 * Retrieves the creation date of this document.
	 * @return Date
	 * @throws WGAPIException
	 */
	public Date getCreated() throws WGAPIException;
	/**
	 * Returns the date of the last modification of this document.
	 * @return Date
	 * @throws WGAPIException 
	 */
	public Date getLastModified() throws WGAPIException;
	
	/**
	 * Sets a new value for a content item.
	 * @param strName Name of the item to be modified.
	 * @param value New value of the item
	 * @return true, if the modification succeeded 
	 * @throws WGAPIException 
	 */
	// Methods to modify document data
	public boolean setItemValue(String strName, Object value) throws WGAPIException;
	/**
	 * Modifies various meta data for the document core.
	 * The method should answer all meta data names, that are provided as constants META_... for the specific WGDocument subclass. Further documentation of the specific data expected for each meta data name will be provided in the wgapi implementation handbook.
	 * @param name The name of the meta data. See constants META_ of the specific WGDocument subclass for this doc class.
	 * @return true, if the modification succeeded 
	 * @throws WGAPIException 
	 */
	public boolean setMetaData(String name, Object value) throws WGAPIException;
	/**
	 * Called to save modifications to this document core.
	 * @return The revision that was the result of this save operation. null if this database does not support revisions.
	 * @throws WGAPIException
	 */
	public WGDatabaseRevision save(java.util.Date lastModified) throws WGAPIException;

	/**
	 * Evaluates an expression in a database-native expression language on this document core.
	 * @param expression The expression to be evaluated
	 * @return Object The expression result.
	 * @throws WGExpressionException
	 * @throws WGBackendException 
	 */
	public Object evaluateExpression(String expression) throws WGAPIException;


	

	
	/**
	 * Called by the WGAPI after instantiation of this document core to provide a reference to the wrapping WGDocument object. The implementation can store this reference in a member variable.
	 * @param doc The WGDocument object, that wraps this document core.
	 */
	public void setWGDocument(WGDocument doc);
	/**
	 * Called when this core is disposed "untimely", i.e. before the current session is closed
	 */
	public void dispose() throws WGAPIException;
	
	/**
	 * Returns the names of all items in this document
	 * @throws WGBackendException 
	 */
	public List<String> getItemNames() throws WGAPIException;
	
	/**
	 * Removes an item from the document
	 * @param Name Name of the item to remove
	 * @throws WGIllegalArgumentException 
	 */
	public boolean removeItem(String Name) throws WGAPIException;
	
	/**
	 * Should return, if the data retrieved as items and metas of this document can be cached while the document doesn't change
	 */
	public boolean isDataCacheable();
	
	/**
	 * Attach a file from disk to the document
	 * @param file
	 * @throws WGAPIException 
	 */
	public boolean attachFile(File file) throws WGAPIException;
	/**
	 * Removes a file attachment from this document
	 * @param name Name of the file to remove
     * @throws WGAPIException
	 */
	public boolean removeFile(String name) throws WGAPIException;
	
    /**
	 * Removes this document from the database
     * @throws WGAPIException
     * @return The revision that was the result of this remove operation. null if this database does not support revisions.
	 */
	public WGDatabaseRevision remove() throws WGAPIException;
	
    /**
	 * Should return if the document has been saved. False means that it is a new document not yet persisted. 
     * @throws WGAPIException 
	 */
	public boolean isSaved() throws WGAPIException;
	
	/**
	 * Returns the native backend object of this doc core if there is any
	 * @throws WGBackendException 
	 */
	public Object getNativeObject() throws WGAPIException;
	
    /**
     * Returns the dbReference of the database that this document core was created from
     */
    public String getOriginDatabase();
    
    /**
     * Renames an attached file
     * @param oldFileName - file to rename
     * @param newFileName - new name for the file
     */
	public void renameFile(String oldFileName, String newFileName) throws WGAPIException ;
	
	/**
	 * retrieves meta informations for the given file
	 * May throw an exception if method is used on a document that does not serve file metadata.
	 * Use {@link #hasFileMetadata()} to determine if metadata is present.
	 * @param strFile - the filename
	 * @return WGFileMetaData
	 * @throws WGAPIException 
	 */
	public WGFileMetaData getFileMetaData(String strFile) throws WGAPIException;
	
	 /**
     * retrieves the extension data handler for the given file or null if extension data is not supported
     * @param strFile - the filename
     * @return WGExtensionDataContainer
     * @throws WGAPIException 
     */
	public WGExtensionDataContainer retrieveFileExtensionDataHandler(String strFile) throws WGAPIException;
	
	/**
	 * Sets a content relation to the designated target
	 * @param data All data of the relation
	 * @return The previously mapped document for this relation
	 * @throws WGAPIException
	 */
	public WGDocumentCore setRelation(WGRelationData data) throws WGAPIException;
	
	/**
	 * Retrieves the target content document of the given content relation
	 * @param name Name of the relation
	 * @return The content behind this relation
	 * @throws WGAPIException
	 */
	public WGDocumentCore getRelation(String name) throws WGAPIException;
	
	/**
	 * Retrieves data for the given content relation.
	 * This should work even if the relation does currently not point to a published content.
	 * @param name The name of the relation
	 * @return The data of the relation
	 * @throws WGAPIException
	 */
	public WGRelationData getRelationData(String name) throws WGAPIException;
	
	   /**
     * Removes a content relation
     * @param name Name of the relation
     * @return The content document previously mapped to this relation
     * @throws WGAPIException
     */
    public WGDocumentCore removeRelation(String name) throws WGAPIException;
    
    /**
     * Returns the names of all relations that this content document holds
     * @return List of Strings, representing relation names
     * @throws WGAPIException
     */
    public List<String> getRelationNames() throws WGAPIException;
    
    /**
     * Determines if this document serves metadata for file attachments
     * @return true, if file metadata is available
     * @throws WGAPIException
     */
    public boolean hasFileMetadata() throws WGAPIException;
    
    
    /**
     * Returns the names of all relations on this content document that
     * belong to the given relation group.
     * @param group The group name
     * @param order Optional order in which to return relations, evaluated against the relation targets, null for no order
     * @return List of Strings, representing relation names
     * @throws WGBackendException
     * @throws WGAPIException 
     */
    
    public List<String> getRelationNamesOfGroup(String group, WGColumnSet order) throws WGBackendException, WGAPIException;
    
    
    /**
     * Creates a derivate for the addressed file. The derivate should be instantly stored.
     * @param originalFileName The name of the original file
     * @param creator Identifier of the creator class of this derivate
     * @param derivateName Name of the derivate
     * @param in The data of the derivate file
     * @param customMdFields Additional custom file metadata fields to store for the derivate
     * @return The metadata of the created derivate
     * @throws WGAPIException 
     * @throws WGNotSupportedException if derivates are not supported on this database
     */
    public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String,Object> customMdFields) throws WGAPIException, WGNotSupportedException;
    
    /**
     * Deletes an existing file derivate
     * @param id The ID of the derivate
     * @throws WGAPIException 
     */
    public void removeFileDerivate(String id) throws WGAPIException;
    
    /**
     * Returns the metadata of a file derivate. Should return null if derivates are not supported
     * @param id The ID of the derivate
     */
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException;
    
    /**
     * Writes the modified metadata of a file derivate to the database.
     * @param md The metadata
     * @throws WGAPIException 
     * @throws WGNotSupportedException if derivates are not supported on this database
     */
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException, WGNotSupportedException;
   
    /**
     * Returns the data of a file derivate. Should return null if derivates are not supported
     * @param id The ID of the derivate
     */
    public InputStream getFileDerivateData(String id) throws WGAPIException;    

    
    /**
     * Returns update logs for the last updates done on this document, in descending chronological order
     * @throws WGAPIException
     */
    public abstract Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException;
    
    /**
     * Marks a file metadata object as being modified, so the core knows that it must be saved
     * @throws WGAPIException
     */
    public void markFileMetaDataModified(WGFileMetaData md) throws WGAPIException;
    
    
		
}

