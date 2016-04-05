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

/**
 * An key for an operation involving the backend database.
 * 
 * This key is used to synchronize similar backend database accesses and avoid parallel execution.
 * The key represents a special backend operation, e.g. read structentry "xyz". The backend operation is synchronized on this key object. 
 * So if another thread tries to fetch structentry "xyz" while a first thread retrieves it from the backend, it is forced to wait for the first thread to finish.
 * Then, it will not retrieve the struct from the backend but from the cache that was filled by the first thread.
 * Once a document is in cache there is no more synchronizing going on. All threads have unsynchronized access to the document cache.
 */
public class WGOperationKey {
	
	private String _string;
	/**
	 * Returns if the operation key is currently in use
	 */
	public boolean isUsed() {
        return _used;
    }


    /**
     * Sets the usage state of the operation key
     * @throws WGIllegalStateException 
     */
    public void setUsed(boolean used) throws WGIllegalStateException {
        
        // Set used
        boolean usedBefore = _used;
        _used = used;
        
        /*// Aquire or release the semaphore lock, depending on used, to prevent the maintenance task from running while the operation is done
        if (_db.isMaintainOperationKeys() && usedBefore != _used) {
            if (used) {
                _db.getWgOperationKeySemaphore().acquireUninterruptibly();
            }
            else {
                _db.getWgOperationKeySemaphore().release();
            }
        }*/        

    }

    /**
	 * Operation type: Retrieve a content by contentkey
	 */
	public static final int OP_DOCUMENT_BY_KEY = 1;
	/**
	 * Operation type: Retrieve a content by it's unique name
	 */
	public static final int OP_CONTENT_BY_NAME = 2;
	/**
	 * Operation type: Retrieve a design document
	 */
	public static final int OP_STRUCT_PARENT = 4;
	/**
	 * Operation type: Retrieve the children of a struct entry
	 */
	public static final int OP_STRUCT_CHILDREN = 5;
	/**
	 * Operation type: Retrieve the root entries of an area
	 */
	public static final int OP_STRUCT_ROOTS = 6;
	
	/**
     * Operation type: Retrieve the contents of a struct entry
     */
    public static final int OP_STRUCT_CONTENTS = 7;

    /**
     * Operation type: Retrieve the list of designs of a specific type
     */
    public static final int OP_DESIGN_LIST = 8;
    
	/**
	 * Operation type: Retrieve a document core (Not used as sync key, just for verbose backend access logging) 
	 */
	public static final int OP_DOCUMENT_CORE  = 9;
    /**
     * Operation type: Retrieve a document core in fast access (Not used as sync key, just for verbose backend access logging)
     */
    public static final int OP_DOCUMENT_CORE_FASTACCESS = 10;
    /**
     * Operation type: Native query to database backend (Not used as sync key, just for verbose backend access logging)
     */
    public static final int OP_QUERY = 11;
    
    /**
     * Operation type: HDB getCreateStorage
     */
    public static final int OP_HDB_CREATE_STORAGE = 12;
    
    /**
     * Operation type: HDB getOrCreateStorage
     */
    public static final int OP_HDB_GET_OR_CREATE_STORAGE = 13;
    
    /**
     * Operation type: Retrieve the contents of a struct entry
     */
    public static final int OP_STRUCT_CONTENTS_INCLUDING_ARCHIVED = 14;
    
    /**
     * Operation type: Save a document
     */
    public static final int OP_SAVE = 15;
    
    /**
     * Operation type: Delete a document
     */
    public static final int OP_DELETE = 16;
    
    /**
     * Operation type: Retrieve the result count of a query 
     */
    public static final int OP_QUERY_RESULT_COUNT = 17;
    
    /**
     * Operation type: Checking for result availability of a query
     */
    public static final int OP_QUERY_HAS_RESULTS = 18;
    
    /**
     * Operation type: Retrieve a struct entry by it's unique name
     */
    public static final int OP_STRUCT_BY_NAME = 19;
    
    /**
     * Operation type: Backend access for design provider
     */
    public static final int OP_DESIGN_BACKEND = 20;
    

    
    
	
	/**
	 * Returns a descriptive name for an operation code
	 * @param op The operation code
	 */
	public static String getOperationName(int op) {
	    
	    switch (op) {
	        
	        case OP_DOCUMENT_BY_KEY:
	            return "Retrieving document by key";
	            
	        case OP_CONTENT_BY_NAME:
	            return "Retrieving content by unique name";
                
	        case OP_STRUCT_PARENT:
	            return "Retrieving parent of struct entry";
	            
            case OP_STRUCT_CHILDREN:
                return "Retrieving children of struct entry";

            case OP_STRUCT_CONTENTS:
                return "Retrieving contents of struct entry";
                
            case OP_STRUCT_ROOTS:
                return "Retrieving root entries of area";

            case OP_DESIGN_LIST:
                return "Retrieving design list";
                
            case OP_DOCUMENT_CORE:
                return "Retrieve document core backend";
                
            case OP_DOCUMENT_CORE_FASTACCESS:
                return "Retrieve document core backend in fast access";

            case OP_QUERY:
                return "Native query";
                
            case OP_STRUCT_CONTENTS_INCLUDING_ARCHIVED:
                return "Retrieving contents of struct entry including archive";
                
            case OP_HDB_CREATE_STORAGE:
                return "Create HDB storage";
                
            case OP_HDB_GET_OR_CREATE_STORAGE:
                return "Get or create HDB storage";
                
            case OP_SAVE:
                return "Save a document";
                
            case OP_DELETE:
                return "Delete a document";
                
            case OP_QUERY_RESULT_COUNT:
                return "Native Query Result Count";
                
            case OP_QUERY_HAS_RESULTS:
                return "Native Query Result Availability Check";
                
            case OP_STRUCT_BY_NAME:
                return "Retrieving struct entry by unique name";

                
            default:
                return "Unknown operation code: " + op;
	    }
	    
	    
	}
		
	private String _key;

	private int _operation;
	
	private volatile boolean _used = false;
    private WGDatabase _db;

	/**
	 * Constructor. Taking operation type and detail key.
	 * @param operation The operation for this key. Use constants WGOperationKey.OP_...
	 * @param key The detail key. This is the key of the document to be fetched, e.g. for Operation OP_CONTENT_BY_KEY the content key
	 */
	public WGOperationKey(WGDatabase db, int operation, String key) {
	    _db = db;
		_operation = operation;
		_key = key;
		_string = createString(operation, key);
	}


	/**
	 * Returns the detail key for this operation, i.e. the special key of the document that should get fetched.
	 */
	public String getKey() {
		return _key;
	}

	/**
	 * The operation type. Uses Constants WGOperationkey.OP_....
	 */
	public int getOperation() {
		return _operation;
	}

	/* (Kein Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object arg0) {
		
		WGOperationKey otherKey = (WGOperationKey) arg0;
		return (otherKey.getOperation() == getOperation() && otherKey.getKey().equals(getKey()));
		
	}

	/* (Kein Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return toString().hashCode();
	}

	/**
	 * Returns the operation key string for this operation.
	 */
	public String toString() {
		return _string;
	}


	/**
	 * Create an operation key string. This one is used to determine equality between operation keys, especially when putting them to a hash map 
	 * @param operation
	 * @param key
	 */
	public static String createString(int operation, String key) {
		return operation + "/" + key;
	}

}
