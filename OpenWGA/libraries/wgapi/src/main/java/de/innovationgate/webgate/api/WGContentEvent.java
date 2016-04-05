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
 * Represents an event that was triggered for a content document.
 */
public class WGContentEvent {
	
	/**
	 * A content was created
	 */
	public static final int TYPE_CREATED = 1;
	/**
	 * A content is to be saved
	 */
	public static final int TYPE_SAVED = 2;
	
	/**
	 * A content has been saved
	 */
	public static final int TYPE_HASBEENSAVED = 3;
	
	/**
	 * A content has been deleted
	 */
	public static final int TYPE_HASBEENDELETED = 4;

	/**
     * A content has been moved (by a movage of a struct entry)
     */
    public static final int TYPE_HASBEENMOVED = 5;
    
    /**
     * The status of a content has been changed
     */
    public static final int TYPE_STATUSCHANGED = 6;

	
	private String _documentKey;

	private int _type;
    private WGDatabase _database;
    private String _contentType;
    private WGContent _content;


	public WGContentEvent(int type, String documentKey, String contentType, WGDatabase db) {
		_type = type;
		_documentKey = documentKey;
        _contentType = contentType;
        _database = db;
	}


	/**
	 * The document key of the content that triggered the event
	 */
	public String getDocumentKey() {
		return _documentKey;
	}

	/**
	 * The type of the event. A constant of WGContentEvent.TYPE_...
	 */
	public int getType() {
		return _type;
	}


    /**
     * Returns the database of the content document.
     */
    public WGDatabase getDatabase() {
        return _database;
    }


    /**
     * The content type of the content. 
     * This may be null if the change was done in background and the document has not yet been in cache.
     */
    public String getContentType() {
        return _contentType;
    }
    
    /**
     * Retrieves and returns the content document that has changed.
     * This might fail obviously when the content has been deleted.
     * @throws WGAPIException 
     */
    public WGContent getContent() throws WGAPIException {
        
        if (_content != null) {
            return _content;
        }
        else if (getType() != TYPE_CREATED) {
            return (WGContent) getDatabase().getDocumentByDocumentKey(getDocumentKey());
        }
        else {
            return null;
        }
    }
 

    /**
     * @param content The content to set.
     */
    protected void setContent(WGContent content) {
        _content = content;
    }

}
