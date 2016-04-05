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

import java.util.EventObject;

/**
 * Information about a database event.
 */
public class WGDatabaseEvent extends EventObject {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private WGDocumentKey _editedDocumentKey = null;    
		
	private WGDocument _editedDocument = null;
	/**
	 * Database data was updated.
	 */
	public static final int TYPE_UPDATE = 1;
    /**
     * Database was connected
     */
    public static final int TYPE_CONNECTED = 2;
    /**
     * Database was tried to connect but failed
     */
    public static final int TYPE_CONNECTION_ERROR = 3;
	private int _type;
	
	/**
	 * Constructor. Should not be used outside WGAPI.
	 * @param source
	 * @param iType
	 * @param editedDocument
	 */
	public WGDatabaseEvent(WGDatabase source, int iType, WGDocument editedDocument) {
		super(source);
		_type = iType;
		_editedDocument = editedDocument;
	}
	
	/**
     * Constructor. Should not be used outside WGAPI.
     * @param source
     * @param iType
     * @param editedDocumentKey
     */
	public WGDatabaseEvent(WGDatabase source, int iType, WGDocumentKey editedDocumentKey) {
	    super(source);
        _type = iType;
	    _editedDocumentKey = editedDocumentKey;
	}
	
	/**
     * Constructor. Should not be used outside WGAPI.
     * @param source
     * @param iType
     */
    public WGDatabaseEvent(WGDatabase source, int iType) {
        super(source);
        _type = iType;
    }
	
	/**
	 * The database, that triggered the event.
	 */
	public WGDatabase getDatabase() {
		return (WGDatabase) getSource();
	}

	/**
	 * The type of event. A constant WGDatabaseEvent.TYPE_...
	 */
	public int getType() {
		return _type;
	}


	/**
	 * The document of the database that was edited and triggered the event.
	 * Can be null if the event was not triggered by a document update.
	 */
	public WGDocument getEditedDocument() {
		return _editedDocument;
	}

    /**
     * Returns the document key of the edited document
     */
    public WGDocumentKey getEditedDocumentKey() {
        if (_editedDocumentKey != null) {
            return _editedDocumentKey;
        }
        else if (_editedDocument != null) {
            return _editedDocument.getDocumentKeyObj();
        }
        else {
            return null;
        }
    }

}

