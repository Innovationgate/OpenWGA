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

import java.util.List;

import de.innovationgate.utils.WGUtils;

/**
 * A class for parsing complete document keys and accessing the single information tokens.
 * 
 * Document keys consist of
 * - a type name declaring the document type, defined by constants WGDatabase.TYPENAME_*
 * - a unique key for the given document type
 * - a media key, mandatory for WebTML modules, empty for all other document types
 * 
 * These parts are concatenated by /-signs to form a complete document key string.
 * 
 * The toString() method of this class returns the rebuilt document key string.
 */
public class WGDocumentKey {
    
    /**
     * Public constructor, taking all information separately
     * @param typename The typename, use constants WGDocument.TYPENAME_*
     * @param name The unique key/name of the document
     * @param mediakey The media key. Should be null when the key does not represent a WebTML module
     */
    public WGDocumentKey(String typename, String name, String mediakey) {
        setTypename(typename);
        setName(name);
        setMediakey(mediakey);
        validate();
    }
    
    /**
     * Public constructor, taking all information separately, using type int
     * @param type The document type, use constants WGDocument.TYPE_*
     * @param name The unique key/name of the document
     * @param mediakey The media key. Should be null when the key does not represent a WebTML module
     */
    public WGDocumentKey(int type, String name, String mediakey) {
        setTypename(WGDocument.doctypeNumberToName(type));
        setName(name);
        setMediakey(mediakey);
        validate();
    }
    
    private void validate() {
        
        // Lowercase names on design document types
        if (isDesignDocumentType()) {
            _name = getName().toLowerCase();
        }
        
    }

    /**
     * Returns if the current document is a design or schema document
     */
    public boolean isDesignDocumentType() {
        return WGDatabase.isDesignDocumentType(getDocType());
    }

    /**
     * Public constructor taking a composite document key string that is parsed for its contents
     * @param key The document key
     */
    public WGDocumentKey(String key) {
        
        if (key == null) {
            throw new IllegalArgumentException("Document key is null");
        }
        
        List parts = WGUtils.deserializeCollection(key, DIVIDER);
        if (parts.size() < 2) {
            throw new IllegalArgumentException("Document keys must have 2 parts minimum, divided by " + DIVIDER);
        }
        
        setTypename((String) parts.get(0));
        
        if (_typename.equals(WGDocument.TYPENAME_TML)) {
            if (parts.size() != 3) {
                throw new IllegalArgumentException("Document keys for tml modules must have 3 parts, divided by " + DIVIDER);
            }
            
            setName((String) parts.get(1));
            setMediakey((String) parts.get(2));
        }
        else if (_typename.equals(WGDocument.TYPENAME_CSSJS)) {
            setName((String) parts.get(1));
            if (parts.size() == 3) {
                setMediakey((String) parts.get(2));
            }
        }
        else {
            // Everything after the typename is the document name, even if it contains more dividers
            String remainingParts = WGUtils.serializeCollection(parts.subList(1, parts.size()), DIVIDER);
            setName(remainingParts);
        }
        
        validate();
        
    }
    
    public static final String DIVIDER = "/";
    private String _typename;
    private String _name;
    private String _originalName;
    private String _mediakey = null;
    private String _qualifier = null;
    /**
     * Returns the media key.
     */
    public String getMediakey() {
        return _mediakey;
    }
    /**
     * Sets the media key
     */
    private void setMediakey(String mediakey) {
        
        if (mediakey != null) {
            if (mediakey.indexOf(DIVIDER) != -1) {
                    throw new IllegalArgumentException("Mediakey may not contain character '" + DIVIDER + "'");
            }
            this._mediakey = mediakey.trim().toLowerCase();
        }
        else {
            this._mediakey = null;
        }
        
        
    }
    /**
     * Returns the unique name/key
     */
    public String getName() {
        return _name;
    }
    /**
     * Sets the unique key/name
     */
    private void setName(String name) {
        
        if (name == null) {
            throw new IllegalArgumentException("Name may not be null");
        }
        
        _name = name.trim();
        _originalName = _name;
    }
    
    /**
     * Returns the type name as constant WGDocument.TYPENAME_*
     */
    public String getTypename() {
        return _typename;
    }
    
    /**
     * Sets the type name. Use constants WGDocument.TYPENAME_*
     */
    private void setTypename(String typename) {
        
        if (typename == null) {
            throw new IllegalArgumentException("Typename may not be null");
        }
        
        int type = WGDocument.doctypeNameToNumber(typename);
        if (type == 0) {
            throw new IllegalArgumentException("No valid typename: " + typename);
        }
        
        this._typename = typename.trim();
    }
    /**
     * Constructs the string representation of this document key
     */
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(_typename).append(DIVIDER).append(_name);
        if (_mediakey != null) {
            buf.append(DIVIDER).append(_mediakey);
        }
        return buf.toString();
    }
    
    /**
     * Returns the numeric document type as constants WGDocument.TYPE_*
     */
    public int getDocType() {
        return WGDocument.doctypeNameToNumber(getTypename());
    }
    
    /**
     * Tries to retrieve the document for the document key from the given database
     * @param db The database
     * @return The document if it was found, else null
     * @throws WGAPIException
     */
    public WGDocument getDocument(WGDatabase db) throws WGAPIException {
        return db.getDocumentByKey(this);
    }
    
    /**
     * Returns if this document key adresses a "real" document.
     * There are some document keys that adress other resources like ACL entries
     * who are no "real" documents.
     */
    public boolean isRealDocumentKey() {
        return !_typename.startsWith("$");
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_mediakey == null) ? 0 : _mediakey.hashCode());
        result = prime * result + ((_name == null) ? 0 : _name.hashCode());
        result = prime * result + ((_qualifier == null) ? 0 : _qualifier.hashCode());
        result = prime * result + ((_typename == null) ? 0 : _typename.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        WGDocumentKey other = (WGDocumentKey) obj;
        if (_mediakey == null) {
            if (other._mediakey != null)
                return false;
        }
        else if (!_mediakey.equals(other._mediakey))
            return false;
        if (_name == null) {
            if (other._name != null)
                return false;
        }
        else if (!_name.equals(other._name))
            return false;
        if (_qualifier == null) {
            if (other._qualifier != null)
                return false;
        }
        else if (!_qualifier.equals(other._qualifier))
            return false;
        if (_typename == null) {
            if (other._typename != null)
                return false;
        }
        else if (!_typename.equals(other._typename))
            return false;
        return true;
    }
    
    protected void useProperCaseNames() {
        _name = _originalName;
    }
    
    /**
     * Creates a new key with an additional qualifier that is just used to distinguish this key via {@link #equals(Object)} and {@link #hashCode()} but is not used within the key itself.
     * This is needed for mapping the session context of temp duplicates of {@link WGDocument}.
     * @param qualifier
     * @return
     */
    public WGDocumentKey withQualifier(String qualifier) {
        WGDocumentKey newKey = new WGDocumentKey(_typename, _name, _mediakey);
        newKey._qualifier = qualifier;
        return newKey;
    }

}
