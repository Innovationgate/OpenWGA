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

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Represents the complete unique key of a content document, consisting of the struct key of the connected struct entry, the language code and the content version.
 */
public class WGContentKey {

	public static final String TOKEN_DIVIDER = ".";
	Object structKey;
	String strLanguage;
	int iVersion;

	/**
	 * Constructor for content key object. Takes all parts of it as parameter.
	 * @param structKey The struct key
	 * @param strLanguage The language name
	 * @param iVersion The content version
	 */
	public WGContentKey(Object structKey, String strLanguage, int iVersion) {
		this.structKey = structKey;
		if (this.structKey == null) {
			this.structKey = "null";
		}
		this.strLanguage = strLanguage.toLowerCase();
		this.iVersion = iVersion;
	}
    
    /**
     * Tests, if this is structurally a valid content key
     */
    public boolean isValid() {
        return (structKey != null && strLanguage != null);
    }

	
	/**
	 * Parses a content key string and returns the according content key object. 
	 * The database, whose content is meant needs to be provided to parse the struct key part.
     * If the database is ommitted the struct key will be left in it's string representation..
	 * An additional parameter determines, if the unique mode key or the URL mode key is to be created.
	 * @param key Content key string representation
	 * @param db Content database, whose content is adressed by the content key. Omit to keep struct key in its string representation.
	 * @param unique When true, creates unique content key (always use real version), else creates url mode key (if content is released, 0 is taken for version)
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public static WGContentKey parse(String key, WGDatabase db, boolean unique) throws WGAPIException {
		
		java.util.StringTokenizer tokens = new java.util.StringTokenizer(key, WGContentKey.TOKEN_DIVIDER);
		if (tokens.countTokens() != 3) {
			return null;
		}
		

        Object structKey = tokens.nextToken();
        if (db != null) {
             structKey = db.parseStructKey((String) structKey);
        }
        
		String strLanguage = tokens.nextToken();
		String strVersion = tokens.nextToken();
		
		int version = 0;
		try {
		    version = Integer.parseInt(strVersion);
		}
		catch (NumberFormatException e) {
		    // Everything ok. All non-numeric versions (p, mediakey) will get parsed as version 0
		}
		

		return new WGContentKey(structKey, strLanguage, version);
				
	}
	
	/**
	 * Parses a content key string and returns the according unique content key object. 
	 * The database that the content key belongs to needs to be provided to parse the struct key part to its native data format
     * If the database is ommitted the struct key will be left in it's string representation..
	 * @param key Content key string representation
	 * @param db Content database, whose content is adressed by the content key. Omit to keep struct key in its string representation.
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public static WGContentKey parse(String key, WGDatabase db) throws WGAPIException {
		return WGContentKey.parse(key, db, false);
	}
	
	/**
	 * Creates a new content key object, representing the content key of the parameter content. 
	 * An additional parameter determines, if the unique mode key or the URL mode key is to be created.
	 * @param content The content, whose content key should be built
	 * @param unique When true, creates unique content key (always use real version), else creates url mode key (if content is released, 0 is taken for version)
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public static WGContentKey create(WGContent content, boolean unique) throws WGAPIException {
		
		if (unique == false && content.getStatus().equals(WGContent.STATUS_RELEASE)) {
			return new WGContentKey(content.getStructKey(), (String) content.getMetaData(WGContent.META_LANGUAGE), 0);
		}
		else {
			return new WGContentKey(content.getStructKey(), (String) content.getMetaData(WGContent.META_LANGUAGE), content.getVersion());
		}

	}
	
	/**
	 * Creates a new content key object, representing the unique content key of the parameter content.
	 * @param content The content, whose content key should be built
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public static WGContentKey create(WGContent content) throws WGAPIException {
		return WGContentKey.create(content, true);
	}
	
	/**
	 * Creates a new content key object, representing the content key of the parameter content core. 
	 * An additional parameter determines, if the unique mode key or the URL mode key is to be created.
	 * @param doc The content core, whose content key should be built
	 * @param unique When true, creates unique content key (always use real version), else creates url mode key (if content is released, 0 is taken for version)
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public static WGContentKey create(WGDocumentCore doc, boolean unique) throws WGAPIException {
		
		if (doc.getType() != WGDocument.TYPE_CONTENT) {
			return null;
		}
		else if (unique == false && doc.getMetaData(WGContent.META_STATUS).equals(WGContent.STATUS_RELEASE)) {
			return new WGContentKey(doc.getMetaData(WGContent.META_STRUCTENTRY), String.valueOf(doc.getMetaData(WGContent.META_LANGUAGE)), 0);
		}
		else {
			return new WGContentKey(doc.getMetaData(WGContent.META_STRUCTENTRY), String.valueOf(doc.getMetaData(WGContent.META_LANGUAGE)), ((Number) doc.getMetaData(WGContent.META_VERSION)).intValue());
		}

	} 
	
	/**
	 * Creates a new content key object, representing the unique content key of the parameter content core. 
	 * @param doc content document core, whose content key should be built.
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public static WGContentKey create(WGDocumentCore doc) throws WGAPIException {
		return WGContentKey.create(doc, false);
	}

	/**
	 * Returns the version number part of the key.
	 * @return int
	 */
	public int getVersion() {
		return this.iVersion;
	}

	/**
	 * Returns the language name part of this content key.
	 * @return Returns a String
	 */
	public String getLanguage() {
		return strLanguage;
	}
	
	/**
	 * Returns the struct key part of this key.
	 * @return Object
	 */
	public Object getStructKey() {
		return this.structKey;
	}
	/**
	 * Returns a string representation for this content key, as used in URLs and parsed by the parse methods on this object.
	 */
	public String toString() {
		return String.valueOf(this.getStructKey()) + TOKEN_DIVIDER + this.getLanguage() + TOKEN_DIVIDER + this.getVersion();
	}

	/* (Kein Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object arg0) {
		
		if (!(arg0 instanceof WGContentKey)) {
			return false;
		}
		
		WGContentKey other = (WGContentKey) arg0;
		return (getStructKey().equals(other.getStructKey()) && getLanguage().equals(other.getLanguage()) && getVersion() == other.getVersion());
		
	}


    /* (non-Javadoc)
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return new HashCodeBuilder()
        .append(getStructKey())
        .append(getLanguage())
        .append(getVersion())
        .toHashCode();
    }
    
    /**
     * Creates a document key pendant for this content key
     */
    public WGDocumentKey toDocumentKey() {
        return new WGDocumentKey(WGDocument.TYPE_CONTENT, toString(), null);
    }

}
