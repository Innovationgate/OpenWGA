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

import de.innovationgate.webgate.api.locking.Lockable;
import de.innovationgate.wga.config.DesignReference;

/**
 * Superclass for WGAPI "design documents", i.e. documents that are not directly related to authoring and therefor not editable by pure authors/editors.
 * This does not only contain actual design resource documents, those that are regarded to build an apps design, but does also contain schema documents.
 */
public abstract class WGDesignDocument extends WGDocument implements PageHierarchyNode {
    
    public static final String META_NAME = "NAME";
    public static final MetaInfo METAINFO_NAME = new MetaInfo(META_NAME, String.class, null);
    static {
        METAINFO_NAME.setLowerCase(true);
    }
    
    public static final String META_VARIANT = "VARIANT";
    public static final MetaInfo METAINFO_VARIANT = new MetaInfo(META_VARIANT, Boolean.class, new Boolean(false));
    
    public static final String META_DESCRIPTION = "DESCRIPTION";
    public static final MetaInfo METAINFO_DESCRIPTION = new MetaInfo(META_DESCRIPTION, String.class, "");
    static {
        METAINFO_DESCRIPTION.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    public static final String META_DESIGNREFERENCE = "DESIGNREFERENCE";
    public static final MetaInfo METAINFO_DESIGNREFERENCE = new MetaInfo(META_DESIGNREFERENCE, DesignReference.class, null);

	private String retrievalName;
	private int retrievalType;
	private String retrievalMediaKey;
	
	/**
	 * Builds a document key for a design document by the given data.
	 * @param type Type of the design document
	 * @param name Name of the design document
	 * @param mediaKey Media key of the design document, if any
	 */
	public static String buildDesignDocumentKey(int type, String name, String mediaKey) {
		
		return (new WGDocumentKey(type, name, mediaKey)).toString();
	}

	/**
	 * Constructor. Should not be used outside WGAPI.
	 * @param db
	 * @param doc
	 * @throws WGAPIException 
	 */
	public WGDesignDocument(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
		super(db, doc, flags);
		this.retrievalName = this.getName();
		this.retrievalType = this.getType();
		if (getType() == WGDocument.TYPE_TML || getType() == WGDocument.TYPE_CSSJS) {
		    this.retrievalMediaKey = this.getMediaKey();
		}
		
	}
	
	/**
	 * The media key of this design document, if any.
	 * @throws WGAPIException 
	 */
	public String getMediaKey() throws WGAPIException {
		return "";
	}
	
	protected boolean setMediaKey(String key) throws WGAPIException {
		return false;
	}
	
	protected abstract boolean setName(String name) throws WGAPIException;
	
	/**
	 * The name of the design document
	 * @throws WGAPIException 
	 */
	public abstract String getName() throws WGAPIException;
	/**
	 * Returns the description of this design document
	 * @throws WGAPIException
	 */
	public String getDescription() throws WGAPIException {
		return (String) getMetaData(META_DESCRIPTION);
	}
	/**
	 * Sets a description
	 * @param desc
	 * @throws WGAPIException 
	 */
	public boolean setDescription(String desc) throws WGAPIException{
		return setMetaData(META_DESCRIPTION, desc);
	}


	public WGDocumentCore retrieveCore() throws WGAPIException {
		return this.db.getDesignObjectCore(this.retrievalType, this.retrievalName, this.retrievalMediaKey);
	}

	/**
	 * Determines, if the currently logged in user may edit this document, judged by his access level.
	 * @deprecated Use {@link #maySave()}
	 */
	public boolean mayEditDocument() {
		try {
            return maySave();
        }
        catch (WGAPIException e) {
            return false;
        }
	}
	
	/**
	 * Tries to rename the design document.
	 * @param name The new name.
	 * @param mediaKey The new media key.
	 * @throws WGAPIException 
	 */
	public void rename(String name, String mediaKey) throws WGAPIException {
		
		if (!maySave()) {
			throw new WGAuthorisationException("You are not allowed to change the name of this design document", WGAuthorisationException.ERRORCODE_OP_NEEDS_DESIGNER_RIGHTS);
		}
		
		WGDesignDocument otherDoc = db.getDesignObject(getType(), name, mediaKey);
		if (otherDoc != null) {
			throw new WGDuplicateKeyException("There is already a design document with name '" + name + "'" + (mediaKey != null ? " and media key '" + mediaKey + "'" : ""));
		}
		
		setName(name);
		setMediaKey(mediaKey);
		save();
		
	}
	
	/**
	 * Tries to rename the design document.
	 * @param name The new name
	 * @throws WGAPIException 
	 */
	public void rename(String name) throws WGAPIException {
		rename(name, null);
	}
	



	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase, de.innovationgate.webgate.api.WGDocument)
	 */
	public WGDocument createClone(WGDatabase db, WGDocument ref)
		throws WGAPIException {
		
		return createClone(db);
		
	}
	
	/**
	 * Creates a clone of this design document in another database
	 * @param db Target database
	 * @return The clone
 
	 * @throws WGAPIException 
	 */
	public abstract WGDocument createClone(WGDatabase db) throws WGAPIException;


    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#getParentLockable()
     */
    public Lockable getParentLockable() {
        return getDatabase();
    }
    
    /**
     * Identifies this design document as a variant, i.e. a design from a design provider that may return different data for different databases.
     * @throws WGAPIException 
     */
    public boolean isVariant() throws WGAPIException {
        Boolean variant = (Boolean) getMetaData(META_VARIANT);
        if (variant != null) {
            return variant.booleanValue();
        }
        else {
            return false;
        }
    }
    
    /**
     * Returns a design reference object that uniquely identifies the location of this design
     */
    public DesignReference getDesignReference() throws WGAPIException {
        return (DesignReference) getMetaData(META_DESIGNREFERENCE);
    }
    
}

