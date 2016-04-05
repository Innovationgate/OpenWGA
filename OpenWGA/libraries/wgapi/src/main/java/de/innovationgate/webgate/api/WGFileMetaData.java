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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Data bean for file meta informations, available since content store version 4.1.
 * All data in regular fields is immutable. Only cached extension data fields on field {@link #_extData} are mutable and stored back.
 */
public class WGFileMetaData implements WGExtensionDataContainer, WGFileAnnotations, MetaInfoProvider {
    
    /**
     * A file derivate that was created manually
     */
    public static class ManualDerivate {
        
        private String _name;
        private String _usage;
        private transient WGFileMetaData _parent;
        
        /**
         * Constructor. Not for use outside the WGAPI
         * @param name
         * @param usage
         */
        public ManualDerivate(String name, String usage) {
            _name = name;
            _usage = usage;
       }

        /**
         * Returns the name of the derivate
         */
        public String getName() {
            return _name;
        }

        /**
         * Returns the usage of the derivate
         */
        public String getUsage() {
            return _usage;
        }

        /**
         * Returns the {@link WGFileMetaData} object to which this derivate belongs
         */
        protected WGFileMetaData getParent() {
            return _parent;
        }

        /**
         * Sets the {@link WGFileMetaData} object to which this derivate belongs
         */
        protected void setParent(WGFileMetaData parent) {
            _parent = parent;
        }
        
        /**
         * Returns the data of the derivate
         */
        public BinaryFieldData getData() {
            return (BinaryFieldData) _parent.getExtensionData(EXTDATA_MANUAL_DERIVATE_DATA_PREFIX + _name);
        }

    }

    public static final String EXTDATA_MIMETYPE = "$mimetype";
    
    public static final String EXTDATA_DISPLAY_WIDTH = "$displaywidth";
    
    public static final String EXTDATA_DISPLAY_HEIGHT = "$displayheight";
    
    public static final String EXTDATA_TITLE = "$title";
    
    public static final String EXTDATA_DESCRIPTION = "$description";
    
    public static final String EXTDATA_COPYRIGHT = "$copyright";
    
    public static final String EXTDATA_CUSTOM_FIELD_PREFIX = "$custom_";
    public static final String EXTDATA_MANUAL_DERIVATE_DEFINITION_PREFIX = "$derivate_def_";
    public static final String EXTDATA_MANUAL_DERIVATE_DATA_PREFIX = "$derivate_data_";
    
    public static final String EXTDATA_UPDATE_REVISION = "$updaterevision";
    
    public static final String EXTDATA_PLAINTEXT = "$plaintext";

	private String _name;
	
	private long _size;	
	
	private Date _created;
	
	private Date _lastmodified;

	private String _md5Checksum;
	
    private String _sha512Checksum = null;
    
    private Map<String,Object> _extData = new HashMap<String, Object>();
    
    private WGFileMetaDataContext _context;
    
    public static final String META_NAME = "FILE_NAME";
    public static final MetaInfo METAINFO_NAME = new MetaInfo(META_NAME, String.class, null);
    static { 
        METAINFO_NAME.setLuceneAddToAllContent(true);
        METAINFO_NAME.setLuceneBoost((float)1.5); 
        METAINFO_NAME.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_KEYWORD);
    }
    
    public static final String META_TITLE = "FILE_TITLE";
    public static final MetaInfo METAINFO_TITLE = new MetaInfo(META_TITLE, String.class, null);
    static { 
        METAINFO_TITLE.setLuceneAddToAllContent(true);
        METAINFO_TITLE.setLuceneBoost(2); 
        METAINFO_TITLE.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
    }
    
    public static final String META_DESCRIPTION = "FILE_DESCRIPTION";
    public static final MetaInfo METAINFO_DESCRIPTION = new MetaInfo(META_DESCRIPTION, String.class, null);
    static { 
        METAINFO_DESCRIPTION.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
        METAINFO_DESCRIPTION.setLuceneBoost((float)1.5);
        METAINFO_DESCRIPTION.setLuceneAddToAllContent(true);
    };
    
    public static final String META_COPYRIGHT = "FILE_COPYRIGHT";
    public static final MetaInfo METAINFO_COPYRIGHT = new MetaInfo(META_COPYRIGHT, String.class, null);
    
    public static final String META_CREATED = "FILE_CREATED";
    public static final MetaInfo METAINFO_CREATED = new MetaInfo(META_CREATED, Date.class, new Date(Long.MIN_VALUE));
    
    public static final String META_LASTMODIFIED = "FILE_LASTMODIFIED";
    public static final MetaInfo METAINFO_LASTMODIFIED = new MetaInfo(META_LASTMODIFIED, Date.class, new Date(Long.MIN_VALUE));
    
    public static final String META_SIZE = "FILE_SIZE";
    public static final MetaInfo METAINFO_SIZE = new MetaInfo(META_SIZE, Long.class, new Long(0));
    
    public static final String META_MD5CHECKSUM = "FILE_MD5CHECKSUM";
    public static final MetaInfo METAINFO_MD5CHECKSUM = new MetaInfo(META_MD5CHECKSUM, String.class, null);
    
    public static final String META_SHA512CHECKSUM = "FILE_SHA512CHECKSUM";
    public static final MetaInfo METAINFO_SHA512CHECKSUM = new MetaInfo(META_SHA512CHECKSUM, String.class, null);
    
    public static final String META_MIMETYPE = "FILE_MIMETYPE";
    public static final MetaInfo METAINFO_MIMETYPE = new MetaInfo(META_MIMETYPE, String.class, null);
    
    
    public WGFileMetaData(WGFileMetaDataContext context, String name, long size, Date created, Date lastModified, String md5Checksum, String sha512Checksum, Map<String,Object> extData) {
        _context = context;
        _name = name;
        _size = size;
        _created = created;
        _lastmodified = lastModified;
        _md5Checksum = md5Checksum;
        _sha512Checksum = sha512Checksum;
        _extData = extData;
    }

	/**
	 * returns the name of the file
	 */
	public String getName() {
		return _name;
	}

	/**
	 * returns the file size in bytes
	 */
	public long getSize() {
		return _size;
	}
	
	/**
	 * returns the date the file was attached on the document
	 */
	public Date getCreated() {
		return _created;
	}
	
	/**
	 * returns the date the file was last modified for e.g. renamed
	 */
	public Date getLastmodified() {
		if (_lastmodified != null) {
		    return _lastmodified;
		}
		else {
		    return _created;
		}
	}
	
	/**
	 * returns an md5 checksum of the file binary data
	 */
	public String getMd5Checksum() {
		return _md5Checksum;
	}
	
	/**
     * returns an SHA-512 checksum of the file binary data, available only since content store version 5 patch level 4
     */
    public String getSha512Checksum() {
        return _sha512Checksum;
    }

	public Object getExtensionData(String strName) {
	    return _extData.get(strName);
    }

    public List<String> getExtensionDataNames() throws WGAPIException {
        return new ArrayList<String>(_extData.keySet());
    }

    public void removeExtensionData(String strName) throws WGAPIException  {
       _extData.remove(strName);
       _context.markMetaDataModified(this);
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        if (!(value instanceof BinaryFieldData) && BinaryFieldData.isBinaryValue(value)) {
            try {
                value = new BinaryFieldData(value);
            }
            catch (IOException e) {
                throw new WGBackendException("Exception writing binary extension data", e);
            }
        }
        _extData.put(strName, value);
        _context.markMetaDataModified(this);
    }
    
    
    public List<String> getCustomFieldNames() {
        
        List<String> fieldNames = new ArrayList<String>();
        for (String name : _extData.keySet()) {
            if (name.startsWith(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX)) {
                fieldNames.add(name.substring(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX.length()));
            }
        }
        
        return fieldNames;
    }
    
    public Object getCustomFieldValue(String fieldName) {
        return _extData.get(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX + fieldName);
    }
    
    public void setCustomFieldValue(String fieldName, Object value) throws WGAPIException {
        checkDatabaseVersion(5, 0);
        _extData.put(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX + fieldName, value);
        _context.markMetaDataModified(this);
    }
    
    private void checkDatabaseVersion(double version, int patchLevel) throws WGNotSupportedException, WGAPIException {

        WGDocument parentDoc = _context.getFileParent();
        if (parentDoc == null) { // Impossible to check
            return;
        }
        
        if (parentDoc.getDatabase().getContentStoreVersion() < version) {
            throw new WGNotSupportedException("This file metadata feature is not supported on content store version " + parentDoc.getDatabase().getContentStoreVersion() + " Patch Level " + parentDoc.getDatabase().getContentStorePatchLevel());
        }
        
        if (parentDoc.getDatabase().getContentStoreVersion() == version) {
            if (parentDoc.getDatabase().getContentStorePatchLevel() < patchLevel) {
                throw new WGNotSupportedException("This file metadata feature is not supported on content store version " + parentDoc.getDatabase().getContentStoreVersion() + " Patch Level " + parentDoc.getDatabase().getContentStorePatchLevel());
            }
        }
        
    }

    public void removeCustomField(String fieldName) throws WGAPIException {
        checkDatabaseVersion(5, 0);
        _extData.remove(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX + fieldName);
        _context.markMetaDataModified(this);
    }
	
    
    /**
     * Returns the file metadata context
     */
    public WGFileMetaDataContext getContext() {
        return _context;
    }

    /**
     * Sets the file metadata context
     * @param parent The context
     */
    public void setContext(WGFileMetaDataContext parent) {
        _context = parent;
    }

    /**
     * Creates an identical clone of this metadata object
     * @throws WGAPIException 
     */
    public WGFileMetaData createClone() throws WGAPIException {
        WGFileMetaData clone = new WGFileMetaData(getContext(), getName(), getSize(), getCreated(), getLastmodified(), getMd5Checksum(), getSha512Checksum(), new HashMap<String,Object>(_extData));
        return clone;
    }
    

    
    /**
     * Pushes all data from this file metadata object to the parameter object
     * @param clone The target object
     * @throws WGAPIException
     */
    public void pushData(WGFileMetaData clone) throws WGAPIException {
        clone._extData = new HashMap<String,Object>(_extData);
        clone._context.markMetaDataModified(clone);
    }

    public String getMimeType() {
        return (String) _extData.get(WGFileMetaData.EXTDATA_MIMETYPE);
    }

    public void setMimeType(String mimeType) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_MIMETYPE, mimeType);
        _context.markMetaDataModified(this);
    }

    public int getDisplayHeight() {
        Number i = (Number) _extData.get(WGFileMetaData.EXTDATA_DISPLAY_HEIGHT);
        if (i == null) {
            i = -1;
        }
        return i.intValue();
    }

    public void setDisplayHeight(int displayHeight) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_DISPLAY_HEIGHT, displayHeight);
        _context.markMetaDataModified(this);
    }

    public int getDisplayWidth() {
        Number i = (Number) _extData.get(WGFileMetaData.EXTDATA_DISPLAY_WIDTH);
        if (i == null) {
            i = -1;
        }
        return i.intValue();
    }

    public void setDisplayWidth(int displayWidth) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_DISPLAY_WIDTH, displayWidth);
        _context.markMetaDataModified(this);
    }
    
    /**
     * Returns the title of this file
     */
    public String getTitle() {
        return (String) _extData.get(EXTDATA_TITLE);
    }
    
    /**
     * Sets the title of this file
     * @param title The title
     */
    public void setTitle(String title) throws WGAPIException {
        _extData.put(EXTDATA_TITLE, title);
        _context.markMetaDataModified(this);
    }

    /**
     * Returns the description of this image
     */
    public String getDescription() {
        return (String) _extData.get(EXTDATA_DESCRIPTION);
    }
    
    /**
     * Sets the description of this image
     * @param desc The description
     * @throws WGAPIException
     */
    public void setDescription(String desc) throws WGAPIException {
        _extData.put(EXTDATA_DESCRIPTION, desc);
        _context.markMetaDataModified(this);
    }
    
    /**
     * Returns the copyright of this image
     */
    public String getCopyright() {
        return (String) _extData.get(EXTDATA_COPYRIGHT);
    }
    
    /**
     * Sets the copyright for this image
     * @param copyright The copyright
     * @throws WGAPIException
     */
    public void setCopyright(String copyright) throws WGAPIException {
        _extData.put(EXTDATA_COPYRIGHT, copyright);
        _context.markMetaDataModified(this);
    }




    protected Map<String, Object> getAllExtensionData() {
        return Collections.unmodifiableMap(_extData);
    }

    @Override
    public String getUsage() throws WGAPIException {
        String mimeType = getMimeType();
        if (mimeType != null && mimeType.startsWith("image/")) {
            return USAGE_POSTER;
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns the revision when the contents of this file was updated the last time
     * @throws WGAPIException
     */
    public WGDatabaseRevision getUpdateRevision() throws WGAPIException {
        Comparable<?> revisionValue = (Comparable<?>) _extData.get(EXTDATA_UPDATE_REVISION);
        if (revisionValue != null) {
            return WGDatabaseRevision.forValue(revisionValue);
        }
        else {
            return null;
        }
    }
    
    @Override
    public BinaryFieldData getPlainText() throws WGAPIException {
        return (BinaryFieldData) getExtensionData(EXTDATA_PLAINTEXT);
    }

    
    @Override
    public void setPlainText(BinaryFieldData plaintext) throws WGAPIException {        
        checkDatabaseVersion(5, 5);        
        _extData.put(EXTDATA_PLAINTEXT, plaintext);
        _context.markMetaDataModified(this);
    }
    
    /**
     * Returns a file metadata value by its field name
     * @param name The field name
     * @return The value
     */
    public Object getMetaData(String name) {
        if (META_TITLE.equals(name)) {
            return getTitle();
        } else if (META_DESCRIPTION.equals(name)) {
            return getDescription();
        } else if (META_NAME.equals(name)) {
            return getName();
        } else if (META_COPYRIGHT.equals(name)) {
            return getCopyright();
        } else if (META_CREATED.equals(name)) {
            return getCreated();
        } else if (META_LASTMODIFIED.equals(name)) {
            return getLastmodified();
        } else if (META_MD5CHECKSUM.equals(name)) {
            return getMd5Checksum();
        } else if (META_SHA512CHECKSUM.equals(name)) {
            return getSha512Checksum();
        } else if (META_SIZE.equals(name)) {
            return getSize();
        } else if (META_MIMETYPE.equals(name)) {
            return getMimeType();
        }
        return null;
    }
    

    

    
    public WGDocument getParentDocument() throws WGAPIException {
        return _context.getFileParent();
    }
  
    
    /**
     * Returns the names of all manual derivates registered for this file
     * @throws WGAPIException
     */
    public List<String> getManualDerivateNames() throws WGAPIException {
        
        List<String> names = new ArrayList<String>();
        for (String name : getAllExtensionData().keySet()) {
            if (name.startsWith(EXTDATA_MANUAL_DERIVATE_DEFINITION_PREFIX)) {
                names.add(name.substring(EXTDATA_MANUAL_DERIVATE_DEFINITION_PREFIX.length()));
            }
        }
        return names;
        
    }
    
    /**
     * Retrieves a manual derivate defined for this file
     * @param name Name of the derivate
     * @throws WGAPIException
     */
    public ManualDerivate getManualDerivate(String name) throws WGAPIException {
        ManualDerivate derivate = (ManualDerivate) getExtensionData(EXTDATA_MANUAL_DERIVATE_DEFINITION_PREFIX + name);
        if (derivate != null) {
            derivate.setParent(this);
            return derivate;
        }
        else {
            return null;
        }
    }
    
    /**
     * Defines a manual derivate for this file
     * @param name Name of the derivate
     * @param usage Usage of the derivate. Use WGFileAnnotations.USAGE_* constants.
     * @param data The data for the derivate
     * @throws WGAPIException
     */
    public void setManualDerivate(String name, String usage, InputStream data) throws WGAPIException {
        checkDatabaseVersion(5, 5);
        ManualDerivate manualDerivate = new ManualDerivate(name, usage);
        writeExtensionData(EXTDATA_MANUAL_DERIVATE_DEFINITION_PREFIX + name, manualDerivate);
        writeExtensionData(EXTDATA_MANUAL_DERIVATE_DATA_PREFIX + name, data);
    }
    
    /**
     * Removes a manual derivate
     * @param name Name of the derivate
     * @throws WGAPIException
     */
    public void removeManualDerivate(String name) throws WGAPIException {
        checkDatabaseVersion(5, 5);
        removeExtensionData(EXTDATA_MANUAL_DERIVATE_DATA_PREFIX + name);
        removeExtensionData(EXTDATA_MANUAL_DERIVATE_DEFINITION_PREFIX + name);
    }
    

    
     
}
