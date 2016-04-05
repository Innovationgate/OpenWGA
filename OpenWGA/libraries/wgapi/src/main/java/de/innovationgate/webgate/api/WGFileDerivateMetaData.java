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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Data bean for file derivate meta informations. Also provides methods to directly modify extdata fields of the contents object.
 */
public class WGFileDerivateMetaData implements WGExtensionDataContainer, WGFileAnnotations {
    
    public static final String EXTDATA_USAGE = "$usage";
    
    public static final String EXTDATA_ORIGINALREVISION = "$originalrevision";

    private long _size; 
    
    private String _sha512Checksum;
    
    private String _parentSha512Checksum;
   
    private Map<String,Object> _extData = new HashMap<String, Object>();

    private String _creator;

    private String _name;

    /**
     * A derivate Id
     */
    private String _id;

    private Date _created;

    private Date _lastModified;

    private WGDatabase _db;

    private WGDocumentKey _relatedDoc;
	

    /**
     * Returns the size of this file in bytes
     */
    public long getSize() {
		return _size;
	}
	

    /**
     * Returns the SHA-512 checksum of this files data
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

    public void removeExtensionData(String strName)  {
       _extData.remove(strName);
    }

    public void writeExtensionData(String strName, Object value) {
        _extData.put(strName, value);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#getMimeType()
     */
    @Override
    public String getMimeType() {
        return (String) _extData.get(WGFileMetaData.EXTDATA_MIMETYPE);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#setMimeType(java.lang.String)
     */
    @Override
    public void setMimeType(String mimeType) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_MIMETYPE, mimeType);
    }
	
    
	 /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#getDisplayHeight()
     */
	@Override
    public int getDisplayHeight() {
        Number i = (Number) _extData.get(WGFileMetaData.EXTDATA_DISPLAY_HEIGHT);
        if (i == null) {
            i = -1;
        }
        return i.intValue();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#setDisplayHeight(int)
     */
    @Override
    public void setDisplayHeight(int displayHeight) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_DISPLAY_HEIGHT, displayHeight);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#getDisplayWidth()
     */
    @Override
    public int getDisplayWidth() {
        Number i = (Number) _extData.get(WGFileMetaData.EXTDATA_DISPLAY_WIDTH);
        if (i == null) {
            i = -1;
        }
        return i.intValue();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#setDisplayWidth(int)
     */
    @Override
    public void setDisplayWidth(int displayWidth) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_DISPLAY_WIDTH, displayWidth);
    }

    public String getUsage() throws WGAPIException {
        return (String) _extData.get(EXTDATA_USAGE);
    }
    
    /**
     * Sets the usage of this derivate as constant WGFileAnnotations.USAGE_...
     * @param usage The usage
     * @throws WGAPIException
     */
    public void setUsage(String usage) throws WGAPIException {
        _extData.put(EXTDATA_USAGE, usage);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#getCustomFieldNames()
     */
    @Override
    public List<String> getCustomFieldNames() {
        
        List<String> fieldNames = new ArrayList<String>();
        for (String name : _extData.keySet()) {
            if (name.startsWith(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX)) {
                fieldNames.add(name.substring(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX.length()));
            }
        }
        
        return fieldNames;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#getCustomFieldValue(java.lang.String)
     */
    @Override
    public Object getCustomFieldValue(String fieldName) {
        return _extData.get(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX + fieldName);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#setCustomFieldValue(java.lang.String, java.lang.Object)
     */
    @Override
    public void setCustomFieldValue(String fieldName, Object value) throws WGAPIException {
        _extData.put(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX + fieldName, value);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGAnnotatedFile#removeCustomField(java.lang.String)
     */
    @Override
    public void removeCustomField(String fieldName) throws WGAPIException {
        _extData.remove(WGFileMetaData.EXTDATA_CUSTOM_FIELD_PREFIX + fieldName);
    }
    
    protected Map<String, Object> getAllExtensionData() {
        return Collections.unmodifiableMap(_extData);
    }

    /**
     * Returns a hash identifying the derivate creator
     */
    public String getCreator() {
        return _creator;
    }

    public WGFileDerivateMetaData(WGDatabase db, WGDocumentKey relatedDoc, String id, String creator, String name, Date created, Date modified, long size, String sha512Checksum, String parentSha512Checksum) {
        _db = db;
        _id = id;
        _creator = creator;
        _name = name;
        _created = created;
        _lastModified = modified;
        _size = size;
        _sha512Checksum = sha512Checksum;
        _parentSha512Checksum = parentSha512Checksum;
        _relatedDoc = relatedDoc;
    }

    /**
     * Returns the name of this derivate
     */
    public String getName() {
        return _name;
    }

    /**
     * Returns the derivate id
     */
    public String getId() {
        return _id;
    }

    /**
     * Returns the date of creation
     */
    public Date getCreated() {
        return _created;
    }

    /**
     * Returns the date of last modification
     */
    public Date getLastModified() {
        return _lastModified;
    }


    /**
     * Returns the checksum of the original files data  for which this derivate was created
     */
    public String getParentSha512Checksum() {
        return _parentSha512Checksum;
    }
    
    /**
     * Returns the revision of the original file for which this derivate was created
     */
    public WGDatabaseRevision getOriginalRevision() {
        
        Object revisionValue = _extData.get(EXTDATA_ORIGINALREVISION);
        if (revisionValue != null) {
            return WGDatabaseRevision.forValue((Comparable<?>) revisionValue);
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Sets the revision of the original file for which this derivate was created
     */
    public void setOriginalRevision(WGDatabaseRevision rev) {
        _extData.put(EXTDATA_ORIGINALREVISION, rev.getRevisionValue());
    }


    @Override
    public WGDocument getParentDocument() throws WGAPIException {
        return _db.getDocumentByKey(_relatedDoc);
    }


    @Override
    public BinaryFieldData getPlainText() throws WGAPIException {
        return (BinaryFieldData) _extData.get(WGFileMetaData.EXTDATA_PLAINTEXT);   
    }


    @Override
    public void setPlainText(BinaryFieldData data) throws WGAPIException {
        checkDatabaseVersion(5, 5);
        _extData.put(WGFileMetaData.EXTDATA_PLAINTEXT, data);        
    }
    
    private void checkDatabaseVersion(double version, int patchLevel) throws WGNotSupportedException, WGAPIException {

        WGDocument parentDoc = getParentDocument();
        if (parentDoc == null) { // Impossible to check
            return;
        }
        
        if (parentDoc.getDatabase().getContentStoreVersion() < version) {
            throw new WGNotSupportedException("This metadata feature is not supported on content store version " + parentDoc.getDatabase().getContentStoreVersion() + " Patch Level " + parentDoc.getDatabase().getContentStorePatchLevel());
        }
        
        if (parentDoc.getDatabase().getContentStoreVersion() == version) {
            if (parentDoc.getDatabase().getContentStorePatchLevel() < patchLevel) {
                throw new WGNotSupportedException("This metadata feature is not supported on content store version " + parentDoc.getDatabase().getContentStoreVersion() + " Patch Level " + parentDoc.getDatabase().getContentStorePatchLevel());
            }
        }
        
    }
}
