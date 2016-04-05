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
package de.innovationgate.webgate.api.templates;

import java.io.File;
import java.io.InputStream;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGUpdateLog;

/**
 * Represents a "folder" of the SimpleContentSource, which is abstracted as Area by the WGAPI.
 */
public class BeanFolder implements WGDocumentCore {
	
	private String _name;

	private SimpleContentSource _db;

	public BeanFolder(SimpleContentSource db, String folderName) {
		_name = folderName;
		_db = db;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getType()
	 */
	public int getType() {
		return WGDocument.TYPE_AREA;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getFastAccessKey()
	 */
	public Object getFastAccessKey() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isDeleted()
	 */
	public boolean isDeleted() {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isTemporary()
	 */
	public boolean isTemporary() {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#hasItem(java.lang.String)
	 */
	public boolean hasItem(String strName) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getItemValue(java.lang.String)
	 */
	public Object getItemValue(String strName) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getMetaData(java.lang.String)
	 */
	public Object getMetaData(String type) throws WGBackendException {
		
		if (type.equals(WGArea.META_CREATED) || type.equals(WGArea.META_LASTMODIFIED)) {
			return getCreated();
		}
		else if (type.equals(WGArea.META_NAME)) {
			return _name;
		}
		else {
			return null;
		}
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getFileNames()
	 */
	public List getFileNames() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getFileData(java.lang.String)
	 */
	public InputStream getFileData(String strFile) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getFileSize(java.lang.String)
	 */
	public int getFileSize(String strFile) {
		return 0;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getCreated()
	 */
	public Date getCreated() throws WGBackendException {
		return new Date(Long.MIN_VALUE);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getLastModified()
	 */
	public Date getLastModified() throws WGBackendException {
	    return new Date(Long.MIN_VALUE);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#setItemValue(java.lang.String, java.lang.Object)
	 */
	public boolean setItemValue(String strName, Object value) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#setMetaData(java.lang.String, java.lang.Object)
	 */
	public boolean setMetaData(String name, Object value) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#save()
	 */
	public WGDatabaseRevision save(java.util.Date lastModified) throws WGNotSupportedException {
        throw new WGNotSupportedException("Saving a bean folder is not supported");
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#evaluateExpression(java.lang.String)
	 */
	public Object evaluateExpression(String expression) throws WGExpressionException {
		return null;
	}





	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#setWGDocument(de.innovationgate.webgate.api.WGDocument)
	 */
	public void setWGDocument(WGDocument doc) {
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#dispose()
	 */
	public void dispose() {
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getItemNames()
	 */
	public List getItemNames() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#removeItem(java.lang.String)
	 */
	public boolean removeItem(String Name) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isDataCacheable()
	 */
	public boolean isDataCacheable() {
		return true;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#attachFile(java.io.File)
	 */
	public boolean attachFile(File file) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#removeFile(java.lang.String)
	 */
	public boolean removeFile(String name) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#remove()
	 */
	public WGDatabaseRevision remove() {
		return null;
	}

	/**
	 * Returns the name of this folder
	 */
	public String getName() {
		return _name;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isSaved()
	 */
	public boolean isSaved() {
		return true;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getNativeObject()
	 */
	public Object getNativeObject() {
		return null;
	}

    public String getOriginDatabase() {
        return _db.getDb().getDbReference();
    }

	public void renameFile(String oldFileName, String newFileName)
			throws WGAPIException {
		throw new WGNotSupportedException("renameFile() is not supported on this document implementation.");		
	}

	public WGFileMetaData getFileMetaData(String strFile) throws WGAPIException {
		throw new WGNotSupportedException("getFileMetaData() is not supported on this document implementation.");
	}

    public WGDocumentCore getRelation(String name) throws WGAPIException {
        return null;
    }

    public WGDocumentCore removeRelation(String name) throws WGAPIException {
        throw new WGNotSupportedException("Content Relations are not supported");
    }

    public WGDocumentCore setRelation(String name, WGDocumentCore target) throws WGAPIException {
        throw new WGNotSupportedException("Content Relations are not supported");
    }
    
    
    public List<String> getRelationNames() throws WGAPIException {
        return Collections.emptyList();
    }

    public boolean hasFileMetadata() throws WGAPIException {
        return false;
    }

    public boolean hasFile(String file) throws WGBackendException {
        return false;
    }

    public WGRelationData getRelationData(String name) throws WGAPIException {
        return null;
    }

    public WGDocumentCore setRelation(WGRelationData data) throws WGAPIException {
        throw new WGNotSupportedException("Content relations are not supported");
    }

    public Object getExtensionData(String strName) throws WGAPIException {
        return null;
    }

    public List getExtensionDataNames() throws WGAPIException {
        return Collections.emptyList();
    }

    public void removeExtensionData(String strName) throws WGAPIException {
        throw new WGNotSupportedException("Attributes are not supported");
        
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        throw new WGNotSupportedException("Attributes are not supported");
        
    }

    public List<String> getRelationNamesOfGroup(String group, WGColumnSet order) throws WGBackendException {
        return Collections.emptyList();
    }

    public WGExtensionDataContainer retrieveFileExtensionDataHandler(String strFile) throws WGAPIException {
        return null;
    }

    @Override
    public List<WGFileDerivateMetaData> getFileDerivates(String strFile) throws WGAPIException {
        return null;
    }

    @Override
    public void markFileMetaDataModified(WGFileMetaData md) throws WGAPIException {
    }
    
    @Override
    public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String,Object> customMdFields) throws WGAPIException {
        throw new WGNotSupportedException("File derivates are not supported on this database");
    }
    
    @Override
    public InputStream getFileDerivateData(String id) {
        return null;
    }
    
    @Override
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) {
        return null;
    }
    
    @Override
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException {
        throw new WGNotSupportedException("File derivates are not supported on this database");
    }
    
    @Override
    public void removeFileDerivate(String id) throws WGAPIException {
        throw new WGNotSupportedException("File derivates are not supported on this database");   
    }
    
    @Override
    public Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException {
        return Collections.<WGUpdateLog>emptyList().iterator();
    }

}
