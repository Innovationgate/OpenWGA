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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
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
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGUpdateLog;

/**
 * The wrapped data key for a data object. This will serve WGAPI as a struct entry. The struct entry key is the BeanKey obejct.
 */
public class WrappedKey implements WGDocumentCore {
	

	private boolean _saved;
	private Object _bean;
	private SimpleContentSource _db;

	/**
	 * Constructor not to be used outside WGAPI.
	 * @param db
	 * @param key
	 * @param bean
	 * @param saved
	 */
	public WrappedKey(SimpleContentSource db, BeanKey key, Object bean, boolean saved) {
		_db = db;
		_key = key;
		_bean = bean;
		_saved = saved;
	}
	
	public BeanKey _key;

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getType()
	 */
	public int getType() {
		return WGDocument.TYPE_STRUCTENTRY;
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
		return (_key.getKey() == null || _key.getKey() instanceof TemporaryKey);
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
	public Object getMetaData(String type) {

		if (type.equals(WGStructEntry.META_AREA)) {
			return _key.getFolder();
		}
		else if (type.equals(WGStructEntry.META_CHILD_EDITORS) || type.equals(WGStructEntry.META_PAGE_EDITORS)) {
			return new ArrayList();
		}
		else if (type.equals(WGStructEntry.META_CONTENTTYPE)) {
			return null;
		}
		else if (type.equals(WGStructEntry.META_CREATED)) {
			return new Date();
		}
		else if (type.equals(WGStructEntry.META_KEY)) {
			return _key;
		}
		else if (type.equals(WGStructEntry.META_LASTMODIFIED)) {
			return new Date();
		}
		else if (type.equals(WGStructEntry.META_POSITION)) {
			return new Integer(0);
		}
		else if (type.equals(WGStructEntry.META_TITLE)) {
			return _key.toString();
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
	public Date getCreated() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getLastModified()
	 */
	public Date getLastModified() {
		return new Date();
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
	public WGDatabaseRevision save(java.util.Date lastModified) throws WGAuthorisationException {
		_saved = true;
		return null;
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

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#isSaved()
	 */
	public boolean isSaved() {
		return _saved;
	}

	/**
	 * Returns the bean that is stored under this key.
	 * @throws WGAPIException 
	 */
	public Object getBean() throws WGAPIException {
		if (_bean == null) {
			_bean = _db.getContent(_key.getFolder(), _key.getKey());
		} 
		return _bean;
	}



	/**
	 * Returns the data key
	 */
	public BeanKey getKey() {
		return _key;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocumentCore#getNativeObject()
	 */
	public Object getNativeObject() {
		return _key;
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
        throw new WGNotSupportedException("Attributes are not supported");    }

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
