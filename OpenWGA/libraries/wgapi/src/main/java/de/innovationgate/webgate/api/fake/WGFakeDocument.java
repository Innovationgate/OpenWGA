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
package de.innovationgate.webgate.api.fake;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGExtensionDataContainer;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUpdateLog;

public class WGFakeDocument implements WGDocumentCore {
    
    public static boolean isFake(WGDocumentCore doc) {
        
        return (
                doc.getClass() == WGFakeDocument.class ||
                doc.getClass() == WGFakeArea.class ||
                doc.getClass() == WGFakeStructEntry.class ||
                doc.getClass() == WGFakeLanguage.class ||                doc.getClass() == WGFakeContentType.class ||
                doc.getClass() == WGFakeTML.class ||
                doc.getClass() == WGFakeUserProfile.class);
        
    }
	
	private WGDatabase db;
	private int type;
    private boolean _deleted = false;
    private boolean _cacheable = false;
    public WGFakeDocument(WGDatabase db, int type) {
		this.db = db;
		this.type = type;
	}
	
	public int getType() {
		return this.type;
	}

	public Object getFastAccessKey() throws WGBackendException {
		return null;
	}

	public boolean isDeleted() {
		return _deleted;
	}
	
	public void setDeleted(boolean deleted) {
	    _deleted = deleted;
	}

	public boolean isTemporary() {
		
		if (this.type == WGDocument.TYPE_LANGUAGE) {
			return false;
		}
		else {
			return true;
		}
	}


	public boolean hasItem(String strName) throws WGBackendException {
		return false;
	}


	public Object getItemValue(String strName) throws WGBackendException {
		return "";
	}

	public boolean setItemValue(String strName, Object value) throws WGBackendException {
		return false;
	}


	public Object getMetaData(String name) throws WGSystemException, WGIllegalArgumentException, WGBackendException {

		return getMetaDataDefault(getType(), name, WGFakeDatabase.getFakeLanguage(db.getCreationOptions()));

	}
	
	public static Object getMetaDataDefault(int type, String name, String fakeLanguage) throws WGSystemException, WGIllegalArgumentException {
		switch (type) {

			case WGDocument.TYPE_CONTENT :
				if (name.equals(WGContent.META_STRUCTENTRY)) {
					return WGFakeDatabase.FAKE_STRUCTKEY;
				}
				else if (name.equals(WGContent.META_LANGUAGE)) {
					return fakeLanguage;
				}
				else if (name.equals(WGContent.META_STATUS)) {
					return de.innovationgate.webgate.api.WGContent.STATUS_RELEASE;
				}
				else if (name.equals(WGContent.META_VERSION)) {
					return new Integer(1);
				}
				else if (name.equals(WGContent.META_TITLE)) {
					return WGFakeDatabase.FAKE_TITLE;
				}
				else if (name.equals(WGContent.META_UNIQUE_NAME)) {
					return null;
				}
				else if (name.equals(WGContent.META_VISIBLE)) {
					return new Boolean(true);
				}
				else if (name.equals(WGContent.META_IS_HIDDEN_FROM)) {
					return new ArrayList();
				}
				else if (name.equals(WGContent.META_VALID_FROM)) {
					return null;
				}
				else if (name.equals(WGContent.META_VALID_TO)) {
					return null;
				}
				else if (name.equals(WGContent.META_CONTENTCLASS)) {
					return "";
				}
				else if (name.equals(WGContent.META_VIRTUAL_LINK)) {
					return "";
				}
				else if (name.equals(WGContent.META_LINK_TARGET)) {
					return "";
				}
				else if (name.equals(WGContent.META_KEYWORDS)) {
					return new ArrayList();
				}
				break;

			case WGDocument.TYPE_AREA :
				if (name.equals(WGArea.META_NAME)) {
					return WGFakeDatabase.FAKE_AREA;
				}
				break;

			case WGDocument.TYPE_CONTENTTYPE :
				if (name.equals(WGContentType.META_NAME)) {
					return WGFakeDatabase.FAKE_CONTENTTYPE;
				}
				else if (name.equals(WGContentType.META_OUTER_LAYOUT)) {
					return WGFakeDatabase.FAKE_TML;
				}
				else if (name.equals(WGContentType.META_INNER_LAYOUT)) {
					return WGFakeDatabase.FAKE_TML;
				}
				break;

			case WGDocument.TYPE_FILECONTAINER :
				if (name.equals(WGFileContainer.META_NAME)) {
					return WGFakeDatabase.FAKE_TITLE;
				}
				break;

			case WGDocument.TYPE_LANGUAGE :
				if (name.equals(WGLanguage.META_NAME)) {
					return fakeLanguage;
				}
				else if (name.equals(WGLanguage.META_TITLE)) {
					return fakeLanguage;
				}
				break;

			case WGDocument.TYPE_STRUCTENTRY :
				if (name.equals(WGStructEntry.META_KEY)) {
					return WGFakeDatabase.FAKE_STRUCTKEY;
				}
				else if (name.equals(WGStructEntry.META_AREA)) {
					return WGFakeDatabase.FAKE_AREA;
				}
				else if (name.equals(WGStructEntry.META_CONTENTTYPE)) {
					return WGFakeDatabase.FAKE_CONTENTTYPE;
				}
				else if (name.equals(WGStructEntry.META_POSITION)) {
					return new Double(0);
				}
				else if (name.equals(WGStructEntry.META_TITLE)) {
					return WGFakeDatabase.FAKE_TITLE;
				}
				break;

			case WGDocument.TYPE_TML :
				if (name.equals(WGTMLModule.META_NAME)) {
					return WGFakeDatabase.FAKE_TML;
				}
				else if (name.equals(WGTMLModule.META_CODE)) {
					return "";
				}
				else if (name.equals(WGTMLModule.META_MEDIAKEY)) {
					return WGDatabase.DEFAULT_MEDIAKEY;
				}
				else if (name.equals(WGTMLModule.META_CACHEABLE)) {
				    return new Boolean(false);
				}
				else if (name.equals(WGTMLModule.META_DIRECTACCESS)) {
				    return new Boolean(false);
				}
				break;

			case WGDocument.TYPE_CSSJS :
				if (name.equals(WGCSSJSModule.META_NAME)) {
					return WGFakeDatabase.FAKE_TITLE;
				}
				else if (name.equals(WGCSSJSModule.META_CODE)) {
					return "";
				}
				else if (name.equals(WGCSSJSModule.META_CODETYPE)) {
					return "text/css";
				}
				break;
		}

		if (name.equals(WGDocument.META_CREATED)) {
			return new Date();
		}
		else if (name.equals(WGDocument.META_LASTMODIFIED)) {
			return new Date();
		}
		else {
			if (WGDocument.isListMeta(type, name)) {
			    return new ArrayList();
			}
			else {
			    return null;
			}
		}

	} 


	public List getFileNames() throws WGBackendException {
		return new ArrayList();
	}

	public InputStream getFileData(String strFile) throws WGBackendException {
		return null;
	}

	public int getFileSize(String strFile) throws WGBackendException {
		return 0;
	}

	public Date getCreated() throws WGBackendException {
		return new Date();
	}

	public Date getLastModified() throws WGBackendException {
		return new Date();
	}

	public Object evaluateExpression(String expression) throws WGExpressionException, WGBackendException {
		return null;
	}


	public void setWGDocument(WGDocument doc) {
	}


	public void dispose() {
	}


	public WGDatabaseRevision save(java.util.Date lastModified) throws WGBackendException {
        throw new WGBackendException("Saving not supported by fake document");
	}

	public boolean setMetaData(String strName, Object value) throws WGBackendException {
		return false;
	}

	public List getItemNames() throws WGBackendException {
		return new ArrayList();
	}


	public boolean removeItem(String Name) throws WGBackendException {
		return false;
	}


	public boolean isDataCacheable() {
		return _cacheable;
	}


	public boolean attachFile(File file) throws WGBackendException {
		return false;
	}


	public WGDatabaseRevision remove() throws WGBackendException {
        throw new WGBackendException("Removing not supported by fake document");
	}


	public boolean removeFile(String name) throws WGBackendException {
		return false;
	}


	public boolean isSaved() {
		return false;
	}


	public Object getNativeObject() throws WGBackendException {
		return null;
	}

    protected static final String FAKENAME_INNERTML = "__fake_innertml";
    protected static final String FAKENAME_OUTERTML = "__fake_outertml";

    public void setCacheable(boolean cacheable) {
        _cacheable = cacheable;
    }

    public String getOriginDatabase() {
        return db.getDbReference();
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
        throw new WGNotSupportedException("Content Relations are not supported");
    }

    public Object getExtensionData(String strName) throws WGAPIException {
        return null;
    }

    public List getExtensionDataNames() throws WGAPIException {
        throw new WGNotSupportedException("Attributes are not supported");
    }

    public void removeExtensionData(String strName) throws WGAPIException {
        throw new WGNotSupportedException("Attributes are not supported");
        
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
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
    
    
    public WGFileDerivateMetaData createFileDerivate(String originalFileName, String creator, String derivateName, InputStream in, Map<String,Object> customMdFields) throws WGAPIException {
        throw new WGNotSupportedException("File derivates are not supported on this database");
    }
    
    @Override
    public void removeFileDerivate(String id) throws WGAPIException {
        throw new WGNotSupportedException("File derivates are not supported on this database");   
    }

    @Override
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException {
        return null;
    }

    @Override
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException, WGNotSupportedException {
        throw new WGNotSupportedException("File derivates are not supported on this database");
    }

    @Override
    public InputStream getFileDerivateData(String id) throws WGAPIException {
        return null;
    }

    @Override
    public Iterator<WGUpdateLog> getLastUpdates() throws WGAPIException {
        return Collections.<WGUpdateLog>emptyList().iterator();
    }

}

