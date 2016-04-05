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

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import de.innovationgate.webgate.api.WGACLCore;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGPageOrderSet;
import de.innovationgate.webgate.api.WGPersonalisationDatabaseCore;
import de.innovationgate.webgate.api.WGProcedureException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGResultSetCore;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.WGWrongRevisionException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.workflow.WGDefaultWorkflowEngine;

public abstract class WGFakeDatabase implements WGDatabaseCore, WGPersonalisationDatabaseCore {
	
	public static final String COPTION_LANGUAGE = "Language";
	
	public static final String DBTYPE = "fake";
	public static final String FAKE_TITLE = "";
	public static final String FAKE_STRUCTKEY = "fake";
	public static final String FAKE_CONTENTNAME = "fake content";
	public static final String FAKE_LANGUAGE = "de";
	public static final String FAKE_CONTENTTYPE = "Standard";
	public static final String FAKE_TML = "empty";
	public static final String FAKE_AREA = "home";
	
	protected WGDatabase enclosingDB;
	protected String path;
	protected String user;
	protected String pwd;
	protected Date openingTime;

	/**
	 * @throws WGBackendException 
	 * @see WGDatabaseCore#open(WGDatabase, String, String, String, boolean)
	 */
	public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGInvalidDatabaseException, WGBackendException {
		this.enclosingDB = db;
		this.path = path;
		this.user = user;
		this.pwd = pwd;
		this.openingTime = new Date();
		return new WGUserAccess(WGDatabase.MASTER_USERNAME, WGDatabase.ACCESSLEVEL_MANAGER);
	}
	
	private List getOneFakeDocumentList(int type) {
		ArrayList list = new ArrayList();
		list.add(new WGFakeDocument(this.enclosingDB, type));
		return list;
	}

	public void close() throws WGBackendException {
	}

	public void closeSession() throws WGAPIException {
	}

	public String getTitle() throws WGBackendException {
		return FAKE_TITLE;
	}

	public String getTypeName() {
		return DBTYPE;
	}

	public Date getCreated() throws WGBackendException {
		return openingTime;
	}

	public Date getRevision() throws WGBackendException {
		return openingTime;
	}

	public Object getExtensionData(String name) throws WGBackendException {
	    return null;
	}
	
	public static String getFakeLanguage(Map creationOptions) {
		String lang = (String) creationOptions.get(WGFakeDatabase.COPTION_LANGUAGE);
		if (lang != null) {
			return lang;
		}
		else {
			return Locale.getDefault().getLanguage();
		}
	}

	public Iterator<WGDocumentCore> getChildEntries(WGStructEntry structEntry, WGPageOrderSet order) {
		return Collections.EMPTY_LIST.iterator();
	}

	public Iterator<WGDocumentCore> getRootEntries(WGArea area, WGPageOrderSet pageOrder) {

		return getOneFakeDocumentList(WGDocument.TYPE_STRUCTENTRY).iterator();

	}

	public WGDocumentCore getStructEntryByKey(Object key) {
		
		if (key != null && key.equals(FAKE_STRUCTKEY)) {
			return new WGFakeDocument(this.enclosingDB, WGDocument.TYPE_STRUCTENTRY);
		}
		else {
			return null;
		}
	}

	public WGDocumentCore getParentEntry(WGStructEntry entry) {
		return null;
	}

	public List getAllContent(WGStructEntry structEntry, boolean includeCurrent) {
		
		return this.getOneFakeDocumentList(WGDocument.TYPE_CONTENT);
		
	}

	public WGDocumentCore getContentByKey(WGContentKey key) {

		if (key.getStructKey().equals(FAKE_STRUCTKEY)) {
			return new WGFakeDocument(this.enclosingDB, WGDocument.TYPE_CONTENT);
		}
		else {
			return null;
		}

	}

	public WGDocumentCore getContentByName(String strName, String strLanguage) {

		if (strName.equals(FAKE_CONTENTNAME) && strLanguage.equals(FAKE_LANGUAGE)) {
			return new WGFakeDocument(this.enclosingDB, WGDocument.TYPE_CONTENT);
		}
		else {
			return null;
		}

	}

	
	public WGDocumentCore getDummyContent(String language) {
		return new WGFakeDocument(this.enclosingDB, WGDocument.TYPE_CONTENT);
	}

	public List getDesignObjects(int type) {
			return Collections.emptyList();
	}

	public WGDocumentCore getDesignObject(int type, String name, String strMediaKey) throws WGAPIException {
		
		List list = this.getDesignObjects(type);
		if (list == null || list.size() == 0) {
			return null;
		}
		
		WGDocumentCore doc = (WGDocumentCore) list.get(0);
		if (!WGDocument.getName(doc).equals(name)) {
			return null;
		} 
		
		if (type == WGDocument.TYPE_TML && !strMediaKey.equals(doc.getMetaData(WGTMLModule.META_MEDIAKEY))) {
			return null;
		}
		else {
			return doc;
		}
		
	}


	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#fastAccess(int, java.lang.Object)
	 */
	public WGDocumentCore fastAccess(int type, Object key) {
		return null;
	}


	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#parseStructKey(java.lang.String)
	 */
	public Object parseStructKey(String key) {
		return key;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#query(java.lang.String, java.lang.String, java.util.Map)
	 */
	public WGResultSetCore query(String type, String query, Map parameters) throws WGQueryException {
		return null;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#hasFeature(java.lang.String)
	 */
	public boolean hasFeature(String feature) {
		return false;
	}
	
	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getRoles()
	 */
	public List getRoles() {
		List list = new ArrayList();
		list.add(WGDatabase.ROLE_CONTENT);
		list.add(WGDatabase.ROLE_DESIGN);
		list.add(WGDatabase.ROLE_REPOSITORY);
		return list;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#createUserProfile(java.lang.String, int)
	 */
	public WGDocumentCore createUserProfile(String name, int type) throws WGAPIException {
		return null;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getUserProfile(java.lang.String)
	 */
	public WGDocumentCore getUserProfile(String name) throws WGBackendException {
		return null;
	}


	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#createContent(de.innovationgate.webgate.api.WGStructEntry, de.innovationgate.webgate.api.WGLanguage, java.lang.String, int)
	 */
	public WGDocumentCore createContent(WGStructEntry structEntry, WGLanguage language, String title, int version) {
		return null;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#createDesignDocument(int, java.lang.String, java.lang.String)
	 */
	public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) {
		return null;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#createStructEntry(java.lang.Object, de.innovationgate.webgate.api.WGDocument, de.innovationgate.webgate.api.WGContentType)
	 */
	public WGDocumentCore createStructEntry(Object key, WGDocument reference, WGContentType contentType) {
		return null;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getDedicatedWorkflowEngine()
	 */
	public Class getDedicatedWorkflowEngine() {
		return WGDefaultWorkflowEngine.class;
	}

	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#isMemberOfUserList(java.util.List)
	 */
	public boolean isMemberOfUserList(List userList) {
		return false;
	}


	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#createCopy(de.innovationgate.webgate.api.WGDocumentCore)
	 */
	public WGDocumentCore createCopy(WGDocumentCore original) {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#beginTransaction()
	 */
	public boolean beginTransaction() {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#commitTransaction()
	 */
	public boolean commitTransaction() {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#rollbackTransaction()
	 */
	public boolean rollbackTransaction() {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getServerName()
	 */
	public String getServerName() throws WGAPIException {
		return "(none)";
	}



	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#resultIsFalse(java.lang.Object, de.innovationgate.webgate.api.WGDocument)
	 */
	public boolean resultIsFalse(Object result, WGDocument doc) {
		return false;
	}
	
	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#resultIsTrue(java.lang.Object, de.innovationgate.webgate.api.WGDocument)
	 */
	public boolean resultIsTrue(Object result, WGDocument doc) {
		return false;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getACL()
	 */
	public WGACLCore getACL() {
		return null;
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getNativeObject()
	 */
	public Object getNativeObject() throws WGBackendException {
		return null;
	}
	
	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#refresh()
	 */
	public void refresh() {}
	
	@Override
	public void clearSessionCache() throws WGAPIException {
	}


	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDatabaseCore#getUpdateLogs(java.lang.Comparable)
	 */
	public List getUpdateLogs(Comparable cutoff) {
		return null;
	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#getContentCount(de.innovationgate.webgate.api.WGStructEntry)
     */
    public int getContentCount(WGStructEntry entry) throws WGNotSupportedException {
        throw new WGNotSupportedException("Not supported");
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#moveStructEntry(de.innovationgate.webgate.api.WGStructEntry, de.innovationgate.webgate.api.WGDocument)
     */
    public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) {
        return false;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#setCurrentSession(de.innovationgate.webgate.api.WGSessionContext)
     */
    public void setCurrentSession(WGSessionContext context) {
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseCore#convertFileNameForAttaching(java.lang.String)
     */
    public String convertFileNameForAttaching(String name) {
        return name;
    }
    
    public List queryUserProfileNames(String type, String query, Map params) {
        return Collections.emptyList();
    }
    
    public Iterator<String> getAllUserProfileNames() throws WGAPIException {
        List<String> emptyList = Collections.emptyList();
        return emptyList.iterator();
    }

    public List getAllContentKeys(boolean includeArchived) throws WGAPIException {
        throw new WGNotSupportedException("This method is not supported by this WGAPI implementation");
    }

    public Date getRevisionDate(Comparable lastChanged) throws WGAPIException, WGWrongRevisionException {
        try {
            return (Date) lastChanged;
        }
        catch (ClassCastException e) {
            throw new WGWrongRevisionException(Date.class); 
        }
    }

    @Override
    public double getContentStoreVersion() throws WGAPIException {
        return WGDatabase.CSVERSION_NO_CONTENTSTORE;
    }
    
    @Override
    public int getContentStorePatchLevel() throws WGAPIException {
        return 0;
    }

    public Set getDeletions(Set documentKeys) throws WGAPIException {
        return Collections.EMPTY_SET;
    }

    public WGDocumentCore getStructEntryByName(String strName) throws WGAPIException {
        return null;
    }

    public void writeExtensionData(String name, Object value) throws WGAPIException {
    }

    public void removeExtensionData(String name) throws WGAPIException {
    }

    public List<String> getExtensionDataNames() throws WGAPIException {
        return Collections.EMPTY_LIST;
    }

    public List<WGRelationData> getIncomingRelations(Object structKey, String language, String contentClass, String relName, String relGroupName, Boolean includeUnreleased, WGColumnSet order) throws WGAPIException {
        return Collections.EMPTY_LIST;
    }


}

