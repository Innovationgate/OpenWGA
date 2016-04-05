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
package de.innovationgate.webgate.api.query.rss;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.GetMethod;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.SAXReader;

import de.innovationgate.webgate.api.WGACLCore;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentStoreVersionException;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseCore;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentCore;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGInvalidDatabaseException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGPageOrderSet;
import de.innovationgate.webgate.api.WGProcedureException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGResultSetCore;
import de.innovationgate.webgate.api.WGSessionContext;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.fake.WGFakeLanguage;

public class WGDatabaseImpl implements WGDatabaseCore {
	
	class CachedPage {
		
		private Document document;
		private Date created;
		
		public CachedPage(Document doc) {
			this.document = doc;
			this.created = new Date();
		}
		
		public Date getCreated() {
			return created;
		}

		public Document getDocument() {
			return document;
		}

	}
	
	class CleanupTask extends TimerTask {
		
		private WGDatabaseImpl db;
		
		public CleanupTask(WGDatabaseImpl db) {
			this.db = db;
		}
		
		public void run() {
			
			Thread.currentThread().setName("WGAPI.RSS.CleanupTask");
			
			// Calculate cutoff date, where cached pages older than this will be deleted
			GregorianCalendar cutoffDateCalendar = (GregorianCalendar) GregorianCalendar.getInstance();
			cutoffDateCalendar.setTime(new Date());
			cutoffDateCalendar.add(Calendar.MINUTE, db.cacheLatency * -1);
			Date cutoffDate = cutoffDateCalendar.getTime();
			
			// Iterate over cached pages - remove outdated
			synchronized (db.cachedPages) {
				Iterator cachedPages = db.cachedPages.values().iterator();
				CachedPage cachedPage;
				Object key;
				while (cachedPages.hasNext()) {
					cachedPage = (CachedPage) cachedPages.next();
					if (cachedPage.getCreated().before(cutoffDate)) {
						cachedPages.remove();
					}
				}
			}
		}
	}
	
	public static final String COPTION_CACHE_LATENCY = "CacheLatency";

    private static final Object COPTION_LANGUAGE = "Language";
	
	private WGDatabase db;
	private int cacheLatency = 10;
	private Map cachedPages = Collections.synchronizedMap(new HashMap());
	private Timer timer = new Timer();

    private String _fakeLanguage;

	public WGUserAccess open(WGDatabase db, String path, String user, String pwd, boolean prepareOnly) throws WGInvalidDatabaseException, WGIllegalDataException {
		this.db = db;
		String cacheLatencyStr = (String) db.getCreationOptions().get(WGDatabaseImpl.COPTION_CACHE_LATENCY);
		if (cacheLatencyStr != null) {
			cacheLatency = Integer.parseInt(cacheLatencyStr);
		}
		
		// Init fake language
        _fakeLanguage = (String) db.getCreationOptions().get(COPTION_LANGUAGE);
        if (_fakeLanguage == null) {
            _fakeLanguage = Locale.getDefault().getLanguage();
        }
		
		this.timer.scheduleAtFixedRate(new CleanupTask(this), 1000 * 60, 1000 * 60);
		
		return new WGUserAccess(WGDatabase.MASTER_USERNAME, WGDatabase.ACCESSLEVEL_READER);
	}

	public WGUserAccess openSession(AuthenticationSession authSession, Object pwd, boolean master) throws WGIllegalDataException {
		return new WGUserAccess(authSession.getDistinguishedName(), WGDatabase.ACCESSLEVEL_READER);
	}

	public void close() {
	    timer.cancel();
	}

	public void closeSession() {}

	public String getTitle() {
		return "";
	}

	public String getTypeName() {
		return "http/rss";
	}

	public Date getRevision() {
		return new Date();
	}

	public Object getExtensionData(String name) {
	    return null;
	}

	public List getRoles() {
		Object[] roles =  {WGDatabase.ROLE_CONTENT };
		return Arrays.asList( roles );
	}

	public boolean hasFeature(String feature) {

		if (feature.equals(WGDatabase.FEATURE_QUERYABLE)) {
			return true;
		}
		else {
			return false;
		}


	}

	public Iterator<WGDocumentCore> getChildEntries(WGStructEntry structEntry, WGPageOrderSet order) {
	    return Collections.EMPTY_LIST.iterator();
	}

	public Iterator<WGDocumentCore> getRootEntries(WGArea area, WGPageOrderSet pageOrder) {
		return null;
	}

	public WGDocumentCore getStructEntryByKey(Object key) {
		return null;
	}

	public WGDocumentCore getParentEntry(WGStructEntry entry) {
		return null;
	}

	public List getAllContent(WGStructEntry structEntry, boolean includeArchived) {
		return null;
	}

	public WGDocumentCore getContentByKey(WGContentKey key) {
		return null;
	}

	public WGDocumentCore getContentByName(String strName, String strLanguage) {
		return null;
	}

	public WGDocumentCore getDummyContent(String language) {
		return null;
	}

	public WGResultSetCore query(String type, String query, Map parameters) throws WGQueryException {

		Document doc = this.retrievePage(query);
		if (doc == null) {
			throw new WGQueryException(query, "Could not retrieve or parse RSS");
		}
		return new WGResultSetImpl(this.db, doc);

	}

	public List getDesignObjects(int type) {	
		
	    if (type == WGDocument.TYPE_LANGUAGE) {
	        return Collections.singletonList(new WGFakeLanguage(db, _fakeLanguage, _fakeLanguage));
	    }
	    
	    return Collections.emptyList();
	    
	}

	public WGDocumentCore getDesignObject(int type, String name, String strMediaKey) {

		if (type == WGDocument.TYPE_LANGUAGE && _fakeLanguage.equals(name)) {
			return new WGFakeLanguage(db, _fakeLanguage, _fakeLanguage);
		}
		else {
			return null;
		}

	}

	public List getNewDesignsSince(Date date) {
		return null;
	}

	public WGDocumentCore getUserProfile(String name) {
		return null;
	}

	public WGDocumentCore createUserProfile(String name, int type) {
		return null;
	}

	public WGDocumentCore fastAccess(int type,Object key) {
		return null;
	}

	public boolean isStructEntryKey(Object key) {
		return false;
	}

	public Object parseStructKey(String key) {
		return null;
	}

	public void cleanup() {}

	public WGDocumentCore createDesignDocument(int type, String name, String mediaKey) {
		return null;
	}

	public WGDocumentCore createStructEntry(Object key, WGDocument reference, WGContentType contentType) {
		return null;
	}

	public WGDocumentCore createContent(WGStructEntry structEntry, WGLanguage language, String title, int version) {
		return null;
	}
	
	private Document retrievePage(String url) {
		
		try {
			
			// Try to retrieve from cache
			CachedPage page = (CachedPage) this.cachedPages.get(url);
			if (page != null) {
				return page.getDocument();
			}
			
			SAXReader reader = new SAXReader();
						
			// Retrieve from web
			HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
			client.setConnectionTimeout(5000);
			HttpMethod method = new GetMethod(url);
			method.setFollowRedirects(true);
			method.setStrictMode(false);
			client.executeMethod(method);
			
			// Read response. Wrap content decoder if necessary.
			InputStream inStream = method.getResponseBodyAsStream();
			
			// Not necessary - HttpClient decodes automatically
			/*Header contentEncoding = method.getResponseHeader("Content-Encoding");
			if (contentEncoding != null) {
				if (contentEncoding.getValue().equals("gzip")) {
					inStream = new GZIPInputStream(inStream);
				}
			}*/
			Document doc = reader.read(inStream);
			
			/*HttpURLConnection conn = (HttpURLConnection) (new URL(url)).openConnection();
			Document doc = reader.read(conn.getInputStream());*/
			
			// Put into cache, if cache latency is set
			if (cacheLatency > 0) {
				this.cachedPages.put(url, new CachedPage(doc));
			}
			
			return doc;
		}
		catch (MalformedURLException e) {
			e.printStackTrace();
			return null;
		}
		catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		catch (DocumentException e) {
			e.printStackTrace();
			return null;
		}
		
	}

	public Class getDedicatedWorkflowEngine() {
		return null;
	}

	public boolean isMemberOfUserList(List userList) {
		return false;
	}

	public WGDocumentCore createCopy(WGDocumentCore original) {
		return null;
	}

	public boolean beginTransaction() {
		return false;
	}

	public boolean commitTransaction() {
		return false;
	}

	public boolean rollbackTransaction() {
		return false;
	}

	public Object execProcedure(String procName, List args) throws WGProcedureException {
		throw new WGProcedureException("Implementation '" + getTypeName() + "' does not support procedures.");
	}

	public String getServerName() {
		return "(none)";
	}


	public boolean resultIsTrue(Object result, WGDocument doc) {
		return false;
	}	
	
	public boolean resultIsFalse(Object result, WGDocument doc) {
		return false;
	}	
	
	public WGACLCore getACL() {
		return null;
	}

	public Object getNativeObject() {
		return null;
	}

	public void refresh() {}

	public List getUpdatedDocumentsSince(Date cutoff) {
		return null;
	}

    public boolean moveStructEntry(WGStructEntry entry, WGDocument newParent) {
        return false;
    }

    public int getContentCount(WGStructEntry entry) throws WGNotSupportedException {
        throw new WGNotSupportedException("Not supported");
    }

    public void setCurrentSession(WGSessionContext context) {
    }

    public String convertFileNameForAttaching(String name) {
        return null;
    }

    public Class[] getAllowedCredentialClasses() {
        return new Class[] { String.class };
    }
    
    
    public List queryUserProfileNames(String type, String query, Map params) {
        return null;
    }
    
    public Set getDeletions(Set contentKeys) throws WGAPIException {
        // unsupported for this implementation
        return Collections.EMPTY_SET;
    }

    public List getAllContentKeys(boolean includeArchived) throws WGAPIException {
        throw new WGNotSupportedException("This method is not supported by this WGAPI implementation");
    }

	public void beginUpdate() {
	}

    public WGDocumentCore getStructEntryByName(String strName) throws WGAPIException {
        throw new WGNotSupportedException("This method is not supported by this WGAPI implementation");
    }

    public Date getRevisionDate(Comparable lastChanged) throws WGAPIException {
        return (Date) lastChanged;
    }

    public List getUpdateLogs(Comparable cutoff) throws WGAPIException {
        return null;
    }

    public double getContentStoreVersion() throws WGAPIException {
        return WGDatabase.CSVERSION_NO_CONTENTSTORE;
    }
    
    @Override
    public int getContentStorePatchLevel() throws WGAPIException {
        return 0;
    }

    public void writeExtensionData(String name, Object value) throws WGAPIException {
    }

    public void removeExtensionData(String name) throws WGAPIException {
    }
    
    public List<String> getExtensionDataNames() throws WGAPIException {
        return Collections.EMPTY_LIST;
    }
    
    public List<WGRelationData> getIncomingRelations(Object structKey, String language, String contentClass, String relName, String relGroupName, Boolean includeUnreleased, WGColumnSet order) throws WGAPIException {
        return Collections.emptyList();
    }

    public boolean isContentTypeUsed(WGContentType ct) throws WGAPIException {
        return true;
    }

    public boolean isLanguageUsed(WGLanguage lang) throws WGAPIException {
        return true;
    }
    
    public boolean isBackendServiceSupported(String serviceName) {
        return false;
    }
    
    public Object callBackendService(String serviceName, Object[] params) throws WGAPIException {
        throw new WGNotSupportedException("No backend services");
    }

    @Override
    public void clearSessionCache() throws WGAPIException {
        // TODO Auto-generated method stub
        
    }
    
    public WGFileDerivateMetaData createFileDerivate(WGDocumentCore doc, String originalFileName, String creator, String derivateName, InputStream in, Map<String,Object> customMdFields) throws WGAPIException {
        throw new WGNotSupportedException("File derivates are not supported on this database");
    }


}
