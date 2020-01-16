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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import javax.activation.DataSource;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.XStreamUtils;
import de.innovationgate.webgate.api.WGSessionContext.DocumentContext;
import de.innovationgate.webgate.api.fake.WGFakeDocument;
import de.innovationgate.webgate.api.locking.Lock;
import de.innovationgate.webgate.api.locking.LockException;
import de.innovationgate.webgate.api.locking.LockOwner;
import de.innovationgate.webgate.api.locking.Lockable;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.common.ImmutableObject;
import eu.medsea.mimeutil.MimeUtil;

/**
 * Parent class for all document objects in a WGA database.
 */
public abstract class WGDocument implements Lockable, WGExtensionDataContainer, WGFileMetaDataContext, MetaInfoProvider {
    
    public static Pattern DERIVATE_NAME_PATTERN = Pattern.compile("[A-Za-z0-9_-]+");
    
    /**
     * An interface defining an action that can be execute after the current
     * document was saved in the current session
     */
    public interface SaveAction {
        
        /**
         * The action to be ran
         * @param doc The saved document
         * @throws Exception
         */
        public void run(WGDocument doc) throws Exception;
    }
    
    /**
     * Fake md context when the metadata does not represent already stored md 
     */
    public static class FakeMetaDataContext implements WGFileMetaDataContext {
        
        private boolean _modified = false;

        @Override
        public void markMetaDataModified(WGFileMetaData md) throws WGAPIException {
            _modified = true;
        }
        
        public boolean isModified() {
            return _modified;
        }
        
        @Override
        public WGDocument getFileParent() {
            return null;
        }
        
    }
    
    public static class Cache {

        private WGDatabaseRevision _cacheRevision = null;
        private Map<String, Object> _extensionDataCache = new ConcurrentHashMap<String,Object>();
        private Map<String, Object> _fileMetaCache = new ConcurrentHashMap<String,Object>();
        private List<String> _fileNamesCache = null;
        private Map<String, Object> _itemCache = new ConcurrentHashMap<String, Object>();
        private Map<String, Object> _metaCache = new ConcurrentHashMap<String, Object>();
        private Map<String,Object> _persistentStore = new ConcurrentHashMap<String, Object>();
        private volatile boolean _deletedFlag = false;
        
        public WGDatabaseRevision getCacheRevision() {
            return _cacheRevision;
        }
        public Map<String, Object> getExtensionDataCache() {
            return _extensionDataCache;
        }
        public Map<String, Object> getFileMetaCache() {
            return _fileMetaCache;
        }
        public List<String> getFileNamesCache() {
            return _fileNamesCache;
        }
        public Map<String, Object> getItemCache() {
            return _itemCache;
        }
        public Map<String, Object> getMetaCache() {
            return _metaCache;
        }
        public Map<String, Object> getPersistentStore() {
            return _persistentStore;
        }
        public void setCacheRevision(WGDatabaseRevision cacheRevision) {
            _cacheRevision = cacheRevision;
        }
        public void setFileNamesCache(List<String> fileNamesCache) {
            _fileNamesCache = fileNamesCache;
        }
        
        public void dropCache() {
            _itemCache.clear();
            _metaCache.clear();
            _extensionDataCache.clear();
            _fileMetaCache.clear();
            _fileNamesCache = null;
            dropRelations();
        }
        
        public void dropRelations() {
        }
        public boolean isDeletedFlag() {
            return _deletedFlag;
        }
        public void setDeletedFlag(boolean deletedFlag) {
            _deletedFlag = deletedFlag;
        }
        
        
    }
	
	/**
	 * Feature identifier for {@link WGContentStoreVersionException} regarding extension data operations
	 */
	public static final String CSFEATURE_EXTDATA = "Extension data";

    static MetaConverter TITLE_CONVERTER = new MetaConverter() {
		public Object convert(WGDocument doc, MetaInfo metaInfo, Object value) throws WGAPIException {
			String title = (String) value;
			if (title != null) {
				title = WGUtils.strReplace(title, "\n", "", true);
				title = WGUtils.strReplace(title, "\r", "", true);
				title = WGUtils.strReplace(title, "\t", "", true);
			}
			return title;
		}    		
	};
    
    //  Metadata available for all documents
    public static final String META_CREATED = "CREATED";
    public static final MetaInfo METAINFO_CREATED = new MetaInfo(META_CREATED, Date.class, new Date(Long.MIN_VALUE));
    
    public static final String META_LASTMODIFIED = "LASTMODIFIED";
    public static final MetaInfo METAINFO_LASTMODIFIED = new MetaInfo(META_LASTMODIFIED, Date.class, new Date(Long.MIN_VALUE));
    static { METAINFO_LASTMODIFIED.addSynonym("MODIFIED"); };

    public static final String META_REVISION = "REVISION";
    public static final MetaInfo METAINFO_REVISION = new MetaInfo(META_REVISION, Integer.class, new Integer(0));
    static { METAINFO_REVISION.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_NOINDEX); };
    
    public static final String META_PASTAUTHORS = "PASTAUTHORS";
    public static final MetaInfo METAINFO_PASTAUTHORS = new MetaInfo(META_PASTAUTHORS, String.class, Collections.EMPTY_LIST);
    static { METAINFO_PASTAUTHORS.setMultiple(true); METAINFO_PASTAUTHORS.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_NOINDEX); };

    public static final String META_PASTEDITDATES = "PASTEDITDATES";
    public static final MetaInfo METAINFO_PASTEDITDATES = new MetaInfo(META_PASTEDITDATES, Date.class, Collections.EMPTY_LIST);
    static { METAINFO_PASTEDITDATES.setMultiple(true); METAINFO_PASTEDITDATES.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_NOINDEX); };



    private Date objectCreated;

    protected boolean temporary;
    
    /**
     * Identifies this content object as a temporary duplicate of a another content object, meant to stay temporary
     */
    protected boolean tempDuplicate = false;
    
    protected WGDatabaseRevision getCacheRevision() {
        return _cache.getCacheRevision();
    }

    /**
     * Divider character for {@link WGDocumentKey} strings, dividing the key parts
     */
    public static final String DOCKEY_DIVIDER = "/";

    // Document types
    /**
     * Document types of WGContent objects
     */
    public static final int TYPE_CONTENT = 1;

    /**
     * Document types of WGStructEntry objects
     */
    public static final int TYPE_STRUCTENTRY = 2;

    /**
     * Document types of WGArea objects
     */
    public static final int TYPE_AREA = 3;

    /**
     * Document types of WGLanguage objects
     */
    public static final int TYPE_LANGUAGE = 4;

    /**
     * Document types of WGContentType objects
     */
    public static final int TYPE_CONTENTTYPE = 5;

    /**
     * Document types of WGTMLModule objects
     */
    public static final int TYPE_TML = 6;

    /**
     * Document types of WGCSSJSModule objects
     */
    public static final int TYPE_CSSJS = 7;

    /**
     * Document types of WGFileContainer objects
     */
    public static final int TYPE_FILECONTAINER = 8;

    /**
     * Document types of WGUserProfile objects
     */
    public static final int TYPE_USERPROFILE = 9;
    
    
    /**
     * Pseudo document type for ACL entries 
     */
    public static final int TYPE_ACLENTRY = 101;

    /**
     * Pseudo document type for database metadata (i.e. extension data on db level) 
     */
    public static final int TYPE_DBMETADATA = 102;
    
    /**
     * Pseudo document type for file derivate 
     */
    public static final int TYPE_FILEDERIVATE = 103;
    
    // Document type names
    /**
     * Document type name of WGContent objects
     */
    public static final String TYPENAME_CONTENT = "content";

    /**
     * Document type name of WGStructEntry objects
     */
    public static final String TYPENAME_STRUCTENTRY = "structentry";

    /**
     * Document type name of WGArea objects
     */
    public static final String TYPENAME_AREA = "area";

    /**
     * Document type name of WGLanguage objects
     */
    public static final String TYPENAME_LANGUAGE = "language";

    /**
     * Document type name of WGContentType objects
     */
    public static final String TYPENAME_CONTENTTYPE = "contenttype";

    /**
     * Document type name of WGTMLModule objects
     */
    public static final String TYPENAME_TML = "tml";

    /**
     * Document type name of WGCSSJSModule objects
     */
    public static final String TYPENAME_CSSJS = "cssjs";

    /**
     * Document type name of WGFileContainer objects
     */
    public static final String TYPENAME_FILECONTAINER = "filecontainer";

    /**
     * Document type name of WGUserProfile objects
     */
    public static final String TYPENAME_USERPROFILE = "userprofile";
    
    /**
     * Pseudo document type name for ACL entries
     */
    public static final String TYPENAME_ACLENTRY = "$aclentry";
    
    /**
     * Pseudo document type name for database metadata
     */
    public static final String TYPENAME_DBMETADATA = "$dbmetadata";
    
    /**
     * Pseudo document type name for file derivate
     */
    public static final String TYPENAME_FILEDERIVATE = "$filederivate";


    /**
     * Prefix for names of extension data fields that actually store document metadata fields
     */
    public static final String EXTDATA_META_PREFIX = "meta_";
    
    /**
     * extension data field which stores the "primary attachment" for this document in CS Versions > 5
     */
    public static final String EXT_PRIMARY_ATTACHMENT = "primaryAttachment";

    // Static mappings from doctype name to doctype number
    private static Map<String,Integer> docTypeNameToNumber = new HashMap<String,Integer>();

    private static Map<Integer,String> docTypeNumberToName = new HashMap<Integer,String>();
    
    private static Map<Integer,Class<?>> docTypeNumberToClass = new HashMap<Integer,Class<?>>();
    
    private static Map<Class<?>,Integer> docTypeClassToNumber = new HashMap<Class<?>,Integer>();


    
    // Initialisations of mappings
    static {

        // Mappings from doctype name to number and vice versa
        docTypeNameToNumber.put(TYPENAME_CONTENT, new Integer(TYPE_CONTENT));
        docTypeNameToNumber.put(TYPENAME_STRUCTENTRY, new Integer(TYPE_STRUCTENTRY));
        docTypeNameToNumber.put(TYPENAME_AREA, new Integer(TYPE_AREA));
        docTypeNameToNumber.put(TYPENAME_LANGUAGE, new Integer(TYPE_LANGUAGE));
        docTypeNameToNumber.put(TYPENAME_CONTENTTYPE, new Integer(TYPE_CONTENTTYPE));
        docTypeNameToNumber.put(TYPENAME_TML, new Integer(TYPE_TML));
        docTypeNameToNumber.put(TYPENAME_CSSJS, new Integer(TYPE_CSSJS));
        docTypeNameToNumber.put(TYPENAME_FILECONTAINER, new Integer(TYPE_FILECONTAINER));
        docTypeNameToNumber.put(TYPENAME_USERPROFILE, new Integer(TYPE_USERPROFILE));
        docTypeNameToNumber.put(TYPENAME_ACLENTRY, new Integer(TYPE_ACLENTRY));
        docTypeNameToNumber.put(TYPENAME_DBMETADATA, new Integer(TYPE_DBMETADATA));
        docTypeNameToNumber.put(TYPENAME_FILEDERIVATE, new Integer(TYPE_FILEDERIVATE));
        docTypeNameToNumber = Collections.unmodifiableMap(docTypeNameToNumber);

        docTypeNumberToName.put(new Integer(TYPE_CONTENT), TYPENAME_CONTENT);
        docTypeNumberToName.put(new Integer(TYPE_STRUCTENTRY), TYPENAME_STRUCTENTRY);
        docTypeNumberToName.put(new Integer(TYPE_AREA), TYPENAME_AREA);
        docTypeNumberToName.put(new Integer(TYPE_LANGUAGE), TYPENAME_LANGUAGE);
        docTypeNumberToName.put(new Integer(TYPE_CONTENTTYPE), TYPENAME_CONTENTTYPE);
        docTypeNumberToName.put(new Integer(TYPE_TML), TYPENAME_TML);
        docTypeNumberToName.put(new Integer(TYPE_CSSJS), TYPENAME_CSSJS);
        docTypeNumberToName.put(new Integer(TYPE_FILECONTAINER), TYPENAME_FILECONTAINER);
        docTypeNumberToName.put(new Integer(TYPE_USERPROFILE), TYPENAME_USERPROFILE);
        docTypeNumberToName.put(new Integer(TYPE_ACLENTRY), TYPENAME_ACLENTRY);
        docTypeNumberToName.put(new Integer(TYPE_DBMETADATA), TYPENAME_DBMETADATA);
        docTypeNumberToName.put(new Integer(TYPE_FILEDERIVATE), TYPENAME_FILEDERIVATE);
        docTypeNumberToName = Collections.unmodifiableMap(docTypeNumberToName);
        
        docTypeNumberToClass.put(new Integer(TYPE_CONTENT), WGContent.class);
        docTypeNumberToClass.put(new Integer(TYPE_STRUCTENTRY), WGStructEntry.class);
        docTypeNumberToClass.put(new Integer(TYPE_AREA), WGArea.class);
        docTypeNumberToClass.put(new Integer(TYPE_LANGUAGE), WGLanguage.class);
        docTypeNumberToClass.put(new Integer(TYPE_CONTENTTYPE), WGContentType.class);
        docTypeNumberToClass.put(new Integer(TYPE_TML), WGTMLModule.class);
        docTypeNumberToClass.put(new Integer(TYPE_CSSJS), WGCSSJSModule.class);
        docTypeNumberToClass.put(new Integer(TYPE_FILECONTAINER), WGFileContainer.class);
        docTypeNumberToClass.put(new Integer(TYPE_USERPROFILE), WGUserProfile.class);
        docTypeNumberToClass.put(new Integer(TYPE_ACLENTRY), WGACLEntry.class);
        docTypeNumberToClass = Collections.unmodifiableMap(docTypeNumberToClass);
        
        docTypeClassToNumber.put(WGContent.class,new Integer(TYPE_CONTENT));
        docTypeClassToNumber.put(WGStructEntry.class,new Integer(TYPE_STRUCTENTRY));
        docTypeClassToNumber.put(WGArea.class,new Integer(TYPE_AREA));
        docTypeClassToNumber.put(WGLanguage.class,new Integer(TYPE_LANGUAGE));
        docTypeClassToNumber.put(WGContentType.class,new Integer(TYPE_CONTENTTYPE));
        docTypeClassToNumber.put(WGTMLModule.class,new Integer(TYPE_TML));
        docTypeClassToNumber.put(WGCSSJSModule.class,new Integer(TYPE_CSSJS));
        docTypeClassToNumber.put(WGFileContainer.class,new Integer(TYPE_FILECONTAINER));
        docTypeClassToNumber.put(WGUserProfile.class,new Integer(TYPE_USERPROFILE));
        docTypeClassToNumber.put(WGACLEntry.class,new Integer(TYPE_ACLENTRY));
        docTypeClassToNumber = Collections.unmodifiableMap(docTypeClassToNumber);
    }

    /**
     * Returns the meta information for a given metadata field.
     * @param metaName The name of the metadate field to return information for
     * @return returns the MetaInfo for given metaName of this class
     * @throws WGSystemException if the metaDataFramework cannot be accessed
     */
    public MetaInfo getMetaInfo(String metaName) throws WGSystemException {
        return WGFactory.getInstance().getMetaInfo(metaName, getClass());
    }
    
    /**
     * returns the metaInfo of the given metaName for the given doctype
     * @param type
     * @param metaName
     * @return MetaInfo
     * @throws WGSystemException if metaDataFramework access fails
     * @throws WGIllegalArgumentException if type or metaName are invalid
     */
    public static MetaInfo getMetaInfoByType(int type, String metaName) throws WGSystemException, WGIllegalArgumentException {
        @SuppressWarnings("unchecked")
        Class<? extends MetaInfoProvider> typeClass = (Class<? extends MetaInfoProvider>) docTypeNumberToClass.get(new Integer(type));
        
        if (typeClass == null) {
            throw new WGIllegalArgumentException("Class for doctype number '" + type + "' not found.");
        }
        MetaInfo info = WGFactory.getInstance().getMetaInfo(metaName, typeClass);
        if (info == null) {
            throw new WGIllegalArgumentException("Unable to find metainfo for doctype '" + type + "' and name '" + metaName + "'.");            
        } else {
            return info;
        }
        
    }
    
    /**
     * Returns the expected data type of a metadata field
     * @param type Document type for the metadata field
     * @param metaName Name of the metadata field
     * @return Expected data type of the metadata as class object
     * @throws WGIllegalArgumentException 
     * @throws WGSystemException 
     */
    public static Class<?> getExpectedMetaClass(int type, String metaName) throws WGSystemException, WGIllegalArgumentException {
        
        MetaInfo info = getMetaInfoByType(type, metaName);
        return info.getDataType();
        
    }

    /**
     * Determines if the given metadata field should return lists or single values.
     * @param type Document type of metadata field
     * @param name Name of metadata field
     * @return True, if the metadata field should return lists, false if it shouls return single values
     * @throws WGIllegalArgumentException 
     * @throws WGSystemException 
     */
    public static boolean isListMeta(int type, String name) throws WGSystemException, WGIllegalArgumentException {

        MetaInfo info = getMetaInfoByType(type, name);
        return info.isMultiple();
        
    }
    

    
    /**
     * checks all metas of this document to ensure value and datatype correspond to the current
     * api implementation. Parameter autoCorrection can be used to automatically correct wrong
     * metadata instead of throwing an exception.
     * @throws WGAPIException 
     */
    public boolean validateMetas(boolean autoCorrection) throws WGAPIException {
        
        boolean somethingCorrected = false;
        Iterator<String> metaNames = getMetaNames().iterator();
        
        double csVersion = getDatabase().getContentStoreVersion();
        while (metaNames.hasNext()) {
            String name = (String) metaNames.next();
            MetaInfo metaInfo = this.getMetaInfo(name);
            if (metaInfo.getMinCsVersion() > getDatabase().getContentStoreVersion()) {
                continue;
            }
            
            
            try {
                Object value = retrieveMetaData(getCore(), metaInfo, csVersion);
                
                // check output conversions (meta can be read from backend)
                value = metaInfo.convertOutput(this, value);
                // check for input (checks also allowed values)
                metaInfo.convertInput(this, value);
            }
            catch (Throwable e) {
                if (autoCorrection) {
                    setMetaToDefault(name);
                    somethingCorrected = true;
                }
                else {
                    throw new WGBackendException("Exception validating metadata", e);
                }
            }
        }
        
        return somethingCorrected;
    }
    
    /**
     * Variant of {@link #validateMetas(boolean)} that performs no auto correction.
     * @return true, if the metadata is valid
     * @throws WGAPIException
     */
    public boolean validateMetas() throws WGAPIException {
        
        return validateMetas(false);
        
    }
    
    /**
     * sets the given meta to the default value
     * @param metaName
     * @throws WGAPIException
     */
    public void setMetaToDefault(String metaName) throws WGAPIException {
        MetaInfo metaInfo = getMetaInfo(metaName);
        setMetaData(metaName, metaInfo.getDefaultValue());
    }
    
    /**
     * Builds the document key for a document core.
     * @param core
     * @throws WGAPIException 
     */
    public static WGDocumentKey buildDocumentKey(WGDocumentCore core, WGDatabase db) throws WGAPIException {

        String name = null;
        String mediaKey = null;
        switch (core.getType()) {

            case WGDocument.TYPE_CONTENT:
                name = String.valueOf(WGContentKey.create(core, true));
                break;

            case WGDocument.TYPE_AREA:
            case WGDocument.TYPE_CONTENTTYPE:
            case WGDocument.TYPE_FILECONTAINER:
            case WGDocument.TYPE_LANGUAGE:
                name = (String) core.getMetaData(WGDesignDocument.META_NAME);
                break;

            case WGDocument.TYPE_TML:
                name = (String) core.getMetaData(WGDesignDocument.META_NAME);
                mediaKey = (String) core.getMetaData(WGTMLModule.META_MEDIAKEY);
                break;
                
            case WGDocument.TYPE_CSSJS:
                name = (String) core.getMetaData(WGDesignDocument.META_NAME);
                // Let's see if generally extending script module keys breaks anything...
                /*if (db.getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {*/
                    mediaKey = (String) core.getMetaData(WGCSSJSModule.META_CODETYPE);                    
                /*}*/


            case WGDocument.TYPE_USERPROFILE:
                name = String.valueOf(core.getMetaData(WGUserProfile.META_NAME));
                break;

            case WGDocument.TYPE_STRUCTENTRY:
                name = String.valueOf(core.getMetaData(WGStructEntry.META_KEY));
                break;

        }

        return new WGDocumentKey(core.getType(), name, mediaKey);

    }    

    protected WGDatabase db = null;

    protected Object fastAccessKey = null;

    protected WGDocumentKey uniqueId = null;

    private boolean cachingEnabled = false;

    private String originDatabase;

    protected WGDocumentCore temporaryCore = null;

    protected Cache _cache;

    /**
     * (Re-)Retrieves the core object for this document. 
     * @throws WGAPIException 
     */
    protected abstract WGDocumentCore retrieveCore() throws WGAPIException;

    /**
     * Returns the document key of this document, which is unique in this
     * database among all documents.
     */
    public String getDocumentKey() {
        return uniqueId.toString();
    }
    
    /**
     * Returns a parsed document key object, ready to access single parts of the key
     */
    public WGDocumentKey getDocumentKeyObj() {
        return uniqueId;
    }

    /**
     * Converts doctype names WGDocument.TYPENAME_... to number constants
     * WGDocument.TYPE_...
     * 
     * @param name
     */
    public static int doctypeNameToNumber(String name) {

        Integer number = (Integer) docTypeNameToNumber.get(name);
        if (number != null) {
            return number.intValue();
        }
        else {
            return 0;
        }
    }
    
    /**
     * Converts doctype numbers WGDocument.TYPE... to document type classes in WGAPI
     * @param number The doctype number
     */
    public static Class<?> doctypeNumberToClass(int number) {
        return (Class<?>) docTypeNumberToClass.get(number);
    }
    
    /**
     * Converts document type classes of the WGAPI to doctype numbers
     * @param clazz the doctype class
     */
    public static Integer docTypeClassToNumber(Class<?> clazz) {
        return (Integer) docTypeClassToNumber.get(clazz);
    }


    /**
     * Converts doctype numbers WGDocument.TYPE_... to doctype names
     * WGDocument.TYPENAME_...
     * 
     * @param number
     */
    public static String doctypeNumberToName(int number) {
        return (String) docTypeNumberToName.get(new Integer(number));
    }

    /**
     *  Drops the cache of this document.
     * @throws WGAPIException 
     */
    public void dropCache() throws WGAPIException {
        
        // Refetch to ensure we have a managed cache instance, unless the doc is deleted whereas the cache is gone
        if (!_cache.isDeletedFlag()) {
            _cache = getDatabase().getDocumentCache(this);
        }
        _cache.dropCache();
        
    }

    protected void dispose() throws WGAPIException {

        disposed = true;

        //		Document will be dropped by WGDatabase. So it is not allowed to cache
        // anymore bc. noone will inform it to drop cache again.
        this.setCachingEnabled(false);
    }

    public WGDocument(WGDatabase db, WGDocumentCore core) throws WGAPIException {
        this(db, core, new WGDocumentObjectFlags());
    }
    
    /**
     * Constructor.
     * @param db
     * @param core
     * @param tempDuplicate
     * @throws WGAPIException 
     */
    public WGDocument(WGDatabase db, WGDocumentCore core, WGDocumentObjectFlags flags) throws WGAPIException {
        
        this.fastAccessKey = core.getFastAccessKey();
        this.uniqueId = buildDocumentKey(core, db);
        
        this.db = db;
        this.objectCreated = new Date();

        this.tempDuplicate = flags.isTempDuplicate();
        if(this.tempDuplicate){
        	/*
        	 * See #00005481 and #00004139
        	 * Generate uniqueId with qualifier to let doc have its own independent session context data
        	 * We use a random value here
        	 */
        	long r = new Random().nextLong();
        	this.uniqueId = this.uniqueId.withQualifier(String.valueOf(r));
        }
        
        this.cachingEnabled = (!tempDuplicate && core.isDataCacheable() && db.isDoctypeCacheable(getType()));
        this.temporary = (tempDuplicate ? true : core.isTemporary());
        this.dummy = flags.isDummy();
        
        
        this.setCore(core);
        
        // Unsaved cores are allowed to be kept so they survive the end of the session. (#00001412) 
        if (this.temporary && !core.isSaved()) {
            this.temporaryCore  = core;
        }
        
        this.originDatabase = core.getOriginDatabase();
        
        _cache = getDocumentCache();

        if (WGDatabase.VERBOSE_DOCUMENT_INSTANTIATION) {
            String className = this.getClass().getName();
            if (className.lastIndexOf(".") != -1) {
                className = className.substring(className.lastIndexOf(".") + 1);
            }
            WGFactory.getLogger().info("Document instantiation DB: " + db.getDbReference() + 
                    " - Class: " + className + 
                    " - Key: "+ this.uniqueId + (this.isTemporary() ? " (temporary)" : "") + 
                    " - Cache: " + (!getDatabase().getSessionContext().isCachingEnabled() ? "disabled" : !getDatabase().getSessionContext().isCacheWritingEnabled() ? "readonly" : "enabled") + 
                    " - Session: " + getDatabase().getSessionContext().hashCode());
        }

        // Update caches that should reflect unmodified backend values
        updateBackendCaches(core);
        
    }

    private Cache getDocumentCache() throws WGAPIException {
        
        // Dummies and uncacheable doc types have no public cache
        if (isDummy() || !getDatabase().isDoctypeCacheable(getType())) {
            return createDocumentCache();
        }
        
        // Temporary duplicates have no public cache
        if (tempDuplicate) {
            return createDocumentCache();
        }
        
        // Documents that are temporary but already saved (so they are no unsaved new instances)
        // Will never get refetched, therefor need no public cache
        if  (isTemporary() && isSaved()) {
            return createDocumentCache();
        }

        // Return a public cache from the database
        return db.getDocumentCache(this);
    }
    
    protected Cache createDocumentCache() {
        return new Cache();
    }


    /**
     * Returns all items as a map
     * 
     * @throws WGAPIException 
     */
    @SuppressWarnings("rawtypes")
	public Map getItems() throws WGAPIException{
    	HashMap<String,Object> map = new HashMap<String,Object>();
    	for(String itemname: getItemNames()){
    		map.put(itemname,  getItemValue(itemname));
    	}
    	return map;
    }

    /**
     * Returns all metas as a map
     * 
     * @throws WGAPIException 
     */
    @SuppressWarnings("rawtypes")
	public Map getMetas() throws WGAPIException{
    	HashMap<String,Object> map = new HashMap<String,Object>();
    	for(String name: getMetaNames()){
    		map.put(name,  getMetaData(name));
    	}
    	return map;
    }

    /**
     * Returns a text item. 
     * If the item is not of type text it returns the text representation (obj.toString())
     * 
     * @param strName Name of the item
     * @throws WGAPIException 
     */
    public String getItemText(String strName) throws WGAPIException {

        if (!hasItem(strName)) {
            return getDatabase().getNoItemBehaviour().getForGetItemText();
        }
        
        Object value = this.getItemValue(strName);
        if (value == null) {
            return null;
        }

        if (value instanceof String) {
            return (String) value;
        }
        if (value instanceof List) {
            List<?> valueList = (List<?>) value;
            if (valueList.size() >= 1) {
                return String.valueOf(valueList.get(0));
            }
            else {
                return null;
            }
        }
        else {
            return String.valueOf(value);
        }
    }

    /**
     * Returns an item's value.
     * If the item does not exist it returns null by default. You can modify this behaviour
     * by database option "NoItemValue". 
     * 
     * @param strName
     *            Name of the item
     * @throws WGAPIException 
     */
    public Object getItemValue(String strName) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        Object value = null;
        
        Map<String, Object> cache = null;
        boolean readFromCache = isCachingEnabled();
        if (readFromCache) {
            cache = getItemCache() ;
        }
        
        String cacheKey = "itemvalue:" + strName;
        if (cache != null && !isCoreRetrieved()) {
			value = cache.get(cacheKey);
        }

        if (value == null) {
            WGDocumentCore theCore = this.getCore();
            if (theCore.hasItem(strName)) {
                value = theCore.getItemValue(strName);
            }
            else {
                value = getDatabase().getNoItemBehaviour().getForGetItemValue();
            }
            
            value = cloneMutableObjects(value);
            
            if (cache != null && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                cache.put(cacheKey, nullPlaceholder(value));
            }
        }
        else if (value instanceof NullPlaceHolder) {
            value = null;
        }
        
        // Return only copies of mutable objects, so modifying them will not modify the cache
        value = cloneMutableObjects(value);

        return value;

    }

    protected Object cloneMutableObjects(Object value) {
        
        if (value == null) {
            return null;
        }
        
        else if (value instanceof Number ||
                value instanceof String ||
                value instanceof Boolean ||
                value instanceof Class) {
            // Do nothing. We have classical immutable objects.
        }
        
        else if (value instanceof ImmutableObject) {
            // Do nothing. The object was flagged to be immutable
        }
        
        // Clone objects with their native functionalities
        else if (value instanceof Date) {
            return new Date(((Date) value).getTime());
        }
        else if (value instanceof List) {
            List<Object> newList = new ArrayList<Object>();
            for (Object element : (List<?>) value) {
                newList.add(cloneMutableObjects(element));
            }
            value = newList;
        }
        else if (value instanceof Set) {
            Set<Object> newSet = new HashSet<Object>();
            for (Object element : (Set<?>) value) {
                newSet.add(cloneMutableObjects(element));
            }
            value = newSet;
        }
        else if (value instanceof Map) {
            Map<Object,Object> newMap = new HashMap<Object,Object>();
            for (Map.Entry<?,?> entry : ((Map<?,?>) value).entrySet()) {
                newMap.put(cloneMutableObjects(entry.getKey()), cloneMutableObjects(entry.getValue()));
            }
            value = newMap;
        }
        else {
            // We have some unknown object that may be mutable.
            // We will try to clone it using XStream
            getDatabase().verboseMutableCloning(value);
            Object clone = XStreamUtils.clone(value);
            if (clone != null) {
                value = clone;
            }
            else {
                WGFactory.getLogger().warn("Object of type '" + value.getClass().getName() + "' cannot be cloned from cache. Modifying this object will influence the cached values.");
            }
        }
        
        return value;
    }

    /**
     * Returns an items value(s) always as list. If the item is not multivalue, this
     * method returns a list containing only the single value.
     * 
     * @param name
     *            The item's name
     * @throws WGAPIException 
     */
    public List<?> getItemValueList(String name) throws WGAPIException {

        if (!hasItem(name)) {
            return getDatabase().getNoItemBehaviour().getForGetItemValueList();
        }
        
        Object value = this.getItemValue(name);
        if (value instanceof List) {
            return (List<?>) value;
        }
        else {
            List<Object> list = new ArrayList<Object>();
            if (value != null) {
                list.add(value);
            }
            return list;
        }

    }

    /**
     * Retrieves a number item.
     * If the item is not a number returns null.
     * 
     * @param strName
     *            Name of the item. 
     * @throws WGAPIException 
     */
    public Number getItemNumber(String strName) throws WGAPIException {

        if (!hasItem(strName)) {
            return null;
        }
        
        Object numberObj = this.getItemValue(strName);
        if (numberObj instanceof List) {
            List<?> numberList = (List<?>) numberObj;
            if (numberList.size() >= 1) {
                numberObj = numberList.get(0);
            }
            else {
                numberObj = null;
            }
        }

        if (numberObj != null && numberObj instanceof Number) {
            return (Number) numberObj;
        }
        else {
            return null;
        }
    }
    
    /**
     * Retrieves a boolean item.
     * If the item is not a boolean returns null.
     * 
     * @param strName
     *            Name of the item. 
     * @throws WGAPIException 
     */
    public Boolean getItemBoolean(String strName) throws WGAPIException {

        if (!hasItem(strName)) {
            return null;
        }
        
        Object booleanObj = this.getItemValue(strName);
        if (booleanObj instanceof List) {
            List<?> list = (List<?>) booleanObj;
            if (list.size() >= 1) {
            	booleanObj = list.get(0);
            }
            else {
            	booleanObj = null;
            }
        }

        if (booleanObj != null && booleanObj instanceof Boolean) {
            return (Boolean) booleanObj;
        }
        else {
            return null;
        }
    }

    /**
     * Retrieves the value of a date item. 
     * If the item is not of type date it returns null. 
     * 
     * @param strName
     *            Name of the item
     * @throws WGAPIException 
     */
    public Date getItemDate(String strName) throws WGAPIException {

        if (!hasItem(strName)) {
            return null;
        }
        
        Object dateObj = this.getItemValue(strName);
        if (dateObj instanceof List) {
            List<?> dateList = (List<?>) dateObj;
            if (dateList.size() >= 1) {
                dateObj = dateList.get(0);
            }
            else {
                dateObj = null;
            }
        }

        if (dateObj != null && dateObj instanceof Date) {
            return (Date) dateObj;
        }
        else {
            return null;
        }
    }

    /**
     * Sets an item to the given value. If the item doesn't exist, it is
     * created. Otherwise it's value gets overwritten.
     * 
     * @param strName
     *            Name of the item
     * @param value
     *            Value of the item. Accepted values are dependent to the
     *            database implementation
     * @return True, if the setting succeeded, false otherwise.
     * @throws WGAPIException  
     */
    public boolean setItemValue(String strName, Object value) throws WGAPIException {

        // check for NaN
        if (value != null) {
            if (value instanceof Number && ((Number)value).equals(new Double(Double.NaN))) {
                throw new WGIllegalArgumentException("NaN is not a valid value for item '" + strName + "'.");
            }            
        }
                
        boolean result = this.getCore().setItemValue(strName, this.parseValue(value));
        if (result == true) {
            setEdited(true);
        }
        return result;
    }

    /**
     * Method parseValue.
     * 
     * @param value
     * @return Object
     */
    private Object parseValue(Object value) {

        if (this.db.hasFeature(WGDatabase.FEATURE_COMPLEXVALUES)) {
            return value;
        }
        else if (value == null) {
            return "";
        } 
        else if (value instanceof Number || value instanceof String || value instanceof Boolean || value instanceof java.util.Date
                || value instanceof Collection) {
            return value;
        }
        else if (value instanceof Object[]) {
            return Arrays.asList((Object[]) value);
        }
        else {
            return value.toString();
        }

    }

    /**
     * Sets a meta data of this document. Since there are "direct" methods for
     * any metadata on the document subclasses, WGAPI users should not use this
     * method directly, but prefer the direct methods. E.g. Use
     * WGContent.setTitle("Home") instead of
     * WGDocument.setMetaData(WGContent.META_TITLE, "Home")
     * 
     * @param strName
     *            Name of the meta data. Use constants META_... of WGDocument
     *            and all descendant objects.
     * @param value
     *            Value to set this metadata
     * @return True, if the setting succeeded, false otherwise
     * @throws WGAPIException  
     */
    public boolean setMetaData(String strName, Object value) throws WGAPIException {

        MetaInfo info = getMetaInfo(strName);
        if (info == null) {
            throw new WGIllegalDataException("Metadata field '" + strName + "' not defined for document type '" + WGDocument.doctypeNumberToName(getType()) + "'");
        }
        
        double csVersion = getDatabase().getContentStoreVersion();
        if (info.getMinCsVersion() > csVersion) {
            throw new WGContentStoreVersionException("Metadata field " + strName, info.getMinCsVersion());
        }
        
        value = info.convertInput(this, value);            
 
        if (info.isExtdataInCsVersion(csVersion)) {
            writeExtensionData(EXTDATA_META_PREFIX + strName, value);
        }
        else {
            this.getCore().setMetaData(strName, value);
        }
        
        setEdited(true);
        return true;
    }



    /**
     * Saves the current document with it's modifications.
     * Special version only to be used inside WGAPI, e.g. for createClone.
     * 
     * @param lastModified Date of last modification to set (if this is supported by the implementation)
     * @return true, if the saving succeded, false otherwise.
     * @throws WGAuthorisationException
     * @throws WGValidationException
     * @throws LockException
     *         - instance of ResourceIsLockedException if a process try to modifiy a locked object without obtaining a lock  
     * @throws WGSystemException 
     * @throws WGIllegalArgumentException 
     */
    public boolean save(Date lastModified) throws WGAPIException {
        
        if (db.isSessionOpen() == false) {
            throw new WGClosedSessionException();
        }
        
        // If doc is dummy this is a noop
        if (isDummy()) {
            return true;
        }

        performSaveCheck();
        WGDocumentCore docCore = getCore();
        boolean isNewDoc = !docCore.isSaved();
        return innerSave(docCore, lastModified, isNewDoc);
        
    }

    protected synchronized boolean innerSave(WGDocumentCore docCore, Date lastModified, boolean isNewDoc) throws WGAPIException, WGClosedSessionException, WGBackendException {
            
        // We will update the last changed date only, when the core is from this database
        // and there are no pending changes in background
        boolean updateLastChanged = false;
        
        // Check origin. If from design provider we should not update lastChanged
        boolean designProviderCore = false;
        
        if (db.getDesignProvider() != null && db.getDesignProvider().isProviderCore(docCore)) {
            designProviderCore = true;
        }
        if (!designProviderCore) {
            updateLastChanged = !getDatabase().isDatabaseUpdatedInBackground();
        }
        
        boolean wasSaved = docCore.isSaved();
        getDatabase().verboseBackendAccess(WGOperationKey.OP_SAVE, getDocumentKeyObj());
        WGDatabaseRevision revision = docCore.save(lastModified);
        setEdited(false);

        // This block necessary, if a document goes from temporary to
        // persistent
        if (!this.tempDuplicate) {
            this.temporary = docCore.isTemporary();
            if (!this.temporary) {
                this.temporaryCore = null;
            }
        }

        DocumentContext docContext = getDocumentSessionContext();
        if (isNewDoc) {
            
            // In case, some parts of the key were generated on first save
            // Note: Implementations that do this MUST NOT support transactions on WGAPI level!
            WGDocumentKey oldId = uniqueId;
            uniqueId = buildDocumentKey(docCore, getDatabase());
            if (!oldId.equals(uniqueId)) {
                getDatabase().getSessionContext().remapDocumentContext(oldId, uniqueId);
            }
            
            // Replace doc and core in current document context, because there still may be "dummy" instances of these (B00005D32)
            docContext.setDocument(this);
            docContext.setCore(docCore);
        }

        if (!getDatabase().getSessionContext().isTransactionActive()) {
            dropCache();
            fastAccessKey = docCore.getFastAccessKey();
            db.documentSaved(this, getDocumentKeyObj(), isNewDoc, true, revision);
        }
        updateBackendCaches(docCore);
        for (SaveAction action : docContext.getAfterSaveActions()) {
            try {
                action.run(this);
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error executing after save action of " + getDocumentKey(), e);
            }
        }
        docContext.getAfterSaveActions().clear();
        
        return true;
        
    }
    


    /**
     * Performs a validation for saving the document at the current state, just like it actually was saved.
     * This method finishes quietly if the save check succeeded. Otherwise an adequate 
     * {@link WGAuthorisationException} is thrown.
     * For checking saving permissions without catching an exception see {@link #maySave()}.
     * @throws WGAPIException
     */
    public void performSaveCheck() throws WGAuthorisationException, WGAPIException, ResourceIsLockedException {
        // Check access level
        if (db.getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_AUTHOR) {
            if (this instanceof WGUserProfile) {
                if (!(db.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_READER && getDatabase().isReaderProfileCreation())) {
                    throw new WGAuthorisationException("You are not authorized to save this document", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);    
                }
            }
            else {
                throw new WGAuthorisationException("You are not authorized to save this document", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
            }
        }
        
        // check lock
        if (getLockStatus() == Lock.LOCKED_BY_FOREIGN_OBJECT) {
            throw new ResourceIsLockedException("The document you try to save is locked. You should obtain the lock first by using 'document.lock()'. Saving aborted.");
        }
    }
    
    /**
     * Tests if the current user is allowed to save the current document
     * For getting more details about failure reasons for save checks see {@link #performSaveCheck()}
     * @throws WGAPIException 
     */
    public boolean maySave() throws WGAPIException {
        
        try {
            performSaveCheck();
            return true;
        }
        catch (WGBackendException e) {
            throw e;
        }
        catch (WGAPIException e) {
            return false;
        }
        
    }
    
    /**
     * Tests if the current user is allowed to remove the current document
     * For getting more details about failure reasons for remove checks see {@link #performRemoveCheck(boolean)}
     * @throws WGAPIException 
     */
    public boolean mayRemove() throws WGAPIException {
        return mayRemove(false);
    }

    /**
     * Tests if the current user is allowed to remove the current document
     * For getting more details about failure reasons for remove checks see {@link #performRemoveCheck(boolean)}
     * @param deepCheck Perform an hierarchical check beyond the current document, which is more expensive but also 100% accurate. This is only effective for document types whose deletion will also delete dependent documents.
     * @throws WGAPIException 
     */
    public boolean mayRemove(boolean deepCheck) throws WGAPIException {
        
        try {
            performRemoveCheck(false);
            return true;
        }
        catch (WGBackendException e) {
            throw e;
        }
        catch (WGAPIException e) {
            return false;
        }
        
    }

    /**
     * Saved the current state of the document.
     * @throws WGAPIException
     */
    public boolean save() throws WGAPIException {
        return save(new Date());
    }

    /**
     * Evaluates a native expression with the current document as context.
     * 
     * @param expression
     *            The expression
     * @return The expression result.
     * @throws WGAPIException 
     */
    public Object evaluateExpression(String expression) throws WGAPIException {

        Object result = getCore().evaluateExpression(expression);
        return result;

    }

    /**
     * Returns the database that stores this document
     */
    public WGDatabase getDatabase() {
        return this.db;
    }

    /**
     * Returns the type of this document as constant WGDocument.TYPE_...
     */
    public abstract int getType();

    /**
     * Returns meta data about this document. Use constants META_... of
     * WGDocument and all descendant objects. Since there are "direct" methods
     * for any metadata on the document subclasses, WGAPI users should not use
     * this method directly, but prefer the direct methods. E.g. Use
     * WGContent.getTitle() instead of
     * WGDocument.getMetaData(WGContent.META_TITLE)
     * 
     * @return The meta data.
     * @throws WGAPIException 
     */
    public Object getMetaData(String name) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        name = name.toUpperCase();
        
        // try to retrieve from cache
        Object value = null;
        Map<String, Object> cache = null;
        MetaInfo metaInfo = getMetaInfo(name);
        boolean readFromCache = isCachingEnabled() && (metaInfo == null || metaInfo.isCacheable());
        if (readFromCache) {
            cache = getMetaCache();
        }        
        String cacheKey = "metavalue:" + name;
        if (cache != null && !isCoreRetrieved()) {
            value = cache.get(cacheKey);
        }
               
        // if not in cache retrieve from backend
        double csVersion = getDatabase().getContentStoreVersion();
        if (value == null) {
            if (metaInfo != null) {
                
                // Unsupported metadata for this cs version are returned as default value
                if (metaInfo.getMinCsVersion() > csVersion) {
                    value = metaInfo.getDefaultValue();
                }
                else {
                    value = retrieveMetaData(getCore(), metaInfo, csVersion);
                    value = metaInfo.convertOutput(this, value);
                }

            }
            
            // Fallback for unregistered metas: Passthrough to backend
            else {
                value = getCore().getMetaData(name);
            }

            // cache
			if (cache != null && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {    
                value = cloneMutableObjects(value);
            	cache.put(cacheKey, nullPlaceholder(value));
            }
        }
        else if (value instanceof NullPlaceHolder) {
            value = null;
        }

        value = cloneMutableObjects(value);

        return value;
    }

    protected Object retrieveMetaData(WGDocumentCore core, String name) throws WGAPIException {
        MetaInfo metaInfo = getMetaInfo(name);
        if (metaInfo != null) {
            return retrieveMetaData(core, metaInfo, getDatabase().getContentStoreVersion());
        }
        else {
            throw new WGIllegalDataException("Unknown metadata name to retrieve: " + name);
        }
        
    }
    
    protected Object retrieveMetaData(WGDocumentCore core, MetaInfo metaInfo, double csVersion) throws WGAPIException {
        
        if (metaInfo.isExtdataInCsVersion(csVersion)) {
            return getExtensionData(getExtdataMetaName(metaInfo));
        }
        else {
            return core.getMetaData(metaInfo.getName());
        }
        
        
    }

    /**
     * Returns the name that a metadata field has when stored as extension data
     * @param metaInfo The metaInfo of the metadata field
     */
    public static String getExtdataMetaName(MetaInfo metaInfo) {
        return EXTDATA_META_PREFIX + metaInfo.getName();
    }

    /**
     * @param value
     */
    protected Object nullPlaceholder(Object value) {
        return (value != null ? value : new NullPlaceHolder());
    }

    /**
     * Returns meta data about this document. Use constants META_... of
     * WGDocument and all descendant objects. Since there are "direct" methods
     * for any metadata on the document subclasses, WGAPI users should not use
     * this method directly, but prefer the direct methods. E.g. Use
     * WGContent.getTitle() instead of
     * WGDocument.getMetaData(WGContent.META_TITLE)
     * 
     * @return The meta data as list.
     * @throws WGAPIException 
     */
    public List<?> getMetaDataList(String name) throws WGAPIException {

        Object value = this.getMetaData(name);
        if (value instanceof List) {
            return (List<?>) value;
        }
        else {
            return new ArrayList<Object>(Collections.singletonList(value));
        }

    }

    /**
     * Returns the creation date of this document
     * @throws WGAPIException 
     */
    public java.util.Date getCreated() throws WGAPIException {
        return (Date) this.getMetaData(WGDocument.META_CREATED);
    }

    /**
     * Returns the last modification date of the document
     * @throws WGAPIException 
     */
    public java.util.Date getLastModified() throws WGAPIException {
        return (Date) this.getMetaData(WGDocument.META_LASTMODIFIED);
    }

    /**
     * Returns the core implementation object for this document.
     * 
     * @return Returns a WGDocument
     * @throws WGAPIException 
     */
    public WGDocumentCore getCore() throws WGAPIException {
        
        if (!db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        WGDocumentCore core = (WGDocumentCore) getDocumentSessionContext().getCore();
        if (core == null) {
            if (this.fastAccessKey != null) {
                getDatabase().verboseBackendAccess(WGOperationKey.OP_DOCUMENT_CORE_FASTACCESS, this.uniqueId);
                core = this.db.getCore().fastAccess(getType(), this.fastAccessKey);
                
                // Look if design document got renamed
                if (core != null && this instanceof WGDesignDocument) {
                    WGDocumentKey newCoreKey = WGDocument.buildDocumentKey(core, getDatabase());
                    if (!newCoreKey.equals(uniqueId)) {
                        // Document got renamed. Drop that one, including it's fast access key. Retrieve new.
                        core = null;
                        fastAccessKey = null;
                    }
                }
            }

            if (core == null) {
                
                // For temporary documents (like unsaved ones) we may return a kept temporary core
                if (this.temporary && this.temporaryCore != null) {
                    return this.temporaryCore;
                }
                else if (isDummy()) {
                    WGFakeDocument fakeDoc = new WGFakeDocument(db, getType());
                    fakeDoc.setDeleted(true);
                    return fakeDoc;
                }
                
                getDatabase().verboseBackendAccess(WGOperationKey.OP_DOCUMENT_CORE, this.uniqueId);
                core = this.retrieveCore();
                if (core != null) {
                    this.fastAccessKey = core.getFastAccessKey();
                }
            }

            if (core != null) {
                setCore(core);
                updateBackendCaches(core);
            }
            else {
                // Could not find document. Mark document object deleted, throw exception                
                setDeleted(true);
                throw new WGDeletedException(getDocumentKey());
            }
        }
        return core;
    }

    /**
     * Callback method that gives the document the opportunity to update backend caches,
     * i.e. caches that should reflect unmodified backend data of the document and not modified values.
     * @param core
     */
    protected void updateBackendCaches(WGDocumentCore core) {
    }

    /**
     * Returns if the current documents holds a document core (i.e. a backend document)
     * @throws WGClosedSessionException 
     */
    public boolean isCoreRetrieved() throws WGClosedSessionException {
        return getDocumentSessionContext().isCoreRetrieved();
    }

    /**
     * Sets the document core.
     * 
     * @param core
     *            The doc to set
     */
    protected void setCore(WGDocumentCore core) {
        if (core != null) {
            getDatabase().getSessionContext().addFetchedCore(this, core);
            core.setWGDocument(this);
        }
    }

    /**
     * Tests, if the given native expression result means "true" for this
     * database implementation.
     */
    public boolean resultIsTrue(Object exprResult) {

        if (exprResult == null) {
            return false;
        }

        return db.getCore().resultIsTrue(exprResult, this);
    }

    /**
     * Tests, if the given native expression result means "false" for this
     * database implementation.
     * 
     * @param exprResult
     */
    public boolean resultIsFalse(Object exprResult) {

        if (exprResult == null) {
            return false;
        }

        return db.getCore().resultIsFalse(exprResult, this);
    }

    /**
     * Tests if this document object is temporary and will be discarded after
     * the session. This doesn't mean that the database document represented by
     * this object will be deleted. It just means that there will be another
     * document object for the database document in every session that accesses
     * it.
     */
    public boolean isTemporary() {

        return temporary;

    }

    /**
     * Returns if this document has been deleted
     * @param forceBackendCheck Specify true if you want to enforce a check on the database backend. Otherwise WGAPI may choose to check cached deletion flags.
     * @throws WGAPIException 
     */
    public boolean isDeleted(boolean forceBackendCheck) throws WGAPIException {

        boolean result = false;
        
        if (!forceBackendCheck && !db.isDeletionCheck() && (isCachingEnabled() || !getDatabase().isSessionOpen())) {
            return isDeletedFlag();
        }
        
        try {
            if (isDeletedFlag() || getCore() == null) {
                return true;
            }
            
            if (getCore().isDeleted()) {
                // Try again with freshly fetched core
                dropCore();
                return (isDeletedFlag() || getCore() == null || getCore().isDeleted());
            }
            else {
                return false;
            }
        }
        catch (WGDeletedException e) {
            return true; // No need to try again when core fetching failed. Return immediately.
        }
        catch (Exception exc) {
            WGFactory.getLogger().error("Exception determining deletion state of " + getDocumentKey(), exc);
            return true;
        }

    }
    
    /**
     * Returns if this document has been deleted
     * This method may choose to only check cached deletion flags for performance reasons. Use {@link #isDeleted(boolean)} to force real backend check.
     * @throws WGAPIException 
     */
    public boolean isDeleted() throws WGAPIException {
        return isDeleted(false);
    }

    protected boolean isDeletedFlag() {
        if (_cache == null) { // May happen on WGDocument initialisation
            return false;
        }
        return _cache.isDeletedFlag();
    }

    /**
     * Returns a list of file attachment names for this document
     * @throws WGAPIException 
     */
    public java.util.List<String> getFileNames() throws WGAPIException {
        
        List<String> fileNames = null;
        if (isCachingEnabled() && !isCoreRetrieved()) {
            fileNames = getFileNamesCache();
        }
        
        if (fileNames == null) {
            fileNames = Collections.unmodifiableList(this.getCore().getFileNames());
            if (getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                setFileNamesCache(fileNames);
            }
        }
        
        return new ArrayList<String>(fileNames);
        
    }
    
    /**
     * Convenience method to remove all file attachments from the document at once
     * @throws WGAPIException 
     */
    public void removeAllFiles() throws WGAPIException {
        Iterator<String> names = getFileNames().iterator();
        while (names.hasNext()) {
            String name = (String) names.next();
            removeFile(name);
        }
    }
    
    /**
     * Copies all attachments of the parameter document to the current document. This variant only copies the files themselves, not their extension data. 
     * @param doc The document to get attachments from
     * @throws IOException 
     * @throws WGAPIException 
     */
    public void attachAllFiles(WGDocument doc) throws IOException, WGAPIException {
        attachAllFiles(doc, false);
    }
    
    /**
     * Copies all attachments of the parameter document to the current document
     * @param doc The document to get attachments from
     * @param copyExtensionData Specify true to also copy extension data from source to target. The sources extension data will overwrite eventual automatic annotations done on the attaching process.
     * @throws IOException 
     * @throws WGAPIException 
     */
    public void attachAllFiles(WGDocument doc, boolean copyExtensionData) throws IOException, WGAPIException {
        
        Iterator<String> names = doc.getFileNames().iterator();
        while (names.hasNext()) {
            String name = (String) names.next();
            TemporaryFile tempFile = new TemporaryFile(name, doc.getFileData(name), WGFactory.getTempDir());            
            attachFile(tempFile.getFile());
            
            // Copy extension data
            if (copyExtensionData && doc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 &&
                    getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
                
                WGFileMetaData sourceMeta = doc.getFileMetaData(name);
                WGFileMetaData targetMeta = getFileMetaData(name);
                sourceMeta.pushData(targetMeta);
            }
            
            tempFile.deleteOnEviction(getDatabase().getSessionContext());
        }
        
    }



    /**
     * Returns the data of a file attachment as input stream
     * Retrieving the data of a recently attached file while the document is not yet saved is not supported and may produce unknown results.
     * @param strFile
     *            The name of the attachment
     * @throws WGAPIException 
     */
    public java.io.InputStream getFileData(String strFile) throws WGAPIException {
        return this.getCore().getFileData(strFile);
    }
    
    /**
     * Returns the meta data object for the given filename
     * @param strFile
     * @return WGFileMetaData
     * @throws WGAPIException
     */
    public WGFileMetaData getFileMetaData(String strFile) throws WGAPIException {

        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        Object value = null;
        
        Map<String, Object> cache = null;
        boolean readFromCache = isCachingEnabled();
        if (readFromCache) {
            cache = getFileMetaCache();
        }
        
        String cacheKey = "fileMetaData:" + strFile;
        if (cache != null && !isCoreRetrieved()) {
            value = cache.get(cacheKey);
        }

        if (value == null) {
            WGDocumentCore theCore = this.getCore();
            value = theCore.getFileMetaData(strFile);
            if (value != null) {
                ((WGFileMetaData) value).setContext(this);
            }
            if (cache != null && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                cache.put(cacheKey, nullPlaceholder(value));
            }
        }
        else if (value instanceof NullPlaceHolder) {
            value = null;
        }

        if (value != null) {
            WGFileMetaData metaData = ((WGFileMetaData) value).createClone();
            metaData.setContext(this);
            return metaData;
        } else {
            return null;
        }
    }
    
    /**
     * Returns the (primary) mime-type for the file of the given name.
     * This method will try to use a previously stored mimetype from the files {@link WGFileMetaData}. If that does not exist it will fallback to mimetype determination based on file data. 
     * @param strFile
     * @return String
     * @throws WGAPIException
     */
    public String getFileMimeType(String strFile) throws WGAPIException{
        
        // Return stored mimetype extdata
        WGFileMetaData fileMetaData = getFileMetaData(strFile);
        if (fileMetaData != null) {
            String mimeType = fileMetaData.getMimeType();
            if (mimeType != null) {
                return mimeType;
            }
        }
        
        // Determine mimetype by data
    	Iterator<String> it = getFileMimeTypes(strFile).iterator();
    	return it.next();
    }
    
    /**
     * Returns all mime-types for the file of the given name.
     * This method always determines mimetypes based on the file data. For a more performant determination use {@link #getFileMimeType(String)}.
     * @param strFile
     * @return Collection
     * @throws WGAPIException
     */
    public Collection<String> getFileMimeTypes(String strFile) throws WGAPIException{
    	InputStream in = getFileData(strFile);
    	@SuppressWarnings("unchecked")
        Collection<String> mimeTypes = MimeUtil.getMimeTypes(in);
		if(in != null){
			try {
				in.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
    	return mimeTypes;
    }
    
    /**
     * Returns the contents of a file attachment in text format. Only valid if
     * the file contains textual information. This tries to read the file in platform encoding.
     * 
     * @param strFile
     *            The file attachment
     * @return A reader object for the text information.
     * @deprecated Use {@link #getFileText(String)} which allows specifying an encoding
     * @throws WGAPIException 
     */
    public java.io.Reader getFileText(String strFile) throws WGAPIException {
        return new java.io.InputStreamReader(this.getCore().getFileData(strFile));
    }
    
    /**
     * Returns the contents of a file attachment in text format. Only valid if
     * the file contains textual information.
     * 
     * @param strFile
     *              The file attachment
     * @param encoding
     *              The text encoding to use when interpreting the file data as text           
     * @return A reader object for the text information.
     * @throws WGAPIException 
     * @throws UnsupportedEncodingException 
     */
    public java.io.Reader getFileText(String strFile, String encoding) throws WGAPIException, UnsupportedEncodingException {
        if (encoding == null) {
            encoding = "UTF-8";
        }
        return new java.io.InputStreamReader(this.getCore().getFileData(strFile), encoding);
    }
 
    /**
     * Returns the size of an attachment in bytes. Returns -1 if a file of the given name is not available.
     * 
     * @param strFile
     *            The file to retrieve size information for
     * @throws WGAPIException 
     */
    public int getFileSize(String strFile) throws WGAPIException {
        return this.getCore().getFileSize(strFile);
    }

    /**
     * Provides the best (most specific) information about the last modified date of an individual file attachment.
     * If file metadata is available the date is retrieved from there.
     * Else the last modified date of the whole container is returned.
     * @param file Name of the file attachment
     * @return the most accurate last modified time for that file that is available
     * @throws WGAPIException
     */
    public Date getFileLastModified(String file) throws WGAPIException {
        if (hasFileMetadata()) {
            try {
                WGFileMetaData md = getFileMetaData(file);
                if (md != null && md.getLastmodified() != null) {
                    return md.getLastmodified();
                }
            }
            catch (Exception e) {
                WGFactory.getLogger().error("Error retrieving file last modified from file metadata, file '" + file + "', doc " + getDocumentKey(), e);
            }
        }

        return getLastModified();

    }
    
    protected void dropCore(boolean untimelyDispose, boolean force) throws WGClosedSessionException {

        if (untimelyDispose && isEdited() && !force) {
            return;
        }
        
        getDocumentSessionContext().dropCore(untimelyDispose);
        temporaryCore = null;
    }

    protected DocumentContext getDocumentSessionContext() throws WGClosedSessionException {
        WGSessionContext sessionContext = getDatabase().getSessionContext();
        if (sessionContext == null) {
            throw new WGClosedSessionException();
        }
        
        return sessionContext.getOrCreateDocumentContext(this);
    }

    /**
     * Drops the retrieved document core (i.e. backend document), if there is one. This also discards any modifications done to this document that are not saved.
     * @throws WGClosedSessionException 
     */
    public void dropCore() throws WGClosedSessionException {
        dropCore(true, true);
    }

    protected void dropRelations() {
        _cache.dropRelations();
    }

    /**
     * Returns a map as persistent store for the implementation of this
     * document. An implementation can store data here that is not discarded
     * when a session closes. Not for external use outside the WGAPI.
     * 
     * @return Returns a HashMap
     */
    public Map<String,Object> getPersistentStore() {
        return _cache.getPersistentStore();
    }

    /**
     * Retrieves the name information for the given document core. Only works
     * with areas, content (unique name), css/js modules, content types, file
     * containers, languages, struct entries (struct keys) and TML modules
     * 
     * @param core
     * @throws WGAPIException 
     */
    public static String getName(WGDocumentCore core) throws WGAPIException {

        int type = core.getType();
        if (type == WGDocument.TYPE_AREA) {
            return (String) core.getMetaData(WGArea.META_NAME);
        }
        else if (type == WGDocument.TYPE_CONTENT) {
            return (String) core.getMetaData(WGContent.META_UNIQUE_NAME);
        }
        else if (type == WGDocument.TYPE_CSSJS) {
            return (String) core.getMetaData(WGCSSJSModule.META_NAME);
        }
        else if (type == WGDocument.TYPE_CONTENTTYPE) {
            return (String) core.getMetaData(WGContentType.META_NAME);
        }
        else if (type == WGDocument.TYPE_FILECONTAINER) {
            return (String) core.getMetaData(WGFileContainer.META_NAME);
        }
        else if (type == WGDocument.TYPE_LANGUAGE) {
            return (String) core.getMetaData(WGLanguage.META_NAME);
        }
        else if (type == WGDocument.TYPE_STRUCTENTRY) {
            return (String) core.getMetaData(WGStructEntry.META_KEY);
        }
        else if (type == WGDocument.TYPE_TML) {
            return (String) core.getMetaData(WGTMLModule.META_NAME);
        }
        else {
            return null;
        }
    }

    /**
     * Tests if the document has an item of this name
     * @throws WGAPIException 
     */
    public boolean hasItem(String itemName) throws WGAPIException {
        
        Boolean result = null;
        boolean readFromCache = isCachingEnabled();
        Map<String,Object> cache = null;
        if (readFromCache) {
            cache = getItemCache();
        }
        
        String cacheKey = "hasitem:" + itemName;
        
        if (readFromCache && !isCoreRetrieved()) {
            result = (Boolean) cache.get(cacheKey);
        }
        
        if (result == null) {
            result = new Boolean(getCore().hasItem(itemName));
            if (readFromCache && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                cache.put(cacheKey, result);
            }
        }
        
        return result.booleanValue();
    }

    /**
     * Retrieves a list of the names of all content items on this document
     * @throws WGAPIException 
     */
    public List<String> getItemNames() throws WGAPIException {
        return this.getCore().getItemNames();
    }

    /**
     * Removes an item from the document
     * 
     * @param name
     *            Name of the item
     * @throws WGAPIException           
     */
    public void removeItem(String name) throws WGAPIException {
        this.getCore().removeItem(name);
        setEdited(true);
    }

    /**
     * Attaches a file to the document.
     * The file will be stored at the document with the same name that it has in file system.
     * 
     * @param file
     *            The file on disk. 
     * @param additionalAnnotators Additional annotators to annotate the attached file with image information or custom fields. Specify null to use no additional annotators.
     * @throws WGAPIException
     */
    public boolean attachFile(final File file, List<WGFileAnnotator> additionalAnnotators) throws WGAPIException {
        
        WGFileConverter converter = getDatabase().getFileConverter();
        if(converter!=null){
			try {
				converter.convert(file);
			} catch (IOException e) {}
        }
        
        // Run annotators
        WGFileMetaData meta = new WGFileMetaData(this, getDatabase().convertFileNameForAttaching(file.getName()), file.length(), new Date(), new Date(), null, null, new HashMap<String, Object>());        
        getDatabase().annotateMetadata(file, meta, additionalAnnotators);
        
        return innerAttachFile(file);
    }

    public boolean innerAttachFile(final File file) throws WGAPIException, WGClosedSessionException {
        // Attach file
        boolean result = getCore().attachFile(file);
        if (result == true) {
            setEdited(true);
        }
        
        // Set primary file if yet unset
        if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            String primary = getPrimaryFileName();
            if (primary == null) {
                setPrimaryFileName(file.getName());
            }
        }
        return result;
    }
    
    /**
     * Attaches a file to the document without running any annotators
     * @param file
     *            The file on disk. 
     * @throws WGAPIException
     */
    public boolean attachFileWithoutAnnotation(final File file) throws WGAPIException {
        return innerAttachFile(file);
    }

    /**
     * Attaches a file to the document.
     * The file will be stored at the document with the same name that it has in file system.
     * 
     * @param file
     *            The file on disk. 
     * @throws WGAPIException
     */
    public boolean attachFile(File file) throws WGAPIException {
        return attachFile(file, null);
    }
    
    /**
     * Renames a file exception on this document
     * @param oldFileName The current file name
     * @param newFileName The new file name
     * @throws WGAPIException
     * @throws IOException
     */
    public void renameFile(final String oldFileName, final String newFileName) throws WGAPIException, IOException {
    	
    	if (!hasFile(oldFileName)) {
    		throw new WGIllegalArgumentException("Unable to rename file '" + oldFileName + "'. There is no file with that name attached on this document.");
    	}
    	
        if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            String convertedName = getDatabase().convertFileNameForAttaching(oldFileName);
            String primary = getPrimaryFileName();
            if (convertedName.equals(primary)) {
                setPrimaryFileName(newFileName);
            }
        }
    	
    	if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1) {
    	    getCore().renameFile(oldFileName, newFileName);
    	    DataSource ds = new DataSource() {

                @Override
                public String getContentType() {
                    return "application/octet-stream";
                }

                @Override
                public InputStream getInputStream() throws IOException {
                    try {
                        return new BufferedInputStream(getFileData(oldFileName));
                    }
                    catch (WGAPIException e) {
                        throw new IOException(e);
                    }
                }

                @Override
                public String getName() {
                    return newFileName;
                }

                @Override
                public OutputStream getOutputStream() throws IOException {
                    throw new IOException("This data source does not provide an output stream");
                }
                
            };
            getDatabase().annotateMetadata(ds, getFileMetaData(newFileName), null);
    		setEdited(true);
    	} 
    	else {
    		// backend does not support optimized file handling
    		// detach and attach the file for renaming
    		attachFile(getFileData(oldFileName), newFileName);
    		removeFile(oldFileName);
    	}
    	
    	
    }
    
    /**
     * Takes data from an input stream and attaches the data as file
     * under the given name.
     * This actually is a wrapper for {@link #attachFile(File)} that creates a temporary file of the desired name
     * and attaches this. This is becaues some WGAPI implementations cannot attach a data stream directly but need
     * a physical file.
     * @param stream The input stream providing the file data
     * @param filename The file name under which the file will be attached
     * @throws WGAPIException 
     * @throws IOException 
     */
    public boolean attachFile(InputStream stream, String filename) throws WGAPIException, IOException {
        
        TemporaryFile tempFile = new TemporaryFile(filename, stream, WGFactory.getTempDir());
        attachFile(tempFile.getFile());
        tempFile.deleteOnEviction(getDatabase().getSessionContext());
        return true;
        
    }
    
    /**
     * Takes data from an input stream and attaches the data as file, without running any annotators
     * under the given name.
     * This actually is a wrapper for {@link #attachFileWithoutAnnotation(File)} that creates a temporary file of the desired name
     * and attaches this. This is becaues some WGAPI implementations cannot attach a data stream directly but need
     * a physical file.
     * @param stream The input stream providing the file data
     * @param filename The file name under which the file will be attached
     * @throws WGAPIException 
     * @throws IOException 
     */
    public boolean attachFileWithoutAnnotation(InputStream stream, String filename) throws WGAPIException, IOException {
        
        TemporaryFile tempFile = new TemporaryFile(filename, stream, WGFactory.getTempDir());
        attachFileWithoutAnnotation(tempFile.getFile());
        tempFile.deleteOnEviction(getDatabase().getSessionContext());
        return true;
        
    }

    /**
     * Removes a file attachment from the document
     * Note: When this method is called you should save the document before attempting to attach a
     * new file of the same name. Some WGAPI implementations will throw exceptions when you remove
     * and attach files of the same name without saving in between.
     * 
     * @param name
     *            Name of the file attachment
     * @throws WGAPIException 
     */
    public boolean removeFile(String name) throws WGAPIException {
        
        String convertedName = getDatabase().convertFileNameForAttaching(name);
        if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            String primary = getPrimaryFileName();
            if (convertedName.equals(primary)) {
                setPrimaryFileName(null);
            }
        }
        
        boolean result = getCore().removeFile(name);
        setEdited(true);
        return result;
    }

    protected abstract boolean remove(WGDocument deletionRoot) throws WGAPIException;
    
    /**
     * Remove the current document permanently from the database.
     * @return false if removal does not apply to this document, true otherwise
     * @throws WGAPIException
     */
    public final boolean remove() throws WGAPIException {
        
        if (db.isSessionOpen() == false) {
            throw new WGClosedSessionException();
        }
        
        // If doc is dummy this is a noop
        if (isDummy()) {
            return false;
        }
        
        return remove(this);
    }
    
    /**
     * Shared function to delete the current document. Calls {@link #performRemoveCheck()} then executes the removal
     * @param deletionRoot The root of the current deletion operation. Is != this if the document is removed bc. some parent document gets removed.
     * @param performRemoveCheck Controls if the remove check should be done here. False may be given if the document already performed this test earlier.
     * @return true
     * @throws WGAPIException
     */
    protected synchronized boolean innerRemove(WGDocument deletionRoot, boolean performRemoveCheck) throws WGAPIException {
        
        // We do this only if this is the deletion root
        // Otherwise it is the resposibility of the deletion root itself to perform deep remove checks before removing anything
        if (performRemoveCheck && this == deletionRoot) {
            performRemoveCheck(true, deletionRoot);
        }
        
        return db.remove(this);

    }

    /**
     * Performs a validation for removing the document at the current state, just like it actually was removed.
     * This method finishes quietly if the removal check succeeded. Otherwise an adequate 
     * {@link WGAuthorisationException} is thrown.
     * For checking remove permissions without catching an exception see {@link #mayRemove(boolean)}.
     * This remove check performs no exhaustive "deep" check, like can be triggered via {@link #performRemoveCheck(boolean)}.
     * 
     * @throws WGAuthorisationException
     * @throws WGAPIException 
     */
    public final void performRemoveCheck() throws WGAuthorisationException, WGAPIException {
        
        if (db.isSessionOpen() == false) {
            throw new WGClosedSessionException();
        }
        
        performRemoveCheck(false);
    }

    /**
     * Performs a validation for removing the document at the current state, just like it actually was removed.
     * This method finishes quietly if the removal check succeeded. Otherwise an adequate 
     * {@link WGAuthorisationException} is thrown.
     * For checking remove permissions without catching an exception see {@link #mayRemove(boolean)}.
     * @param deepCheck Perform an hierarchical check beyond the current document, which is more expensive but also 100% accurate. This is only effective for document types whose deletion will also delete dependent documents. 
     * 
     * @throws WGAuthorisationException
     * @throws WGAPIException 
     */
    protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAuthorisationException, WGAPIException {
        if (!db.getSessionContext().getUserAccess().mayDeleteDocuments()) {
            // Deletion right flag does not apply to draft contents, inactive structs, and contents in project mode (#00001184) 
            if (!(this instanceof WGContent && ((WGContent) this).getStatus().equals(WGContent.STATUS_DRAFT)) && 
                !(this instanceof WGStructEntry &&((WGStructEntry) this).isActive() == false) &&
                !(this instanceof WGContent && getDatabase().isProjectMode() && db.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_AUTHOR)) {
                throw new WGAuthorisationException("You are not authorized to delete active documents in this database!", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
            }
        }
        
        
    }
    
    /**
     * Performs a validation for removing the document at the current state, just like it actually was removed.
     * This method finishes quietly if the removal check succeeded. Otherwise an adequate 
     * {@link WGAuthorisationException} is thrown.
     * For checking remove permissions without catching an exception see {@link #mayRemove(boolean)}.
     * @param deepCheck Perform an hierarchical check beyond the current document, which is more expensive but also 100% accurate. This is only effective for document types whose deletion will also delete dependent documents.
     *
     * @throws WGAuthorisationException
     * @throws WGAPIException 
     */
    public final void performRemoveCheck(boolean deepCheck) throws WGAuthorisationException, WGAPIException {
        performRemoveCheck(deepCheck, this);
    }

    /**
     * Creates a clone of this document in another database
     * 
     * @param db
     *            The target database
     * @param ref
     *            A reference document in the target database. For struct
     *            entries, this is the parent entry in the target db. For
     *            contents this is the struct entry in the target db.
     * @return The clone
     * @throws WGAPIException
     */
    public abstract WGDocument createClone(WGDatabase db, WGDocument ref) throws WGAPIException;

    /**
     * Tests if caching for this document is enabled. If so, the document caches
     * item and metadata values.
     */
    public boolean isCachingEnabled() {
        
        if (!cachingEnabled) {
            return false;
        }
         
        if (temporary) {
            return false;
        }
        
        WGSessionContext sessionContext = getDatabase().getSessionContext();
        if (sessionContext == null) {
            return false;
        }
        
        if (!sessionContext.isCachingEnabled()) {
            return false;
        }
        
        return true;
    }

    /**
     * Controls if item/meta caching is enabled for this document.
     * 
     * @param b
     */
    public void setCachingEnabled(boolean b) {
        cachingEnabled = b;
    }

    /**
     * Returns if the document has been saved to the database. False means that it is a new document not yet persisted.
     * @throws WGAPIException 
     */
    public boolean isSaved() throws WGAPIException {
        if (isCoreRetrieved()) {
            return getCore().isSaved();
        }
        else if (temporaryCore != null) {
            return temporaryCore.isSaved();
        }
        else {
            return true;
        }
        
    }

    /**
     * Gets the names list for past authors of this document. The indexing of
     * the given list corresponds to the indexes of the lists given by
     * getPastEditDates().
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public List<String> getPastAuthors() throws WGAPIException {
        return (List<String>) getMetaDataList(META_PASTAUTHORS);
    }

    /**
     * Gets the list of dates, when this document was edited in the past. The
     * indexing of the given list corresponds to the indexes of the lists given
     * by getPastAuthors().
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public List<Date> getPastEditDates() throws WGAPIException {
        return (List<Date>) getMetaDataList(META_PASTEDITDATES);
    }

    /**
     * Returns the revision number of this document.
     * @throws WGAPIException 
     */
    public int getRevision() throws WGAPIException {
        return ((Integer)getMetaData(META_REVISION)).intValue();
    }

    /**
     * Returns all meta names that are supported by the current document type.
     * @throws WGSystemException 
     */
    public List<String> getMetaNames() throws WGSystemException {
        Field[] fields = getClass().getFields();
        List<String> result = new ArrayList<String>();
        for (int i = 0; i < fields.length; i++) {
            Field field = fields[i];
            if (field.getName().indexOf("META_") == 0) {
                try {
                    String metaName = (String) field.get(this);
                    result.add(metaName);
                }
                catch (IllegalAccessException e) {
                    throw new WGSystemException("Unable to retrieve metanames for class '" + getClass().getName() + "'.", e);
                }
            }
        }
        return result;
    }

    /**
     * Returns the native backend object of this document if there is one.
     * @throws WGAPIException 
     */
    public Object getNativeObject() throws WGAPIException {

        if (!db.isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        return getCore().getNativeObject();
    }

    /**
     * String used in WGStructEntry fields for edit access to symbolize that noone may edit this document.
     */
    public static final String NOONE_ALLOWED = "%none";
    
    
    /**
     * Restriction string for page positioning: No restriction set here, retrieve from "parent"
     */
    public static final String PAGERESTRICTION_UNSPECIFIED = "unspecified";

    /**
     * Restriction string for page positioning: No pages allowed
     */
    public static final String PAGERESTRICTION_NONE = "none";

    /**
     * Restriction string for page positioning: Any page allowed
     */
    public static final String PAGERESTRICTION_ANY = "any";

    /**
     * Restriction string for page positioning: Pages of special content types allowed
     */
    public static final String PAGERESTRICTION_FIXEDTYPES = "fixDocTypes";

    /**
     * Returns if the document was edited and the current state of the document is not yet saved.
     * @throws WGClosedSessionException 
     */
    public boolean isEdited() throws WGClosedSessionException {
        return getDocumentSessionContext().isEdited();
    }

    /**
     * @param b
     * @throws WGClosedSessionException 
     */
    protected void setEdited(boolean b) throws WGClosedSessionException {
        getDocumentSessionContext().setEdited(b);
    }
    
    /**
     * Manually sets this document as being edited.
     * This is useful for making the document show up in {@link WGSessionContext#getEditedDocuments()}.
     * @throws WGClosedSessionException 
     */
    public void markEdited() throws WGClosedSessionException {
        setEdited(true);
    }

    /**
     * Pushed the data of this document to another document of the same type.
     * This method is allowed to save the target document if necessary, but is not required to.
     * @param doc The target document receiving the data.
     * @throws WGAPIException If something goes wrong
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        // Push extension data fields
        if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && doc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            // Clear all extension data with exception of !mayPushExtData
        	// See #00005182
            if (doc.getExtensionDataNames().size() > 0) {

                Iterator<String> names = getExtensionDataNames().iterator();
                while (names.hasNext()) {
                    String name = (String) names.next();
                    if(!mayPushExtData(name))
                    	continue;
                    doc.removeExtensionData(name);
                }
                
                if (!doc.getDatabase().hasFeature(WGDatabase.FEATURE_DIRECT_ENTITY_READDING)) {
                    doc.save();
                }
            }
        
            // Copy extension data
            Iterator<String> extDataNames = getExtensionDataNames().iterator();
            String extDataName;
            while (extDataNames.hasNext()) {
                extDataName = (String) extDataNames.next();
                if(!mayPushExtData(extDataName))
                	continue;
                try {
                    Object value = getExtensionData(extDataName);
                    if (value instanceof List) {
                        value = new ArrayList<Object>((List<?>) value);
                    }
                    doc.writeExtensionData(extDataName, value);
                }
                catch (WGIllegalDataException e) {
                    WGFactory.getLogger().warn("Cannot copy item '" + extDataName + "' because of unrestorable data", e);
                }
            }
        }
        
    }

    /**
     * checks if extension data may be "pushed". Returns true per default but may be overwritten by implementations
     * @param extName
     * @return true
     */
    public boolean mayPushExtData(String extName){
    	return true;
    }
    
    /**
     * @param b
     */
    protected void setDeleted(boolean b) {
        setDeletedFlag(b);
        
    }

    private boolean dummy = false;

    private boolean disposed = false;

    

    /**
     * Returns, if this document is a "dummy content document", i.e. this document is nowhere stored in the database
     * but is only temporarily created to provide a content context for the current request.
     * @return boolean
     */
    public boolean isDummy() {
    	return dummy;
    }

    /**
     * Returns the time when the document core was retrieved in the current session. Is null if there has no core been retrieved yet.
     * @throws WGClosedSessionException 
     */
    public Date getCoreRetrieved() throws WGClosedSessionException {
        return new Date(getDocumentSessionContext().getCoreRetrievalTime());
    }

    /**
     * Returns the time that this document object was created
     */
    public Date getObjectCreated() {
        return objectCreated;
    }
    
    /**
     * Tests if a file of this name is attached to the document. 
     * This method should also consider file name conversions that take place when attaching.
     * If this method returns true then attaching a file of this name will throw an error.
     * Vice versa if this method returns false it is safe to attach a file of this name,.
     * @param name The name of file to search
     * @return true, if a file of this name (maybe in a converted form) exists
     * @throws WGAPIException 
     */
    public boolean hasFile(String name) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() >= 4.1) {
            return (getFileMetaData(name) != null);
        }
        
        String convertedFileName = db.convertFileNameForAttaching(name);
        if (convertedFileName == null) {
            return false;
        }
        
        return getCore().hasFile(name);
        
    }
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#lock(de.innovationgate.webgate.api.locking.LockOwner)
     */
    public void lock(LockOwner owner) throws WGAPIException {
        getDatabase().getLockManager().obtainLock(this, owner);
    }
 
    
    /**
     * locks the document for the current sessionContext  
     * @throws WGAPIException 
     */
    public void lock() throws WGAPIException {        
        WGSessionContext sessionContext = getDatabase().getSessionContext();
        lock((LockOwner)sessionContext);
    }

    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#unlock(de.innovationgate.webgate.api.locking.LockOwner)
     */
    public void unlock(LockOwner owner) {      
        getDatabase().getLockManager().releaseLock(this, owner);
    }
    
    /**
     * unlocks the document for the current sessioncontext     
     */
    public void unlock() {
       WGSessionContext sessionContext = getDatabase().getSessionContext();
       unlock(sessionContext);
    }    
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#getLockStatus(de.innovationgate.webgate.api.locking.LockOwner)
     */
    public int getLockStatus(LockOwner owner) throws WGAPIException {
        return getDatabase().getLockManager().getLockStatus(this, owner);
    }
    
    /**
     * returns the lock status for the current sessioncontext
     * @throws WGAPIException 
     */
    public int getLockStatus() throws WGAPIException {
        return getLockStatus(getDatabase().getSessionContext());
    }
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#getParentLockable()
     */
    public abstract Lockable getParentLockable() throws WGAPIException;
    
    protected boolean isDisposed() {
        return disposed;
    }

    public int hashCode() {
        final int PRIME = 31;
        int result = 1;
        result = PRIME * result + ((db == null) ? 0 : db.hashCode());
        result = PRIME * result + ((uniqueId == null) ? 0 : uniqueId.hashCode());
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final WGDocument other = (WGDocument) obj;
        if (db == null) {
            if (other.db != null)
                return false;
        }
        else if (!db.equals(other.db))
            return false;
        if (uniqueId == null) {
            if (other.uniqueId != null)
                return false;
        }
        else if (!uniqueId.equals(other.uniqueId))
            return false;
        return true;
    }

    protected String getOriginDatabase() {
        return originDatabase;
    }

    /**
     * Convenience method to remove all items from the document at once
     * @throws WGAPIException 
     */
    public void removeAllItems() throws WGAPIException {
        Iterator<String> names = getItemNames().iterator();
        while (names.hasNext()) {
            String name = (String) names.next();
            removeItem(name);
        }
    }
    
    /**
     * Sets the given dates as created and modified dates of the document and saved it with them
     * @param created The created date
     * @param modified The modified date
     * @throws WGAPIException
     */
    public void saveWithGivenTimestamps(Date created, Date modified) throws WGAPIException {
        
        // We must ensure that the doc is saved before modifying created date
        // because implementations may rely on an unset created date
        // to tell if the document has been saved yet
        if (!isSaved()) {
            save();
        }
        
        setMetaData(WGDocument.META_CREATED, created);
        save(modified);
        
    }
    
    /**
     * Saves the document without updating the last modified timestamp
     */
    public void saveQuiet() throws WGAPIException {
        save(getLastModified());
    }
    
    /**
     * Determines if this document serves metadata for file attachments
     * @return true, if file metadata is available
     * @throws WGAPIException
     */
    public boolean hasFileMetadata() throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        Object value = null;
        
        Map<String, Object> cache = null;
        boolean readFromCache = isCachingEnabled();
        if (readFromCache) {
            cache = getFileMetaCache();
        }
        
        String cacheKey = "$hasFileMetaData";
        if (cache != null && !isCoreRetrieved()) {
            value = cache.get(cacheKey);
        }

        if (value == null) {
            WGDocumentCore theCore = this.getCore();
            value = getCore().hasFileMetadata();
                                   
            if (cache != null && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                cache.put(cacheKey, nullPlaceholder(value));
            }
        }
        else if (value instanceof NullPlaceHolder) {
            value = null;
        }

        return (Boolean) value;
    }

    public Object getExtensionData(String strName) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return null;
        }
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }

        Object value = null;
        
        Map<String, Object> cache = null;
        boolean readFromCache = isCachingEnabled();
        if (readFromCache) {
            cache = getExtensionDataCache();
        }
        
        String cacheKey = "extvalue:" + strName;
        if (cache != null && !isCoreRetrieved()) {
            value = cache.get(cacheKey);
        }

        if (value == null) {
            WGDocumentCore theCore = this.getCore();
            value = theCore.getExtensionData(strName);
            value = cloneMutableObjects(value);
            
            if (cache != null && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                cache.put(cacheKey, nullPlaceholder(value));
            }
        }
        else if (value instanceof NullPlaceHolder) {
            value = null;
        }
        
        // Return only copies of mutable objects, so modifying them will not modify the cache
        value = cloneMutableObjects(value);

        return value;
        
    }



    public List<String> getExtensionDataNames() throws WGAPIException {
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        return getCore().getExtensionDataNames();
    }

    public void removeExtensionData(String strName) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException(CSFEATURE_EXTDATA, WGDatabase.CSVERSION_WGA5);
        }
        
        getCore().removeExtensionData(strName);
        setEdited(true);
    }

    public void writeExtensionData(String strName, Object value) throws WGAPIException {
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException(CSFEATURE_EXTDATA, WGDatabase.CSVERSION_WGA5);
        }  
        
        getCore().writeExtensionData(strName, value);
        setEdited(true);
    }
    
    /**
     * Returns the name of the primary attachment of this document
     * The primary attachment is a file attachment that is regarded the main attachment of this document (to be interpreted in a custom way).
     * By default the primary attachment of a documents is the first attachment that was attached to it.
     * Not all documents must feature a primary attachment. It can be manually set/changed via {@link #setPrimaryFileName(String)}.
     * @throws WGAPIException 
     */
    public String getPrimaryFileName() throws WGAPIException {
        
        String name = (String) getExtensionData(EXT_PRIMARY_ATTACHMENT);
        if (name != null && hasFile(name)) {
            return name;
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Manually sets the primary attachment of this document
     * @param name The name of the file to become primary attachment. Use null to remove the current primary attachment reference (not the attachment itself)
     * @throws WGAPIException 
     */
    public void setPrimaryFileName(String name) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGContentStoreVersionException(CSFEATURE_EXTDATA, WGDatabase.CSVERSION_WGA5);
        }
        
        if (name != null) {
            name = getDatabase().convertFileNameForAttaching(name);
            writeExtensionData(EXT_PRIMARY_ATTACHMENT, name);
        }
        else {
            removeExtensionData(EXT_PRIMARY_ATTACHMENT);
        }
    }

    protected WGDocumentCore getTemporaryCore() {
        return temporaryCore;
    }

    
    /**
     * Returns a list of all available derivates for the specified file and a specified usage
     * @param fileName The name of the file
     * @param usage the usage to filter for
     * @return List of derivate metadata objects
     * @throws WGAPIException
     */
    public List<WGFileDerivateMetaData> getFileDerivates(String fileName, List<String> usage) throws WGAPIException {
        
        List<WGFileDerivateMetaData> all_derivates = getCore().getFileDerivates(fileName);
        if (all_derivates == null) {
            return Collections.emptyList();
        }
        else if(usage!=null){
        	List<WGFileDerivateMetaData> derivates = new ArrayList<WGFileDerivateMetaData>();
        	for (WGFileDerivateMetaData md : all_derivates) {
        		if(usage.contains(md.getUsage()))
					derivates.add(md);
			}
        	return derivates;
        }
        else return all_derivates;
        
    }

    /**
     * Returns a list of all available derivates for the specified file
     * @param fileName The name of the file
     * @return List of derivate metadata objects
     * @throws WGAPIException
     */
    public List<WGFileDerivateMetaData> getFileDerivates(String fileName) throws WGAPIException {
    	return getFileDerivates(fileName, null);
    }
    
    @Override
    public void markMetaDataModified(WGFileMetaData md) throws WGAPIException {
        getCore().markFileMetaDataModified(md);
        setEdited(true);
    }
    
    /**
     * Returns the metadata of a file derivate
     * @param id ID of the derivate
     */
    public WGFileDerivateMetaData getFileDerivateMetaData(String id) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getCore().getFileDerivateMetaData(id);
        
    }
    
    /**
     * Write modifications to derivate metadata to the derivate
     * @param md The modified metadata
     * @throws WGAPIException
     */
    public void writeFileDerivateMetaData(WGFileDerivateMetaData md) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        getCore().writeFileDerivateMetaData(md);
    }
    
    /**
     * Returns the data of a file derivate
     * @param id The derivate id
     */
    public InputStream getFileDerivateData(String id) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return getCore().getFileDerivateData(id);
        
    }
    
    /**
     * Creates a new file derivate for an existing original file attachment
     * @param fileName The name of the original file on the document
     * @param creator An ID of the creator class
     * @param name The name of the derivate to create
     * @param file The derivate data as file
     * @param usage The usage type of the derivate
     * @param additionalAnnotators Additional annotators to annotate the attached derivate with image information or custom fields. Specify null to use no additional annotators.
     * @return Metadata of the created file derivate
     */
    public WGFileDerivateMetaData createFileDerivate(String fileName, String creator, String name, final File file, String usage, List<WGFileAnnotator> additionalAnnotators) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (creator.length() > 32) {
            throw new WGIllegalArgumentException("Creator parameter may not be above a length of 32. Use an MD5 Hash of the creator name.");
        }
        
        if (!DERIVATE_NAME_PATTERN.matcher(name).matches()) {
            throw new WGIllegalArgumentException("Invalid derivate name. Derivat names may only have ASCII alphanumeric characters plus the underscore.");
        }
        
        WGFileDerivateMetaData meta = new WGFileDerivateMetaData(getDatabase(), getDocumentKeyObj(), null, null, null, null, null, 0, null, null);
        meta.setUsage(usage);
        List<WGFileAnnotator> annotators = new ArrayList<WGFileAnnotator>(getDatabase().getFileAnnotators());
        if (additionalAnnotators != null) {
            annotators.addAll(additionalAnnotators);
        }
        
        DataSource ds = new DataSource() {

            @Override
            public String getContentType() {
                return "application/octet-stream";
            }

            @Override
            public InputStream getInputStream() throws IOException {
                return new BufferedInputStream(new FileInputStream(file));
            }

            @Override
            public String getName() {
                return null;
            }

            @Override
            public OutputStream getOutputStream() throws IOException {
                throw new IOException("This data source does not provide an output stream");
            }
            
        };
        
        try {
            for (WGFileAnnotator annotator : annotators) { 
                annotator.annotateFile(ds, meta);
            }
        }
        catch (Throwable e) {
            throw new WGAPIException("Exception running annotator on attaching file '" + file.getName() + "'", e);
        }

        
        try {
            return getCore().createFileDerivate(fileName, creator, name, new BufferedInputStream(new FileInputStream(file)), meta.getAllExtensionData());
        }
        catch (FileNotFoundException e) {
            throw new WGAPIException("File not found: " + file.getPath(), e);
        }
        
        
    }
    
    /**
     * Creates a new file derivate for an existing original file attachment
     * @param fileName The name of the original file on the document
     * @param creator An ID of the creator class
     * @param derivateName The name of the derivate to create
     * @param in The data of the derivate
     * @param usage The usage type of the derivate
     * @param additionalAnnotators Additional annotators to run on the new derivate
     * @return Metadata of the created file derivate
     * @throws IOException 
     */
    public WGFileDerivateMetaData createFileDerivate(String fileName, String creator, String derivateName, InputStream in, String usage, List<WGFileAnnotator> additionalAnnotators) throws WGAPIException, IOException {
        TemporaryFile tempFile = new TemporaryFile(fileName, in, WGFactory.getTempDir());
        WGFileDerivateMetaData md = createFileDerivate(fileName, creator, derivateName, tempFile.getFile(), usage, additionalAnnotators);
        tempFile.deleteOnEviction(getDatabase().getSessionContext());
        return md;
    }
    
    /**
     * Deletes an existing file derivate
     * @param id The derivate ID
     * @throws WGAPIException
     */
    public void removeFileDerivate(String id) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        getCore().removeFileDerivate(id);
    }
    
    protected boolean isReadableForUser() throws WGAPIException {
        return true;
    }

    protected void setCacheRevision(WGDatabaseRevision cacheRevision) {
        _cache.setCacheRevision(cacheRevision);
    }

    protected boolean testUniqueNameUniqueness(String uniqueName, String language) throws WGAPIException {
    
        if (uniqueName == null || uniqueName.equals("")) {
            return true;
        }
        
        WGDatabase db = getDatabase();
         
        try {
            
            // Test for existing struct entry
            if (getDatabase().getContentStoreVersion() >= 5) {
                WGDocumentCore existingStructEntry = db.getCore().getStructEntryByName(uniqueName);
                if (existingStructEntry != null && !WGDocument.buildDocumentKey(existingStructEntry, db).equals(getDocumentKeyObj())) {
                    return false;
                }
            }
                       
            // Test for existing content
            WGDocumentCore existingContent = null;
            if (language != null) {
                existingContent = db.getCore().getContentByName(uniqueName, language);
                if (existingContent != null && !WGDocument.buildDocumentKey(existingContent, db).equals(getDocumentKeyObj())) {
                    return false;
                }
            }
            else {
                Iterator<WGLanguage> langs = getDatabase().getLanguages().values().iterator();
                while (langs.hasNext()) {
                    WGLanguage lang = (WGLanguage) langs.next();
                    existingContent = db.getCore().getContentByName(uniqueName, lang.getName());
                    if (existingContent != null && !WGDocument.buildDocumentKey(existingContent, db).equals(getDocumentKeyObj())) {
                        return false;
                    }
                }
            }
            
            return true;
            
        }
        catch (WGAPIException e) {
            WGFactory.getLogger().error("Exception testing unique name", e);
            return false;
        }
        
    }
    
    /**
     * Returns document keys of documents who may hold cached references to this document. The caches of these will get cleared when this document is removed.
     * Calling this method must be possible even if the core is not fetchable.
     */
    protected List<WGDocumentKey> getCacheParents() {
        return Collections.emptyList();
    }
    
    /**
     * Returns update logs for the last updates done to this document, in descending chronological order
     */
    public SkippingIterator<WGUpdateLog> getLastUpdates() throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        return new SkippingIteratorWrapper<WGUpdateLog>(getCore().getLastUpdates());
        
    }
    
    @Override
    public WGDocument getFileParent() {
        return this;
    }
    
    /**
     * Adds an action that will be executed after the document is saved the next time on this database session.
     * This will also be done if that save happens in a transaction.
     * @param action The action to execute
     * @throws WGAPIException
     */
    public void afterSave(SaveAction action) throws WGAPIException {
        getDocumentSessionContext().getAfterSaveActions().add(action);
    }

    protected boolean isTempDuplicate() {
        return tempDuplicate;
    }

    private Map<String, Object> getExtensionDataCache() {
        return _cache.getExtensionDataCache();
    }

    private Map<String, Object> getFileMetaCache() {
        return _cache.getFileMetaCache();
    }

    private List<String> getFileNamesCache() {
        return _cache.getFileNamesCache();
    }

    private void setFileNamesCache(List<String> fileNamesCache) {
        _cache.setFileNamesCache(fileNamesCache);
    }

    private Map<String, Object> getItemCache() {
        return _cache.getItemCache();
    }

    private Map<String, Object> getMetaCache() {
        return _cache.getMetaCache();
    }

    private void setDeletedFlag(boolean deletedFlag) {
        _cache.setDeletedFlag(deletedFlag);
    }
    
    /**
     * Marks the document to be automatically saved at the end of the session
     * @throws WGAPIException
     */
    public void autoSave() throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        getDatabase().getSessionContext().addAutoSaveDoc(this);
    }

    /*
     * JavaScript like util methods able to be chained
     */
    
    public WGDocument setItem(String name, Object value) throws WGAPIException{
    	setItemValue(name, value);
    	return this;
    }
    public WGDocument setItems(Map<String, Object> values) throws WGAPIException{
    	for (Map.Entry<String, Object> entry : values.entrySet()){
    		setItemValue(entry.getKey(), entry.getValue());
    	}
    	return this;
    }

    public WGDocument setMeta(String name, Object value) throws WGAPIException{
    	setMetaData(name, value);
    	return this;
    }
    public WGDocument setMetas(Map<String, Object> values) throws WGAPIException{
    	for (Map.Entry<String, Object> entry : values.entrySet()){
    		setMetaData(entry.getKey(), entry.getValue());
    	}
    	return this;
    }
    
    public WGDocument setValues(Map<String, Object> values) throws WGAPIException{
    	for (Map.Entry<String, Object> entry : values.entrySet()){
    		String name = entry.getKey();
    		if(name.equals(name.toUpperCase()))
    			setMetaData(name, entry.getValue());
    		else setItemValue(name, entry.getValue());
    	}
    	return this;
    }
    
}
