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
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.PageRightsFilter.Right;
import de.innovationgate.webgate.api.WGSessionContext.DocumentContext;
import de.innovationgate.webgate.api.fake.WGFakeArea;
import de.innovationgate.webgate.api.locking.Lockable;
import de.innovationgate.webgate.api.utils.MasterSessionTask;

/**
 * A struct entry that is used to build the site hierarchy, determine the
 * content types at the hierarchy nodes and the editing rights in the hierarchy.
 */
public class WGStructEntry extends WGDocument implements Comparable<WGStructEntry>, PageHierarchyNode {
    
    /**
     * Value to place as first value in {@link #META_CHILD_EDITORS} or {@link #META_PAGE_EDITORS} to inherit editor rights from the parent and further reduce it with the other values of the meta.
     * This effectively simulates the behaviour of {@link #META_READERS} for these metadata fields where it is only possible to further reduce rights down the hierarchy and impossible to loosen them.
     */
    public static final String EDITORS_INHERIT_AND_REDUCE = "#inherit-and-reduce";

    /**
     * extension data field which stores the page sequence for this document in CS Versions > 5
     */
    public static final String EXT_PAGE_SEQUENCE = "page-sequence";
    
    public class SessionData {
        
        private boolean _performUserRightsTestsOnSave = true;
        
        private List<String> _backendReaders;
        
        public List<String> getBackendReaders() {
            return _backendReaders;
        }

        public void setBackendReaders(List<String> backendReaders) {
            this._backendReaders = backendReaders;
        }

        private List<String> _backendPageEditors;

        public List<String> getBackendPageEditors() {
            return _backendPageEditors;
        }

        public void setBackendPageEditors(List<String> backendContentEditors) {
            this._backendPageEditors = backendContentEditors;
        }
        
        private List<String> _backendChildEditors;

        public List<String> getBackendChildEditors() {
            return _backendChildEditors;
        }

        public void setBackendChildEditors(List<String> backendChildEditors) {
            this._backendChildEditors = backendChildEditors;
        }

        protected boolean isPerformStatusTestsOnSave() {
            return _performUserRightsTestsOnSave;
        }

        protected void setPerformUserRightsTestsOnSave(boolean performStatusTestsOnSave) {
            this._performUserRightsTestsOnSave = performStatusTestsOnSave;
        }

    }
    
    public static class StructCache extends WGDocument.Cache {

        private WGDocumentListCache _allContentCache = null;
        private int _allContentCacheFetchState = FETCHSTATE_NONE;
        private WGDocumentKey _areaCache = null;
        private WGDocumentListCache _childEntryCache = null;
        private Integer _childEntryCountCache = null;
        private Integer _contentCountCache = null;
        private WGDocumentKey _parentEntryCache = null;

        private Map<String,WGDocumentKey> _releasedContentCache = new ConcurrentHashMap<String, WGDocumentKey>();

        public WGDocumentListCache getAllContentCache() {
            return _allContentCache;
        }

        public void setAllContentCache(WGDocumentListCache allContentCache) {
            _allContentCache = allContentCache;
        }

        public int getAllContentCacheFetchState() {
            return _allContentCacheFetchState;
        }

        public void setAllContentCacheFetchState(int allContentCacheFetchState) {
            _allContentCacheFetchState = allContentCacheFetchState;
        }

        public WGDocumentKey getAreaCache() {
            return _areaCache;
        }

        public void setAreaCache(WGDocumentKey areaCache) {
            _areaCache = areaCache;
        }

        public WGDocumentListCache getChildEntryCache() {
            return _childEntryCache;
        }

        public void setChildEntryCache(WGDocumentListCache childEntryCache) {
            _childEntryCache = childEntryCache;
        }

        public Integer getChildEntryCountCache() {
            return _childEntryCountCache;
        }

        public void setChildEntryCountCache(Integer childEntryCountCache) {
            _childEntryCountCache = childEntryCountCache;
        }

        public Integer getContentCountCache() {
            return _contentCountCache;
        }

        public void setContentCountCache(Integer contentCountCache) {
            _contentCountCache = contentCountCache;
        }

        public WGDocumentKey getParentEntryCache() {
            return _parentEntryCache;
        }

        public void setParentEntryCache(WGDocumentKey parentEntryCache) {
            _parentEntryCache = parentEntryCache;
        }

        public Map<String, WGDocumentKey> getReleasedContentCache() {
            return _releasedContentCache;
        }

        public void setReleasedContentCache(Map<String, WGDocumentKey> releasedContentCache) {
            _releasedContentCache = releasedContentCache;
        }
        
        
        @Override
        public void dropRelations() {
            super.dropRelations();
            _areaCache = null;
            _parentEntryCache = null;
            _childEntryCache = null;
            _allContentCache = null;
            _allContentCacheFetchState = FETCHSTATE_NONE;
            _releasedContentCache.clear();

        }
        
        @Override
        public void dropCache() {
            super.dropCache();
            _contentCountCache = null;
            _childEntryCountCache = null;
        }
        
    }
	
    /**
     * The set of contents that belongs to one struct entry, divided up into different maps
     *
     */
    public class ContentSet {
        
        private Map<String,WGContent> _allContent = new HashMap<String,WGContent>();
        private Map<String,WGContent> _releasedContent = new HashMap<String,WGContent>();
        private Map<String,WGContent> _archivedContent = new HashMap<String,WGContent>();
        
        /**
         * Constructor for empty content sets, returned when the user is not allowed to read the contents
         */
        public ContentSet() {
        }
        
        public ContentSet(List<WGContent> contents) throws WGAPIException {
 
            Iterator<WGContent> contentListIt = contents.iterator();
            WGContent newContent;
                               
            while (contentListIt.hasNext()) {
                newContent = (WGContent) contentListIt.next();
                if (!newContent.isReadableForUser()) {
                    continue;
                }
                
                String cachingKey = newContent.getLanguage().getName() + WGContentKey.TOKEN_DIVIDER + newContent.getVersion();

                if (newContent.getStatus().equals(WGContent.STATUS_ARCHIVE)) {
                    getArchivedContent().put(cachingKey, newContent);
                } 
                
                else {
                    getAllContent().put(cachingKey, newContent);
                    if (newContent.getStatus().equals(WGContent.STATUS_RELEASE)) {
                        WGContent oldContent = (WGContent) _releasedContent.put(newContent.getLanguage().getName(), newContent);
                        if (oldContent != null && oldContent.getVersion() != newContent.getVersion()) {
                            WGFactory.getLogger().warn("There are multiple released content versions for structentry " + newContent.getStructKey() + " (db:" + newContent.getDatabase().getDbReference() + ") on language " + newContent.getLanguage().getName() + ": Versions " + newContent.getVersion() + " and " + oldContent.getVersion());
                        }
                    }
                }
            }
            
        }
        
        /**
         * Returns all ACTIVE content for the struct entry (excluding archived ones), mapped by a string "language.version"
         */
        public Map<String,WGContent> getAllContent() {
            return _allContent;
        }
        /**
         * Returns the archived content for the struct entry, mapped by a string "language.version"
         */
        public Map<String,WGContent> getArchivedContent() {
            return _archivedContent;
        }
        /**
         * Returns the released content for the struct entry, mapped by the language code
         */
        public Map<String,WGContent> getReleasedContent() {
            return _releasedContent;
        }
        
    }
    
    class TestForContent {

        private String _language;       
        private String _status;

        
        public TestForContent(String language, String status) {
            _language = language;
            _status = status;
        }
        
        public TestForContent() {
            _language = null;
            _status = null;
        }
        
        public boolean doesContentExist() {

            try {
                List<WGDocumentCore> allContent = getDatabase().getCore().getAllContent(WGStructEntry.this, _status == null || _status.equals(WGContent.STATUS_ARCHIVE));    
                	
        		Iterator<WGDocumentCore> itContent = allContent.iterator();                		
            	while( itContent.hasNext() ){  
            	    WGDocumentCore content = itContent.next();
            	    
            	    String language = (String) content.getMetaData(WGContent.META_LANGUAGE);
            	    String status = (String) content.getMetaData(WGContent.META_STATUS);
            	    if (_status != null && !status.equals(_status)) {
            	        continue;
            	    }
            	    
            	    if (_language != null && !language.equals(_language)) {
            	        continue;
            	    }
            	    
            	    return true;
            		
        		}
                
                return false;
            }
            catch (Throwable t) {
                WGFactory.getLogger().error("Error testing for content existence", t);
                return true;
            } 
        }
    }

    public static final String META_KEY = "KEY";
    public static final MetaInfo METAINFO_KEY = new MetaInfo(META_KEY, Object.class, null);
    
    public static final String META_TITLE = "TITLE";
    public static final MetaInfo METAINFO_TITLE = new MetaInfo(META_TITLE, String.class, "");
    static {
    	METAINFO_TITLE.setInputConverter(TITLE_CONVERTER);
    	METAINFO_TITLE.setOutputConverter(TITLE_CONVERTER);
    }

    public static final String META_AREA = "AREA";
    public static final MetaInfo METAINFO_AREA = new MetaInfo(META_AREA, String.class, null);

    public static final String META_POSITION = "POSITION";
    public static final MetaInfo METAINFO_POSITION = new MetaInfo(META_POSITION, Number.class, new Integer(0));

    public static final String META_CONTENTTYPE = "CONTENTTYPE";
    public static final MetaInfo METAINFO_CONTENTTYPE = new MetaInfo(META_CONTENTTYPE, Object.class, null);
    
    public static final String META_READERS = "READERS";
    public static final MetaInfo METAINFO_READERS = new MetaInfo(META_READERS, String.class, Collections.EMPTY_LIST);
    static { 
        METAINFO_READERS.setMultiple(true);
        METAINFO_READERS.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT); 
        METAINFO_READERS.setInputConverter(new MetaConverter() {

            public Object convert(WGDocument doc, MetaInfo metaInfo, Object value) throws WGAPIException {
                    List readers = (List) value;
                    
                    // Test if the current user is contained in the list. If not, he is added to avoid self disclosure
                    if (!WGDatabase.anyoneAllowed(readers) && !doc.getDatabase().isMemberOfUserList(readers)) {
                        readers.add(doc.getDatabase().getSessionContext().getUser());
                    }
                return readers;
            }
            
        });
    };
    
    public static final String META_PAGE_EDITORS = "PAGEEDITORS";
    public static final MetaInfo METAINFO_PAGE_EDITORS = new MetaInfo(META_PAGE_EDITORS, String.class, Collections.EMPTY_LIST);
    static { METAINFO_PAGE_EDITORS.setMultiple(true); };

    public static final String META_CHILD_EDITORS = "CHILDEDITORS";
    public static final MetaInfo METAINFO_CHILD_EDITORS = new MetaInfo(META_CHILD_EDITORS, String.class, Collections.EMPTY_LIST);
    static { METAINFO_CHILD_EDITORS.setMultiple(true); };
   
    public static final String META_WORKFLOW_NAME = "OVERRIDE_WORKFLOW";
    public static final MetaInfo METAINFO_WORKFLOW_NAME = new MetaInfo(META_WORKFLOW_NAME, String.class, null);
    
    public static final String META_UNIQUENAME = "UNIQUENAME";
    public static final MetaInfo METAINFO_UNIQUENAME = new MetaInfo(META_UNIQUENAME, String.class, null);
    static  {
        METAINFO_UNIQUENAME.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
        METAINFO_UNIQUENAME.setLowerCase(true);
        METAINFO_UNIQUENAME.addSynonym("PAGENAME");
        METAINFO_UNIQUENAME.addSynonym("PAGEUNIQUENAME");
        METAINFO_UNIQUENAME.addSynonym("PAGEDOCNAME");
    }
    
    public static final String META_PUBLISHED = "PUBLISHED";
    public static final MetaInfo METAINFO_PUBLISHED = new MetaInfo(META_PUBLISHED, Map.class, Collections.emptyMap());
    static {
        METAINFO_PUBLISHED.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    public static final String META_CHILDPAGERESTRICTIONS = "CHILDPAGERESTRICTIONS";
    public static final MetaInfo METAINFO_CHILDPAGERESTRICTIONS = new MetaInfo(META_CHILDPAGERESTRICTIONS, String.class, PAGERESTRICTION_UNSPECIFIED);
    static {
        METAINFO_CHILDPAGERESTRICTIONS.setExtdata(true);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_ANY);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_NONE);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_FIXEDTYPES);
        METAINFO_CHILDPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_UNSPECIFIED);
    }
    
    public static final String META_ALLOWED_CHILDTYPES = "ALLOWEDCHILDTYPES";
    public static final MetaInfo METAINFO_ALLOWED_CHILDTYPES = new MetaInfo(META_ALLOWED_CHILDTYPES, String.class, Collections.EMPTY_LIST);
    static {
        METAINFO_ALLOWED_CHILDTYPES.setMultiple(true);
        METAINFO_ALLOWED_CHILDTYPES.setExtdata(true);
    }

    public static final String META_PAGESEQUENCE = "PAGESEQUENCE";
    public static final MetaInfo METAINFO_PAGESEQUENCE = new MetaInfo(META_PAGESEQUENCE, Long.class, 0L);

    public static final String META_PAGE_DISABLED = "PAGEDISABLED";
    public static final MetaInfo METAINFO_PAGE_DISABLED = new MetaInfo(META_PAGE_DISABLED, Boolean.class, false);
    static {
    	METAINFO_PAGE_DISABLED.setExtdata(true);
    }

    
    /**
     * No content has yet been fetched for this struct entry.
     */
    public static final int FETCHSTATE_NONE = 0;

    /**
     * Only active contents (non-archived) have been fetched for this struct
     * entry.
     */
    public static final int FETCHSTATE_ACTIVE_ONLY = 1;

    /**
     * All contents - including archived - have been fetched for this struct
     * entry.
     */
    public static final int FETCHSTATE_ARCHIVED = 2;

    private Object _retrievalKey = null;

    

    /*
    private Map fetchedContentForUsers = Collections
            .synchronizedMap(new HashMap());
    */
    
    private static final Object FETCH_RELEASED_CONTENT_CACHE = "RELEASED_CONTENT_CACHE_";
    
    /**
     * Determines if this entry is a root entry
     * @throws WGAPIException 
     */
    public boolean isRoot() throws WGAPIException {
        if (this.getParentEntry() == null) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Sets allowed editors for child entries and their content (if they inherit
     * their editing rights)
     * 
     * @param editors
     * @throws WGAPIException 
     */
    public boolean setChildEditors(List editors) throws WGAPIException {
        return setMetaData(META_CHILD_EDITORS, editors);
    }
 
    /**
     * Sets names that are allowed to edit this struct and it's contents.
     * 
     * @param editors
     * @throws WGAPIException 
     */
    public void setPageEditors(List editors) throws WGAPIException {
        setMetaData(META_PAGE_EDITORS, editors);
    }
    
    /**
     * Old version of {@link #setPageEditors(List)}. Avoid in new developments.
     * @param editors
     * @throws WGAPIException
     * @deprecated
     */
    public boolean setStructEditors(List editors) throws WGAPIException {
        setPageEditors(editors);
        return true;
    }

    /**
     * Determines if this entry has child entries
     * @throws WGAPIException 
     */
    public boolean hasChildren() throws WGAPIException {

        if (this.getChildEntryIterator(1).hasNext()) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Returns a list of names that are allowed to edit this struct document and
     * it's contents.
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public List<String> getPageEditors() throws WGAPIException {
        return (List<String>) this.getMetaData(WGStructEntry.META_PAGE_EDITORS);
    }
    
    /**
     * Returns the name of the workflow explicitly set for the current struct entry.
     * A value of null means that no workflow is set and this page inherits its workflow from either parent structs or the content type.
     * @throws WGAPIException
     */
    public String getLocalWorkflowName() throws WGAPIException {
        return (String) getMetaData(META_WORKFLOW_NAME);
    }
    
    /**
     * Old version for {@link #getPageEditors()}. Avoid in new developments.
     * @throws WGAPIException
     * @deprecated
     */
    public List getStructEditors() throws WGAPIException {
        return getPageEditors();
    }

    /**
     * Returns the editors allowed to edit child entries and content of this
     * struct entry
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public List<String> getChildEditors() throws WGAPIException {
        return (List<String>) this.getMetaData(WGStructEntry.META_CHILD_EDITORS);
    }

    /**
     * Returns the parent entry, null if this is a root entry.
     * @throws WGAPIException 
     */
    public WGStructEntry getParentEntry() throws WGAPIException {

        // Double checked cache (un-synchronized and synchronized)
        WGDocument theEntry = null;
        if (this.getParentEntryCache() != null && isCachingEnabled()) {
            theEntry = this.getParentEntryCache().getDocument(getDatabase());
        }
        
        if (theEntry == null) {
            WGOperationKey op = db.obtainOperationKey(
                    WGOperationKey.OP_STRUCT_PARENT, String
                            .valueOf(getStructKey()));
            synchronized (op) {

                try {
                    op.setUsed(true);
                    if (this.getParentEntryCache() != null && isCachingEnabled()) {
                        theEntry = this.getParentEntryCache().getDocument(getDatabase());
                    }
                    
                    if (!isCachingEnabled() || theEntry == null) {
                        theEntry = this.db.getParentEntry(this);
                        if (theEntry == null) {
                            theEntry = this;
                        }
                        if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
                            this.setParentEntryCache(theEntry.getDocumentKeyObj());
                        }
                    }
                }
                finally {
                    op.setUsed(false);
                }
            }
        }

        if (!WGUtils.nullSafeEquals(theEntry, this)) {
            return (WGStructEntry) theEntry;
        } else {
            return null;
        }
    }

    /**
     * Returns the area of this struct entry
     * @throws WGAPIException 
     */
    public WGArea getArea() throws WGAPIException {

        WGArea theArea = null;
        if (this.getAreaCache() != null) {
            theArea = (WGArea) this.getAreaCache().getDocument(getDatabase());
        }
        
        if (!isCachingEnabled() || theArea == null) {
            if (this.isRoot()) {
                theArea = this.db.getArea((String) this.getMetaData(META_AREA));
            } 
            else {
                theArea = this.getRootEntry().getArea();
            }
            if (theArea == null) {
                if (isDeleted(true)) {
                    throw new WGDeletedException(getDocumentKey());
                }
                else {
                    throw new WGIllegalDataException("Struct entry with unretrievable parent area: " + getDocumentKey());
                }
            }
            
            if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
                this.setAreaCache(theArea.getDocumentKeyObj());
            }
        }

       
        return theArea;

    }

    /**
     * Constructor. Should not be used outside the WGAPI
     * 
     * @param db
     * @param doc
     * @throws WGAPIException 
     */
    public WGStructEntry(WGDatabase db, WGDocumentCore doc) throws WGAPIException {
        this(db, doc, new WGDocumentObjectFlags());
        
    }

    public WGStructEntry(WGDatabase wgDatabase, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
        super(wgDatabase, doc, flags);
        gatherRetrievalKeys();
    }

    private void gatherRetrievalKeys() throws WGAPIException {
        this._retrievalKey = this.getMetaData(META_KEY);
    }



    /**
     * Gets the sibling entries (including the current struct entry) 
     * @throws WGAPIException 
     */
    public WGStructEntryList getSiblingEntries() throws WGAPIException {
        if (this.isRoot()) {
            return this.getArea().getRootEntries();
        }
        else {
            WGStructEntry entry = this.getParentEntry();
            if (entry != null) {
                return entry.getChildEntries();
            }
            else {
                WGFactory.getLogger().error(
                        "Could not get parent entry for struct '"
                                + getStructKey() + "'");
                return null;
            }
        }
    }
    
    /**
     * Gets an struct entry iterator for the sibling entries (including the current struct entry)
     * @param pageSize The size of retrieval pages
     * @throws WGAPIException 
     */
    public WGStructEntryIterator getSiblingEntryIterator(int pageSize) throws WGAPIException {
        if (this.isRoot()) {
            return this.getArea().getRootEntryIterator(pageSize);
        }
        else {
            WGStructEntry entry = this.getParentEntry();
            if (entry != null) {
                return entry.getChildEntryIterator(pageSize);
            } else {
                WGFactory.getLogger().error(
                        "Could not get parent entry for struct '"
                                + getStructKey() + "'");
                return null;
            }
        }
    }
    
    /**
     * Gets an struct entry iterator for the sibling entries (including the current struct entry), ordered by a given order expression
     * @param pageSize The size of retrieval pages
     * @param orderExpression Column set expression determining the order in which to return pages
     * @throws WGAPIException 
     */
    public WGStructEntryIterator getOrderedSiblingEntryIterator(int pageSize, String orderExpression) throws WGAPIException {
        if (this.isRoot()) {
            return this.getArea().getOrderedRootEntryIterator(pageSize, orderExpression);
        }
        else {
            WGStructEntry entry = this.getParentEntry();
            if (entry != null) {
                return entry.getOrderedChildEntryIterator(pageSize, orderExpression);
            } else {
                WGFactory.getLogger().error(
                        "Could not get parent entry for struct '"
                                + getStructKey() + "'");
                return null;
            }
        }
    }

    /**
     * Returns the root entry of this entry
     * @throws WGAPIException 
     */
    public WGStructEntry getRootEntry() throws WGAPIException {

        WGStructEntry entry = this;
        while (!entry.isRoot()) {
            entry = entry.getParentEntry();
        }
        return entry;

    }

    /**
     * Returns the position number.
     * @throws WGAPIException 
     */
    public Number getPosition() throws WGAPIException {
        return (Number) this.getMetaData(META_POSITION);
    }
    
    /**
     * Returns a path of position numbers for all ancestors of the current struct entry
     * @return A list of position numbers for the struct entry and all it's ancestors, beginning with the current struct entry and proceeding up the hierarchy
     * @throws WGAPIException
     */
    public List getPositionPath() throws WGAPIException {
    
        List path = new ArrayList();
        WGStructEntry entry = this;
        while (entry != null) {
            path.add(entry.getPosition());
            entry = entry.getParentEntry();
        }

        Collections.reverse(path);
        return path;
        
    }

    /**
     * Sets the position number.
     * 
     * @param pos
     * @throws WGAPIException 
     */
    public boolean setPosition(int pos) throws WGAPIException {
        return setMetaData(META_POSITION, new Integer(pos));
    }

    /**
     * Sets the content type
     * 
     * @param contenttype name
     * @throws WGAPIException 
     */
    public boolean setContentType(String name) throws WGAPIException {
        return setMetaData(META_CONTENTTYPE, name);
    }

    /**
     * Returns the child entries of this struct entry
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public WGStructEntryList getChildEntries() throws WGAPIException {
        
        
        List<WGStructEntry> theEntries = (List<WGStructEntry>) getDatabase().fetchDocumentListCache(this.getChildEntryCache(), WGDocument.TYPE_STRUCTENTRY);
        if (!isCachingEnabled() || theEntries == null) {
            WGOperationKey op = db.obtainOperationKey(
                    WGOperationKey.OP_STRUCT_CHILDREN, String
                            .valueOf(getStructKey()));
            synchronized (op) {
                try {
                    op.setUsed(true);
                    theEntries = (List<WGStructEntry>) getDatabase().fetchDocumentListCache(this.getChildEntryCache(), WGDocument.TYPE_STRUCTENTRY);
                    if (!isCachingEnabled() || theEntries == null) {
                        
                        theEntries = fetchChildEntries(null);
                        
                        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
                            Collections.sort(theEntries);
                        }
                        
                        if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
                            this.setChildEntryCache(WGDocumentListCache.buildFromDocuments(theEntries));
                        }
                    }
                }
                finally {
                    op.setUsed(false);
                }
            }
        }
        
        return WGStructEntryList.create(theEntries);
    }

    protected List<WGStructEntry> fetchChildEntries(WGPageOrderSet pageOrder) throws WGAPIException {
        List<WGStructEntry> theEntries;
        theEntries = new ArrayList<WGStructEntry>();
        WGStructEntryRetrievalIterator it = this.db.getChildEntries(this, pageOrder);
        try {
            while (it.hasNext()) {
                WGStructEntry child = it.next();
                child.setParentEntryCache(getDocumentKeyObj()); // Ensure child knows parent (#00004330)
                theEntries.add(child);
            }
        }
        finally {
            it.close();
        }
        return theEntries;
    }
    
    private void setParentEntryCache(WGDocumentKey documentKeyObj) {
        getCache().setParentEntryCache(documentKeyObj);
    }

    
    /**
     * Returns an iterator over the child entries of this struct entry
     * Internally the iterator fetches multiple entities at once from the backend for more performant backend retrieval processes. The size of those rertrieval pages must be determined as argument..
     * @param pageSize The size of retrieval pages
     * @return An iterator over all child entries of this struct entry
     * @throws WGAPIException
     */
    public WGStructEntryIterator getChildEntryIterator(int pageSize) throws WGAPIException {
        return new WGStructEntryIterator(this, pageSize);
    }
    
    /**
     * Returns a partial list of the child entries of this struct entry
     * @param offset The offset of the first entry to retrieve, 0 being the first one
     * @param size The number of entries to retrieve
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public WGStructEntryList getChildEntries(int offset, int size) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            WGStructEntryList entries = getChildEntries();
            int toIndex = offset + size;
            if (toIndex > entries.size()) {
                toIndex = entries.size();
            }
            return WGStructEntryList.create(entries.asList().subList(offset, toIndex));
        }

        List<WGStructEntry> theEntries = getDatabase().fetchDocumentListCache(this.getChildEntryCache(), WGDocument.TYPE_STRUCTENTRY, offset, size);
        if (!isCachingEnabled() || theEntries == null) {
            WGOperationKey op = db.obtainOperationKey(
                    WGOperationKey.OP_STRUCT_CHILDREN, String
                            .valueOf(getStructKey()));
            synchronized (op) {
                try {
                    op.setUsed(true);
                    theEntries = getDatabase().fetchDocumentListCache(this.getChildEntryCache(), WGDocument.TYPE_STRUCTENTRY, offset, size);
                    if (!isCachingEnabled() || theEntries == null) {
                        
                        theEntries = fetchChildEntryPage(offset, size, null);
                        
                        if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
                            if (getChildEntryCache() == null) {
                                setChildEntryCache(WGDocumentListCache.buildFromDocuments(theEntries, offset, size));
                            }
                            else {
                                setChildEntryCache(getChildEntryCache().mergeWithDocuments(theEntries, offset, size));
                            }
                        }
                    }
                }
                finally {
                    op.setUsed(false);
                }
            }
        }
        
        return WGStructEntryList.create(theEntries);
    }
    
    /**
     * Returns an iterator over the child entries of this struct entry, returned in a particular order
     * Internally the iterator fetches multiple entities at once from the backend for more performant backend retrieval processes. The size of those retrieval pages must be determined as argument..
     * @param pageSize The size of retrieval pages
     * @param pageOrder Order expression denoting the order in which pages should be returned
     * @return An iterator over all child entries of this struct entry
     * @throws WGAPIException
     */
    public WGStructEntryIterator getOrderedChildEntryIterator(int pageSize, String pageOrder) throws WGAPIException {
        
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            throw new WGNotSupportedException("This database does not support ordered navigation results");
        }
        
        return new WGStructEntryIterator(this, pageSize, pageOrder); 
    }
    
    /**
     * Returns a partial list of the child entries of this struct entry, ordered by a special order expression
     * @param offset The offset of the first entry to retrieve, 0 being the first one
     * @param size The number of entries to retrieve
     * @param orderExpression Page order set expression
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public WGStructEntryList getOrderedChildEntries(int offset, int size, String orderExpression) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            throw new WGNotSupportedException("This database does not support ordered navigation results");
        }
        
        WGPageOrderSet pageOrder = WGPageOrderSet.parse(orderExpression);
        return WGStructEntryList.create(fetchChildEntryPage(offset, size, pageOrder));
        
    }
    
    /**
     * Returns a list of all child entries of this struct entry, ordered by a special order expression
     * @param orderExpression Page order set expression
     * @throws WGAPIException 
     */
    @SuppressWarnings("unchecked")
    public WGStructEntryList getOrderedChildEntries(String orderExpression) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            throw new WGNotSupportedException("This database does not support ordered navigation results");
        }
        
        WGPageOrderSet pageOrder = WGPageOrderSet.parse(orderExpression);
        return WGStructEntryList.create(fetchChildEntries(pageOrder));
        
    }

    protected List<WGStructEntry> fetchChildEntryPage(int offset, int size, WGPageOrderSet pageOrder) throws WGAPIException {
        List<WGStructEntry> theEntries;
        theEntries = new ArrayList<WGStructEntry>();
        WGStructEntryRetrievalIterator it = this.db.getChildEntries(this, pageOrder);
        try {
            it.skip(offset);
            int fetched = 0;
            
            while (it.hasNext() && fetched < size) {
                WGStructEntry child = it.next();
                theEntries.add(child);
                fetched++;
            }
        }
        finally {
            it.close();
        }
        return theEntries;
    }

    /**
     * Returns all content documents under this struct entry
     * 
     * @param includeArchived
     *            Specify true to also include archived documents in result.
     * @throws WGAPIException 
     */
    public WGContentList getAllContent(boolean includeArchived) throws WGAPIException {
        ContentSet contentSet = getContentSet(includeArchived);
        
        if (includeArchived) {
            List allContent = new ArrayList();
            allContent.addAll(contentSet.getAllContent().values());
            allContent.addAll(contentSet.getArchivedContent().values());
            return WGContentList.create(allContent);
        } else {
            return WGContentList.create(contentSet.getAllContent().values());
        }
    }
       
        
    /**
     * Returns all active (non-archived) content under this struct entry.
     * @throws WGAPIException 
     */
    public WGContentList getAllContent() throws WGAPIException {
        return getAllContent(false);
    }

    private boolean alreadyFetched(boolean includeArchived) {

        int status = getContentFetchState();
        if (status == FETCHSTATE_NONE) {
            return false;
        }

        if (includeArchived && status == FETCHSTATE_ACTIVE_ONLY) {
            return false;
        }

        return true;

    }

    /**
     * Returns a specific content of the given language and content version
     * 
     * @param language
     * @param version
     * @throws WGAPIException 
     */
    public WGContent getContent(String language, int version) throws WGAPIException {
        ContentSet contentSet = getContentSet(true);
        String cachingKey = language + WGContentKey.TOKEN_DIVIDER + version;

        // Try non-archived
        WGContent content = (WGContent) contentSet.getAllContent().get(cachingKey);
        if (content != null)  {
            return content;
        }
        
        // Try archived
        return (WGContent) contentSet.getArchivedContent().get(cachingKey);
    }
    
    /**
     * Returns a content of the given language and status, if any exists.
     * If multiple contents exist with these parameters a random content of
     * that group is returned.
     * 
     * @param language The language of the content
     * @param status The workflow status of the content
     * @return A content with that parameters. null if none exists.
     * @throws WGAPIException 
     */
    public WGContent getContent(String language, String status) throws WGAPIException {
        
        if (WGContent.STATUS_RELEASE.equals(status)) {
            return getReleasedContent(language);
        }
        
        WGStructEntry.ContentSet contentSet = getContentSet(status.equals(WGContent.STATUS_ARCHIVE));
        
        Iterator contents;
        if (status.equals(WGContent.STATUS_ARCHIVE)) {
            contents = contentSet.getArchivedContent().values().iterator();
        }
        else {
            contents = contentSet.getAllContent().values().iterator();
        }
        
        WGContent content;
        while (contents.hasNext()) {
            content = (WGContent) contents.next();
            if (content.getLanguage().getName().equals(language) && content.getStatus().equals(status)) {
                return content;
            }
        }
        
        return null;
        
        
    }

    /**
     * Returns the title of the struct entry. We prohibit this on struct entries whose contents may not be read by the current user.
     * @throws WGAPIException 
     */
    public String getTitle() throws WGAPIException {
        
        if (!mayReadContent()) {
            return null;
        }
        
        return this.getMetaData(META_TITLE).toString();
    }

    /**
     * Sets the struct title.
     * 
     * @param title
     * @throws WGAPIException 
     */
    public boolean setTitle(String title) throws WGAPIException {
        return this.setMetaData(META_TITLE, title);
    }
    
    /**
     * Returns the name of the workflow that is effective on this struct entry.
     * This method is assymetric to {@link #setWorkflowName(String)} as it does not just return a workflow that is stored on this concrete struct.
     * Instead it really determines the workflow active for this page, either from a workflow stored on this struct, stored on some parent struct
     * or on the content type.
     * @throws WGAPIException 
     */
    public String getWorkflowName() throws WGAPIException {
    	String wfName = "";
    	WGStructEntry pEntry = this;
    	
    	while( pEntry != null ){
    		wfName = (String)pEntry.getMetaData(META_WORKFLOW_NAME);
    		if( wfName != null && !wfName.equals("") ){
    			break;
        	}
    		pEntry = pEntry.getParentEntry();
    	}
    	
    	if( wfName == null || wfName.equals("") ){
    		wfName = this.getContentType().getWorkflow();
    	}
    	
        return wfName;
    }

    /**
     * Sets a workflow on this concrete struct entry
     * @param wfname The workflow name to set
     * @deprecated Assymetric to {@link #getWorkflowName()} and thus irritating. Use {@link #setBranchWorkflow(String)} instead. 
     * @throws WGAPIException 
     */
    public boolean setWorkflowName(String wfname) throws WGAPIException {
        setBranchWorkflow(wfname);
        return true;
    }
    
    /**
     * Sets a workflow to use on this page and all subpages that themselves do not redefine the workflow.
     * @param wfName Name of the workflow
     * @throws WGAPIException
     */
    public void setBranchWorkflow(String wfName) throws WGAPIException {
        this.setMetaData(META_WORKFLOW_NAME, wfName);
    }
    
    /**
     * Returns a workflow that was set to be used on this page and all subpages that themselves do not redefine the workflow.
     * Returns null if no such workflow is set on this page.
     * @throws WGAPIException
     */
    public String getBranchWorkflow() throws WGAPIException {
        return (String) this.getMetaData(META_WORKFLOW_NAME);
    }
    
    /**
     * Returns the content type for this struct entry
     * @throws WGAPIException 
     */
    public WGContentType getContentType() throws WGAPIException {


        WGContentType theContentType = null;
        String doctypeName = (String) this.getMetaData(META_CONTENTTYPE);
        if (doctypeName != null) {
            theContentType = (WGContentType) this.db.getContentType(doctypeName);
        }


        if (theContentType != null) {
            return theContentType;
        } 
        else {
            return null;
        }

    }
    
    /**
     * Change the content type for this struct entry and all its contents.
     * This is only possible while no contents are in a approval workflow.
     * @param contentType The content type to set
     * @throws WGAPIException
     */
    public void changeContentType(WGContentType contentType) throws WGAPIException {
    	
    	// master task to check for unreleased content
    	MasterSessionTask checkUnreleasedContent = new MasterSessionTask(getDatabase()) {
			protected void exec(WGDatabase db) throws Throwable {
		    	Iterator contents = getAllContent().iterator();
		    	while (contents.hasNext()) {
		    		WGContent content = (WGContent) contents.next();
		    		if (!content.getStatus().equals(WGContent.STATUS_RELEASE)) {
		    			throw new WGIllegalStateException("ContentType cannot be changed bc. there is unreleased content in draft or review state.");
		    		}    		
		    	}
				
			}    		
    	};
    	
    	// check for unreleased content
    	try {
			checkUnreleasedContent.runWithExceptions();
		} catch (WGAPIException e)  {
			throw e;
		} catch (Throwable e) {
			throw new WGAPIException(e.getMessage(), e);
		}
		
		// Check if we may modify this content type
		performSaveCheck();
    	
		// check if contenttype can be used by current user at this position
		// null is also allowed if user is at least designer
		WGDocument parent = null;
		if (isRoot()) {
			parent = getArea();
		} else {
			parent = getParentEntry();
		}		
        if (contentType != null && !contentType.mayCreateChildEntry(parent)) {
            throw new WGAuthorisationException("User is not allowed to use this content type at this position", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE);
        }

        if (getDatabase().hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && contentType == null) {
            throw new WGIllegalArgumentException("You cannot create a struct entry without content type");
        }
        
    	
        if (contentType != null) {
            setMetaData(WGStructEntry.META_CONTENTTYPE, (getDatabase().hasFeature(WGDatabase.FEATURE_USE_OBJECTS_AS_REFERENCES) ? contentType.getCore() : (Object) contentType.getName()));
        }
        
        // save changes
        save();
        
        // Trigger content saved events for all contents so they are reindexed
        for (WGContent content : getAllContent(true)) {
            WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_HASBEENSAVED, content.getDocumentKey(), content.getStructEntry().getContentType().getName(), content.getDatabase());
            event.setContent(content);
            getDatabase().fireContentEvent(event);
        }
    }

    /**
     * Returns the inner layout for this struct entry. This may be determined by
     * it's content type or by internal struct fields.
     * 
     * @param mediaKey
     *            The media key of the desired layout
     * @return The TML module for the layout, if it exists for this media key,
     *         null otherwise
     * @throws WGAPIException 
     */
    public WGTMLModule getInnerLayout(String mediaKey) throws WGAPIException {

        WGContentType contentType = getContentType();
        if (contentType != null) {
            return contentType.getInnerLayout(mediaKey);
        } 
        else {
            return null;
        }
    }

    /**
     * Returns the outer layout for this struct entry. This may be determined by
     * it's content type or by internal struct fields.
     * 
     * @param mediaKey
     *            The media key of the desired layout
     * @return The TML module for the layout, if it exists for this media key,
     *         null otherwise
     * @throws WGAPIException 
     */
    public WGTMLModule getOuterLayout(String mediaKey) throws WGAPIException {

        WGContentType contentType = getContentType();
        if (contentType != null) {
            return contentType.getOuterLayout(mediaKey);
        }
        else {
            return null;
        }
    }

    /**
     * Compares two struct entries. Position is taken into account for
     * higher/lower but does not result in equality when positions are equal.
     * @throws WGAPIException 
     */
    public int compareTo(WGStructEntry otherEntry) throws ClassCastException {

        try {
            
            // Look if one is deleted - Then only the hashCode can compare them
            if (this.isDeleted() || otherEntry.isDeleted()) {
                return (this.hashCode() == this.hashCode() ? 0 : -1);
            }
               
            
            // Try to compare via position
            Double thisPos = new Double(this.getPosition().doubleValue());
            Double otherPos = new Double(otherEntry.getPosition().doubleValue());
            int posCompare = thisPos.compareTo(otherPos);
            if (posCompare != 0)
                return posCompare;
            
            
            // Try to compare via title
	        String thisTitle = WGUtils.getValueOrDefault(this.getTitle(), "");
    	    String otherTitle = WGUtils.getValueOrDefault(otherEntry.getTitle(), "");
        	int titleCompare = getDatabase().getDefaultLanguageCollator().compare(thisTitle, otherTitle);
	        if (titleCompare != 0)
    	        return titleCompare;
            
            // If everything else fails, compare Struct key
            return this.getStructKey().toString().compareTo(
                    otherEntry.getStructKey().toString());
            
        } catch (WGAPIException e) {
            throw new RuntimeException("Unable to compare bc. of WGAPIException", e);
        }
    }

    /**
     * Returns the child level of this entry. 1 is root.
     * @throws WGAPIException 
     */
    public Integer getLevel() throws WGAPIException {

        WGStructEntry entry = this;
        int level = 0;
        while (entry != null) {
            level++;
            entry = entry.getParentEntry();         
        }
        return new Integer(level);
    }

    /**
     * Returns the struct key.
     */
    public Object getStructKey() {
        return this._retrievalKey;
    }

    /**
     * Returns the released content of the given language
     * 
     * @param strLanguage
     * @throws WGAPIException 
     */
    public WGContent getReleasedContent(String strLanguage) throws WGAPIException {
        
        if (isDeleted()) {
            return null;
        }
        
        // Test for read access
        if (!mayReadContent()) {
            return null;
        }
        
        if (strLanguage == null) {
            strLanguage = getDatabase().getDefaultLanguage();
        }
        
        try {
            WGContent content = null;
            if (isCachingEnabled()) {
                content = (WGContent) getDatabase().fetchDocumentFromCache(getReleasedContentCache(), strLanguage);
            }
            if (content == null) {
                WGOperationKey op = getDatabase().obtainOperationKey(WGOperationKey.OP_STRUCT_CONTENTS, String.valueOf(getStructKey()));
                synchronized (op) {
                    try {
                        op.setUsed(true);
                        if (isCachingEnabled()) {
                            content = (WGContent) getDatabase().fetchDocumentFromCache(getReleasedContentCache(), strLanguage);
                        }
                        if (content == null) {
                            content = retrieveReleasedContent(strLanguage);
                        }
                    }
                    finally {
                        op.setUsed(false);
                    }
                }
            }
            
            return content;
            
        }
        catch (WGDocumentDoesNotExistException e) {
            return null;
        }
    }

    private WGContent retrieveReleasedContent(String strLanguage) throws WGAPIException {
        WGContent content = getDatabase().getReleasedContent(this, strLanguage);
        if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
            if (content != null) {
                getReleasedContentCache().put(strLanguage, content.getDocumentKeyObj());
            }
            else {
                getDatabase().mapNonExistentDocIndicator(getReleasedContentCache(), strLanguage, WGDocument.TYPE_CONTENT);
            }
        }
        
        if (content != null && content.isReadableForUser()) {
            return content;
        }
        else {
            throw new WGDocumentDoesNotExistException();
        }
        
    }

    /**
     * Tests if this entry has an released content for the given language.
     * This method is capable of regarding contents that the current user is not allowed to see.
     * @param strLanguage The language to test
     * @return true, if there is a released content, false otherwise 
     */
    public boolean hasReleasedContent(String strLanguage) throws WGAPIException {
        return hasContent(strLanguage, WGContent.STATUS_RELEASE);
    }
    
    /**
     * Tests if struct entry has content in given language code / status, even if the user doesn't have the authorisation to read it.
     * If language code parameter is null, proves on any language code.
     * If status parameter is null, proves on any status.
     * @throws WGAPIException 
     */
    public boolean hasContent(String strLanguage, String strStatus) throws WGAPIException {
        
        // Look at cache first
        boolean needArchive = WGContent.STATUS_ARCHIVE.equals(strStatus);
        if (alreadyFetched(needArchive)) {
            if (strLanguage != null && strStatus != null) {
                WGContent con = getContent(strLanguage, strStatus);
                if (con != null) {
                    return true;
                }
            }
            else if (strLanguage == null && strStatus == null) {
                if (getContentSet(needArchive).getAllContent().size() > 0) {
                    return true;
                }
            }
        }
        
        // If only pure existence of any content is tested
        // we redirect to the optimized WGDatabase.hasContent() method
        if (strLanguage == null && strStatus == null) {
            return (getDatabase().hasContents(this));
        }
        
        // Try to use optimized backend service
        if (getDatabase().isBackendServiceSupported(WGDatabase.BACKENDSERVICE_PROBE_CONTENT)) {
            return (Boolean) getDatabase().callBackendService(WGDatabase.BACKENDSERVICE_PROBE_CONTENT, new Object[] {this, strLanguage, strStatus});
        }
        
        // Fallback implementation, checking for contents in a separate thread
        TestForContent test = new TestForContent(strLanguage, strStatus);
        return test.doesContentExist();
    }

    /**
     * Returns the number of content documents in any status or language, that are located at this struct entry.
     * This includes content documents that are invisible to the current user because of reader settings, so this
     * method may be used to determine the existence of such documents while they cannot be retrieved directly.
     * @throws WGAPIException 
     */
    public int getContentCount() throws WGAPIException {
        
        if (getContentCountCache() == null) {
            
            // Technique 1: Try native content count functionalty (in-session) from backend
            try {
                setContentCountCache(new Integer(getDatabase().getCore().getContentCount(this)));
                return getContentCountCache().intValue();
            }
            catch (WGNotSupportedException e) {
            }
            catch (WGAPIException e) {
                WGFactory.getLogger().error("Exception retrieving content count from " + getDocumentKey(), e);
            }
            
            
            // Techique 2: Retrieve number of contents from master session
            MasterSessionTask task = new MasterSessionTask(getDatabase()) {
                protected void exec(WGDatabase db) throws Throwable {
                    setResult(new Integer(WGStructEntry.this.getAllContent(true).size()));
                }
            };
            
            if (task.run() == true) {
                setContentCountCache(((Integer) task.getResult()));;
            }
            else {
                throw new WGAPIException("Exception retrieving content count", task.getThrowable());
            }
        }
        
        return getContentCountCache().intValue();
        
    }

    /**
     * Returns all released content documents under this struct entry, keyed by
     * their language code.
     * @throws WGAPIException 
     */
    public Map getAllReleasedContent() throws WGAPIException {
        return new HashMap(getContentSet(false).getReleasedContent());
    }

    /**
     * @throws WGAPIException 
     * @see WGDocument#retrieveCore()
     */
    protected WGDocumentCore retrieveCore() throws WGAPIException {
        return this.db.getCore().getStructEntryByKey(this._retrievalKey);
    } 

    /**
     * Returns the next sibling by siblings index.
     * @throws WGAPIException 
     */
    public WGStructEntry getNextSibling() throws WGAPIException {

        WGStructEntryList siblings = this.getSiblingEntries();
        int idx = siblings.getIndexOfEntry(this);
        return siblings.getByIndex(++idx);

    }

    /**
     * Returns the previous sibling by siblings index
     * @throws WGAPIException 
     */
    public WGStructEntry getPreviousSibling() throws WGAPIException {

        WGStructEntryList siblings = this.getSiblingEntries();
        int idx = siblings.getIndexOfEntry(this);
        return siblings.getByIndex(--idx);

    }


    /**
     * Tests if the current user may edit this struct entry and it's contents.
     * 
     * @return null, if the user may edit. If not, the WGStructEntry or WGArea
     *         object ist returned that denies editing access
     * @deprecated Because of counter-intuitive return values. Use {@link #performChildCreationCheck()} or {@link #mayEditChildPages()}
     * @throws WGAPIException 
     */
    public WGDocument mayEditEntryAndContent() throws WGAPIException {

        return testEditPageHierarchyRights();
    }
    
    /**
     * Test if user may create content in a given language
     * @param lang
     * @return true if content creation is allowed
     * @throws WGAPIException
     */
    public boolean mayCreateContent(WGLanguage lang) throws WGAPIException{
        // Test for read access. Without read access no write access (not even for managers)
        WGDocument prohibitingDoc = getReadProtectionCause();
        if (prohibitingDoc != null) {
            return false;
        }
        
        // Ask PageRightsFilter first end stop other checks if ALLOWED_SKIP_DEFAULT_CHECKS
        PageRightsFilter.Right right = getDatabase().getPageRightsFilter().mayEditContent(this, getDatabase().getSessionContext().getUserAccess(), lang);
        if (right == Right.DENIED) 
        	return false;
        else if (right == Right.ALLOWED_SKIP_DEFAULT_CHECKS)
        	return true;
        
    	return lang.mayCreateContent() && mayEditPage();
    }
    
    /**
     * Tests if the current user generally may edit this struct entry and it's contents, according to the hierarchical access rights. If so the method exits normally. Otherwise an rexception is thrown.
     */
    public void performEditPageCheck() throws WGAPIException {
        
        WGDocument restrictingDoc = testEditPageHierarchyRights();
        if (restrictingDoc != null) {
            String code = (restrictingDoc instanceof WGArea ? WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA : WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE);
            throw new WGAuthorisationException("The document " + restrictingDoc.getDocumentKey() + " denies editing this page", code, restrictingDoc);
        }
        
    }
    
    /**
     * Tests if the current user generally may edit this struct entry and it's contents, according to the hierarchical access rights.
     */
    public boolean mayEditPage() throws WGBackendException {
        
        try {
            performEditPageCheck();
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
     * Tests if the current user generally may move this struct entry.
     */
    public boolean mayMovePage() throws WGBackendException {
    	return mayEditPage() && getDatabase().getSessionContext().getUserAccess().mayMoveStructEntries();
    }


    protected WGDocument testEditPageHierarchyRights() throws WGAPIException {
        // returns null if user may edit Entry
        // Otherwise returns WGStructEntry, which prohibits user from editing
        // this Entry.

        // Test for read access. Without read access no write access (not even for managers)
        WGDocument prohibitingDoc = getReadProtectionCause();
        if (prohibitingDoc != null) {
            return prohibitingDoc;
        }
        
        // Ask PageRightsFilter first end stop other checks if ALLOWED_SKIP_DEFAULT_CHECKS
        PageRightsFilter.Right editRight = getDatabase().getPageRightsFilter().mayEditPage(this, getDatabase().getSessionContext().getUserAccess());
        if (editRight == PageRightsFilter.Right.DENIED) 
        	return this;
        else if (editRight == PageRightsFilter.Right.ALLOWED_SKIP_DEFAULT_CHECKS)
        	return null;

        // test if user has at least chief editor rights, if so grant access in anyway.
        if (this.getDatabase().getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
            return null;
        }

        // Retrieve the list of editors. If we have retrieved backend data we prefer that one
        // bc. the current data may have been modified
        List<String> editors = getEffectivePageEditors();
        
        // Case 1: Anyone allowed
        if (WGDatabase.anyoneAllowed(editors, false)) {
            return null;
        }

        // Case 2: regular struct-document - no editor is assigned, so lookup editors from parent-Struct
        if (editors.isEmpty() || (editors.size() == 1 && (editors.get(0) == null || editors.get(0).toString().equals("")))) {
            return testInheritedEditRights();
        }
        
        // Case 3: nobody is allowed to edit children of this struct.
        if (editors.size() == 1 && editors.get(0).toString().equals(WGStructEntry.NOONE_ALLOWED)) {
            return this;
        }
        
        // Case 4: A list of users is given. If the user is not contained in the list he may not edit
        
        // The list may choose to inherit parent rights and reduce them
        if (EDITORS_INHERIT_AND_REDUCE.equals(editors.get(0))) {
            WGDocument cause = testInheritedEditRights();
            if (cause != null) {
                return cause;
            }
            editors = new ArrayList<String>(editors);
            editors.remove(0);
        }
        
        if  (!this.getDatabase().isMemberOfUserList(editors)) {
            return this;
        }

        // everything is cool, user edit Entry
        return null;
        
    }
    
    /**
     * Tests if the current user may modify each document of the current pages subtree.
     * This is intended to be tested on operations that involve each document of a page subtree including the current one, like moving it.
     * The method exits normally if the user may do this operation or with an {@link WGAuthorisationException} if he does not, holding detail information about the cause.
     */
    public void performSubtreeModificationCheck() throws WGAPIException, WGAuthorisationException {
        
        Iterator<WGStructEntry> it = getChildEntryIterator(10);
        while (it.hasNext()) {
            
            WGStructEntry child = it.next();
            
            WGDocument causeDoc = child.testEditPageHierarchyRights();
            if (causeDoc != null) {
                if (causeDoc instanceof WGStructEntry) {
                    WGStructEntry causeStruct = (WGStructEntry) causeDoc;
                    throw new WGAuthorisationException("Page " + causeStruct.getDocumentKey() + " prevents you from modifying the given page subtree", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE , causeStruct);
                }
                else if (causeDoc instanceof WGArea) {
                    WGArea causeArea = (WGArea) causeDoc;
                    throw new WGAuthorisationException("Area " + causeArea.getName() + " prevents you from modifying the given page subtree", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, causeArea);
                }
                else  {
                    throw new WGAuthorisationException("Document " + causeDoc.getDocumentKey() + " prevents you from modifying the given page subtree", causeDoc);
                }
            }
            
            if (child.getContentCount() != child.getAllContent(true).size()) {
                throw new WGAuthorisationException("A read protected document, invisible to you, prevents you from modifying the given page subtree", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_INVISIBLE_CONTENT);
            }
            
            child.performSubtreeModificationCheck();
            
        }
        
    }

    
    /**
     * Tests if the current user may read contents on this page and below, and if so returns the document whose settings prohibit it.
     * @return null, if the user may read. If not, the WGStructEntry or WGArea
     *         object ist returned that denies reading access
     * @throws WGAPIException
     * @{@link Deprecated} Not all read protection is caused in documents. On these non-document causes this method returns the current page. Better use {@link #performReadProtectionCheck()} which throws more informative exceptions.
     */
    public WGDocument getReadProtectionCause() throws WGAPIException {
        
        try {
            performReadProtectionCheck();
            return null;
        }
        catch (WGAuthorisationException e) {
            if (e.getCauseDocumentKey() != null) {
                WGDocument doc = getDatabase().getDocumentByKey(e.getCauseDocumentKey());
                if (doc != null) {
                   return doc;
                }
            }

            
            return this;
        }
        
    }
    
    public void performReadProtectionCheck() throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        PageRightsFilter.Right readRight = getDatabase().getPageRightsFilter().mayReadContent(this, getDatabase().getSessionContext().getUserAccess());
        if (readRight ==  Right.DENIED) {
        	List<String> mandatory = getDatabase().getMandatoryReaders();
        	if (!getDatabase().isMemberOfUserList(mandatory)) {        	
        		throw new WGAuthorisationException("The page rights filter denies reading content on this page", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGERIGHTSFILTER);
        	}
        }
        
        if (readRight != Right.ALLOWED_SKIP_DEFAULT_CHECKS) {
        
            // Are hierarchical reader fields enabled ?
            if (!getDatabase().isPageReadersEnabled()) {
                return;
            }
            
            // No read protection on master session
            if (getDatabase().getSessionContext().isMasterSession()) {
                return;
            }
            
            // Parent check
            if (isRoot()) {
                if (!getArea().mayReadContent()) {
                    throw new WGAuthorisationException("The area denies reading content on this page", getArea());
                }
            }
            else {
                WGDocument prohibitingDoc = getParentEntry().getReadProtectionCause();
                if (prohibitingDoc != null) {
                    throw new WGAuthorisationException("The document '" + prohibitingDoc.getDocumentKey() + " denies reading content on this page", prohibitingDoc);
                }
            }
                
            // Local check
            List<String> readers = getEffectiveReaders();
            if (!WGDatabase.anyoneAllowed(readers, true)) {
                readers.addAll(getDatabase().getMandatoryReaders());
                if (!getDatabase().isMemberOfUserList(readers)) {
                    throw new WGAuthorisationException("The rights of this page deny reading its contents",  this);
                }
            }
        
        }
        
    }
    
    /**
     * Tests if this page branch is publicly accessible, i.e. anyone can generally read its content
     * @throws WGAPIException
     */    
    public boolean isPublic() throws WGAPIException {
        
        if (isRoot()) {
            if (!getArea().isPublic()) {
                return false;
            }
        }
        else {
            if (!getParentEntry().isPublic()) {
                return false;
            }
        }
        
        List readers = getEffectiveReaders();
        return WGDatabase.anyoneAllowed(readers, true);
    }
    
    /**
     * Tests if the current user may read contents on this page and below
     * @return true if the user may, false if not
     * @throws WGAPIException
     */
    public boolean mayReadContent() throws WGAPIException {
        
        try {
            WGDocument cause = getReadProtectionCause();
            return (cause == null);
        }
        catch (WGAuthorisationException e) {
            return false;
        }
        
    }
    
    /**
     * Tests if the current user may edit child entries and their contents.
     * 
     * @return null, if the user may edit. If not, the WGStructEntry or WGArea
     *         object is returned that denies editing access
     * @deprecated Because of counter-intuitive return values. Use {@link #mayEditChildPages()}
     * @throws WGAPIException 
     */
    public WGDocument mayEditChildren() throws WGAPIException {
        return testEditChildPagesHierarchyRights();
    }
    
    /**
     * Tests if the current user generally may edit child pages of this page, according to the hierarchical access rights.
     * @throws WGAPIException
     */
    public boolean mayEditChildPages() throws WGAPIException {
        
        try {
            return (testEditChildPagesHierarchyRights() == null);
        }
        catch (WGBackendException e) {
            throw e;
        }
        catch (WGAPIException e) {
            return false;
        }
        
    }
    
    /**
     * Performs a check if the current user generally create child pages of this page, according to the hierarchical access rights.
     * Runs through if he may, throws an exception otherwise, identifying the cause of restriction.
     * @throws WGAPIException
     */
    public void performChildCreationCheck() throws WGAPIException {
        
        WGDocument restrictingDoc = testEditChildPagesHierarchyRights();
        if (restrictingDoc != null) {
            String code = (restrictingDoc instanceof WGArea ? WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA : WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE);
            throw new WGAuthorisationException("The document " + restrictingDoc.getDocumentKey() + " denies editing child pages on this page", code, restrictingDoc);
        }
        
    }

    protected WGDocument testEditChildPagesHierarchyRights() throws WGAPIException {
        // returns null if user may edit Children
        // Otherwise returns WGStructEntry, which prohibits user from editing
        // this Entry.
        
        
        PageRightsFilter.Right editRight = getDatabase().getPageRightsFilter().mayEditChildPages(this, getDatabase().getSessionContext().getUserAccess());
        if (editRight ==  Right.DENIED) {
        	return this;
            //throw new WGAuthorisationException("The page rights filter denies editing child pages of this page", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGERIGHTSFILTER);
        }
        
        if (editRight != Right.ALLOWED_SKIP_DEFAULT_CHECKS) {
        
            // test if user is at least chief editor, if so grant access in anyway.
            if (this.getDatabase().getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
                return null;
            }
            
            // Test for read access. Without read access no write acces
            WGDocument prohibitingDoc = getReadProtectionCause();
            if (prohibitingDoc != null) {
                return prohibitingDoc;
            }
    
            // fetch information about Struct-Editors
            List<String> editors = getEffectiveChildEditors();
    
            // Case 1: Anyone allowed
            if (WGDatabase.anyoneAllowed(editors, false)) {
                return null;
            }
    
            // Case 2: regular struct-document - no editor is assigned, so lookup
            // editors from parent-Struct
            if (editors.isEmpty()
                    || (editors.size() == 1 && editors.get(0).toString().equals(""))) {
               
                return testInheritedEditRights();
            }
            
            // Case 3: nobody is allowed to edit children of this struct.
            if (editors.size() == 1 && editors.get(0).toString().equals(WGStructEntry.NOONE_ALLOWED)) {
                return this;
            }
    
            // Case 4: A list of users is given. If the user is not contained in the list he may not edit
            
            // The list may choose to inherit parent rights and reduce them
            if (EDITORS_INHERIT_AND_REDUCE.equals(editors.get(0))) {
                WGDocument cause = testInheritedEditRights();
                if (cause != null) {
                    return cause;
                }
            }
            
            if (!this.getDatabase().isMemberOfUserList(editors)) {
                return this;
            }
            
        }
        
        // Case 5: everything is cool, user may create children :-)
        return null;
        
    }

    private WGDocument testInheritedEditRights() throws WGAPIException {
        if (this.isRoot()) {
            if (!getArea().mayEditPages()) {
               return getArea();
            }
            else {
               return null;
            }

        }
        else {
            return this.getParentEntry().testEditChildPagesHierarchyRights();
        }
    }
    


    /**
     * Create a child struct entry
     * 
     * @param contentType
     *            Content type for new entry
     * @param title
     *            Title of new entry
     * @return The newly created entry
     * @throws WGAPIException 
     */
    public WGStructEntry createChildEntry(WGContentType contentType,
            String title) throws WGAPIException {
        return this.getDatabase().createStructEntry(this, contentType, title);
    }

    /**
     * Create a content document under this struct entry.
     * 
     * @param language
     *            The language for the content
     * @param title
     *            The title of the content
     * @return The newly created content
     * @throws WGAPIException
     */
    public WGContent createContent(WGLanguage language, String title)
            throws WGAPIException {
        return this.getDatabase().createContent(this, language, title);
    }
    
    /**
     * Create a content document under this struct entry.
     * 
     * @param language
     *            The language for the content
     * @param title
     *            The title of the content
     * @return The newly created content
     * @throws WGAPIException
     */
    public WGContent createContent(String language, String title)
            throws WGAPIException {
        return this.getDatabase().createContent(this, getDatabase().getLanguage(language), title);
    }
    
    /**
     * Creates a new content for this struct entry, using the default language of the database.
     * @param title The title of the new content
     * @return The newly created content
     * @throws WGAPIException
     */
    public WGContent createContent(String title) throws WGAPIException {
        
        WGLanguage lang = getDatabase().getLanguage(getDatabase().getDefaultLanguage());
        if (lang != null) {
            return createContent(lang, title);
        }
        else {
            throw new WGCreationException("No default language configured for this database");
        }
        
    }

    /*
     * (Kein Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
     */
    public WGDocument createClone(WGDatabase db, WGDocument ref)
            throws WGAPIException {

        if (!(ref instanceof WGStructEntry || ref instanceof WGArea)) {
            return null;
        }

        WGContentType contentType = db.getContentType((String) getMetaData(META_CONTENTTYPE));
        if (contentType == null) {
            throw new WGCreationException("The content type '" + getMetaData(META_CONTENTTYPE) + "' is not available in target database");
        }
        
        WGStructEntry newEntry = db.createStructEntry(getStructKey(), ref, contentType,
                getTitle());
        pushData(newEntry);

        newEntry.saveWithGivenTimestamps(getCreated(), getLastModified());
        return newEntry;
        

    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGStructEntry entry = (WGStructEntry) doc;
        entry.setPosition(getPosition().intValue());
        
        entry.setMetaData(META_WORKFLOW_NAME, getMetaData(META_WORKFLOW_NAME));
        entry.setMetaData(META_CONTENTTYPE, getMetaData(META_CONTENTTYPE));
        entry.setReaders(new ArrayList(getReaders()));
        entry.setChildEditors(new ArrayList(getChildEditors()));
        entry.setPageEditors(new ArrayList(getPageEditors()));
        entry.setTitle(getTitle());
        
        if (doc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            
            if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
                entry.setUniqueName(getUniqueName());
                entry.setPublished(new HashMap(getPublished()));
            }
            
            // If the source database is no CS5, but the target is, we initialize the pagepublished values with the creation dates of the first versions per language
            else {
                entry.initializePagePublished(this);
            }
        }
        
        boolean wasRepositioned = isDifferentParent(entry);

        // We must test the target parents for existence. They may not yet exist (see #00000640)
        if (wasRepositioned) {
            if (this.getParentEntry() == null) {
                WGArea targetArea = entry.getDatabase().getArea(getArea().getName());
                if (targetArea != null) {
                    entry.getDatabase().moveStructEntry(entry, targetArea);
                }
            }
            
            else {
                WGStructEntry targetEntry = entry.getDatabase().getStructEntryByKey(getParentEntry().getStructKey());
                if (targetEntry != null) {
                    entry.getDatabase().moveStructEntry(entry, targetEntry);
                }
            }

        }
        
        super.pushData(doc);        

    }

    public boolean mayPushExtData(String extName){
    	if(extName.equals(EXT_PAGE_SEQUENCE))
    		return false;
    	return super.mayPushExtData(extName);
    }

    
    /**
     * Initializes empty published fields on page level by the first released content versions' creation date
     * This is a tool method that can be used to init given published field from non CS5 sources, on migration for example.
     * @param source The source page whose contents should be used to init the published dates. This may be the current page but also a diffent one (migration, sync). 
     * @throws WGAPIException
     */
    public void initializePagePublished(WGStructEntry source) throws WGAPIException {

        Map<String,Date> published = new HashMap<String, Date>();
        for (WGLanguage lang : getDatabase().getLanguages().values()) {
            
            // Look if we already have a published date for this language
            Date originalPublished = getPublished().get(lang.getName());
            if (originalPublished != null) {
                published.put(lang.getName(), originalPublished);
                continue;
            }
            
            // Take the currently released document as starting point. If we have none we quit the language
            WGContent releasedDoc = source.getReleasedContent(lang.getName());
            if (releasedDoc == null) {
                continue;
            }
            
            // Iterate over the content versions from released version backwards. Take the lowest available version
            WGContent versionDoc = releasedDoc;
            Date lowestCreatedDate = null;
            do {
                if (versionDoc.getStatus().equals(WGContent.STATUS_RELEASE) || versionDoc.getStatus().equals(WGContent.STATUS_ARCHIVE)) {
                    lowestCreatedDate = versionDoc.getCreated();
                }
                
                if (versionDoc.getVersion() > 1) {
                    versionDoc = source.getContent(lang.getName(), versionDoc.getVersion() - 1);
                }
                else {
                    versionDoc = null;
                }
                
            }
            while (versionDoc != null);
            
            if (lowestCreatedDate != null) {
                published.put(lang.getName(), lowestCreatedDate);
            }
            
        }
        
        setPublished(published);
        
        
    }

    /**
     * Tests if this struct entry has a different parent than the given clone.
     * Used in synchronisation to test if the struct was repositioned since last sync.
     * @param clone The struct entry clone
     * @return true if it was repositioned, false otherwise
     * @throws WGAPIException
     */
    public boolean isDifferentParent(WGStructEntry clone) throws WGAPIException {
        // Look for reposition - We compare document keys bc. the tested documents may be from different databases, yet still represent the same document
        boolean wasRepositioned = false;
        if (this.isRoot() && clone.isRoot()) {
            wasRepositioned = (this.getArea().getDocumentKey().equals(clone.getArea().getDocumentKey()) ? false
                    : true);
        } else if (!this.isRoot() && !clone.isRoot()) {
            wasRepositioned = (this.getParentEntry().getDocumentKey().equals(
                    clone.getParentEntry().getDocumentKey()) ? false : true);
        } else if (this.isRoot() != clone.isRoot()) {
            wasRepositioned = true;
        }
        return wasRepositioned;
    }


    protected boolean remove(WGDocument deletionRoot) throws WGAPIException {

        // We must explicitly call the removal check before innerRemove() because we are gonna delete document before we reach this method
        if (deletionRoot == this) {
            performRemoveCheck(true, deletionRoot);
        }

        // Deleting child contents and struct entries
        if (!db.hasFeature(WGDatabase.FEATURE_AUTOCASCADES_DELETIONS)
                && db.getSessionContext().isCascadeDeletions()) {

            // Delete all content
            Iterator doomed = getAllContent(true).iterator();
            while (doomed.hasNext()) {
                WGContent doomedContent = (WGContent) doomed.next();
                if (!doomedContent.isDeleted()) {
                    if (doomedContent.remove(deletionRoot) == false) {
                        throw new WGBackendException("Unable to delete content " + doomedContent.getContentKey().toString());
                    }
                }
            }

            // There are still contents on this struct entry (that the current user is not allowed to read) so we cancel
            // Should not happen any more since we test this previously
            /*
            if (db.hasContents(this)) {
                throw new WGAuthorisationException("Unable to delete struct entry " + getStructKey() + " because it contains contents that are not under your access");
            }
            */

            // Delete all children
            doomed = getChildEntries().iterator();
            while (doomed.hasNext()) {
                WGStructEntry doomedEntry = (WGStructEntry) doomed.next();
                if (!doomedEntry.isDeleted()) {
                    if (doomedEntry.remove(deletionRoot) == false) {
                        throw new WGBackendException("Unable to delete struct entry " + doomedEntry.getStructKey());
                    }
                }
            }
        }

        return innerRemove(deletionRoot, false);
    }
    


    @Override
    protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAuthorisationException, WGAPIException {
        
        super.performRemoveCheck(deepCheck, deletionRoot);
        
        // Test general right to delete struct entries        
        if (getDatabase().getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_EDITOR) {
            if (getDatabase().getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_AUTHOR) {
                if (hasChildren()) {
                    throw new WGAuthorisationException("You are not allowed to delete this struct entry", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
                }
            }
            else {
                throw new WGAuthorisationException("You are not allowed to delete struct entries", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
            }
        }            

        // Test rights to delete this entry and content
        // Test rights to edit this entry and content
        if (getContentType() != null && !getContentType().mayCreateContent()) {
            throw new WGAuthorisationException("You are not allowed to delete documents of content type '" + getContentType().getName() + "'", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, getContentType());
        }
        
        // Test languages
        for(WGContent content: getAllContent(true)){
        	WGLanguage lang = content.getLanguage(); 
        	if(!lang.mayCreateContent(this)){
        		throw new WGAuthorisationException("You are not allowed to delete documents of language '" + lang.getName() + "'", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_LANGUAGE, lang);
        	}
        }
        
        WGDocument cause = testEditPageHierarchyRights();
        if (cause != null) {
            if (cause instanceof WGStructEntry) {
                WGStructEntry entry = (WGStructEntry) cause;
                throw new WGAuthorisationException(
                        "You cannot delete struct entry " + getStructKey() + " because the settings of struct entry " + entry.getStructKey() + " deny this", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE , entry);
            } else {
                WGArea area = (WGArea) cause;
                throw new WGAuthorisationException(
                        "You cannot delete struct entry " + getStructKey() + " because the settings of area '" + area.getName() + " deny this", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, area);
            }
        }

        if (deepCheck) {
            // Test rights to delete children, if there are any
            if (hasChildren()) {
                cause = testEditChildPagesHierarchyRights();
                if (cause != null) {
                    if (cause instanceof WGStructEntry) {
                        WGStructEntry entry = (WGStructEntry) cause;
                        throw new WGAuthorisationException(
                                "You cannot delete the children of struct entry " + getStructKey() + " because the settings of child entry " + entry.getStructKey() + " deny this", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, entry);
                    } else {
                        WGArea area = (WGArea) cause;
                        throw new WGAuthorisationException(
                                "You cannot delete the children of struct entry " + getStructKey() + " children, because the settings of area '" + area.getName() + " denies this", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, area);
                    }
                }
            }
            
            if (!db.getSessionContext().isMasterSession()) {
                // Test if there are read protected contents on this struct entry that we cannot see
                if (getContentCount() != getAllContent(true).size()) {
                    throw new WGAuthorisationException("You cannot delete struct entry " + getStructKey() + " because it contains read protected contents invisible to you", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_INVISIBLE_CONTENT, this);
                }
            }
            
            
            if (getDatabase().getSessionContext().isProtectedRelationsEnabled() && !getDatabase().getSessionContext().isMasterSession()) {
                // Perform removal check on all child contents
                Iterator contents = getAllContent(true).iterator();
                while (contents.hasNext()) {
                    ((WGContent) contents.next()).performRemoveCheck(deepCheck, deletionRoot);
                }
                
                // Perform removal check on all child entries
                Iterator entries = getChildEntryIterator(10);
                while (entries.hasNext()) {
                    ((WGStructEntry) entries.next()).performRemoveCheck(deepCheck, deletionRoot);
                }
            }
        }
        
    }

    /**
     * Determines if this page is already "active", i.e. it contains contents that are no more in draft state or child pages
     * @throws WGAPIException
     */
    public boolean isActive() throws WGAPIException {
        
        if (hasChildren()) {
            return true;
        }
        
        // First do with un-archived docs only. Will save performance if we exit here
        for (WGContent con : getAllContent(false)) {
            if (!con.getStatus().equals(WGContent.STATUS_DRAFT)) {
                return true;
            }
        }
        
        // Then retrieve archived docs. If we didn't find any non-draft documents yet it is unlikely that we will find many archived ones now (unless we have a real "archived page")
       if (getContentSet(true).getArchivedContent().size() > 0) {
           return true;
       }

        
        return false;
    }

    protected boolean save(Date lastModified, boolean performTests) throws WGAPIException {
        try {
            getSessionData().setPerformUserRightsTestsOnSave(performTests);
            boolean result = super.save(lastModified);
            gatherRetrievalKeys();
            return result;
        }
        finally {
            getSessionData().setPerformUserRightsTestsOnSave(false);
        }
    }
    
    public boolean save(Date lastModified) throws WGAPIException {
        return save(lastModified, true);
    }
    
    @Override
    public boolean maySave() throws WGAPIException {

        try {
            getSessionData().setPerformUserRightsTestsOnSave(true);
            return super.maySave();
        }
        finally {
            getSessionData().setPerformUserRightsTestsOnSave(false);
        }
        
    }

    public void performSaveCheck() throws WGAPIException, WGAuthorisationException {
        
        super.performSaveCheck();        
        
        // Test rights to edit this entry and content
        if (getContentType() != null && !getContentType().mayCreateContent()) {
            throw new WGAuthorisationException("You are not allowed to edit documents of content type '" + getContentType().getName() + "'", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, getContentType());
        }
        
        boolean performUserTests = getSessionData().isPerformStatusTestsOnSave();
        if (performUserTests) {
            if (getDatabase().getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_EDITOR) {
                if (getDatabase().getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_AUTHOR) {
                    if (!isTemporary()) { // Bypass if struct is not yet persistently saved
                        if (hasChildren()) {
                            throw new WGAuthorisationException("You are not allowed to modify the settings on struct entries", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
                        }
                        
                        for (WGContent con : getAllContent(false)) {
                            if (!con.getStatus().equals(WGContent.STATUS_DRAFT) || !con.isAuthorOrOwner()) {
                                throw new WGAuthorisationException("You are not allowed to modify the settings on struct entries", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);    
                            }
                        }
                    }
                }
                else {
                    throw new WGAuthorisationException("You are not allowed to edit struct entries", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
                }
            }
        
            WGDocument cause = testEditPageHierarchyRights();
            if (cause != null) {
                if (cause instanceof WGStructEntry) {
                    WGStructEntry entry = (WGStructEntry) cause;
                    throw new WGAuthorisationException(
                            "You cannot edit this struct entry, because the settings of struct entry " + entry.getStructKey() + " deny this", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, entry);
                } else {
                    throw new WGAuthorisationException(
                            "You cannot edit struct entries in this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, cause);
                }
            }
        }
        
        // Check if Unique Name exists
        if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && 
                getDatabase().getSessionContext().isTestUniqueNames()) {
            boolean isUniqueNameOK = testUniqueNameUniqueness(getUniqueName(), null);

            if (!isUniqueNameOK) {
                throw new WGDuplicateKeyException("Unique name '" + this.getUniqueName() + "' already exists.");
            }
        }
    }

    /**
     * Returns the fetch state for content of this structentry as constant
     * FETCHSTATE_... WGA clients may determine by this state how expensive a
     * desired fetch operation might be. E.g. retrieving content including
     * archived versions may be expensive if it has not been fetched yet
     * Otherwise, everything that is already fetched is cheap to retrieve.
     */
    public int getContentFetchState() {
        return getAllContentCacheFetchState();
    }

    /**
     * Returns a path consisting of the structkeys from root entry to this
     * entry, divided by slashes. This information can be used as an easy
     * reference for the position of this entry in the area's hierarchy
     * @throws WGAPIException 
     */
    public String getStructKeyPath() throws WGAPIException {

        List path = new ArrayList();
        WGStructEntry entry = this;
        while (entry != null) {
            path.add(String.valueOf(entry.getStructKey()));
            entry = entry.getParentEntry();
        }

        Collections.reverse(path);
        return WGUtils.serializeCollection(path, "/");

    }

    /**
     * Returns a path consisting of the entry titles from root entry to this
     * entry, divided by slashes This information can be used as an easy
     * reference for the position of this entry in the area's hierarchy
     * @throws WGAPIException 
     */
    public String getTitlePath() throws WGAPIException {

        List path = new ArrayList();
        WGStructEntry entry = this;
        while (entry != null) {
            path.add(String.valueOf(entry.getTitle()));
            entry = entry.getParentEntry();
        }

        Collections.reverse(path);
        return WGUtils.serializeCollection(path, "/");

    }

    /**
     * Determines if this struct entry is a descendant of the given document, i.e.
     * is a part of it's child hierarchy.
     * 
     * @param doc
     *            The document that may be a descendant. Allows {@link WGStructEntry} and {@link WGArea} type documents
     * @return yes if it is a descendant of the given entry, false if not
     * @throws WGAPIException 
     */
    public boolean isDescendantOf(WGDocument doc) throws WGAPIException {

        if (doc instanceof WGArea) {
            return getArea().equals(doc);
        }
        else if (doc instanceof WGStructEntry){
            WGStructEntry entry = (WGStructEntry) doc;
            WGStructEntry parent = getParentEntry();
            while (parent != null) {
                if (parent.equals(entry)) {
                    return true;
                }
                parent = parent.getParentEntry();
            }
            return false;
        }
        else {
           return false;
        }

    }
    
    protected boolean knowsParentEntry() {
        return (this.getParentEntryCache() != null);
    } 
    
    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#getParentLockable()
     */
    public Lockable getParentLockable() throws WGAPIException {
        WGStructEntry parentEntry =  getParentEntry();
        if (parentEntry != null) {
            return parentEntry;
        }
        else {
            return getArea();
        }

    }
    
    /**
     * Returns the set of contents for the current WGStructEntry
     * @param includeArchived Set true to also retrieve archived contents
     * @return The content set
     * @throws WGAPIException
     */
    public ContentSet getContentSet(boolean includeArchived) throws WGAPIException {
        
        // Test for read access
        if (!mayReadContent()) {
            return new ContentSet();
        }
        
        // This set is returned. It is filled either from cache or from backend database
        ContentSet contentSet = null;
        
        // We retrieve the cached contents. We can bypass this potentially expensive operation if we know the needed set has not yet been fetched
        // or caching is disabled
        List<WGContent> cachedContents = null;
        if (alreadyFetched(includeArchived) && isCachingEnabled()) {
            cachedContents = (List<WGContent>) getDatabase().fetchDocumentListCache(this.getAllContentCache(), WGDocument.TYPE_CONTENT);
        }
        
        // Double checked locking
        if (cachedContents == null) {
            int operationType = (includeArchived ? WGOperationKey.OP_STRUCT_CONTENTS_INCLUDING_ARCHIVED : WGOperationKey.OP_STRUCT_CONTENTS);
            WGOperationKey op = db.obtainOperationKey(operationType, String.valueOf(getStructKey()));
            synchronized (op) {
        
                try {
                    op.setUsed(true);
                    if (alreadyFetched(includeArchived) && isCachingEnabled()) {
                        cachedContents = (List<WGContent>) getDatabase().fetchDocumentListCache((WGDocumentListCache) this.getAllContentCache(), WGDocument.TYPE_CONTENT);
                    }
                    
                    if (cachedContents == null) {
                        
                        WGContentList contentList = this.db.getAllContent(this, includeArchived);
                        
                        // If cache writing enabled we will write the contentSet to the cache
                        if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
                            setAllContentCache(WGDocumentListCache.buildFromDocuments(contentList));
                            setAllContentCacheFetchState(includeArchived ? FETCHSTATE_ARCHIVED : FETCHSTATE_ACTIVE_ONLY);
                            for (WGContent content : contentList) {
                                if (WGContent.STATUS_RELEASE.equals(content.getStatus())) {
                                    getReleasedContentCache().put(content.getLanguage().getName(), content.getDocumentKeyObj());
                                }
                            }
                        }
                        
                        contentSet = new ContentSet(contentList);
                    }
                }
                finally {
                    op.setUsed(false);
                }
            }
        }
        
        // Fill buffers from cache
        // Might be ineffective to build the set on each request from a list. Should we cache a more organized form?
        if (contentSet == null && cachedContents != null) {
            contentSet = new ContentSet(cachedContents);
        }

       return contentSet;
        
        
    }

    /**
     * Returns the index of the current struct entry in the list of it's siblings
     * @throws WGAPIException 
     */
    public int getSiblingIndex() throws WGAPIException {
        return getSiblingEntries().getIndexOfEntry(this);
    }

    public Class<?> getChildNodeType() {
        return WGStructEntry.class;
    }

    public List<PageHierarchyNode> getChildNodes() throws WGAPIException {
        return new ArrayList<PageHierarchyNode>(getChildEntries());
    }
    
    @Override
    public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
        return getChildEntryIterator(pageSize);
    }
    
    public String getNodeKey() throws WGAPIException {
        return getDocumentKey();
    }

    public String getNodeTitle(String language) throws WGAPIException {
        WGContent con = getReleasedContent(language);
        if (con != null) {
            return con.getTitle();
        }
        else {
            return getTitle();
        }
    }

    public PageHierarchyNode getParentNode() throws WGAPIException {
        if (isRoot()) {
            return getArea();
        }
        else {
            return getParentEntry();
        }
    }
    
    /**
     * Returns the archived contents for one language, as list in the order of their version numbers.
     * @param language The language to retrieve archived contents for
     * @return List of archived contents of the given language, ordered by version ascending
     * @throws WGAPIException
     */
    public List<WGContent> getArchivedContent(String language) throws WGAPIException {
        
        ContentSet set = getContentSet(true);
        List<WGContent> archivedContent = new ArrayList<WGContent>();
        Iterator<WGContent> contents = set.getArchivedContent().values().iterator();
        WGContent content;
        while (contents.hasNext()) {
            content = (WGContent) contents.next();
            if (content.getLanguage().getName().equals(language)) {
                archivedContent.add(content);
            }
        }
        
        Collections.sort(archivedContent, new Comparator<WGContent>() {

            public int compare(WGContent o1, WGContent o2) {
                try {
                    int v1 = o1.getVersion();
                    int v2 = o2.getVersion();
                    return v2 - v1;
                }
                catch (WGAPIException e) {
                    throw new RuntimeException("WGAPI Exception comparing content versions", e);
                }
            }
            
        });
        return archivedContent;
        
    }

    protected void updateBackendCaches(WGDocumentCore core) {
        super.updateBackendCaches(core);
        
        try {
            de.innovationgate.webgate.api.WGStructEntry.SessionData sessionData = getSessionData();
            sessionData.setBackendReaders((List) cloneMutableObjects(retrieveMetaData(core, WGStructEntry.META_READERS)));
            sessionData.setBackendPageEditors((List) cloneMutableObjects(retrieveMetaData(core, WGStructEntry.META_PAGE_EDITORS)));
            sessionData.setBackendChildEditors((List) cloneMutableObjects(retrieveMetaData(core, WGStructEntry.META_CHILD_EDITORS)));
        }
        catch (WGAPIException e) {
            WGFactory.getLogger().error("Error updating struct entry backend cache", e);
        }
    }
    
    private SessionData getSessionData() throws WGClosedSessionException {
        
        DocumentContext con = getDocumentSessionContext();
        SessionData data = (SessionData) con.getCustomData();
        if (data == null) {
            data = new SessionData();
            con.setCustomData(data);
        }
        return data;
        
    }
    
    /**
     * Returns the unique name of this struct entry, null if there is none
     * @throws WGAPIException
     */
    public String getUniqueName() throws WGAPIException {
        return (String) getMetaData(META_UNIQUENAME);
    }
    
    /**
     * Sets the unique name of this struct entry
     */
    public void setUniqueName(String name) throws WGAPIException {
        setMetaData(META_UNIQUENAME, name);
    }
    
    /**
     * Returns the dates when this page was first published in its available languages
     * The returned map is keyed by language codes. The corresponding value is the date/time when the page was first published in this language.
     * @throws WGAPIException
     */
    public Map<String,Date> getPublished() throws WGAPIException {
        return (Map<String,Date>) getMetaData(META_PUBLISHED);
    }
    
    protected void setPublished(Map<String,Date> published) throws WGAPIException {
        setMetaData(META_PUBLISHED, published);
    }
    
    /**
     * Implements the visitor pattern on the page hierarchy branch of this struct entry. The visitor visits this and all struct entries below and all of their (non-archived) contents.
     * @param visitor
     * @throws WGAPIException
     */
    public void visit(WGPageVisitor visitor) throws WGAPIException {
        
        visitor.visit(this);
        
        List<WGContent> contents = getAllContent();
        for (WGContent content : contents) {
            visitor.visit(content);
        }
        
        List<WGStructEntry> children = getChildEntries().asList();
        for (WGStructEntry child : children) {
            child.visit(visitor);
        }
        
    }
    
    /**
     * Checks if the current user may create a new child struct entry of the given content type. If so the method exits normally. Otherwise an rexception is thrown.
     * @param ct The content type to use.
     * @throws WGAPIException @throws WGAPIException If the user may not create the document. The exception informs about the reason.
     */
    public void performChildCreationCheck(WGContentType ct) throws WGAPIException {
        getDatabase().performStructCreationCheck(null, this, ct);
    }
    
    /**
     * Checks if the current user may create a new child page with the given data. If so the method exits normally. Otherwise an exception is thrown.
     * @param ct The content type to use for the struct entry
     * @param lang The language to use for the content
     * @throws WGAPIException If the user may not create the document. The exception informs about the reason.
     */
    public void performChildPageCreationCheck(WGContentType ct, WGLanguage lang) throws WGAPIException {
        performChildCreationCheck(ct);
        getDatabase().performContentCreationCheck(null, lang);
    }
    
    /**
     * Creates a new child page, including struct entry and content.
     * Both documents are already saved when the method exists.
     * @param contentType The content type of the page
     * @param title The title of the page that will be used for both struct entry 
     * @param language The language of the content to create. Leave null to use databases default language.
     * @return The content of the created page
     * @throws WGAPIException
     */
    public WGContent createChildPage(WGContentType contentType, String title, String language) throws WGAPIException {
        
        if (language == null) {
            language = getDatabase().getDefaultLanguage();
        }
        
        WGLanguage lang = getDatabase().getLanguage(language);
        performChildPageCreationCheck(contentType, lang);
        
        WGStructEntry entry = createChildEntry(contentType, title);
        entry.save();
        WGContent content = entry.createContent(lang, title);
        content.save();
        return content;
        
    }

    @Override
    public int getType() {
        return WGDocument.TYPE_STRUCTENTRY;
    }
    
    /**
     * Returns a list of allowed readers of contents on this struct entry. This method only returns the readers set on this entry, not those that are effective in hierarchy.
     * This is only supported on content stores of version 5.
     * @return List
     * @throws WGAPIException 
     */
    public List<String> getReaders() throws WGAPIException {
        @SuppressWarnings("unchecked")
        List<String> readers = (List<String>) this.getMetaData(WGArea.META_READERS);
        if (readers != null) {
            return readers;
        }
        else {
            return new ArrayList<String>();
        }
    }
    
    /**
     * Sets the allowed readers for this struct entry. This is only supported on content stores of version 5.
     * @param list
     * @throws WGAPIException 
     */
    public boolean setReaders(List list) throws WGAPIException {
        return setMetaData(META_READERS, list);
    }
    
    /**
     * Returns the list of currently effective readers. These may differ from the currently set readers in a yet unsaved document.
     * @throws WGAPIException
     */
    public List<String> getEffectiveReaders() throws WGAPIException {
        
        List<String> readers = getSessionData().getBackendReaders();
        if (readers == null) {
            readers = getReaders();
        }
        return readers;
        
    }
    
    /**
     * Returns the list of currently effective page editors. These may differ from the currently set editors in a yet unsaved document.
     * @throws WGAPIException
     */
    public List<String> getEffectivePageEditors() throws WGAPIException {
        
        List<String> editors = getSessionData().getBackendPageEditors();
        if (editors == null) {
            editors = getPageEditors();
        }
        return editors;
        
    }
    
    /**
     * Returns the list of currently effective child editors. These may differ from the currently set editors in a yet unsaved document.
     * @throws WGAPIException
     */
    public List<String> getEffectiveChildEditors() throws WGAPIException {
        
        List<String> editors = getSessionData().getBackendChildEditors();
        if (editors == null) {
            editors = getChildEditors();
        }
        return editors;
        
    }
    
    /**
     * Returns the number of child entries without actually retrieving them.
     * Most content store implementations provide optimized functionality to retrieve the count in a performant, resource neutral way. 
     * Use it to determine if a struct entry has children where you do not actually need to retrieve those children. 
     * @return Number of child entries.
     * @throws WGAPIException
     */
    public int getChildEntryCount() throws WGAPIException {
        
        if (this.getChildEntryCountCache() == null) {
            
            WGDatabaseCore dbCore = getDatabase().getCore();
            if (this.getChildEntryCache() != null && this.getChildEntryCache().isComplete()) {
                this.setChildEntryCountCache(this.getChildEntryCache().size());
            }
            else if (dbCore instanceof WGDatabaseCoreFeatureReturnHierarchyCount) {
                this.setChildEntryCountCache(((WGDatabaseCoreFeatureReturnHierarchyCount) dbCore).getChildEntryCount(this));
            }
            else {
                this.setChildEntryCountCache(getChildEntries().size());
            }
            
        }
        return this.getChildEntryCountCache().intValue();
        
    }
    
    /**
     * Returns the effective restrictions regarding childpages of this struct. These are either restrictions set on this struct or, if not set there, on its content type. Returns constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public String getEffectiveChildPageRestrictions() throws WGAPIException {
        
        String restrictions = getChildPageRestrictions();
        if (!PAGERESTRICTION_UNSPECIFIED.equals(restrictions)) {
            return restrictions;
        }
        
        WGContentType ct = getContentType();
        if (ct != null) {
            return ct.getChildPageRestrictions();
        }
        else {
            return PAGERESTRICTION_UNSPECIFIED;
        }
        
        
    }
    
    /**
     * Returns the restrictions regarding childpages of this struct. Returns constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public String getChildPageRestrictions() throws WGAPIException {
        return (String) getMetaData(META_CHILDPAGERESTRICTIONS);
    }
    
    /**
     * Sets the restrictions regarding childpages of this struct. 
     * @param childPages Restriction string. Use constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public void setChildPageRestrictions(String childPages) throws WGAPIException {
        setMetaData(META_CHILDPAGERESTRICTIONS, childPages);
    }
    
    
    /**
     * Returns the allowed content types for child pages of this struct that are actually effective. These are either types set on this struct or, if not set there, on its content type. Returns constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public List<String> getEffectiveAllowedChildTypes() throws WGAPIException {
        
        String restrictions = getChildPageRestrictions();
        if (!PAGERESTRICTION_UNSPECIFIED.equals(restrictions)) {
            return getAllowedChildTypes();
        }
        
        WGContentType ct = getContentType();
        if (ct != null) {
            return ct.getAllowedChildTypes();
        }
        else {
            return Collections.emptyList();
        }

        
    }
    
    /**
     * Returns the allowed content types for child pages of this struct. Only effective when {@link #getChildPageRestrictions()} is {@link #PAGERESTRICTION_FIXEDTYPES}
     * @throws WGAPIException
     */
    public List<String> getAllowedChildTypes() throws WGAPIException {
        return (List<String>) getMetaData(META_ALLOWED_CHILDTYPES);
    }
    
    /**
     * Sets the allowed content types for child pages of this struct.  Only effective when {@link #getChildPageRestrictions()} is {@link #PAGERESTRICTION_FIXEDTYPES}
     * @param types Names of content types
     * @throws WGAPIException
     */
    public void setAllowedChildTypes(List<String> types) throws WGAPIException {
        setMetaData(META_ALLOWED_CHILDTYPES, types);
    }
    
    protected void testChildPageRestrictions(WGContentType childType) throws WGAPIException {
        
        if (getDatabase().getSessionContext().getAccessLevel() == WGDatabase.ACCESSLEVEL_MANAGER) {
            return;
        }
        
        String restriction = getEffectiveChildPageRestrictions();
        if (WGDocument.PAGERESTRICTION_NONE.equals(restriction)) {
            throw new WGAuthorisationException("No child entries are allowed on this page", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, this);
        }
        else if (WGDocument.PAGERESTRICTION_FIXEDTYPES.equals(restriction) && !getEffectiveAllowedChildTypes().contains(childType.getName())) {
            throw new WGAuthorisationException("No child entries of this content type are allowed on this page", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, this);
        }
    }
    
    @Override
    protected List<WGDocumentKey> getCacheParents() {
        List<WGDocumentKey> keys = new ArrayList<WGDocumentKey>();
        if (getParentEntryCache() != null) {
            keys.add(getParentEntryCache());
        }
        if (getAreaCache() != null) {
            keys.add(getAreaCache());
        }
        return keys;
    }

    
    protected boolean isContainedInChildEntryCache(WGStructEntry entry) {
        
        if (getChildEntryCache() != null) {
            return getChildEntryCache().containsKey(entry.getDocumentKeyObj());
        }
        
        return false;
        
    }
    
    @Override
    protected Cache createDocumentCache() {
        return new StructCache();
    }
    
    private StructCache getCache() {
        return (StructCache) _cache;
    }

    private WGDocumentListCache getAllContentCache() {
        return getCache().getAllContentCache();
    }

    private void setAllContentCache(WGDocumentListCache allContentCache) {
        getCache().setAllContentCache(allContentCache);
    }

    private int getAllContentCacheFetchState() {
        return getCache().getAllContentCacheFetchState();
    }

    private void setAllContentCacheFetchState(int allContentCacheFetchState) {
        getCache().setAllContentCacheFetchState(allContentCacheFetchState);
    }

    private WGDocumentKey getAreaCache() {
        return getCache().getAreaCache();
    }

    private void setAreaCache(WGDocumentKey areaCache) {
        getCache().setAreaCache(areaCache);
    }

    private WGDocumentListCache getChildEntryCache() {
        return getCache().getChildEntryCache();
    }

    private void setChildEntryCache(WGDocumentListCache childEntryCache) {
        getCache().setChildEntryCache(childEntryCache);
    }

    private Integer getChildEntryCountCache() {
        return getCache().getChildEntryCountCache();
    }

    private void setChildEntryCountCache(Integer childEntryCountCache) {
        getCache().setChildEntryCountCache(childEntryCountCache);
    }

    private Integer getContentCountCache() {
        return getCache().getContentCountCache();
    }

    private void setContentCountCache(Integer contentCountCache) {
        getCache().setContentCountCache(contentCountCache);
    }

    protected WGDocumentKey getParentEntryCache() {
        return getCache().getParentEntryCache();
    }

    private Map<String,WGDocumentKey> getReleasedContentCache() {
        return getCache().getReleasedContentCache();
    }

    public long getPageSequence() throws WGAPIException{
    	return (long)getMetaData(META_PAGESEQUENCE);
    }

    public void createPageSequence() throws WGAPIException, InstantiationException, IllegalAccessException{
    	createPageSequence(false);
    }
    public void createPageSequence(boolean forceCreate) throws WGAPIException, InstantiationException, IllegalAccessException{
    	getDatabase().createPageSequence(this, forceCreate);
    }

    public void setPageDisabled(boolean value) throws WGAPIException{
    	setMetaData(META_PAGE_DISABLED, value);
    }
    public boolean isPageDisabled() throws WGAPIException{
    	return (Boolean)getMetaData(META_PAGE_DISABLED);
    }
    
}
