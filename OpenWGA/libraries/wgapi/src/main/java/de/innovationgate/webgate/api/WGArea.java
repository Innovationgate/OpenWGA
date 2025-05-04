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
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGSessionContext.DocumentContext;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;

/**
 * <p>Represents an area of the content database, containing content documents.
 * Areas are treated as separated regions of the content, where each has it's own access controlling.</p>
 */
public class WGArea extends WGSchemaDocument implements PageHierarchyNode {
    
    public class SessionData {
        
        private List backendEditors;

        public List getBackendEditors() {
            return backendEditors;
        }

        public void setBackendEditors(List backendEditors) {
            this.backendEditors = backendEditors;
        }
        
        private List backendReaders;

        public List getBackendReaders() {
            return backendReaders;
        }

        public void setBackendReaders(List backendEditors) {
            this.backendReaders = backendEditors;
        }
        
    }
    
    public static class AreaCache extends WGDocument.Cache {

        private WGDocumentListCache rootEntryCache = null;
        private Integer rootEntryCountCache = null;
        public WGDocumentListCache getRootEntryCache() {
            return rootEntryCache;
        }
        public void setRootEntryCache(WGDocumentListCache rootEntryCache) {
            this.rootEntryCache = rootEntryCache;
        }
        public Integer getRootEntryCountCache() {
            return rootEntryCountCache;
        }
        public void setRootEntryCountCache(Integer rootEntryCountCache) {
            this.rootEntryCountCache = rootEntryCountCache;
        }
        
        @Override
        public void dropRelations() {
            super.dropRelations();
            this.rootEntryCache = null;
            this.rootEntryCountCache = null;
        }
        
    }
	
    public static final String META_READERS = "READERS";
    public static final MetaInfo METAINFO_READERS = new MetaInfo(META_READERS, String.class, Collections.EMPTY_LIST);
    static { 
        METAINFO_READERS.setMultiple(true);
        METAINFO_READERS.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT); 
        METAINFO_READERS.setInputConverter(new MetaConverter() {

            public Object convert(WGDocument doc, MetaInfo metaInfo, Object value) throws WGAPIException {
            	List<String> readers = (List) value;
                // Test if the current user is contained in the list. If not, he is added to avoid self disclosure
                if (!WGDatabase.anyoneAllowed(readers) && !doc.getDatabase().isMemberOfUserList(readers)) {
                    readers.add(doc.getDatabase().getSessionContext().getUser());
                }
                return readers;
            }
            
        });
    };
    
	public static final String META_EDITORS = "EDITORS";
    public static final MetaInfo METAINFO_EDITORS = new MetaInfo(META_EDITORS, String.class, Collections.EMPTY_LIST);
    static { 
    	METAINFO_EDITORS.setMultiple(true);
    };
    
    public static final String META_SYSTEM = "SYSTEM";
    public static final MetaInfo METAINFO_SYSTEM = new MetaInfo(META_SYSTEM, Boolean.class, Boolean.FALSE);
    static {
        METAINFO_SYSTEM.setExtdata(true);
    }
    
    public static final String META_ROOTPAGERESTRICTIONS = "ROOTPAGERESTRICTIONS";
    public static final MetaInfo METAINFO_ROOTPAGERESTRICTIONS = new MetaInfo(META_ROOTPAGERESTRICTIONS, String.class, PAGERESTRICTION_ANY);
    static {
        METAINFO_ROOTPAGERESTRICTIONS.setExtdata(true);
        METAINFO_ROOTPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_ANY);
        METAINFO_ROOTPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_FIXEDTYPES);
        METAINFO_ROOTPAGERESTRICTIONS.addAllowedValue(PAGERESTRICTION_NONE);
    }
    
    public static final String META_ALLOWED_ROOTTYPES = "ALLOWEDROOTTYPES";
    public static final MetaInfo METAINFO_ALLOWED_ROOTTYPES = new MetaInfo(META_ALLOWED_ROOTTYPES, String.class, Collections.EMPTY_LIST);
    static {
        METAINFO_ALLOWED_ROOTTYPES.setMultiple(true);
        METAINFO_ALLOWED_ROOTTYPES.setExtdata(true);
    }
    
	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.webgate.api.WGDocument#WGDocument(WGDatabase, WGDocumentCore)
	 */
	public WGArea(WGDatabase db, WGDocumentCore doc) throws WGAPIException {
		this(db, doc, new WGDocumentObjectFlags());
	}
	
	public WGArea(WGDatabase wgDatabase, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
	    super(wgDatabase, doc, flags);
    }

    /**
	 * Returns the name of this area.
	 * @throws WGAPIException 
	 */
	public String getName() throws WGAPIException {
		return String.valueOf(this.getMetaData(META_NAME));
	}
	
	/**
	 * Returns a list of all (root) entries in this area.
	 * Synonym for getRootEntries()
	 * @return WGStructEntryList
	 * @throws WGSystemException 
	 * @throws WGBackendException 
	 */
    public WGStructEntryList getChildEntries() throws WGAPIException {
		return getRootEntries();
	}
	/**
	 * Returns a list of all root entries in this area.
	 * @return WGStructEntryList
	 * @throws WGSystemException 
	 * @throws WGBackendException 
	 */
    public WGStructEntryList getRootEntries() throws WGAPIException {

	    // Double checked cache (un-synchronized and synchronized)
	    
	    List<WGStructEntry> entries = null;
	    if (isCachingEnabled()) {
	        entries = (List<WGStructEntry>) getDatabase().fetchDocumentListCache(this.getRootEntryCache(), WGDocument.TYPE_STRUCTENTRY);
	    }
		if (entries == null) {
			WGOperationKey op = db.obtainOperationKey(WGOperationKey.OP_STRUCT_ROOTS, getName());
            synchronized (op) {
                try {
                    op.setUsed(true);
                    if (isCachingEnabled()) {
    		       entries = (List<WGStructEntry>) getDatabase().fetchDocumentListCache(this.getRootEntryCache(), WGDocument.TYPE_STRUCTENTRY);
                    }
    			
    				if (!isCachingEnabled() || entries == null) {
    				    entries = fetchRootEntries(null);
                        
                        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
                            Collections.sort(entries);
                        }
    					
    					if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
    						this.setRootEntryCache(WGDocumentListCache.buildFromDocuments(entries));
    					}
    				}
                }
                finally {
                    op.setUsed(false);
                }
			} 
		}
		
		return WGStructEntryList.create(entries);

	}

    protected List<WGStructEntry> fetchRootEntries(WGPageOrderSet pageOrder) throws WGAPIException {
        List<WGStructEntry> entries;
        entries = new ArrayList<WGStructEntry>();
        WGStructEntryRetrievalIterator it = this.db.getRootEntries(this, pageOrder);
        try {
            while (it.hasNext()) {
                WGStructEntry root = it.next();
                entries.add(root);
            }
        }
        finally {
            it.close();
        }
        return entries;
    }
	
	/**
	 * Returns an iterator over the root entries of this area.
	 * Internally the iterator fetches multiple entities at once from the backend for more performant backend retrieval processes. The size of those retrieval pages must be determined as argument..
	 * @param pageSize The size of retrieval pages
	 * @return An iterator over all root entries of this area
	 * @throws WGAPIException
	 */
	public WGStructEntryIterator getRootEntryIterator(int pageSize) throws WGAPIException {
	    return new WGStructEntryIterator(this, pageSize, null);
	}
	
	
    /**
     * Returns an iterator over the root entries of this area, returned in a particular order
     * Internally the iterator fetches multiple entities at once from the backend for more performant backend retrieval processes. The size of those retrieval pages must be determined as argument..
     * @param pageSize The size of retrieval pages
     * @param pageOrder Order expression denoting the order in which pages should be returned
     * @return An iterator over all root entries of this area
     * @throws WGAPIException
     */
    public WGStructEntryIterator getOrderedRootEntryIterator(int pageSize, String pageOrder) throws WGAPIException {
        
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            throw new WGNotSupportedException("This database does not support ordered navigation results");
        }
        
        return new WGStructEntryIterator(this, pageSize, pageOrder);
    }
	
    /**
     * Returns an iterator over the root contents of area in a given language
     * Internally the iterator fetches multiple pages at once from the backend for more performant backend retrieval processes. The size of those retrieval pages must be determined as argument..
     * This method does only return released contents. Pages without released contents in the current language are skipped.
     * @param pageSize The size of retrieval pages
     * @return An iterator over the root contents of this area
     * @throws WGAPIException
     */
    public WGHierarchyContentIterator getRootContentIterator(String language, int pageSize) throws WGAPIException {
        return new WGHierarchyContentIterator(getRootEntryIterator(pageSize), language);
    }
	
	   /**
     * Returns a partial list of the root entries in this area.
     * @param offset The offset of the first entry to retrieve, 0 being the first one
     * @param size The number of entries to retrieve
     * @return WGStructEntryList
     * @throws WGSystemException 
     * @throws WGBackendException 
     */
    public WGStructEntryList getRootEntries(int offset, int size) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            WGStructEntryList entries = getRootEntries();
            int toIndex = offset + size;
            if (toIndex > entries.size()) {
                toIndex = entries.size();
            }
            return WGStructEntryList.create(entries.asList().subList(offset, toIndex));
        }

        // Double checked cache (un-synchronized and synchronized)
        List entries = getDatabase().fetchDocumentListCache(this.getRootEntryCache(), WGDocument.TYPE_STRUCTENTRY, offset, size);
        if (!isCachingEnabled() || entries == null) {
            WGOperationKey op = db.obtainOperationKey(WGOperationKey.OP_STRUCT_ROOTS, getName());
            synchronized (op) {
                try {
                   op.setUsed(true);
                   entries = getDatabase().fetchDocumentListCache(this.getRootEntryCache(), WGDocument.TYPE_STRUCTENTRY, offset, size);
                
                    if (!isCachingEnabled() || entries == null) {
                        
                        entries = fetchRootEntriesPage(offset, size, null);
                        
                        if (isCachingEnabled() && getDatabase().getSessionContext().isCacheWritingEnabled()) {
                            if (this.getRootEntryCache() == null) {
                                this.setRootEntryCache(WGDocumentListCache.buildFromDocuments(entries, offset, size));
                            }
                            else {
                                this.setRootEntryCache(this.getRootEntryCache().mergeWithDocuments(entries, offset, size));
                            }
                        }
                }
                }
                finally {
                    op.setUsed(false);
                }
            } 
        }
        
        return WGStructEntryList.create(entries);

    }
    
    /**
     * Returns a partial list of the root entries of this area, ordered by a special order expression
     * @param offset The offset of the first entry to retrieve, 0 being the first one
     * @param size The number of entries to retrieve
     * @param orderExpression Page order set expression
     * @throws WGAPIException 
     */
    public WGStructEntryList getOrderedRootEntries(int offset, int size, String orderExpression) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            throw new WGNotSupportedException("This database does not support ordered navigation results");
        }

        // Double checked cache (un-synchronized and synchronized)
        WGPageOrderSet pageOrder = WGPageOrderSet.parse(orderExpression);
        return WGStructEntryList.create(fetchRootEntriesPage(offset, size, pageOrder));

    }
    
    /**
     * Returns a list of all root entries in this area, ordered by a special order expression
     * @param orderExpression Page order set expression
     * @return WGStructEntryList
     * @throws WGSystemException 
     * @throws WGBackendException 
     */
    public WGStructEntryList getOrderedRootEntries(String orderExpression) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            throw new WGNotSupportedException("This database does not support ordered navigation results");
        }

        // Double checked cache (un-synchronized and synchronized)
        WGPageOrderSet pageOrder = WGPageOrderSet.parse(orderExpression);
        return WGStructEntryList.create(fetchRootEntries(pageOrder));

    }

    protected List<WGStructEntry> fetchRootEntriesPage(int offset, int size, WGPageOrderSet pageOrder) throws WGAPIException {
        List<WGStructEntry> entries;
        entries = new ArrayList<WGStructEntry>();
        WGStructEntryRetrievalIterator it = this.db.getRootEntries(this, pageOrder);
        try {
            it.skip(offset);
            int fetched = 0;                        
            
            while (it.hasNext() && fetched < size) {
                WGStructEntry root = it.next();
                entries.add(root);
                fetched++;
            }
        }
        finally {
            it.close();
        }
        return entries;
    }

	/**
	 * Returns a list of allowed content editors in this area.
	 * @return List
	 * @throws WGAPIException 
	 */
	public List getEditors() throws WGAPIException {
		List editors = (List) this.getMetaData(WGArea.META_EDITORS);
		if (editors != null) {
			return editors;
		}
		else {
			return new ArrayList();
		}
	}
	
	   /**
     * Returns a list of allowed readers of contents in this area. This is only supported on content stores of version 5.
     * @return List
     * @throws WGAPIException 
     */
    public List getReaders() throws WGAPIException {
        List readers = (List) this.getMetaData(WGArea.META_READERS);
        if (readers != null) {
            return readers;
        }
        else {
            return new ArrayList();
        }
    }
	
	/**
	 * Sets the allowed editors for this area.
	 * @param list
	 * @throws WGAPIException 
	 */
	public boolean setEditors(List list) throws WGAPIException {
		return setMetaData(META_EDITORS, list);
	}
	
	   /**
     * Sets the allowed readers for this area. This is only supported on content stores of version 5.
     * @param list
     * @throws WGAPIException 
     */
    public boolean setReaders(List list) throws WGAPIException {
        return setMetaData(META_READERS, list);
    }
 
	
	/**
	 * Tests if the current user is allowed to edit documents in this area.
	 * @return boolean
	 * @deprecated Use {@link #mayEditPages()}
	 * @throws WGAPIException 
	 */
	public boolean mayEditAreaChildren() throws WGAPIException {
	    
	    return mayEditPages();
	}

    /**
     * Tests if the current user is generally allowed to edit documents in this area, according to area access rights
     * @throws WGAPIException
     */
    public boolean mayEditPages() throws WGAPIException {
    	
        // Ask PageRightsFilter first and stop other checks if DENIED or ALLOWED_SKIP_DEFAULT_CHECKS
        PageRightsFilter rightsFilter = getDatabase().getPageRightsFilter();
        if(rightsFilter instanceof PageAndAreaRightsFilter) {
        	PageRightsFilter.Right editRight = ((PageAndAreaRightsFilter)rightsFilter).mayEditPages(this, getDatabase().getSessionContext().getUserAccess());
            if (editRight == PageRightsFilter.Right.DENIED) 
            	return false;
            else if (editRight == PageRightsFilter.Right.ALLOWED_SKIP_DEFAULT_CHECKS)
            	return true;
        }
    	
        if( this.db.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_CHIEF_EDITOR ){
            return true;
        }
	    
	    List readers = (List) getEffectiveReaders();
	    if (!WGDatabase.anyoneAllowed(readers) && !this.db.isMemberOfUserList(readers)) {
	        return false;
        }
	
		List editors = (List) this.getEffectiveEditors();
		if (!WGDatabase.anyoneAllowed(editors) && !this.db.isMemberOfUserList(editors)) {
            return false;
        }
		
		return true;
    }
	
	/**
	 * Returns the list of currently effective readers. These may differ from the currently set readers in a yet unsaved document.
	 * @throws WGAPIException
	 */
	public List getEffectiveReaders() throws WGAPIException {
	    
        List readers = getSessionData().getBackendReaders();
        if (readers == null) {
            readers = getReaders();
        }
        return readers;
	    
	}
	
	   /**
     * Returns the list of currently effective editors. These may differ from the currently set editors in a yet unsaved document.
     * @throws WGAPIException
     */
    public List getEffectiveEditors() throws WGAPIException {
        
        List editors = getSessionData().getBackendEditors();
        if (editors == null) {
            editors = getEditors();
        }
        return editors;
        
    }
	
	/**
	 * Creates a new root entry for this area.
	 * @param contentType The content type for this new entry.
	 * @param title The title of this new entry.
	 * @return A newly created root entry
	 * @throws WGAPIException 
	 */
	public WGStructEntry createRootEntry(WGContentType contentType, String title) throws WGAPIException {
		return getDatabase().createStructEntry(this, contentType, title);
	}
	
	/**
	 * Creates a new root page, including struct entry and content.
	 * Both documents are already saved when the method exists.
	 * @param contentType The content type of the page
	 * @param title The title of the page that will be used for both struct entry 
	 * @param language The language of the content to create. Leave null to use databases default language.
	 * @return The content of the created page
	 * @throws WGAPIException
	 */
	public WGContent createRootPage(WGContentType contentType, String title, String language) throws WGAPIException {
	    
	    WGTransaction trans = getDatabase().startTransaction();	    
	    try {
    	    if (language == null) {
    	        language = getDatabase().getDefaultLanguage();
    	    }
    	    
    	    WGLanguage lang = getDatabase().getLanguage(language);
    	    performRootPageCreationCheck(contentType, lang);
    	    
    	    WGStructEntry entry = createRootEntry(contentType, title);
    	    entry.save();
    	    WGContent content = entry.createContent(lang, title);
    	    content.save();
    	    trans.commit();
    	    return content;
	    }
	    finally {
	        if (trans.isOpen()) {
	            trans.rollback();
	        }
	    }
	    
	}
	
	/**
	 * Variant of {@link #createRootPage(WGContentType, String, String)} that always uses default language.
	 * @param contentType
	 * @param title
	 * @return The content of the created page
	 * @throws WGAPIException
	 */
	public WGContent createRootPage(WGContentType contentType, String title) throws WGAPIException {
	    return createRootPage(contentType, title, null);
	}
	
	/**
	 * Sets the name of this area.
	 * @throws WGAPIException 
	 */
	protected boolean setName(String name) throws WGAPIException {
		return setMetaData(META_NAME, name);
	}

	/*
	 * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
	 */
	public WGDocument createClone(WGDatabase db) throws WGAPIException {
		
        WGArea newArea = db.createArea(getName());
		try {
            pushData(newArea);
            newArea.saveWithGivenTimestamps(getCreated(), getLastModified());
            return newArea;
        }
		catch (WGAPIException e) {
            // try to remove previous created area
            try {
                newArea.remove();
            }
            catch (Throwable e1) {
            }

            throw e;
        }        

		
		
	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        WGArea area = (WGArea) doc;
        area.setDescription(getDescription());
		area.setEditors(new ArrayList(getEditors()));
		area.setReaders(new ArrayList(getReaders()));
		super.pushData(doc);
    }

    /* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#remove()
	 */
	protected boolean remove(WGDocument deletionRoot) throws WGAPIException {
		
        // We must explicitly call the removal check before innerRemove() because we are gonna delete document before we reach this method
	    if (this == deletionRoot) {
	        performRemoveCheck(true, deletionRoot);
	    }
		
		if (!db.hasFeature(WGDatabase.FEATURE_AUTOCASCADES_DELETIONS) && db.getSessionContext().isCascadeDeletions()) {
			Iterator doomed = getRootEntries().iterator();
			while (doomed.hasNext()) {
				WGStructEntry doomedEntry = (WGStructEntry) doomed.next();
				if (!doomedEntry.isDeleted()) {
					doomedEntry.remove(this);
				}
			}
		}
		
		
		return innerRemove(deletionRoot, false);
	}

    @Override
    public void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAPIException {
        super.performRemoveCheck(deepCheck, deletionRoot);
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_AREA)) {
            throw new WGIllegalStateException("Removing areas via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
        
        if (!mayEditPages()) {
            throw new WGAuthorisationException("You are not authorized to delete this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA);
        }

        if (deepCheck) {
            // Perform removal check on all child entries
            Iterator<WGStructEntry> entries = getRootEntryIterator(10);
            while (entries.hasNext()) {
                entries.next().performRemoveCheck(deepCheck, deletionRoot);
            }
        }

    }
	
	/**
	 * Creates a new content object as root in this area. This is only usable in content stores without struct entries (e.g. all descendants of SimpleContentSource).
	 * @param key The key for the new content
	 * @param title The title of the new content
	 * @return Newly created content object
	 * @throws WGAPIException
	 */
	public WGContent createContent(Object key, String title) throws WGAPIException {
		return getDatabase().createContent(this, key, title);
	}
	
	/**
	 * Creates a new content as root for this area. This is only usable in content stores without struct entries (e.g. all descendants of SimpleContentSource).
	 * @return Newly created root content.
	 * @throws WGAPIException
	 */
	public WGContent createContent() throws WGAPIException {
		return createContent(null, "");
	}

    public Class<?> getChildNodeType() {
        return WGStructEntry.class;
    }

    public List<PageHierarchyNode> getChildNodes() throws WGAPIException {
        return new ArrayList<PageHierarchyNode>(getRootEntries());
    }
    
    @Override
    public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
        return getRootEntryIterator(pageSize);
    }

    public PageHierarchyNode getParentNode() throws WGAPIException {
        return getDatabase().getAllDocumentsHierarchy().getCollectionForType(WGDocument.TYPE_AREA);
    }

    public String getNodeKey() throws WGAPIException {
        return getDocumentKey().toString();
    }
    
    public String getNodeTitle(String language) throws WGAPIException {
        return getName();
    }

    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException {
        super.performSaveCheck();

        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_AREA)) {
            throw new WGIllegalStateException("Updating areas via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }

        List readers = (List) getEffectiveReaders();
        if (!WGDatabase.anyoneAllowed(readers) && !this.db.isMemberOfUserList(readers)) {
            throw new WGAuthorisationException("You are not authorized to modify read protecting area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA);
        }
        
    }
    
    /**
     * Implements the visitor pattern on the page hierarchy of this area. The visitor visits all struct entries and all of their (non-archived) contents.
     * @param visitor
     * @throws WGAPIException
     */
    public void visit(WGPageVisitor visitor) throws WGAPIException {
        
        visitor.visit(this);
        
        List<WGStructEntry> roots = getRootEntries().asList();
        for (WGStructEntry root : roots) {
            root.visit(visitor);
        }
        
    }
    
    /**
     * Checks if the current user may create a new root struct entry of the given content type. If so the method exits normally. Otherwise an exception is thrown.
     * @param ct The content type to use.
     * @throws WGAPIException If the user may not create the document. The exception informs about the reason.
     */
    public void performRootCreationCheck(WGContentType ct) throws WGAPIException {
        getDatabase().performStructCreationCheck(null, this, ct);
    }

    /**
     * Checks if the current user may create a new root page with the given data. If so the method exits normally. Otherwise an exception is thrown.
     * @param ct The content type to use for the struct entry
     * @param lang The language to use for the content
     * @throws WGAPIException If the user may not create the document. The exception informs about the reason.
     */
    public void performRootPageCreationCheck(WGContentType ct, WGLanguage lang) throws WGAPIException {
        performRootCreationCheck(ct);
        getDatabase().performContentCreationCheck(null, lang);
    }

    @Override
    public int getType() {
        return WGDocument.TYPE_AREA;
    }
    
    /**
     * Returns if this area is marked as being a system area, which contains no publishable content
     * @throws WGAPIException
     */
    public boolean isSystemArea() throws WGAPIException {
        return (Boolean) getMetaData(META_SYSTEM);
    }
    
    /**
     * Sets if this area should be marked a system area.
     * Pages in system areas are not published and may be used for internal purposes by the system.
     * @param system
     * @throws WGAPIException
     */
    public void setSystemArea(boolean system) throws WGAPIException {
        setMetaData(META_SYSTEM, system);
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
    
    protected void updateBackendCaches(WGDocumentCore core) {
        super.updateBackendCaches(core);
        
        try {
            de.innovationgate.webgate.api.WGArea.SessionData sessionData = getSessionData();
            sessionData.setBackendEditors((List) cloneMutableObjects(retrieveMetaData(core, WGArea.META_EDITORS)));
            sessionData.setBackendReaders((List) cloneMutableObjects(retrieveMetaData(core, WGArea.META_READERS)));
        }
        catch (WGAPIException e) {
            WGFactory.getLogger().error("Error updating area backend cache", e);
        }
    }
    
    /**
     * Tests if the current user may read contents in this area
     * @throws WGAPIException
     */
    public boolean mayReadContent() throws WGAPIException {
        
        // Are hierarchical reader fields enabled ?
        if (!getDatabase().isPageReadersEnabled()) {
            return true;
        }
        
        List readers = getEffectiveReaders();
        if (WGDatabase.anyoneAllowed(readers, true)) {
            return true;
        }
        
        readers.addAll(getDatabase().getMandatoryReaders());
        if (getDatabase().isMemberOfUserList(readers)) {
            return true;
        }
        
        return false;
            
        
    }
    
    public int getRootEntryCount() throws WGAPIException {
        
        if (this.getRootEntryCountCache() == null) {
            
            WGDatabaseCore dbCore = getDatabase().getCore();
            if (this.getRootEntryCache() != null && this.getRootEntryCache().isComplete()) {
                this.setRootEntryCountCache(this.getRootEntryCache().size());
            }
            else if (dbCore instanceof WGDatabaseCoreFeatureReturnHierarchyCount) {
                this.setRootEntryCountCache(((WGDatabaseCoreFeatureReturnHierarchyCount) dbCore).getRootEntryCount(this));
            }
            else {
                this.setRootEntryCountCache(getRootEntries().size());
            }
            
        }
        return this.getRootEntryCountCache() .intValue();
        
    }
    
    /**
     * Tests if this area is publicly accessible, i.e. anyone can generally read its content
     * @throws WGAPIException
     */
    public boolean isPublic() throws WGAPIException {
        
        List<String> readers = getEffectiveReaders();
        return WGDatabase.anyoneAllowed(readers, true);
        
    }
    
    /**
     * Returns the restrictions regarding root pages on this area. Returns constants PAGERESTRICTION_...
     * @throws WGAPIException
     */
    public String getRootPageRestrictions() throws WGAPIException {
        return (String) getMetaData(META_ROOTPAGERESTRICTIONS);
    }
    
    /**
     * Sets the restrictions regarding root pages on this area 
     * @param rootPages Restriction string. Use constants PAGERESTRICTION_...s
     * @throws WGAPIException
     */
    public void setRootPageRestrictions(String rootPages) throws WGAPIException {
        setMetaData(META_ROOTPAGERESTRICTIONS, rootPages);
    }
    
    /**
     * Returns the allowed content types for root pages on this area. Only effective when {@link #getRootPageRestrictions()} is {@link #PAGERESTRICTION_FIXEDTYPES}
     * @throws WGAPIException
     */
    public List<String> getAllowedRootTypes() throws WGAPIException {
        return (List<String>) getMetaData(META_ALLOWED_ROOTTYPES);
    }
    
    /**
     * Sets the allowed content types for root pages on this area.  Only effective when {@link #getRootPageRestrictions()} is {@link #PAGERESTRICTION_FIXEDTYPES}
     * @param types Names of content types
     * @throws WGAPIException
     */
    public void setAllowedRootTypes(List<String> types) throws WGAPIException {
        setMetaData(META_ALLOWED_ROOTTYPES, types);
    }
    
    protected void testRootPageRestrictions(WGContentType childType) throws WGAPIException {
        
        if (getDatabase().getSessionContext().getAccessLevel() == WGDatabase.ACCESSLEVEL_MANAGER) {
            return;
        }
        
        String restriction = getRootPageRestrictions();
        if (WGDocument.PAGERESTRICTION_NONE.equals(restriction)) {
            throw new WGAuthorisationException("No root entries are allowed on this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, this);
        }
        else if (WGDocument.PAGERESTRICTION_FIXEDTYPES.equals(restriction) && !getAllowedRootTypes().contains(childType.getName())) {
            throw new WGAuthorisationException("No root entries of this content type are allowed on this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, this);
        }
        
    }
    
    protected boolean isContainedInRootEntryCache(WGStructEntry entry) {
        
        if (getRootEntryCache() != null) {
            return getRootEntryCache().containsKey(entry.getDocumentKeyObj());
        }
        
        return false;
        
    }
    
    
    @Override
    protected Cache createDocumentCache() {
        return new AreaCache();
    }
    
    private AreaCache getCache() {
        return (AreaCache) _cache;
    }

    private WGDocumentListCache getRootEntryCache() {
        return getCache().getRootEntryCache();
    }

    private void setRootEntryCache(WGDocumentListCache rootEntryCache) {
        getCache().setRootEntryCache(rootEntryCache);
    }

    private Integer getRootEntryCountCache() {
        return getCache().getRootEntryCountCache();
    }

    private void setRootEntryCountCache(Integer rootEntryCountCache) {
        getCache().setRootEntryCountCache(rootEntryCountCache);
    }

    public boolean isTrashArea() throws WGAPIException{
    	return getName().startsWith("$trash");
    }
    
}

