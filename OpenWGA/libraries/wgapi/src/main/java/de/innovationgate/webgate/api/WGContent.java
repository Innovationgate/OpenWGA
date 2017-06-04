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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.webgate.api.WGDatabase.FreeContentVersionFinder;
import de.innovationgate.webgate.api.WGSessionContext.DocumentContext;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.fake.WGFakeStructEntry;
import de.innovationgate.webgate.api.locking.Lockable;
import de.innovationgate.webgate.api.workflow.WGAutoApprovalCapableWorkflow;
import de.innovationgate.webgate.api.workflow.WGWorkflow;

/**
 *
 * Represents a content document of this content database.
 */
public class WGContent extends WGDocument implements PageHierarchyNode {
    
    private static final String STORE_AUTHOR_EMAIL = "AuthorEMail";
    /**
     * Sysproperty name to determine of metadata field {@link #META_KEYWORDS} should be included in lucenes "allcontent" field
     */
    public static final String SYSPROPERTY_LUCENE_KEYWORDS_AS_CONTENT = "de.innovationgate.wga.lucene.keywords_as_content";
    
    public static class ContentCache extends WGDocument.Cache {

        private Map<String, Object> _relationCache = new ConcurrentHashMap<String, Object>();
        private Map<String,Boolean> _visibilityCache = new ConcurrentHashMap<String, Boolean>();
        
        public Map<String, Object> getRelationCache() {
            return _relationCache;
        }
        public Map<String, Boolean> getVisibilityCache() {
            return _visibilityCache;
        }
        
        @Override
        public void dropCache() {
            super.dropCache();
            _visibilityCache.clear();
            _relationCache.clear();
        }
        
    }

    public class SessionData {
        private boolean saveEventSemaphore = false;
        private Float searchScore = null;
        private Object searchExplaination = null;
        private boolean performStatusTestsOnSave = true;
        private Map<String,Object> virtualItems = new HashMap<String,Object>();
        private String backendStatus = null;
        private SearchDetails _searchDetails = null;
        
        public boolean isSaveEventSemaphore() {
            return saveEventSemaphore;
        }
        public void setSaveEventSemaphore(boolean saveEventSemaphore) {
            this.saveEventSemaphore = saveEventSemaphore;
        }
        /**
         *@deprecated use {@link #setSearchDetails(SearchDetails)} instead.
         */
        @Deprecated
        public Float getSearchScore() {
            if (_searchDetails != null) {
                return  _searchDetails.getScore();
            }
            else {
                return null;
            }
        }
        /**
         *@deprecated use {@link #setSearchDetails(SearchDetails)} instead.
         */
        @Deprecated
        public void setSearchScore(Float searchScore) {
            if (_searchDetails == null) {
                _searchDetails = new SearchDetails();
            }
            _searchDetails.setScore(searchScore);
        }
        /**
         *@deprecated use {@link #getSearchDetails()} instead.
         */
        @Deprecated
        public Object getSearchExplaination() {
            if (_searchDetails != null) {
                return _searchDetails.getExplanation();
            }
            else {
                return null;
            }
        }
        /**
         *@deprecated use {@link #setSearchDetails(SearchDetails)} instead.
         */
        @Deprecated
        public void setSearchExplaination(Object searchExplaination) {
            if (_searchDetails == null) {
                _searchDetails = new SearchDetails();
            }
            _searchDetails.setExplanation(searchExplaination);
        }
        public boolean isPerformStatusTestsOnSave() {
            return performStatusTestsOnSave;
        }
        public void setPerformStatusTestsOnSave(boolean performStatusTests) {
            this.performStatusTestsOnSave = performStatusTests;
        }
        protected Map getVirtualItems() {
            return virtualItems;
        }
        protected void setVirtualItems(Map virtualItems) {
            this.virtualItems = virtualItems;
        }
        public String getBackendStatus() {
            return backendStatus;
        }
        public void setBackendStatus(String backendStatus) {
            this.backendStatus = backendStatus;
        }
        public SearchDetails getSearchDetails() {
            return _searchDetails;
        }
        public void setSearchDetails(SearchDetails searchDetails) {
            _searchDetails = searchDetails;
        }
        
    }

    
    
    /**
     * Content is in status draft can be edited.
     */
    public static final String STATUS_DRAFT = "w";
    /**
     * Content is reviewed by workflow approvers
     */
    public static final String STATUS_REVIEW = "g";
    /**
     * Content is released and shown online
     */
    public static final String STATUS_RELEASE = "p";
    /**
     * Document is archived and no longer used.
     */
    public static final String STATUS_ARCHIVE = "a";
    
    /**
     * Item to store the replace reason comment (workflow).
     */
    public static final String ITEM_REPLACEREASON = "Ersetzungsgrund";
    
    /**
     * Content is hidden for navigators.
     */
    public static final String DISPLAYTYPE_NAVIGATOR = "nav";
    /**
     * Content is invisible for searches
     */
    public static final String DISPLAYTYPE_SEARCH = "search";
    /**
     * Content is invisible in sitemaps.
     */
    public static final String DISPLAYTYPE_SITEMAP = "sitemap";
    /**
     * Content is not hidden for any navigation facility. 
     * (Not to be used in setHiddenFrom() but in other occurences, where the DISPLAYTYPE constants are used to specify which content should be filtered) 
     */
    public static final String DISPLAYTYPE_NONE = "none";

    /**
     * Virtual link to another content in this database
     */
    public static final String VIRTUALLINKTYPE_CONTENT = "int";
    /**
     * Virtual link to a file on some file container
     */
    public static final String VIRTUALLINKTYPE_FILE = "file";
    /**
     * Virtual link to an attachment file in this document
     */
    public static final String VIRTUALLINKTYPE_INTERNAL_FILE = "intfile";
    /**
     * Virtual link to any URL
     */
    public static final String VIRTUALLINKTYPE_EXTERNAL = "exturl";
    /**
     * Link type was not specified.
     */
    public static final String VIRTUALLINKTYPE_NOT_SPECIFIED = "";
    /**
     * Virtual link to a document with unique name
     */
    public static final String VIRTUALLINKTYPE_UNIQUENAME = "intname";
    
    /**
     * Virtual link to a document with context expression
     */
    public static final String VIRTUALLINKTYPE_CONTEXREXPRESSION = "exp";
    /**
     * Virtual link to a file on another content document
     */
    public static final String VIRTUALLINKTYPE_EXTERNAL_FILE = "extfile";

    /**
     * Virtual link to the primary file of a content document
     */
    public static final String VIRTUALLINKTYPE_PRIMARY_FILE = "primaryfile";
    
    /**
     * The date format used in workflow history entries
     */
    public static final DateFormat DATEFORMAT_HISTORY = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");

    /**
     * Used with WGContentNavigator to determine ascending searchorder when finding content
     */
    public static final int SEARCHORDER_ASCENDING = 1;
    /**
     * Used with WGContentNavigator to determine descending searchorder when finding content
     */
    public static final int SEARCHORDER_DESCENDING = -1;
    
    /**
     * Relation type that will allow deletion of targeted documents without further notice
     */
    public static final int RELATIONTYPE_NORMAL = 1;
    
    /**
     * Relation type that will deny deletion of targeted documents
     */
    public static final int RELATIONTYPE_PROTECTED = 2;

	public static final String META_STRUCTENTRY = "STRUCTENTRY";
    public static final MetaInfo METAINFO_STRUCTENTRY = new MetaInfo(META_STRUCTENTRY, Object.class, null);
    static { METAINFO_STRUCTENTRY.addSynonym("STRUCTKEY"); };
    
	public static final String META_TITLE = "TITLE";
    public static final MetaInfo METAINFO_TITLE = new MetaInfo(META_TITLE, String.class, "");
    static { 
    	METAINFO_TITLE.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
    	METAINFO_TITLE.setInputConverter(TITLE_CONVERTER);
    	METAINFO_TITLE.setOutputConverter(TITLE_CONVERTER);    
    }
    
    public static final String META_BROWSERTITLE = "BROWSERTITLE";
    public static final MetaInfo METAINFO_BROWSERTITLE = new MetaInfo(META_BROWSERTITLE, String.class, null);
    static {
    	METAINFO_BROWSERTITLE.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
        METAINFO_BROWSERTITLE.setExtdata(true);
        METAINFO_BROWSERTITLE.setInputConverter(TITLE_CONVERTER);
        METAINFO_BROWSERTITLE.setOutputConverter(TITLE_CONVERTER);
    }
    
    public static final String META_TITLEPATH = "TITLEPATH";
    public static final MetaInfo METAINFO_TITLEPATH = new MetaInfo(META_TITLEPATH, String.class, null);
    static {
        METAINFO_TITLEPATH.setExtdata(true);
    }
    
	public static final String META_LANGUAGE = "LANGUAGE";
    public static final MetaInfo METAINFO_LANGUAGE = new MetaInfo(META_LANGUAGE, String.class, null);
    static { METAINFO_LANGUAGE.setLowerCase(true); };
    
	public static final String META_STATUS = "STATUS";
    public static final MetaInfo METAINFO_STATUS = new MetaInfo(META_STATUS, String.class, null);
    static {
        METAINFO_STATUS.addAllowedValue(STATUS_ARCHIVE);
        METAINFO_STATUS.addAllowedValue(STATUS_DRAFT);
        METAINFO_STATUS.addAllowedValue(STATUS_RELEASE);
        METAINFO_STATUS.addAllowedValue(STATUS_REVIEW);
        
    };
    
	public static final String META_VERSION = "VERSION";
    public static final MetaInfo METAINFO_VERSION = new MetaInfo(META_VERSION, Integer.class, null);

	public static final String META_SEO_SITEMAP_PRIORITY= "SEO_SITEMAP_PRIORITY";
    public static final MetaInfo METAINFO_SEO_SITEMAP_PRIORITY = new MetaInfo(META_SEO_SITEMAP_PRIORITY, String.class, "0.5");
    static {
        METAINFO_SEO_SITEMAP_PRIORITY.setExtdata(true);
    };
	public static final String META_SEO_SITEMAP_CHANGEFREQ = "SEO_SITEMAP_CHANGEFREQ";
    public static final MetaInfo METAINFO_SEO_SITEMAP_CHANGEFREQ = new MetaInfo(META_SEO_SITEMAP_CHANGEFREQ, String.class, "monthly");
    static {
    	METAINFO_SEO_SITEMAP_CHANGEFREQ.setExtdata(true);
    }
    
    public static final String META_SEO_ROBOTS_INDEX = "SEO_ROBOTS_INDEX";
    public static final MetaInfo METAINFO_SEO_ROBOTS_INDEX = new MetaInfo(META_SEO_ROBOTS_INDEX, Boolean.class, Boolean.TRUE);
    static {
    	METAINFO_SEO_ROBOTS_INDEX.setExtdata(true);
    }
    public static final String META_SEO_ROBOTS_FOLLOW = "SEO_ROBOTS_FOLLOW";
    public static final MetaInfo METAINFO_SEO_ROBOTS_FOLLOW = new MetaInfo(META_SEO_ROBOTS_FOLLOW, Boolean.class, Boolean.TRUE);
    static {
    	METAINFO_SEO_ROBOTS_FOLLOW.setExtdata(true);
    }
    
    
	public static final String META_UNIQUE_NAME = "UNIQUENAME";
    public static final MetaInfo METAINFO_UNIQUE_NAME = new MetaInfo(META_UNIQUE_NAME, String.class, null);
    static { 
        METAINFO_UNIQUE_NAME.setLowerCase(true);
        METAINFO_UNIQUE_NAME.addSynonym("NAME");METAINFO_UNIQUE_NAME.addSynonym("DOCNAME");
    };
    
	public static final String META_IS_HIDDEN_FROM = "ISHIDDENFROM";
    public static final MetaInfo METAINFO_IS_HIDDEN_FROM = new MetaInfo(META_IS_HIDDEN_FROM, String.class, Collections.EMPTY_LIST);
    static { 
        METAINFO_IS_HIDDEN_FROM.setMultiple(true); 
        METAINFO_IS_HIDDEN_FROM.setLuceneSpecialTreatment(true); 
        METAINFO_IS_HIDDEN_FROM.addAllowedValue(DISPLAYTYPE_NAVIGATOR);
        METAINFO_IS_HIDDEN_FROM.addAllowedValue(DISPLAYTYPE_SEARCH);
        METAINFO_IS_HIDDEN_FROM.addAllowedValue(DISPLAYTYPE_SITEMAP);
    };
    
	public static final String META_VISIBLE = "VISIBLE";
    public static final MetaInfo METAINFO_VISIBLE = new MetaInfo(META_VISIBLE, Boolean.class, Boolean.TRUE);
    
	public static final String META_VALID_FROM = "VALIDFROM";
    public static final MetaInfo METAINFO_VALID_FROM = new MetaInfo(META_VALID_FROM, Date.class, null);
    static { METAINFO_VALID_FROM.setLuceneSpecialTreatment(true); };
    
	public static final String META_VALID_TO = "VALIDTO";
    public static final MetaInfo METAINFO_VALID_TO = new MetaInfo(META_VALID_TO, Date.class, null);
    static { METAINFO_VALID_TO.setLuceneSpecialTreatment(true); };
    
	public static final String META_CONTENTCLASS = "CONTENTCLASS";
    public static final MetaInfo METAINFO_CONTENTCLASS = new MetaInfo(META_CONTENTCLASS, String.class, null);
    static {
        METAINFO_CONTENTCLASS.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
	public static final String META_VIRTUAL_LINK = "VIRTUALLINK";
    public static final MetaInfo METAINFO_VIRTUAL_LINK = new MetaInfo(META_VIRTUAL_LINK, String.class, null);
    static {
        METAINFO_VIRTUAL_LINK.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
        METAINFO_VIRTUAL_LINK.setOutputConverter(new MetaConverter() {

            public Object convert(WGDocument doc, MetaInfo metaInfo, Object value) throws WGAPIException {
                if (!(doc instanceof WGContent)) {                    
                    throw new WGIllegalArgumentException("Unable to execute output converter for meta '" + metaInfo.getName() + "'. Given document is no instance of WGContent.");
                } else {
                    WGContent content = (WGContent) doc;
                    String link = (String) value;
                    if (content.getVirtualLinkType().equals(WGContent.VIRTUALLINKTYPE_INTERNAL_FILE) && link.indexOf("/") != -1) {
                        link = link.substring(link.lastIndexOf("/") + 1);
                    }
                    return link;
                }
            }   
        });
        
    };
    
	public static final String META_VIRTUAL_LINK_TYPE = "VIRTUALLINKTYPE";
    public static final MetaInfo METAINFO_VIRTUAL_LINK_TYPE = new MetaInfo(META_VIRTUAL_LINK_TYPE, String.class, VIRTUALLINKTYPE_EXTERNAL);
    static {
        METAINFO_VIRTUAL_LINK_TYPE.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
        
        /* VIRTUAL_LINK_TYPE is now open for injecting custom link types (#00003207)
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_CONTENT);
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_EXTERNAL);
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_FILE);
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_INTERNAL_FILE);
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_EXTERNAL_FILE);
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_UNIQUENAME);
        METAINFO_VIRTUAL_LINK_TYPE.addAllowedValue(VIRTUALLINKTYPE_NOT_SPECIFIED);
        */
    };
    
	public static final String META_LINK_TARGET = "LINKTARGET";
    public static final MetaInfo METAINFO_LINK_TARGET = new MetaInfo(META_LINK_TARGET, String.class, null);
       
	public static final String META_KEYWORDS = "KEYWORDS";
    public static final MetaInfo METAINFO_KEYWORDS = new MetaInfo(META_KEYWORDS, String.class, Collections.EMPTY_LIST);
    static { 
        METAINFO_KEYWORDS.setMultiple(true); 
        METAINFO_KEYWORDS.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
    };
    
	public static final String META_AUTHOR = "AUTHOR";
    public static final MetaInfo METAINFO_AUTHOR = new MetaInfo(META_AUTHOR, String.class, null);
    static { METAINFO_AUTHOR.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT); };
    
    public static final String META_OWNER = "OWNER";
    public static final MetaInfo METAINFO_OWNER = new MetaInfo(META_OWNER, String.class, null);
    static { 
        METAINFO_OWNER.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
        METAINFO_OWNER.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    };

    public static final String META_COAUTHORS = "COAUTHORS";
    public static final MetaInfo METAINFO_COAUTHORS = new MetaInfo(META_COAUTHORS, String.class, Collections.EMPTY_LIST);
    static { 
        METAINFO_COAUTHORS.setMultiple(true); 
        METAINFO_COAUTHORS.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
        METAINFO_COAUTHORS.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    };
    
	public static final String META_SEARCHSCORE = "SEARCHSCORE";
    public static final MetaInfo METAINFO_SEARCHSCORE = new MetaInfo(META_SEARCHSCORE, Integer.class, new Integer(0));
    static { 
        METAINFO_SEARCHSCORE.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_NOINDEX); 
        METAINFO_SEARCHSCORE.setCacheable(false);
    };
    
	public static final String META_WFHISTORY = "WFHISTORY";
    public static final MetaInfo METAINFO_WFHISTORY = new MetaInfo(META_WFHISTORY, String.class, Collections.EMPTY_LIST);
    static { METAINFO_WFHISTORY.setMultiple(true); METAINFO_WFHISTORY.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT); };
    
	public static final String META_DESCRIPTION = "DESCRIPTION";
    public static final MetaInfo METAINFO_DESCRIPTION = new MetaInfo(META_DESCRIPTION, String.class, "");
    static { 
        METAINFO_DESCRIPTION.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
        METAINFO_DESCRIPTION.setLuceneIndexType(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT);
    };
    
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
    
	public static final String META_LASTCLIENT = "LASTCLIENT";
    public static final MetaInfo METAINFO_LASTCLIENT = new MetaInfo(META_LASTCLIENT, String.class, null);
    static {
        METAINFO_LASTCLIENT.setExtdataSinceCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    public static final String META_PUBLISHED = "PUBLISHED";
    public static final MetaInfo METAINFO_PUBLISHED = new MetaInfo(META_PUBLISHED, Date.class, null);
    static {
        METAINFO_PUBLISHED.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    public static final String META_PENDINGRELEASE = "PENDINGRELEASE";
    public static final MetaInfo METAINFO_PENDINGRELEASE = new MetaInfo(META_PENDINGRELEASE, Boolean.class, Boolean.FALSE);
    static {
        METAINFO_PENDINGRELEASE.setExtdata(true);
        METAINFO_PENDINGRELEASE.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    /**
     * Item to store the "pending release" flag on non-extdata-capable content store versions lower than 5
     */
    public static final String ITEM_PENDINGRELEASE = "wf" + META_PENDINGRELEASE.toLowerCase();

    private WGContentKey retrievalContentKey;

	private String retrievalStatus;
    
    private String retrievalLanguage;
    
    /**
	 * Constructor. Should not be used outside the WGAPI.
	 * @throws WGAPIException 
	 */
	public WGContent(WGDatabase db, WGDocumentCore doc) throws WGAPIException {
		super(db, doc);
	}
	
    /**
     * Constructor. Should not be used outside the WGAPI.
     * @throws WGAPIException 
     */
    public WGContent(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
        super(db, doc, flags);
    }

    /**
     * Gather all keys that must be available, undependent of the cache situation.
     * These will be needed if the document was deleted in background, to unmap
     * this object
     * @throws WGAPIException 
     */
    private void gatherRetrievalKeys() throws WGAPIException {
		this.retrievalContentKey = WGContentKey.create(this, true);
        this.retrievalStatus = getStatus();
        this.retrievalLanguage = (String) getMetaData(META_LANGUAGE);
    }

	/**
	 * Returns the language of this content document.
	 * @return WGLanguage
	 * @throws WGAPIException 
	 */
	public WGLanguage getLanguage() throws WGAPIException {
		return this.db.getLanguage(String.valueOf(this.getMetaData(META_LANGUAGE)));
	}

	/**
	 * Returns the title of this content document.
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getTitle() throws WGAPIException {
		return this.getMetaData(META_TITLE).toString();
	}

	/**
	 * @throws WGAPIException 
	 * @see WGDocument#dropCache()
	 */
	public void dropCache() throws WGAPIException {
		super.dropCache();
        if (!isDisposed() && !isDeleted()) {
            gatherRetrievalKeys();
        }
	}


	/**
	 * Returns the struct entry, which this content document is attached to.
	 * @return WGStructEntry
	 * @throws WGAPIException 
	 */
	public WGStructEntry getStructEntry() throws WGAPIException {

	    if (isDeleted()) {
	        throw new WGDeletedException(getDocumentKey());
	    }
	    
		if (isDummy()) {
			return null;
		}
		
		WGStructEntry theEntry = this.db.getStructEntryByKey(getContentKey().getStructKey());
		if (theEntry != null) {
		    return theEntry;
		}
		else {
		    if (isDeleted(true)) {
		        throw new WGDeletedException(getDocumentKey());
		    }
		    
		    // Unsaved docs of non-cs databases are provided a "fake struct entry"
		    if (!getDatabase().hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && !getCore().isSaved()) {
		        theEntry = db.getOrCreateStructEntryObject(new WGFakeStructEntry(db, getStructKey()), new WGDocumentObjectFlags().setDummy(true));
		        return theEntry;
		    }
		    
		    throw new WGIllegalDataException("Content with unretrievable struct entry: " + getDocumentKey());
		    
		}
		
	}

	/**
	 * Returns the unique key of this document, which can be used instead of the content key in an URL.
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getUniqueName() throws WGAPIException {
		return (String) this.getMetaData(META_UNIQUE_NAME);
	}

	/**
	 * Returns the workflow status of this content document, which is one of the constants WGContent.STATUS_...
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getStatus() throws WGAPIException {
		return this.getMetaData(META_STATUS).toString();
	}

	/**
	 * Makes a complete test about the visibility of the content in publishing:
	 * - Is the general visibility flag set?
	 * - Is the content visible for the given role?
	 * - Are the content validity dates allowing publishing right now? 
	 * @param displayType The navigation element to test, if this content is shown in it. Should be one of the constants WGContent.DISPLAYTYPE_...
	 * @return boolean
	 * @throws WGAPIException 
	 */
	public boolean isVisibleFor(String displayType) throws WGAPIException {

		// Static visibility information is cached
		Boolean isVisibleFor = (Boolean) this.getVisibilityCache().get(displayType);
		if (!isCachingEnabled() || isVisibleFor == null) {
			if (this.isVisible() == false) {
				isVisibleFor = new Boolean(false);
			}
			else if (displayType != null && this.isHiddenFrom().contains(displayType)) {
				isVisibleFor = new Boolean(false);
			}
			else {
				isVisibleFor = new Boolean(true);
			}
            
            if (getDatabase().getSessionContext().isCacheWritingEnabled()) {
                this.getVisibilityCache().put(displayType, isVisibleFor);
            }
		}
		
		if (isVisibleFor.booleanValue() == false) {
			return false;
		}

		// Dynamic visibility information is evaluated everytime
		Date now = new Date();
		if (this.getValidFrom() != null && this.getValidFrom().after(now)) {
			return false;
		}
		else if (this.getValidTo() != null && this.getValidTo().before(now)) {
			return false;
		}
		
		return true;

	}

	/**
	 * Tests the general visibility of this content (see WGContent.isVisible()), and if the current settings for the "Valid from" and "Valid to" date
	 * allow it to the visible right now.
	 * @return boolean
	 * @throws WGAPIException 
	 */
	public boolean isVisibleNow() throws WGAPIException {

		if (this.isVisible() == false) {
			return false;
		}

		Date validFrom = this.getValidFrom();
		Date validTo = this.getValidTo();
		Date now = new Date();

		if (validFrom != null && validFrom.after(now)) {
			return false;
		}
		else if (validTo != null && validTo.before(now)) {
			return false;
		}

		return true;
	}

	/**
	 * Returns the general visibility setting of this document. If this method returns true, the document can nevertheless be not visible bc. of other settings.
	 * If this method returns false, the document is in no case visible.
	 * @return boolean
	 * @throws WGAPIException 
	 */
	public boolean isVisible() throws WGAPIException {
		return ((Boolean)this.getMetaData(META_VISIBLE)).booleanValue();
	}

	/**
	 * Returns the Date, since when this document is valid (i.e. is shown in the website) or null if no such Date is specified.
	 * @return Date
	 * @throws WGAPIException 
	 */
	public java.util.Date getValidFrom() throws WGAPIException {
		return (java.util.Date) this.getMetaData(META_VALID_FROM);
	}

	/**
	 * Returns the Date, until when this document is valid (i.e. is shown in the website) or null if no such Date is specified.
	 * @return Date
	 * @throws WGAPIException 
	 */
	public java.util.Date getValidTo() throws WGAPIException {
		return (java.util.Date) this.getMetaData(META_VALID_TO);
	}

	/**
	 * Returns a list of the navigation elements, where this content document should not be shown. 
	 * These are represented as constants WGContent.DISPLAYTYPE_...
	 * @return List
	 * @throws WGAPIException 
	 */
	public java.util.List isHiddenFrom() throws WGAPIException {
		return (List) this.getMetaData(META_IS_HIDDEN_FROM);
	}

	/**
	 * Returns a link url, that is called instead of an URL to this content, when it is clicked in a navigator.
	 * If this field is filled, the content document is regarded a "virtual document" bc. it's content is never shown
	 * and it is only used as a pointer to the link url.
	 * @return The virtual link url.
	 * @throws WGAPIException 
	 */
	public String getVirtualLink() throws WGAPIException {
		return (String) this.getMetaData(META_VIRTUAL_LINK);
	}

	/**
	 * Returns the type of this virtual link, which is one of the constants WGContent.VIRTUALLINKTYPE_...
	 * The type of a virtual link decides, if the link retrieved via WGContent.getVirtualLink() is a relative url,
	 * and how it is completed to an absolute url at runtime.
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getVirtualLinkType() throws WGAPIException {
		return (String) this.getMetaData(META_VIRTUAL_LINK_TYPE);
	}

	/**
	 * Determines, if this document is a "virtual document", i.e. if another link is shown instead of a link to this content in navigators.
	 * @return boolean
	 * @throws WGAPIException 
	 */
	public boolean isVirtual() throws WGAPIException {
        String link = getVirtualLink();
		if (link == null || link.equals("")) {
			return false;
		}
		else {
			return true;
		}
	}

	/**
	 * Returns the link target of this document, i.e. the name of the frame/browser window, in which this content is displayed
	 * when it is clicked in a navigator.
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getLinkTarget() throws WGAPIException {
		return (String) this.getMetaData(META_LINK_TARGET);
	}

	/**
	 * Returns the struct key for this content document, which is the key of the struct entry this content document is attached to.
	 * @return Object
	 * @throws WGAPIException 
	 */
	public Object getStructKey() throws WGAPIException {
		return this.getMetaData(META_STRUCTENTRY);
	}

	/**
	 * Returns the content key of this document, either in unique mode (real version is always used) or in URL mode (0 is used as version, when document is released)
	 * @param unique Determines, if the unique mode is used. If false, the URL mode is used.
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public WGContentKey getContentKey(boolean unique) throws WGAPIException {

		if (unique) {
			return retrievalContentKey;
		}
		else {
			return WGContentKey.create(this, false);
		}
	}

	/**
	 * Returns the unique content key of this content document.
	 * @return WGContentKey
	 * @throws WGAPIException 
	 */
	public WGContentKey getContentKey() throws WGAPIException {
		return getContentKey(true);
	}

	/**
	 * Returns the version number of this content document.
	 * All content documents under one struct entry in the same language share a version number pool beginning with 1.
	 * @return int
	 * @throws WGAPIException 
	 */
	public int getVersion() throws WGAPIException {
		return ((Integer)this.getMetaData(WGContent.META_VERSION)).intValue();
	}
	/* (non-Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#retrieveCore()
	 */
	public WGDocumentCore retrieveCore() throws WGAPIException {
		return this.db.getCore().getContentByKey(this.retrievalContentKey);
	} 

	/**
	 * Sets the title of this content document.
	 * @param title
	 * @throws WGAPIException 
	 */
	public WGContent setTitle(String title) throws WGAPIException {
		this.setMetaData(WGContent.META_TITLE, title);
		return this;
	}
	
	/**
     * Sets the owner of the content document
     * @param owner
     * @throws WGAPIException 
     */
    public void setOwner(String owner) throws WGAPIException {
        this.setMetaData(WGContent.META_OWNER, owner);
    }
 
	/**
	 * Returns a list of keywords for this content document to be used to identify this document in search machines.
	 * @return List
	 * @throws WGAPIException 
	 */
	public List getKeywords() throws WGAPIException {
		return (List) this.getMetaData(META_KEYWORDS);
	}


	/**
	 * Sets the workflow status of this content document.
	 * @param status
	 * @throws WGAPIException 
	 */
	protected void setStatus(String status) throws WGAPIException {
		this.setMetaData(WGContent.META_STATUS, status);
	}

	/**
	 * Sets the unique name of this content document, which can be used in URLs instead of the content key.
	 * @param name
	 * @throws WGAPIException 
	 */
	public void setUniqueName(String name) throws WGAPIException {
		this.setMetaData(WGContent.META_UNIQUE_NAME, name);
	}

	/**
	 * Sets the navigation elements, in that this content should not be shown.
	 * @param hiddenFrom List of navigation elements, represented as constants WGContent.DISPLAYTYPE_..., where to hide this content. 
	 * Provide empty list or null completely disable hiding.
	 * @throws WGAPIException 
	 */
	public void setHiddenFrom(List hiddenFrom) throws WGAPIException {

		if (hiddenFrom == null) {
			hiddenFrom = new ArrayList();
		}

		this.setMetaData(WGContent.META_IS_HIDDEN_FROM, hiddenFrom);
	}

	/**
	 * Sets the general visibility flag, choosing if this document is visible. If set to "true" document can be nevertheless invisible because of other settings.
	 * @param visible
	 * @throws WGAPIException 
	 */
	public void setVisible(boolean visible) throws WGAPIException {
		this.setMetaData(WGContent.META_VISIBLE, new Boolean(visible));
	}

	/**
	 * Sets validity dates, narrowing the period of time, in which a document will be visible.
	 * Provide null as parameter if you do not wish to limit the period in one or both directions.
	 * @param from Date, from which the content will be visible (inclusive)
	 * @param to Date, to which the content will be visible (inclusive)
	 * @throws WGAPIException 
	 */
	public void setValidity(Date from, Date to) throws WGAPIException {
		this.setMetaData(WGContent.META_VALID_FROM, from);
		this.setMetaData(WGContent.META_VALID_TO, to);
	}





	/**
	 * Sets a virtual link for this document, making the content document a "virtual document". 
	 * The link will be shown in navigators instead of a link to this content document.
	 * @param type Type of the virtual link. Deciding how "explicit" the link url needs to be. Represented by the following constants at object WGContent:
	 * </p>VIRTUALLINKTYPE_CONTENT: Link to another content document. Only content key is needed as URL<br/>
	 * VIRTUALLINKTYPE_EXTERNAL: External link. Absolute link needed<br/>
	 * VIRTUALLINKTYPE_FILE: Link to file resource. Relative link to the file resource needed.<br/>
	 * VIRTUALLINKTYPE_INTERNAL_FILE: Link to file, attached to this content document. Only file name needed.</br>
	 * </p>
	 * @param url The url given for the virtual link.
	 * @throws WGAPIException 
	 */
	public void setVirtualLink(String type, String url) throws WGAPIException {
		this.setMetaData(WGContent.META_VIRTUAL_LINK_TYPE, type);
		this.setMetaData(WGContent.META_VIRTUAL_LINK, url);
	}

	/**
	 * Disables virtual link functionality for this content document and clears all fields related to it
	 * @throws WGAPIException 
	 */
	public void clearVirtualLink() throws WGAPIException {
		this.setMetaData(WGContent.META_VIRTUAL_LINK_TYPE, null);
		this.setMetaData(WGContent.META_VIRTUAL_LINK, null);
	}

	/**
	 * Sets the link target of this content, i.e. the frame or browser window name, in which this content should be displayed 
	 * when clicked in a navigator.
	 * @param target
	 * @throws WGAPIException 
	 */
	public void setLinkTarget(String target) throws WGAPIException {
		this.setMetaData(WGContent.META_LINK_TARGET, target);
	}

	/**
	 * Sets keywords for this content to provide for internet search services.
	 * @param keywords List of String keywords
	 * @throws WGAPIException 
	 */
	public void setKeywords(List keywords) throws WGAPIException {
		this.setMetaData(WGContent.META_KEYWORDS, keywords);
	}

	/**
	 * Returns the name of the content author.
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getAuthor() throws WGAPIException {
		return (String) this.getMetaData(WGContent.META_AUTHOR);
	}
	
	/**
     * Returns the name of the content owner.
     * @return String
     * @throws WGAPIException 
     */
    public String getOwner() throws WGAPIException {
        return (String) this.getMetaData(WGContent.META_OWNER);
    }

	/**
	 * If the document was retrieved via query (and the db implementation supports this feature)
	 * returns the search score of this document, i.e. a number representing how much this document matches
	 * the search query.
	 * @throws WGAPIException 
	 */
	public float getSearchScore() throws WGAPIException {
	    
	    // In case of lucene search: Try to retrieve score that was inserted from outside
	    Float score = getSearchDetails().getScore();
	    if (score != null) {
	        return score.floatValue();
	    }
	    
	    // In case of native search (domino fulltext): Try to retrieve score from backend document
		return ((Integer)this.getMetaData(WGContent.META_SEARCHSCORE)).floatValue();
	}
	
	/**
	 * Returns an explaination object that shows information about the choosage of this content as a query result in the most recent query.
	 * The type of object returned here is dependent on the type of query.
	 * @throws WGClosedSessionException 
	 */
	public Object getSearchExplanation() throws WGClosedSessionException {
		try {
            return getSearchDetails().getExplanation();
        }
        catch (WGAPIException e) {
            // Unfortunately necessary to keep exception interface of this method
            throw new RuntimeException("Exception retrieving search explanation", e);
        }		
	}
	
	/**
	 * Returns detail information about the content depending on the last search
	 * @throws WGAPIException 
	 */
	public SearchDetails getSearchDetails() throws WGAPIException {
	    
	    SearchDetails searchDetails = getSessionData().getSearchDetails();
	    if (searchDetails != null) {
	        return searchDetails;
	    } else {
	        SearchDetails details = new SearchDetails();
	        // In case of native search (domino fulltext): Try to retrieve score from backend document
	        details.setScore(((Integer)this.getMetaData(WGContent.META_SEARCHSCORE)).floatValue());
	        return details;
	    }  
	}

	/**
	 * Releases this document immediately while exiting workflow. The user must be a valid admin approver of the current workflow to do this action.
	 * @param comment Comment for the release action.
	 * @throws WGAPIException
	 */
	public void releaseImmediately(String comment) throws WGAPIException {

		// Test access level
		int accessLevel = this.db.getSessionContext().getAccessLevel();
		String user = this.db.getSessionContext().getUser();
		if (accessLevel < WGDatabase.ACCESSLEVEL_AUTHOR
			|| (accessLevel == WGDatabase.ACCESSLEVEL_AUTHOR && !isAuthorOrOwner())) {
			throw new WGAuthorisationException("You are not authorized to publish this document", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
		}

		// Test workflow role
		WGWorkflow workflow = getWorkflow();
		if (workflow.getWorkflowRole() != WGWorkflow.ROLE_ADMINISTRATOR) {
			throw new WGAuthorisationException("You are no workflow administrator", WGAuthorisationException.ERRORCODE_OP_NEEDS_WORKFLOW_ADMIN_RIGHTS);
		}

        // release
		WGTransaction trans = getDatabase().startTransaction();
		try {
			release(comment, workflow, "Released immediately by workflow admin");
			this.save();
			trans.commit();
		}
		finally {
		    if (trans.isOpen()) {
		        trans.rollback();
		    }
		}

	}
	
	/**
	 * For a content document in status DRAFT, set it to status REVIEW and let it enter publishing workflow.
	 * @param comment A comment for publishing this content.
	 * @throws WGAPIException
	 */
	public void publish(String comment) throws WGAPIException {
		publish(comment, null);
	}
	
	   /**
     * For a content document in status DRAFT, set it to status REVIEW and let it enter publishing workflow.
     * @throws WGAPIException
     */
     public void publish() throws WGAPIException {
        publish("", null);
     }
	
	/**
	 * For a content document in status DRAFT, set it to status REVIEW and let it enter publishing workflow.
	 * @param comment A comment for publishing this content.
	 * @param reasonForReplacement If a released content allready exists, a reason for it's replacement can be set by this param. Allowes null value.
	 * @throws WGAPIException
	 */
	public void publish(String comment,String reasonForReplacement) throws WGAPIException {

		// Test access level
		int accessLevel = this.db.getSessionContext().getAccessLevel();
		String user = this.db.getSessionContext().getUser();
		if (accessLevel < WGDatabase.ACCESSLEVEL_AUTHOR
			|| (accessLevel == WGDatabase.ACCESSLEVEL_AUTHOR && !isAuthorOrOwner())) {
			throw new WGAuthorisationException("You are not authorized to publish this document", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
		}

		if (!this.getStatus().equals(WGContent.STATUS_DRAFT)) {
			throw new WGIllegalStateException("The content is not in status draft and cannot be published");
		}
		
		
		WGTransaction trans = getDatabase().startTransaction();
		try {
		
    		// Set ITEM_REPLACEREASON (We must save after setting that to workaround B0000445E)
    		
    		if( reasonForReplacement != null && !reasonForReplacement.trim().equals("") ){
    			this.setItemValue(ITEM_REPLACEREASON, reasonForReplacement);
    			save(new Date(), false);		
    		}
    		else{			
    			if( this.hasItem(ITEM_REPLACEREASON) ){
    				this.removeItem(ITEM_REPLACEREASON);
    				save(new Date(), false);
    			}
    		}
            
    		// Content should be saved prior to publishing (Also workaround for bug B0000445E)
    		if (save() == false) {
                throw new WGBackendException("Could not publish document " + getContentKey().toString() + " because it could not be saved");
            }
    		
            WGWorkflow workflow = getWorkflow();
		    
		    if (getDatabase().isProjectMode()) {
		        release(comment, workflow, "Released in project mode");
		    }
		    else {
		        
		        boolean result = workflow.publish(comment);
		        
		        if (result == false) {
		            this.setStatus(WGContent.STATUS_REVIEW);
		            this.addWorkflowHistoryEntry("Submitted for approval");
		            
		            fireStatusChangeEvent();
   		            result = maybeAutoApprove(workflow);
		        }
		        
		        // Release doc if publish returned true
		        else {
                  this.release(comment, workflow, "Released");
		        }
    	
    			if (save(new Date(), false) == false) {
    				throw new WGBackendException("Could not publish document " + getContentKey().toString() + " because it could not be saved");
    			}
    			
    		}
		    trans.commit();
		}
		finally {
		    if (trans.isOpen()) {
		        trans.rollback();
		    }
		}

	}

    private boolean maybeAutoApprove(WGWorkflow workflow) throws WGAPIException {
        boolean result = false;
        while (result == false && isAutoApprovable(workflow)) {
           result = approve("Automatic approval for user '" + getDatabase().getSessionContext().getUser() + "'", workflow); 
        }
        return result;
    }
    
    /**
     * Determine if the current user will auto-approve the content in the current state with the given workflow
     * Auto-approve should only happen when enabled, the user is a regular approver of the current workflow level (NOT just admin approver!) and has not already approved it
     * @param workflow The workflow
     * @throws WGAPIException
     */
    public boolean isAutoApprovable(WGWorkflow workflow) throws WGAPIException {
        
        if (!getDatabase().isAutoApprove()) {
            return false;
        }
        
        if (!(workflow instanceof WGAutoApprovalCapableWorkflow)) {
            return false;
        }
        
        WGAutoApprovalCapableWorkflow autoWorkflow = (WGAutoApprovalCapableWorkflow) workflow;
        return autoWorkflow.getWorkflowLevelRole() == WGWorkflow.ROLE_APPROVER && !autoWorkflow.isAlreadyApprovedByUser();
    }

	/**
	 * For a content document in status REVIEW, approves this document and continues the workflow. The user must be a valid approver of the current workflow level to do this action.
	 * @param comment A comment for the approval action
	 * @throws WGAPIException
	 */
	public void approve(String comment) throws WGAPIException {
	    WGWorkflow workflow = getWorkflow();
	    approve(comment, workflow);
	}
	
	private boolean approve(String comment, WGWorkflow workflow) throws WGAPIException {

	    
	    WGTransaction trans = getDatabase().startTransaction();
	    try {
    	    // Content should be saved prior to approval
            if (!isSaved() || isEdited()) {
                if (save() == false) {
                    throw new WGBackendException("Could not approve document " + getContentKey().toString() + " because it could not be saved");
                }
            }
            
            // Test access level
    		int accessLevel = this.db.getSessionContext().getAccessLevel();
    		String user = this.db.getSessionContext().getUser();
    		if (accessLevel < WGDatabase.ACCESSLEVEL_AUTHOR
    			|| (accessLevel == WGDatabase.ACCESSLEVEL_AUTHOR && !isAuthorOrOwner())) {
    			throw new WGAuthorisationException("You are not authorized to approve documents in this database", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
    		}
    
    		// Test doc status
    		if (!this.getStatus().equals(WGContent.STATUS_REVIEW)) {
    			throw new WGIllegalStateException("The content is not in status review and cannot be approved");
    		}
    
    		// Test workflow role
    		if (workflow.getWorkflowRole() < WGWorkflow.ROLE_APPROVER) {
    			throw new WGAuthorisationException("You are no approver for the current workflow level", WGAuthorisationException.ERRORCODE_OP_NEEDS_WORKFLOW_APPROVER_RIGHTS);
    		}
    		
    		// Write workflow history
		    boolean result = workflow.approve(comment);
   			if (result) {
				release(comment, workflow, "Approved and released");
    		}
    		else {
    			this.addWorkflowHistoryEntry("Approved and submitted for further approval");
    			result = maybeAutoApprove(workflow); // auto-approve will automatically call release() when workflow is finished
    		}
    
    		if (save(new Date(), false) == false) {
    			throw new WGBackendException("Could not approve document " + getContentKey().toString() + " because it could not be saved");
    		}
    		
    		trans.commit();
    		return result;
    		
	    }
	    finally {
	        if (trans.isOpen()) {
	            trans.rollback();
	        }
	    }

	}

	/**
	 * Internal method to process a workflow release.
	 * @param comment Comment for the release
	 * @param workflow A workflow object for the current content document.
	 * @throws WGAPIException
	 * @return true if the document was released immediately, false if the document is waiting for pending release
	 */
	protected boolean release(String comment, WGWorkflow workflow, String workflowHistoryEntry) throws WGAPIException {
	    
	    // Determine if this document should go into pending release
	    if (getDatabase().isBackendServiceSupported(WGDatabase.BACKENDSERVICE_SELECT_PENDINGRELEASE_DOCS)) {
    	    if (getValidFrom() != null && getValidFrom().after(new Date())) {
    	        setPendingRelease(true);
    	        this.addWorkflowHistoryEntry("Waiting for pending release");
    	        if (!getStatus().equals(STATUS_REVIEW)) {
    	            setStatus(STATUS_REVIEW);
    	            fireStatusChangeEvent();
    	        }
    	        return false;
    	    }
	    }

	    // Otherwise reset the pending release flag
	    if (isPendingRelease()) {
	        setPendingRelease(false);
	    }
	    
		// Get previously released. If present, archive it (in project mode: delete it)
	    boolean didArchive = false;
		WGContent prevContent = getStructEntry().getReleasedContent(this.getLanguage().getName());
		Date publishedDate = new Date();
        if (prevContent != null) {
            if (getDatabase().isProjectMode()) {
                prevContent.remove();
            }
            else {
    			if( this.hasItem(ITEM_REPLACEREASON) ){
    				prevContent.setItemValue(ITEM_REPLACEREASON, this.getItemValue(ITEM_REPLACEREASON));
                    prevContent.save(publishedDate, false);
    			}
    			prevContent.archive(comment, false);
            }
            didArchive = true;
		}

		if (getStructEntry().hasReleasedContent(this.getLanguage().getName())) {
	        if (didArchive) {
	            throw new WGIllegalStateException("Cannot release because after archiving the previous release, there still seems to be a released content.");
	        }
	        else {
	            throw new WGIllegalStateException("Cannot release because there is a released content that is invisible to the current user " + getDatabase().getSessionContext().getUser());
	        }
	    }

		this.setStatus(WGContent.STATUS_RELEASE);
		
		if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
    		this.setPublished(publishedDate);
    		WGStructEntry entry = getStructEntry();
    		Map published = entry.getPublished();
    		if (!published.containsKey(getLanguage().getName())) {
    		    try {
                    published.put(getLanguage().getName(), publishedDate);
                    entry.setPublished(published);
                    entry.save(new Date(), false);
                }
                catch (Exception e) {
                    WGFactory.getLogger().error("Exception setting published date on struct entry '" + String.valueOf(entry.getStructKey()) + "'", e);
                }
    		}
		}
		
		if( this.hasItem(ITEM_REPLACEREASON) ){
			this.removeItem(ITEM_REPLACEREASON);
		}
		workflow.release(comment);
		
		// Workflow history
		this.addWorkflowHistoryEntry(workflowHistoryEntry);
		fireStatusChangeEvent();
		return true;

	}

    private void fireStatusChangeEvent() throws WGAPIException {
        WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_STATUSCHANGED, getDocumentKey(), getStructEntry().getContentType().getName(), getDatabase());
        event.setContent(this);
        getDatabase().fireContentEvent(event);
    }

	/**
	 * Method reject.
	 * For a content document in status REVIEW, rejects this document and resets it to status DRAFT. The user must be a valid approver of the current workflow level to do this action.
	 * @param comment A comment for the rejectal action
	 * @throws WGAPIException
	 */
	public void reject(String comment) throws WGAPIException {

        // Content should be saved prior to rejection to enforce validity
        if (!isSaved() || isEdited()) {
            if (save() == false) {
                throw new WGBackendException("Could not reject document " + getContentKey().toString() + " because it could not be saved");
            }
        }
        
		// Test access level
		int accessLevel = this.db.getSessionContext().getAccessLevel();
		String user = this.db.getSessionContext().getUser();
		if (accessLevel < WGDatabase.ACCESSLEVEL_AUTHOR
			|| (accessLevel == WGDatabase.ACCESSLEVEL_AUTHOR && !isAuthorOrOwner())) {
			throw new WGAuthorisationException("You are not authorized to reject this document", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
		}

		// Test document status
		if (!this.getStatus().equals(WGContent.STATUS_REVIEW) && !this.getStatus().equals(WGContent.STATUS_RELEASE)) {
			throw new WGIllegalStateException("The content is not in status review or release and cannot be rejected");
		}

		// Test workflow role - On reject must only be approver if one is not author/owner and may not edit the page generally
		WGWorkflow workflow = getWorkflow();
		if (!isAuthorOrOwner() && !getStructEntry().mayEditPage()) {
    		if (workflow.getWorkflowRole() < WGWorkflow.ROLE_APPROVER) {
    			throw new WGAuthorisationException("You are no approver for the current workflow level", WGAuthorisationException.ERRORCODE_OP_NEEDS_WORKFLOW_APPROVER_RIGHTS);
    		}
		}
		
		String previousStatus = getStatus();

		// Reject
		workflow.reject(comment);
		setStatus(WGContent.STATUS_DRAFT);
		setPendingRelease(false);
		
		// If the previous state was released, query- and structentry cache must be cleared (default cache maintenance won't do this)
		if (previousStatus == WGContent.STATUS_RELEASE) {
			try {
                getDatabase().masterQueryCache.flushAll();
            }
            catch (CacheException e) {
                WGFactory.getLogger().error("Exception flushing query cache on database '" + getDatabase().getDbReference() + "'", e);
            }
			getStructEntry().dropCache();
		}

		// Write history
		this.addWorkflowHistoryEntry("Rejected and returned to draft status");
		
        fireStatusChangeEvent();

		if (save(new Date(), false) == false) {
			throw new WGBackendException("Could not reject document " + getContentKey().toString() + " because it could not be saved");
		}

	}

	/**
	 * For a content document in Status RELEASE archives this document immediately. User must be the author of this document or have editor rights.
	 * @param comment A comment for archiving this document
	 * @throws WGAPIException 
	 */
	public void archive(String comment) throws WGAPIException {
	    archive(comment, true);
	}
	
	private void archive(String comment, boolean checkProtectedRelations) throws WGAPIException {

	    // Check if a protected relation prevents archiving of this content
        if (checkProtectedRelations && getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            for (WGRelationData rel : getIncomingRelations(true)) {
                if (rel.getType() == WGContent.RELATIONTYPE_PROTECTED) {
                    throw new WGRestrictionException("The content is target of protected relation '" + rel.getName() + "' from content '" + rel.getParentContentKey().toString() + "' and cannot be deleted", 
                            WGRestrictionException.ERRORCODE_OP_DENIED_BY_PROTECTED_RELATION, 
                            new WGDocumentKey(WGDocument.TYPE_CONTENT, rel.getParentContentKey().toString(), null));
                }
            }
        }
	    
        // Content should be saved prior to archiving
        if (!isSaved() || isEdited()) {
            if (save() == false) {
                throw new WGBackendException("Could not archive document " + getContentKey().toString() + " because it could not be saved");
            }
        }
        
        // Test access level
		int accessLevel = this.db.getSessionContext().getAccessLevel();
		String user = this.db.getSessionContext().getUser();
		if (accessLevel < WGDatabase.ACCESSLEVEL_AUTHOR
			|| (accessLevel == WGDatabase.ACCESSLEVEL_AUTHOR && !isAuthorOrOwner())) {
			throw new WGAuthorisationException("You are not authorized to archive this document because you are not the author", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
		}

		if (!this.getStatus().equals(WGContent.STATUS_RELEASE) && !this.getStatus().equals(WGContent.STATUS_DRAFT)) {
			throw new WGIllegalStateException("The content is in status '" + getStatus() + "' and cannot be archived");
		}

		// Archive
		this.setStatus(WGContent.STATUS_ARCHIVE);
		WGWorkflow workflow = getWorkflow();
		workflow.archive(comment);

		// Write workflow history
		if (comment != null && !comment.trim().equals("")) {
			this.addWorkflowHistoryEntry("Archived. Replace reason: " + comment);
		}
		else {
			this.addWorkflowHistoryEntry("Archived");
		}
		
        fireStatusChangeEvent();
		

		if (save(new Date(), false) == false) {
			throw new WGBackendException("Could not archive document " + getContentKey().toString() + " because it could not be saved");
		}

	}

	/**
	 * Adds an entry to the end of the workflow history. The entry count is kept at a maximum number of 50.
	 * Therefor the oldest entries will be removed when this number is exceeded.
	 * @param entry The entry to add.
	 * @throws WGAPIException 
	 */
	protected void addWorkflowHistoryEntry(String entry) throws WGAPIException {

		List workflowHistory = this.getMetaDataList(META_WFHISTORY);
		String dateFormatted = DATEFORMAT_HISTORY.format(new Date());
		
		String completeEntry = dateFormatted + " - Version " + getVersion() + " - User " + getDatabase().getSessionContext().getUser()+  " - " + entry;
        completeEntry = WGUtils.reduce(completeEntry, 250);
        
        workflowHistory.add(completeEntry);
		while (workflowHistory.size() > 50) {
			workflowHistory.remove(0);
		}
		this.setMetaData(META_WFHISTORY, workflowHistory);

	}

	/**
	 * Saves changes made to this document. You must be a valid editor to do this.
	 * @throws WGAPIException 
	 */
    public boolean save(Date lastModified) throws WGAPIException {
        return save(lastModified, true);
    }
    
	private boolean save(Date lastModified, boolean performTests) throws WGAPIException {

        String contentType = null;
        if (hasCompleteRelationships()) {
            contentType = getStructEntry().getContentType().getName();
        }

        // Event contentSaved (semaphore-checked to prevent recursive savings)
        if (!getSessionData().isSaveEventSemaphore()) {
            getSessionData().setSaveEventSemaphore(true);
            try {
                WGContentEvent event = new WGContentEvent(WGContentEvent.TYPE_SAVED, getDocumentKey(), contentType, getDatabase());
                event.setContent(this);
                if (db.fireContentEvent(event) == false) {
                    throw new WGCancelledException("Save operation was canceled by the 'SaveContent' event");
                }
            }
            finally {
                getSessionData().setSaveEventSemaphore(false);
            }
        }
        
		//	Set last client
		String client = getDatabase().getSessionContext().getClient();
		if (client != null) {
			setLastClient(getDatabase().getSessionContext().getClient());
		}
		
		// Set visibility
		if (hasCompleteRelationships() && getStructEntry().getArea().isSystemArea()) {
		    setVisible(false);
		}
		else if (!isVisible()) {
		    setVisible(true);
		}

		try {
		    getSessionData().setPerformStatusTestsOnSave(performTests);
            boolean result = super.save(lastModified);
            return result;
		}
		finally {
		    getSessionData().setPerformStatusTestsOnSave(false);
		}

	}
    
    /**
     * Tests if the current user is allowed to edit (i.e. save modifications) the current document
     * @throws WGAPIException 
     */
    public boolean mayEditContent() throws WGAPIException {
        
        try {
            performSaveCheck();
            return true;
        }
        catch (WGAuthorisationException e) {
            return false;
        }
        catch (WGIllegalStateException e) {
            return false;
        }
        
    }


    public void performSaveCheck() throws WGAuthorisationException, WGAPIException {
        
        super.performSaveCheck();
        
        // Integrity checks. Must be done for all users (including master)
        
        if (isDummy()) {
            throw new WGIllegalStateException("This is a dummy content that cannot be saved");
        }
        
        // Check if Unique Name exists
        if (getStatus().equals(WGContent.STATUS_RELEASE) && db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && 
                getDatabase().getSessionContext().isTestUniqueNames()) {
            boolean isUniqueNameOK = testUniqueNameUniqueness(getUniqueName(), getLanguage().getName());
            if (!isUniqueNameOK) {
                throw new WGDuplicateKeyException("Unique name " + this.getUniqueName() + " already exists.");
            }
        }
        
        // Master sessions may bypass all save checks that are for authorisation only
        if (getDatabase().getSessionContext().isMasterSession()) {
            return;
        }
        
        // Accesslevel EDITOR is needed everywhere where released docs are saved
        boolean performStatusTests = getSessionData().isPerformStatusTestsOnSave();
        int accessLevel = db.getSessionContext().getAccessLevel();
        
        if (performStatusTests && getStatus().equals(WGContent.STATUS_RELEASE)) {
            if (accessLevel < WGDatabase.ACCESSLEVEL_EDITOR) {
                throw new WGAuthorisationException("This content is released and can only be edited by editors or higher", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
            }
        }

        // Checks only done on full content stores
        if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
        
            // Check (hierarchical) editor rights from struct
            WGDocument document = this.getStructEntry().mayEditEntryAndContent();
            if (document != null) {
            	if (document instanceof WGArea) {
            		throw new WGAuthorisationException("User is not allowed to edit content in this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, document);
            	}
            	else {
            		WGStructEntry entry = (WGStructEntry) document;
            		throw new WGAuthorisationException(
            			"User is not allowed to edit this content, because struct entry '"
            				+ entry.getTitle()
            				+ "' (Key "
            				+ entry.getStructKey()
            				+ ") disallows it", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, document);
            	}
            }
    
            // Check edit rights from content type
            WGContentType contentType = getStructEntry().getContentType();
            if (contentType != null && !contentType.mayCreateContent()) {
            	throw new WGAuthorisationException("User is not allowed to edit content of this content type", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, contentType);
            }
    
            // Check edit rights from language
            WGLanguage language = getLanguage();
            if (language != null && !language.mayCreateContent()) {
            	throw new WGAuthorisationException("User is not allowed to edit content of this language", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_LANGUAGE, language);
            }
        
            // Check author status
            if (accessLevel == WGDatabase.ACCESSLEVEL_AUTHOR) {
            	if (!isAuthorOrOwner()) {
            		throw new WGAuthorisationException("You are not authorized to save this document, because you are no author", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHORING_RIGHTS);
            	}
            }
            
            // Check workflow status and special dependencies related to them
            if (performStatusTests) {
                if (getStatus().equals(WGContent.STATUS_REVIEW)) {
                	WGWorkflow workflow = db.getWorkflowEngine().getWorkflow(this);
            		if (workflow.getWorkflowRole() < WGWorkflow.ROLE_APPROVER) {
            			throw new WGAuthorisationException("This content is in review and can only be edited by an approver or workflow administrator", WGAuthorisationException.ERRORCODE_OP_NEEDS_WORKFLOW_APPROVER_RIGHTS);
            		}
                }
        
                if (getStatus().equals(WGContent.STATUS_ARCHIVE)) {
                	if (accessLevel < WGDatabase.ACCESSLEVEL_CHIEF_EDITOR) {
                		throw new WGAuthorisationException("This content is archived and can only be edited by a manager", WGAuthorisationException.ERRORCODE_OP_NEEDS_MANAGER_LEVEL);
                	}
                }
            }
        }
        
    }
    
    /**
     * Determines if the current user is an author of this document
     */
    public boolean isAuthorOrOwner() throws WGAPIException {
        
        List userNameList = new ArrayList();
        userNameList.add(getAuthor());
        userNameList.add(getOwner());
        userNameList.addAll(getCoauthors());
        return db.isMemberOfUserList(userNameList);
        
    }



	/**
	 * Returns a description for this content document
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getDescription() throws WGAPIException {
		return (String) this.getMetaData(META_DESCRIPTION);
	}

	/**
	 * Set a description for this content document.
	 * @param desc
	 * @throws WGAPIException 
	 */
	public void setDescription(String desc) throws WGAPIException {
		this.setMetaData(META_DESCRIPTION, desc);
	}

	/**
	 * Returns the role of the current user in the content's current workflow state.
	 * @return A constant of type WGWorkflow.ROLE_....
	 * @throws WGAPIException 
	 */
	public int getWorkflowRole() throws WGAPIException {

		WGWorkflow workflow = getWorkflow();
		return workflow.getWorkflowRole();
	}

    /**
     * Retrieves the workflow object for the current content document, if it is available
     * @throws WGAPIException
     */
    public WGWorkflow getWorkflow() throws WGAPIException {
        return this.db.getWorkflowEngine().getWorkflow(this);
    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
	 */
	public WGDocument createClone(WGDatabase db, WGDocument ref) throws WGAPIException {

		if (ref == null || !(ref instanceof WGStructEntry)) {
			return null;
		}

		// Metadata
		WGContent newContent =
			db.createContent(
				(WGStructEntry) ref,
				db.getLanguage((String) getMetaData(META_LANGUAGE)),
				getTitle(),
				new Integer(getVersion()));
		newContent.save();
		
        try {
            pushData(newContent);
      		newContent.saveWithGivenTimestamps(getCreated(), getLastModified());
    		return newContent;
        }
    	catch (WGAPIException e) {
            // try to remove created content object
            try {
                newContent.remove();
            } catch (Throwable e1) {                
            }
            
            throw e;
        }

	}

    public void pushData(WGDocument newDoc) throws WGAPIException, WGCreationException {
        
        WGContent newContent = (WGContent) newDoc;
        newContent.setMetaData(META_AUTHOR, getAuthor());
        
        // New WGACS 5 Metas
        if (newDoc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            
            if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
                newContent.setOwner(getOwner());
                newContent.setCoauthors(new ArrayList(getCoauthors()));
                newContent.setContentClass(getContentClass());
                newContent.setPublished(getPublished());
            }
            
            // If the source is no CS5 we initialize the published date of the target with the creation date
            else if (getStatus().equals(WGContent.STATUS_RELEASE) || getStatus().equals(WGContent.STATUS_ARCHIVE)) {
                newContent.setPublished(getCreated());
            }
            
        }
        
        newContent.setTitle(getTitle());
        newContent.setDescription(getDescription());
        newContent.setHiddenFrom(new ArrayList(getMetaDataList(META_IS_HIDDEN_FROM)));
        newContent.setKeywords(new ArrayList(getKeywords()));
        newContent.setLinkTarget(getLinkTarget());
   
        newContent.setStatus(getStatus());
        
        newContent.setUniqueName(getUniqueName());
        newContent.setValidity(getValidFrom(), getValidTo());
        
        if (isVirtual()) {
        	newContent.setVirtualLink(getVirtualLinkType(), getVirtualLink());
        }
        else {
            newContent.clearVirtualLink();
        }
        
        newContent.setVisible(isVisible());
        newContent.setReaders(new ArrayList(getReaders()));
        newContent.setLastClient(getLastClient());
        newContent.setMetaData(WGContent.META_WFHISTORY, new ArrayList(getWorkflowHistory()));        
        
        // Clear all items
        if (newContent.getItemNames().size() > 0) {
            newContent.removeAllItems();
            if (!newContent.getDatabase().hasFeature(WGDatabase.FEATURE_DIRECT_ENTITY_READDING)) {
                newContent.save();
            }
        }
        
        // Copy Items
        Iterator itemNames = getItemNames().iterator();
        String itemName;
        while (itemNames.hasNext()) {
        	itemName = (String) itemNames.next();
        	try {
        	    Object value = getItemValue(itemName);
        	    if (value instanceof List) {
                    value = new ArrayList((List) value);
                }
                newContent.setItemValue(itemName, value);
            }
            catch (WGIllegalDataException e) {
                WGFactory.getLogger().warn("Cannot copy item '" + itemName + "' because of unrestorable data", e);
            }
        }
        
        // Copy files
        if (newDoc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
            copyFilesCS5(newContent);
        }
        else {
            copyFilesLegacy(newContent);
        }
    	
    	// Clear all relations
        if (newDoc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && newContent.getRelationNames().size() > 0) {
            newContent.removeAllRelations();
            if (!newContent.getDatabase().hasFeature(WGDatabase.FEATURE_DIRECT_ENTITY_READDING)) {
                newContent.save();
            }
        }
    	
    	// Copy relations
    	if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && newDoc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
        	Iterator relations = getRelationNames().iterator();
        	String relName;
        	while (relations.hasNext()) {
                relName = (String) relations.next();
                WGRelationData address = getCore().getRelationData(relName);
                if (address != null) {
                    newContent.setRelation(address);
                }
                else {
                    WGFactory.getLogger().error("Cannot copy relation '" + relName + "' because it is not retrievable");
                }
            }
    	}
    	
    	super.pushData(newDoc);
    	
    }

    private void copyFilesCS5(WGContent newContent) throws WGAPIException {

        List<String> filesToCopy = getFileNames();
        
        // Remove files that are changed
        boolean anythingRemoved = false;
        Iterator<String> filesIt = filesToCopy.iterator();
        while (filesIt.hasNext()) {
            String file = filesIt.next();
            if (newContent.hasFile(file)) {
                String targetHash = newContent.getFileMetaData(file).getMd5Checksum();
                String sourceHash = getFileMetaData(file).getMd5Checksum();
                if (!targetHash.equals(sourceHash)) {
                    newContent.removeFile(file);
                    anythingRemoved = true;
                }
                
                // If already exists unchanged we can remove the file from the filesToCopy list
                else {
                    filesIt.remove();
                }
            }
        }
        
        // Remove files that are no longer existent on source
        List<String> targetFiles = newContent.getFileNames();
        targetFiles.removeAll(getFileNames());
        for (String file : targetFiles) {
            newContent.removeFile(file);
            anythingRemoved = true;
        }
        
        if (anythingRemoved && !newContent.getDatabase().hasFeature(WGDatabase.FEATURE_DIRECT_ENTITY_READDING)) {
            newContent.save();
        }
   
        // Copy Files
        try {
            Iterator fileNames = filesToCopy.iterator();
            while (fileNames.hasNext()) {
                String fileName = (String) fileNames.next();
                
                // filenames might contain path separators for e.g. in domino rtf fields (body/file.txt)
                // these files cannot be extracted without filename convertion bc. the file cannot be extracted
                // to the filesystem - therefore '/' is replaced by '���'
                // the attachFile method of the JDBC-ContentStore do the back convertion so
                // the file name stays constant after migration
                String convertedFileName = WGUtils.strReplace(fileName, "/", "���", true);
                                                
                final TemporaryFile tempFile = new TemporaryFile(convertedFileName, getFileData(fileName), WGFactory.getTempDir());
                newContent.afterSave(new SaveAction() {
                    @Override
                    public void run(WGDocument doc) throws Exception {
                        tempFile.delete();
                    }
                });
                newContent.attachFile(tempFile.getFile());

                WGFileMetaData sourceMeta = getFileMetaData(fileName);
                WGFileMetaData targetMeta = newContent.getFileMetaData(convertedFileName);
                sourceMeta.pushData(targetMeta);
                
            }
        } 
        catch (IOException e) {
                WGFactory.getLogger().error("Error pushing content data", e);
                throw new WGCreationException("IO Error copying files for content clone", e);
        }
        
    }

    private void copyFilesLegacy(WGContent newContent) throws WGAPIException, WGCreationException {
        // Clear all files
        if (newContent.getFileNames().size() > 0) {
            newContent.removeAllFiles();
            newContent.save();
        }
   
        // Copy Files
    	try {
        	Iterator fileNames = getFileNames().iterator();
        	while (fileNames.hasNext()) {
        		String fileName = (String) fileNames.next();
        		
        		// filenames might contain path separators for e.g. in domino rtf fields (body/file.txt)
        		// these files cannot be extracted without filename convertion bc. the file cannot be extracted
        		// to the filesystem - therefore '/' is replaced by '���'
        		// the attachFile method of the JDBC-ContentStore do the back convertion so
        		// the file name stays constant after migration
        		String convertedFileName = WGUtils.strReplace(fileName, "/", "���", true);
        		        		        		
        		final TemporaryFile tempFile = new TemporaryFile(convertedFileName, getFileData(fileName), WGFactory.getTempDir());
        		newContent.afterSave(new SaveAction() {
                    @Override
                    public void run(WGDocument doc) throws Exception {
                        tempFile.delete();
                    }
                });
        		newContent.attachFile(tempFile.getFile());
        		
        		
                // Copy extension data
                if (newContent.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1 &&
                        getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1) {
                    
                    WGFileMetaData sourceMeta = getFileMetaData(fileName);
                    WGFileMetaData targetMeta = newContent.getFileMetaData(convertedFileName);
                    sourceMeta.pushData(targetMeta);
                }

        		
        	}
        } 
    	catch (IOException e) {
        		WGFactory.getLogger().error("Error pushing content data", e);
        		throw new WGCreationException("IO Error copying files for content clone", e);
        }
    }

    /**
     * Returns a list of history entries, describing the events that occured to this document in authoring workflow
     * @throws WGAPIException
     */
    public List<String> getWorkflowHistory() throws WGAPIException {
        return (List<String>) getMetaDataList(WGContent.META_WFHISTORY);
    }
	
	/**
	 * Removes all relations from this content
	 * @throws WGAPIException 
	 */
	public void removeAllRelations() throws WGAPIException {
	    Iterator names = getRelationNames().iterator();
        while (names.hasNext()) {
            String name = (String) names.next();
            removeRelation(name);
        }
    }

    /**
	 * Creates a draft copy for this content that gets a new version number and starts in status "draft".
	 * @return The draft copy
	 * @throws WGAPIException
	 */
	public WGContent createDraftCopy() throws WGAPIException {
		return getDatabase().createDraftCopy(this);
	}

	/**
	 * Returns the list of allowed readers of this content.
	 * @throws WGAPIException 
	 */
	public List getReaders() throws WGAPIException {
		return (List)getMetaDataList(META_READERS);
	}
	
	/**
     * Returns the list of co-authors of this content.
     * @throws WGAPIException 
     */
    public List getCoauthors() throws WGAPIException {
        return (List)getMetaDataList(META_COAUTHORS);
    }

	/**
	 * Sets a list of readers allowed to see this content
	 * @param readers
	 * @throws WGAPIException 
	 */
	public boolean setReaders(List readers) throws WGAPIException {
		return setMetaData(META_READERS, readers);
	}
	
	   /**
     * Sets a list of co-authors allowed to edit this content
     * @param coauthors
     * @throws WGAPIException 
     */
    public boolean setCoauthors(List coauthors) throws WGAPIException {
        return setMetaData(META_COAUTHORS, coauthors);
    }

	/**
	 * Indicates if this content has "complete relationships", meaning it is no dummy, it has an associated language definition, a struct entry, and via this entry a content type
	 * @throws WGAPIException 
	 */
	public boolean hasCompleteRelationships() throws WGAPIException {

		if (isDummy()) {
			return false;
		}

		WGLanguage lang = getLanguage();
        
        // Dummy/temporary languages are ok
		if (lang == null) {
			return false;
		}
        
        // Dummy/temporary structs and content types are not
		WGStructEntry entry = getStructEntry();
		if (entry == null || entry.isTemporary() || entry.isDummy()) {
			return false;
		}

		WGContentType contentType = entry.getContentType();
		if (contentType == null || contentType.isTemporary() || contentType.isDummy()) {
			return false;
		}

		return true;
	}


	@Override
	protected boolean remove(WGDocument deletionRoot) throws WGAPIException {

		if (!db.isSessionOpen()) {
		    throw new WGClosedSessionException();
		}
		return innerRemove(deletionRoot, true);
        
	}
    
    @Override
	protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAuthorisationException, WGAPIException {
        
        super.performRemoveCheck(deepCheck, deletionRoot);
        
        if (db.getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_EDITOR) {
            
            if (db.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_AUTHOR) {
                // Conditions where an author may delete a content: Either project mode or his own draft (#00001184)
                if (!getDatabase().isProjectMode() &&  !(isAuthorOrOwner() && getStatus().equals(STATUS_DRAFT))) {
                    throw new WGAuthorisationException("You are not authorized to delete this content!", WGAuthorisationException.ERRORCODE_OP_NEEDS_EDITOR_LEVEL);
                }
            }
            else {
                throw new WGAuthorisationException("You are not authorized to delete contents in this database!", WGAuthorisationException.ERRORCODE_OP_NEEDS_AUTHOR_LEVEL);
            }
		}
	    
	    if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {

			WGDocument document = this.getStructEntry().testEditPageHierarchyRights();
			if (document != null) {
				if (document instanceof WGArea) {
					throw new WGAuthorisationException("User is not allowed to delete content in this area", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_AREA, document);
				}
				else {
					WGStructEntry entry = (WGStructEntry) document;
					throw new WGAuthorisationException(
						"User is not allowed to delete this content, because struct entry '"
							+ entry.getTitle()
							+ "' (Key "
							+ entry.getStructKey()
							+ ") disallows it", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_PAGE, entry);
				}

			}

	         // Check edit rights from content type
			WGContentType contentType = this.getStructEntry().getContentType();
			if (contentType != null && !contentType.mayCreateContent()) {
				throw new WGAuthorisationException("User is not allowed to delete content of this content type", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_CONTENTTYPE, contentType);
			}
			
            // Check edit rights from language
            WGLanguage language = getLanguage();
            if (language != null && !language.mayCreateContent()) {
                throw new WGAuthorisationException("User is not allowed to delete content of this language", WGAuthorisationException.ERRORCODE_OP_DENIED_BY_LANGUAGE, language);
            }
			
			// Check restricted relations
            if (deepCheck) {
    			if (getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5 && getDatabase().getSessionContext().isProtectedRelationsEnabled()) {
    			    for (WGRelationData rel : getIncomingRelations(true)) {
    			        if (rel.getType() == WGContent.RELATIONTYPE_PROTECTED && !getContentKey().equals(rel.getParentContentKey())) {
    			            
    			            // Test if the parent document of the relation is itself part of the deletion
    			            boolean throwException = true;

    			            if (deletionRoot != null) {
    			                WGStructEntry entry = getDatabase().getStructEntryByKey(rel.getParentContentKey().getStructKey());
    			                if (entry != null && (entry.equals(deletionRoot) || entry.isDescendantOf(deletionRoot))) {
    			                    throwException  = false;
    			                }
    			            }
    			            
    			            if (throwException) {
    			                throw new WGRestrictionException("The content is target of protected relation '" + rel.getName() + "' from content '" + rel.getParentContentKey().toString() + "' and cannot be deleted",
    			                        WGRestrictionException.ERRORCODE_OP_DENIED_BY_PROTECTED_RELATION,
    			                        new WGDocumentKey(WGDocument.TYPE_CONTENT, rel.getParentContentKey().toString(), null));
    			            }
    			        }
    			    }
    			}
            }
		}
    }
	
	/**
	 * Returns the type of the last client that edited this content.
	 * @return a client type string.
	 * @throws WGAPIException 
	 */
	public String getLastClient() throws WGAPIException {
		return (String) getMetaData(META_LASTCLIENT);
	}
	
	/**
	 * Sets the last client that edited this content
	 * @param client The client string.
	 * @throws WGAPIException 
	 */
	public boolean setLastClient(String client) throws WGAPIException {
		return setMetaData(META_LASTCLIENT, client);
	}
		
	
	/**
	 * Determines, if a content may be published (visible to a user) right now, testing all necessary conditions.
	 * @param content The content to test
	 * @param isAuthoringMode When true, the WGAPI assumes usage by an author in some kind of authoring app (who may see any docs that are visibility protected), when false assumes usage by a web user that may see only released content
	 * @param displayType The display type for which the content is used. Use constants WGContent.DISPLAYTYPE_...
	 * @return true, if the document may be published, false if not
	 * @throws WGAPIException 
	 */
	public static boolean mayBePublished(WGContent content, boolean isAuthoringMode, String displayType) throws WGAPIException {
	    
	    // Sanity
	    if (content == null) {
            return false;
        }

	    // When the content is in review and the user is approver for it we automatically enable authoring mode
	    if (content.getStatus().equals(WGContent.STATUS_REVIEW) && content.getWorkflowRole() >= WGWorkflow.ROLE_APPROVER) {
	        isAuthoringMode = true;
	    }
	    
	    // A user below ACL level author cannot use authoring mode
	    if (isAuthoringMode == true && content.getDatabase().getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_AUTHOR) {
	        isAuthoringMode = false;
	    }
	    
	    // If not released and may-not-edit author cannot use authoring mode
	    if (!content.getStatus().equals(WGContent.STATUS_REVIEW) && !content.getStructEntry().mayEditPage())
	    	isAuthoringMode = false;
	    
	    // Tests that are bypassed by authoring mode
	    if (!isAuthoringMode) {
	        
	            // Workflow status: document must be released
	            if (!content.getStatus().equals(WGContent.STATUS_RELEASE)) {
	                return false;
	            }

	            // Visible flag
	            if (content.isVisible() == false) {
	                return false;
	            }

    	        // Valid/From to dates
    	        Date now = new Date();
    	        if (content.getValidFrom() != null && content.getValidFrom().after(now)) {
    	            return false;
    	        }
    	        if (content.getValidTo() != null && content.getValidTo().before(now)) {
    	            return false;
    	        }
	        
	    }
	    
	    // Hidden flags for navigational structures
        if (displayType != null && content.isHiddenFrom().contains(displayType)) {
            return false;
        }
        
        return true;

	}
	
	/**
	 * A executed the static method "mayBePublished" on this content.
	 * @param isAuthoringMode When true, the WGAPI assumes usage by an author (who can see docs with status != published), when false assumes usage by a web user that may see only released content
	 * @param displayType The display type for which the content is used. Use constants WGContent.DISPLAYTYPE_...
	 * @return true, if the document may be published, false if not
	 * @throws WGAPIException 
	 */
	public boolean mayBePublished(boolean isAuthoringMode, String displayType) throws WGAPIException {
		return mayBePublished(this, isAuthoringMode, displayType);
	}

    /**
     * Sets the search score for this document on the last fulltext search.
     * This method should only get used internally by the WGAPI.
     * @param score
     * @throws WGClosedSessionException 
     */
    public void setSearchScore(float score) throws WGClosedSessionException {
        try {
            getSearchDetails().setScore(new Float(score));
        }
        catch (WGAPIException e) {
            // Unfortunately necessary to keep exception interface of this method
            throw new RuntimeException("Exception setting search score", e);
        }
    }
    
    /**
     * Sets the search explanation for this document
     * This method is for e.g. used by lucene in 'explain' mode to give back a detail information of the scoring
     * @param explaination
     * @throws WGClosedSessionException 
     */
    public void setSearchExplanation(Object explaination) throws WGClosedSessionException {
    	try {
            getSearchDetails().setExplanation(explaination);
        }
        catch (WGAPIException e) {
            // Unfortunately necessary to keep exception interface of this method
            throw new RuntimeException("Exception setting search explanation", e);
        }
    }


    /**
     * Sets detail information for this document related to the last search.
     * @param details
     * @throws WGClosedSessionException
     */
    public void setSearchDetails(SearchDetails details) throws WGClosedSessionException {
        getSessionData().setSearchDetails(details);
    }

    /*
     *  (non-Javadoc)
     * @see de.innovationgate.webgate.api.locking.Lockable#getParentLockable()
     */
    public Lockable getParentLockable() throws WGAPIException {
        return getStructEntry();
    }
    
    /**
     * Returns the next released content of identical language up the struct hierarchy
     * Depending on parameter skipUnreleasedParents it only tests the immediate parent or goes further up it that one has no released content in the right language.
     * It returns null if no suitable parent could be found.
     * @param skipUnreleasedParents Determines the behaviour when a parent has no released content. If true the parent is skipped and the method continues with the next higher parent. If false the method returns null.
     * @throws WGAPIException 
     * @return The content or null if none was found or we are at root.
     */
    public WGContent getParentContent(boolean skipUnreleasedParents) throws WGAPIException {
        
        WGStructEntry structEntry = getStructEntry();
        if (structEntry == null) {
            return null;
        }
        
        WGStructEntry parent = structEntry.getParentEntry();
        while (parent != null) {
            WGContent content = parent.getReleasedContent(getLanguage().getName());
            if (content != null) {
                return content;
            }
            if (skipUnreleasedParents) {
                parent = parent.getParentEntry();
            }
            else {
                return null;
            }
        }
        
        return null;
        
    }
    
    /**
     * Returns the next released content of identical language up the struct hierarchy
     * This variant skips parents which have no released content in the right language and returns the first parent which has one.
     * @throws WGAPIException 
     * @return The content or null if none was found or we are at root.
     */
    public WGContent getParentContent() throws WGAPIException {
        return getParentContent(true);
    }
    
    /**
     * Collects all released contents of identical language on child struct entries.
     * @throws WGAPIException
     */
    public List<WGContent> getChildContents() throws WGAPIException {
        
        List<WGContent> contents  = new ArrayList<WGContent>();
        Iterator<WGStructEntry> childEntries = getStructEntry().getChildEntries().iterator();
        WGStructEntry child;
        while (childEntries.hasNext()) {
            child = childEntries.next();
            WGContent content = child.getReleasedContent(getLanguage().getName());
            if (content != null) {
                contents.add(content);
            }
        }
        return contents;
        
    }
    
    /**
     * Collects a range of released contents of identical language on child struct entries.
     * This method first skips to the offset, then tries to retrieve the requested number of contents from that position, skipping entries without appropriate contents.
     * @param offset The offset of the first content to retrieve, 0 being the first one
     * @param size The number of entries to retrieve
     * @throws WGAPIException
     */
    public List<WGContent> getChildContents(int offset, int size) throws WGAPIException {
        
        // If we cannot retrieve ordered results we also cannot serve partial results without retrieving the whole list
        if (!getDatabase().hasFeature(WGDatabase.FEATURE_ORDERED_NAVIGATION_RESULTS)) {
            List<WGContent> entries = getChildContents();
            int toIndex = offset + size;
            if (toIndex > entries.size()) {
                toIndex = entries.size();
            }
            return entries.subList(offset, toIndex);
        }
        
        List<WGContent> contents  = new ArrayList<WGContent>();
        WGStructEntryIterator childEntries = getStructEntry().getChildEntryIterator(size);
        childEntries.skip(offset);
        
        WGStructEntry child;
        while (childEntries.hasNext()) {
            child = childEntries.next();
            WGContent content = child.getReleasedContent(getLanguage().getName());
            if (content != null) {
                contents.add(content);
                if (contents.size() >= size) {
                    break;
                }
            }
        }
        return contents;
        
    }
    
    /**
     * Returns an iterator over the child contents of this content
     * Internally the iterator fetches multiple pages at once from the backend for more performant backend retrieval processes. The size of those retrieval pages must be determined as argument.
     * This method does only return released contents. Pages without released contents in the current language are skipped.
     * @param pageSize The size of retrieval pages
     * @return An iterator over all child contents of this content
     * @throws WGAPIException
     */
    public synchronized WGHierarchyContentIterator getChildContentIterator(int pageSize) throws WGAPIException {
        return new WGHierarchyContentIterator(getStructEntry().getChildEntryIterator(pageSize), getLanguage().getName());
    }

    /**
     * Collects all released contents of identical language on sibling struct entries (including the current one)
     * @throws WGAPIException
     */
    public List<WGContent> getSiblingContents() throws WGAPIException {
        
        List<WGContent> contents  = new ArrayList<WGContent>();
        Iterator<WGStructEntry> childEntries = getStructEntry().getSiblingEntries().iterator();
        WGStructEntry child;
        while (childEntries.hasNext()) {
            child = (WGStructEntry) childEntries.next();
            WGContent content = child.getReleasedContent(getLanguage().getName());
            if (content != null) {
                contents.add(content);
            }
        }
        return contents;
        
    }
    
    /**
     * Creates a new child page, including struct entry and content.
     * Both documents are already saved when the method exits. The language used for the new content is the one of the current content.
     * @param contentType The content type of the page
     * @param title The title of the page that will be used for both struct entry 
     * @return The created content, already saved, but still in draft state
     * @throws WGAPIException
     */
    public WGContent createChildPage(WGContentType contentType, String title) throws WGAPIException {

        WGTransaction trans = getDatabase().startTransaction();
        try {
            WGStructEntry entry = getStructEntry().createChildEntry(contentType, title);
            entry.save();
            WGContent content = entry.createContent(getLanguage(), title);
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
     * @return Returns the retrievalLanguage.
     */
    protected String getRetrievalLanguage() {
        return retrievalLanguage;
    }

    /**
     * @return Returns the retrievalStatus.
     */
    protected String getRetrievalStatus() {
        return retrievalStatus;
    }


    
    /**
     * Returns the next released content on the previous struct entries of identical language
     * Starts with the immediately previous struct entry and goes on with the one before until
     * it finds a released content. If none is found it returns null.
     * @throws WGAPIException 
     * @return The content or null if none was found.
     */
    public WGContent getPreviousContent() throws WGAPIException {
        
        WGStructEntry struct = getStructEntry().getPreviousSibling();
        while (struct != null) {
            WGContent content =struct.getReleasedContent(getLanguage().getName());
            if (content != null) {
                return content;
            }
            struct = struct.getPreviousSibling();
        }

        return null;
        
    }
    
    /**
     * Returns the next released content on the following struct entries in identical language
     * Starts with the immediately next struct entry and goes on with the one after that until
     * it finds a released content. If none is found it returns null.
     * @throws WGAPIException 
     * @return The content or null if none was found.
     */
    public WGContent getNextContent() throws WGAPIException {
        
        WGStructEntry struct = getStructEntry().getNextSibling();
        while (struct != null) {
            WGContent content = struct.getReleasedContent(getLanguage().getName());
            if (content != null) {
                return content;
            }
            struct = struct.getNextSibling();
        }
        
        return null;
        
        
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
     * Retrieves the target content document of the given content relation
     * @param strName Name of the relation
     * @return The content behind this relation
     * @throws WGAPIException
     */
    public WGContent getRelation(String strName) throws WGAPIException {
        WGRelationData relData = getRelationData(strName);
        if (relData != null) {
            return relData.getTargetContent();
        }
        else {
            return null;
        }
    }

    /**
     * Returns the names of all relations that this content document holds
     * @return List of Strings, representing relation names
     * @throws WGAPIException
     */
    public List<String> getRelationNames() throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        return getCore().getRelationNames();
    }
    
    /**
     * Sets a content relation to the designated target content.
     * The target content must be of the same database as the current content
     * @param name The name of the relation
     * @param target The relation target content
     * @param type The type of relation. Use constants WGContent.RELATIONTYPE_...
     * @return The previously mapped content for this relation
     * @throws WGAPIException
     */
    public WGContent setRelation(String name, WGContent target, int type) throws WGAPIException {

        if (target.getDatabase() != getDatabase()) {
            throw new WGIllegalArgumentException("The target content of this relation is from another database: " + target.getDatabase().getDbReference());
        }
        
        if (!target.getStatus().equals(WGContent.STATUS_RELEASE) && type == RELATIONTYPE_PROTECTED) {
            throw new WGIllegalArgumentException("You cannot set a protected relation to a non-released content: " + target.getContentKey().toString());
        }

        
        Object structKey = target.getStructKey();
        String language = target.getLanguage().getName();

        WGContent content = setRelation(new WGRelationData(getDatabase(), getContentKey(), name, structKey, language, type, null));
        setEdited(true);
        
        return content;
        
        
    }
    
    /**
     * Sets a normal content relation to the designated target content.
     * The target content must be of the same database as the current content
     * @param name The name of the relation
     * @param target The relation target content
     * @return The previously mapped content for this relation
     * @throws WGAPIException
     */
    public WGContent setRelation(String name, WGContent target) throws WGAPIException {
        return setRelation(name, target, RELATIONTYPE_NORMAL);
    }

    /**
     * Sets a content relation with the given data
     * @param address The data of the relation to set
     * @return The previously mapped content of this relation
     * @throws WGAPIException
     */
    public WGContent setRelation(WGRelationData address) throws WGAPIException {
        WGDocumentCore previousTargetCore = getCore().setRelation(address);
        setEdited(true);
        if (previousTargetCore != null) {
            return getDatabase().getOrCreateContentObject(previousTargetCore);
        }
        else {
            return null;
        }
    }
    
    /**
     * Removes a content relation
     * @param name Name of the relation
     * @return The content document previously mapped to this relation
     * @throws WGAPIException
     */
    public WGContent removeRelation(String name) throws WGAPIException {
        
        name = name.toLowerCase();
        
        WGDocumentCore previousTargetCore = getCore().removeRelation(name);
        setEdited(true);
        
        if (previousTargetCore != null) {
            return getDatabase().getOrCreateContentObject(previousTargetCore);
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Sets a virtual item for this content document.
     * Virtual items remain at the document until the current session ends.
     * They are only visible in the current session and are retrievable via
     * normal item retrieval methods like {@link #getItemValue(String)}.
     * A virtual item whose name matches a persistent item on the same document 
     * hides this item untilthe end of the session.
     * @param name Name of the item
     * @param value Value of the item
     * @throws WGClosedSessionException 
     */
    public void setVirtualItemValue(String name, Object value) throws WGClosedSessionException {
        getSessionData().getVirtualItems().put(name.toLowerCase(), value);
    }
    
    /**
     * Removes an virtual item from this content document
     * @param name
     * @param value
     * @return The value of the removed item
     * @throws WGClosedSessionException 
     */
    public Object removeVirtualItemValue(String name, Object value) throws WGClosedSessionException {
        return getSessionData().getVirtualItems().remove(name.toLowerCase());
    }

    public List<String> getItemNames() throws WGAPIException {
        List<String> names = super.getItemNames();
        Map virtualItems = getSessionData().getVirtualItems();
        if (virtualItems.size() > 0) {
            Set virtualItemNames = new HashSet(virtualItems.keySet());
            virtualItemNames.removeAll(names);
            names.addAll(virtualItemNames);
        }
        return names;
    }

    public Object getItemValue(String strName) throws WGAPIException {
        
        Object value = getSessionData().getVirtualItems().get(strName.toLowerCase());
        if (value != null) {
            return cloneMutableObjects(value);
        }
        
        
        return super.getItemValue(strName);
    }

    public boolean hasItem(String itemName) throws WGAPIException {

        if (getSessionData().getVirtualItems().containsKey(itemName.toLowerCase())) {
            return true;
        }
        
        return super.hasItem(itemName);
    }
    
    /**
     * Returns the content class of this content.
     * @throws WGAPIException
     */
    public String getContentClass() throws WGAPIException {
        return (String) getMetaData(META_CONTENTCLASS);
    }
    
    /**
     * Sets the content class for this content.
     * The content class is a categorisation string for the current document
     * that can be use arbitrary. It is of special meaning when using
     * relations inversely to filter out those relations that come from a
     * special document class.
     * @param clazz The content class to set
     * @throws WGAPIException
     */
    public void setContentClass(String clazz) throws WGAPIException {
        setMetaData(META_CONTENTCLASS, clazz);
    }
    
    /**
     * Retrieves the released content of identical language on the root entry of the current content's struct
     * @throws WGAPIException
     */
    public WGContent getRootContent() throws WGAPIException {
        
        WGStructEntry root = getStructEntry().getRootEntry();
        return root.getReleasedContent(getLanguage().getName());
        
    }

    public List<PageHierarchyNode> getChildNodes() throws WGAPIException {
        return null;
    }
    
    
    @Override
    public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
        return new SkippingIteratorWrapper(Collections.EMPTY_LIST.iterator());
    }

    public Class getChildNodeType() {
         return null;
    }

    public String getNodeKey() throws WGAPIException {
         return getDocumentKey();
    }

    public String getNodeTitle(String language) throws WGAPIException {
        return getTitle();
    }

    public PageHierarchyNode getParentNode() throws WGAPIException {
        return getStructEntry();
    }
    
    /**
     * Returns the time when this content version was published
     * @throws WGAPIException
     */
    public Date getPublished() throws WGAPIException {
        return (Date) getMetaData(META_PUBLISHED);
    }
    
    protected void setPublished(Date published) throws WGAPIException {
        setMetaData(META_PUBLISHED, published);
    }
    
    /**
     * Returns the E-Mail address of the author of the current document
     * This method remains bc. of compatibility reasons. Unlike in earlier WGA versions it does not read an E-Mail address stored in the document
     * but rather does a lookup of the address at the databases authentication module, using the currently stored name under {@link #getAuthor()}.
     * @throws WGAPIException
     */
    public String getAuthorEMail() throws WGAPIException {
        
        AuthenticationModule auth = getDatabase().getAuthenticationModule();
        if (auth == null) {
            return null;
        }
        
        return auth.getEMailAddress(getAuthor());
        
    }
    
    /**
     * Returns a list of relations that point the current document
     * @param includeUnreleased Specify true to also retrieve documents in draft or approval state. False will retrieve only published documents.
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelations(boolean includeUnreleased) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        // Non-released contents cannot be target of relations
        if (!getStatus().equals(WGContent.STATUS_RELEASE)) {
            return Collections.emptyList();
        }
        
        return getDatabase().getCore().getIncomingRelations(getStructKey(), getLanguage().getName(), null, null, null, includeUnreleased, null);
        
    }

    /**
     * Returns a list of relations that point the current document, having a given relation name and being owned by a content of the given content class
     * @param contentClass Content class of the content owning the relations to retrieve
     * @param relName Name of the relations to retrieve
     * @param includeUnreleased Specify true to also retrieve documents in draft or approval state. False will retrieve only published documents.
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelations(String contentClass, String relName, boolean includeUnreleased) throws WGAPIException {
        return getIncomingRelations(contentClass, relName, includeUnreleased, null);
    }
    
    /**
     * Returns a list of relations that point the current document, having a given relation name and being owned by a content of the given content class
     * @param contentClass Content class of the content owning the relations to retrieve
     * @param relName Name of the relations to retrieve
     * @param includeUnreleased Specify true to also retrieve documents in draft or approval state. False will retrieve only published documents.
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelations(String contentClass, String relName, boolean includeUnreleased, String orderExpression) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        // Non-released contents cannot be target of relations
        if (!getStatus().equals(WGContent.STATUS_RELEASE)) {
            return Collections.emptyList();
        }
        
        WGColumnSet order = (orderExpression != null ? new WGColumnSet(orderExpression) : null);
        return getDatabase().getCore().getIncomingRelations(getStructKey(), getLanguage().getName(), contentClass, relName, null, includeUnreleased, order);
        
    }
    
    /**
     * Returns a list of relations that point the current document, having a given relation name and being owned by a released content of the given content class
     * @param contentClass Content class of the content owning the relations to retrieve
     * @param relName Name of the relations to retrieve
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelations(String contentClass, String relName) throws WGAPIException {
        return getIncomingRelations(contentClass, relName, false);
    }
    
    /**
     * Returns a list of relations that point the current document, having a given relation group name and being owned by a content of the given content class
     * @param contentClass Content class of the content owning the relations to retrieve
     * @param relGroupName Group name of the relations to retrieve
     * @param includeUnreleased Specify true to also retrieve documents in draft or approval state. False will retrieve only published documents.
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelationsOfGroup(String contentClass, String relGroupName, boolean includeUnreleased) throws WGAPIException {
        return getIncomingRelationsOfGroup(contentClass, relGroupName, includeUnreleased, null);
    }
    
    /**
     * Returns a list of relations that point the current document, having a given relation group name and being owned by a content of the given content class
     * @param contentClass Content class of the content owning the relations to retrieve
     * @param relGroupName Group name of the relations to retrieve
     * @param includeUnreleased Specify true to also retrieve documents in draft or approval state. False will retrieve only published documents.
     * @param orderExpression Order expression denoting the order in which to return relations, evaluated against the parent content of the relation
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelationsOfGroup(String contentClass, String relGroupName, boolean includeUnreleased, String orderExpression) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        // Non-released contents cannot be target of relations
        if (!getStatus().equals(WGContent.STATUS_RELEASE)) {
            return Collections.emptyList();
        }
        
        WGColumnSet order = (orderExpression != null ? new WGColumnSet(orderExpression) : null);
        return getDatabase().getCore().getIncomingRelations(getStructKey(), getLanguage().getName(), contentClass, null, relGroupName, includeUnreleased, order);
        
    }
    
    /**
     * Returns a list of relations that point the current document, having a given relation group name and being owned by a released content of the given content class
     * @param contentClass Content class of the content owning the relations to retrieve
     * @param relGroupName Group name of the relations to retrieve
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelationsOfGroup(String contentClass, String relGroupName) throws WGAPIException {
        return getIncomingRelationsOfGroup(contentClass, relGroupName, false);
    }
    
    /**
     * Returns a list of relations that point the current document. Only relations of published documents will be returned.
     * @throws WGAPIException 
     */
    public List<WGRelationData> getIncomingRelations() throws WGAPIException {
        return getIncomingRelations(false);
    }
    
    /**
     * Returns the data of a content relation
     * @param name Name of the relation
     * @return The relation data or null if the relation does not exist
     * @throws WGAPIException
     */
    public WGRelationData getRelationData(String name) throws WGAPIException {
        
        if (!getDatabase().isSessionOpen()) {
            throw new WGClosedSessionException();
        }
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return null;
        }
        
        Object value = null;
        
        Map<String,Object> cache = null;
        boolean cacheable = isCachingEnabled();
        if (cacheable) {
            cache = getRelationCache();
        }
        
        String cacheKey = "relation:" + name;
        if (cache != null && !isCoreRetrieved()) {
            value = cache.get(cacheKey);
        }
        
        if (value == null) {
            value = getCore().getRelationData(name);
            if (cache != null && getDatabase().getSessionContext().isCacheWritingEnabled() && !isEdited()) {
                cache.put(cacheKey, nullPlaceholder(value));
            }
        }
        
        if (value instanceof WGRelationData) {
            return (WGRelationData) value;
        }
        else {
            return null;
        }
        
    }
    
    
    
    /**
     * Assigns a new version number to this content
     * Used to resolve key conflicts with contents.
     * Note: The current WGContent object is INVALID after this call and MUST NOT be used for any further operations. It should be refetched from he database
     * @throws WGAPIException
     */
    public void reassignVersionNumber() throws WGAPIException {
        
        synchronized (getDatabase()) {

            // Find the next free version
            FreeContentVersionFinder finder =  getDatabase().new FreeContentVersionFinder(getStructEntry(), getLanguage());
            int newVersion = finder.findNewVersion();        
    
            // Unmap the content under its old keys.
            WGDocumentKey oldDocKey = getDocumentKeyObj();
            getDatabase().unmapDocumentObject(oldDocKey);
            
            
            setMetaData(WGContent.META_VERSION, new Integer(newVersion));
            
            // We must keep the lastmodified date here, bc. there is a high change that this doc will
            // also be picked up by the multi released validation. When that happens, we STILL want
            // this version of the document to be the older one!
            save(getLastModified());
            
            // Changing version numbers of content documents is normally not supported so this WGContent object becomes invalid
            // It was remapped on save with new keys. We unmap it again to prevent it from being served again
            dropCore();
            getDatabase().unmapDocumentObject(getDocumentKeyObj());
            getDatabase().getSessionContext().removeDocumentContext(oldDocKey);
            getDatabase().getSessionContext().removeDocumentContext(getDocumentKeyObj());
            

        }
        
    }

    @Override
    public int getType() {
        return WGDocument.TYPE_CONTENT;
    }
    
    protected void updateBackendCaches(WGDocumentCore core) {
        super.updateBackendCaches(core);
        
        try {
            getSessionData().setBackendStatus((String) retrieveMetaData(core, WGContent.META_STATUS));
            gatherRetrievalKeys();
        }
        catch (WGAPIException e) {
            WGFactory.getLogger().error("Error updating struct entry backend cache", e);
        }
        
        
    }
    
    /**
     * Returns the status of this content that is currently stored to the content store.
     * This is especially helpful in status change events to determine the previous status of the document
     * @throws WGClosedSessionException 
     */
    public String getStoredStatus() throws WGClosedSessionException {
        return getSessionData().getBackendStatus();
    }
    
    /**
     * Returns the names of all relations that belong to the given relation group
     * @param group
     * @return List of relation names
     */
    public List<String> getRelationNamesOfGroup(String group) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
        
        return getCore().getRelationNamesOfGroup(group, null);
        
    }
    
    /**
     * Returns the names of all relations that belong to the given relation group in a given order
     * @param group The group name
     * @param orderExpression Order expression denoting the order in which to return relations, evaluated against their target content
     * @return List of relation names
     */
    public List<String> getRelationNamesOfGroup(String group, String orderExpression) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            return Collections.emptyList();
        }
                
        WGColumnSet order = (orderExpression != null ? new WGColumnSet(orderExpression) : null);
        return getCore().getRelationNamesOfGroup(group, order);
        
    }


    /**
     * Returns the target documents of all relations that belong to the given relation group
     * @param group The group to query
     * @return List of target documents
     */
    public List<WGContent> getRelationsOfGroup(String group) throws WGAPIException  {
        
        List<WGContent> targets = new ArrayList<WGContent>();
        for (String relName : getRelationNamesOfGroup(group)) {
            WGContent con = getRelation(relName);
            if (con != null) {
                targets.add(con);
            }
        }
        return targets;
        
    }
    
    /**
     * Returns the target documents of all relations that belong to the given relation group in a given order
     * @param group The group to query
     * @param orderExpression Order expression denoting the order in which to return relations, evaluated against their target content
     * @return List of target documents
     */
    public List<WGContent> getRelationsOfGroup(String group, String orderExpression) throws WGAPIException  {
        
        List<WGContent> targets = new ArrayList<WGContent>();
        for (String relName : getRelationNamesOfGroup(group, orderExpression)) {
            WGContent con = getRelation(relName);
            if (con != null) {
                targets.add(con);
            }
        }
        return targets;
        
    }
    
    /**
     * Returns the data of all relations that belong to the given relation group
     * @param group The group to query
     * @return List of relation data objects
     */
    public List<WGRelationData> getRelationsDataOfGroup(String group) throws WGAPIException  {
        
        List<WGRelationData> targets = new ArrayList<WGRelationData>();
        for (String relName : getRelationNamesOfGroup(group)) {
            WGRelationData relData = getRelationData(relName);
            if (relData != null) {
                targets.add(relData);
            }
        }
        return targets;
        
    }
    
    /**
     * Returns the data of all relations that belong to the given relation group in a given order
     * @param group The group to query
     * @param order Order expression denoting the order in which to return relations, evaluated against their target content
     * @return List of relation data objects
     */
    public List<WGRelationData> getRelationsDataOfGroup(String group, String order) throws WGAPIException  {
        
        List<WGRelationData> targets = new ArrayList<WGRelationData>();
        for (String relName : getRelationNamesOfGroup(group, order)) {
            WGRelationData relData = getRelationData(relName);
            if (relData != null) {
                targets.add(relData);
            }
        }
        return targets;
        
    }
    
    /**
     * Creates a new relation and adds it to a relation group. The relation will be issued a random name that is returned by this method.
     * @param group The group of the relation.
     * @param target The target of the relation
     * @param relType The type of relation. Use constants RELATIONTYPE_*.
     * @return The name of the created relation
     * @throws WGAPIException
     */
    public String addRelationToGroup(String group, WGContent target, int relType) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGNotSupportedException("Relation groups are only supported in content stores of version 5 or higher");
        }
        
        String relName = group + "#" + UIDGenerator.generateUID();
        WGRelationData relation = new WGRelationData(getDatabase(), getContentKey(true), relName, target.getStructKey(), target.getLanguage().getName(), relType, group);
        setRelation(relation); 
        return relName;
    }
    
    /**
     * Removes the relation to a given target document from a relation group, if it contains such a relation.
     * If multiple relations point to the given target the method will remove all of them.
     * @param group The group
     * @param target The target document that should no longer be addressed by the group
     * @return true if a relation was found and removed, false otherwise
     * @throws WGAPIException
     */
    public boolean removeRelationFromGroup(String group, WGContent target) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5) {
            throw new WGNotSupportedException("Relation groups are only supported in content stores of version 5 or higher");
        }

        
        boolean somethingRemoved = false;
        for (String relName : getRelationNamesOfGroup(group)) {
            WGContent relTarget = getRelation(relName);
            if (target.equals(relTarget)) {
                removeRelation(relName);
                somethingRemoved = true;
            }
        }
        return somethingRemoved;
    }
    
    /**
     * Creates a new normal relation and adds it to a relation group. This is like calling {@link #addRelationToGroup(String, WGContent, int)} with relation type {@link #RELATIONTYPE_NORMAL}.
     * @param group The group of the relation.
     * @param target The target of the relation
     * @return The name of the created relation
     * @throws WGAPIException
     */
    public String addRelationToGroup(String group, WGContent target) throws WGAPIException {
        return addRelationToGroup(group, target, RELATIONTYPE_NORMAL);
    }
    
    /**
     * Removes all relations of the given group
     * @param group The relation group to remove
     * @throws WGAPIException
     */
    public void clearRelationGroup(String group) throws WGAPIException {
        
        List<WGRelationData> targets = new ArrayList<WGRelationData>();
        for (String relName : getRelationNamesOfGroup(group)) {
            removeRelation(relName);
        }
    }
    
    /**
     * Returns the names of all existing relation groups on this content document
     * @throws WGAPIException
     */
    public Set<String> getRelationGroups() throws WGAPIException {
        
        Set<String> groups = new HashSet<String>();
        for (String relName : (List<String>) getRelationNames()) {
            WGRelationData relData = getRelationData(relName);
            if (relData != null && relData.getGroup() != null) {
                groups.add(relData.getGroup());
            }
        }
        return groups;
        
    }
    
    /**
     * Returns if this document is pending for release, meaning it has passed the workflow and is waiting for an external process to publish it
     * @throws WGAPIException 
     */
    public boolean isPendingRelease() throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() >= 5) {
            return (Boolean) getMetaData(META_PENDINGRELEASE);
        }
        else {
            if (hasItem(ITEM_PENDINGRELEASE)) {
                Number pending = (Number) getItemValue(ITEM_PENDINGRELEASE);
                return (pending.intValue() == 1);
            }
            else {
                return (Boolean) METAINFO_PENDINGRELEASE.getDefaultValue();
            }
            
        }
        
    }
    
    /**
     * Sets if this document is pending for release, meaning it has passed the workflow and is waiting for an external process to publish it
     * @throws WGAPIException 
     */
    protected void setPendingRelease(boolean pending) throws WGAPIException {
        
        if (getDatabase().getContentStoreVersion() >= 5) {
            setMetaData(META_PENDINGRELEASE, pending);
        }
        else {
             setItemValue(ITEM_PENDINGRELEASE, (pending ? 1 : 0));
        }
        
    }
    
    /**
     * Tests if this content is publicly accessible, i.e. anyone can read its data
     * @throws WGAPIException
     */
    public boolean isPublic() throws WGAPIException {
        
        if (isDummy()) {
            return true;
        }
        
        if (getDatabase().hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && !getStructEntry().isPublic()) {
            return false;
        }
        
        List<String> readers = getReaders();
        return WGDatabase.anyoneAllowed(readers, true);
        
    }
    
    /**
     * Sets the browser title of this document, the title to be shown as for the browser window when this document is loaded
     * @param title The title
     * @throws WGAPIException
     */
    public void setBrowserTitle(String title) throws WGAPIException {
        setMetaData(META_BROWSERTITLE, title);
    }

    /**
     * Returns the browser title of this document, the title to be shown as for the browser window when this document is loaded
     * @throws WGAPIException
     */
    public String getBrowserTitle() throws WGAPIException {
        return (String) getMetaData(META_BROWSERTITLE);
    }
    
    @Override
    protected boolean isReadableForUser() throws WGAPIException {

        if (getDatabase().getSessionContext().isMasterSession()) {
            return true;
        }
        
        // Hierarchy check per base WGAPI
        if (getDatabase().isPageReadersEnabled()) {
            WGStructEntry entry = getStructEntry();
            if (!entry.mayReadContent()) {
                return false;
            }
        }

        // Local check
        List<String> readers = new ArrayList<String>();
        List<String> conReaders = getReaders();
        if (conReaders != null) {
            readers.addAll(conReaders);
        }

        // If list is empty, return immediately
        if (WGDatabase.anyoneAllowed(readers)) {
            return true;
        }

        // Test user names
        readers.addAll(getDatabase().getMandatoryReaders());
        return getDatabase().getCore().isMemberOfUserList(readers);

    }
    
    @Override
    protected List<WGDocumentKey> getCacheParents() {
        return Collections.singletonList(new WGDocumentKey(WGDocument.TYPE_STRUCTENTRY, this.retrievalContentKey.getStructKey().toString(), null));
    }
    
    /**
     * Creates a duplicate of this content object that represents a search result for some specific part of the content, identified by the given SearchDetails object
     * @param searchDetails The object representing the search result
     * @return The duplicate
     * @throws WGAPIException
     */
    public WGContent createSearchResultDuplicate(SearchDetails searchDetails) throws WGAPIException {
        WGContent dup = new WGContent(getDatabase(), getCore(), new WGDocumentObjectFlags().setTempDuplicate(true));
        dup.setSearchDetails(searchDetails);
        return dup;
    }
    
    @Override
    protected Cache createDocumentCache() {
        return new ContentCache();
    }
    
    private ContentCache getCache() {
        return (ContentCache) _cache;
    }

    private Map<String, Object> getRelationCache() {
        return getCache().getRelationCache();
    }

    private Map<String,Boolean> getVisibilityCache() {
        return getCache().getVisibilityCache();
    }

   
    
}
