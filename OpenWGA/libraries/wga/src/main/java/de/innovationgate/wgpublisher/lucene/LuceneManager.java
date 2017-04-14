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
package de.innovationgate.wgpublisher.lucene;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;

import org.apache.commons.vfs2.FileMonitor;
import org.apache.log4j.Logger;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.de.GermanAnalyzer;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.analysis.es.SpanishAnalyzer;
import org.apache.lucene.analysis.fr.FrenchAnalyzer;
import org.apache.lucene.analysis.it.ItalianAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.CorruptIndexException;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriter.MaxFieldLength;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.queryParser.QueryParser.Operator;
import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.Explanation;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.Similarity;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.highlight.Formatter;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.search.highlight.QueryTermExtractor;
import org.apache.lucene.search.highlight.QueryTermScorer;
import org.apache.lucene.search.highlight.WeightedTerm;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.dom4j.DocumentException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.MetaInfo;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGConfigurationException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentEvent;
import de.innovationgate.webgate.api.WGContentEventListener;
import de.innovationgate.webgate.api.WGContentIterator;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseConnectListener;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseRevision;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGSystemException;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.webgate.api.WGUpdateLog;
import de.innovationgate.webgate.api.WGWrongRevisionException;
import de.innovationgate.webgate.api.workflow.WGWorkflow;
import de.innovationgate.wga.common.beans.LuceneConfiguration;
import de.innovationgate.wga.common.beans.LuceneIndexFileRule;
import de.innovationgate.wga.common.beans.LuceneIndexItemRule;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginConfig;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.LuceneManagerConfiguration;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.lucene.analysis.FileHandler;
import de.innovationgate.wgpublisher.lucene.analysis.FileHandlerException;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class LuceneManager implements WGContentEventListener, WGDatabaseConnectListener {
    
    public static final Logger LOG = Logger.getLogger("wga.lucene");
    
    public static final String VIRTUALMETA_PATH = "PATH";

    public static final String VIRTUALMETA_PARENT = "PARENT";

    public static final String VIRTUALMETA_AREA = "AREA";

    public static final String VIRTUALMETA_CONTENTTYPE = "CONTENTTYPE";

    public static final String VIRTUALMETA_KEY = "KEY";

    public static final String VIRTUALMETA_DBKEY = "DBKEY";
    
    public static final String VIRTUALMETA_PAGEPUBLISHED = "PAGEPUBLISHED";
    
    public static final String[] VIRTUALMETAS = new String[] {
        VIRTUALMETA_AREA,
        VIRTUALMETA_CONTENTTYPE,
        VIRTUALMETA_DBKEY,
        VIRTUALMETA_KEY,
        VIRTUALMETA_PARENT,
        VIRTUALMETA_PATH,
        VIRTUALMETA_PAGEPUBLISHED
    };

    public static final List<String> TECHNICAL_ITEMS = Arrays.asList(new String[] {
        WGWorkflow.ITEM_APPROVEDBY.toLowerCase(),
        WGWorkflow.ITEM_ARCHIVINGDATE.toLowerCase(),
        WGWorkflow.ITEM_COMMENT.toLowerCase(),
        WGWorkflow.ITEM_CURRENTLEVEL.toLowerCase(),
        WGWorkflow.ITEM_INITIATOR.toLowerCase(),
        WGWorkflow.ITEM_LEVELNAME.toLowerCase(),
        WGWorkflow.ITEM_PUBLISHINGDATE.toLowerCase(),
        WGWorkflow.ITEM_RELEASEDATE.toLowerCase(),
        WGWorkflow.ITEM_REPLACEMENT_REASON.toLowerCase(),
        WGWorkflow.ITEM_REVIEWERCOUNT.toLowerCase(),
        WGWorkflow.ITEM_REVIEWERS.toLowerCase(),
        WGWorkflow.ITEM_WFNAME.toLowerCase(),
        WGWorkflow.ITEM_WRITEACCESS.toLowerCase()
    });
    
    
    /**
     * search_scope for lucene search
     * Set to String SEARCHSCOPE_DOMAIN, SEARCHSCOPE_WGA, SEARCHSCOPE_DB, SEARCHSCOPE_DB_LIST
     */
    public static final String QUERYOPTION_SEARCHSCOPE = "lucene_search_scope";
    
    /**
     * comma separated list of db_keys to search in if search scope is SEARCHSCOPE_DB_LIST
     */
    public static final String QUERYOPTION_SEARCHDBKEYS = "lucene_search_db_keys";
    
    /**
     * Boolean option to enable highlighting support
     */
    public static final String QUERYOPTION_HIGHLIGHT = "highlight";
    
    /**
     * if this option is given as native query option - virtual content will be included in search results
     * per default virtual content is excluded 
     */
    public static final String NATIVE_QUERYOPTION_INCLUDEVIRTUALCONTENT = "includeVirtualContent";
    
    public static final String NATIVE_QUERYOPTION_DOCTYPE = "doctype";
    
    /**
     * values for QUERYOPTION_SEARCHSCOPE
     */
    public static final String SEARCHSCOPE_DOMAIN = "lucene_search_scope_domain";
    public static final String SEARCHSCOPE_WGA = "lucene_search_scope_wga";
    public static final String SEARCHSCOPE_DB = "lucene_search_scope_db";
    public static final String SEARCHSCOPE_DB_LIST = "lucene_search_scope_db_list";
    
    /**
     * constants for indexing internal fields
     */
    private static final String INDEXFIELD_UNIQUEKEY = "$LUCENE_KEY";
    // used for index cross reference
    private static final String INDEXFIELD_PARENTKEY = "$LUCENE_PARENTKEY";
    private static final String INDEXFIELD_DOCUMENTKEY = "$LUCENE_DOCUMENTKEY"; 
    // also used by LuceneMultiDBResultset
    public static final String INDEXFIELD_DBKEY = "$LUCENE_DBKEY";
    public static final String INDEXFIELD_CONTENTKEY = "$LUCENE_CONTENTKEY";
    
    public static final String INDEXFIELD_ISVIRTUALCONTENT = "$LUCENE_ISVIRTUALCONTENT";
    
    public static final String INDEXFIELD_ALLCONTENT = "allcontent";
    public static final String INDEXFIELD_ALLATTACHMENTS = "allattachments";
    private static final String SORTITEM_PREFIX = "$sort_";
    
    // how to index empty dates validFrom and validTo
    private static final String EMPTY_VALID_FROM = "00000000000000";
    private static final String EMPTY_VALID_TO = "99999999999999";
    
    public static final String SYSPROP_DISABLE_AUTO_INDEX_OPTIMIZATION = "de.innovationgate.wga.lucene.disableAutoIndexOptimization";
    public static final String SYSPROP_USE_CONSTANT_IDF_SIMILARITY = "de.innovationgate.wga.lucene.useConstantIDFSimilarity";

    public static final String INDEXFIELD_FILENAME = "$LUCENE_FILENAME";
    
    public static final String INDEXFIELD_DOCTYPE = "$LUCENE_DOCTYPE";
    public static final String DOCTYPE_CONTENT = "content";
    public static final String DOCTYPE_ATTACHMENT = "attachment";
    private static final String DOCTYPE_ALL = "all";

    private WGACore _core;
    
    private long _indexInterval = 1000 * 5;
    private Indexer _indexer;

    private File _dir;
    private Directory _indexDirectory;
    
    private boolean _rebuildingIndex = false;
    
    private LuceneIndexConfiguration _indexConfig;
    
    // meta keywords for lucene index
    // contains metas of an indexDocument which are indexed as keyword
    // used by IndexRuleBasedQueryParser during search()
    private Set _metaKeywordFields;
        
    // stores all indexed dbKeys as key and contains indexingrules as value
    private Map<String,LuceneConfiguration> _indexedDbs = new HashMap<String,LuceneConfiguration>();
    
    private volatile boolean _indexerIsRunning;

    private boolean _indexReleasedOnly=false;
    private int _booleanQueryMaxClauseCount = BooleanQuery.getMaxClauseCount();
    private int _maxDocsPerDBSession = 50;
    
    public static final String TAGINFO_UNSPECIFICQUERY = "LUCENE_TAGINFO_UNSPECIFICQUERY";
    
    public static final String TAGINFO_SIMPLIFIEDQUERY = "LUCENE_TAGINFO_SIMPLYFIEDQUERY";

	public static final String RELATION_PREFIX = "$rel_";
	public static final String RELATIONGROUP_PREFIX = "$relgroup_";

    // contains opened resultssets per thread
    private ThreadLocal _resultsetList = new ThreadLocal();

	private boolean _shutdownRequested = false;
    
    // flag if manager has been started (method startup has been called on this instance)
	private boolean _started = false;
    
    
    private static DecimalFormat DECIMALFORMAT_SORTFIELD;
    
    private Object _indexWriteLock = new Object();
    
    private Object _indexingRequestLock = new Object();
    
    static {
        //build padding string
        String format = "";
        String maxLong = new Long(Long.MAX_VALUE).toString();
        for (int i=0; i < maxLong.length(); i++) {
            format += "0";
        }
        format += ".";
        for (int i=0; i < maxLong.length(); i++) {
            format += "#";
        }                        
        
        // create dcf for padding
        DECIMALFORMAT_SORTFIELD = new DecimalFormat(format);
    };
    
    private static DateFormat DATEFORMAT_SORTFIELD = new SimpleDateFormat("yyyyMMddHHmmss");
    
    private static DateFormat DATEFORMAT_KEYWORD = new SimpleDateFormat("yyyyMMddHHmmss");
    
    // B000047B2
    private static NumberFormat NUMBERFORMAT_KEYWORD = DecimalFormat.getNumberInstance(java.util.Locale.ENGLISH);    
    static {
    	NUMBERFORMAT_KEYWORD.setGroupingUsed(false);
    	NUMBERFORMAT_KEYWORD.setMaximumFractionDigits(Integer.MAX_VALUE);
    	NUMBERFORMAT_KEYWORD.setMaximumIntegerDigits(Integer.MAX_VALUE);
    }
    
    // shared index searcher
	private volatile IndexSearcher _indexSearcher;         

    public static final int MAX_CONCURRENT_SEARCHES = 1000;



    
    private final Semaphore _indexSearcherSemaphore = new Semaphore(MAX_CONCURRENT_SEARCHES, true);

    private Map<String, List<LuceneIndexEnhancer>> _indexEnhancers = new HashMap<String, List<LuceneIndexEnhancer>>();
    
	
    /**
     * info bean for an indexing run
     * used in performAdditions and performDeletions to determine the status of the current indexupdate
     *
     */
    private class IndexingProcessInfo {
    	int _deletedDocs = 0;
    	int _addedDocs = 0;
    	int _reinsertedRequests = 0;
    	int _skippedRequests = 0;
    	
		public int getAddedDocs() {
			return _addedDocs;
		}
		public int getDeletedDocs() {
			return _deletedDocs;
		}
		public int getReinsertedRequests() {
			return _reinsertedRequests;
		}
		public int getSkippedRequests() {
			return _skippedRequests;
		}
		
		public void docAdded() {
			_addedDocs++;
		}
    	
		public void docDeleted() {
			_deletedDocs++;
		}
		
		public void requestReinserted() {
			_reinsertedRequests++;
		}
		
		public void requestSkipped() {
			_skippedRequests++;
		}
    }

    public static LuceneManager retrieve(WGACore core, LuceneManagerConfiguration config) throws IllegalArgumentException, IOException, DocumentException, WGSystemException {

        String lucenePath = config.getPath();
        File dirFile = core.getOrCreateWGAFolder(lucenePath);
        
        if (dirFile == null || !dirFile.isDirectory()) {
            throw new IllegalArgumentException("The file '" + config.getPath() + "' does not exist or is no directory");
        }

        LuceneManager manager =  new LuceneManager(core, dirFile);
        manager.setBooleanQueryMaxClauseCount(config.getMaxBooleanClauseCount());
        manager.setMaxDocsPerDBSession(config.getMaxDocsPerDBSession());
        manager.setIndexInterval(config.getIndexInterval());
        manager.init();
        return manager;

    }

    private LuceneManager(WGACore core, File dir) throws IOException, DocumentException, WGSystemException {
            	    	
        // Set lock directory to WGA temp dir
        System.setProperty("org.apache.lucene.lockDir", core.getWgaTempDir().getPath());
        
        _dir = dir;
        _core = core;
        
        // init metakeywords - used during search analysis
        List<MetaInfo> metaInfoList = new ArrayList<MetaInfo>();
        metaInfoList.addAll(WGFactory.getInstance().getMetaInfos(WGContent.class).values());
        metaInfoList.addAll(WGFactory.getInstance().getMetaInfos(WGFileMetaData.class).values());
        Iterator metaInfos = metaInfoList.iterator();
        _metaKeywordFields = new HashSet();
        while (metaInfos.hasNext()) {
            MetaInfo metaInfo = (MetaInfo) metaInfos.next();
            //B0000485A
            if (metaInfo.getLuceneIndexType() == MetaInfo.LUCENE_INDEXTYPE_KEYWORD) {
	            if (!_metaKeywordFields.contains(metaInfo.getName())) {
	                _metaKeywordFields.add(metaInfo.getName());
	                Iterator synonyms = metaInfo.getSynonyms().iterator();
	                while (synonyms.hasNext()) {
	                    String synonym = (String) synonyms.next();
	                    _metaKeywordFields.add(synonym);
	                }
	            }
            }
        }
        
        // Add virtual metas
        _metaKeywordFields.addAll(Arrays.asList(VIRTUALMETAS));               
        
        if (Boolean.getBoolean(SYSPROP_USE_CONSTANT_IDF_SIMILARITY)) {
            _customSimilarity = new ConstantIDFSimilarity();
    }
    }
    
    private void init() throws IOException, DocumentException {              
        _indexer = new Indexer();
        _timer = new Timer();
        _timer.schedule(_indexer, 1000 * 60, _indexInterval);
        
        _indexConfig = new LuceneIndexConfiguration(_core, _dir);
        _indexDirectory = FSDirectory.open(_dir);  
        
        if (_indexConfig.isNewConfiguration()) {        	
            IndexWriter writer = new IndexWriter(_indexDirectory, null, true, MaxFieldLength.LIMITED);
            writer.close();
        }

        // Try to remove an existing lock
        if (IndexWriter.isLocked(_indexDirectory)) {
            LOG.warn("Lucene index directory was locked. Removing lock now");
            IndexWriter.unlock(_indexDirectory);
        }
    }
    
    public void updateIndex() throws InterruptedException {
        
        Runnable updateIndexRunnable = new Runnable() {
            @Override
            public void run() {
                _indexer.run();
                
            }
        };
        Thread t = new java.lang.Thread();
        t.start();
        t.join(1000 * 10);
        
    }
    
    public long getIndexInterval() {
        return _indexInterval;
    }

    public void setIndexInterval(long indexInterval) {
        _indexInterval = indexInterval;
    }

    /**
     * disables the timer task, deletes the index_dir and reinit index
     */
    public void rebuildIndex() throws IOException, DocumentException {
        try {
            if (_rebuildingIndex) {
                LOG.warn("Rebuilding of lucene index already in progress, please wait ...");
                throw new IllegalStateException("Rebuilding of lucene index already in progress, please wait ...");
            }
            LOG.info("Rebuilding lucene index started ...");
            _rebuildingIndex = true;

            // destroy timer task
            if (_timer != null) {
                _timer.cancel();
                _timer = null;
            }
            
            // stop indexer
            _indexer = null;

            // Delete all files in _dir
            File[] indexFiles = _dir.listFiles();
            if (indexFiles != null) {
                for (int i = 0; i < indexFiles.length; i++) {
                    File file = indexFiles[i];
                    if (!file.delete()) {
                        LOG.error("Could not delete lucene index file '" + file.getAbsolutePath() + "'");
                        _rebuildingIndex = false;
                        throw new IOException("Could not delete lucene index file '" + file.getAbsolutePath() + "'.");
                    }
                }
            }

            // reinit
            this.init();
            
            this.startup();
        }
        catch (IOException e) {
            _rebuildingIndex = false;
            throw e;
        }
        catch (DocumentException e) {
            _rebuildingIndex = false;
            throw e;
        }
    }
    
    /**
     * reindex given dbkey
     * @param dbkey
     */
    public void rebuildIndex(String dbkey) {
    	addDBUpdateRequest(new IndexingRequest(dbkey, null, true));
    }    

    public void destroy() {
        // destroy timer task
        if (_timer != null) {
            _timer.cancel();

            
            // notify indexer about shutdown
            _shutdownRequested = true;
            
            // wait until current timerTask is completed to allow a lucene to close all index writers
            LOG.info("Waiting for lucene to shutdown...");
            while (_indexerIsRunning) {
                // wait
                Thread.yield();
                try {
                    Thread.sleep(1000);
                }
                catch (InterruptedException e) {
                    //ignore
                }
            }            
            _timer = null;
            LOG.info("Lucene finished shutdown.");
        }
    }
    
    /**
     * if wga starts up it notifies lucene by this method
     */
    public synchronized void startup() {        
    	_started = true;
    	
    	
        // load indexed dbs from configfile
        _indexedDbs = _indexConfig.retrieveIndexedDbs();
        
        /*
         * not necessary here - already done in configurationHasChanged()
         * 
        // for each indexed db, register eventlistener if db is connected to the core
        Iterator indexedDBKeys = _indexedDbs.keySet().iterator();
        while (indexedDBKeys.hasNext()) {
            String dbKey = (String) indexedDBKeys.next();
            WGDatabase db = (WGDatabase) _core.getContentdbs().get(dbKey);
            if (db != null) {
                registerForContentEvents(db);
            }
            else {
                LOG.info("Indexed db " + dbKey + " is not connected to WGA");
            }
        }*/
        
        configurationHasChanged(null);
    }
    
    /**
     * wga-configuration has changed, read new configuration and do necessary index updates
     */
    public synchronized void configurationHasChanged(Set newConnectedDBKeys) {
    	if (!_started) {
    		// skip config update if lucene manager has not been started (method startup called) yet
    		// this happens on an initial WGA startup
    		return;
    	}
    	
    	if(_core.getWgaConfiguration().getLuceneManagerConfiguration().isUseLanguageAnalyzers()){
            _core.addAnalyzerMapping("de", new GermanAnalyzer(org.apache.lucene.util.Version.LUCENE_35));
            _core.addAnalyzerMapping("en", new EnglishAnalyzer(org.apache.lucene.util.Version.LUCENE_35));
            _core.addAnalyzerMapping("it", new ItalianAnalyzer(org.apache.lucene.util.Version.LUCENE_35));
            _core.addAnalyzerMapping("fr", new FrenchAnalyzer(org.apache.lucene.util.Version.LUCENE_35));
            _core.addAnalyzerMapping("es", new SpanishAnalyzer(org.apache.lucene.util.Version.LUCENE_35));
    	}
    	else{
    		_core.removeAllAnalyzerMappings();
    	}
        
    	_indexReleasedOnly = _core.getWgaConfiguration().getLuceneManagerConfiguration().isIndexReleasedContentsOnly();
    	
        // check if each DB in _indexedDBKeys is in configfile and enabled by wga
        // if not create dropRequest
        Iterator itIndexedDbKeys = _indexedDbs.keySet().iterator();
        while (itIndexedDbKeys.hasNext()) {
            String dbKey = (String) itIndexedDbKeys.next();
            
            ContentStore dbConfig = _core.getWgaConfiguration().getContentStore(dbKey);
            if (dbConfig == null) {
                // indexed db not found in config, remove db and drop from index
                removeDatabase(dbKey, true);
                // remove from indexed dbs, cannot be done in removeDatabase() because of current iteration over indexedDbKeys
                //itIndexedDbKeys.remove(); // now done in removeDatabase via copy-replace
            } else if ( !dbConfig.isEnabled() ) {
                // if db was disabled, only remove from indexedDbs - do not drop index
                //itIndexedDbKeys.remove();
                removeDatabase(dbKey, false);
            }
        }        
        
        // get all active databases from core
        Iterator contentDbs = _core.getContentdbs().values().iterator();
        while (contentDbs.hasNext()) {
            WGDatabase db = (WGDatabase) contentDbs.next();
            // use db only if it is a real contentStore (has feature FULLCONTENTFEATURES)
            if ( (db != null) && (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) ) {
                                
                
                // WGA Plugins are not fulltext indexed
                if (db.getDbReference().startsWith(PluginConfig.PLUGIN_DBKEY_PREFIX)) {
                    continue;
                }
                
                // If db not yet connected, listen for connect event and execute this method again when it happens
                if (!db.isConnected()) {
                    db.addDatabaseConnectListener(this);
                    continue;
                }
                
                createOrUpdateDBIndex(db, newConnectedDBKeys);                   
            }
        }
        
    }

    private void createOrUpdateDBIndex(WGDatabase db, Set newConnectedDBKeys) {
        initIndexEnhancers(db);
        
        LuceneConfiguration config;
        // if database is already in index on the filesystem
        HashMap currentDBsInIndexOnFileSystem = _indexConfig.retrieveIndexedDbs(); 
        String dbKey = db.getDbReference();
        if (currentDBsInIndexOnFileSystem.containsKey(dbKey)) {
            LuceneConfiguration oldConfig = (LuceneConfiguration) currentDBsInIndexOnFileSystem.get(dbKey);
            LuceneConfiguration newConfig = retrieveLuceneConfig(dbKey);
            // if configuration has changed
            if (!oldConfig.equals(newConfig)) {
                // update database
                updateDatabase(dbKey, newConfig);
                if (newConfig.isEnabled()) {
	                registerForContentEvents(db);
                    // rebuild index of db
                    LOG.info("Indexing rules for db '" + dbKey + "' has changed. Indexed content will be truncated and reindexed ...");
                    addDBUpdateRequest(new IndexingRequest(dbKey, null, true));
                } else {
                    // remove db and drop from index
                    removeDatabase(dbKey, true);
                    // remove from indexed dbs - this is not done by removeDatabase because of list iteration issue see creating droprequests above
                    //_indexedDbs.remove(dbKey); // now done in removeDatabase via copy-replace
                }
            } else {
                if (newConnectedDBKeys == null) {
                    // this is an initial startup - perform incremental updates
                    registerForContentEvents(db);
                    addDBUpdateRequest(new IndexingRequest(dbKey, null));                    
                } else if (newConnectedDBKeys.contains(dbKey)) {
                    // perform incremental update bc. db was connected within this configurationupdate
                    registerForContentEvents(db);
                    addToIndexedDbs(dbKey, newConfig);
                    addDBUpdateRequest(new IndexingRequest(dbKey, null));
                }
            }
        } else {
            // if db is not yet indexed --> check config
            config = retrieveLuceneConfig(dbKey);
            if ( (config != null) && (config.isEnabled()) ) {
                // add db
                addDatabase(db, config);
            }
        }
    }
    
    private void initIndexEnhancers(WGDatabase db) {
        List<LuceneIndexEnhancer> enhancers = _indexEnhancers.get(db.getDbReference());
        if (enhancers != null) {
            Iterator<LuceneIndexEnhancer> it = enhancers.iterator();
            while (it.hasNext()) {
                LuceneIndexEnhancer enhancer = it.next();
                try {
                    enhancer.shutdown();
                }
                catch (WGException e) {
                    LOG.error("Failed to shutdown LuceneIndexEnhancer '" + enhancer.getClass().getName() + "' for database '" + db.getDbReference() + "'", e);
                }
            }
        }
        _indexEnhancers.remove(db);
        
        enhancers = new ArrayList<LuceneIndexEnhancer>();
        ContentStore dbConfig = _core.getWgaConfiguration().getContentStore(db.getDbReference());
        if (dbConfig != null) {
            de.innovationgate.wga.config.LuceneIndexConfiguration luceneIndexConfig = dbConfig.getLuceneIndexConfiguration();
            if (luceneIndexConfig != null && luceneIndexConfig.getIndexEnhancers() != null) {
                Iterator<String> enhancerClassNames = luceneIndexConfig.getIndexEnhancers().iterator();
                while (enhancerClassNames.hasNext()) {
                    String enhancerClassName = enhancerClassNames.next();
                    try {
                        Class enhancerClass = Class.forName(enhancerClassName, true, _core.getLibraryLoader());
                        if (enhancerClass != null) {
                            LuceneIndexEnhancer enhancer = (LuceneIndexEnhancer)enhancerClass.newInstance();
                            enhancer.init(_core, db);
                            enhancers.add(enhancer);
                            LOG.info("LuceneIndexEnhancer '" + enhancerClassName + "' successfully initialized for database '" + db.getDbReference() + "'.");
                        }
                    }
                    catch (Exception e) {
                        LOG.error("Failed to initialize LuceneIndexEnhancer '" + enhancerClassName + "' for database '" + db.getDbReference() + "'", e);
                    }                    
                }
                _indexEnhancers.put(db.getDbReference(), enhancers);
            }
        }
    }

    private void registerForContentEvents(WGDatabase db) {
        if (db != null) {
            // ensure that we are registered as contentEventListener
            LOG.info("Lucene registered for receiving content changes from db " + db.getDbReference());
            // first remove, than register to ensure not to register twice
            db.removeContentEventListener(this);
            db.addContentEventListener(this);
        } else {
            LOG.warn("Lucene is unable to register as ContentEventListener for 'null' db.");
        }
    }
    
    /**
     * adds an database to lucene
     * @param db WGDatabase
     * @param config LuceneConfiguration
     */
    private void addDatabase(WGDatabase db, LuceneConfiguration config) {
        LOG.info("Lucene index enabled for db '" + db.getDbReference() + "'.");
        
        //update configfile        
        _indexConfig.addDBConfig(db.getDbReference(), config);
        
        registerForContentEvents(db);
        
        
        
        addToIndexedDbs(db.getDbReference(), config);
        addDBUpdateRequest(new IndexingRequest(db.getDbReference(), null));          
    }
    
    private void addToIndexedDbs(String dbkey, LuceneConfiguration config) {
        synchronized (_indexedDbs) {
            Map<String,LuceneConfiguration> newIndexedDbs = new HashMap<String,LuceneConfiguration>();
            newIndexedDbs.putAll(_indexedDbs);
            newIndexedDbs.put(dbkey, config);
            _indexedDbs = newIndexedDbs;
        }
    }
    
    private void removeFromIndexedDbs(String dbkey) {
        synchronized (_indexedDbs) {
            // copy indexed dbs
            HashMap newIndexedDbs = new HashMap();
            newIndexedDbs.putAll(_indexedDbs);
            // remove in copy
            newIndexedDbs.remove(dbkey);
            // switch maps
            _indexedDbs = newIndexedDbs;
        }
    }    
    
    private void removeDatabase(String dbKey, boolean dropindex) {
        if (dropindex) {
            LOG.info("Indexed db '" + dbKey + "' was removed from wga or disabled for lucene. DB will be dropped from index ...");
            addDropDBRequest(new IndexingRequest(dbKey, null));
            _additionRequestsMap.remove(dbKey);
        }
        
        // try to remove contentEventListener
        WGDatabase db = (WGDatabase) _core.getContentdbs().get(dbKey);
        if (db != null) {
            LOG.info("Removing '" + this.getClass().getName() + "' as ContentEventListener for db " + db.getDbReference() + ".");            
            db.removeContentEventListener(this);
        }        
        
        // remove db from indexedDbs
        removeFromIndexedDbs(dbKey);
    }
        
    private void updateDatabase(String dbKey, LuceneConfiguration config) {
        addToIndexedDbs(dbKey, config);
        _indexConfig.updateDBConfig(dbKey, config);
    }
    
    /**
     * get LuceneConfiguration from config file
     * @param dbKey DatabaseKey
     * @return LuceneConfiguration, null if not found
     */
    public LuceneConfiguration retrieveLuceneConfig(String dbKey) {       
        //Create Lucene Config 
    	ContentStore dbConfig = _core.getWgaConfiguration().getContentStore(dbKey);
    	if (dbConfig != null) {
    		de.innovationgate.wga.config.LuceneIndexConfiguration luceneIndexConfig = dbConfig.getLuceneIndexConfiguration();
    		if (luceneIndexConfig != null) {
    			LuceneConfiguration config = new LuceneConfiguration();
    			config.setEnabled(luceneIndexConfig.isEnabled());
    			List itemRules = LuceneIndexItemRule.getRules(luceneIndexConfig.getItemRules());
    			List fileRules = LuceneIndexFileRule.getRules(luceneIndexConfig.getFileRules());
    			config.setItemRules(itemRules);
    			config.setFileRules(fileRules);    			
    			return config;
    		} else {
    			LOG.error("Could not retrieve lucene config for db '" + dbKey + "'");
                return null;	
    		}
    	} else {
            LOG.error("Could not retrieve lucene config for db '" + dbKey + "'");
            return null;
        }
    }   
    
    private void addAdditionRequest(IndexingRequest request) {
        synchronized (_indexingRequestLock) {
        // add to list and set service status running for DB
            Queue<IndexingRequest> requests = _additionRequestsMap.get(request.getDbkey());
        if (requests == null) {
        	requests = new ConcurrentLinkedQueue<LuceneManager.IndexingRequest>();
            _additionRequestsMap.put(request.getDbkey(), requests);
        }        
        requests.add(request);     
    }
    }

    private void addDBUpdateRequest(IndexingRequest request) {
        // if request is not yet in list
        if (!_dbUpdateRequests.contains(request)) {        
            // add to list
            _dbUpdateRequests.add(request);
            // if index rebuild for this db is needed - we should clear the lastupdate flag in index configuration
            // this will ensure that if wga is terminated before the index rebuild finished
            // the index will be rebuilded on the next wga runtime
            if (request.isIndexRebuildNeeded()) {
            	_indexConfig.clearLastUpdated(request.getDbkey());
            }
        }
    }        
    
    private void addDeletionRequest(IndexingRequest request) {
        synchronized (_indexingRequestLock) {
        // add to list and set service status running for DB
            Queue<IndexingRequest> requests = _deletionRequestsMap.get(request.getDbkey());
        if (requests == null) {
                requests = new ConcurrentLinkedQueue<IndexingRequest>();
            _deletionRequestsMap.put(request.getDbkey(), requests);
        }
        requests.add(request);
    }        
    }        

    private void addDropDBRequest(IndexingRequest request) {
        // add to list and set service status running for DB
        _dropDBRequests.add(request);
    }       

    private void addTruncateDBRequest(IndexingRequest request) {
        // add to list and set service status running for DB
        _truncateDBRequests.add(request);
    }        
    
    

    public void optimizeIndex() {
        optimizeIndex(LOG);
    }
    
    public void optimizeIndex(Logger log) {
		synchronized (_indexWriteLock) {
			IndexWriter writer = null;
			try {				
				writer = new IndexWriter(_indexDirectory, null, MaxFieldLength.UNLIMITED);
				log.info("Optimizing lucene index ...");
				long start = System.currentTimeMillis();
				writer.optimize();
				long end = System.currentTimeMillis();
				log.info("Optimized lucene index in " + (end - start) + " ms.");
			}
			catch (Throwable e) {
				log.error("Exception performing index optimization.", e);
			}
			finally {
				if (writer != null) {
					try {
						writer.close();
						writer = null;
					}
					catch (IOException e1) {
						log.error("Unable to close lucene index writer because of I/O exception", e1);
					}
				}
			}
		}
	}
    
    /**
     * @param content
     * @return configured lucene analyzer for content language or default analyzer
     * @throws WGAPIException 
     */
    public Analyzer retrieveAnalyzer(WGContent content) throws WGAPIException {            
        String langCode = content.getLanguage().getName();
        Analyzer analyzer = null;
        if (langCode != null) {
            analyzer = _core.getAnalyzerForLanguageCode(langCode.substring(0, 1));
            if (analyzer == null) {
                analyzer = _core.getDefaultAnalyzer();
            }
        } else {
            analyzer = _core.getDefaultAnalyzer();
        }
        return analyzer;
    }
    
    Timer _timer;

    public class Indexer extends TimerTask {

        private long _lastFullIndexingRun = System.currentTimeMillis();

        public synchronized void run() {

            try {
                _indexerIsRunning = true;
                
                Thread.currentThread().setName("WGA Lucene Indexer");
                Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
                LOG.debug("Running lucene indexer");                
                                
                performDbUpdateRequests();
                performDropDBRequests();
                performTruncateDBRequests();
                
                // ensure that deletions and additions are in sync
                // --> threadsave contentHasBeenSaved() and contentHasBeenDeleted()
                Object[] keys = _indexedDbs.keySet().toArray();
                int currentKeySize = keys.length;
                for (int i = 0; i < currentKeySize; i++) {
                    String dbKey = (String) keys[i];
                                     
                    // if shutdown is requested - skip further index updates and let wga shutdown
                    // lastupdate flag of index will not be updated - therefore all pending requests will be indexed
                    // during incremental update on next startup
                    if (_shutdownRequested) {
                    	break;
                    }
                    
                	// revision of this indexer run --> result in the lastmodified revision of index config
                    WGDatabase db = _core.getContentdbs().get(dbKey);
                    if (db == null || !db.isConnected()) {
                        continue;
                    }
                    
                    _core.openContentDB(db, null, true);
                        
                    int currentDeletionsCount = 0;
                    int currentAdditionsCount = 0;
                        
                    WGDatabaseRevision revisionOfThisUpdate = null;
                    
                    synchronized (_indexingRequestLock) {                            
                    	revisionOfThisUpdate = db.getRevisionObject();
                    	
                        // count deletions for db for this run
                        Queue deletions = _deletionRequestsMap.get(dbKey);                        
                        if (deletions != null) {
                            currentDeletionsCount = deletions.size();
                        }
    
                        // count additions for db for this run
                        Queue additions = _additionRequestsMap.get(dbKey);                       
                        if (additions != null) {
                            currentAdditionsCount = additions.size();
                        }                    
                    }
                    
                    IndexingProcessInfo processInfo = new IndexingProcessInfo();
                    
                    performDeletionRequests(dbKey, currentDeletionsCount, processInfo);
                    performAdditionRequests(dbKey, currentAdditionsCount, processInfo);
                    if (currentDeletionsCount > 0 || currentAdditionsCount > 0) {
                        LOG.info("Lucene indexer performed " + WGUtils.DECIMALFORMAT_STANDARD.format(processInfo.getAddedDocs()) + " additions and " + WGUtils.DECIMALFORMAT_STANDARD.format(processInfo.getDeletedDocs()) + " deletions in fulltext index of database '" + dbKey + "'");
                    }
                    
                    // if docs has been deleted or added and no requests has been reinserted or skipped this db is finshed
                    // the lastupdate flag should be updated in index config
                    if ((processInfo.getAddedDocs() > 0 || processInfo.getDeletedDocs() > 0) && processInfo.getReinsertedRequests() == 0 && processInfo.getSkippedRequests() == 0) {
                    	_indexConfig.setLastUpdated(dbKey, revisionOfThisUpdate);
                    }
                }                
                
                //reset rebuilding index flag
                if (_rebuildingIndex) {
                    _rebuildingIndex = false;
                    LOG.info("Rebuilding Lucene index finished.");
                }
                
                _lastFullIndexingRun = System.currentTimeMillis();
              
            }
            catch (WGUnavailableException e) {
                if (e.getDatabase() != null) {
                    LOG.error("Unable to perform index update because database '" + e.getDatabase().getTitle() + "' is unavailable");
                }
                else {
                    LOG.error("Unable to perform index update because a database is unavailable");
                }
            }
            catch (WGIllegalArgumentException e) {
                LOG.error(e.getMessage());
            }
            catch (IOException e) {
                LOG.error("Unable to perform index update because of I/O exception", e);
            }
            catch (ParseException e) {
                LOG.error("Unable to perform index update because of XML parsing exception: '" + e.getMessage());
            }
            catch (Throwable e) {
                LOG.error("Unable to perform index update because of exception", e);
            }
            finally {
                try {
                    WGFactory.getInstance().closeSessions();
                    LOG.debug("Finished running lucene indexer");
                } catch (Exception e) {
                    // keep timer alive
                }
                _indexerIsRunning = false;                
            }
        }
           
        /**
         * performs addition requests for the database dbKey
         * only requests within currentListSize are processed to ensure additions and deletions are in sync
         * @param dbKey database to process
         * @param currentListSize number of items from additionrequestsList to process
         * @param info IndexingProcessInfo of the current run
         * @throws IOException
         */
        private void performAdditionRequests(String dbKey, int currentListSize, IndexingProcessInfo info) throws IOException {
            // continue only if necessary (requests in list)
            if (currentListSize <= 0) {
                return;
            }
            
            IndexingRequest request = null;
            WGDatabase db;
            Set<String> alreadyAdded = new HashSet<String>();

            LOG.debug("Performing index addition requests for db '" + dbKey + "'.");
            // Additions
            synchronized (_indexWriteLock) {
                IndexWriter writer = null;
                try {
                    writer = new IndexWriter(_indexDirectory, null, MaxFieldLength.UNLIMITED);
    
                    // additionrequests are grouped by dbKey
                    // get list from map
                    Queue<IndexingRequest> requests = _additionRequestsMap.get(dbKey);
                    // iterate over current listSize to be sure to process addition
                    // and deletions in sync
                    // if an request is succesfully completed (indexed), remove it
                    // from the list
                    // else add the request again to the end of the list
                    // elements should not be removed outside this code from lists
                    // in additionRequestMap
                    // while indexing process is running
                    int docsWithinThisSession = 0;
                    for (int j = 0; j < currentListSize; j++) {
                        try {
                            Thread.yield();
                            
                        	// if shutdown is requested skip current request and let wga shutdown
                        	// bc. of skip requests is != 0 lastupdate flag of index will not be updated - therefore all pending requests will be indexed
                            // during incremental update on next startup
                        	if (_shutdownRequested) {
                        		info.requestSkipped();
                        		break;
                        	}
                            
                            
                            request = requests.poll();
                            if (request == null) {
                                LOG.warn("Performing additions on lucene indexer finished early at " + (j+1) + " + of " + currentListSize + " entries.");
                                break;
                            }
    
                            // check if database for this request is in indexing
                            // list
                            // if not, this is an (old) readded request
                            if (!_indexedDbs.containsKey(dbKey)) {
                                continue;
                            }
    
                            if (alreadyAdded.contains(request.toString())) {
                                continue;
                            }
    
                            db = (WGDatabase) _core.getContentdbs().get(request.getDbkey());
    
                            if ((db == null) || (!db.isReady())) {
                                info.requestReinserted();
                                LOG.info(
                                        "Indexer cannot access database '" + request.getDbkey() + "'. Indexing requests for content " + request.getDocumentKey()
                                                + " will be tried again later.");
                                // put request back in list and continue with next
                                // request
                                requests.add(request);
                                continue;
                            }
    
                            if (!db.isSessionOpen() && db.openSession() == WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
                                LOG.info(
                                        "Indexer cannot access database '" + request.getDbkey() + "'. Indexing requests for content " + request.getDocumentKey()
                                                + " will be tried again later.");
                                info.requestReinserted();
                                // put request back in list and continue with next
                                // request
                                requests.add(request);
                                continue;
                            } else {
                                // db session is open - ensure not more than <MaxDocsPerDBSession> docs are indexed within this session to ensure gc of hibernate-entities
                                // @see Bugfix: B000037EA
                                if (docsWithinThisSession >= getMaxDocsPerDBSession()) {
                                    db.getSessionContext().clearCache();                                
                                    if (!db.isSessionOpen() && db.openSession() == WGDatabase.ACCESSLEVEL_NOTLOGGEDIN) {
                                        LOG.info(
                                                "Indexer cannot access database '" + request.getDbkey() + "'. Indexing requests for content " + request.getDocumentKey()
                                                        + " will be tried again later.");
                                        info.requestReinserted();
                                        // put request back in list and continue with next
                                        // request
                                        requests.add(request);
                                        continue;
                                    }
                                    docsWithinThisSession = 0;
                                }
                            }
                            
                            
    
                            db.getSessionContext().setTask("WGA Lucene Indexer");
                            db.getSessionContext().setBatchProcess(true);
                            try {
                                WGContent content = (WGContent) db.getDocumentByKey(request.getDocumentKey());
                                if (content != null){
                                	if(content.getStatus().equals(WGContent.STATUS_RELEASE) || !_indexReleasedOnly) {
	                                    addToIndex(writer, db, content);
	                                    info.docAdded();
	                                    docsWithinThisSession++;
	                                    // the content-core is not needed anymore for data gathering - so drop core to free HEAP
	                                	content.dropCore();
                                	}
                                }
                                else {
                                    // content was deleted
                                    LOG.debug("Content " + request.getDocumentKey() + " was deleted during indexer run. Indexing skipped.");
                                }
                            } catch (WGBackendException e) {
                                // check if backendexception is repairable
                                if (e.isRepairable()) {
                                    // content temporary not accessable bc. of backendexception
                                    LOG.info("Content " + request.getDocumentKey() + " cannot be accessed this time bc. of WGBackendException '" + e.getMessage() + "'. Indexing will be tried again later.");
                                    // raise request timeout
                                    request.raiseRetryCount();
                                    // if not timed out yet
                                    // put request back in list and continue
                                    // with next request
                                    if (!request.isTimedOut()) {
                                        requests.add(request);
                                    } else {
                                        LOG.warn("Content " + request.getDocumentKey() + " could not be accessed " + request.getRetryCount() + " times bc. of WGBackendException '" + e.getMessage() + "'. Indexing request timed out. Luceneindex may be inconsistent.");
                                        continue;
                                    } 
                                } else {
                                    LOG.warn("Unrecoverable error during indexing of content " + request.getDocumentKey() + ". Luceneindex may be inconsistent.", e);
                                }
                            }                        
                        }
                        catch (Exception e) {
                            LOG.error(e.getMessage(), e);
                            if (request != null) {
                                info.requestReinserted();
                                LOG.info(
                                        "Indexer cannot access database '" + request.getDbkey() + "'. Indexing requests for content " + request.getDocumentKey()
                                                + " will be tried again later.");
                                // put request back in list and continue with next
                                // request
                                addAdditionRequest(request);
                                continue;
                            }
                        }
                        alreadyAdded.add(request.toString());
                        if (j != 0 && j % 100 == 0) {
                            LOG.info("Performed " + WGUtils.DECIMALFORMAT_STANDARD.format(j) + " of " + WGUtils.DECIMALFORMAT_STANDARD.format(currentListSize) + " queued index addition requests for app '" + dbKey + "'");
                        }
                    }
                }
                finally {
                    if (writer != null) {
                        try {
                        	writer.commit();
                        	boolean disableAutoOptimize = WGUtils.toBoolean(System.getProperty(SYSPROP_DISABLE_AUTO_INDEX_OPTIMIZATION), true);
                            if (!disableAutoOptimize) {
                                writer.optimize();
                            }
                        }
                        catch (Throwable e) {
                            LOG.error("Exception committing lucene index writer", e);
                        }
                        
                        try {
                            writer.close();
                            writer = null;
                        }
                        catch (Throwable e) {
                            LOG.error("Exception committing lucene index writer", e);
                        }
                    }
                }
            }       
        }       

             
        /**
         * performs deletion requests for the database dbKey
         * only requests within currentListSize are processed to ensure additions and deletions are in sync
         * @param dbKey database to process
         * @param currentListSize number of items from deletionrequestsList to process
         * @param info IndexingProcessInfo of the current run
         * @throws IOException
         */ 
        private void performDeletionRequests(String dbKey, int currentListSize, IndexingProcessInfo info) throws IOException {
            
            // continue only if necessary (requests in list)
            if (currentListSize <= 0) {
                return;
            }
            
            IndexingRequest request;
            // WGDatabase db;
            Set<String> alreadyDeleted = new HashSet<String>();

            LOG.debug("Performing index deletion requests for app '" + dbKey + "'.");
            // Deletions
            synchronized (_indexWriteLock) {
                IndexReader reader = null;
                try {                
                    reader = IndexReader.open(_indexDirectory, false);
    
                    // deletionrequests are grouped by dbKey
                    // get list from deletionRequestsMap
                    Queue<IndexingRequest> requests = _deletionRequestsMap.get(dbKey);
    
                    // iterate over current list size to ensure deletions and
                    // additions are in sync
                    // elements should not be removed outside this code from lists
                    // in deletionRequestsMap
                    // while indexing process is running
                    for (int j = 0; j < currentListSize; j++) {
                    	
                    	// if shutdown is requested skip current request and let wga shutdown
                    	// bc. of skip requests is != 0 lastupdate flag of index will not be updated - therefore all pending requests will be indexed
                        // during incremental update on next startup
                    	if (_shutdownRequested) {
                    		info.requestSkipped();
                    		break;
                    	}
                    	
                        Thread.yield();
    
                        request = requests.poll();
                        if (request == null) {
                            LOG.warn("Performing deletions on lucene indexer finished early at " + (j+1) + " + of " + currentListSize + " entries.");
                            break;
                        }
                        
                        
    
                        if (alreadyDeleted.contains(request.toString())) {
                            continue;
                        }
    
                        deleteContent(reader, request.getDbkey(), request.getDocumentKey());
                        info.docDeleted();
                        alreadyDeleted.add(request.toString());
                        
                        if (j != 0 && j % 100 == 0) {
                            LOG.info("Performed " + WGUtils.DECIMALFORMAT_STANDARD.format(j) + " of " + WGUtils.DECIMALFORMAT_STANDARD.format(currentListSize - j) + " queued index deletion requests for app '" + dbKey + "'");
                        }
                    }
                }
                finally {
                    if (reader != null) {
                        try {
                            reader.close();
                            reader = null;
                        }
                        catch (IOException e1) {
                            LOG.error("Unable to close lucene index reader because of I/O exception: '" + e1.getMessage());
                        }
                    }
                }
            }
        }        
        

        private void performDropDBRequests() throws IOException, WGUnavailableException {
            
            // continue only if requests in list
            if (listIsEmpty(_dropDBRequests)) {
                return;
            }
            
            IndexingRequest request;
            Set<String> alreadyDeleted = new HashSet<String>();
            
            LOG.debug("Performing index drop db requests");
            // Deletions
            synchronized (_indexWriteLock) {
                IndexReader reader = null;
                try {
                    reader = IndexReader.open(_indexDirectory, false);
    
                    while (true) {
                        Thread.yield();
                        request = fetchNextFromList(_dropDBRequests);
                        if (request == null) {
                            break;
                        }
                        
                        if (alreadyDeleted.contains(request.toString())) {
                            continue;
                        }
    
                        dropDatabaseFromIndex(reader, request.getDbkey());
                        alreadyDeleted.add(request.toString());
                    }
                }
                finally {
                    if (reader != null) {
                        try {
                            reader.close();
                            reader = null;
                        }
                        catch (IOException e1) {
                            LOG.error("Unable to close lucene index reader because of I/O exception: '" + e1.getMessage());
                        }
                    }
                }
            }
        }        
        

        private void performTruncateDBRequests() throws IOException, WGUnavailableException {

            // continue only if requests in list
            if (listIsEmpty(_truncateDBRequests)) {
                return;
            }            
            
            IndexingRequest request;
            Set<String> alreadyTruncated = new HashSet<String>();
            
            LOG.debug("Performing index truncate db requests");
            // Deletions
            synchronized (_indexWriteLock) {                            
                IndexReader reader = null;
                try {
                    reader = IndexReader.open(_indexDirectory, false);
    
                    while (true) {
                        Thread.yield();
                        request = fetchNextFromList(_truncateDBRequests);
                        if (request == null) {
                            break;
                        }
                        
                        if (alreadyTruncated.contains(request.toString())) {
                            continue;
                        }
    
                        truncateDatabaseFromIndex(reader, request.getDbkey());
                        alreadyTruncated.add(request.toString());
                    }
                }
                finally {
                    if (reader != null) {
                        try {
                            reader.close();
                            reader = null;
                        }
                        catch (IOException e1) {
                            LOG.error("Unable to close lucene index reader because of I/O exception: '" + e1.getMessage());
                        }
                    }
                }
            }
        }        
        
        
        /**
         * @throws WGUnavailableException
         * @throws IOException
         * @throws ParseException
         */
        private boolean performDbUpdateRequests() throws WGUnavailableException, IOException, ParseException {
            IndexingRequest request = null;
            WGDatabase db;
            // Update db requests. Will result in filling other
            // IndexingRequest
            // lists.            
            LOG.debug("Performing database update requests");
            
            // iterate over current list size
            // if an request is succesfully completed (splitted in addition and deleterequests), remove it from the list
            // else add the request again to the end of the list
            // elements should not be removed outside this code from _dbUpdateRequests 
            // while indexing process is running 
            int currentListSize = _dbUpdateRequests.size();
            for (int i=0; i < currentListSize; i++) {                
                try {
                    Thread.yield();
                    
                    request = _dbUpdateRequests.poll();
                    
                    // check if database for this request is in indexing list
                    // if not, this is an (old) readded request
                    if (!_indexedDbs.containsKey(request.getDbkey())) {
                        continue;
                    }                
                    
                    db = (WGDatabase) _core.getContentdbs().get(request.getDbkey());
                    
                    if ( (db == null) || (!db.isReady()) ) {
                        LOG.info("Indexer cannot access database '" + request.getDbkey() + "'. Indexing request will be tried again later.");
                        // put request back in list and continue with next request                        
                        addDBUpdateRequest(request);
                        continue;
                    }
                    
                    int access = WGDatabase.ACCESSLEVEL_NOTLOGGEDIN;
                    if (!db.isSessionOpen()) {
                        access = db.openSession();
                    } else {
                        access = db.getSessionContext().getAccessLevel();
                    }
                    if ( access == WGDatabase.ACCESSLEVEL_NOTLOGGEDIN ) {                    
                        LOG.warn("Cannot update lucene index for database '" + request.getDbkey() + "' because it cannot be opened by master login. Indexing request will be tried again later.");
                        // put request back in list and continue with next request                        
                        addDBUpdateRequest(request);                    
                        continue;
                    }

                    db.getSessionContext().setTask("WGA Lucene Indexer");
                    db.getSessionContext().setBatchProcess(true);
                    updateDb(request, db);
                }
                catch (Exception e) {
                    LOG.info("Indexer cannot access database '" + request.getDbkey() + "'. Indexing request will be tried again later." , e);
                    // put request back in list and continue with next request                        
                    addDBUpdateRequest(request);
                    continue;
                }
            }
            return true;
        }

        /**
         * @return
         */
        private IndexingRequest fetchNextFromList(Queue<IndexingRequest> list) {
            synchronized (LuceneManager.this) {
                if (list.isEmpty()) {
                    return null;
                }
                else {
                    return list.poll();
                }

            }
        }

        /**
         * @return
         */
        private boolean listIsEmpty(Queue list) {
            synchronized (LuceneManager.this) {
                if (list.isEmpty()) {
                    return true;
                }
                else {
                    return false;
                }
            }
        }        
        
        /**
         * @param dbkey
         * @param documentKey
         * @throws IOException
         */
        private void deleteContent(IndexReader reader, String dbkey, String documentKey) throws IOException {
            String uniqueIndexKey = buildUniqueIndexKey(dbkey, documentKey);
            reader.deleteDocuments(new Term(INDEXFIELD_UNIQUEKEY, uniqueIndexKey));
            reader.deleteDocuments(new Term(INDEXFIELD_PARENTKEY, uniqueIndexKey));
            LOG.debug("Index of " + documentKey + " deleted from db " + dbkey);            
        }

        private void dropDatabaseFromIndex(IndexReader reader, String dbkey) throws IOException {            
            reader.deleteDocuments(new Term(INDEXFIELD_DBKEY, dbkey));
            // remove from configfile
            _indexConfig.removeDBConfig(dbkey);
            LOG.info("Content of db '" + dbkey + "' dropped from index.");
        }                
        
        private void truncateDatabaseFromIndex(IndexReader reader, String dbkey) throws IOException {            
            reader.deleteDocuments(new Term(INDEXFIELD_DBKEY, dbkey));
            LOG.info("Content of db '" + dbkey + "' truncated from index.");
        }        
                    
        /**
         * @param dbkey
         * @param content
         * @throws IOException
         * @throws WGAPIException 
         */
        private void addToIndex(IndexWriter writer, WGDatabase db, WGContent content) throws IOException, WGAPIException {
            
            if (content == null) {
                LOG.error("cannot add 'null' content to index");
                return;
            }
            
            de.innovationgate.wga.config.LuceneIndexConfiguration luceneIndexConfig = null;
            ContentStore dbConfig = _core.getWgaConfiguration().getContentStore(db.getDbReference());
            if (dbConfig != null) {
                luceneIndexConfig = dbConfig.getLuceneIndexConfiguration();
            }
            
            String dbkey = db.getDbReference();
            LOG.debug("Indexing " + content.getDocumentKey() + " from db " + dbkey);
            
            Document document = new org.apache.lucene.document.Document();
            addKeyword(document, INDEXFIELD_UNIQUEKEY, buildUniqueIndexKey(content));
            addMetas(document, content, true);   
            addKeyword(document, INDEXFIELD_DOCTYPE, DOCTYPE_CONTENT);

            // index content items
            Iterator itemNames = content.getItemNames().iterator();
            while (itemNames.hasNext()) {
                // retrieve configured indexing rule
                String itemName = (String) itemNames.next();                
                LuceneIndexItemRule rule = retrieveItemRule(db, itemName);
                
                if (rule == null) {
                    throw new WGConfigurationException("No lucene configuration available for database " + db.getDbReference() +". The database may have been removed from lucene index.");
                }
                
                if (rule.getIndexType().equals(LuceneIndexItemRule.INDEX_TYPE_FULLTEXT)) {
                    addToFulltextIndex(content, document, itemName, rule);
                }
                if (rule.getIndexType().equals(LuceneIndexItemRule.INDEX_TYPE_KEYWORD)) {
                    addToKeywordIndex(content, document, itemName, rule);
                }
                
                // add item as sort field if configured
                if (rule.isSortable()) {
                    addForSorting(content, document, itemName, rule);
                }
            }
            
            // index content relations
            Iterator<String> relationNames = content.getRelationNames().iterator();
            while (relationNames.hasNext()) {
            	String relationName = relationNames.next();
            	WGRelationData data = content.getRelationData(relationName);
            	if (data != null && data.getGroup() == null) {
            		String luceneItemName = RELATION_PREFIX + relationName.toLowerCase();
            		addKeyword(document, luceneItemName, data.getTargetStructkey().toString() + "." + data.getTargetLanguage());
            		addSortField(document, luceneItemName, data.getTargetStructkey().toString() + "." + data.getTargetLanguage());  
            	}            	
            }
            
            // Index content relation groups
            for (String relGroup : content.getRelationGroups()) {
                for (WGRelationData data : content.getRelationsDataOfGroup(relGroup)) {
                    if (data != null) {
                        String luceneItemName = RELATIONGROUP_PREFIX + relGroup.toLowerCase();
                        addKeyword(document, luceneItemName, data.getTargetStructkey().toString() + "." + data.getTargetLanguage());
                        addSortField(document, luceneItemName, data.getTargetStructkey().toString() + "." + data.getTargetLanguage());
                    }
                }
            }            
            
            // get analyzer for document language
            Analyzer analyzer = retrieveAnalyzer(content);
            
            // index file attachments
            boolean useFileRuleBasedIndexing = true;
            if (db.getContentStoreVersion() > WGDatabase.CSVERSION_WGA5 || (db.getContentStoreVersion() == WGDatabase.CSVERSION_WGA5 && db.getContentStorePatchLevel() >= 5)) {
                useFileRuleBasedIndexing = false;
            }
            
            
            List<String> filenames = content.getFileNames();
            if (filenames != null) {
                Iterator<String> it = filenames.iterator();
                while (it.hasNext()) {
                    String filename = it.next();
                    
                    if (filename.startsWith(".")) {
                    	// this indicates normally a hidden file so skip it
                    	continue;
                    }
                    
                    LuceneIndexFileRule rule = null;
                    if (useFileRuleBasedIndexing) {
                        // use old file rule behaviour for CS version < v5 (patch level 5)
                        rule = retrieveFileRule(db, filename);
                        if (rule != null) {
                            if (rule.getFileSizeLimit() == LuceneIndexFileRule.FILESIZELIMIT_INDEX_NONE) {
                                // skip file
                                continue;
                            }
                            if (rule.getFileSizeLimit() != LuceneIndexFileRule.FILESIZELIMIT_INDEX_ALL) {
                                int filesize = content.getFileSize(filename);
                                // check filesize                    
                                if (filesize > (rule.getFileSizeLimit() * 1024) || filesize == -1) {
                                    LOG.debug("Skipping file " + filename + " of content " + content.getContentKey().toString() + " - filesizelimit " + rule.getFileSizeLimit() + " kb exceeded.");
                                    // file not found or filesize exceeded limit - skip file
                                    continue;
                                }
                            }
                        }
                    }                                        
                    
                    // create attachment doc in index for each file
                    Document attachmentDoc = new Document();
                    addKeyword(attachmentDoc, INDEXFIELD_UNIQUEKEY, buildUniqueIndexKey(content, filename));
                    addKeyword(attachmentDoc, INDEXFIELD_PARENTKEY, buildUniqueIndexKey(content));
                    
                    // add content metas to attachmentDoc - so this doc will hit the same meta queries
                    addMetas(attachmentDoc, content, false);
                    
                    // add file metas
                    WGFileMetaData md = content.getFileMetaData(filename);
                    if (md != null) {
                        Iterator<MetaInfo> metaInfos = WGFactory.getInstance().getMetaInfos(WGFileMetaData.class).values().iterator();
                        while (metaInfos.hasNext()) {
                            MetaInfo metaInfo = metaInfos.next();
                            if (!metaInfo.isLuceneSpecialTreatment() && !metaInfo.getLuceneIndexType().equals(MetaInfo.LUCENE_INDEXTYPE_NOINDEX)) {
                                addMeta(attachmentDoc, metaInfo, md.getMetaData(metaInfo.getName()), true);
                            }
                        }
                    }
                    
                    // add items of type keyword to attachmentDoc
                    itemNames = content.getItemNames().iterator();
                    while (itemNames.hasNext()) {
                        // retrieve configured indexing rule
                        String itemName = (String) itemNames.next();                
                        LuceneIndexItemRule itemRule = retrieveItemRule(db, itemName);

                        if (itemRule.getIndexType().equals(LuceneIndexItemRule.INDEX_TYPE_KEYWORD)) {
                            addToKeywordIndex(content, attachmentDoc, itemName, itemRule);
                        }                                    
                        // add item as sort field if configured
                        if (itemRule.isSortable()) {
                            addForSorting(content, attachmentDoc, itemName, itemRule);
                        }
                    }
                    
                    addKeyword(attachmentDoc, INDEXFIELD_DOCTYPE, DOCTYPE_ATTACHMENT);
                    addKeyword(attachmentDoc, INDEXFIELD_FILENAME, filename);
                    
                    if (useFileRuleBasedIndexing) {
                        // try to retrieve filehandler
                        FileHandler handler = retrieveFileHandler(filename);
                        if (handler != null) {
                            // parse text from file
                            InputStream is = content.getFileData(filename);
                            if (is != null) {
                                String text = null;
                                try {
                                    handler.parse(is);
                                    text = handler.getText();
                                } catch (FileHandlerException e) {
                                    LOG.warn("Unable to extract text from file '" + filename + "' of content '" + content.getContentKey().toString() + "' using filehandler '" + handler.getClass().getName() + "'.", e);
                                }
                                if (text != null) {
                                    // index file text
                                    LOG.debug("Indexing file " +filename + " of content " + content.getContentKey().toString() + " from db " + db.getDbReference() + ".");
                                    addUnStored(document, INDEXFIELD_ALLATTACHMENTS, text, rule.getBoost());
                                    
                                    if (rule.isIncludedInAllContent()) {
                                        addUnStored(document, INDEXFIELD_ALLCONTENT, text, rule.getBoost());
                                    }
    
                                    addUnStored(attachmentDoc, INDEXFIELD_ALLCONTENT, text, rule.getBoost());
                                }                                                                                                                       
                            }
                        } else {
                            LOG.debug("No filehandler found for file " + filename + " of content " + content.getContentKey().toString() + " from db " + db.getDbReference() + ".");
                        }
                    } else {
                        WGFileMetaData fileMetaData = content.getFileMetaData(filename);
                        BinaryFieldData plaintext = fileMetaData.getPlainText();
                        if (plaintext != null) {
                            Field allAttachments = new Field(INDEXFIELD_ALLATTACHMENTS, new InputStreamReader(plaintext.getInputStream()));
                            document.add(allAttachments);
                            
                            Field allContent = new Field(INDEXFIELD_ALLCONTENT, new InputStreamReader(plaintext.getInputStream()));
                            attachmentDoc.add(allContent);
                            
                            if (luceneIndexConfig != null && luceneIndexConfig.isIndexFileContentOnDocuments()) {
                               allContent = new Field(INDEXFIELD_ALLCONTENT, new InputStreamReader(plaintext.getInputStream()));
                               document.add(allContent);
                            }                            
                        }                        
                    }
                    
                    
                    // call enhancer for attachmentDoc
                    List<LuceneIndexEnhancer> enhancers = _indexEnhancers.get(db.getDbReference());
                    if (enhancers != null) {
                        for (LuceneIndexEnhancer enhancer : enhancers) {
                            if (enhancer instanceof AttachmentAwareLuceneIndexEnhancer) {
                                try {
                                    ((AttachmentAwareLuceneIndexEnhancer)enhancer).enhance(attachmentDoc, content, filename);
                                } catch (Throwable e) {
                                    LOG.error("Enhancer call failed for '" + content.getDocumentKey() + "/" + filename + "'", e);
                                }
                            }
                        }
                    }
                    
                    writer.addDocument(attachmentDoc, analyzer);
                }
            }
            
            List<LuceneIndexEnhancer> enhancers = _indexEnhancers.get(db.getDbReference());
            if (enhancers != null) {
                for (LuceneIndexEnhancer enhancer : enhancers) {
                    try {
                        enhancer.enhance(document, content);
                    } catch (Throwable e) {
                        LOG.error("Enhancer call failed for '" + content.getDocumentKey() + "'", e);
                    }
                }
            }
            
            //analyze document with configured analyzer and add to index
            writer.addDocument(document, analyzer);
            LOG.debug("Index of " + content.getDocumentKey() + " from db " + dbkey + " added using analyzer '" + analyzer.getClass().getName());
        }

        private void addMetas(org.apache.lucene.document.Document document, WGContent content, boolean addToAllContent) throws WGIllegalArgumentException, WGAPIException,
                WGSystemException {

            String dbkey = content.getDatabase().getDbReference();

            // index keywords internal usage            
            addKeyword(document, INDEXFIELD_DBKEY, dbkey);
            addKeyword(document, INDEXFIELD_DOCUMENTKEY, content.getDocumentKey());
            addKeyword(document, INDEXFIELD_CONTENTKEY, content.getContentKey().toString());
            
            addKeyword(document, INDEXFIELD_ISVIRTUALCONTENT, content.isVirtual());
             
            // meta keywords            
            // synonyms for internal fields --> encapsulate internal fields from metas
            addKeyword(document, VIRTUALMETA_DBKEY, dbkey);
            addSortField(document, VIRTUALMETA_DBKEY, dbkey);
            addKeyword(document, VIRTUALMETA_KEY, content.getContentKey().toString());
            addSortField(document, VIRTUALMETA_KEY, content.getContentKey().toString());

            if (content.hasCompleteRelationships()) {
                addKeyword(document, VIRTUALMETA_AREA, content.getStructEntry().getArea().getName());
                addSortField(document, VIRTUALMETA_AREA, content.getStructEntry().getArea().getName());
                addKeyword(document, VIRTUALMETA_CONTENTTYPE, content.getStructEntry().getContentType().getName());
                addSortField(document, VIRTUALMETA_CONTENTTYPE, content.getStructEntry().getContentType().getName());
                addKeyword(document, VIRTUALMETA_PAGEPUBLISHED, content.getStructEntry().getPublished().get(content.getLanguage().getName()));
                addSortField(document, VIRTUALMETA_PAGEPUBLISHED, content.getStructEntry().getPublished().get(content.getLanguage().getName()));
            }
            
            if (!content.getStructEntry().isRoot()) {
                WGStructEntry ancestor = content.getStructEntry().getParentEntry();
                addKeyword(document, VIRTUALMETA_PARENT, ancestor.getStructKey().toString());
                do {
                    addKeyword(document, VIRTUALMETA_PATH, ancestor.getStructKey().toString());
                    ancestor = ancestor.getParentEntry();
                } while (ancestor != null);                
                
            }
            
            Iterator contentMetaNames = content.getMetaNames().iterator();
            while (contentMetaNames.hasNext()) {
                String metaName = (String) contentMetaNames.next();
                MetaInfo metaInfo = (MetaInfo) content.getMetaInfo(metaName);
                if (metaInfo == null) {
                    throw new WGSystemException("MetaInfo for meta '" + metaName + "' not found.");
                }
                if (!metaInfo.isLuceneSpecialTreatment() && !metaInfo.getLuceneIndexType().equals(MetaInfo.LUCENE_INDEXTYPE_NOINDEX)) {
                    addMeta(document, metaInfo, content.getMetaData(metaName), addToAllContent);
                } else {
                    if (metaInfo.getName().equals(WGContent.META_IS_HIDDEN_FROM)) {
                        // split hidden from
                        List isHiddenFrom = content.isHiddenFrom();
                        addKeyword(document, "HIDDENINNAV", Boolean.valueOf(isHiddenFrom.contains(WGContent.DISPLAYTYPE_NAVIGATOR)), metaInfo.getLuceneBoost());
                        addSortField(document, "HIDDENINNAV", Boolean.valueOf(isHiddenFrom.contains(WGContent.DISPLAYTYPE_NAVIGATOR)));
                        addKeyword(document, "HIDDENINSEARCH", Boolean.valueOf(isHiddenFrom.contains(WGContent.DISPLAYTYPE_SEARCH)), metaInfo.getLuceneBoost());
                        addSortField(document, "HIDDENINSEARCH", Boolean.valueOf(isHiddenFrom.contains(WGContent.DISPLAYTYPE_SEARCH)));
                        addKeyword(document, "HIDDENINSITEMAP", Boolean.valueOf(isHiddenFrom.contains(WGContent.DISPLAYTYPE_SITEMAP)), metaInfo.getLuceneBoost());
                        addSortField(document, "HIDDENINSITEMAP", Boolean.valueOf(isHiddenFrom.contains(WGContent.DISPLAYTYPE_SITEMAP)));
                    } else if (metaInfo.getName().equals(WGContent.META_VALID_FROM)) {
                        // bugfix B00003432 by tb 10.03.2006
                        if (content.getValidFrom() != null) {
                            addKeyword(document, WGContent.META_VALID_FROM, content.getValidFrom(), metaInfo.getLuceneBoost());
                            addSortField(document, WGContent.META_VALID_FROM, content.getValidFrom());
                        } else {
                            // index not set validFrom as 00000 ...
                            addKeyword(document, WGContent.META_VALID_FROM, EMPTY_VALID_FROM, metaInfo.getLuceneBoost());
                            addSortField(document, WGContent.META_VALID_FROM, EMPTY_VALID_FROM);
                        }
                    } else if (metaInfo.getName().equals(WGContent.META_VALID_TO)) {
                        // bugfix B00003432 by tb 10.03.2006
                        if (content.getValidTo() != null) {                
                            addKeyword(document, WGContent.META_VALID_TO, content.getValidTo(), metaInfo.getLuceneBoost());
                            addSortField(document, WGContent.META_VALID_TO, content.getValidTo());
                        } else {
                            // index not set validTo as 99999...
                            addKeyword(document, WGContent.META_VALID_TO, EMPTY_VALID_TO, metaInfo.getLuceneBoost());
                            addSortField(document, WGContent.META_VALID_TO, EMPTY_VALID_TO);
                        }
                    }
                }
            }
        }        
             

        private void addKeyword(org.apache.lucene.document.Document doc, String name, Object value) throws WGIllegalArgumentException {
            addKeyword(doc, name, value, 1);
        }
        
        private void addKeyword(org.apache.lucene.document.Document doc, String name, Object value, float boost) throws WGIllegalArgumentException {
            
            if ( (name != null) && (value != null) ) {
                Field field;
                if (value instanceof String) {
                    field = new Field(name, (String) value, Field.Store.YES, Field.Index.NOT_ANALYZED);
                } else if (value instanceof Date) {
                    field = new Field(name, DATEFORMAT_KEYWORD.format(value), Field.Store.YES, Field.Index.NOT_ANALYZED);
                } else if (value instanceof Number) {
                	// B000047B2
                    field = new Field(name, NUMBERFORMAT_KEYWORD.format(value), Field.Store.YES, Field.Index.NOT_ANALYZED);                    
                } else if (value instanceof Boolean) {
                    field = new Field(name, String.valueOf(value), Field.Store.YES, Field.Index.NOT_ANALYZED);
                } else {
                    throw new WGIllegalArgumentException("Unsupported value '" + value.getClass().getName() + "' for keyword '" + name + "'.");
                } 
                field.setBoost(boost);
                doc.add(field);
            }
        }
        
        private void addSortField(org.apache.lucene.document.Document doc, String name, Object value) throws WGIllegalArgumentException {
            if (name != null) {
                Field field;
                String fieldName = SORTITEM_PREFIX + name;
                if (value instanceof String) {
                    if (value != null) {
                        field = new Field(fieldName, (String)value, Field.Store.YES, Field.Index.NOT_ANALYZED);
                    } else {
                        field = new Field(fieldName, "", Field.Store.YES, Field.Index.NOT_ANALYZED);
                    }
                } else if (value instanceof Date) {
                    if (value != null) {
                        field = new Field(fieldName, DATEFORMAT_SORTFIELD.format(value), Field.Store.YES, Field.Index.NOT_ANALYZED);
                    } else {
                        field = new Field(fieldName, DATEFORMAT_SORTFIELD.format(new Date(0)), Field.Store.YES, Field.Index.NOT_ANALYZED);
                    }
                } else if (value instanceof Number) {
                    if (value != null) {
                       field = new Field(fieldName, DECIMALFORMAT_SORTFIELD.format(value), Field.Store.YES, Field.Index.NOT_ANALYZED); 
                    } else {
                       field = new Field(fieldName, DECIMALFORMAT_SORTFIELD.format(Long.MIN_VALUE), Field.Store.YES, Field.Index.NOT_ANALYZED);                 
                    }
                } else if (value instanceof Boolean) {
                    if (value != null) { 
                        field = new Field(fieldName, String.valueOf(value), Field.Store.YES, Field.Index.NOT_ANALYZED);
                    } else {
                        field = new Field(fieldName, String.valueOf(false), Field.Store.YES, Field.Index.NOT_ANALYZED);
                    }
                } else {
                    if (value != null) {
                        throw new WGIllegalArgumentException("Unsupported value '" + value.getClass().getName() + "' for sortfield '" + fieldName + "'.");
                    } else {
                        // sort field must exist
                        field = new Field(fieldName, "", Field.Store.YES, Field.Index.NOT_ANALYZED);
                    }
                } 
                doc.add(field);
            }
        }                
        
        private void addMeta(org.apache.lucene.document.Document doc, MetaInfo metaInfo, Object value, boolean addToAllContent) throws WGIllegalArgumentException { 
            if (metaInfo.getLuceneIndexType().equals(MetaInfo.LUCENE_INDEXTYPE_KEYWORD)) {
                               
                if (!metaInfo.isMultiple()) {                          
                        addKeyword(doc, metaInfo.getName(), value, metaInfo.getLuceneBoost());
                        addSortField(doc, metaInfo.getName(), value);
                        Iterator synonyms = metaInfo.getSynonyms().iterator();
                        while (synonyms.hasNext()) {
                            String synonym = (String) synonyms.next();
                            addKeyword(doc, synonym, value, metaInfo.getLuceneBoost());
                            addSortField(doc, synonym, value);
                        }
                        if (metaInfo.getLuceneAddToAllContent() && addToAllContent) {
                            addUnStored(doc, INDEXFIELD_ALLCONTENT, value, metaInfo.getLuceneBoost());
                        }
                } else {
                    if (value == null) {
                        addKeyword(doc, metaInfo.getName(), "");
                    } else {                                                        
                        Iterator it = ((List)value).iterator();
                        while (it.hasNext()) {
                            Object singleValue = it.next();
                            addKeyword(doc, metaInfo.getName(), singleValue);
                            if (metaInfo.getLuceneAddToAllContent() && addToAllContent) {
                                addUnStored(doc, INDEXFIELD_ALLCONTENT, value, metaInfo.getLuceneBoost());
                            }
                            Iterator synonyms = metaInfo.getSynonyms().iterator();
                            while (synonyms.hasNext()) {
                                String synonym = (String) synonyms.next();
                                addKeyword(doc, synonym, singleValue);
                            }
                        }                                
                    }
                }
            } else if (metaInfo.getLuceneIndexType().equals(MetaInfo.LUCENE_INDEXTYPE_FULLTEXT)) {
                if (!metaInfo.isMultiple()) {
                    addUnStored(doc, metaInfo.getName(), value, metaInfo.getLuceneBoost());                    
                    addSortField(doc, metaInfo.getName(), value);
                    Iterator synonyms = metaInfo.getSynonyms().iterator();
                    while (synonyms.hasNext()) {
                        String synonym = (String) synonyms.next();
                        addUnStored(doc, synonym, value, metaInfo.getLuceneBoost());
                        addSortField(doc, synonym, value);
                    }
                    if (metaInfo.getLuceneAddToAllContent() && addToAllContent) {
                        addUnStored(doc, INDEXFIELD_ALLCONTENT, value, metaInfo.getLuceneBoost());
                    }
                } else {
                    if (value != null) {
                        Iterator it = ((List)value).iterator();
                        while (it.hasNext()) {
                            Object singleValue = it.next();
                            addUnStored(doc, metaInfo.getName(), singleValue, metaInfo.getLuceneBoost());
                            if (metaInfo.getLuceneAddToAllContent() && addToAllContent) {
                                addUnStored(doc, INDEXFIELD_ALLCONTENT, singleValue, metaInfo.getLuceneBoost());    
                            }
                            Iterator synonyms = metaInfo.getSynonyms().iterator();
                            while (synonyms.hasNext()) {
                                String synonym = (String) synonyms.next();
                                addUnStored(doc, synonym, singleValue, metaInfo.getLuceneBoost());
                            }
                        }                                
                    }
                }
            }
        }       
       
        
        private void addUnStored(org.apache.lucene.document.Document doc, String name, Object value) throws WGIllegalArgumentException {
            addUnStored(doc, name, value, 1.0F);
        }
      
        private void addUnStored(org.apache.lucene.document.Document doc, String name, Object value, float boost) throws WGIllegalArgumentException {
            if ( (name != null) && (value != null) ) {
                Field field;
                if (value instanceof String) {
                    field = new Field(name, (String)value, Field.Store.NO, Field.Index.ANALYZED); 
                } else {
                    throw new WGIllegalArgumentException("Unsupported value '" + value.getClass().getName() + "' for unstored field '" + name + "'.");
                } 
                field.setBoost(boost);
                doc.add(field);
            }
        }
        

        
        /**
         * @param filename
         * @return configured filehandler for fileextension or null
         */
        private FileHandler retrieveFileHandler(String filename) {
            int pos = filename.lastIndexOf(".");
            if (pos != -1) {
                if (filename.length() > 1) {
                    String extension = filename.substring(pos + 1);
                    return _core.getFileHandlerForExtension(extension);
                } else {
                    // only a '.'
                    LOG.warn("Could not optain a filehandler for indexing content of file '" + filename + "'. Unable to parse file extension - there is only a '.'.");
                    return null;
                }
            } else {
                LOG.warn("Could not optain a filehandler for indexing content of file '" + filename + "'. Unable to parse file extension - there is no '.'.");
                return null;
            }
            
        }        

        private void addToFulltextIndex(WGContent content, org.apache.lucene.document.Document document, String itemName, LuceneIndexItemRule rule) throws WGAPIException {
            LOG.debug("Adding item " + itemName + " to fulltextIndex.");
            Iterator itemElements = content.getItemValueList(itemName).iterator();
            while (itemElements.hasNext()) {
                Object itemElement = itemElements.next();
                if (itemElement instanceof String) {
                    String textToIndex = (String) itemElement;
                    textToIndex = rule.parseItemValue(textToIndex);
                    if (textToIndex != null) {                    
                        addUnStored(document, itemName.toLowerCase(), textToIndex, rule.getBoost());
                        if (!TECHNICAL_ITEMS.contains(itemName.toLowerCase())) {
                            addUnStored(document, INDEXFIELD_ALLCONTENT, textToIndex, rule.getBoost());
                        }
                    }
                }
            }          
        }

        private void addToKeywordIndex(WGContent content, org.apache.lucene.document.Document document, String itemName, LuceneIndexItemRule rule) throws WGAPIException {
            LOG.debug("Adding item " + itemName + " to keywordIndex.");
            Iterator itemElements = content.getItemValueList(itemName).iterator();
            while (itemElements.hasNext()) {
                Object itemElement = itemElements.next();
                if (itemElement instanceof String) {
                    String textToIndex = (String) itemElement;
                                        
                    if (textToIndex != null) {
                        // if content type html/xml remove tags
                        if (rule.getContentType().equals(LuceneIndexItemRule.CONTENT_TYPE_HTML_XML)) {
                            try {
                                textToIndex = WGUtils.toPlainText(textToIndex, "", false);
                            }
                            catch (IOException e) {
                                // cannot parse, so use original text
                            }
                        }
                        
                        addKeyword(document, itemName.toLowerCase(), textToIndex, rule.getBoost());
                    }                    
                                        
                }
                else if (itemElement instanceof Date) {
                    addKeyword(document, itemName.toLowerCase(), (Date) itemElement, rule.getBoost());
                }
                else if (itemElement instanceof Number) {
                    addKeyword(document, itemName.toLowerCase(), (Number) itemElement, rule.getBoost());
                }
                else if (itemElement != null) {
                    addKeyword(document, itemName.toLowerCase(), String.valueOf(itemElement), rule.getBoost());
                }
            }         
        }        
        

        private void addForSorting(WGContent content, org.apache.lucene.document.Document document, String itemName, LuceneIndexItemRule rule) throws WGAPIException {            
            LOG.debug("Adding item " + itemName + " for sorting purposes.");
            Iterator itemElements = content.getItemValueList(itemName).iterator();
            // not necessary since bugfix #00000249
            /*
            if (!itemElements.hasNext()) {
                // Sortingfields must exist in index
                addSortField(document, itemName.toLowerCase(), (String) null);
            }*/
            while (itemElements.hasNext()) {
                Object itemElement = itemElements.next();
                if (itemElement instanceof String) {
                    addSortField(document, itemName.toLowerCase(), (String) itemElement);                    
                }
                else if (itemElement instanceof Date) {
                    addSortField(document, itemName.toLowerCase(), (Date) itemElement);
                }
                else if (itemElement instanceof Number) {
                    addSortField(document, itemName.toLowerCase(), (Number) itemElement);
                }
                else if (itemElement != null) {
                    addSortField(document, itemName.toLowerCase(), String.valueOf(itemElement)); 
                }
            }         
        } 
        
        /**
         * @param db
         * @throws IOException
         * @throws ParseException
         * @throws WGAPIException 
         */
        private void updateDb(IndexingRequest request, WGDatabase db) throws IOException, ParseException, WGAPIException {
           
            if (request.isIndexRebuildNeeded()) {
                // rebuild the complete index for this db
                // --> lucene config has changed
                LOG.info("Rebuilding index of db " + request.getDbkey());
                initialUpdateDb(request.getDbkey(), db);                
            } else {    
                // Fetch the revision of last index update from config doc
                WGDatabaseRevision lastUpdated = _indexConfig.getLastUpdated(request.getDbkey());
    
                // New indexing of all documents
                if (lastUpdated == null) {
                    LOG.info("Initial indexing of db " + request.getDbkey());
                    initialUpdateDb(request.getDbkey(), db);
                }
                else {
                    // Test if the stored index revision is comparable to the current revision given by the database
                    // If it is not we must rebuild the whole index again
                    boolean comparable = false;
                    try {
                        db.getRevisionDate(lastUpdated);
                        comparable = true;
                    }
                    catch (WGWrongRevisionException e) {
                    }

                    if (comparable) {
                        // Indexing of updated and deleted documents
                        incrementalUpdateDb(db, lastUpdated);
                    }
                    else {
                        // Rebuild index because the revision type has changed and is no more comparable to the stored revision
                        LOG.info("Initial indexing of db " + request.getDbkey()  + " because the type of database revision has changed");
                        initialUpdateDb(request.getDbkey(), db);
                    }
                }
            }
        }

        /**
         * @param dbkey
         * @param db
         * @param lastUpdated
         * @throws WGAPIException 
         */
        private void incrementalUpdateDb(WGDatabase db, WGDatabaseRevision lastUpdated) throws WGAPIException {
        	
            WGDatabaseRevision dbLastChanged = db.getRevisionObject();            
            
            
            if (lastUpdated.compareTo(dbLastChanged) > 0) {
                // nothing to do for incremental update          
                return;
            }
            
            LOG.info("Incremental index update of db " + db.getDbReference());            
                        
            List logs = db.getUpdatedDocumentsSince(lastUpdated);
            /*
            if ( (logs == null) || (logs.size() == 0) ) {
                // try to reset serviceStatus to idle
                updateServiceStatus(dbkey);                
                return;
            } */           
                
            Iterator logsIt = logs.iterator();
            String contentPrefix = WGDocument.TYPENAME_CONTENT + "/";
            while (logsIt.hasNext()) {
                Thread.yield();
                WGUpdateLog log = (WGUpdateLog) logsIt.next();
                
                // old notes-design does not support logs
                if ( (log == null) || (log.getDocumentKey() == null) ) {
                    LOG.warn("Cannot process incremental index update for 'null' log or 'null' log.getDocumentKey() - ensure you use WGAContentStoreDesign >= 3.1.2");
                    continue;
                }

                if (!log.getDocumentKey().startsWith(contentPrefix)) {
                    continue;
                }
                if (log.getType() == WGUpdateLog.TYPE_DELETE) {
                    addDeletionRequest(new IndexingRequest(db.getDbReference(), log.getDocumentKey()));
                }
                else if (log.getType() == WGUpdateLog.TYPE_UPDATE) {
                    addDeletionRequest(new IndexingRequest(db.getDbReference(), log.getDocumentKey()));
                    addAdditionRequest(new IndexingRequest(db.getDbReference(), log.getDocumentKey()));
                }
            }
        }

        /**
         * @param dbkey
         * @param db
         * @throws WGAPIException 
         */
        private void initialUpdateDb(String dbkey, WGDatabase db) throws WGAPIException {
            //delete all content for db
            addTruncateDBRequest(new IndexingRequest(dbkey, null));
            
            // add dbkey to initalIndexDBs set
            //_initialIndexDBs.add(dbkey);
            
            //index all content
            long time = System.currentTimeMillis();
            LOG.info("Fetching all contentkeys from db '" + dbkey + "'.");
            WGContentIterator docsIt = (WGContentIterator)db.getAllContent(true);
            if (docsIt != null) {
                Iterator contentKeys = docsIt.getContentKeys().iterator();
                while (contentKeys.hasNext()) {
                    Thread.yield();
                    WGContentKey key = (WGContentKey) contentKeys.next();                    
                    addAdditionRequest(new IndexingRequest(dbkey, WGDocument.TYPENAME_CONTENT + "/" + key.toString()));
                }                
            }
            else {
                LOG.error("Fetching all contenkeys from db '" + dbkey + "' returned 'null'. Database will not be indexed.");
            }
            LOG.info("Fetching all contentkeys from db '" + dbkey + "' done in " + (System.currentTimeMillis() - time) + "ms.");
        }

        public long getLastFullIndexingRun() {
            return _lastFullIndexingRun;
        }
    }

    class IndexingRequest {

        private String _dbkey;

        private String _documentKey;
        
        private boolean _indexRebuildNeeded;
        
        private int _retryCount = 0;
        
        private static final int _retryTimeout = 10;

        public IndexingRequest(String dbkey, String documentKey) {
            _dbkey = dbkey;
            _documentKey = documentKey;
            _indexRebuildNeeded = false;
        }

        public IndexingRequest(String dbkey, String documentKey, boolean indexRebuildNeeded) {
            _dbkey = dbkey;
            _documentKey = documentKey;
            _indexRebuildNeeded = indexRebuildNeeded;
        }        
        
        /**
         * @return Returns the contentKey.
         */
        public String getDocumentKey() {
            return _documentKey;
        }

        /**
         * @return Returns the dbkey.
         */
        public String getDbkey() {
            return _dbkey;
        }
        
        /**
         * @return Returns if indexRebuild is needed. --> lucene config has changed
         */
        public boolean isIndexRebuildNeeded() {
            return _indexRebuildNeeded;
        }
        
        /* (non-Javadoc)
         * @see java.lang.Object#toString()
         */
        public String toString() {
            return getDbkey() + "/" + getDocumentKey();
        }
        
        public boolean equals(Object obj) {
            if (obj instanceof IndexingRequest) {
                IndexingRequest req = (IndexingRequest) obj;
                
                String checkDBKey1 = req.getDbkey();
                if (checkDBKey1 == null) {
                    checkDBKey1 = "";
                }
                String checkDBKey2 = this.getDbkey();
                if (checkDBKey2 == null) {
                    checkDBKey2 = "";
                }
                   
      			String checkDocumentKey1 = req.getDocumentKey();
                if (checkDocumentKey1 == null) {
                    checkDocumentKey1 = "";
                }                
                String checkDocumentKey2 = this.getDocumentKey();
                if (checkDocumentKey2 == null) {
                    checkDocumentKey2 = "";
                }
                
                
                if ( (checkDBKey1.equals(checkDBKey2)) &&
                     (checkDocumentKey1.equals(checkDocumentKey2)) ) {
                    return true;
                } else {
                    return false;
                }
            }
            return super.equals(obj);
        }

        public int getRetryCount() {
            return _retryCount;
        }

        public void raiseRetryCount() {
            _retryCount++;
        }
        
        public boolean isTimedOut() {
            return _retryCount >= IndexingRequest._retryTimeout;
        }
        
    }

    
    private Queue<IndexingRequest> _dropDBRequests = new ConcurrentLinkedQueue<IndexingRequest>();
    
    private Queue<IndexingRequest> _truncateDBRequests = new ConcurrentLinkedQueue<IndexingRequest>();    

    private Queue<IndexingRequest> _dbUpdateRequests = new ConcurrentLinkedQueue<LuceneManager.IndexingRequest>();

    
    private Map<String,Queue<IndexingRequest>> _additionRequestsMap = new HashMap<String,Queue<IndexingRequest>>();
    
    private Map<String,Queue<IndexingRequest>> _deletionRequestsMap = new HashMap<String, Queue<IndexingRequest>>();
 
    private Similarity _customSimilarity = null;
 

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGContentEventListener#contentCreated(de.innovationgate.webgate.api.WGContentEvent)
     */
    public void contentCreated(WGContentEvent contentEvent) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGContentEventListener#contentSaved(de.innovationgate.webgate.api.WGContentEvent)
     */
    public boolean contentSaved(WGContentEvent contentEvent) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGContentEventListener#contentHasBeenDeleted(de.innovationgate.webgate.api.WGContentEvent)
     */
    public void contentHasBeenDeleted(WGContentEvent event) {

        if (event.getDatabase().getSessionContext().isTransactionActive()) {
            return;
        }
        String dbkey = event.getDatabase().getDbReference();
        addDeletionRequest(new IndexingRequest(dbkey, event.getDocumentKey()));

    }

    /*
     * (non-Javadoc)
     * 
     * @see de.innovationgate.webgate.api.WGContentEventListener#contentHasBeenSaved(de.innovationgate.webgate.api.WGContentEvent)
     */
    public void contentHasBeenSaved(WGContentEvent event) throws WGAPIException {
        if (event.getDatabase().getSessionContext().isTransactionActive()) {
            return;
        }
        
        String dbkey = (String) event.getDatabase().getDbReference();
        IndexingRequest indexingRequest = new IndexingRequest(dbkey, event.getDocumentKey());
        addDeletionRequest(indexingRequest);
       	addAdditionRequest(indexingRequest);
    }

    protected void finalize() throws Throwable {

        if (_timer != null) {
            _timer.cancel();
            _timer = null;
        }        
    }

    public WGResultSet search(WGDatabase db, String phrase, Map parameters, WGA wga) throws WGQueryException {
        
        if (wga == null) {
            wga = WGA.get(_core);
        }
        
        // set max clause count for boolean queries
        BooleanQuery.setMaxClauseCount(_booleanQueryMaxClauseCount);
        
        if (this.isRebuildingIndex()) {            
            throw new WGQueryException(phrase, "Lucene search temporary disabled. Rebuilding lucene index ...");
        }
        
        // Registering problem in that case but not cancelling the query, as this is old, expected behaviour. The query will just return no results.
        if (!_core.getLuceneManager().indexIsEnabled(db.getDbReference())) {
            _core.getProblemRegistry().addProblem(Problem.create(new TMLContext.WebTMLOccasion(), new DatabaseScope(db.getDbReference()), "webtmlProblem.luceneIndexExpected", ProblemSeverity.LOW));
        }
        
        if (phrase == null || phrase.trim().equals("")) {
            return null;
        }
                
        try {
            BooleanQuery wholeQuery = new BooleanQuery();
            
            int max = WGACore.DEFAULT_QUERY_MAXRESULTS;
            Integer maxResults = (Integer) parameters.get(WGDatabase.QUERYOPTION_MAXRESULTS);
            if (maxResults != null) {
                if (maxResults == 0 || maxResults == -1) {
                    max = Integer.MAX_VALUE;
                }
            else {
                    max = maxResults;
                }
            }
            
            // handle dboption EXCLUDEDOCUMENT
            WGContent excludeContent = (WGContent) parameters.get(WGDatabase.QUERYOPTION_EXCLUDEDOCUMENT);
            if (excludeContent != null) {
                String uniqueKey = buildUniqueIndexKey(excludeContent.getDatabase().getDbReference(), excludeContent.getDocumentKey());                
                wholeQuery.add(new TermQuery(new Term(INDEXFIELD_UNIQUEKEY, uniqueKey)), BooleanClause.Occur.MUST_NOT);
                wholeQuery.add(new TermQuery(new Term(INDEXFIELD_PARENTKEY, uniqueKey)), BooleanClause.Occur.MUST_NOT);
            }
            
            // list of dbs to search in
            String searchScope = (String) parameters.get(LuceneManager.QUERYOPTION_SEARCHSCOPE);
            List searchDBKeys = new ArrayList();            
            if (searchScope.equals(LuceneManager.SEARCHSCOPE_DB)) {
                searchDBKeys.add(db.getDbReference());
            }
            if (searchScope.equals(LuceneManager.SEARCHSCOPE_DOMAIN)) {
                Iterator<WGDatabase> dbs = _core.getDatabasesForDomain((String) db.getAttribute(WGACore.DBATTRIB_DOMAIN)).iterator();
                while (dbs.hasNext()) {
                    WGDatabase currentDB = dbs.next();
                    if (wga.openDatabase(currentDB)) {
                        searchDBKeys.add(currentDB.getDbReference());
                    }
                }                               
            }
            if (searchScope.equals(LuceneManager.SEARCHSCOPE_WGA)) {
                Iterator dbs = _core.getContentdbs().values().iterator();
                while (dbs.hasNext()) {
                    WGDatabase currentDB = (WGDatabase) dbs.next();
                    if (wga.openDatabase(currentDB)) {
                        searchDBKeys.add(currentDB.getDbReference());
                    }
                }                
            }
            if (searchScope.equals(LuceneManager.SEARCHSCOPE_DB_LIST)) {
                String dbListCSV = (String) parameters.get(QUERYOPTION_SEARCHDBKEYS);
                if (dbListCSV == null || dbListCSV.trim().equals("")) {
                    throw new WGQueryException(phrase, "Search scope is 'dblist' but no db keys given.");
                } else {
                    Iterator dbkeys = WGUtils.deserializeCollection(dbListCSV, ",").iterator(); 
                    while (dbkeys.hasNext()) {
                        String dbkey = (String) dbkeys.next();
                        WGDatabase currentDB = wga.db(dbkey);
                        if (currentDB.isSessionOpen()) {
                            searchDBKeys.add(dbkey.trim().toLowerCase());
                        }
                    }       
                }         
            }
            
            
            // Handle language selection;
            List<WGLanguage> languagesPriorityList = null;
            boolean filterLanguages = false;
            if (parameters.containsKey(WGDatabase.QUERYOPTION_LANGUAGES)) {
                List<WGLanguage> langs = (List<WGLanguage>) parameters.get(WGDatabase.QUERYOPTION_LANGUAGES);
                if (langs.size() > 1) {
                    BooleanQuery langQuery = new BooleanQuery();
                    for (WGLanguage lang : langs) {
                        langQuery.add(new TermQuery(new Term(WGContent.META_LANGUAGE, lang.getName())), BooleanClause.Occur.SHOULD);
                    }
                    wholeQuery.add(langQuery, BooleanClause.Occur.MUST);
                    languagesPriorityList = langs;
                    filterLanguages = true;
                }
                else if (langs.size() == 1){
                    wholeQuery.add(new TermQuery(new Term(WGContent.META_LANGUAGE, langs.get(0).getName())), BooleanClause.Occur.MUST);
                    languagesPriorityList = Collections.singletonList(langs.get(0));
                }
            }
            else if (parameters.containsKey(WGDatabase.QUERYOPTION_ONLYLANGUAGE)) {
                String language = (String)parameters.get(WGDatabase.QUERYOPTION_ONLYLANGUAGE);
                wholeQuery.add(new TermQuery(new Term(WGContent.META_LANGUAGE, language)), BooleanClause.Occur.MUST);
                languagesPriorityList = Collections.singletonList(db.getLanguage(language));
            }
            
            if (languagesPriorityList == null) {
                languagesPriorityList = getLanguagesForSearchDBKeys(searchDBKeys);;
            }
            
            // Handle visibility selection
            if (!parameters.containsKey(WGDatabase.QUERYOPTION_ENHANCE) || parameters.get(WGDatabase.QUERYOPTION_ENHANCE).equals(new Boolean(true))) {
 
                wholeQuery.add(new TermQuery(new Term(WGContent.META_VISIBLE, "true")), BooleanClause.Occur.MUST);
                
                String role = (String)parameters.get(WGDatabase.QUERYOPTION_ROLE);
                if (role != null) {
                	if (!role.equalsIgnoreCase(WGContent.DISPLAYTYPE_NONE)) {            
	            		wholeQuery.add(new TermQuery(new Term("HIDDENIN" + role.toUpperCase(), "false")), BooleanClause.Occur.MUST);
                	}
                }
            }
            
            if (parameters.containsKey(WGDatabase.QUERYOPTION_ONLYRELEASED)) {
                wholeQuery.add(new TermQuery(new Term(WGContent.META_STATUS, WGContent.STATUS_RELEASE)), BooleanClause.Occur.MUST);
            }

            
            // build dbQuery (OR combination of all searchDbs indexed by lucene)
            BooleanQuery dbQuery = new BooleanQuery();
            Iterator itSearchDBKeys = searchDBKeys.iterator();
            while (itSearchDBKeys.hasNext()) {
                String currentDBKey = (String) itSearchDBKeys.next();
                if (_indexedDbs.containsKey(currentDBKey)) {
                    dbQuery.add(new TermQuery(new Term(INDEXFIELD_DBKEY, currentDBKey)), BooleanClause.Occur.SHOULD);
                }
            }
            wholeQuery.add(dbQuery, BooleanClause.Occur.MUST);
            

            // Add parsed search phrase.
            // Search in allcontent for each language using the configured analyzer
            // if no analyzer is configured for a language search at least with one
            // default analyzer
            boolean searchWithDefaultAnalyzer = false;
            
            //if no languages found search at least with DefaultAnalyzer
            if (languagesPriorityList.size() <= 0) {
              searchWithDefaultAnalyzer = true;
            }
                        
            // parse native options
            Sort sort = null;
            String sortFieldName = "";
            Operator defaultOperator = QueryParser.AND_OPERATOR; 
            String nativeOptionsStr = (String) parameters.get(WGDatabase.QUERYOPTION_NATIVEOPTIONS);
            boolean includeVirtualContent = false;
            String doctype = DOCTYPE_CONTENT;
            if (nativeOptionsStr != null) {
                Iterator nativeOptions = WGUtils.deserializeCollection(nativeOptionsStr, ",", true).iterator();
                while (nativeOptions.hasNext()) {
                    String option = (String) nativeOptions.next();
                    if (option.startsWith("sort:")) {
                        sortFieldName = option.substring(5).trim();
                        boolean reverse = false;
                        if (sortFieldName.toLowerCase().endsWith("(asc)")) {
                            sortFieldName = sortFieldName.substring(0, sortFieldName.length() - 5).trim();
                        }
                        else if (sortFieldName.toLowerCase().endsWith("(desc)")) {
                            sortFieldName = sortFieldName.substring(0, sortFieldName.length() - 6).trim();
                            reverse = true;
                        }
                        
                        if (sortFieldName.length() > 0) {
                        	char first = sortFieldName.charAt(0);
	                        if (first >= 'A' && first <= 'Z') {
	                        	// meta sort
	                        	sortFieldName = sortFieldName.toUpperCase();
	                        } else {
	                        	// item sort
	                        	sortFieldName = sortFieldName.toLowerCase();
	                        }
                        }
                        
                        // sort order currently only german
                        sort = new Sort(new SortField(SORTITEM_PREFIX + sortFieldName, Locale.GERMANY, reverse));
                    } else if (option.equalsIgnoreCase(NATIVE_QUERYOPTION_INCLUDEVIRTUALCONTENT)) {
                    	includeVirtualContent = true;
                    } else if (option.startsWith("doctype:")) {
                        doctype = option.substring("doctype:".length()).trim();
                    }
                    else if(option.startsWith("operator:")) {
                    	String op = option.substring("operator:".length()).trim();
                    	if(op.equalsIgnoreCase("or"))
                    		defaultOperator = QueryParser.OR_OPERATOR;
                    }
                    
                }
            }    
            
            if (!includeVirtualContent) {
            	wholeQuery.add(new TermQuery(new Term(INDEXFIELD_ISVIRTUALCONTENT, String.valueOf(true))), BooleanClause.Occur.MUST_NOT);
            }
            
            // handle doctype option
            // we cannot be sure that all documents in index already contains the field DOCTYPE (introduced with OpenWGA 7.1) therefore we have to perform some excludes
            if (doctype.equals(DOCTYPE_CONTENT)) {
                wholeQuery.add(new TermQuery(new Term(INDEXFIELD_DOCTYPE, DOCTYPE_ATTACHMENT)), BooleanClause.Occur.MUST_NOT);
            } else if (!doctype.equals(DOCTYPE_ALL)) {
                wholeQuery.add(new TermQuery(new Term(INDEXFIELD_DOCTYPE, doctype)), BooleanClause.Occur.MUST);
            }

            //build phrase query                
            BooleanQuery phraseQuery = new BooleanQuery();                
            Iterator languageList = languagesPriorityList.iterator();                
            while (languageList.hasNext()) {
                WGLanguage languageItem = (WGLanguage) languageList.next();
                Analyzer analyzer = _core.getAnalyzerForLanguageCode(languageItem.getName());
                if (analyzer != null) {
                    QueryParser parser = new IndexingRuleBasedQueryParser(INDEXFIELD_ALLCONTENT, analyzer, defaultOperator, _indexedDbs, searchDBKeys, _metaKeywordFields);
                    Query query = parser.parse(phrase);
                    
                    BooleanQuery testPhraseAndLangQuery = new BooleanQuery();
                    testPhraseAndLangQuery.add(query, BooleanClause.Occur.MUST);
                    testPhraseAndLangQuery.add(new TermQuery(new Term(WGContent.META_LANGUAGE, languageItem.getName())), BooleanClause.Occur.MUST);
                    
                    phraseQuery.add(testPhraseAndLangQuery, BooleanClause.Occur.SHOULD);
                }
                else {
                    searchWithDefaultAnalyzer = true;
                }
            }
            
            if (searchWithDefaultAnalyzer) {
                QueryParser parser = new IndexingRuleBasedQueryParser(INDEXFIELD_ALLCONTENT, _core.getDefaultAnalyzer(), defaultOperator, _indexedDbs, searchDBKeys, _metaKeywordFields);
                Query query = parser.parse(phrase);
                phraseQuery.add(query, BooleanClause.Occur.SHOULD);
            }
            wholeQuery.add(phraseQuery, BooleanClause.Occur.MUST);
    
            TopDocs hits;
            //register executed query as output parameter
            parameters.put(WGDatabase.QUERYOPTION_RETURNQUERY, wholeQuery.toString());   
            // simplify query and register as taginfo
            parameters.put(TAGINFO_SIMPLIFIEDQUERY, rewrite(wholeQuery));
            
            long timeBefore = System.currentTimeMillis();
            if (sort != null) {
            	try {
            		hits = search(wholeQuery, max, sort);
            	} catch (NullPointerException e) {
            		// lucene bug when sorting for non existing fields with Locale
            		throw new WGQueryException(wholeQuery.toString(), "Sortfield '" + sortFieldName + "' not indexed.");
            	}
            }
            else {                
                try {
                    hits = search(wholeQuery, max, null);
                } catch (BooleanQuery.TooManyClauses e) {
                    parameters.put(TAGINFO_UNSPECIFICQUERY, new Boolean(true));
                    throw new WGQueryException(phrase, "Too many BooleanClauses in query. " +
                            "Please use a more specific query or increase value of " +
                            "'booleanQueryMaxClauseCount' via WGAManager. Current value is '" + this.getBooleanQueryMaxClauseCount() + "'.");
                }
            }
            
            long timeAfter = System.currentTimeMillis();
            long executionTime = timeAfter - timeBefore;

            LuceneResultSet resultSet;
            if (filterLanguages) {
                resultSet = new LuceneLanguageChoosingResultSet(hits,wga, parameters, wholeQuery, executionTime, languagesPriorityList);
            }
            else {
                resultSet = new LuceneMultiDBResultSet(hits, wga, parameters, wholeQuery, executionTime);
            }
            
            // put resultset in per thread list
            List rsList = (List) _resultsetList.get();
            if (rsList == null) {
                rsList = new LinkedList();
                _resultsetList.set(rsList);
            }
            rsList.add(resultSet);
            
            return resultSet;
        }
        catch (org.apache.lucene.queryParser.ParseException e) {
            throw new WGQueryException("Unable to parse lucene query", e.getMessage(), e);
        }
        catch (Exception e) {
            LOG.error("Error executing lucene search: " + e.getClass().getName() + " - " + e.getMessage(), e);
            throw new WGQueryException(phrase, e.getClass().getName() + ": " + e.getMessage(), e);
        }
    }
    
    /**
     * close opened resultsets for current thread
     */
    public void closeOpenedResultSets() {
        // get opened resultsSets for current thread
        List rsList = (List) _resultsetList.get();
        if (rsList != null) {
            Iterator it = rsList.iterator();
            // close each resultset and remove from list
            while (it.hasNext()) {
                LuceneResultSet rs = (LuceneResultSet) it.next();
                rs.close();
                it.remove();
            }
        }
    }
         
    /**
     * @param searchDBKeys
     * @return list with all languages for searchDBKeys indexed by lucene
     * @throws WGAPIException 
     */
    private List<WGLanguage> getLanguagesForSearchDBKeys(List searchDBKeys) throws WGAPIException {
        
        LinkedList<WGLanguage> languages = new LinkedList<WGLanguage>();
        
        Iterator it = searchDBKeys.iterator();
        while (it.hasNext()) {
            String dbKey = (String) it.next();            
            WGDatabase db = (WGDatabase) _core.getContentdbs().get(dbKey);
            if (db != null) {
                if (_indexedDbs.containsKey(dbKey)) {
                    languages.addAll(db.getLanguages().values());
                }
            }
        }
        return languages;
    }    
    
    
    public boolean isRebuildingIndex() {
        return _rebuildingIndex;
    }

    public boolean indexIsEnabled(String dbKey) {        
        return _indexedDbs.containsKey(dbKey);
    }

    public int getBooleanQueryMaxClauseCount() {
        return _booleanQueryMaxClauseCount;
    }

    public void setBooleanQueryMaxClauseCount(int booleanQueryMaxClauseCount) {
        this._booleanQueryMaxClauseCount = booleanQueryMaxClauseCount;
    }
    
    /**
     * @param dbkey
     * @param content
     * @return
     */
    private String buildUniqueIndexKey(String dbkey, String documentkey) {
        return dbkey + "/" + documentkey;
    }
    
    private String buildUniqueIndexKey(WGContent content) {
        return buildUniqueIndexKey(content.getDatabase().getDbReference(), content.getDocumentKey());
    }
    
    private String buildUniqueIndexKey(WGContent content, String filename) {
        return buildUniqueIndexKey(content) + "/" + filename;
    }

    /**
     * @return Returns the indexerIsRunning.
     */
    public boolean isIndexerRunning() {
        return _indexerIsRunning;
    }

    public void databaseConnected(WGDatabaseEvent event) {
        HashSet newConnectedDBKeys = new HashSet();
        newConnectedDBKeys.add(event.getDatabase().getDbReference());
        createOrUpdateDBIndex(event.getDatabase(), newConnectedDBKeys);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDatabaseConnectListener#databaseConnectionError(de.innovationgate.webgate.api.WGDatabaseEvent)
     */
    public void databaseConnectionError(WGDatabaseEvent event) {
    }

    public int getMaxDocsPerDBSession() {
        return _maxDocsPerDBSession;
    }

    public void setMaxDocsPerDBSession(int maxDocsPerDBSession) {
        _maxDocsPerDBSession = maxDocsPerDBSession;
    }    

    public Highlighter createHighlighter(String itemOrMeta, Query query, Formatter formatter) {
        if (itemOrMeta == null) {
            return null;
        }                   
        if (query == null) {            
            return null;
        }                 
                          
        // create scorer
        // use only terms for this item and terms for allcontent for scoring
        List terms = new ArrayList();
        WeightedTerm[] termsForThisItem = QueryTermExtractor.getTerms(query, false, itemOrMeta);
        if (termsForThisItem != null) {
            for (int i=0; i < termsForThisItem.length; i++) {
                terms.add(termsForThisItem[i]);
            }
        }
        WeightedTerm[] termsForAllContent = QueryTermExtractor.getTerms(query, false, LuceneManager.INDEXFIELD_ALLCONTENT);
        if (termsForAllContent != null) {
            for (int i=0; i < termsForAllContent.length; i++) {
                terms.add(termsForAllContent[i]);
            }
        }
        WeightedTerm[] termsArray = new WeightedTerm[terms.size()];
        for (int i=0; i < terms.size(); i++) {
            WeightedTerm term = (WeightedTerm) terms.get(i);
            termsArray[i] = term;
        }
        QueryTermScorer scorer = new QueryTermScorer(termsArray);

        // create highlighter
        Highlighter highlighter = new Highlighter(formatter, scorer);
        
        return highlighter;
    }    
    
    public TokenStream createTokenStream(String text, WGContent content) throws WGAPIException {
       return createTokenStream(new StringReader(text), content);
    }
    
    public TokenStream createTokenStream(Reader reader, WGContent content) throws WGAPIException {
        // retrieve analyzer
        Analyzer analyzer = retrieveAnalyzer(content);
                
        // @see org.apache.lucene.analysis.Analyzer - null for fieldname should be supported
        // most analyzers do not use the parameter fieldname
        TokenStream tokenStream = analyzer.tokenStream(null, reader);
        return tokenStream;
    }
    
    /**
     * 
     * @param db
     * @throws IOException
     * @throws WGIllegalArgumentException
     * @throws InterruptedException 
     * @deprecated pending feature - F00003426
     */
    public void performCustomDBIndexDeletions(WGDatabase db) throws IOException, WGIllegalArgumentException, InterruptedException {
        // check if db is not a full contentstore
        // this method is only supported for none fullcontentstores
        if (db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            throw new WGIllegalArgumentException("Method performCustomDBIndexDeletions() is unsupported for full featured contentstores.");
        }
        
        // check if db is already indexed
        if (!_indexedDbs.containsKey(db.getDbReference())) {
            throw new WGIllegalArgumentException("Cannot perform deletions on database '" + db.getDbReference() + "'. Database is not yet indexed. Ensure performCustomDBIndexUpdates() has been called first.");
        }
        
        // collect currently indexed document keys for this db
        Set currentlyIndexedDocuments = new HashSet();
        BooleanQuery query = new BooleanQuery();
        
        query.add(new TermQuery(new Term(INDEXFIELD_DBKEY, db.getDbReference())), BooleanClause.Occur.MUST);
                    
        TopDocs indexedDocuments = search(query, 500, null);
        for (ScoreDoc scoreDoc : indexedDocuments.scoreDocs) {
        	 org.apache.lucene.document.Document luceneDoc = getDocument(scoreDoc.doc);
        	 String documentKey = luceneDoc.get(LuceneManager.INDEXFIELD_DOCUMENTKEY);
             currentlyIndexedDocuments.add(documentKey);
        }
    }

    public File getIndexDirectory() {
        return _dir;
    }

    public void contentHasBeenMoved(WGContentEvent event) {
        if (event.getDatabase().getSessionContext().isTransactionActive()) {
            return;
        }
        String dbkey = (String) event.getDatabase().getDbReference();
        IndexingRequest indexingRequest = new IndexingRequest(dbkey, event.getDocumentKey());
        addDeletionRequest(indexingRequest);
        addAdditionRequest(indexingRequest);
    }  
    
    private IndexSearcher getIndexSearcher() throws CorruptIndexException, IOException, InterruptedException {
    	if (_indexSearcher == null) {
    		synchronized (this) {
				if (_indexSearcher == null) {						
					_indexSearcher = new IndexSearcher(IndexReader.open(_indexDirectory, true));
					if (_customSimilarity != null) {
					    _indexSearcher.setSimilarity(_customSimilarity);
					}
				}
			}
    	}
    	
    	//if (!_indexSearcher.getIndexReader().isCurrent()) {
    		synchronized (this) {
    			if (!_indexSearcher.getIndexReader().isCurrent()) {
					// wait for current searches to finish and block further searches
					_indexSearcherSemaphore.acquire(MAX_CONCURRENT_SEARCHES);
    				try {
    				    _indexSearcher.getIndexReader().close();
    					_indexSearcher.close(); 
    					_indexSearcher = new IndexSearcher(IndexReader.open(_indexDirectory, true));
    					if (_customSimilarity != null) {
                            _indexSearcher.setSimilarity(_customSimilarity);
                        }
    				} finally {    					
    					_indexSearcherSemaphore.release(MAX_CONCURRENT_SEARCHES);	
    				}
    			}
    		}   				
    	//}	    	
    	return _indexSearcher;
    }
    
    private TopDocs search(Query query, int max, Sort sort) throws IOException, InterruptedException {			    
		IndexSearcher searcher = getIndexSearcher();
		_indexSearcherSemaphore.acquire();
		try {    					
    		if (sort != null) {
    			return searcher.search(query, null, max, sort);	
    		} else {
    			return searcher.search(query, null, max);
    		}    
		} finally {
			_indexSearcherSemaphore.release();
		}
    }
    
    private Query rewrite(Query query) throws CorruptIndexException, IOException, InterruptedException {
    	IndexSearcher searcher = getIndexSearcher();
    	_indexSearcherSemaphore.acquire();
    	try {    		
    		return query.rewrite(searcher.getIndexReader());
    	} finally {
    		_indexSearcherSemaphore.release();
    	}
    }

	public org.apache.lucene.document.Document getDocument(int i) throws CorruptIndexException, IOException, InterruptedException {
		IndexSearcher searcher = getIndexSearcher();
		_indexSearcherSemaphore.acquire();
		try {
			return searcher.doc(i);
		} finally {
			_indexSearcherSemaphore.release();
		}
	}

	public Explanation explain(Query query, int doc) throws CorruptIndexException, IOException, InterruptedException {
		IndexSearcher searcher = getIndexSearcher();
		_indexSearcherSemaphore.acquire();
		try {
			return searcher.explain(query, doc);
		} finally {
			_indexSearcherSemaphore.release();
		}
	}

    public void contentStatusChanged(WGContentEvent event) {
    }
    
    public Map<String,Queue<IndexingRequest>> getDeletionRequestsMap() {
        return _deletionRequestsMap;
    }

    public Map<String,Queue<IndexingRequest>> getAdditionRequestsMap() {
        return _additionRequestsMap;
    }

    public LuceneIndexFileRule retrieveFileRule(WGDatabase db, String filename) {            
        LuceneConfiguration config = (LuceneConfiguration) _indexedDbs.get(db.getDbReference());
        return config.getMatchingFileRule(filename);
    }

    public LuceneIndexItemRule retrieveItemRule(WGDatabase db, String itemName) {            
        LuceneConfiguration config = (LuceneConfiguration) _indexedDbs.get(db.getDbReference());
        if (config != null) {
            return config.getMatchingItemRule(itemName);
        }
        else {
            return null;
        }
    }
    
    public long getRemainingAdditionRequests() {
        
        int remaining = 0;
        for (Queue<IndexingRequest> req : _additionRequestsMap.values()) {
            remaining+=req.size();
        }
        return remaining;
    }
    
    public long getRemainingDeletionRequests() {
        
        int remaining = 0;
        for (Queue<IndexingRequest> req : _deletionRequestsMap.values()) {
            remaining+=req.size();
        }
        return remaining;
    }
    
    public long getRemainingDbUpdateRequests() {
        
        return _dbUpdateRequests.size();
        
    }

    public Indexer getIndexer() {
        return _indexer;
    }
    
    public WGDatabaseRevision getDatabaseUpdateStatus(String dbKey) {
        return _indexConfig.getLastUpdated(dbKey);
    }
    
    
}
