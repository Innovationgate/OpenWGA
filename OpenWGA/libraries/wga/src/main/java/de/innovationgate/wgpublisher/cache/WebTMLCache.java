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

package de.innovationgate.wgpublisher.cache;

import java.io.IOException;
import java.io.Serializable;
import java.io.Writer;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.apache.commons.io.IOExceptionWithCause;
import org.apache.log4j.Logger;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheEntryParams;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentEvent;
import de.innovationgate.webgate.api.WGContentEventListener;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.cache.WebTMLCache.PreloaderTask;
import de.innovationgate.wgpublisher.modules.serveroptions.CacheModuleDefinition;
import de.innovationgate.wgpublisher.scheduler.Job;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;

/**
 * A wrapper around a generic cache object to use it as WebTML cache
 */
public class WebTMLCache {
    
    public static final String CACHEPRELOADTASK_NAME = "OpenWGA WebTML Preload Cache Updating Task";
    
    public static final String SERVEROPTION_CACHE_PRELOAD_INTERVAL = "Cache.WebTML.Preload.Interval";
    public static final int SERVEROPTIONDEFAULT_CACHE_PRELOAD_INTERVAL = 5;
    
    public static final String SERVEROPTION_CACHE_PRELOAD_REFRESHINTERVAL = "Cache.WebTML.Preload.RefreshInterval";
    public static final int SERVEROPTIONDEFAULT_CACHE_PRELOAD_REFRESHINTERVAL = 30;

    public static final int MAX_ATTEMPTS = 10;

    private static final String DBATTRIB_PRELOAD_LISTENERS = "WebTMLCache.PreloadListeners";
    
    private int _capacity;
    private int _preloadInterval;
    private int _preloadRefreshInterval;
    private ConcurrentLinkedQueue<PreloadEvent> _preloadEvents = new ConcurrentLinkedQueue<WebTMLCache.PreloadEvent>();
    

    
    public ConcurrentLinkedQueue<PreloadEvent> getPreloadEvents() {
        return _preloadEvents;
    }
    
    public class RefreshTask extends TimerTask {

        @Override
        public void run() {
            
            boolean anythingAdded = false;
            for (WGDatabase db : _core.getContentdbs().values()) {
                List<PreloaderListener> listeners = (List<PreloaderListener>) db.getAttribute(DBATTRIB_PRELOAD_LISTENERS);
                if (listeners != null) {
                    for (PreloaderListener listener : listeners) {
                        if (anythingAdded == false) {
                            anythingAdded = true;
                            _core.getLog().info("Refreshing all WebTML preload caches with next update run");
                        }
                        listener.addPreloadEvent();
                    }
                }
            }
            
            
            
        }
        
        
    }
    
    public class PreloaderTask extends TimerTask {
        
        List<PreloadEvent> _currentEvents = null;
        private boolean _initial = true;

        @Override
        public void run() {
            
            Thread.currentThread().setName("WebTML Cache Preloader Task");
            
            if (_core.getStatus() != WGACoreEvent.TYPE_ONLINE) {
                return;
            }
            
            if (_initial) {
                _core.getLog().info("Initial preloading of WebTML caches starting");
            }
            
            Set<PreloaderListener> alreadyProcessedListeners = new HashSet<PreloaderListener>(); 

            boolean anythingPreloaded = false;
            
            // First read all current elements of the queue
            _currentEvents = new ArrayList<PreloadEvent>();
            while (true) {
                PreloadEvent event = getPreloadEvents().poll();
                if (event == null) {
                    break;
                }
                _currentEvents.add(event);
            }
            
            if (_initial && _currentEvents.size() == 0) {
                _core.getLog().info("No preload caches registered");
            }

            // Then process the current elements. On lister duplicates in this list we can be sure that only the first needs to be processed.
            int preloads = 0;
            int errors = 0;
            int fatalErrors = 0;
            for (PreloadEvent event : _currentEvents) {
                
                anythingPreloaded = true;
                PreloaderListener listener = event.getListener();
               
                if (!listener.getDb().isReady()) { // Caches for an already closed database. Just drop these.
                    continue;
                }
                
                if (!_core.getContentdbs().values().contains(listener.getDb())) { // Databases that are just being connected. Event will get requeued
                    getPreloadEvents().add(event);   
                }
                
                if (alreadyProcessedListeners.contains(listener)) {
                    event.setProcessed(true);
                    continue;
                }
                
                event.addAttempt();
                CachePreloader preloader = listener.getPreloader();
                    
                try {
                    Date cacheDate = new Date();
                    String code = preloader.preloadCache(_core, listener.getDb());
                    if (code != null) {
                        if (_initial || event.isInitial()) {
                            _core.getLog().info("Preloading WebTML cache '" + listener.getCacheId() + "/" +listener.getCacheKey() + "' for database '" + listener.getDb().getDbReference() + "'. Size: " + code.length() + " characters");
                            if (code.length() == 0) {
                                _core.getLog().info("Content of WebTML preload cache '" + listener.getCacheId() + "/" +listener.getCacheKey() + "' for database '" + listener.getDb().getDbReference() + "' is empty");
                            }
                        }
                        putPreloadCacheEntry(listener.getDb().getDbReference(), listener.getCacheId(), listener.getCacheKey(), code, cacheDate, 0);
                        listener.setInitialized(true);
                        event.setProcessed(true);
                        alreadyProcessedListeners.add(listener);
                        preloads++;
                    }
                }
                catch (CachePreloadException e) {
                    errors++;
                    if (event.getAttempts() >= MAX_ATTEMPTS) {
                        _core.getLog().error("WebTML cache preloading of cache '" + listener.getCacheId() + "/" + listener.getCacheKey() + "' for database '" + listener.getDb().getDbReference() + "' failed because of exception. Attempt: " + event.getAttempts() + " .Requeue: " + e.isRequeue(), e);
                    }
                    else {
                        _core.getLog().error("WebTML cache preloading of cache '" + listener.getCacheId() + "/" + listener.getCacheKey() + "' for database '" + listener.getDb().getDbReference() + "' failed because of exception. Attempt: " + event.getAttempts() + " .Requeue: false", e);
                        if (e.isRequeue()) {
                            getPreloadEvents().add(event);
                        }
                    }
                }
                catch (Exception e) {
                    fatalErrors++;
                    _core.getLog().error("Fatal exception preloading WebTML cache '" + listener.getCacheId() + "/" + listener.getCacheKey() + "' for database '" + listener.getDb().getDbReference() + "'. Requeue: false", e);
                }
            }
            
            if (_initial) {
                if (errors == 0 && fatalErrors == 0) {
                    if (preloads > 0) {
                        _core.getLog().info("Initial preloading of WebTML caches finished");
                    }
                }
                else if (errors > 0 && fatalErrors == 0){
                    _core.getLog().warn("Initial preloading of WebTML caches finished with " + errors + " errors. Some preload caches are not yet available.");
                }
                else {
                    _core.getLog().error("Initial preloading of WebTML caches finished with " + errors + " recoverable errors and " + fatalErrors + " fatal errors. Some preload caches are missing!");
                }
                    
                _initial = false;
            }
            else {
                if (errors == 0 && fatalErrors == 0) {
                    if (preloads > 0) {
                        _core.getLog().info("WebTML cache preloader updated " + preloads + " cache entries");
                    }
                }
                else if (errors > 0 && fatalErrors == 0){
                    _core.getLog().warn("Preloading of WebTML caches updated " + preloads + " cache entries correctly but encountered recoverable errors on " + errors + " other entries. Some preload caches failed to update and were requeued.");
                }
                else {
                    _core.getLog().error("Preloading of WebTML caches updated " + preloads + " cache entries correctly but encountered recoverable errors on " + errors + " and fatal errors on " + fatalErrors + " others. Some preload caches are not updatable!");
                }
                
            }
        }

        protected List<PreloadEvent> getCurrentEvents() {
            return _currentEvents;
        }

    }

    public class PreloadEvent {

        private PreloaderListener _listener;
        private int _attempts = 0;
        private boolean _initial = false;
        private boolean _processed = false;

        protected boolean isProcessed() {
            return _processed;
        }

        protected void setProcessed(boolean processed) {
            _processed = processed;
        }

        public int getAttempts() {
            return _attempts;
        }

        public void addAttempt() {
            _attempts++;
        }

        public PreloadEvent(PreloaderListener listener, boolean initial) {
            _listener = listener;
            _initial  = initial;
        }

        public PreloaderListener getListener() {
            return _listener;
        }

        public boolean isInitial() {
            return _initial;
        }
        
    }
    
    public class PreloaderListener implements WGContentEventListener, WGDesignChangeListener {

        private WGDatabase _db;
        private CachePreloader _preloader;
        public String _cacheId;
        public String _cacheKey;
        private boolean _initialized = false;

        public boolean isInitialized() {
            return _initialized;
        }

        public void setInitialized(boolean initialized) {
            _initialized = initialized;
        }

        public PreloaderListener(CachePreloader preloader, WGDatabase db, String cacheId, String cacheKey) {
            _preloader = preloader;
            _db = db;
            _cacheId = cacheId;
            _cacheKey = cacheKey;
        }

        private void addPreloadEvent() {
            WebTMLCache.this._preloadEvents.add(new PreloadEvent(this, false));
        }

        public WGDatabase getDb() {
            return _db;
        }

        public CachePreloader getPreloader() {
            return _preloader;
        }

        public String getCacheId() {
            return _cacheId;
        }

        public String getCacheKey() {
            return _cacheKey;
        }

        @Override
        public void contentCreated(WGContentEvent contentEvent) {
        }

        @Override
        public boolean contentSaved(WGContentEvent contentEvent) {
            return true;
        }

        @Override
        public void contentHasBeenSaved(WGContentEvent event) {
            try {
                if (!event.getContent().getStatus().equals(WGContent.STATUS_DRAFT) && !event.getContent().getStatus().equals(WGContent.STATUS_REVIEW)) {
                    addPreloadEvent();
                }
            }
            catch (WGAPIException e) {
                _core.getLog().error("Exception processing contentHasBeenSaved event", e);
            }
        }

        @Override
        public void contentHasBeenDeleted(WGContentEvent event) {
            addPreloadEvent();
            
        }

        @Override
        public void contentHasBeenMoved(WGContentEvent event) {
            try {
                if (!event.getContent().getStatus().equals(WGContent.STATUS_DRAFT) && !event.getContent().getStatus().equals(WGContent.STATUS_REVIEW)) {
                    addPreloadEvent();
                }
            }
            catch (WGAPIException e) {
                _core.getLog().error("Exception processing contentHasBeenSaved event", e);
            }
            
        }

        @Override
        public void contentStatusChanged(WGContentEvent event) {
        }

        @Override
        public void designChanged(WGDesignChangeEvent event) {
            addPreloadEvent();
        }
        
        
    }

    public static class CacheEntry implements Serializable {

        /**
         * 
         */
        private static final long serialVersionUID = 1L;

        private String _code;

        private Date _testedDate;
        
        private long _writeTime;
        
        private boolean _valid = true;

        private int _latency;

        public CacheEntry(String code, Date date, int latency) {
            super();
            _code = code;
            _testedDate = date;
            _writeTime = System.currentTimeMillis();
            _latency = latency;
        }

        public String getCode() {
            return _code;
        }

        public Date getDate() {
            return _testedDate;
        }

        public boolean isValid() {
            return _valid && !isOutdated();
        }

        public void setCode(String code) {
            _code = code;
        }

        public void setTestedDate(Date date) {
            _testedDate = date;
        }

        protected void setValid(boolean valid) {
            _valid = valid;
        }

        public int getLatency() {
            return _latency;
        }
        
        public boolean isOutdated() {
            if (_latency != 0) {
                return System.currentTimeMillis() > _writeTime + (_latency * 1000);
            }
            else {
                return false;
            }
                
        }

    }

    public static class CacheKey {
        
        private String _dbKey;
        private String _tagId;
        private String _expressionKey;

        public CacheKey(String dbKey, String tag, String expressionKey) {
            super();
            _dbKey = dbKey;
            _tagId = tag;
            _expressionKey = expressionKey;
        }
        
        public CacheKey(String keyStr) {

            StringTokenizer tokens = new StringTokenizer(keyStr, "//");
            _dbKey = tokens.nextToken();
            _tagId = tokens.nextToken();
            _expressionKey = WGUtils.joinRemainingTokens(tokens, "//");
            
        }

        public String getExpressionKey() {
            return _expressionKey;
        }

        public String getTagId() {
            return _tagId;
        }

        public String toString() {
            return _dbKey + "//" +_tagId + "//" + _expressionKey;
        }

        public String getDbKey() {
            return _dbKey;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((_dbKey == null) ? 0 : _dbKey.hashCode());
            result = prime * result + ((_expressionKey == null) ? 0 : _expressionKey.hashCode());
            result = prime * result + ((_tagId == null) ? 0 : _tagId.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey) obj;
            if (_dbKey == null) {
                if (other._dbKey != null)
                    return false;
            }
            else if (!_dbKey.equals(other._dbKey))
                return false;
            if (_expressionKey == null) {
                if (other._expressionKey != null)
                    return false;
            }
            else if (!_expressionKey.equals(other._expressionKey))
                return false;
            if (_tagId == null) {
                if (other._tagId != null)
                    return false;
            }
            else if (!_tagId.equals(other._tagId))
                return false;
            return true;
        }
    }

    private Cache _cache;
    private Cache _preloadCache;

    private int _cacheIndex;
    private WGACore _core;

    private Timer _preloaderTimer;

    private PreloaderTask _preloaderTask;

    public void clear() throws CacheException {
        init();
        runPreloadCacheTask();
    }
    
    public void runPreloadCacheTask() {
        
        if (_core.getStatus() != WGACoreEvent.TYPE_ONLINE) {
            return;
        }
        
        try {
            Job cachePreloadTask = _core.getScheduler().getJob(CACHEPRELOADTASK_NAME);
            if (cachePreloadTask != null) {
                _core.getScheduler().run(CACHEPRELOADTASK_NAME, "OpenWGA Cache Update Trigger", null, null);
            }
        }
        catch (JobFailedException e) {
            _core.getLog().error("Exception running preload cache task", e);
        }
    }

    public void clearForDatabase(String dbKey) throws CacheException {
       _cache.flushGroup(dbKey); 
    }

    public void close() throws CacheException {
        _cache.destroy();
        _preloadCache.destroy();
        _preloaderTimer.cancel();
    }

    public void dump(Writer out) throws IOException {
        out.write("DBKEY;TAGID;CACHEKEY;TIME;SIZE\n");
        DateFormat dateFormat = new SimpleDateFormat();
        try {
            Iterator keys = _cache.getEntryKeys().iterator();
            while (keys.hasNext()) {
                String keyStr = (String) keys.next();
                try {
                    CacheEntry entry = (CacheEntry) _cache.readEntry(keyStr);
                    if (entry != null) {
                        CacheKey key = new CacheKey(keyStr);
                        out.write(key.getDbKey());
                        out.write(";");
                        out.write(key.getTagId());
                        out.write(";");
                        out.write(key.getExpressionKey());
                        out.write(";");
                        out.write(dateFormat.format(entry.getDate()));
                        out.write(";");
                        out.write(String.valueOf(((String) entry.getCode()).length()));
                        out.write("\n");
                    }
                }
                catch (CacheException e) {
                    out.write("Exception reading cache entry " + keyStr + ": " + e.getClass().getName() + " - " + e.getMessage() + "\n");
                    Logger.getLogger("wga.webtmlcache").error("Exception dumping WebTML cache entry " + keyStr, e);
                }
            }
        }
        catch (CacheException e) {
            throw new IOExceptionWithCause("Exception dumping WebTML Cache", e);
        }
    }

    public CacheEntry getCacheEntry(String dbKey, String tagid, String key) throws CacheException {

        CacheKey cacheKey = new CacheKey(dbKey, tagid, key);

        return (CacheEntry) _cache.readEntry(cacheKey.toString());
                
    }
    
    public CacheEntry getPreloadCacheEntry(String dbKey, String tagid, String key) throws CacheException {

        CacheKey cacheKey = new CacheKey(dbKey, tagid, key);

        return (CacheEntry) _preloadCache.readEntry(cacheKey.toString());
                
    }
    
    public void removeCache(String dbKey, String tagid, String key) throws CacheException {
        CacheKey cacheKey = new CacheKey(dbKey, tagid, key);
        _cache.flushEntry(cacheKey.toString());
    }

    public long getEntriesCount() {
        try {
            return _cache.getSize();
        }
        catch (CacheException e) {
            return 0;
        }
    }
    
    public long getPreloadEntriesCount() {
        try {
            return _preloadCache.getSize();
        }
        catch (CacheException e) {
            return 0;
        }
    }

    public WebTMLCache(WGACore core, WGAConfiguration config) throws CacheException, OptionConversionException {

        _core = core;
        configure(config);
        init();

    }

    public boolean configure(WGAConfiguration config) throws CacheException, OptionConversionException {
        
        boolean changed = false;
        
        int tmlCache = config.getWebtmlCacheSize();
        if (tmlCache != getCapacity()) {
            setCapacity(tmlCache);
            changed = true;
        }
        
        OptionReader reader = OptionReader.create(config.getServerOptions(), new CacheModuleDefinition());
        
        int preloadInterval = (Integer) reader.readOptionValueOrDefault(SERVEROPTION_CACHE_PRELOAD_INTERVAL);
        if (preloadInterval != _preloadInterval) {
            _preloadInterval = preloadInterval;
            changed = true;
        }
        
        int preloadRefreshInterval = (Integer) reader.readOptionValueOrDefault(SERVEROPTION_CACHE_PRELOAD_REFRESHINTERVAL);
        if (preloadRefreshInterval != _preloadRefreshInterval) {
            _preloadRefreshInterval = preloadRefreshInterval;
            changed = true;
        }
        
        return changed;
        
    }

    private synchronized void init() throws CacheException {

        int cacheIndex = increaseCacheIndex();
        
        Cache oldCache = _cache;
        _cache = CacheFactory.createCache("WebTMLCache-" + cacheIndex, _capacity, null);
        if (oldCache != null) {
            oldCache.destroy();
        }
        
        // Will not be replaced on clear(), instead the preload cache task will run again
        if (_preloadCache == null) {
            _preloadCache = CacheFactory.createCache("WebTMLPreloadCache-" + cacheIndex, 0, null);
        }
        
        if (_preloaderTimer != null) {
            _preloaderTimer.cancel();
        }
        _preloaderTimer = new Timer();
        _preloaderTask = new PreloaderTask();
        _preloaderTimer.schedule(_preloaderTask, 1000 * 60, 1000 * _preloadInterval);
        _preloaderTimer.schedule(new RefreshTask(), _preloadRefreshInterval * 1000 * 60, _preloadRefreshInterval * 1000 * 60);
        
        
    }

    private int increaseCacheIndex() {
        _cacheIndex++;
        return _cacheIndex;
    }

    private CacheEntry putCacheEntry(Cache cache, String dbKey, String tagid, String key, String content, Date cacheDate, int latency) throws CacheException {

        CacheKey cacheKey = new CacheKey(dbKey, tagid, key);

        CacheEntry cacheEntry = new CacheEntry(content, cacheDate, latency);
        CacheEntryParams params = new CacheEntryParams(dbKey, 0);
        cache.writeEntryWithParams(cacheKey.toString(), cacheEntry, params);
        
        return cacheEntry;
        
    }
    
    
    public CacheEntry putCacheEntry(String dbKey, String tagid, String key, String content, Date cacheDate, int latency) throws CacheException {
        return putCacheEntry(_cache, dbKey, tagid, key, content ,cacheDate, latency);
    }
    
    public CacheEntry putPreloadCacheEntry(String dbKey, String tagid, String key, String content, Date cacheDate, int latency) throws CacheException {
        return putCacheEntry(_preloadCache, dbKey, tagid, key, content ,cacheDate, latency);
    }
   
    


    public int getCapacity() {
        return _capacity;
    }

    public void setCapacity(int capacity) throws CacheException {
        _capacity = capacity;
    }

    public Cache getCache() {
        return _cache;
    }
    
    public void addCachePreloader(CachePreloader preloader, WGDatabase db, String cacheId, String cacheKey) {
        PreloaderListener listener = new PreloaderListener(preloader, db, cacheId, cacheKey);
        db.addContentEventListener(listener);
        db.addDesignChangeListener(listener);
        
        List<PreloaderListener> listeners = (List<PreloaderListener>) db.getAttribute(DBATTRIB_PRELOAD_LISTENERS);
        if (listeners == null) {
            listeners = new ArrayList<PreloaderListener>();
            db.setAttribute(DBATTRIB_PRELOAD_LISTENERS, listeners);
        }
        listeners.add(listener);
        
        _preloadEvents.add(new PreloadEvent(listener, true));
    }
    
    public int getUninitializedPreloadCacheCount(WGDatabase db) {
        
        int count = 0;
        List<PreloaderListener> listeners = (List<PreloaderListener>) db.getAttribute(DBATTRIB_PRELOAD_LISTENERS);
        if (listeners == null) {
            return 0;
        }
        
        for (PreloaderListener listener : listeners) {
            if (!listener.isInitialized()) {
                count++;
            }
        }
        
        return count;
        
    }
    
    public int getRegisteredPreloadCacheCount(WGDatabase db) {
        
        int count = 0;
        List<PreloaderListener> listeners = (List<PreloaderListener>) db.getAttribute(DBATTRIB_PRELOAD_LISTENERS);
        if (listeners == null) {
            return 0;
        }
        else {
            return listeners.size();
        }
        
        
    }
    
    public int getRemainingPreloadInitEventsCount() {
        
        int count=0;
        
        // Currently processed
        List<PreloadEvent> currentEvents = _preloaderTask.getCurrentEvents();
        if (currentEvents != null) {
            currentEvents = new ArrayList<PreloadEvent>(currentEvents);
            for (PreloadEvent event : currentEvents) {
                if (!event.isProcessed() && event.isInitial()) {
                    count++;
                }
            }
        }
        
        // Waiting in queue
        for (PreloadEvent event : _preloadEvents) {
            if (event.isInitial()) {
                count++;
            }
        }
        
        return count;
        
        
    }

}
