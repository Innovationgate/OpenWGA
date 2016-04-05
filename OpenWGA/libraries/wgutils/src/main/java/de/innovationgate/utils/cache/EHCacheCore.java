/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.utils.cache;

import java.io.Serializable;
import java.lang.management.ManagementFactory;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.management.MBeanServer;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheException;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Element;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.config.Configuration;
import net.sf.ehcache.event.CacheEventListener;
import net.sf.ehcache.management.ManagementService;
import net.sf.ehcache.store.FifoPolicy;
import net.sf.ehcache.store.LfuPolicy;
import net.sf.ehcache.store.LruPolicy;
import net.sf.ehcache.store.Policy;

/**
 * Cache core implementation based on EHCache (http://ehcache.sourceforge.net)
 *
 */
public class EHCacheCore implements de.innovationgate.utils.cache.CacheCore {
    
    private static CacheManager _cacheManager;
   
    class RemoveListener implements CacheEventListener {
        
        /* (non-Javadoc)
         * @see java.lang.Object#clone()
         * Workaround for weird clone() method overload error
         */
        @Override
        public Object clone() {
            try {
                return super.clone();
            }
            catch (CloneNotSupportedException e) {
                e.printStackTrace();
                return null;
            }
        }
        
        public void notifyElementRemoved(Ehcache arg0, Element arg1) throws CacheException {
            dispose(arg1);
        }
        
        public void notifyElementExpired(Ehcache arg0, Element arg1) {
            dispose(arg1);
        }

        private void dispose(Element arg1) {
            Entry entry = (Entry) arg1.getValue();
            if (entry != null) {
                _disposalListener.dispose(String.valueOf(arg1.getKey()), entry.getValue());
            }
        }
        
        public void notifyElementEvicted(Ehcache arg0, Element arg1) {
            dispose(arg1);                        
        }

        public void dispose() {
        }

        public void notifyElementPut(Ehcache arg0, Element arg1) throws CacheException {
        }

        public void notifyElementUpdated(Ehcache arg0, Element arg1) throws CacheException {
        }

        public void notifyRemoveAll(Ehcache arg0) {
            _disposalListener.disposeAll();
        }
    }
    
    class Entry implements Serializable {
        /**
         * 
         */
        private static final long serialVersionUID = 1L;
        public Entry(Object value, String group) {
            super();
            this.value = value;
            this.group = group;
        }
        private String group;
        private Object value;
        public String getGroup() {
            return group;
        }
        public Object getValue() {
            return value;
        }
        
    }
    
    public static synchronized CacheManager getCacheManager() {
        if (_cacheManager == null) {
            Configuration config = new Configuration();
            CacheConfiguration defaultConfig = new CacheConfiguration();
            defaultConfig.setEternal(false);
            defaultConfig.setOverflowToDisk(false);
            defaultConfig.setTimeToIdleSeconds(120);
            defaultConfig.setTimeToLiveSeconds(120);
            defaultConfig.setDiskPersistent(false);
            defaultConfig.setDiskExpiryThreadIntervalSeconds(120);
            config.addDefaultCache(defaultConfig);
            config.setName(EHCacheCore.class.getName() + "#CacheManager");
            
            _cacheManager = new CacheManager(config);
            MBeanServer mBeanServer = ManagementFactory.getPlatformMBeanServer();
            ManagementService.registerMBeans(_cacheManager, mBeanServer, false, false, false, true);
        }
        return _cacheManager;

    }

    private Cache _ehcache;

    private String _name;

    private int _capacity;

    private Map<String,Date> _lastCacheFlush = new ConcurrentHashMap<String,Date>();

    private int _timeToLive;

    private CacheDisposalListener _disposalListener;

    private Policy _evictionPolicy;

    public void destroy() throws de.innovationgate.utils.cache.CacheException {
        if (_ehcache == null) {
            return;
        }

        try {
            CacheManager cacheManager = getCacheManager();
            cacheManager.removeCache(_name);
        }
        catch (CacheException e) {
            throw new de.innovationgate.utils.cache.CacheException("Exception closing ehcache", e);
        }
    }

    public void flushAll() throws de.innovationgate.utils.cache.CacheException {
        if (_ehcache == null) {
            return;
        }

        try {
            _ehcache.removeAll();
        }
        catch (Exception e) {
            throw new de.innovationgate.utils.cache.CacheException("Exception flushing all entries", e);
        }

    }

    public void flushEntry(Serializable key) throws de.innovationgate.utils.cache.CacheException {
        try {
            _ehcache.remove(key);
        }
        catch (Exception e) {
            throw new de.innovationgate.utils.cache.CacheException("Exception flushing entry", e);
        }
    }

    public void flushGroup(String group) {
        _lastCacheFlush.put(group, new Date());
    }

    public long getMaxSize() {
        return _capacity;
    }

    public long getSize() throws de.innovationgate.utils.cache.CacheException {
        try {
            return _ehcache.getSize();
        }
        catch (Exception e) {
            throw new de.innovationgate.utils.cache.CacheException("Exception retrieving size" ,e);
        }
    }

    public int getUtilisation() {
        long hitsCount = _ehcache.getStatistics().getCacheHits();
        long totalCount = hitsCount + _ehcache.getStatistics().getCacheMisses();
        if (totalCount == 0) {
            return 0;
        }
        return (int) Math.round(hitsCount / (totalCount / (double) 100));
    }

    public void init(String name, int capacity, Map<String,Object> params) throws de.innovationgate.utils.cache.CacheException {
        
        _name = name;
        _capacity = capacity;
        _timeToLive = 0;
        _evictionPolicy = new LruPolicy();
        
        // Use params
        if (params.containsKey(de.innovationgate.utils.cache.Cache.PARAM_TIME_TO_LIVE_SECONDS)) {
            try {
                _timeToLive = ((Integer) params.get(de.innovationgate.utils.cache.Cache.PARAM_TIME_TO_LIVE_SECONDS)).intValue();
            }
            catch (ClassCastException e) {
                throw new CacheException("Invalid data type for timeToLive, must be Integer but is: " + params.get(de.innovationgate.utils.cache.Cache.PARAM_TIME_TO_LIVE_SECONDS).getClass().getName());
            } 
        }
        
        if (params.containsKey(de.innovationgate.utils.cache.Cache.PARAM_DISPOSAL_LISTENER)) {
            _disposalListener = (CacheDisposalListener) params.get(de.innovationgate.utils.cache.Cache.PARAM_DISPOSAL_LISTENER);
        }
        
        if (params.containsKey(de.innovationgate.utils.cache.Cache.PARAM_EVICTION_POLICY)) {
            String policyString = (String) params.get(de.innovationgate.utils.cache.Cache.PARAM_EVICTION_POLICY);
            if ("fifo".equals(policyString)) {
                _evictionPolicy = new FifoPolicy();
            }
            else if ("lfu".equals(policyString)) {
                _evictionPolicy = new LfuPolicy();
            }
            else if (!"lru".equals(policyString)) {
                throw new de.innovationgate.utils.cache.CacheException("Unsupported eviction policy: " + policyString);
            }
        }
        
        try {
            _ehcache = new Cache(name, capacity, false, (_timeToLive == 0), _timeToLive, 0);
            if (_disposalListener != null) {
                boolean registerListener = _ehcache.getCacheEventNotificationService().registerListener(new RemoveListener());
            }
            getCacheManager().addCache(_ehcache);
            _ehcache.setMemoryStoreEvictionPolicy(_evictionPolicy);
            _ehcache.setStatisticsEnabled(true);
        }
        catch (Exception e) {
            throw new de.innovationgate.utils.cache.CacheException("Exception initializing cache", e);
        }
        
    }
    
    private boolean isActive(Element element) {
        

        if (element == null) {
            return false;
        }

        Entry entry = (Entry) element.getValue();
        if (entry.getGroup() == null) {
            return true;
        }
        
        Date cacheFlush = (Date) _lastCacheFlush.get(entry.getGroup());
        if (cacheFlush != null && cacheFlush.getTime() > element.getCreationTime()) {
            return false;
        }
        
        return true;
    }

    public Object readEntry(Serializable key) {
        if (_ehcache == null) {
            return null;
        }
        
        try {
            Element element = _ehcache.get(key);
            if (isActive(element)) {
                Entry entry = (Entry) element.getValue();
                return entry.getValue();
            }
            else {
                return null;
            }
        }
        catch (Exception e) {
            throw new CacheException("Error reading item cache for key '" + key + "'", e);
        }
    }

    public void writeEntry(Serializable key, Object obj, CacheEntryParams params) {
        if (_ehcache == null) {
            return;
        }
        
        String group = params.getGroup();
        int secondsToLive = params.getSecondsToLive();
        
        Entry entry = new Entry(obj, group);
        
        try {
            Element element = new Element(key, entry);
            if (secondsToLive != 0) {
                element.setTimeToLive(secondsToLive);
            }
            _ehcache.put(element);
        }
        catch (RuntimeException e) {
            throw new CacheException("Exception writing cache entry for key '" + key + "', group '" + group + "'",e);
        }


    }

    public Set<Serializable> getEntryKeys() {
        @SuppressWarnings("unchecked")
        List<Serializable> keys = _ehcache.getKeys();
        return new HashSet<Serializable>(keys);
    }

    public void touchEntry(Serializable key) throws de.innovationgate.utils.cache.CacheException {
        if (_ehcache == null) {
            return;
        }
        
        Element element = _ehcache.get(key);
        if (isActive(element)) {
            element.updateAccessStatistics();
        }
        
    }

    public Cache getEhcache() {
        return _ehcache;
    }

}
