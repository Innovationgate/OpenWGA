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
import java.util.Map;
import java.util.Set;

/**
 * A general purpose cache for WGA and all accompanied functionalities.
 * Create a cache object by using {@link CacheFactory#createCache(String, int, Map)}.
 * <p>
 * This cache offers the following features:
 * <ul>
 * <li>In-Memory caching
 * <li>Reading and writing of cache entries
 * <li>Specifying a maximum number of entries. After reaching this threshold adding new elements will 
 * result in other elements being dropped (chosen on a "Least recently used" basis by default)
 * <li>Specifying a global "timeToLive" time in seconds, which is the maximum allowed lifetime for an entry
 * <li>Specifying a specific "timeToLive" time for each individual entry in seconds, which is the maximum allowed lifetime for this entry
 * <li>Specifying groups of cache entries that may be flushed separately
 * <li>Retrieve Size, MaxSize and Utilisation of the cache
 * </ul>
 * </p> 
 * <p>
 * This cache object uses different backend cache implementations ("cache cores") based on configuration.
 * Use sysproperty "de.innovationgate.utils.cache.core" to specify the implementation to use.
 * Cache implementations must use the interface {@link CacheCore}.
 * </p>
 */
public class Cache {
    
    /**
     * A java.lang.Integer that determines how long a cache entry may live in cache
     */
    public static final String PARAM_TIME_TO_LIVE_SECONDS = "TimeToLiveSeconds";

    /**
     * Injects an {@link CacheDisposalListener} implementation which will be notified about cache elements being removed/evicted 
     */
    public static final String PARAM_DISPOSAL_LISTENER = "DisposalListener";
    
    /**
     * Control the eviction policy of the cache. Default is LRE (least recently used). Options are dependent on the backend.
     */
    public static final String PARAM_EVICTION_POLICY = "EvictionPolicy";
    
    
    private CacheCore _core;
    
    protected Cache(CacheCore core) {
        _core = core;
    }

    /**
     * Callback function used, when the cache is no longer needed. Enables the cache to do cleanup operations.
     * @throws CacheException 
     */
    public void destroy() throws CacheException {
        _core.destroy();
    }

    /**
     * Flushes the complete cache
     */
    public void flushAll() throws CacheException {
        _core.flushAll();
    }

    /**
     * Flushes a single entry of the cache
     * @param key The cache key
     */
    public void flush(Serializable key) throws CacheException {
        _core.flushEntry(key);
    }
    
    /**
     * Flushes a single entry of the cache
     * @param key The cache key
     * @deprecated Use {@link #flush(Serializable)}
     */
    public void flushEntry(String key) throws CacheException {
        flush((Serializable) key);
    }

    /**
     * Flushes the entries of the cache that belong to the given group
     * @param group The group to be flushed
     */
    public void flushGroup(String group) throws CacheException {
        _core.flushGroup(group);
    }

    /**
     * Returns a set of the keys of the currently stored entries
     * May throw a CacheException if this is not supported by the cache implementation
     * @throws CacheException 
     */
    public Set<Serializable> getEntryKeys() throws CacheException {
        return _core.getEntryKeys();
    }

    /**
     * Returns the maximum number of entries allowed in the cache
     */
    public long getMaxSize() throws CacheException {
        return _core.getMaxSize();
    }

    /**
     * Returns the current number of entries in the cache
     */
    public long getSize() throws CacheException {
        return _core.getSize();
    }

    /**
     * Returns a percent value, describing how many cache retrieval operations actually could return a cache value.
     * This value tends against 100, never reaching it because retrievals at the beginning of cache operation, where the cache is empty, are counted too.
     * 
     * Nevertheless this is a good indicator if the caches maximum size is efficient. If the max size of the cache is reached since a long time and utilization
     * is not above, say 70%, this means that still 30% of all calls are uncached. If possible (this is, if the server's resources allow it) you should
     * consider rising the maximum cache size. 
     */
    public int getUtilisation() throws CacheException {
        return _core.getUtilisation();
    }

    /**
     * Reads a cache entry.
     * @param key The key of the cache entry
     * @return The cached object or null if the entry does not exist or is stale
     */
    public Object read(Serializable key) throws CacheException {
        return _core.readEntry(key);
    }
    
    
    

    /**
     * Reads a cache entry.
     * @param key The key of the cache entry
     * @return The cached object or null if the entry does not exist or is stale
     * @deprecated Use {@link #read(Serializable)}
     */
    public Object readEntry(String key) throws CacheException {
        return read((Serializable) key);
    }

    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     * @param group An optional group for the entry. Specify null if no group desired.
     */
    public void write(Serializable key, Object obj, String group) throws CacheException {
        _core.writeEntry(key, obj, new CacheEntryParams(group, 0));
    }
    
    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     * @param group An optional group for the entry. Specify null if no group desired.
     * @deprecated Use {@link #write(Serializable, Object, String)}
     */
    public void writeEntry(String key, Object obj, String group) throws CacheException {
        write((Serializable) key, obj, group);
    }
    
    
    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     * @param group An optional group for the entry. Specify null if no group desired.
     * @param secondsToLive Lateny of this cache entry in seconds
     */
    public void write(Serializable key, Object obj, String group, int secondsToLive) throws CacheException {
        _core.writeEntry(key, obj, new CacheEntryParams(group, secondsToLive));
    }
    
    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     * @param group An optional group for the entry. Specify null if no group desired.
     * @param secondsToLive Lateny of this cache entry in seconds
     * @deprecated Use {@link #write(Serializable, Object, String, int)}
     */
    public void writeEntry(String key, Object obj, String group, int secondsToLive) throws CacheException {
        write((Serializable) key, obj, group, secondsToLive);
    }
    
    /**
     * Writes a cache entry, being able to use all available entry params
     * @param key The key of the entry
     * @param obj The entry value
     * @param params Cache entry parameters
     */
    public void writeWithParams(Serializable key, Object obj, CacheEntryParams params) throws CacheException {
        _core.writeEntry(key, obj, params);
    }
    
    /**
     * Writes a cache entry, being able to use all available entry params
     * @param key The key of the entry
     * @param obj The entry value
     * @param params Cache entry parameters
     * @deprecated Use {@link #writeWithParams(Serializable, Object, CacheEntryParams)}
     */
    public void writeEntryWithParams(String key, Object obj, CacheEntryParams params) throws CacheException {
        writeWithParams((Serializable) key, obj, params);
    }
    
    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     */
    public void write(Serializable key, Object obj) throws CacheException {
        _core.writeEntry(key, obj, new CacheEntryParams());
    }
    
    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     * @deprecated Use {@link #write(Serializable, Object)}
     */
    public void writeEntry(String key, Object obj) throws CacheException {
        write((Serializable) key, obj);
    }

    public CacheCore getCore() {
        return _core;
    }

}
