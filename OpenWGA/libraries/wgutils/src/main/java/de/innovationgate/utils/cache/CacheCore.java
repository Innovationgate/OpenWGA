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
 * Interface for cache core implementations for {@link Cache} 
 */
public interface CacheCore {
    
    /**
     * Initialisation method. Being called immediately after creation and before using the cache.
     * @param name An application-wide unique name of the cache (if the implementation needs this)
     * @param capacity Maximum capacity (entries) of the cache
     * @param params Optional parameters. Keys are Constants PARAM_... at class {@link Cache}
     * @throws Exception
     */
    public void init(String name, int capacity, Map<String,Object> params) throws CacheException;
    
    /**
     * Callback function used, when the cache is no longer needed. Enables the cache to do cleanup operations.
     * @throws CacheException 
     */
    public void destroy() throws CacheException;

    /**
     * Writes a cache entry
     * @param key The key of the entry
     * @param obj The entry value
     * @param params Parameters for the cache entry to write
     */
    public void writeEntry(Serializable key, Object obj, CacheEntryParams params) throws CacheException;;
    
    /**
     * Reads a cache entry.
     * @param key The key of the cache entry
     * @return The cached object or null if the entry does not exist or is stale
     */
    public Object readEntry(Serializable key) throws CacheException;
    
    /**
     * Mark an existing cache entry as updated, so the eviction policy regards it as new
     * This method does nothing if there is no entry under the given key.
     * @param key The key of the cache entry
     * @throws CacheException
     */
    public void touchEntry(Serializable key) throws CacheException;
    
    /**
     * Flushes a single entry of the cache
     * @param key The cache key
     */
    public void flushEntry(Serializable key) throws CacheException;;

    /**
     * Flushes the entries of the cache that belong to the given group
     * @param group The group to be flushed
     */
    public void flushGroup(String group) throws CacheException;;
    
    /**
     * Flushes the complete cache
     */
    public void flushAll() throws CacheException;;
    
    /**
     * Returns the current number of entries in the cache
     */
    public long getSize() throws CacheException;;
    /**
     * Returns the maximum number of entries allowed in the cache
     */
    public long getMaxSize() throws CacheException;;
    
    /**
     * Returns a percent value, describing how many cache retrieval operations actually could return a cache value.
     * This value tends against 100, never reaching it because retrievals at the beginning of cache operation, where the cache is empty, are counted too.
     * 
     * Nevertheless this is a good indicator if the caches maximum size is efficient. If the max size of the cache is reached since a long time and utilization
     * is not above, say 70%, this means that still 30% of all calls are uncached. If possible (this is, if the server's resources allow it) you should
     * consider rising the maximum cache size. 
     */
    public int getUtilisation() throws CacheException;;
    
    
    /**
     * Returns a set of the keys of the currently stored entries
     * May throw a CacheException if this is not supported
     * @throws CacheException 
     */
    public Set<Serializable> getEntryKeys() throws CacheException;

}
