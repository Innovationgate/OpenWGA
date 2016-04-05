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

import java.util.Collections;
import java.util.Map;

/**
 * Factory for creating and managing {@link Cache} objects
 */
public class CacheFactory {
    
    /**
     * Sysproperty determining the cache core implementation to use.
     * Specify the name of a class implementing {@link CacheCore}
     */
    public static final String SYSPROPERTY_CACHE = "de.innovationgate.utils.cache.core";

    /**
     * Creates a cache object
     * @param name A unique name for this cache to create.
     * @param capacity A maximum capacity of entries in the cache. Specify 0 for no limit.
     * @param params Optional params using constants PARAM_... on class {@link Cache} as keys. May be null.
     * @return The newly created cache object
     * @throws CacheException
     */
    public synchronized static Cache createCache(String name, int capacity, Map params) throws CacheException {
        
        if (params == null) {
            params = Collections.EMPTY_MAP;
        }
        
        // Instantiate a special cache implementation
        CacheCore cacheCore = null;
        
        String cacheImpl = System.getProperty(SYSPROPERTY_CACHE);
        if (cacheImpl != null) {
            try {
                Class clazz = Class.forName(cacheImpl);
                cacheCore = (CacheCore) clazz.newInstance();
            }
            catch (Exception e) {
                throw new CacheException("Exception instantiating cache of type " + cacheImpl, e);
            }
       }
        
        // Use default cache EHCache (B00005BDA)
        if (cacheCore == null) {
            cacheCore = new EHCacheCore();
        }
        
        // Initialize the cache
        try {
            cacheCore.init(name, capacity, params);
        }
        catch (Exception e) {
            throw new CacheException("Error initializing cache of type " + cacheCore.getClass().getName(), e);
        }
        
        // Return cache core in wrapper object
        return new Cache(cacheCore);
        
    }

}
