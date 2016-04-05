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

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import org.apache.log4j.Logger;

import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.ManagedDBAttribute;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult.IntegratedResource;

public class PostprocessedResourcesCache implements ManagedDBAttribute {
    
    public static final Logger LOG = Logger.getLogger("wga.pprcache");
    
    class PPRCacheKey implements Serializable {
        
        private WGDocumentKey _key;
        private Serializable _cacheQualifier;
        public PPRCacheKey(WGDocumentKey key, Serializable cacheQualifier) {
            super();
            _key = key;
            _cacheQualifier = cacheQualifier;
        }
        public WGDocumentKey getKey() {
            return _key;
        }
        public Serializable getCacheQualifier() {
            return _cacheQualifier;
        }
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((_cacheQualifier == null) ? 0 : _cacheQualifier.hashCode());
            result = prime * result + ((_key == null) ? 0 : _key.hashCode());
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
            PPRCacheKey other = (PPRCacheKey) obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (_cacheQualifier == null) {
                if (other._cacheQualifier != null)
                    return false;
            }
            else if (!_cacheQualifier.equals(other._cacheQualifier))
                return false;
            if (_key == null) {
                if (other._key != null)
                    return false;
            }
            else if (!_key.equals(other._key))
                return false;
            return true;
        }
        private PostprocessedResourcesCache getOuterType() {
            return PostprocessedResourcesCache.this;
        }
        
        
        
    }
	
	class PPRCacheItem implements Serializable {
		
        private static final long serialVersionUID = 1L;
        private WGDocumentKey _key;
		private PostProcessResult _result;
        
        public PPRCacheItem(WGDocumentKey key, PostProcessResult result) {
			_key = key;
			_result = result;
		}

        public WGDocumentKey getKey() {
            return _key;
        }

        public PostProcessResult getResult() {
            return _result;
        }

	}


	private Cache cache;
	private int maxSize = Constants.DEFAULT_PPRCACHE_ENTRIES;
	
	public PostprocessedResourcesCache(WGDatabase db, WGACore core) throws CacheException {
        
        if (core.getModuleRegistry() != null) {
            this.maxSize = ((Integer) core.readPublisherOptionOrDefault(db, WGACore.DBATTRIB_PPRCACHE_ENTRIES)).intValue();
        }
        
        cache = CacheFactory.createCache("PPRCache_" + db.getDbReference(), maxSize, null);
	}
	
	public void putResource(PostProcessData data, PostProcessResult result) {
		
        try {
            PPRCacheItem item = new PPRCacheItem(data.getDocument().getDocumentKeyObj(), result);
            this.cache.write(new PPRCacheKey(data.getDocument().getDocumentKeyObj(), data.getCacheQualifier()), item);
        }
        catch (Exception e) {
            LOG.error("Exception writing PPR cache" ,e);
        }
		
	}
    
    public void clear() {
        try {
            cache.flushAll();
        }
        catch (CacheException e) {
            LOG.error("Exception flushing PPR cache", e);
        }
    }
	
	public PostProcessResult getResource(WGA wga, PostProcessData data) throws WGException {
		
	    try {
            PPRCacheItem item = (PPRCacheItem) cache.read(new PPRCacheKey(data.getDocument().getDocumentKeyObj(), data.getCacheQualifier()));
            if (item == null) {
                return null;
            }
            
            // Check state of integrated resources
            for (IntegratedResource res : item.getResult().getIntegratedResources()) {
                WGDatabase database = wga.db(res.getDbKey());
                if (!database.isSessionOpen()) {
                    return null;
                }
                
                WGDesignDocument resDoc = (WGDesignDocument) database.getDocumentByKey(res.getKey());
                if (resDoc == null || resDoc.getLastModified().compareTo(res.getLastModified()) > 0) {
                    return null;
                }
            }
            
            return item.getResult();
        }
        catch (CacheException e) {
            LOG.error("Exception reading PPR cache", e);
            return null;
        }
		
	}

	/**
	 * @see de.innovationgate.webgate.api.WGDatabaseEventListener#isTemporary()
	 */
	public boolean isTemporary() {
		return false;
	}

	/**
	 * Returns the maxSize.
	 * @return long
	 */
	public long getMaxSize() {
		return maxSize;
	}
	
	public int getUtilisation() throws CacheException {
	    return cache.getUtilisation();
	}


    public void close() {
        try {
            cache.destroy();
        }
        catch (CacheException e) {
            LOG.error("Exception closing PPR cache", e);
        }
    }
    
    public long getActualSize() {
        try {
            return cache.getSize();
        }
        catch (CacheException e) {
            return 0;
        }
    }
    

}
