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

import org.apache.log4j.Logger;

import de.innovationgate.utils.cache.Cache;
import de.innovationgate.utils.cache.CacheException;
import de.innovationgate.utils.cache.CacheFactory;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDesignChangeEvent;
import de.innovationgate.webgate.api.WGDesignChangeListener;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wgpublisher.ManagedDBAttribute;
import de.innovationgate.wgpublisher.PublishingFile;
import de.innovationgate.wgpublisher.WGACore;

public class FileCache implements ManagedDBAttribute {
    
    public static final Logger LOG = Logger.getLogger("wga.filecache");
	
	class FileCacheItem implements Serializable {
		
		/**
         * 
         */
        private static final long serialVersionUID = 1L;
        private String fileName;
		private byte[] data;
        private long containerTime;
        
        public FileCacheItem(PublishingFile file, byte[] data, long containerTime) {
			this.fileName = file.getFileName();
			this.data = data;
			this.containerTime = containerTime;
		}
		
		/**
		 * Returns the data.
		 * @return byte[]
		 */
		public byte[] getData() {
			return data;
		}

		/**
		 * Returns the fileName.
		 * @return String
		 */
		public String getFileName() {
			return fileName;
		}

        protected long getContainerTime() {
            return containerTime;
        }

	}


	private Cache cache;
	private int maxSize = Constants.DEFAULT_FILECACHE_ENTRIES;
	private long threshold = Constants.DEFAULT_FILECACHE_THRESHOLD * 1024;
	
	public FileCache(WGDatabase db, WGACore core) throws CacheException {
        
        if (core.getModuleRegistry() != null) {
            this.maxSize = ((Integer) core.readPublisherOptionOrDefault(db, WGACore.DBATTRIB_FILECACHE_ENTRIES)).intValue();
            this.threshold = ((Integer) core.readPublisherOptionOrDefault(db, WGACore.DBATTRIB_FILECACHE_THRESHOLD)).intValue() * 1024;
        }
        
        cache = CacheFactory.createCache("FileCache_" + db.getDbReference(), maxSize, null);
	}
	
	public void putFile(PublishingFile file, byte[] data, long containerTime) {
		
        try {
            FileCacheItem item = new FileCacheItem(file, data, containerTime);
            this.cache.writeEntry(file.getCachingKey(), item);
        }
        catch (CacheException e) {
            LOG.error("Exception writing file cache" ,e);
        }
		
	}
    
    public void clear() {
        try {
            cache.flushAll();
        }
        catch (CacheException e) {
            LOG.error("Exception flushing file cache", e);
        }
    }



	
	public byte[] getFile(PublishingFile file, long cutoffTime) {
		
	    try {
            FileCacheItem item = (FileCacheItem) cache.readEntry(file.getCachingKey());
            if (item != null && item.getContainerTime() >= cutoffTime) {
                return item.getData();
            }
            else {
                return null;
            }
        }
        catch (CacheException e) {
            LOG.error("Exception reading file cache", e);
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

	/**
	 * Returns the threshold.
	 * @return long
	 */
	public long getThreshold() {
		return threshold;
	}
	
	public int getUtilisation() throws CacheException {
	    return cache.getUtilisation();
	}



    public void close() {
        try {
            cache.destroy();
        }
        catch (CacheException e) {
            LOG.error("Exception closing file cache", e);
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
