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

package de.innovationgate.utils.cache;

/**
 * Parameters for writing a cache entry
 */
public class CacheEntryParams {
    
    private String _group;
    private int _secondsToLive = 0;

    /**
     * Constructs a cache param without group and latency, not pinned
     */
    public CacheEntryParams() {
    }
    
    /**
     * Constructs a cache param with group and latency, not pinned
     * @param group Cache group
     * @param secondsToLive Cache entry latency in seconds
     */
    public CacheEntryParams(String group, int secondsToLive) {
        _group = group;
        _secondsToLive = secondsToLive;
    }
    
    /**
     * Returns the cache group
     */
    public String getGroup() {
        return _group;
    }
    /**
     * Sets the cache group
     */
    public void setGroup(String group) {
        _group = group;
    }
    /**
     * Returns the cache entry latency in seconds
     */
    public int getSecondsToLive() {
        return _secondsToLive;
    }
    /**
     * Sets the cache entry latency in seconds
     */
    public void setSecondsToLive(int secondsToLive) {
        _secondsToLive = secondsToLive;
    }

}
