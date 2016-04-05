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
package de.innovationgate.utils;

import java.lang.ref.WeakReference;
import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

import de.innovationgate.wga.common.Constants;

/**
 * A group of UserHashMap objects, whose currently served user can be switched at once
 * 
 * Users should perform {@link #maintenance(boolean)} regularly to remove user hash maps that are not used any more 
 */
public class UserHashMapGroup {

    private Queue<WeakReference<UserHashMap<Object,Object>>> _instances = new ConcurrentLinkedQueue<WeakReference<UserHashMap<Object,Object>>>();
    protected ThreadLocal<String> _currentUser = new ThreadLocal<String>();
    private boolean _singleUserMode = false;


    /**
     * Creates a new UserHashMap that is member of this group
     */
    public UserHashMap<Object,Object> newUserHashMap(String id) {
        UserHashMap<Object,Object> map = new UserHashMap<Object,Object>(this, id);
        _instances.add(new WeakReference<UserHashMap<Object,Object>>(map));
        return map;
    }
        
    public void maintenance(boolean cleanupOutdatedMaps) {
        Iterator<WeakReference<UserHashMap<Object,Object>>> it = _instances.iterator();
        while (it.hasNext()) {
            WeakReference<UserHashMap<Object,Object>> ref = it.next();
            UserHashMap<Object,Object> map = ref.get();
            if (map == null) {
                it.remove();
            }
            else if (cleanupOutdatedMaps) {
                map.cleanupOutdatedMaps();
            }
        }
        
    }

    /**
     * Fetches the map of the given user in all UserHashMaps
     * @param user The user
     */
    public void fetchAllMapsForUser(String user) {
        
        if (isSingleUserMode()) {
            user = Constants.ANONYMOUS_USER;
        }
        
        _currentUser.set(user);
    }
    
    /**
     * Clears the map of the given user in all UserHashMaps
     * @param user The user
     */
    public void clearAllMapsForUser(String user) {
        
        if (isSingleUserMode()) {
            user = Constants.ANONYMOUS_USER;
        }
        
        Iterator<WeakReference<UserHashMap<Object,Object>>> instancesIt = _instances.iterator();
        while (instancesIt.hasNext()) {
            WeakReference<UserHashMap<Object,Object>> mapRef = instancesIt.next();
            if (mapRef != null && mapRef.get() != null) {
                mapRef.get().getMapForUser(user).clear();
            }
        }
    }
    
    /**
     * Clears all maps for all users of this group.
     */
    public void clearAllMaps() {
        Iterator<WeakReference<UserHashMap<Object,Object>>> instancesIt = _instances.iterator();
        while (instancesIt.hasNext()) {
            WeakReference<UserHashMap<Object,Object>> mapRef = instancesIt.next();
            if (mapRef != null && mapRef.get() != null) {
                mapRef.get().clear();
            }
        }        
    }
    




    /**
     * Returns the user whose UserHashMaps are currently fetched.
     */
    public String getCurrentUser() {
        return (String) _currentUser.get();
    }

    /**
     * Returns the number of UserHashMaps in this group
     */
    public int size() {
        return _instances.size();
    }

    public boolean isSingleUserMode() {
        return _singleUserMode;
    }

    public void setSingleUserMode(boolean singleUserMode) {
        _singleUserMode = singleUserMode;
    }
}
