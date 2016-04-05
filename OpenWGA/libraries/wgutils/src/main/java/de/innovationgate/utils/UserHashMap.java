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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.collections.map.LRUMap;

import de.innovationgate.wga.common.Constants;


/**
 * A Map that contains different maps for different users identified by a unique user name.
 * <p>
 * The UserHashMap may be regarded a "ThreadLocal" Map where every thread may have it's own private map.
 * Main difference to ThreadLoval: Every thread may choose from a list of available maps where each map 
 * belongs to a single user.
 * </p>
 * <p>
 * UserHashMaps belong to {@link UserHashMapGroup} objects. A UserHashMapGroup collects UserHashMaps that
 * should switch to the same users map at the same time. The UserHashMapGroup hosts methods for creating
 * new UserHashMaps ({@link UserHashMapGroup#newUserHashMap(String)}) and for switching all of the maps
 * in the group to a specified user ({@link UserHashMapGroup#fetchAllMapsForUser(String)}).
 * </p>
 */
public class UserHashMap<KeyType,ValueType> implements Map<KeyType,ValueType> {
    
    class SingleUserMap implements Map<KeyType,ValueType> {
        
        private Map<KeyType,ValueType> _baseMap;
        private long _lifetime = Long.MIN_VALUE;
        
        public SingleUserMap(long latency) {
            
            _baseMap = new ConcurrentHashMap<KeyType,ValueType>();
            
            if (latency != 0) {
                _lifetime = System.currentTimeMillis() + latency;
            }
            
        }
        
        public boolean isOutdated() {
            return _lifetime != Long.MIN_VALUE && System.currentTimeMillis() > _lifetime;
        }

        public void clear() {
            _baseMap.clear();
        }

        public boolean containsKey(Object key) {
            return _baseMap.containsKey(key);
        }

        public boolean containsValue(Object value) {
            return _baseMap.containsValue(value);
        }

        public Set<Map.Entry<KeyType,ValueType>> entrySet() {
            return _baseMap.entrySet();
        }

        public boolean equals(Object o) {
            return _baseMap.equals(o);
        }

        public ValueType get(Object key) {
            return _baseMap.get(key);
        }

        public int hashCode() {
            return _baseMap.hashCode();
        }

        public boolean isEmpty() {
            return _baseMap.isEmpty();
        }

        public Set<KeyType> keySet() {
            return _baseMap.keySet();
        }

        public ValueType put(KeyType key, ValueType value) {
            return _baseMap.put(key, value);
        }

        public void putAll(Map<? extends KeyType, ? extends ValueType> t) {
            _baseMap.putAll(t);
        }

        public ValueType remove(Object key) {
            return _baseMap.remove(key);
        }

        public int size() {
            return _baseMap.size();
        }

        public Collection<ValueType> values() {
            return _baseMap.values();
        }
        
    }

	private static final String SYSPROP_MAX_CONCURRENT_PRIVATE_USERCACHES = "de.innovationgate.wga.maxConcurrentPrivateUserCaches";

	private static final int DEFAULT_MAX_CONCURRENT_PRIVATE_USERCACHES = 250;
	
	private UserHashMapGroup _group;
    private long _mapLatency = 0;
    
	private Map<String,SingleUserMap> _userMaps = null;
	private SingleUserMap _noLatencyAnonymousMap = new SingleUserMap(0);
	
    private String _id;
    
    @SuppressWarnings("unchecked")
    protected UserHashMap(UserHashMapGroup mapGroup, String id) {
        _id = id;
		_group = mapGroup;
		int max = DEFAULT_MAX_CONCURRENT_PRIVATE_USERCACHES;		
		try {
			max = Integer.parseInt(System.getProperty(SYSPROP_MAX_CONCURRENT_PRIVATE_USERCACHES, Integer.toString(DEFAULT_MAX_CONCURRENT_PRIVATE_USERCACHES)));
		} catch (NumberFormatException e) {
		}		
		_userMaps = Collections.synchronizedMap(new LRUMap(max));
	}
	


	/**
     * Returns the map for a specific user without switching the methods of the normal map interface to this map.
	 * @param user
	 */
	public Map<KeyType,ValueType> getMapForUser(String user) {
        
	    // Fast lockless mode for anonymous user without latency
	    if (_group.isSingleUserMode() || (Constants.ANONYMOUS_USER.equals(user) && _mapLatency == 0)) {
	        return _noLatencyAnonymousMap;
	    }
	    
	    // synchronize on our application specific lockId String
	    // previously we used commons locking here but bc. of the many different lock objects this leads to
	    // heavy memory consumption
	    synchronized (getLockId(user).intern()) {           
    		SingleUserMap userMap = (SingleUserMap) this._userMaps.get(user);
    		if (userMap == null || userMap.isOutdated()) {
    			userMap = createSingleUserMap();
   			    this._userMaps.put(user, userMap);
    		}
    		return userMap;        
		}
	}
	


    private SingleUserMap createSingleUserMap() {
        return new SingleUserMap(_mapLatency);
    }
	
	/**
     * Builds a lock id for the method getMapForUser.
     * We want to synchronize this method for all calls on this map for the same user.
     * Bc. we use String.internal() for synchronization we need to qualify the lock id with 'class name' + 'hash code' of the current group
	 * @param user
	 * @return The lock id
	 */
	private String getLockId(String user) {
        return _group.getClass().getName() + "." + _group.hashCode() + "." +_id + "///" + user;
    }



    /**
	 * Returns the currently served map as a single map.
	 */
	public Map<KeyType,ValueType> getCurrentUserMap() {
		return getMapForUser(_group.getCurrentUser());
	}
	
	public ValueType get(Object key) {
		return this.getCurrentUserMap().get(key);
	}
	
	public ValueType put(KeyType key, ValueType value) {
		return this.getCurrentUserMap().put(key, value);
	}
	
	/**
	 * @see java.util.Map#containsKey(Object)
	 */
	public boolean containsKey(Object key) {
		return this.getCurrentUserMap().containsKey(key);
	}
	
	/**
     * Clears all entries from all users maps. For clearing the entries for a single user see clearUser().
	 */
	public void clear() {
		
		synchronized (this._userMaps) {
			Iterator<SingleUserMap> maps = this._userMaps.values().iterator();
			while (maps.hasNext()) {
				maps.next().clear();
			}
		}
		_noLatencyAnonymousMap.clear();
	}
	
	/**
	 * Clears the entries of the current users map.
	 */
	public void clearUser() {
	    getCurrentUserMap().clear();
	}

	/**
	 * @see java.util.Map#containsValue(Object)
	 */
	@Override
	public boolean containsValue(Object value) {
		return this.getCurrentUserMap().containsValue(value);
	}

	/**
	 * @see java.util.Map#entrySet()
	 */
	public Set<Map.Entry<KeyType,ValueType>> entrySet() {
		return this.getCurrentUserMap().entrySet();
	}

	/**
	 * @see java.util.Map#isEmpty()
	 */
	public boolean isEmpty() {
		return this.getCurrentUserMap().isEmpty();
	}

	/**
	 * @see java.util.Map#keySet()
	 */
	public Set<KeyType> keySet() {
		return this.getCurrentUserMap().keySet();
	}

	/**
	 * @see java.util.Map#putAll(Map)
	 */
	public void putAll(Map<? extends KeyType, ? extends ValueType> t) {
		this.getCurrentUserMap().putAll(t);
	}
	
	/**
     * Removes a map key from all users maps
     * If parameter value is given it only removes those keys that map equal values.
	 * @param key The key to remove
	 * @param value A value that must match the value mapped to key if the key should be removed. Specify null to remove the key in any case.
	 */
	public void removeFromAllUsers(KeyType key, ValueType value) {

	    // Retrieve user names first, the only synchronized operation
	    Set<String> userNames = new HashSet<String>();
	    synchronized (this._userMaps) {
	        userNames.addAll(this._userMaps.keySet());
	    }

	    // Retrieve user maps and remove the key
	    for (String name : userNames) {
	        UserHashMap<KeyType,ValueType>.SingleUserMap userMap = _userMaps.get(name);
	        if (userMap != null) {
	            removeKey(userMap, key, value);
	        }
	    }

	    // Remove the key from the anonymous map
		removeKey(_noLatencyAnonymousMap, key, value);
		
	}
	
	   /**
     * Removes a map key from all users maps
     * @param key The key to remove
     */
	public void removeFromAllUsers(KeyType key) {
	    removeFromAllUsers(key, null);
	}



    private void removeKey(SingleUserMap map, Object key, Object value) {
        if (value == null) {
            map.remove(key);
        }
        else {
            Object oldValue = map.get(key);
            if (oldValue != null && oldValue.equals(value)) {
                map.remove(key);
            }
        }
    }

	/**
	 * @see java.util.Map#remove(Object)
	 */
	public ValueType remove(Object key) {
		return this.getCurrentUserMap().remove(key);
	}

	/**
	 * @see java.util.Map#size()
	 */
	public int size() {
		return this.getCurrentUserMap().size();
	}

	/**
	 * @see java.util.Map#values()
	 */
	public Collection<ValueType> values() {
		return this.getCurrentUserMap().values();
	}
	
	/**
	 * Returns the number of users who have own hashmaps
	 */
	public int getUserCount() {
		int size = this._userMaps.size();
		
		// Without latency we use a separate map for the anonymous user that is not in _userMaps
		if (_mapLatency == 0) {
		    size++;
		}
		
		return size;
	}
	
	
	/**
	 * Returns the parent UserHashMapGroup object
	 */
	public UserHashMapGroup getGroup() {
		return _group;
	}

	/**
	 * Returns a map of user's hash maps. The keys are the user names, the values are the single maps themselves.
	 */
	public Map<String,SingleUserMap> getUserMaps() {
		Map<String,SingleUserMap> map = new HashMap<String,SingleUserMap>(_userMaps);
		
	      // Without latency we use a separate map for the anonymous user that is not in _userMaps
		if (_mapLatency == 0) {
		    map.put(Constants.ANONYMOUS_USER, _noLatencyAnonymousMap);
		}
		
		return map;
	}

    /**
     * Returns the map latency in milliseconds.
     */
    public long getMapLatency() {
        return _mapLatency;
    }

    /**
     * Sets the latency of the map. 
     * If a latency other as 0 (the default) is specified the map will have a lifetime of the given latency in milliseconds.
     * After that time the map is dicarded and a new empty one is created. This is helpful to cache user-specific data that
     * should be discarded after some time.
     * @param mapLatency
     */
    public void setMapLatency(long mapLatency) {
        this._mapLatency = mapLatency;
    }



    /**
     * Removes all user maps that are outdated.
     */
    protected void cleanupOutdatedMaps() {
        
        synchronized(_userMaps) {
            Iterator<SingleUserMap> userMapsIt = _userMaps.values().iterator();
            while (userMapsIt.hasNext()) {
                SingleUserMap userMap = userMapsIt.next();
                if (userMap.isOutdated()) {
                    userMapsIt.remove();
                }
            }
        }
        
        
    }

}
