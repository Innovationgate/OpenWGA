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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * A map that is intended to be used in a hierarchy of maps.
 * The HierarchyMap may have a parent map. All requests that are issued to this map
 * and which cannot be served by it (bc. the entry does not exist) are issued
 * to the parent map. So a request to a HierarchyMap may be transported upside in
 * the hierarchy if the parents are also HierarchyMaps.
 * NOTE that this - in some ways - breaks intended java.util.Map behaviour!
 */
public class HierarchyMap extends HashMap {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private Map _parent;


    /**
     * Constructs a HierarchyMap with the given parent
     * @param parent
     */
    public HierarchyMap(Map parent) {
        if (_parent != this) {
            _parent = parent;
        }
    }
    
    /**
     * Clears the current map (NOT the parent!)
     */
    public void clear() {
        super.clear();
    }

    /**
     * Looks for the key in the current and parent map
     */
    public boolean containsKey(Object key) {
       
        boolean result = super.containsKey(key);
        if (result == false && _parent != null) {
            result = _parent.containsKey(key);
        }
        return result;
        
    }

    /**
     * Looks for the value in the current and parent map
     */
    public boolean containsValue(Object value) {
        
        boolean result = super.containsValue(value);
        if (result == false && _parent != null) {
            result = _parent.containsValue(value);
        }
        return result;
        
    }

    /**
     * Returns an entry set of the current and parent map
     */
    public Set entrySet() {
        
        Set set = new HashSet();
        set.addAll(super.entrySet());
        if (_parent != null) {
            set.addAll(_parent.entrySet());
        }
        return set;
        
    }

    /**
     * Returns an entry from the current map.
     * If it does not exist there, it is returned from the parent map.
     */
    public Object get(Object key) {
        
        if (super.containsKey(key)) {
            return super.get(key);
        }
        else if (_parent != null) {
            return _parent.get(key);
        }
        else {
            return null;
        }
        
    }

    /**
     * Returns true if the current and parent map is empty.
     */
    public boolean isEmpty() {
        
        return super.isEmpty() && (_parent != null ? _parent.isEmpty() : true);
        
    }

    /**
     * Returns a key set of the current and parent map
     */
    public Set keySet() {

        Set set = new HashSet();
        set.addAll(super.keySet());
        if (_parent != null) {
            set.addAll(_parent.keySet());
        }
        return set;
        
        
    }


    /** 
     * Size of the current and parent map together.
     * This may not match the number of individual keys in both maps together
     * as there may be keys that both maps have. 
     */
    public int size() {

        return super.size() + (_parent != null ? _parent.size() : 0);
        
    }

    /**
     * Returns a value collection of the current and parent map.
     * The collection is a list that contains the current maps values
     * first, then the parent map values.
     */
    public Collection values() {

        List values = new ArrayList();
        values.add(super.values());
        if (_parent != null) {
            values.add(_parent.values());
        }
        return values;
    
    }

    /**
     * Adds a value to the current map.
     */
    public Object put(Object key, Object value) {
        return super.put(key, value);
    }

    /**
     * Adds multiple values to the current map.
     */
    public void putAll(Map m) {
        super.putAll(m);
    }

    /**
     * Removes a value from the current map.
     */
    public Object remove(Object key) {
        return super.remove(key);
    }



}
