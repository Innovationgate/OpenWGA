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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * wrapper for a map to use with session serialization
 * the originalmap is a transient field and is therefore not serialized
 * if the object is deserialized it behaves like an empty map
 * @deprecated Use {@link TransientWrappedMap} because that allows you to define the
 * wrapped map implementation in all cases. This one uses always simple unsynchronized
 * {@link HashMap}s when the session is activated on another node. 
 *
 */
public class TransientMap<K,V> implements Map<K,V>, Serializable {
    
    private static final long serialVersionUID = -296137482951619606L;
    
    private transient Map<K,V> _map;
    
    public TransientMap(Map<K,V> originalMap) {
        _map = originalMap;
    }
    
    private void writeObject(ObjectOutputStream out) throws IOException {
        out.defaultWriteObject();
    }
    
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
         // "pseudo-constructor"
         in.defaultReadObject();
         // create empty map
         _map = new HashMap<K, V>();    
    }    

    public int size() {
        return _map.size();
    }

    public boolean isEmpty() {
        return _map.isEmpty();
    }

    public boolean containsKey(Object arg0) {
        return _map.containsKey(arg0);
    }

    public boolean containsValue(Object arg0) {
        return _map.containsValue(arg0);
    }

    public V get(Object arg0) {
        return _map.get(arg0);
    }

    public V put(K arg0, V arg1) {
        return _map.put(arg0, arg1);
    }

    public V remove(Object arg0) {
       return _map.remove(arg0);               
    }

    public void putAll(Map<? extends K, ? extends V> arg0) {
        _map.putAll(arg0);
    }

    public void clear() {
        _map.clear();
    }

    public Set<K> keySet() {
        return _map.keySet();
    }

    public Collection<V> values() {
        return _map.values();
    }

    public Set<Map.Entry<K, V>> entrySet() {
       return _map.entrySet();
    }

}
