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
import java.util.Map;
import java.util.Set;

/**
 * Base class for a wrapper map to use with session serialization if the map contents should not
 * be serialized.
 * Implement method {@link #initWrappedMap()} to create the wrapped map implementation 
 * to use. The TransientWrappedMap will be serialized but not the wrapped map 
 * which will be replaced by an empty map of the same implementation.
 * As anonymous classes are not serializable you must build concrete subclasses for the map type
 * to use for session serialisation.
 */
public abstract class TransientWrappedMap<K,V> implements Map<K,V>, Serializable {
    
    private static final long serialVersionUID = -296137482951619606L;
    
    private transient Map<K,V> _map;
    
    public TransientWrappedMap() {
        _map = initWrappedMap();
    }
    
    protected abstract Map<K, V> initWrappedMap();

    private void writeObject(ObjectOutputStream out) throws IOException {
        out.defaultWriteObject();
    }
    
    private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
         // "pseudo-constructor"
         in.defaultReadObject();
         // create empty map
         _map = initWrappedMap();    
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
