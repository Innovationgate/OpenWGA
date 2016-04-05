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

package de.innovationgate.utils;

import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A concurrent hash map whose values are stored in weak references. So unlike the regular {@link ConcurrentHashMap}
 * this map may actually contain null values, as the reference value either be null from the start or after the object
 * was GCd. In order to cleanup entries of GCd values to keep the map size small {@link #maintenance()} should be called
 * on a schedule, which will remove empty entries. Some map operations are currently not implemented and cannot be used:
 * <ul>
 * <li>{@link #entrySet()}
 * <li>{@link #values()}
 * <li>{@link #containsValue(Object)}
 * <li>{@link #contains(Object)}
 * </ul> 
 * @param <K>
 * @param <V>
 */
public class ConcurrentWeakHashMap<K,V> implements Map<K, V> {
    
    private ConcurrentHashMap<K, WeakReference<V>> _backendMap = new ConcurrentHashMap<>();

    public boolean equals(Object o) {
        return _backendMap.equals(o);
    }

    public int hashCode() {
        return _backendMap.hashCode();
    }

    public String toString() {
        return _backendMap.toString();
    }

    public boolean isEmpty() {
        return _backendMap.isEmpty();
    }

    public int size() {
        return _backendMap.size();
    }

    public V get(Object key) {
        return unpack(_backendMap.get(key));
    }

    private V unpack(WeakReference<V> weakReference) {
        if (weakReference == null) {
            return null;
        }
        return weakReference.get();
    }

    public boolean containsKey(Object key) {
        return _backendMap.containsKey(key);
    }

    public boolean containsValue(Object value) {
        throw new UnsupportedOperationException();
    }

    public boolean contains(Object value) {
        throw new UnsupportedOperationException();
    }

    public V put(K key, V value) {
        return unpack(_backendMap.put(key, new WeakReference<V>(value)));
    }

    public WeakReference<V> putIfAbsent(K key, WeakReference<V> value) {
        return _backendMap.putIfAbsent(key, value);
    }

    public void putAll(Map<? extends K, ? extends V> m) {
        
        for (Map.Entry<? extends K, ? extends V> entry : m.entrySet()) {
            put(entry.getKey(), entry.getValue());
        }
        
    }

    public V remove(Object key) {
        return unpack(_backendMap.remove(key));
    }

    public void clear() {
        _backendMap.clear();
    }

    public Set<K> keySet() {
        return _backendMap.keySet();
    }

    public Collection<V> values() {
        throw new UnsupportedOperationException();
    }

    public Set<java.util.Map.Entry<K, V>> entrySet() {
        throw new UnsupportedOperationException();
    }
    
    public void maintenance() {
        Iterator<Map.Entry<K,WeakReference<V>>> it = _backendMap.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry<K,WeakReference<V>> entry = it.next();
            if (entry.getValue().get() == null) {
                it.remove();
            }
        }
    }


}
