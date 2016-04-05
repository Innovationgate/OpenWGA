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
package de.innovationgate.wgpublisher.webtml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

/**
 * wrapper to support old option behaviour for elements (pre WGA feature F0000378E) 
 *
 */
public class ElementOptionWrapper implements Map {
    
    private Map _map;
    
    public ElementOptionWrapper(Map map) {
        _map = map;
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
        return values().contains(arg0);
    }

    public Object get(Object arg0) {
        TMLOption option = (TMLOption) _map.get(arg0);
        if (option != null) {
            return option.getValue();
        }
        else {
            return null;
        }
    }

    public Object put(Object key, Object value) {
        if (!(key instanceof String)) {
            throw new IllegalArgumentException("Only key of type '" + String.class.getName() + "' supported.");
        }
        TMLOption option = new TMLOption((String)key, value, null);
        _map.put(key, option);
        return value;
    }

    public Object remove(Object arg0) {
        TMLOption option = (TMLOption) _map.remove(arg0);
        if (option != null) {
            return option.getValue();
        } else {
            return null;
        }
    }

    public void putAll(Map arg0) {
        Iterator it = arg0.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry) it.next();
            if (entry != null) {
                put(entry.getKey(), entry.getValue());
            } 
        }
    }

    public void clear() {
        _map.clear();        
    }

    public Set keySet() {
        return _map.keySet();
    }

    public Collection values() {
       List values = new ArrayList();
       Iterator it = _map.values().iterator();
       while (it.hasNext()) {
           TMLOption option = (TMLOption) it.next();
           if (option != null) {
               values.add(option.getValue());
           } else {
               values.add(null);
           }
       }
       return values;
    }

    public Set entrySet() {
        Set set = new HashSet();
        Iterator it = _map.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry) it.next();
            if (entry != null) {
                set.add(new TMLOptionEntryWrapper(entry));
            } else {
                set.add(null);
            }
        }
        return set;
    }

    private class TMLOptionEntryWrapper implements Map.Entry {
        
        Map.Entry _entry;
        
        public TMLOptionEntryWrapper(Map.Entry entry) {
            _entry = entry;
        }

        public Object getKey() {
            return _entry.getKey();
        }

        public Object getValue() {
            TMLOption option = (TMLOption) _entry.getValue();
            if (option != null) {
                return option.getValue();
            } else {
                return null;
            }
        }

        public Object setValue(Object arg0) {
            TMLOption option = new TMLOption((String)getKey(), arg0, null);
            _entry.setValue(option);
            return arg0;
        }
        
    }
}
