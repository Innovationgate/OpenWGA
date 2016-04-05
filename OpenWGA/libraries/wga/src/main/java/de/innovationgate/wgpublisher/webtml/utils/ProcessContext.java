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

package de.innovationgate.wgpublisher.webtml.utils;

import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import de.innovationgate.wga.common.CodeCompletion;

public abstract class ProcessContext {

    private Map<String, Object> _attributes = new ConcurrentHashMap<String, Object>();
    protected Date _created;
    protected String _processId;
    protected ProcessContextRegistration _registration;
    protected String _parentId;
    public ProcessContext(String processid, String parentId, ProcessContextRegistration registration) {
        _processId = processid;
        _parentId = parentId;
        _created = new Date();
        _registration = registration;
        if (_registration != null) {
            _registration.setProcessContext(_processId, this);
        }
    }

    /**
     * Remove this process context
     */
    @CodeCompletion
    public void kill() {
        if (_registration != null) {
            _registration.removeProcessContext(_processId);
        }
    }

    @CodeCompletion
    public void clear() {
        _attributes.clear();
    }

    /**
     * Reset all data 
     */
    protected void reset() {
        clear();
    }

    @CodeCompletion
    public boolean containsKey(Object key) {
        return _attributes.containsKey(key);
    }

    @CodeCompletion
    public boolean containsValue(Object value) {
        return _attributes.containsValue(value);
    }

    @CodeCompletion
    public Set<Entry<String, Object>> entrySet() {
        return _attributes.entrySet();
    }

    @CodeCompletion
    public boolean equals(Object o) {
        return _attributes.equals(o);
    }

    @CodeCompletion
    public Object get(Object key) {
        return _attributes.get(key);
    }

    @CodeCompletion
    public int hashCode() {
        return _attributes.hashCode();
    }

    @CodeCompletion
    public boolean isEmpty() {
        return _attributes.isEmpty();
    }

    @CodeCompletion
    public Set<String> keySet() {
        return _attributes.keySet();
    }

    @CodeCompletion
    public Object put(String key, Object value) {
        return _attributes.put(key, value);
    }

    @CodeCompletion
    public void putAll(Map<? extends String,? extends Object> t) {
        _attributes.putAll(t);
    }

    @CodeCompletion
    public Object remove(Object key) {
        return _attributes.remove(key);
    }

    @CodeCompletion
    public int size() {
        return _attributes.size();
    }

    @CodeCompletion
    public Collection<Object> values() {
        return _attributes.values();
    }

    /**
     * Returns the creation time of this process context
     */
    @CodeCompletion
    public Date getCreated() {
        return _created;
    }

    /**
     * Returns the unique process id
     */
    @CodeCompletion
    public String getProcessId() {
        return _processId;
    }

    /**
     * Returns the ID of the parent entity
     */
    protected String getParentId() {
        return _parentId;
    }

}