package de.innovationgate.wga.services.rest.v1.types;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public class References {
    
    private Map<String,Reference> _refs = new HashMap<String, Reference>();
    
    public int size() {
        return _refs.size();
    }

    public boolean isEmpty() {
        return _refs.isEmpty();
    }

    public boolean containsKey(Object key) {
        return _refs.containsKey(key);
    }

    public Reference get(Object key) {
        return _refs.get(key);
    }

    public Reference remove(Object key) {
        return _refs.remove(key);
    }

    public void clear() {
        _refs.clear();
    }

    public Set<String> keySet() {
        return _refs.keySet();
    }

    public Collection<Reference> values() {
        return _refs.values();
    }

    public Set<Entry<String, Reference>> entrySet() {
        return _refs.entrySet();
    }

    public void add(Reference ref) {
        _refs.put(ref.getIdString(), ref);
    }

}
