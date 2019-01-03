/*
 * Created on Dec 2, 2006 from ow
 *
 */
package de.innovationgate.groq;

import java.util.Collection;

public class GroqSelectionSet<T> extends GroqFilterSet {
    
    private Collection<T> _collection;

    public GroqSelectionSet(Collection<T> col) {
        super(null, col.iterator());
        _collection = col;
    }

    protected boolean survivesFilter(Object nextItem) {
        return true;
    }

    protected Collection<T> getCollection() {
        return _collection;
    }
}
