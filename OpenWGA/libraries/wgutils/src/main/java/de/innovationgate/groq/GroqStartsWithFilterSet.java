/*
 * Created on Dec 2, 2006 from ow
 *
 */
package de.innovationgate.groq;

import java.util.Collection;
import java.util.Map;

public class GroqStartsWithFilterSet<T> extends GroqFilterSet<T> {

    private String _start;

    public GroqStartsWithFilterSet(GroqSelectionSet<T> baseSet, GroqFilterSet<T> set, String start) {
        super(baseSet, set);
        _start = start;
    }

    protected boolean survivesFilter(T nextItem) {
        String stringValue = String.valueOf(nextItem);
        return stringValue.startsWith(_start);
            
                
    }
    
    

}
