/*
 * Created on Jan 23, 2007 from ow
 *
 */
package de.innovationgate.groq;

import org.apache.commons.jxpath.JXPathContext;

public class GroqPropertyEqualsSet<T> extends GroqFilterSet<T> {
    
    private String _prop;
    private Object _value;

    public GroqPropertyEqualsSet(GroqSelectionSet<T> baseSet, GroqFilterSet<T> set, String prop, Object value) {
        super(baseSet, set);
        _prop = prop;
        _value = value;
    }

    protected boolean survivesFilter(T nextItem) {
        JXPathContext cx = JXPathContext.newContext(nextItem);
        Object propValue = cx.getValue(_prop);
        if (propValue == null && _value == null) {
            return true;
        }
        else if (propValue != null && propValue.equals(_value)) {
            return true;
        }
        else {
            return false;
        }
        
    }

}
