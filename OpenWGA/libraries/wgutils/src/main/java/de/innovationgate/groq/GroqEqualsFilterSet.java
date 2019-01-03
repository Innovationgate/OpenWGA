/*
 * Created on Dec 2, 2006 from ow
 *
 */
package de.innovationgate.groq;

public class GroqEqualsFilterSet<T> extends GroqFilterSet<T> {

    private Object _value;
    private boolean _ignoreCase;

    public GroqEqualsFilterSet(GroqSelectionSet<T> baseSet, GroqFilterSet<T> set, Object value, boolean ignoreCase) {
        super(baseSet, set);
        _value = value;
        _ignoreCase = ignoreCase;
    }

    protected boolean survivesFilter(T nextItem) {
        if (_ignoreCase) {
            return (String.valueOf(nextItem).equalsIgnoreCase(String.valueOf(_value)));
        }
        else {
            return (_value.equals(nextItem));
        }
    }
    
    

}
