/*
 * Created on Dec 2, 2006 from ow
 *
 */
package de.innovationgate.groq;

import java.util.regex.Pattern;

public class GroqEqualsRegexpFilterSet<T> extends GroqFilterSet<T> {

    private Pattern _pattern;

    public GroqEqualsRegexpFilterSet(GroqSelectionSet<T> baseSet, GroqFilterSet<T> set, String regexp) {
        super(baseSet, set);
        _pattern = Pattern.compile(regexp);
    }

    protected boolean survivesFilter(T nextItem) {
        return (_pattern.matcher(String.valueOf(nextItem)).matches());
    }
    
    

}
