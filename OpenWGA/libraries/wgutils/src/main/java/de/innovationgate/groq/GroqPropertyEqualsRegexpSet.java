/*
 * Created on Jan 23, 2007 from ow
 *
 */
package de.innovationgate.groq;

import java.util.regex.Pattern;

import org.apache.commons.jxpath.JXPathContext;

public class GroqPropertyEqualsRegexpSet<T> extends GroqFilterSet<T> {
    
    private String _prop;
    private Pattern _pattern;

    public GroqPropertyEqualsRegexpSet(GroqSelectionSet<T> baseSet, GroqFilterSet<T> set, String prop, String regexp) {
        super(baseSet, set);
        _prop = prop;
        _pattern = Pattern.compile(regexp);
    }

    protected boolean survivesFilter(T nextItem) {
        JXPathContext cx = JXPathContext.newContext(nextItem);
        Object propValue = cx.getValue(_prop);
        String propValueStr = String.valueOf(propValue);
        
        return (_pattern.matcher(propValueStr).matches());
    }

}
