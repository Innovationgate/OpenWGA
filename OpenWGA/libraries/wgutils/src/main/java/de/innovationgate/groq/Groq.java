/*
 * Created on Dec 2, 2006 from ow
 *
 */
package de.innovationgate.groq;

import java.util.Arrays;
import java.util.Collection;

public abstract class Groq {
    
    public static <T> GroqSelectionSet<T> selectFrom(Collection<T> col) {
        return new GroqSelectionSet<T>(col);
    }
    
    public static GroqSelectionSet<Object> selectFrom(Object[] array) {
        return new GroqSelectionSet<Object>(Arrays.asList(array));
    }

}
