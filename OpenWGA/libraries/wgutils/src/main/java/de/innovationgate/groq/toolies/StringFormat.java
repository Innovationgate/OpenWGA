/*
 * Created on Feb 2, 2007 from ow
 *
 */
package de.innovationgate.groq.toolies;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;

public abstract class StringFormat extends Format {

    public StringBuffer format(Object obj, StringBuffer toAppendTo, FieldPosition pos) {
        String result = formatString(String.valueOf(obj));
        toAppendTo.append(result);
        return toAppendTo;
    }

    protected abstract String formatString(String string);

    public Object parseObject(String source, ParsePosition pos) {
        throw new UnsupportedOperationException("StringFormat cannot parse objects");
    }

}
