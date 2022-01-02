/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.utils;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.MalformedURLException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.CharacterIterator;
import java.text.DecimalFormat;
import java.text.Normalizer;
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.swing.JList;
import javax.swing.ListModel;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.ParserDelegator;

import org.apache.commons.collections.MapIterator;
import org.apache.commons.collections.iterators.EnumerationIterator;
import org.apache.commons.io.output.NullOutputStream;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.SystemUtils;
import org.apache.commons.vfs2.Capability;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileType;
import org.apache.log4j.Logger;
import org.dom4j.Attribute;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentFactory;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.io.OutputFormat;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;


/**
 * Container object for diverse static utility methods.
 */
public abstract class WGUtils {
    
    
    private static final Pattern PATTERN_ENCODE_SCRIPT_END = Pattern.compile("</script[^>]*>", Pattern.CASE_INSENSITIVE);

    private static final Pattern PATTERN_ENCODE_SCRIPT_START = Pattern.compile("<script[^>]*>", Pattern.CASE_INSENSITIVE);
    

    private static class PatchedGZIPInputStream extends java.util.zip.GZIPInputStream {

        public PatchedGZIPInputStream(InputStream in, int size) throws IOException {
            super(in, size);
        }

        public PatchedGZIPInputStream(InputStream in) throws IOException {
            super(in);
        }

        public int read(byte buf[], int off, int len) throws IOException {
            try {
                return super.read(buf, off, len);
            }
            catch (IOException e) {
                if (e.getMessage().indexOf("Corrupt GZIP trailer") != -1) {
                    return -1;
                }
                else {
                    throw e;
                }
            }
        }
    }
    
    /**
     * Name of a directory link file, used by {@link #createDirLink(File, String)} and {@link #resolveDirLink(File)}
     */
    public static final String DIRLINK_FILE = "dirlink.xml";

    private static final String ASN1_ESCAPE_CHARS = ",+\"\\<>;";
    private static final String ASN1_FIRSTCHAR_ESCAPE_CHARS = " #";
    
    public static final Pattern JS_IDENTIFIER_CHARS = Pattern.compile("[A-Za-z0-9_$]");
    public static final Pattern JS_IDENTIFIER_FIRSTCHARS = Pattern.compile("[A-Za-z_$]");
    
    public static final Pattern ILLEGAL_HTML_CHARS_PATTERN = Pattern.compile("[\\x00-\\x08\\x0B-\\x0C\\x0E-\\x1F\\x7F]");
    
    private static ObjectOutputStream _serialisation_test_stream;
    static {
        try {
            _serialisation_test_stream = new ObjectOutputStream(NullOutputStream.NULL_OUTPUT_STREAM);
        }
        catch (IOException e) {
            Logger.getLogger("wga.utils").error("Exception initializing serialisation test stream", e);
        }
    }
    
    
    private WGUtils() {
    }
    
   
    private static Method _threadLocalRemoveMethod = null;
    static {
        try {
            _threadLocalRemoveMethod = ThreadLocal.class.getMethod("remove", new Class[] {});
        }
        catch (Exception e) {
        }
    }

    static class PlainTextParserCallback extends javax.swing.text.html.HTMLEditorKit.ParserCallback {

        private boolean _ignoreWhitespace = false;

        private String _divider = " ";
        private boolean _formatted=false;    
        private boolean dividerAdded=false;

        private StringBuffer _text = new StringBuffer();

        public PlainTextParserCallback(boolean ignoreWhitespace, String divider) {
            _ignoreWhitespace = ignoreWhitespace;
            _divider = divider;            
        }

        public PlainTextParserCallback(boolean ignoreWhitespace, String divider, boolean formatted) {
            _ignoreWhitespace = ignoreWhitespace;
            _divider = divider;
            _formatted = formatted;
        }

        private boolean insertLf(HTML.Tag t){
        	return (t == HTML.Tag.P
                || t == HTML.Tag.H1
                || t == HTML.Tag.H2
                || t == HTML.Tag.H3
                || t == HTML.Tag.H4
                || t == HTML.Tag.H5
                || t == HTML.Tag.H6
                || t == HTML.Tag.BLOCKQUOTE
                || t == HTML.Tag.UL
                || t == HTML.Tag.OL
                || t == HTML.Tag.LI);
        }
        
        public void handleSimpleTag(HTML.Tag t, MutableAttributeSet a, int pos) {
        	if(t == HTML.Tag.BR){
        		_text.append(_divider);
        		dividerAdded=true;
        	}
        }
        
        public void handleStartTag(HTML.Tag t, MutableAttributeSet a, int pos) {
            if(!getText().isEmpty() && !dividerAdded && insertLf(t)){
            	_text.append(_divider);
            	dividerAdded = true;
            }
        	if(_formatted && t == HTML.Tag.LI)
        		_text.append("- ");
        }
        public void handleEndTag(HTML.Tag t, int pos) {
            if(insertLf(t) && !dividerAdded){
            	_text.append(_divider);
            	dividerAdded=true;
            }        	
        }
        
        public void handleText(char[] data, int pos) {

            String newText = new String(data);
            if (this._ignoreWhitespace == true && newText.trim().equals("")) {
                return;
            }

            _text.append(newText);
            dividerAdded = false;
        }

        public String getText() {
            return _text.toString();
        }

        public void resetText() {
            _text = new StringBuffer();
        }
    }

    @SuppressWarnings("rawtypes")
    static class PropertyComparator implements Comparator {

        private String _prop;

        public PropertyComparator(String prop) {
            _prop = prop;
        }

        /*
         * (Kein Javadoc)
         * 
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @SuppressWarnings("unchecked")
        public int compare(Object arg0, Object arg1) {

            JXPathContext con0 = JXPathContext.newContext(arg0);
            JXPathContext con1 = JXPathContext.newContext(arg1);

            Comparable val0 = (Comparable) con0.getValue(_prop);
            Comparable val1 = (Comparable) con1.getValue(_prop);
            return val0.compareTo(val1);

        }
    }

    /**
     * A full german date/time format: dd.MM.yyyy HH:mm:ss SSS
     */
    public static final java.text.DateFormat DATEFORMAT_FULL = new java.text.SimpleDateFormat("dd.MM.yyyy HH:mm:ss SSS");

    /**
     * A normal german date format: dd.MM.yyyy
     */
    public static final java.text.DateFormat DATEFORMAT_STANDARD = new java.text.SimpleDateFormat("dd.MM.yyyy");
    
    /**
     * A normal decimal format including a thousands separator character
     */
    public static final java.text.DecimalFormat DECIMALFORMAT_STANDARD = new java.text.DecimalFormat("#,##0");
    
    /**
     * A decimal format for programmatic uses, guaranteeing to have no thousands separator and also no "default" fraction digits.
     * Existing fractions are given out up to the tenth digit
     */
    public static final java.text.DecimalFormat DECIMALFORMAT_SYSTEM = new java.text.DecimalFormat("#0.##########");

    /**
     * A normal german time format: HH:mm:ss
     */
    public static final java.text.DateFormat TIMEFORMAT_STANDARD = new java.text.SimpleDateFormat("HH:mm:ss");

    /**
     * For {@link #encodeHTML(String, boolean, int)}, to let linefeeds be converted to <br> tags
     */
    public static final int LINEFEEDS_CONVERT_TO_BR = 1;
    
    /**
     * For {@link #encodeHTML(String, boolean, int)}, to let linefeeds be kept untouched
     */
    public static final int LINEFEEDS_KEEP = 2;

    /**
     * For {@link #encodeHTML(String, boolean, int)}, to let linefeeds be removed from the output
     */
    public static final int LINEFEEDS_REMOVE = 3;

    /**
     * Deletes the given file or the contents of a folder.
     * If the file is a directory this method will
     * recurse through the contents of the directory and delete any file and
     * directory within, then finally will delete the initial directory (if param "deleteFileItself" is true). 
     * This can be used to delete directory trees of any size and depth (yes, this is a warning).
     * 
     * @param file
     *            The file or folder to delete/clear
     * @param deleteFileItself
     *            Setting this to false will only delete the files in the given folder (if it is one), but not the folder itself. If it is no folder this is a noop.
     */
    public static void delTree(File file, boolean deleteFileItself) {

        if (file.isDirectory()) {
            File[] subFiles = file.listFiles();
            for (int i = 0; i < subFiles.length; i++) {
                WGUtils.delTree(subFiles[i], true);
            }
        }
        if (deleteFileItself) {
            boolean fileDeleted = false;
            int tryCounter = 1;
            while (!fileDeleted) {
                fileDeleted = file.delete();
                if (!fileDeleted) {
                    tryCounter++;
                    if (tryCounter > 5) {
                        throw new IllegalStateException("Undeletable file " + file.getAbsolutePath());
                    }
                    
                    try {
                        Thread.sleep(100);
                    }
                    catch (InterruptedException e) {
                    }
                }
            }
        }
    }
    
    /**
     * Deletes the given file. If the file is a directory this method will
     * recurse through the contents of the directory and delete any file and
     * directory within, then finally will delete the initial directory. This
     * can be used to delete directory trees of any size and depth.
     * 
     * @param file
     *            The file to delete.
     */
    public static void delTree(File file) {
        delTree(file, true);
    }

    /**
     * Determines if the given object is any kind of collection
     * 
     * @param obj
     *            The object to test
     * @return true, if it is a collection.
     */
    public static boolean isCollection(Object obj) {

        return (obj != null && java.util.Collection.class.isAssignableFrom(obj.getClass()));
    }

    /**
     * Serializes a collection to a single String using the given divider.
     * Collections serialized by this method can be deserialized again by
     * {@link de.innovationgate.utils#WGUtils.deserializeCollection deserializeCollection}.
     * This version takes a special object formatter object, that formats the
     * collection elements before they are added to the result string. The
     * formatter can be used to convert non-string values in the collection to
     * strings by a special method.
     * 
     * @param col
     *            The collection
     * @param divider
     *            The divider
     * @param formatter
     *            The formatter
     * @param includeNulls
     *            Specify null to treat null values in the collection as emtpy strings, false to omit them in output
     * @return A string containing the collection items connected by the given
     *         divider
     */
    public static String serializeCollection(java.util.Collection<? extends Object> col, String divider, ObjectFormatter formatter, boolean includeNulls) {

        if (divider == null) {
            divider = "";
        }

        if (formatter == null) {
            formatter = DefaultObjectFormatter.getInstance();
        }

        Iterator<? extends Object> elements = col.iterator();
        StringBuilder output = new StringBuilder();
        Object element;
        boolean firstElement = true;
        
        while (elements.hasNext()) {
            element = elements.next();
            if (element == null) {
                if (includeNulls) {
                    element = "";
                }
                else {
                    continue;
                }
            }

            if (!firstElement) {
                output.append(divider);
            }
            else {
                firstElement = false;
            }
            
            try {
                output.append(formatter.format(element));
            }
            catch (FormattingException e) {
                Logger.getLogger("wga.utils").error("Error formatting element", e);
                output.append(String.valueOf(element));
            }
        }
        return output.toString();

    }
    
    /**
     * Serializes a collection to a single String using the given divider.
     * Collections serialized by this method can be deserialized again by
     * {@link de.innovationgate.utils#WGUtils.deserializeCollection deserializeCollection}.
     * This version takes a special object formatter object, that formats the
     * collection elements before they are added to the result string. The
     * formatter can be used to convert non-string values in the collection to
     * strings by a special method.
     * 
     * This method version omits null values in the collection.
     * 
     * @param col
     *            The collection
     * @param divider
     *            The divider
     * @param formatter
     *            The formatter
     * @return A string containing the collection items connected by the given
     *         divider
     */
    public static String serializeCollection(java.util.Collection<? extends Object> col, String divider, ObjectFormatter formatter) {
        return serializeCollection(col, divider, formatter, false);
    }

    /**
     * Creates a List based on string, that contains substring tokens divided by a
     * special divider string. Can be used to recreate lists that were
     * serialized by
     * {@link #serializeCollection serializeCollection}.
     * 
     * This command: WGUtils.deserializeCollection("a;b;c", ";")
     * 
     * Would create a list containing the three strings "a", "b" and "c".
     * 
     * 
     * @param colString
     *            The string that contains the list information
     * @param divider
     *            divider string that separates the substrings.
     * @param trimTokens
     *            Decides if the tokens are trimmed (via String.trim())
     *            before they are put into the list
     * @param stringDelimiter
     *            Describes a character that should be treated as string
     *            delimiter, like " or '. Text contained in these signs will be
     *            ignored when the split operation is done. Use null if you do
     *            not want to ignore strings.
     * @param removeTokenStringDelimiter
     *            If true and a string delimiter was given will remove delimiters that wrap whole tokens on output. So input string "'a','b'" will return strings "a" and "b" on deserialisation if the ' character was given as delimiter. 
     * @return A list consisting of the substrings inside the parameter string.
     *         Divider strings are omitted.
     */
    public static List<String> deserializeCollection(String colString, String divider, boolean trimTokens, Character stringDelimiter, boolean removeTokenStringDelimiter) {

        List<String> col = new ArrayList<String>();
        int dividerLength = divider.length();
        if (dividerLength == 0) {
            throw new IllegalArgumentException("Cannot deserialize collection with empty string as divider");
        }

        String searchString = colString;
        if (stringDelimiter != null) {
            searchString = clearStrings(colString, stringDelimiter.charValue(), (divider.trim().equals("") ? 'X' : ' '));
        }

        int startPos = 0;
        int nowPos = searchString.indexOf(divider);
        String token;
        while (nowPos != -1) {
            token = colString.substring(startPos, nowPos);
            token = processDeserializedToken(token, trimTokens, stringDelimiter, removeTokenStringDelimiter);
            col.add(token);
            startPos = nowPos + dividerLength;
            nowPos = searchString.indexOf(divider, startPos);
        }

        if (startPos <= colString.length()) {
            token = colString.substring(startPos);
            token = processDeserializedToken(token, trimTokens, stringDelimiter, removeTokenStringDelimiter);
            col.add(token);
        }

        return col;
    }

    private static String processDeserializedToken(String token, boolean trimTokens, Character stringDelimiter, boolean removeTokenStringDelimiter) {
        if (trimTokens) {
            token = token.trim();
        }
        
        if (removeTokenStringDelimiter && stringDelimiter != null && token.startsWith(stringDelimiter.toString()) && token.endsWith(stringDelimiter.toString())) {
            token = token.substring(1, token.length() - 1);
        }
        return token;
    }
    
    /**
     * Creates a List based on string, that contains substrings divided by a
     * special divider string. Can be used to recreate lists that were
     * serialized by
     * {@link #serializeCollection serializeCollection}.
     * 
     * This command: WGUtils.deserializeCollection("a;b;c", ";")
     * 
     * Would create a list containing the three strings "a", "b" and "c".
     * 
     * 
     * @param colString
     *            The string that contains the list information
     * @param divider
     *            divider string that separates the substrings.
     * @param trimTokens
     *            Decides if the substrings are trimmed (via String.trim())
     *            before they are put into the list
     * @param stringDelimiter
     *            Describes a character that should be treated as string
     *            delimiter, like " or '. Text contained in these signs will be
     *            ignored when the split operation is done. Use null if you do
     *            not want to ignore strings.
     * @return A list consisting of the substrings inside the parameter string.
     *         Divider strings are omitted.
     */
    public static List<String> deserializeCollection(String colString, String divider, boolean trimTokens, Character stringDelimiter) {
        return deserializeCollection(colString, divider, trimTokens, stringDelimiter, false);
    }

    /**
     * Clears out "internal strings" from a text that contains some kind of
     * program code. I.e. if the text itself contains string delimiter
     * characters, like " or ', the contents between these characters is
     * regarded a string. Its contents will be cleared in the text version that
     * is returned by this method. This is useful to prepare a text for an
     * operation, that may not react on the contents of strings inside it. This
     * method regards the character \ as an escape sign for string delimiters.
     * So delimiter characters that are prefixed by a \ will be ignored.
     * 
     * @param colString
     *            The text
     * @param stringDelimiter
     *            The character that introduces and closes strings inside the
     *            text
     * @param replaceChar
     *            The character that is used to clear out strings.
     * @return The text with cleared out internal strings
     */
    public static String clearStrings(String colString, char stringDelimiter, char replaceChar) {

        CharacterIterator it = new StringCharacterIterator(colString);
        StringBuffer out = new StringBuffer();
        boolean inAString = false;
        char prevChar = ' ';
        for (char c = it.first(); c != CharacterIterator.DONE; c = it.next()) {
            // Look for string introducor
            if (c == stringDelimiter && prevChar != '\\') {
                inAString = !inAString;
                out.append(stringDelimiter);
            }
            else if (inAString) {
                out.append(replaceChar);
            }
            else {
                out.append(c);
            }
            prevChar = c;
        }
        return out.toString();

    }

    /**
     * Creates a List based on string, that contains substrings divided by a
     * special divider string. Can be used to recreate lists that were
     * serialized by
     * {@link #serializeCollection serializeCollection}.
     * 
     * This command: WGUtils.deserializeCollection("a;b;c", ";")
     * 
     * Would create a list containing the three strings "a", "b" and "c".
     * 
     * 
     * @param colString
     *            The string that contains the list information
     * @param divider
     *            divider string that separates the substrings.
     * @param trimTokens
     *            Decides if the substrings are trimmed (via String.trim())
     *            before they are put into the list
     * @return A list consisting of the substrings inside the parameter string.
     *         Divider strings are omitted.
     */
    public static List<String> deserializeCollection(String colString, String divider, boolean trimTokens) {
        return deserializeCollection(colString, divider, trimTokens, null);
    }

    /**
     * Creates a List based on string, that contains substrings divided by a
     * special divider string. Can be used to recreate lists that were
     * serialized by
     * {@link #serializeCollection serializeCollection}.
     * 
     * This command: WGUtils.deserializeCollection("a;b;c", ";")
     * 
     * Would create a list containing the three strings "a", "b" and "c".
     * 
     * 
     * @param colString
     *            The string that contains the list information
     * @param divider
     *            divider string that separates the substrings.
     * @return A list consisting of the substrings inside the parameter string.
     *         Divider strings are omitted.
     */
    public static List<String> deserializeCollection(String colString, String divider) {
        return deserializeCollection(colString, divider, false);
    }

    /**
     * Serializes a collection (containing strings) to a single String using the
     * given divider. Collections serialized by this method can be deserialized
     * again by
     * {@link de.innovationgate.utils#WGUtils.deserializeCollection deserializeCollection}.
     * 
     * @param col
     *            The collection
     * @param divider
     *            The divider
     * @return A string containing the collection items connected by the given
     *         divider
     */
    public static String serializeCollection(Collection<? extends Object> col, String divider) {
        return WGUtils.serializeCollection(col, divider, null);
    }

    /**
     * Encodes an input string of plain text to it's XML representation. The
     * special characters &, <, > and " and all characters with character code >=
     * 127 are converted to numeric XML entities.
     * 
     * @param input
     *            The plain text string to convert
     * @return The XML representation of the plain text.
     */
    public static String encodeXML(String input) {

        StringReader in = new StringReader(input);
        StringBuffer out = new StringBuffer();
        int ch;

        try {
            while ((ch = in.read()) != -1) {
                if (ch < 127 && ch != '&' && ch != '<' && ch != '>' && ch != '"') {
                    out.append((char) ch);
                }
                else {
                    out.append('&').append('#').append(ch).append(';');
                }
            }
        }
        catch (IOException exc) {
            exc.printStackTrace();
        }

        return out.toString();
    }

    /**
     * Joins the remaining tokens of a StringTokenizer, using the given divider
     * 
     * @param tokenizer
     *            The StringTokenizer
     * @param delim
     *            The divider
     * @return A string containing the remaining tokens of the tokenizer
     *         connected by the given divider
     */
    public static String joinRemainingTokens(java.util.StringTokenizer tokenizer, String delim) {

        StringBuffer joined = new StringBuffer();
        while (tokenizer.hasMoreTokens()) {
            joined.append(tokenizer.nextToken());
            if (tokenizer.hasMoreTokens()) {
                joined.append(delim);
            }
        }
        return joined.toString();

    }

    /**
     * Old version of strReplace. No longer used.
     * 
     * @deprecated
     * @param strText
     * @param strFrom
     * @param strTo
     * @param bMultiple
     * @return Converted string
     */
    public static String strReplaceOld(String strText, String strFrom, String strTo, boolean bMultiple) {

        int iFromLength = strFrom.length();
        int iToLength = strTo.length();

        int iOccurs = 0;
        String strOutput = new String(strText);

        iOccurs = strOutput.toLowerCase().indexOf(strFrom.toLowerCase(), 0);

        int iStartWith = 0;

        while (iOccurs != -1) {
            strOutput = strOutput.substring(0, iOccurs) + strTo + strOutput.substring(iOccurs + iFromLength);
            iStartWith = (iOccurs - 1) + iToLength + 1;

            if (bMultiple)
                iOccurs = strOutput.toLowerCase().indexOf(strFrom.toLowerCase(), iStartWith);
            else
                iOccurs = -1;
        }

        return strOutput;
    }

    /**
     * Replaces occurences of a substring inside a string by another substring.
     * This variant of the method takes a replace processor object that can be
     * used to further specify the replacing mechanism.
     * 
     * @param strText
     *            The text to search for occurences of the substring
     * @param strFrom
     *            The substring to search for
     * @param proc
     *            The processor that will make the replacement and tell the
     *            method where to continue searching.
     * @param bMultiple
     *            Specify true if multiple occurences should be replaced.
     *            Specify false if only the first occurence should be replaced.
     * @param exactCase
     *            Determines if strings should be compared with exact case.
     *            If false, string are matched case insensitive
     * @return The string with occurences of substring replaced.
     */
    public static String strReplace(String strText, String strFrom, ReplaceProcessor proc, boolean bMultiple, boolean exactCase) {

        if (strText == null || strFrom == null) {
            return "";
        }

        if (proc == null) {
            proc = new DefaultReplaceProcessor("");
        }

        int iFromLength = strFrom.length();

        String strLCText = (exactCase ? strText : strText.toLowerCase());
        String strLCFrom = (exactCase ? strFrom : strFrom.toLowerCase());

        int iOccurs = strLCText.indexOf(strLCFrom, 0);
        int iStartWith = 0;
        StringWriter out = new StringWriter();

        try {
            while (iOccurs != -1) {
                out.write(strText.substring(iStartWith, iOccurs));
                int iTo = iOccurs + iFromLength;
                iStartWith = proc.replace(strText, iOccurs, iTo, out);

                if (bMultiple)
                    iOccurs = strLCText.toLowerCase().indexOf(strLCFrom, iStartWith);
                else
                    iOccurs = -1;
            }

            if (iStartWith < strLCText.length()) {
                out.write(strText.substring(iStartWith));
            }

            return out.toString();
        }
        catch (IOException e) {
            e.printStackTrace();
            return strText;
        }
    }

    /**
     * Replaces occurences of a substring inside a string by another substring.
     * This variant of the method takes a replace processor object that can be
     * used to further specify the replacing mechanism. It matches strings case sensitive.
     * 
     * @param text
     *            The text to search for occurences of the substring
     * @param substring
     *            The substring to search for
     * @param proc
     *            The processor that will make the replacement and tell the
     *            method where to continue searching.
     * @param multiple
     *            Specify true if multiple occurences should be replaced.
     *            Specify false if only the first occurence should be replaced.
     * @return The string with occurences of substring replaced.
     */
    public static String strReplace(String text, String substring, ReplaceProcessor proc, boolean multiple) {
        return strReplace(text, substring, proc, multiple, false);
    }

    /**
     * Replaces occurences of a substring inside a string by another substring.
     * 
     * @param strText
     *            The text to search for occurences of the substring
     * @param strFrom
     *            The substring to search for
     * @param strTo
     *            The substring used to replace the string in strFrom
     * @param bMultiple
     *            Specify true if multiple occurences should be replaced.
     *            Specify false if only the first occurence should be replaced.
     * @param exactCase
     *            Determines if strings should be compared with exact case.
     *            If false, string are matched case insensitive
     * @return The string with occurences of substring replaced.
     */
    public static String strReplace(String strText, String strFrom, String strTo, boolean bMultiple, boolean exactCase) {
        return strReplace(strText, strFrom, new DefaultReplaceProcessor(strTo), bMultiple, exactCase);
    }

    /**
     * Replaces occurences of a substring inside a string by another substring.
     * This variant of the method matches strings case sensitive.
     * 
     * @param strText
     *            The text to search for occurences of the substring
     * @param strFrom
     *            The substring to search for
     * @param strTo
     *            The substring used to replace the string in strFrom
     * @param bMultiple
     *            Specify true if multiple occurences should be replaced.
     *            Specify false if only the first occurence should be replaced.
     * @return The string with occurences of substring replaced.
     */
    public static String strReplace(String strText, String strFrom, String strTo, boolean bMultiple) {
        return strReplace(strText, strFrom, strTo, bMultiple, false);
    }

    /**
     * Converts a base64-encoded string into bytes, then constructs a string
     * from these bytes and returns it
     * 
     * @param base64
     *            base64-encoded information as string
     * @return The decoded string
     */
    public static String base64toString(String base64) {

        try {
            byte[] data = Base64.decode(base64);
            InputStreamReader streamReader = new InputStreamReader(new ByteArrayInputStream(data));
            int ch = -1;
            StringBuffer result = new StringBuffer();
            for (ch = streamReader.read(); ch != -1; ch = streamReader.read()) {
                result.append((char) ch);
            }
            return result.toString();
        }
        catch (Exception exc) {
            exc.printStackTrace();
            return null;
        }

    }

    /**
     * Determines the location of the classfile that defines the given class.
     * This can be useful if a class is in classpath multiple times to determine
     * which version is really used.
     * 
     * @param className
     *            The class to find.
     * @param refClass
     *            The class that itself will use this class in its code. It's
     *            classloader will be used to find the location
     * @return The location of the classfile as path.
     */
    public static String which(String className, Class<?> refClass) {

        if (!className.startsWith("/")) {
            className = "/" + className;
        }
        className = className.replace('.', '/');
        className = className + ".class";

        java.net.URL classUrl = refClass.getResource(className);

        if (classUrl != null) {
            return classUrl.getFile();
        }
        else {
            return null;
        }
    }
    
    /**
     * Determines the location of the classfile that defines the given class.
     * This can be useful if a class is in classpath multiple times to determine
     * which version is really used.
     * 
     * @param className
     *            The class to find.
     * @param cl
     *            The classloader to use to load the class
     * @return The location of the classfile as path.
     */
    public static String which(String className, ClassLoader cl) {

        /*if (!className.startsWith("/")) {
            className = "/" + className;
        }*/
        className = className.replace('.', '/');
        className = className + ".class";

        java.net.URL classUrl = cl.getResource(className);

        if (classUrl != null) {
            return classUrl.getFile();
        }
        else {
            return null;
        }
    }

    /**
     * Counts the occurences of a special text phrase in another text
     * 
     * @param text
     *            The text, in which will be searched
     * @param subtext
     *            The text phrase, that is searched in the text of the first
     *            parameter
     * @return The number of occurences
     */
    public static int countOccurences(String text, String subtext) {

        int count = 0;
        int idx = 0;
        int lenSubtext = subtext.length();
        while ((idx = text.indexOf(subtext, idx + lenSubtext)) != -1) {
            count++;
        }

        return count;

    }

    /**
     * Tests a collection retrieved by lotus.domino.Document.getItemValue() if
     * it is an empty notes field.
     * 
     * @param col
     *            The collection
     * @return true if it is the content of an empty notes field
     */
    public static boolean isEmptyNotesField(Collection<Object> col) {

        if (col == null || col.size() == 0 || (col.size() == 1 && col.toArray()[0].equals(""))) {
            return true;
        }
        else {
            return false;
        }

    }

    /**
     * Tests the object for "emptiness" which is defined as one of the following conditions
     * <ul>
     * <li>The object is null
     * <il>The object is an empty string
     * <li>The object is a collection with size 0 or is only containing one object that itself is "empty" (applies to the given emptiness rules)
     * </ul>
     * @param obj
     * @return true if the object is empty
     */
    public static boolean isEmpty(Object obj) {

        if (obj == null) {
            return true;
        }

        if (obj instanceof String) {
            return obj.equals("");
        }

        if (obj instanceof Collection<?>) {
            Collection<?> col = (Collection<?>) obj;
            if (col.size() == 0) {
                return true;
            }
            if (col.size() == 1) {
                Object firstValue = col.iterator().next();
                return isEmpty(firstValue);
            }
        }

        return false;

    }
    
    
    /**
     * Encodes an input string of plain text to it's HTML representation.
     * Therefor:
     * <ul>
     * <li>All line feeds are converted to <br/> (if param useHTMLTags is true)
     * <li>The special characters &, <, > and " are converted to entities
     * <li> All characters with character code >= 127 are converted to HTML entities (if param reduceToASCII==true)
     * </ul>
     * 
     * @param input
     *            The plain text string to convert
     * @param reduceToASCII
     *            true if you want all non-ASCII characters to be converted to entities
     * @param useHTMLTags
     *            true if you want line feeds to be converted to <br> tags, false if they should be removed
     * @return The HTML representation of the plain text.
     */
    public static String encodeHTML(String input, boolean reduceToASCII, boolean useHTMLTags) {
        return encodeHTML(input, reduceToASCII, (useHTMLTags ? LINEFEEDS_CONVERT_TO_BR : LINEFEEDS_REMOVE));
    }

    /**
     * Encodes an input string of plain text to it's HTML representation.
     * Therefor:
     * <ul>
     * <li>All line feeds are converted to <br/> (if param useHTMLTags is true)
     * <li>The special characters &, <, > and " are converted to entities
     * <li> All characters with character code >= 127 are converted to HTML entities (if param reduceToASCII==true)
     * </ul>
     * 
     * @param input
     *            The plain text string to convert
     * @param reduceToASCII
     *            true if you want all non-ASCII characters to be converted to entities
     * @param lineFeedTreatment
     *            Way how line feeds are treated in the source. Use constants LINEFEEDS_... if you want them to be converted, ignored or removed
     * @return The HTML representation of the plain text.
     */
    public static String encodeHTML(String input, boolean reduceToASCII, int lineFeedTreatment) {
        return encodeHTML(input, reduceToASCII, lineFeedTreatment, null);
    }
    
    
    public static String encodeHTML(String input, boolean reduceToASCII, int lineFeedTreatment, Set<Integer> additionalCharsToEncode) {
        if (input == null) {
            return "";
        }
        
        if (additionalCharsToEncode == null) {
            additionalCharsToEncode = Collections.emptySet();
        }

        StringReader in = new StringReader(input);
        StringBuffer out = new StringBuffer();
        int ch;

        try {
            while ((ch = in.read()) != -1) {
                if (ch == '\n') {
                    if (lineFeedTreatment == LINEFEEDS_CONVERT_TO_BR) {
                        out.append("<br>");
                    }
                    else if (lineFeedTreatment == LINEFEEDS_KEEP) {
                        out.append("\n");
                    }
                }
                else if ((!reduceToASCII || ch < 127) && ch != '&' && ch != '<' && ch != '>' && ch != '"' && !additionalCharsToEncode.contains(ch)) {
                    out.append((char) ch);
                }
                else {
                    out.append('&').append('#').append(ch).append(';');
                }
            }
        }
        catch (IOException exc) {
            exc.printStackTrace();
        }

        return out.toString();
    }
    
    /**
     * Encodes an input string of plain text to it's HTML representation.
     * Therefor:
     * <ul>
     * <li>All line feeds are converted to <br/>
     * <li>The special characters &, <, > and " are converted to entities
     * <li> All characters with character code >= 127 are converted to HTML entities (if param reduceToASCII==true)
     * </ul>
     * 
     * @param input
     *            The plain text string to convert
     * @param reduceToASCII
     *            true if you want all non-ASCII characters to be converted to entities
     * @return The HTML representation of the plain text.
     */
    public static String encodeHTML(String input, boolean reduceToASCII) {
        return encodeHTML(input, reduceToASCII, true);
    }
    
    /**
     * Encodes an input string of plain text to it's HTML representation.
     * Therefor:
     * <ul>
     * <li>All line feeds are converted to <br/>
     * <li>The special characters &, <, > and " are converted to entities
     * <li> All characters with character code >= 127 are converted to HTML entities
     * </ul>
     * 
     * @param input
     *            The plain text string to convert
     * @return The HTML representation of the plain text.
     */
    public static String encodeHTML(String input) {
        return encodeHTML(input, true, true);
    }

    /**
     * Tests if any value in one collection is part of another collection
     * 
     * @param col1
     *            First collection
     * @param col2
     *            Second collection
     * @return The first matching object, if any matches. If there are not
     *         matches returns null.<
     */
    public static Object containsAny(Collection<?> col1, Collection<?> col2) {

        Collection<?> largerCol;
        Collection<?> smallerCol;

        if (col1.size() > col2.size()) {
            largerCol = col1;
            smallerCol = col2;
        }
        else {
            largerCol = col2;
            smallerCol = col1;
        }

        Iterator<?> smallerValues = smallerCol.iterator();
        Object value;
        while (smallerValues.hasNext()) {
            value = smallerValues.next();
            if (largerCol.contains(value)) {
                return value;
            }
        }

        return null;

    }

    /**
     * Sorts a collection by a property of the collection contents. The property
     * to use for sorting is specified as JXPath. JXPath makes JavaBean
     * properties accessible via XPath expressions. e.g. if the collection
     * contains beans with a method "getStatus", this method can sort the list
     * based on the return value of this method by specifying this XPath:
     * /status
     * 
     * Cascaded access to bean properties is also possible. This XPath:
     * /address/zipcode
     * 
     * Tests this method-cascade bean.getAddress().getZipcode()
     * 
     * The sorting is always ascending. You can have descending sorting by
     * reversing the results.
     * 
     * @param col
     *            The collection to sort
     * @param property
     *            The property to use for sorting, specified as JXPath.
     * @return The sorted collection as list
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static List sortByProperty(Collection col, String property) {
        PropertyComparator comparator = new PropertyComparator(property);
        List list = new ArrayList(col);
        Collections.sort(list, comparator);
        return list;

    }

    /**
     * Sets a property to a map only if the map does not yet contain the
     * property key.
     * 
     * @param props
     *            The map
     * @param key
     *            The property key
     * @param value
     *            The value of the property
     */
    public static void setDefaultProperty(Map<Object,Object> props, Object key, Object value) {
        if (!props.containsKey(key)) {
            props.put(key, value);
        }
    }

    /**
     * Hashes a given password using the SHA-1 algorithm. SHA-1 is a
     * one-way-encrypting. The password hash cannot be decoded. However similar
     * passwords will result in similar password hashes.
     * 
     * @param pwd
     *            The password
     * @return The password hash
     * @throws NoSuchAlgorithmException
     */
    public static String hashPassword(String pwd) {
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("SHA-1");
            byte[] digestedPwdBytes = messageDigest.digest(pwd.getBytes());
            return Base64.encode(digestedPwdBytes);
        }
        catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("Algorithm SHA-1 is not available", e);
        }
    }

    /**
     * Converts a byte array to a MD5 hexadecimal string
     * @param input The byte input
     * @return The MD5 string
     * @throws NoSuchAlgorithmException
     */
    public static String createMD5HEX(byte[] input) throws NoSuchAlgorithmException {
        MessageDigest algorithm = MessageDigest.getInstance("MD5");
        algorithm.reset();
        algorithm.update(input);
        byte messageDigest[] = algorithm.digest();

        StringBuffer hexString = new StringBuffer();
        for (int i = 0; i < messageDigest.length; i++) {
            hexString.append(Integer.toHexString(0xFF & messageDigest[i]));
        }
        return hexString.toString();
    }

    /**
     * Converts an inputstream to a MD5 hex string
     * the inputstream is implicit closed after reading
     * @param input
     * @return MD5 checksum
     * @throws NoSuchAlgorithmException
     * @throws IOException
     */
    public static String createMD5HEX(InputStream input) throws NoSuchAlgorithmException, IOException {
    	
    	MessageDigest algorithm = MessageDigest.getInstance("MD5");
    	algorithm.reset();
    	
    	InputStream in = null;
    	try {
    		in = new BufferedInputStream(input);
			byte[] buffer = new byte[1024];
			int len = in.read(buffer);						
			while (len > 0) {
				algorithm.update(buffer, 0, len);
				len = in.read(buffer);
			}
			
			byte messageDigest[] = algorithm.digest();		
			StringBuffer hexString = new StringBuffer();
	        for (int i = 0; i < messageDigest.length; i++) {
	            hexString.append(Integer.toHexString(0xFF & messageDigest[i]));
	        }
	        return hexString.toString();
    	} finally {
    		if (in != null) {
    			in.close();
    		}
    	}
    }
    
    /**
     * Converts a byte array to a hexadecimal string
     * @param bytes The byte array
     * @return Hexadecimal representation of the given bytes
     */
    public static String toHexString(byte[] bytes) {

        StringBuffer hex = new StringBuffer();
        for (byte aByte : bytes) {
             String hexByte = java.lang.Integer.toHexString(0xFF & aByte);
             if (hexByte.length() == 1) {
               hex.append("0");
             }
             hex.append(hexByte);
        }
        return hex.toString();
        
    }
    
    /**
     * Retrieves the value of java.lang.Runtime.maxMemory() which is only
     * available if using a JRE of version 1.4 or higher. This value shows the
     * maximum size of the java heap in bytes (as set by the vm parameter -Xmx).
     * If this method is not available because of an older java runtime, the
     * method returns -1.
     * 
     * @return The max size of the heap or -1 if this information is not
     *         available.
     */
    public static long getMaxHeap() {

        long maxHeap = -1;

        try {
            Method maxHeapMethod = Runtime.class.getMethod("maxMemory", new Class[] {});
            if (maxHeapMethod != null) {
                Number maxHeapNumber = (Number) maxHeapMethod.invoke(Runtime.getRuntime(), (Object[]) null);
                maxHeap = maxHeapNumber.longValue();
            }
        }
        catch (Exception e) {
        }
        return maxHeap;

    }

    /**
     * Converts a HTML to plain text by removing all HTML tags.
     * @param html
     * 		The html
     * @return The plain text
     * @throws IOException
     */
    public static String toPlainText(String html) throws IOException {
    	return toPlainText(html, " ", false, false);
    }
    /**
     * Converts a HTML to plain text by removing all HTML tags.
     * @param html
     * 		The html
     * @param lfDivider
     *      The divider by which separate block elements
     * @return The plain text
     * @throws IOException
     */
    public static String toFormattedPlainText(String html) throws IOException {
    	return toPlainText(html, "\n", false, true);
    }
    /**
     * Converts a HTML to plain text by removing all HTML tags.
     * @param html
     * 		The html
     * @param divider
     *      The divider by which separate text fragments that were parsed from the HTML should be divided.
     * @param ignoreWhitespace
     *      Specify true if pure whitespace text fragments should be ignored
     * @return The plain text
     * @throws IOException
     */
    public static String toPlainText(String html, String divider, boolean ignoreWhitespace) throws IOException {
    	return toPlainText(html, divider, ignoreWhitespace, false);
    }
    /**
     * Converts a HTML to plain text by removing all HTML tags.
     * @param html
     * 		The html
     * @param divider
     *      The divider by which separate text fragments that were parsed from the HTML should be divided.
     * @param lfDivider
     *      The divider by which separate block elements
     * @param ignoreWhitespace
     *      Specify true if pure whitespace text fragments should be ignored
     * @return The plain text
     * @throws IOException
     */
    public static String toPlainText(String html, String divider, boolean ignoreWhitespace, boolean formatted) throws IOException {

        // First remove data URLs from code which may bloat the process
        html = WGUtils.strReplace(html, "src=\"data:", new ReplaceProcessor() {
            
            @Override
            public int replace(String text, int from, int to, Writer out) throws IOException {

                int linkEnd = text.indexOf("\"", to);
                out.write("src=\"");
                if (linkEnd != -1) {
                    return linkEnd;
                }
                else {
                    return text.length() - 1;
                }
            }
            
        }, true);
        
        html = WGUtils.strReplace(html, "href=\"data:", new ReplaceProcessor() {
            
            @Override
            public int replace(String text, int from, int to, Writer out) throws IOException {

                int linkEnd = text.indexOf("\"", to);
                out.write("href=\"");
                if (linkEnd != -1) {
                    return linkEnd;
                }
                else {
                    return text.length() - 1;
                }
            }
            
        }, true);
        
        // Convert to plaintext
        PlainTextParserCallback callback = new PlainTextParserCallback(ignoreWhitespace, divider, formatted);
        ParserDelegator parserDelegator = new javax.swing.text.html.parser.ParserDelegator();
        parserDelegator.parse(new java.io.StringReader(html), callback, true);
        return callback.getText().trim();

    }

    /**
     * Copies a file from source to target.
     * This method also is able to copy complete directories. The following behaviour applies by condition of the parameter file types:<br>
     * <p>
     * <b>source is regular file. Target is regular file or nonexistent:</b><br>
     * Source file is copied to target file. Target is overwritten if it already exists.
     * </p>
     * <p>
     * <b>source is regular file. Target is directory:</b><br>
     * Source file is copied into target directory. Gets the same name as the source file.
     * </p>
     * <p>
     * <b>source is directory.. Target is directory or nonexistent:</b><br>
     * Complete source directory is copied into the target directory. The copied directory becomes a sub directory of the target which is created if it does not yet exist.
     * </p>
     * @param source Source file or directory
     * @param target Target file or directory
     * @throws IOException
     */
    public static void copyFile(File source, File target) throws IOException {

        if (source.isDirectory()) {
            File subtarget = new File(target, source.getName());
            copyDirContent(source, subtarget);
        }
        else {
            if (!target.exists()) {
                if (!target.createNewFile()) {
                    throw new IOException("Unable to create target file");
                }
            }
            else if (target.isDirectory()) {
                target = new File(target, source.getName());
            }
    
            byte[] buf = new byte[2048];
            InputStream in = new BufferedInputStream(new FileInputStream(source));
            OutputStream out = new BufferedOutputStream(new FileOutputStream(target));
            int len;
            while ((len = in.read(buf)) != -1) {
                out.write(buf, 0, len);
            }
    
            out.flush();
            in.close();
            out.close();
        }

    }

    /**
     * Copies the contents of a directory into a target directory.
     * @param source The source directory
     * @param targetDir The target directory
     * @throws IOException
     */
    public static void copyDirContent(File source, File targetDir) throws IOException {
        
        if (!source.exists() || !source.isDirectory()) {
            throw new IOException("Source file is no directory: " + source.getPath());
        }
        
        if (!targetDir.exists()) {
            if (!targetDir.mkdirs()) {
                throw new IOException("Unable to create target directory " + targetDir.getPath());
            }
        }
        else if (!targetDir.isDirectory()) {
            throw new IOException("Cannot copy to directory " + targetDir.getPath() + " because there already is a regular file of that name");
        }
        
        File[] files = source.listFiles();
        for (int i = 0; i < files.length; i++) {
            copyFile(files[i], targetDir);
        }
    }
    
    /**
     * Variant of {@link #copyFile(File, File)} that takes file paths as argument
     * @param source File path of source file 
     * @param target File path of target file
     * @throws IOException
     */
    public static void copyFile(String source, String target) throws IOException {
        copyFile(new File(source), new File(target));
    }

    /**
     * Returns english counting endings for numbers "st", "nd", "rd" and "th" like in "1st", "2nd", "3rd", "4th", "21st" etc.
     * @param no The number to determine ending for
     * @return The ending string exclusive the number
     */
    public static String countEnding(int no) {

        int remainder = no % 10;
        if (remainder == 1) {
            return "st";
        }
        else if (remainder == 2) {
            return "nd";
        }
        else if (remainder == 3) {
            return "rd";
        }
        else {
            return "th";
        }

    }

    /**
     * Returns a formatted number inclusive counting ending (see {@link #countEnding(int)}.
     * @param no The number to format
     * @return The formatted number
     */
    public static String countFormat(int no) {
        return new DecimalFormat().format(no) + countEnding(no);
    }

    /**
     * Variant of {@link #countFormat(int)} that takes a string that will be parsed to an integer
     * @param noStr A string that is parsable as integer
     * @return  The formatted number
     */
    public static String countFormat(String noStr) {
        return countFormat(Integer.parseInt(noStr));
    }

    /**
     * Writes all data from a reader to a writer
     * @param read The reader
     * @param write The writer
     * @param bufferSize The size of the buffer to use for data transfer
     * @throws IOException
     */
    public static void inToOut(Reader read, Writer write, int bufferSize) throws IOException {

        char[] buf = new char[bufferSize];
        int len;
        while ((len = read.read(buf)) != -1) {
            write.write(buf, 0, len);
        }

    }


    /**
     * Writes all data from an input stream to an output stream
     * @param read The input stream
     * @param write The output stream
     * @param bufferSize The size of the buffer to use for data transfer
     * @throws IOException
     */
    public static void inToOut(InputStream read, OutputStream write, int bufferSize) throws IOException {

        byte[] buf = new byte[bufferSize];
        int len;
        while ((len = read.read(buf)) != -1) {
            write.write(buf, 0, len);
        }

    }
    
    /**
     * Writes a chosen amount of data from an input stream to an output stream
     * @param read The input stream
     * @param write The output stream
     * @param length number of bytes to write
     * @param bufferSize The size of the buffer to use for data transfer
     * @throws IOException
     */
    public static void inToOutLimited(InputStream read, OutputStream write, int length, int bufferSize) throws IOException {

        byte[] buf = new byte[bufferSize];
        
        int len;
        while (length >= bufferSize && (len = read.read(buf)) != -1) {
            write.write(buf, 0, len);
            length -= len;
        }

        int aByte;
        while (length > 0 && (aByte = read.read()) != -1) {
            write.write(aByte);
            length--;
        }


    }

    /**
     * Searches a map for entries whose string keys start with the given prefix.
     * Extracts these entries and puts them in the result map, cutting off the
     * prefix from the keys.
     * 
     * @param map
     *            The map to search
     * @param prefix
     *            The prefix. Entries with a string key that starts with this
     *            prefix will get extracted.
     * @return The map with the extracted entries. entry keys are the keys of
     *         the original map minus the prefix.
     */
    public static <K extends Object, V extends Object> Map<String,V> extractMapByPrefix(Map<K,V> map, String prefix) {

        int prefixLen = prefix.length();
        Map<String,V> result = new HashMap<>();
        Iterator<K> keys = map.keySet().iterator();
        Object key;
        String keyStr;
        while (keys.hasNext()) {
            key = keys.next();
            if (key instanceof String) {
                keyStr = (String) key;
                if (keyStr.startsWith(prefix)) {
                    result.put(keyStr.substring(prefixLen), map.get(key));
                }
            }
        }
        return result;

    }

    /**
     * Returns a file for a folder. If the folder does not exist it is created.
     * @param parent The parent folder of the retrieved folder
     * @param name The name of the folder to retrieve
     * @return The folder
     * @throws IOException
     */
    public static File getOrCreateFolder(File parent, String name) throws IOException {
        if (!parent.isDirectory()) {
            throw new IllegalArgumentException("Parent file is no folder: " + parent.getPath());
        }

        File folder = new File(parent, name);
        if (!folder.exists()) {
            if (!folder.mkdir()) {
                throw new IOException("Unable to create directory '" + folder.getPath() + "'");
            }
        }
        if (!folder.isDirectory()) {
            throw new IllegalArgumentException("There is already a file of this name: " + name);
        }
        return folder;
    }

    /**
     * Returns a VFS file object for a folder. If the  folder does not exist it is created.
     * @param parent The parent folder of the retrieved folder
     * @param name The name of the folder to retrieve
     * @return The folder
     * @throws IOException
     */
    public static FileObject getOrCreateFolder(FileObject parent, String name) throws IOException {
        if (!parent.getType().equals(FileType.FOLDER)) {
            throw new IllegalArgumentException("Parent file is no folder: " + parent.getName().getPathDecoded());
        }

        FileObject folder = parent.resolveFile(name);
        if (!folder.exists()) {
            if (!folder.getFileSystem().hasCapability(Capability.CREATE)) {
                throw new IOException("File system of file " + folder.getURL().toString() + " is read only");
            }
            folder.createFolder();
        }
        if (!folder.getType().equals(FileType.FOLDER)) {
            throw new IllegalArgumentException("There is already a file of this name: " + name);
        }
        return folder;
    }

    /**
     * Cutoff milliseconds from a time value
     * @param time The time value
     * @return A time value with removed milliseconds
     */
    public static long cutoffTimeMillis(long time) {

        return (long) Math.floor(((double) time) / 1000) * 1000;

    }

    /**
     * Cutoff milliseconds from a date
     * @param date The date
     * @return The date without milliseconds
     */
    public static Date cutoffDateMillis(Date date) {

        long time = date.getTime();
        time = cutoffTimeMillis(time);
        return new Date(time);

    }

    /**
     * Sets a swing/awt container and all of its child components enabled or
     * disabled
     * 
     * @param con
     *            The container
     * @param enabled
     *            The state to set. true for enabled. false for disabled
     */
    public static void setAllEnabled(Container con, boolean enabled) {

        Component[] children = con.getComponents();
        for (int i = 0; i < children.length; i++) {
            children[i].setEnabled(enabled);
        }
        con.setEnabled(enabled);

    }

    /**
     * Updates a list with the state represented by another list, with as less
     * changes as possible. Elements that are new in newCol will get added to
     * list. Elements that do not exist in newCol but exist in list will get
     * removed from list. Elements that both, list and newCol, contain will
     * remain. This method can be used to update lists that are stored via
     * hibernate. Just replacing the old list with a new one would result in
     * hibernate removing all rows from the table and re-insert all values anew.
     * If lists are updated with this method, hibernate can focus on the real
     * removements and added elements without touching unmodified values.
     * WARNING: Only use this when the sorting order of the list is of no
     * importance!
     * 
     * @param list
     * @param newCol
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static void updateList(List list, Collection newCol) {
        List newList = new ArrayList(newCol);
        list.retainAll(newList);
        newList.removeAll(list);
        list.addAll(newList);
    }

    /**
     * Creates a new list that contains the same elements than the parameter
     * list, but with all string elements converted to lower case
     * 
     * @param listOriginal
     * @return The list with all strings converted to lower case
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static List toLowerCase(List listOriginal) {

        List list = new ArrayList();
        Object elem;
        for (int i = 0; i < listOriginal.size(); i++) {
            elem = listOriginal.get(i);
            if (elem instanceof String) {
                list.add(((String) elem).toLowerCase());
            }
            else {
                list.add(elem);
            }
        }

        return list;

    }
    
    /**
     * Creates a new list that contains the string representations
     * of all elements of the original list
     * null values are preserved.
     * 
     * @param listOriginal
     * @return The list with all elements converted to their string representation
     */
    public static List<String> toString(List<Object> listOriginal) {

        List<String> list = new ArrayList<String>();
        Object elem;
        for (int i = 0; i < listOriginal.size(); i++) {
            elem = listOriginal.get(i);
            if (elem != null) {
                list.add(String.valueOf(elem));
            }
            else {
                list.add(null);
            }
        }

        return list;

    }

    /**
     * Makes a bitsise compare of two bitsets, represented by ints. Returns true
     * if all of the bits in bitset 2 are contained in bitset 1. (It may seem
     * strange to make a special method for this, since the operation is not
     * that complicated, but it will make the code that uses this method more
     * readable than the bit operation).
     * 
     * @param bitset1
     *            The bitset 1
     * @param bitset2
     *            The int whose bits should be tested
     * @return True if all of bits in bitset2 are contained in bitset1.
     */
    public static boolean testBits(int bitset1, int bitset2) {
        return (bitset1 & bitset2) == bitset2;
    }

    /**
     * Reduces a string to a given maximum length. If the string must be
     * truncated to match the max length the last two characters of the string
     * will get converted to "..".
     * 
     * @param str
     *            The string to reduce
     * @param length
     *            The maximum length of the string
     * @return The, eventually truncated, string
     */
    public static String reduce(String str, int length) {
        
        if (length == 0) {
            return str;
        }

        if (str == null) {
            return null;
        }
        else if (str.length() > length) {
            return str.substring(0, length - 2) + "..";
        }
        else {
            return str;
        }

    }

    /**
     * Converts a string to a boolean value, accepting "true", "t", "1", "yes"
     * and "y" as true, accepting "false", "f", "0", "no", "n" as false, and
     * throwing an IllegalArgumentException when none of these strings match.
     * The method's test is case-insensitive.
     * 
     * @param expr
     *            The boolean string
     * @return The boolean value
     */
    public static boolean stringToBoolean(String expr) {
        
        String cleanExpr = expr.toLowerCase().trim();

        if (cleanExpr.equals("true") || cleanExpr.equals("t") || cleanExpr.equals("1") || cleanExpr.equals("yes") || cleanExpr.equals("y")) {
            return true;
        }
        else if (cleanExpr.equals("false") || cleanExpr.equals("f") || cleanExpr.equals("0") || cleanExpr.equals("no") || cleanExpr.equals("n")) {
            return false;
        }
        else {
            throw new IllegalArgumentException("Expression could not be interpreted as boolean: " + expr);
        }
    }
    
    /**
     * checks if the given string can be interpreted as boolean
     * accepting "true", "t", "1", "yes" and "y", "false", "f", "0", "no", "n"
     * @param expr
     * @return the boolean interpretation of the string 
     */
    public static boolean isBooleanValue(String expr) {
    	String cleanExpr = expr.toLowerCase().trim();
    	if (cleanExpr.equals("true") || cleanExpr.equals("t") || cleanExpr.equals("1") || cleanExpr.equals("yes") || cleanExpr.equals("y") || cleanExpr.equals("false") || cleanExpr.equals("f") || cleanExpr.equals("0") || cleanExpr.equals("no") || cleanExpr.equals("n")) {
    		return true;
    	} else {
    		return false;
    	}
    }

    /**
     * Retrieves a boolean value from a map value. Will convert string
     * representations of booleans automatically by using
     * WGUtils.stringToBoolean().
     * 
     * @param options
     *            The options map.
     * @param name
     *            The name of the option
     * @param defaultValue
     *            The default value to use if the option is not set or it's
     *            boolean value is not determinable
     * @return The boolean value if any could get determined, the default value
     *         otherwise
     */
    public static boolean getBooleanMapValue(Map<?,?> options, String name, boolean defaultValue) {

        Object obj = options.get(name);
        if (obj == null) {
            return defaultValue;
        }
        else if (!(obj instanceof Boolean)) {
            try {
                return WGUtils.stringToBoolean(obj.toString());
            }
            catch (IllegalArgumentException e) {
                return defaultValue;
            }
        }
        else {
            return ((Boolean) obj).booleanValue();
        }

    }
    
    /**
     * Very simple method returning a parameter value if it is non null, or else a default value
     * @param value The value, returned when != null
     * @param defaultValue The default value, returned when value == null
     */
    public static <X> X getValueOrDefault(X value, X defaultValue) {
        if (value != null) {
            return value;
        }
        else {
            return defaultValue;
        }
    }

    /**
     * Removes single quotes from a string
     * @param strName The string
     * @return String without single quotes
     */
    public static String escapeSQ(String strName) {
        return strReplace(strName, "'", "", true);
    }
    
    /**
     * Escapes Strings to be used in ASN.1 attributes (like LDAP attributes)
     * @param str The string to escape
     * @return The escaped string
     */
    public static String escapeASN1(String str) {
        
        StringBuffer out = new StringBuffer();
        char c;
        boolean escape;
        for (int i=0; i < str.length(); i++) {
            c = str.charAt(i);
            
            escape = false;
            if (i == 0 && ASN1_FIRSTCHAR_ESCAPE_CHARS.indexOf(c) != -1) {
                escape = true;
            }
            
            if (escape == false && (i == str.length() - 1) &&  c == ' ') {
                escape = true;
            }
            
            if (escape == false && ASN1_ESCAPE_CHARS.indexOf(c) != -1) {
                escape = true;
            }
            
            if (escape) {
                out.append("\\");
            }
            out.append(c);
            
        }
        
        return out.toString();
        
    }
    
    /**
     * Escapes a string to be put out as JavaScript string literal. 
     * Contained string delimiters are escaped so they do not terminate the literal.
     * This method also puts out the surrounding string delimiters ".
     * @param value The string literal
     * @return The escaped literal
     * @deprecated Use {@link #encodeJS(String)} instead
     */
    public static String escapeJsString(String value) {
        return "\"" + WGUtils.strReplace(value, "\"", "\\\"", true) + "\";\n";
    }

    /**
     * Removes the suffix part (i.e. the part after the last ".") from a file name
     * @param name The file name
     * @return File name w/o suffix
     */
    public static String removeFileNameSuffix(String name) {
        return name.substring(0, name.lastIndexOf("."));
    }

    /**
     * Reads data from a reader into a string
     * @param reader The reader
     * @return The string with the read data
     * @throws IOException
     */
    public static String readString(Reader reader) throws IOException {

        StringWriter writer = new StringWriter();
        inToOut(reader, writer, 2048);
        return writer.toString();

    }
    
    /**
     * Readers data from an input stream into a string 
     * @param in The input stream
     * @param encoding The text encoding of the stream
     * @return The string with the read data
     * @throws IOException
     * @throws UnsupportedEncodingException
     */
    public static String readString(InputStream in, String encoding) throws IOException, UnsupportedEncodingException {
        
        InputStreamReader reader = new InputStreamReader(in, encoding);
        return readString(reader);
        
    }

    /**
     * Sorts the child elements of an element based on their indiviual XPath
     * result. This utility method should be used instead of direct sorting of
     * the elements()-list of a parent element, since this will fail with
     * exception on most occasions.
     * 
     * @param parent
     *            The parent element
     * @param xpath
     *            The xpath that is evaluated on each child element
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static void sortChildElements(Element parent, String xpath) {

        // Create a copy of the elements list and sort it
        List list = new ArrayList(parent.elements());
        DocumentHelper.sort(list, xpath);

        // Iterate over sorted list. Remove and add all in order
        Iterator it = list.iterator();
        while (it.hasNext()) {
            Element element = (Element) it.next();
            parent.remove(element);
            parent.add(element);
        }

    }

    /**
     * Zips the contents of a directory to a ZipOutputStream.
     * This method uses the {@link DirZipper} class with default settings. Use the class directly for more control.
     * @param zipDir The directory to zip up
     * @param zos The ZipOutputStream where the data is written to
     * @throws IOException
     */
    public static void zipDirectory(File zipDir, ZipOutputStream zos) throws IOException {
        DirZipper zipper = new DirZipper();
        zipper.zipDirectory(zipDir, zos);
    }
    
    /**
     * Calculates a relative file path, relative to the given parent path.
     * Example:
     * <code>
     * relativeFilePath("D:\Daten\WGAConfig\WGA32Test\designsync\mysql", "D:\Daten\WGAConfig\WGA32Test")
     * </code>
     * returns "designsync\mysql"
     * <p>
     * This works only when
     * <ul>
     * <li> path itself starts with parentPath
     * <li>parentPath is a syntacticly valid directory path (which does not mean that the directory must exist)
     * </ul>
     * <p>
     * If these conditions are not met, the method either throws an IllegalArgumentException (if param failIfNoParent==true)
     * or just returns the path again completely (if param failIfNoParent==false) 
     * </p>
     * @param path The path from which a relative path should be extracted
     * @param parentPath A parent path of path, to which the calculated relative path should be relative to
     * @param failIfNoParent Controls the failure behaviour. See method description.
     * @return The relative path
     */
    public static String relativeFilePath(String path, String parentPath, boolean failIfNoParent) {
        
        // Test if parent path is the beginning of path
        if (!path.startsWith(parentPath)) {
            if (failIfNoParent) {
                throw new IllegalArgumentException("Path '" + parentPath + "' is no parent path of path '" + path + "'");
            }
            else {
                return path;
            }
        }
        
        // Test if parent path does not end inside a filename
        if (!parentPath.endsWith(SystemUtils.FILE_SEPARATOR)) {
            if (path.length() > parentPath.length() && !path.substring(parentPath.length(), parentPath.length() + 1).equals(SystemUtils.FILE_SEPARATOR)) {
                throw new IllegalArgumentException("Path '" + parentPath + "' is no parent directory of path '" + path + "'");
            }
        }
        
        int cutoffLength = (parentPath.endsWith(SystemUtils.FILE_SEPARATOR) ? parentPath.length() : parentPath.length() + 1);
        return path.substring(cutoffLength);
        
    }
    
    /**
     * A variant of {@link #relativeFilePath(String, String, boolean)} which always throws a IllegalArgumentException if the
     * parent path is not valid.
     * @param path
     * @param parentPath
     * @return The absolute path
     */
    public static String relativeFilePath(String path, String parentPath) {
        return relativeFilePath(path, parentPath, true);
    }
    
    /**
     * Finds an object inside any collection based on its hashcode.
     * @param col The collection to search
     * @param hash HashCode of the searched object
     * @return The object if it was contained in the collection, null if not
     */
    public static Object findObjectByHash(Collection<?> col, int hash) {
        
        Iterator<?> it = col.iterator();
        Object obj;
        while (it.hasNext()) {
            obj = it.next();
            if (obj.hashCode() == hash) {
                return obj;
            }
        }
        
        return null;
        
    }

    /**
     * Retrieves an DOM Element with a given name. The element is created if it does not yet exist.
     * @param parent The parent element of the element to retrieve
     * @param name The name of the element
     * @return The element
     */
    public static Element getOrCreateElement(Element parent, String name) {
    
        Element element = parent.element(name);
        if (element == null) {
            element = parent.addElement(name);
        }
    
        return element;
    }

    /**
     * Retrieves a DOM attribute with a given name. The attribute is created if it does not yet exist
     * @param element The element containing the attribute
     * @param name The name of the attribute
     * @param defaultValue The default value of the attribute, used when it must be created
     * @return The attribute
     */
    public static Attribute getOrCreateAttribute(Element element, String name, String defaultValue) {
    
        Attribute att = element.attribute(name);
        if (att == null) {
            element.addAttribute(name, defaultValue);
            att = element.attribute(name);
        }
        return att;
    
    }
    
    /**
     * Sets the selected items on a {@link JList}, which is a tedious task to do by hand.
     * @param selection The items that should be selected in the list
     * @param list The list itself
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static void setJListSelection(List selection, JList list) {
        
        // Load JList model data in array list
        List modelItems = new ArrayList();
        ListModel listModel = list.getModel();
        for (int i=0; i < listModel.getSize(); i++) {
            modelItems.add(listModel.getElementAt(i));
        }
        
        // Determine indices of selection items
        List indices = new ArrayList();
        Iterator items = selection.iterator();
        while (items.hasNext()) {
            Object item = items.next();
            int index = modelItems.indexOf(item);
            if (index != -1) {
                indices.add(new Integer(index));
            }
        }
        
        // Convert Integer list to int array (man, this is awkward...)
        int[] indicesArr = new int[indices.size()];
        for (int i=0; i < indices.size(); i++) {
            indicesArr[i] = ((Integer) indices.get(i)).intValue();
        }
        
        // Set selection
        list.setSelectedIndices(indicesArr);
        
    }
    
    /**
     * Extracts the messages of an Throwable and it's causes.
     * The message includes the Throwable's class name and the message itself. 
     * The result list begins with the message of the given throwable and continues with the message of it's cause, and the causes of that cause.
     * This is continued until a cause throwable itself has no cause.
     * @param th The throwable
     * @return List of messages
     */
    public static List<String> extractMessages(Throwable th) {
        
        List<String> msg = new ArrayList<String>();
        while (th != null) {
            msg.add(th.getClass().getName() + " - " + th.getMessage());
            if (th.getCause() != null  && th.getCause() != th) {
                th = ((Exception) th).getCause();
            }
            else {
                th = null;
            }
        }
        return msg;
        
    }

    /**
     * Fills the millisecond part of a date value with 999 if it is 000.
     * This can be used to round up time values with missing millisecond precision (e.g. from database columns)
     * that may not be lower than the actual time they refer to, which might be problematic when comparing these
     * dates to other values.
     * @param lastModified
     * @return The date with millsecond part filled, if it was empty, or the unmodified date
     */
    public static Date roundMillisUp(Date lastModified) {
        
        long time = lastModified.getTime();
        long millis = time % 1000;
        if (millis == 0) {
            time += 999;
            return new Date(time);
        }
        else {
            return lastModified;
        }
        
    }
    
    
    /**
     * Does a null safe compare on two objects. The objects are considered equal if:
     * - Both are null
     * - Both are non-null and a normal equals()-compare returns true
     * @param obj1 Compared object 1
     * @param obj2 Compared object 2
     * @param ignoreCase If true and both objects are strings, will use equalsIgnoreCase()
     * @return true if objects are considered equal
     */
    public static boolean nullSafeEquals(Object obj1, Object obj2, boolean ignoreCase) {
        
        if (obj1 == null && obj2 == null) {
            return true;
        }
        
        if (obj1 != null && obj2 != null) {
            if (ignoreCase && obj1 instanceof String && obj2 instanceof String) {
                return ((String) obj1).equalsIgnoreCase((String) obj2);
            }
            else {
                return obj1.equals(obj2);
            }
        }
        
        return false;
    }
    
    /**
     * Does a null safe compare on two objects. The objects are considered equal if:
     * - Both are null
     * - Both are non-null and a normal equals()-compare returns true
     * This is a variant of {@link #nullSafeEquals(Object, Object, boolean)} which will not use case-insensitive compare.
     * @param obj1 Compared object 1
     * @param obj2 Compared object 2
     * @return true if the objects are considered equal
     */
    public static boolean nullSafeEquals(Object obj1, Object obj2) {
        return nullSafeEquals(obj1, obj2, false);
    }
    
    /**
     * Returns the root cause throwable for the given throwable
     * @param e The throwable
     * @return The root cause
     */
    public static Throwable getRootCause(Throwable e) {
        while (e.getCause() != null) {
            e = e.getCause();
        }
        return e;
    }
    
    /**
     * Returns the root cause throwable for the given throwable of a given type
     * @param e The throwable
     * @param throwableClass The class that is searched as cause
     * @return The root cause or null if none of the given class was found;
     */
    @SuppressWarnings("unchecked")
    public static <T extends Throwable> T getRootCause(Throwable e, Class<T> throwableClass) {
        while (true) {
            if (throwableClass.isAssignableFrom(e.getClass())) {
                return (T) e;
            }
            if (e.getCause() != null) {
                e = e.getCause();
            }
            else {
                return null;
            }
        }
        
    }
    
    /**
     * Returns a cause of the given throwable (or the throwable itself) which is of the given type. Returns null if no such cause was found.
     * @param e The exception
     * @param type The type of exception searched
     * @return The found exception or null
     */
     public static <CauseType extends Throwable> CauseType getCauseOfType(Throwable e, Class<? extends CauseType> type) {
        
        while (e != null) {
            if (type.isAssignableFrom(e.getClass())) {
                @SuppressWarnings("unchecked")
                CauseType e2 = (CauseType) e;
                return e2;
            }
            e = e.getCause();
        }
        return null;
        
    }
    
    /**
     * Creates a synchronized map instance.
     * Uses the most effective available synchronized map in the current java runtime.
     * Either ConcurrentHashMap for Java 5 or Collections.synchronizedMap(new HashMap()) for older Java runtimes
     * @return A synchronized map instance
     * @deprecated As the minimum Java version for WGA is now at least 5
     */
    public static <K extends Object, V extends Object> Map<K,V> createSynchronizedMap() {
        return new ConcurrentHashMap<K,V>();
    }
    
    /**
     * Method to clear a value inside a ThreadLocal.
     * This method will use the JDK5 method ThreadLocal.remove() when available to do this task. 
     * This will allow the ThreadLocalMap-Entry to be garbage collected. Otherwise it sets the Entry to the value of null.
     * @param tl The ThreadLocal whose value to clear
     * @deprecated since Java 5 is minimum dependency of OpenWGA
     */
    public static void removeThreadLocalValue(ThreadLocal<?> tl) {
        
        if (_threadLocalRemoveMethod != null) {
            try {
                _threadLocalRemoveMethod.invoke(tl, new Object[]{});
                return;
            }
            catch (Exception e) {
                Logger.getLogger("wga.utils").error("Error removing thread local value", e);
            }
        }
        
        tl.set(null);
    }
    
    /**
     * Removes daytime information from a date, leaving date information only
     * @param date The date to strip daytime information from
     * @return The date only date object
     */
    public static Date dateOnly(Date date) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
        cal.set(Calendar.AM_PM, 0);
        cal.set(Calendar.HOUR, 0);
		cal.set(Calendar.HOUR_OF_DAY, 0);
		cal.set(Calendar.MINUTE, 0);
		cal.set(Calendar.SECOND, 0);
		cal.set(Calendar.MILLISECOND, 0);
		return cal.getTime();

	}
    
    /**
     * Removes date information from a date, leaving daytime information only
     * @param date The date to strip date information from
     * @return The daytime only date object
     */
	public static Date timeOnly(Date date) {

		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.clear(Calendar.YEAR);
		cal.clear(Calendar.MONTH);
		cal.clear(Calendar.DATE);
		cal.clear(Calendar.DAY_OF_WEEK);
		cal.clear(Calendar.DAY_OF_MONTH);
		return cal.getTime();

	}
	
	/**
	 * Formats a date by WGAs default date format dd.MM.yyyy
	 */
	public static String stdDateFormat(Date date) {
	    return DATEFORMAT_STANDARD.format(date);
	}
	
   /**
    * Formats a date by WGAs default time format HH:mm:SS
    */
    public static String stdTimeFormat(Date date) {
        return DATEFORMAT_STANDARD.format(date);
    }
    
    /**
     * Formats a number by WGAs default decimal format #,##0
     */
    public static String stdFormat(Number num) {
        return DECIMALFORMAT_STANDARD.format(num.doubleValue());
    }
    
    /**
     * Puts all entries from map source to map target whose values are non-null.
     * This can be used to fill ConcurrentHashMaps that do not take null values.
     * @param source Map providing the entries
     * @param target Map getting all entries
     * @return A set of keys that contained null values and therefor were not put to target
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static Set putAllNonNullValues(Map source, Map target) {
        
        Iterator entries = source.entrySet().iterator();
        Set nullKeys = new HashSet();
        while (entries.hasNext()) {
            Map.Entry entry = (Map.Entry) entries.next();
            if (entry.getValue() != null) {
                target.put(entry.getKey(), entry.getValue());
            }
            else {
                nullKeys.add(entry.getKey());
            }
        }
        return nullKeys;
        
    }
    
        /**
     * Extracts a list of entries from any iterator.
     * May be useful with some commons collections classes like LinkedMap, that maintain order but give
     * no direct access to some ordered list, only via iterators. If the given iterator is a map iterator
     * the method returns the values of the map entries NOT the keys (because for key lists there already
     * is a method in the map itself).
     * @param iterator The iterator whose elements are put to the list
     * @return The list with the iterator elements
     */
    @SuppressWarnings({ "rawtypes", "unchecked" })
    public static List extractEntryList(Iterator iterator) {
        
        List list = new ArrayList();
        while (iterator.hasNext()) {
            if (iterator instanceof MapIterator) {
                iterator.next();
                list.add(((MapIterator) iterator).getValue());
            }
            else {
                list.add(iterator.next());
            }
            
        }
        return list;
        
    }
    
    /**
     * Version of {@link #extractEntryList(Iterator)} which takes an Enumeration instead
     */
    @SuppressWarnings({ "rawtypes"})
    public static List extractEntryList(Enumeration en) {
        return extractEntryList(new EnumerationIterator(en));
    }
    
    /**
     * Parses an integer from a string.
     * Other than the JRE functions this method also copes with integers that are expressed like floats, like 10.0 
     * @param str A string representing a number.
     * @return The integer. If the string represented a float the integer part of that is returned
     */
    public static int parseInt(String str) {
        
        double dValue = Double.parseDouble(str);
        return (int) Math.floor(dValue);
        
    }
    
	/**
	 * Creates a directory link file pointing to a target path
	 * @param parentDir The directory to contain the link file
	 * @param target The target path that the directory link should point to
	 * @throws IOException
	 */
	public static void createDirLink(File parentDir, String target) throws IOException {
		File link = new File(parentDir, DIRLINK_FILE);
		Document doc = DocumentFactory.getInstance().createDocument();
		Element dirlink = doc.addElement("dirlink");
		Element path = dirlink.addElement("path");
		path.addAttribute("location", target);
		XMLWriter writer = new XMLWriter(OutputFormat.createPrettyPrint());
		writer.setOutputStream(new FileOutputStream(link));
		writer.write(doc);
		writer.close();
	}
	
	/**
	 * Resolves an eventually present directory link file. Use this with folders that either may be used themselves or that contain a directory link pointing to the directory to use.
	 * @param file The directory that might contain a directory link file.
	 * @return Either the path that an available directory link file points to or the given directory itself again.
	 */
	public static File resolveDirLink(File file) {
		if (file != null && file.exists()) {
			if (file.isDirectory()) {
				File link = new File(file, DIRLINK_FILE);
				if (link.exists()) {
					// dir link present resolve
					
					Document doc;
					try {
						FileInputStream fileInputStream = new FileInputStream(link);
						String linkLocation = readDirLinkLocation(fileInputStream);
						if (linkLocation != null) {
							if (linkLocation.startsWith("../")) {
								return new File(file, linkLocation);
							} else {
								return new File(linkLocation);
							}
						}
					} catch (Exception e) {
						Logger.getLogger("wga.utils").error("Unable to resolve dir link. '" + link.getAbsolutePath() + "'.", e);
					}					
				}
			}
		}
		// no dir link or file does not exist - just return
		return file;
	}

    private static String readDirLinkLocation(InputStream fileInputStream) throws DocumentException, IOException {
        Document doc;
        SAXReader reader = new SAXReader();
        doc = reader.read(fileInputStream);
        Element dirlink = doc.getRootElement();
        String linkLocation = dirlink.element("path").attributeValue("location", null);
        fileInputStream.close();
        return linkLocation;
    }
	
    /**
     * Resolves an eventually present directory link file (variant with Commons VFS file objects). Use this with folders that either may be used themselves or that contain a directory link pointing to the directory to use.
     * @param file The directory that might contain a directory link file.
     * @return Either the path that an available directory link file points to or the given directory itself again.
     */
    public static FileObject resolveDirLink(FileObject file) throws FileSystemException {
	    
	    if (file != null && file.exists()) {
            if (file.getType().equals(FileType.FOLDER)) {
                FileObject link = file.resolveFile(DIRLINK_FILE);
                if (link.exists()) {
                    // dir link present resolve
                    
                    Document doc;
                    try {
                        InputStream fileInputStream = link.getContent().getInputStream();
                        String linkLocation = readDirLinkLocation(fileInputStream);
                        if (linkLocation != null) {
                            if (linkLocation.startsWith("../")) {
                                return file.resolveFile(linkLocation);
                            } else {
                                return file.getFileSystem().resolveFile(linkLocation);
                            }
                        }
                    } catch (Exception e) {
                        Logger.getLogger("wga.utils").error("Unable to resolve dir link. '" + link.getName().getPath() + "'.", e);
                    }                   
                }
            }
        }
        // no dir link or file does not exist - just return
        return file;
	    
	}
	
	/**
	 * Return the path of the given classes package, that must be used when loading resources from it
	 * This returns the name of the package of the given class, converted to a resource path. You can use
	 * the returned path to load non-class resources from the package folder.
	 * @param clazz The class whose package is used
	 */
	public static String getPackagePath(Class<? extends Object> clazz) {
	    return WGUtils.strReplace(clazz.getPackage().getName(), ".", "/", true);
	}
    
    /**
     * Lowercases the string elements of a list. Elements of other types remain untouched.
     */
	@SuppressWarnings({ "rawtypes", "unchecked" })
    public static void lowerCaseList(List list) {
        
        for (int idx=0; idx < list.size(); idx++) {
            Object element = list.get(idx);
            if (element instanceof String) {
                String str = (String) element;
                str = str.toLowerCase();
                list.set(idx, str);
            }
        }
        
    }
    
    /**
     * Reduces a user agent string to a given size and keeping the basic syntax intact when possible
     * The method tries to reduce the content of the outer bracket content so the user agent is still parseable
     * @param str User agent string
     * @param len Maximum length allowed
     * @return Truncated user agent string
     */
    public static String reduceUserAgentString(String str, int len) {
        
        if (str == null) {
            return null;
        }
        
     // Enforce maximum size of len chars
        int oversize = str.length() - len;
        
        // If there is oversize we try to truncate the client string without breaking its format (B0000596A)
        if (oversize > 0) {
            int outerBracketStart = str.indexOf("(");
            int outerBracketEnd = str.lastIndexOf(")");
            if (outerBracketStart != -1 && outerBracketEnd != -1 && (outerBracketEnd - outerBracketStart - 2) >= oversize) {
                String bracketContent = str.substring(outerBracketStart + 1, outerBracketEnd);
                bracketContent = WGUtils.reduce(bracketContent, bracketContent.length() - oversize);
                str = str.substring(0, outerBracketStart + 1) + bracketContent + str.substring(outerBracketEnd);
            }
            
            // If we do not find these brackets, or their content is not long enough we must truncate the string without respecting its structure
            else {
                str = WGUtils.reduce(str, len);
            }
        }
        
        return str;
    }
    
    /**
     * executes the given runnable with timeout
     * @param runnable The runnable to execute
     * @param timeout The timeout in ms.
     * @throws WGTimeoutException when timeout occurs.
     * @throws InterruptedException when the runnable before timeout occurs.
     * @throws Throwable on errors of runnable execution.
     */
    public static void executeWithTimeout(RunnableWithExceptions runnable, long timeout) throws Throwable {
    	TimeoutThread thread = new TimeoutThread(runnable);
    	thread.start();
    	
    	thread.join(timeout);
	
    	if (!thread.isFinished()) {
    		thread.interrupt();    		
    		throw new WGTimeoutException("Execution of '" + runnable.getClass().getName() + "' timed out after " + timeout + " ms.");
    	} else if (thread.getThrowable() != null) { 
    		throw thread.getThrowable();    		
    	}
    }
    
    private static class TimeoutThread extends Thread {
    	
    	private boolean _finished = false;


		private RunnableWithExceptions _runnable;

		private Throwable _throwable = null;
    	
    	public Throwable getThrowable() {
			return _throwable;
		}

		public TimeoutThread(RunnableWithExceptions runnable) {
    		_runnable = runnable;
    	}
    	
		public void run() {
			if (_runnable != null) {
				try {
					_runnable.run();
				} catch (Throwable e) {
					_throwable  = e;
				}
			}
			_finished = true;
		}
    	
		public boolean isFinished() {
			return _finished;
		}
    }
    
    /**
     * Thrown when {@link WGUtils#executeWithTimeout(RunnableWithExceptions, long)} runs on the timeout 
     */
    public static class WGTimeoutException extends Exception {

		private static final long serialVersionUID = 1L;

		public WGTimeoutException() {
			super();
		}

		public WGTimeoutException(String message, Throwable cause) {
			super(message, cause);
		}

		public WGTimeoutException(String message) {
			super(message);
		}

		public WGTimeoutException(Throwable cause) {
			super(cause);
		}
    	
    }
    
    /**
     * Interface for a runnable to use with {@link WGUtils#executeWithTimeout(RunnableWithExceptions, long)}
     */
    public static interface RunnableWithExceptions {
    	public abstract void run() throws Throwable;
    }
    
    /**
     * Tool function to log category headers to a logger
     * @param log The logger
     * @param msg The title of the category header
     * @param level The level of category info, resulting in different category characters. Use 1 or 2.
     */
    public static void logCategoryInfo(Logger log, String msg, int level) {
        
        String categoryMarker = (level == 1 ? "#" : "=");
        StringBuffer line = new StringBuffer();
        line.append(StringUtils.repeat(categoryMarker, 3));
        line.append(" ");
        line.append(msg);
        line.append(" ");
        
        int remainingChars = 80 - msg.length();
        if (remainingChars > 0) {
            line.append(StringUtils.repeat(categoryMarker, remainingChars));
        }
        log.info(line.toString());
    }

    /**
     * Returns the field reflection object of the field of the given name.
     * This method will find fields of any scope in the given class and all superclasses. 
     * @param theClass The class searched for the field
     * @param name The field name
     * @return The field reflection object or null if the field does not exist
     */
    public static Field getClassField(Class<?> theClass, String name) {
        
        Field field = null;
        while (true) {
            try {
                field = theClass.getDeclaredField(name);
            }
            catch (Exception e) {
            }
            
            if (field != null) {
                return field;
            }
            
            if (theClass.getSuperclass() != null) {
                theClass = theClass.getSuperclass();
            }
            else {
                return null;
            }
        }
        
        
    }
    
    /**
     * Encodes some string to be safely used inside a JavaScript literal
     * @param str The string to encode
     * @return The encoded string
     */
    public static String encodeJS(String str) {
        
        // Escape the backslash character itself
        str = str.replaceAll("\\\\", "\\\\\\\\");
        
        // String delimiters are escaped with backslashes 
        str = str.replaceAll("'", "\\\\'");
        str = str.replaceAll("\"", "\\\\\"");
        
        // Script tags are removed
        str = PATTERN_ENCODE_SCRIPT_START.matcher(str).replaceAll("");
        str = PATTERN_ENCODE_SCRIPT_END.matcher(str).replaceAll("");
        
        // Various types of linefeeds are replaced
        str = str.replaceAll("\n", "\\\\n");
        str = str.replaceAll("\u0085", "\\\\n");
        str = str.replaceAll("\u2028", "\\\\n");
        str = str.replaceAll("\u2029", "\\\\n");
        
        // Carriage return is removed
        str = str.replaceAll("\r", "");
        
        return str;
        
    }
    
    /**
     * Encodes some string to be safely used inside a JSON literal
     * JSON encoding has subtile differences from JavaScript (no encoding of single quotes).
     * @param str The string to encode
     * @return The encoded string
     */
    public static String encodeJSON(String str) {
        
        // Escape the backslash character itself
        str = str.replaceAll("\\\\", "\\\\");
        
        // String delimiters are escaped with backslashes 
        str = str.replaceAll("\"", "\\\\\"");
        
        // Script tags are removed
        str = PATTERN_ENCODE_SCRIPT_START.matcher(str).replaceAll("");
        str = PATTERN_ENCODE_SCRIPT_END.matcher(str).replaceAll("");
        
        // Various types of linefeeds are replaced
        str = str.replaceAll("\n", "\\\\n");
        str = str.replaceAll("\u0085", "\\\\n");
        str = str.replaceAll("\u2028", "\\\\n");
        str = str.replaceAll("\u2029", "\\\\n");
        
        // Carriage return is removed
        str = str.replaceAll("\r", "");
        
        // #00005192: encode Tabs
        str = str.replaceAll("\t", "\\\\t");
        
        return str;
        
    }
    
    /**
     * Creates a temporary file with the given name and fills it with the data from the given stream.
     * If the input stream is no {@link ZipInputStream} it will be implicitly closed by this call.
     * @param name The file name
     * @param data The data for the file
     * @return A temporary file object
     * @throws IOException
     */
    public static TemporaryFile createTempFile(String name, InputStream data) throws IOException {
        return new TemporaryFile(name, data, null);
    }
    
    /**
     * Creates an empty temporary file with the given name
     * @param name The file name
     * @return A temporary file object
     * @throws IOException
     */
    public static TemporaryFile createTempFile(String name) throws IOException {
        return new TemporaryFile(name, null, null);
    }
    
    /**
     * performs a unicode normalization to NFC form (java.text.Normalizer.Form.NFC) for the given input
     * @param input The input string
     * @return the normalized or original value if already NFC form
     */
    public static String normalizeUnicode(String input) {
        if (input != null && !Normalizer.isNormalized(input, Normalizer.Form.NFC)) {
            return Normalizer.normalize(input, Normalizer.Form.NFC);
        }
        return input;
    }
    
    /**
     * Converts arbitrary objects into their boolean counterpart
     * Boolean true are:
     * <ul>
     * <li>Boolean.TRUE
     * <li>All strings that {@link #stringToBoolean(String)} thinks are true
     * <li>The numbers 1 and -1 
     *  </ul>
     *  All other objects evaluate to the default given as argument.
     * @param obj Input object
     * @param def Default for returning on unconvertible objects (and null objects)
     * @deprecated Use {@link ConversionUtils#getBoolean(Object, boolean)}
     */
    public static boolean toBoolean(Object obj, boolean def) {
        
        if (obj instanceof Boolean) {
            return (Boolean) obj;
        }
        
        if (obj instanceof String) {
            return stringToBoolean((String) obj);
        }
        else if (obj instanceof Number) {
            Number num = (Number) obj;
            return num.intValue() == 1 || num.intValue() == -1;
        }
        else {
           return def;
        }
        
    }
    
    /**
     * Converts arbitrary objects into their integer counterpart
     * Interpretable as integer are all {@link java.lang.Number} instances and all strings that represent representations of numbers,
     * also all other objects whose {@link #toString()} returns somthing that may be interpreted as number.
     * Non-interpretable objects and null return the default given as argument
     * @param obj Input object
     * @param def Default for returning on unconvertible objects (and null objects)
     * @deprecated Use {@link ConversionUtils#getInteger(Object, int)}
     */
    public static Integer toInteger(Object obj, int def) {
        return ConversionUtils.getInteger(obj, def);
    }
    

    
    /**
     * This method tests if an object is serializable via the standard Java serialisation mechanism by attempting a serialisation
     * @param obj Object to test
     * @return true if the object is serialisable without errors 
     */
    public static boolean isSerializable(Object obj) {
        
        try {
            _serialisation_test_stream.writeObject(obj);
            return true;
        }
        catch (Throwable e) {
            return false;
        }
        
    }
    
    
    /**
     * Decodes an encoded URI sequence according to the given charset
     * This is able to decode URL-encoded characters but also leaves existing Non-ASCII characters intact (unlike Common HttpClients URIUtil)
     * @param uriCharSequence The URI sequence
     * @param charset The charset used to decode
     * @return The decoded string
     * @throws UnsupportedEncodingException
     * @throws MalformedURLException
     */
    public static String decodeURI(CharSequence uriCharSequence, String charset)
            throws UnsupportedEncodingException, MalformedURLException {
        
            if (uriCharSequence == null) {  
                return null;
            }
            String uri = uriCharSequence.toString();
                
            int oi = 0; // output index
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            for (int i=0; i < uri.length(); i++) {
                char c = uri.charAt(i);
                if (c == '%' && i + 2 <= uri.length())  {
                    byte high = (byte) Character.digit((char) uri.charAt(++i), 16);
                    byte low = (byte) Character.digit((char) uri.charAt(++i), 16);
                    if (high == -1 || low == -1) {
                        throw new MalformedURLException("Invalid escape pattern");
                                
                    }
                    byte aByte = (byte) ((high << 4) + low);
                    out.write(aByte);
                }
                else if (c == '+') {
                    out.write(' ');
                }
                else {
                    byte[] bytes = String.valueOf(c).getBytes(charset);
                    out.write(bytes, 0, bytes.length);
                }
            }
    
            return out.toString(charset);
        }
    
    
    /**
     * Zips a string input to compressed bytes
     * @param input The string
     * @return Output bytes as array, null if it is not zippable
     */
    public static byte[] zipString(String input) {
                           
        try {
            byte [] uncompressedBytes   = input.getBytes("UTF-8");
            return zip(uncompressedBytes);
        }
        catch (Exception e) {
            if ("true".equals(System.getProperty("de.innovationgate.wga.debug.zipping"))) {
                e.printStackTrace();
            }
            return null;
        }
        
              
    }

    /**
     * Zips bytes to compressed bytes
     * @param uncompressedBytes Input
     * @throws IOException
     */
    public static byte[] zip(byte[] uncompressedBytes) throws IOException {
        ByteArrayOutputStream baos  = new ByteArrayOutputStream();            
        GZIPOutputStream zos        = new GZIPOutputStream(baos);
        zos.write(uncompressedBytes, 0, uncompressedBytes.length);
        zos.close();
 
        return baos.toByteArray();
    }
    
    /**
     * Unzips a string from bytes previously produced by {@link #zipString(String)}
     * @param input The bytes
     * @return The string, null if it is not unzippable
     */
    public static String unzipString(byte[] input) {        
        
        try {
            byte[] outBytes = unzip(input);
            return new String(outBytes, "UTF-8");
        }
        catch (Exception e) {
            if ("true".equals(System.getProperty("de.innovationgate.wga.debug.zipping"))) {
                e.printStackTrace();
            }
            return null;
        }

    }

    /**
     * Unzips compressed bytes previously produced by {@link #zip(byte[])@}
     * @param input The compressed bytes
     */
    public static byte[] unzip(byte[] input) throws IOException {
        
        String unzipped = null;
        ByteArrayInputStream byteIn = new ByteArrayInputStream(input);
        PatchedGZIPInputStream zipIn = new PatchedGZIPInputStream(byteIn);

        ByteArrayOutputStream out = new ByteArrayOutputStream();
                    
        int numBytesRead = 0;
        byte[] tempBytes = new byte[1024];
        while ((numBytesRead = zipIn.read(tempBytes, 0, tempBytes.length)) != -1) {
            out.write(tempBytes, 0, numBytesRead);
        }

        byte[] outBytes = out.toByteArray();
        
        out.close();
        zipIn.close();
        byteIn.close();
        return outBytes;
    }
    
    /**
     * Removes C0 control characters that are invalid in XML 1.0 and not even allowed as escaped entities from the string
     * This method removes all characters from U+0000 to U+001F except U+0009, U+000A, U+000D.
     * @param str Input string
     * @return String with invalid characters removed
     */
    public static String toValidXmlString(String str) {
        return ILLEGAL_HTML_CHARS_PATTERN.matcher(str).replaceAll("");
    }
    
    /**
     * Converts a {@link Properties} object to a {@link Map} with string generics.
     * Although {@link Properties} is only able to store String keys and values it inherits from Map<Object,Object> for funky reasons.
     * This method is a toolie to convert {@link Properties} to be usable as String map
     * @param props The properties object
     */
    public static Map<String,String> propertiesToStringMap(Properties props) {
        
        Map<String,String> map = new HashMap<String, String>();
        for (Object key : props.keySet()) {
            map.put((String) key, props.getProperty((String) key));
        }
        return map;
        
    }
    
    /**
     * Removes all characters from a string that are not legal as JavaScript identifier
     * Interprets an underscore as the next character being forced to upper case.
     * The upper case rules, that are enforced when param enforceUpperCaseRules is true are:
     * <ul>
     * <li>Non-alphabetic characters at the beginning are stripped off
     * <li>The first alphabetic character is uppercase
     * <li>An alphabetic character preceded by an underscore (which is removed) will also be uppercase
     * <li>All other alphabetic characters are lowercase
     * </ul>
     * @param txt The string
     * @param enforceUpperCaseRules If true enforces the upper case rules described above, meant to transfor, a lowercase design reference to an Object Identifier with cases in an reversible way.
     * In that case the created string can be reverted to the original by using {@link #fromJSIdentifier(String)}.
     * @return The string converted to a valid JS identifier
     */
    public static String toJSIdentifier(String txt, boolean enforceUpperCaseRules) {
        
        StringBuilder out = new StringBuilder();
        
        // Flags for enforceUpperCaseRules
        boolean upperCase = true; // First character is uppercase
        boolean startCharacter = true;
        
        for (int i=0; i< txt.length(); i++) {
            Character c = txt.charAt(i);
            
            if (enforceUpperCaseRules) {
                // Must start with a letter
                if (startCharacter && !Character.isLetter(c)) {
                    continue;
                }
                
                // Convert letters to uppercase, depending on flag
                if (Character.isLetter(c)) {
                    if (upperCase) {
                        c = Character.toUpperCase(c);
                        upperCase = false;
                    }
                    else {
                        c = Character.toLowerCase(c);
                    }
                }
                // Trigger the next letter to be uppercase
                else if (c == '_') {
                    upperCase = true;
                    continue;
                }
            }
            
            Pattern p = (startCharacter ? JS_IDENTIFIER_FIRSTCHARS : JS_IDENTIFIER_CHARS);
            if (p.matcher(c.toString()).matches()) {
                out.append(c);
                startCharacter = false;
            }
            else if (enforceUpperCaseRules) {
                throw new IllegalArgumentException("This string cannot be transformed to a reversible JS identifier because it contains invalid characters: " + txt);
            }
        }
        
        return out.toString();

        
    }
    
    /**
     * Reconstructs the original string from the output of {@link #toJSIdentifier(String, boolean)} with enforced upper case rules.
     * @param txt A string created by {@link #toJSIdentifier(String)} with enforced upper case rules
     * @return The original string
     */
    public static String fromJSIdentifier(String txt) {
     
        StringBuilder out = new StringBuilder();
        
        
        int newWordIdx = -1;
        for (int i=0; i< txt.length(); i++) {
            Character c = txt.charAt(i);
            newWordIdx++;
            
            if (Character.isUpperCase(c)) {
                if (newWordIdx != 0) {
                    out.append("_");
                }
                c = Character.toLowerCase(c);
            }
            else if (c == '.') {
                newWordIdx = -1;
            }
            
            out.append(c);
            
            
        }
        
        return out.toString();
        
    }
    
    /**
     * Removes all characters from a string that are not legal as JavaScript identifier
     * Interprets an underscore as the next character being forced to upper case.
     * @param txt The string
     * @return The string converted to a valid JS identifier
     */
    public static String toJSIdentifier(String txt) {
        return toJSIdentifier(txt, false);
    }
    
    /**
     * Convenience function to create a list from elements
     * @param elements The elements
     */
    @SafeVarargs
    public static<Type> List<Type> list(Type... elements) {
        return new ArrayList<Type>(Arrays.<Type>asList(elements));
    }
    
    /**
     * Convenience function to create a list from elements, overtaking the elements of the first parameter list
     * The parameter lists elements will be first;
     * @param list The parameter lister
     * @param elements The elements
     */
    @SafeVarargs
    public static<Type> List<Type> list(List<Type> list, Type... elements) {
        List<Type> newList =  new ArrayList<Type>(list);
        newList.addAll(Arrays.asList(elements));
        return newList;
    }
    
    /**
     * Converts any number to a BigDecimal, suited for comparison and (monetary) arithmetic.
     * This class uses conversions that avoid rounding errors by forced conversion for the 
     * Number types from JRE. Fallback for other types is to convert them to Double.
     * @param n The number
     */
    public static BigDecimal toBigDecimal(Number n) {
        
        if (n instanceof BigDecimal) {
            return (BigDecimal) n;
        }
        if (n instanceof Long) {
            return new BigDecimal((Long) n);
        }
        else if (n instanceof Integer) {
            return new BigDecimal((Integer) n);
        }
        else if (n instanceof Float) {
            return new BigDecimal((Float) n);
        }
        else if (n instanceof Double) {
            return new BigDecimal((Double) n);
        }
        else if (n instanceof Short) {
            return new BigDecimal((Short) n);
        }
        else if (n instanceof Byte) {
            return new BigDecimal((Byte) n);
        }
        else if (n instanceof BigInteger) {
            return new BigDecimal((BigInteger) n);
        }
        else if (n instanceof AtomicInteger) {
            return new BigDecimal(n.intValue());
        }
        else if (n instanceof AtomicLong) {
            return new BigDecimal(n.longValue());
        }
        else {
            return new BigDecimal(n.doubleValue());
        }
        
    }
    
    public static String generateUID(){
    	return UIDGenerator.generateUID();
    }
            
}
