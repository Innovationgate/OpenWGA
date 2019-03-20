/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wga.server.api;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.search.highlight.Fragmenter;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.search.highlight.InvalidTokenOffsetsException;
import org.apache.lucene.search.highlight.SimpleFragmenter;
import org.apache.lucene.search.highlight.SimpleHTMLFormatter;

import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.BinaryFieldData;
import de.innovationgate.webgate.api.SearchDetails;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileMetaData;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.beans.LuceneIndexFileRule;
import de.innovationgate.wga.common.beans.LuceneIndexItemRule;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.lucene.LuceneSearchDetails;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.webtml.Query;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * The "Lucene" object provides functionalities related to fulltext search in TMLScript.
 * Each Lucene object is constructed with an application in context which determines a context for all lucene operations. Additionally a {@link Context} may be given which provides the necessary context for operations that refer to previously ran queries and a context document, like {@link #bestFragments(String, int, int)}, {@link #highlightItem(String, String, String, String)} and {@link #highlightMeta(String, String, String)}.
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Lucene {
    
    /**
     * This object represents a specific field configuration for the Lucene fulltext index, like it is specified on the Lucene configuration for individual OpenWGA applications. It determines how items of some name are indexed and can be used in fulltext queries.
     */
    public class FieldIndexType {

        private LuceneIndexItemRule _rule;

        private FieldIndexType(LuceneIndexItemRule rule) {
            _rule = rule;
        }

        /**
         * Returns if the pattern of the rule contains a wildcard
         * This is about the pattern of the lucene rule which serves this index type. A wildcard pattern ends with an "*" character and is able to match multiple items with the same beginning part.
         */
        public boolean isWildcardPattern() {
            return _rule.hasWildcard();
        }

        /**
         * Returns if the field index type is from the default rule
         * The default rule of Lucene index configurations is the fallback rule that determines the indexing type of all items that have no explicit rule matching them. When it is explicitly specified it has the pattern "*".
         */
        public boolean isDefaultRule() {
            return _rule.isDefaultRule();
        }

        /**
         * Returns the content type as which the field content is interpreted
         * This property contains predefined strings which identify content types for lucene indexing and determines what the Lucene index assumes to be the content of this field:
         * <ul>
         * <li> "plaintext": Simple text without formatting
         * <li> "htmlxml": HTML or XML markup
         * </ul>
         * The content type determines what part of the fields content is actually interpreted as real content text. For example on fields with content type "htmlxml" the Lucene index will ignore HTML tag names and attributes and only index the tags content.
         */
        public String getContentType() {
            return _rule.getContentType();
        }

        /**
         * Returns the field name matching pattern of the rule
         * This is the matching pattern of the Lucene configuration rule from which the information of the current object is served. It either matches a complete field name or consists of some beginning part of it plus a trailing wildcard character "*". In the latter case the rule is used for all fields whose name match the beginning part.
         */
        public String getPattern() {
            return _rule.getItemExpression();
        }

        /**
         * Determines if the results of a lucene query can be sorted by fields of this rule
         * Lucene queries support sorting its results by individual fields when their index type is determined as being "sortable". This property identifies if this is possible for the current index type.
         */
        public boolean isSortable() {
            return _rule.isSortable();
        }

        /**
         * Returns the index boost
         * The index boost determines the weight of the field for the search score. If a term is found in multiple documents then those documents will have a higher search score where the term is found in fields with a higher boost.
         */
        public float getBoost() {
            return _rule.getBoost();
        }

        /**
         * Returns the actual lucene index type
         * This is string of a predefined range of values:
         * <ul>
         * <li> "fulltext" - Normal fulltext indexing
         * <li> "keyword" - Keyword indexing
         * </ul>
         * The index type determines how the contents of the fields is actually indexed and therefor can be queried. Type "fulltext" tries to find individual words in the content and indexes them separately. Type "keyword" assumes the content of the field to be one complete value which is indexed 1:1.
         */
        public String getIndexType() {
            return _rule.getIndexType();
        }
        
        
        
    }
    
    
    /**
     * This object represents a specific file configuration for the Lucene fulltext index, like it is specified on the Lucene configuration for individual OpenWGA applications. It determines how files of some name are indexed and can be used in fulltext queries.
     * In order to be indexable files from content attachments need to have a content type for which a lucene file handler exists.
     */
    public class FileIndexType {

        private LuceneIndexFileRule _rule;

        private FileIndexType(LuceneIndexFileRule rule) {
            _rule = rule;
        }

        /**
         * Returns if the pattern of the rule contains a wildcard
         * This is about the pattern of the lucene rule which serves this index type. A wildcard pattern starts with an "*" character and is able to match multiple file names with the same ending part.
         */
        public boolean isWildcardPattern() {
            return _rule.hasWildcard();
        }

        /**
         * Returns if the file index type is from the default rule
         * The default rule of Lucene index configurations is the fallback rule that determines the indexing type of all file that have no explicit rule matching them. When it is explicitly specified it has the pattern "*".
         */
        public boolean isDefaultRule() {
            return _rule.isDefaultRule();
        }

        /**
         * Returns the file name matching pattern of the rule
         * This is the matching pattern of the Lucene configuration rule from which the information of the current object is served. It either matches a complete file name or begins with the wildcard character "*", continued by some ending part of a file name. In the latter case the rule is used for all files whose name match the ending part.
         */
        public String getPattern() {
            return _rule.getFilePattern();
        }

        /**
         * Returns the maximum file size for indexed files in bytes
         * Files that are matched via this rule will only be indexed if their file size is lower or equal than this value. Larger files are not indexed. 
         * The size of indexed files is limited mostly to protect the servers health, because indexing large files takes up much memory. A value of 0 means that no file is indexed that matches this rule. A value of -1 means all files are indexed, no matter the size.
         */
        public int getFileSizeLimit() {
            return _rule.getFileSizeLimit();
        }

        /**
         * Returns if file content will be found in field-unspecific searches
         * Lucene queries may be "field-unspecific" when they search for a term but do not determine in what fields this should be found. Internally this actually searches in a field named "allcontent" which contains all content for field-unspecific searches.
         * This property determines if file content indexed via this index type will be included in this "allcontent" field and therefor can be found in those searches.
         */
        public boolean isIncludedInAllContent() {
            return _rule.isIncludedInAllContent();
        }

        /**
         * Returns the index boost
         * The index boost determines the weight of the file for the search score. If a term is found in multiple documents then those documents will have a higher search score where the term is found in files with a higher boost.
         */
        public float getBoost() {
            return _rule.getBoost();
        }
        
    }
    
    private TMLContext _cx;
    private WGA _wga;

    protected Lucene(WGA wga, TMLContext cx) throws WGException {
        _wga = wga;
        _cx = cx;
        if (!_wga.app(_cx.db()).isLuceneIndexed()) {
            _wga.getCore().getProblemRegistry().addProblem(Problem.create(new TMLContext.WebTMLOccasion(), new DatabaseScope(_cx.db().getDbReference()), "webtmlProblem.luceneIndexExpected", ProblemSeverity.LOW));
            throw new WGAServerException("Lucene index is not enabled for database " + _cx.db().getDbReference());
        }
    }
    
    /**
     * Performs a lucene search.
     * @param phrase Search phrase
     * @return Search result set
     * @deprecated Use {@link Database#query(String, String)} and variants instead
     * @throws WGQueryException
     */
    @CodeCompletion
    public WGResultSet search(String phrase) throws WGException {
        return search(phrase, "db", null);
    }
    
    /**
     * Performs a lucene search.
     * @param phrase Search phrase
     * @param scope Either "db" for the current context application, "domain" for all apps in the same domain or "wga" for all apps  
     * @return Search result set
     * @deprecated Use {@link Database#query(String, String)} and variants instead
     * @throws WGQueryException
     */
    @CodeCompletion
    public WGResultSet search(String phrase, String scope) throws WGException {
        return search(phrase, scope, null);
    }

    /**
     * Performs a lucene search.
     * @param phrase Search phrase
     * @param scope Either "db" for the current context application, "domain" for all apps in the same domain or "wga" for all apps
     * @param params Additional parameters for the query
     * @return Search result set
     * @deprecated Use {@link Database#query(String, String)} and variants instead
     * @throws WGQueryException
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    @CodeCompletion
    public WGResultSet search(String phrase, String scope, Map params) throws WGException {
        
        if (params == null) {
            params = new HashMap();
        }
        
        scope = scope.toLowerCase();
        if ("wga".equals(scope)) {
            params.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, LuceneManager.SEARCHSCOPE_WGA);
        }
        else if ("domain".equals(scope)) {
            params.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, LuceneManager.SEARCHSCOPE_DOMAIN);  
        }
        else {
            params.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, LuceneManager.SEARCHSCOPE_DB);
        }
        
        return _wga.getCore().getLuceneManager().search(_cx.db(), phrase, params, _wga);
        
    }
    
    /**
     * Removes the data of the last lucene query with activated result highlighting from the users session
     * The lucene functionality around highlighting consumes some memory on the users session as the data of found terms need to be preserved while highlighting should be possible. If possible this data should be cleared using this method once the highlighting data is not needed any more to save system resources.
     * The highlighting data will be automatically cleared on the following conditions:
     * <ul>
     * <li> Another lucene query with activated highlighting is executed
     * <li> The users session is invalidated by the JavaEE server
     * </ul>
     */
    public void removeQuery() throws WGException {
        _cx.removelucenequery();
    }
    
    /**
     * Returns those parts of an items value that matched a lucene search
     * The method provides an advanced feature of lucene result highlighting which automatically provides those fragments of an items content that matched the lucene query. The fragments are returned in descending order of relevance.
     * You must have activated the highlighting feature on the lucene search via attribute highlight. The fragments of the last lucene search stay available until another lucene search is performed under the same user session.
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param itemname Name of the item on the current context document whose fragments are retrieved 
     * @param fragmentSize Desired number of characters in the fragments  
     * @param maxFragments Maximum number of fragments to return. 
     * @return List of text fragments
     * @throws WGAPIException
     */
    @SuppressWarnings("unchecked")
    public List<String> bestFragments(String itemname, int fragmentSize, int maxFragments) throws WGException {
        return _cx.bestfragments(itemname, fragmentSize, maxFragments, "", "");
    }

    /**
     * Returns those parts of an items value that matched a lucene search
     * The method provides an advanced feature of lucene result highlighting which automatically provides those fragments of an items content that matched the lucene query. The fragments are returned in descending order of relevance.
     * You must have activated the highlighting feature on the lucene search via attribute highlight. The fragments of the last lucene search stay available until another lucene search is performed under the same user session.
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param itemname Name of the item on the current context document whose fragments are retrieved 
     * @param fragmentSize Desired number of characters in the fragments  
     * @param maxFragments Maximum number of fragments to return.
     * @param prefix Prefix text to put out before each found search phrase in the fragments
     * @param suffix Suffix text to put out after each found search phrase in the fragments 
     * @return List of text fragments
     * @throws WGAPIException
     */
    @SuppressWarnings("unchecked")
    public List<String> bestFragments(String itemname, int fragmentSize, int maxFragments, String prefix, String suffix) throws WGException {
        return _cx.bestfragments(itemname, fragmentSize, maxFragments, prefix, suffix, null);
    }
    
    /**
     * Returns those parts of an items value that matched a lucene search
     * The method provides an advanced feature of lucene result highlighting which automatically provides those fragments of an items content that matched the lucene query. The fragments are returned in descending order of relevance.
     * You must have activated the highlighting feature on the lucene search via attribute highlight. The fragments of the last lucene search stay available until another lucene search is performed under the same user session.
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param itemname Name of the item on the current context document whose fragments are retrieved 
     * @param fragmentSize Desired number of characters in the fragments  
     * @param maxFragments Maximum number of fragments to return.
     * @param prefix Prefix HTML code that is put out before each term in a fragment that was matched by the lucene query, meant to highlight those terms. 
     * @param suffix Suffix HTML code that is put out after each term in a fragment that was matched by the lucene query, meant to close tags that were opened in the prefix. 
     * @param encode A WebTML encoding to encode the fragments before output. Accepts the encodings also available to WebTML attribute encode   
     * @return List of text fragments
     * @throws WGAPIException
     */
    @SuppressWarnings("unchecked")
    public List<String> bestFragments(String itemname, int fragmentSize, int maxFragments, String prefix, String suffix, String encode) throws WGException {
        return _cx.bestfragments(itemname, fragmentSize, maxFragments, prefix, suffix, encode);
    }

    /**
     * Returns an item of the current context document, highlighting found terms of the last lucene search
     * This puts out items of a lucene search result while highlighting the found terms. It combines the functionalitites of <tml:item> and its attributes highlight, highlightprefix and highlightsuffix for use in Java
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param name Name of the item to return
     * @param prefix HTML code that is added before each found lucene term
     * @param suffix HTML code that is added after each found lucene term
     * @throws WGAPIException
     */
    public String highlightItem(String name, String prefix, String suffix) throws WGException {
        return _cx.highlightitem(name, prefix, suffix, null);
    }

    /**
     * Returns an item of the current context document, highlighting found terms of the last lucene search
     * This puts out items of a lucene search result while highlighting the found terms. It combines the functionalitites of <tml:item> and its attributes highlight, highlightprefix and highlightsuffix for use in Java
     * Additionally it allows to encode the item before the highlighting prefix and suffix are applied.
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param name Name of the item to return
     * @param prefix HTML code that is added before each found lucene term
     * @param suffix HTML code that is added after each found lucene term
     * @param encode A WebTML encoding in which the item text is encoded before the prefix and suffixes are applied. Accepts alle encodings that are available to the OpenWGA runtime
     * @throws WGAPIException
     */
    public String highlightItem(String name, String prefix, String suffix, String encode) throws WGException {
        return _cx.highlightitem(name, prefix, suffix, encode);
    }
    
    /**
     * Returns a metadata field of the current context document, highlighting found terms of the last lucene search
     * This puts out metadata fields of a lucene search result while highlighting the found terms.
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param name Name of the metadata field to return
     * @param prefix HTML code that is added before each found lucene term
     * @param suffix HTML code that is added after each found lucene term
     * @throws WGAPIException
     */
    @SuppressWarnings("unchecked")
    public List<String> highlightMeta(String name, String prefix, String suffix) throws WGException {
        return _cx.highlightMeta(name, prefix, suffix, null);
    }

    /**
     * Returns a metadata field of the current context document, highlighting found terms of the last lucene search
     * This puts out metadata fields of a lucene search result while highlighting the found terms.
     * Additionally it allows to encode the item before the highlighting prefix and suffix are applied.
     * This method will only work when the document of the chosen context was directly retrieved from a lucene search result.
     * @param name Name of the metadata field to return
     * @param prefix HTML code that is added before each found lucene term
     * @param suffix HTML code that is added after each found lucene term
     * @param encode A WebTML encoding in which the metadata text is encoded before the prefix and suffixes are applied. Accepts alle encodings that are available to the OpenWGA runtime
     * @throws WGAPIException
     */
    @SuppressWarnings("unchecked")
    public List<String> highlightMeta(String name, String prefix, String suffix, String encode) throws WGException {
        return _cx.highlightMeta(name, prefix, suffix, encode);
    }

    @CodeCompletion
    public List<String> highlightLuceneField(String field, String originalText, String prefix, String suffix, String encoder) throws WGException, FormattingException{
    	String encodedText = WGA.get(_cx).encode(encoder, originalText);
    	return highlightLuceneField(field, encodedText, prefix, suffix);
    }
    
    @CodeCompletion
    public List<String> highlightLuceneField(String field, String originalText, String prefix, String suffix) throws WGException{
    	WGACore core = _wga.getCore();
        if (!core.isLuceneEnabled()) {
            _wga.getLog().warn("Unable to highlight text bc. lucene is not enabled.");
            return Collections.singletonList(originalText);
        }
        // try to retrieve last lucene query for highlighting
        org.apache.lucene.search.Query query = (org.apache.lucene.search.Query) _wga.getRequest().getSession().getAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY);
        if (query == null) {
            // no query in session - highlighting not possible
        	return Collections.singletonList(originalText);
        }
                
        // create htmlformatter to highlight fragments with "$HIGHLIGHT_PREFIX$", "$HIGHLIGHT_SUFFIX$"
        // these placeholders are later on replaced by the given prefix and suffix
        // this additional step is necessary to encode the fragment text properly
        String prefixPlaceholder = "$HIGHLIGHT_PREFIX$";
        String suffixPlaceholder = "$HIGHLIGHT_SUFFIX$";
        SimpleHTMLFormatter formatter = new SimpleHTMLFormatter(prefixPlaceholder, suffixPlaceholder);

        // create highlighter
        Highlighter highlighter = core.getLuceneManager().createHighlighter(field, query, formatter);
        
        // create tokenstream
        TokenStream tokenStream = core.getLuceneManager().createTokenStream(originalText, _cx.content());
        
        // create fragmenter and set fragmentsize to metaText.length to ensure only one fragments with the whole metaText is returned        
        Fragmenter fragmenter = new SimpleFragmenter(originalText.length() + 1); // +1 is necessary here 
        highlighter.setTextFragmenter(fragmenter);
                
        try {
            String highlighted = highlighter.getBestFragment(tokenStream, originalText);
            if (highlighted != null) {

                // replace highlight placeholders with correct prefix and suffix
            	highlighted = WGUtils.strReplace(highlighted, prefixPlaceholder, prefix, true);
            	highlighted = WGUtils.strReplace(highlighted, suffixPlaceholder, suffix, true);
            	            	
                return Collections.singletonList(highlighted);
            } else {
                return Collections.singletonList(originalText);
            }
        } catch (IOException e) {
        	_wga.getLog().warn("Unable to highlight text bc. of exception '" + e.getMessage() + "'.");
            return Collections.singletonList(originalText);
        } catch (InvalidTokenOffsetsException e) {
        	_wga.getLog().warn("Unable to highlight meta text bc. of exception '" + e.getMessage() + "'.");
        	return Collections.singletonList(originalText);
		}                

    }
    
    
    /**
     * Returns the FieldIndexType for a specific field name
     * Use this method to find out about the lucene index configuration for a specific item name. It will lookup the lucene indexing rule that matches the given item name and returns its information as FieldIndexType object. 
     * The lucene rules used are those from the OpenWGA application that the current Lucene object has in context.
     * @param fieldName The field name
     */
    @CodeCompletion
    public FieldIndexType getFieldIndexType(String fieldName) throws WGException {
        LuceneIndexItemRule rule = _wga.getCore().getLuceneManager().retrieveLuceneConfig(_cx.db().getDbReference()).getMatchingItemRule(fieldName);
        if (rule != null) {
            return new FieldIndexType(rule);
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Returns the FileIndexType for a specific file name
     * Use this method to find out about the lucene index configuration for a specific file attachment name. It will lookup the lucene indexing rule that matches the given file name and returns its information as FileIndexType object. 
     * The lucene rules used are those from the OpenWGA application that the current Lucene object has in context.
     * @param fileName The file name
     */
    public FileIndexType getFileIndexType(String fileName) throws WGException {
        
        LuceneIndexFileRule rule = _wga.getCore().getLuceneManager().retrieveLuceneConfig(_cx.db().getDbReference()).getMatchingFileRule(fileName);
        if (rule != null) {
            return new FileIndexType(rule);
        }
        else {
            return null;
        }
        
        
    }
    
    /**
     * Runs the lucene index updater immediately which will process all currently pending indexing request.
     * The method returns after the indexing is completed and the index up-to-date. After a timeout of 10 seconds it throws an exception.
     * Use this after modifying content documents to stop execution until the modifications were indexed. 
     * @throws InterruptedException 
     */
    public void updateIndex() throws WGException, InterruptedException {
        _wga.getCore().getLuceneManager().updateIndex();
    }
    
    public List<String> bestFileFragments(int fragmentSize, int maxFragments) throws WGException {
        return bestFileFragments(fragmentSize, maxFragments, "", "");
    }

    public List<String> bestFileFragments(int fragmentSize, int maxFragments, String prefix, String suffix) throws WGException {
        return bestFileFragments(fragmentSize, maxFragments, prefix, suffix, null);
    }
   
    public List<String> bestFileFragments(int fragmentSize, int maxFragments, String prefix, String suffix, String encode) throws WGException {
        if (!_wga.getCore().isLuceneEnabled()) {
            _cx.addwarning("Unable to retrieve best file fragments - lucene is not enabled.");
            return Collections.emptyList();
        }
        
        if (_wga.database().db().getContentStoreVersion() < WGDatabase.CSVERSION_WGA5 || (_wga.database().db().getContentStoreVersion() == WGDatabase.CSVERSION_WGA5 && _wga.database().db().getContentStorePatchLevel() < 5)) {
            _cx.addwarning("bestFileFragments() is not supported on this content store version.");
            return Collections.emptyList();
        }

        org.apache.lucene.search.Query query = (org.apache.lucene.search.Query) _cx.gethttpsession().getAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY);
        if (query == null) {
            // no query in session
            return Collections.emptyList();
        }
        
        String filename = null;
        SearchDetails sd = _cx.getcontent().getSearchDetails();
        if (sd != null && sd instanceof LuceneSearchDetails) {
            filename = ((LuceneSearchDetails)sd).getFilename();
        }
        
        if (filename == null) {
            return Collections.emptyList();
        }
        
        if (encode == null) {
            encode = _wga.design().getTmlDefaultEncoding();
        }
                
        String prefixPlaceholder = "$HIGHLIGHT_PREFIX$";
        String suffixPlaceholder = "$HIGHLIGHT_SUFFIX$";
        SimpleHTMLFormatter formatter = new SimpleHTMLFormatter(prefixPlaceholder, suffixPlaceholder);

        // create highlighter
        Highlighter highlighter = _wga.getCore().getLuceneManager().createHighlighter(LuceneManager.INDEXFIELD_ALLCONTENT, query, formatter);
        
        // retrieve attachment text
        WGFileMetaData md = _cx.content().getFileMetaData(filename);
        if (md == null) {
            return Collections.emptyList();
        }
        
        BinaryFieldData textData = md.getPlainText();        
        if (textData == null) {
            return Collections.emptyList();
        }
        
        try {
            // TODO highlighter does not support streams - should we limit plaintext size here?
            Reader textReader = new InputStreamReader(textData.getInputStream());
            String text = IOUtils.toString(textReader);
            textReader.close();
            
            // create tokenstream
            TokenStream tokenStream = _wga.getCore().getLuceneManager().createTokenStream(text, _cx.content());
                    
            // create fragmenter
            Fragmenter fragmenter = new SimpleFragmenter(fragmentSize);
            highlighter.setTextFragmenter(fragmenter);            

            String[] highlighted = highlighter.getBestFragments(tokenStream, text, maxFragments);            
            if (highlighted != null) {
                List<String> list = new ArrayList<String>();
                for (int i=0; i < highlighted.length; i++) {
                    String fragment  = highlighted[i];              
                    if (encode != null) {
                        try {
                            fragment = _cx.multiencode(encode, fragment);
                        } catch (FormattingException e) {
                            _cx.addwarning("Unable to retrieve best fragments for file '" + filename + "' bc. of formating exception '" + e.getMessage() + "'.");
                            return Collections.emptyList();
                        }
                    }
                    fragment = WGUtils.strReplace(fragment, prefixPlaceholder, prefix, true);
                    fragment = WGUtils.strReplace(fragment, suffixPlaceholder, suffix, true);
                    list.add(fragment);
                }
                return list;
            } else {
                return Collections.emptyList();
            }
        } catch (Exception e) {
            _cx.addwarning("Unable to retrieve best fragments for file '" + filename + "' bc. of exception '" + e.getMessage() + "'.");
            return Collections.emptyList();
        }             
    }
}