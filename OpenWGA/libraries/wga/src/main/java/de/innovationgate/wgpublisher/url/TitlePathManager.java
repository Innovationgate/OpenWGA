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
package de.innovationgate.wgpublisher.url;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.httpclient.URIException;
import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDatabaseConnectListener;
import de.innovationgate.webgate.api.WGDatabaseEvent;
import de.innovationgate.webgate.api.WGDatabaseEventListener;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGDocumentKey;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGStructEntryIterator;
import de.innovationgate.wgpublisher.ManagedDBAttribute;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;

/**
 * Object that takes care of various tasks around the "title path URL" new in WGA 4.1
 * <ul>
 * <li> Indicator that title path is active for a database, if it is present as database attribute
 * <li> Determine if a path is a title path URL
 * <li> Transforming normal titles to URL-compatible titles
 * </ul>
 */
public class TitlePathManager implements ManagedDBAttribute, WGDatabaseEventListener, WGDatabaseConnectListener {
    
    public static String SHARE_NAME_SPECIAL_CHARS = " _-?!(),.§%&*#'";
    
    public static String MODE_URL = "url";
    public static String MODE_SHARE = "share";
    
    public static String EXTDATA_TITLEPATH_CONFLICT = "titlepathconflict";
    
    public class RemainingPathElementException extends Exception {
        
        private static final long serialVersionUID = 1L;
        private WGContent _content;
        private PathTitle _remainingElement;

        public RemainingPathElementException(WGContent content, PathTitle remainingElement) {
            _content = content;
            _remainingElement = remainingElement;
        }

        public WGContent getContent() {
            return _content;
        }

        public PathTitle getRemainingElement() {
            return _remainingElement;
        }
        
    }

    public class TitlePathRoot {

        public static final int TYPE_AREA = 1;

        public static final int TYPE_CONTENT = 2;

        private int _type;

        private WGDocumentKey _key;

        public TitlePathRoot(WGDocumentKey key) {
            _key = key;
            _type = key.getDocType();
        }

        protected int getType() {
            return _type;
        }

        protected WGDocumentKey getKey() {
            return _key;
        }

        public WGDocument getDocument(WGDatabase db) throws WGAPIException {
        	return db.getDocumentByKey(_key);
        }

    }
    
    public abstract class PathTitle {
        
        public static final String INDEX_DIVIDER = "~";
        private Pattern _regexPattern;

        public abstract String getTitle();
        
        public abstract String getNormalizedTitle();

        public boolean matches(WGContent content) throws WGAPIException, UnsupportedEncodingException {

            Iterator contentTitles = getContentTitles(content).iterator();
            while (contentTitles.hasNext()) {
                String normalizedContentTitle = normalizeTitle((String) contentTitles.next());
                if (getNormalizedTitle().equals(normalizedContentTitle)) {
                    if (getIndex() != -1) {
                        if (content.getStructEntry().getSiblingIndex() == getIndex()) {
                            return true;
                        }
                    }
                    else {
                        return true;
                    }
                }
            }

            return false;

        }
        
        protected abstract List getContentTitles(WGContent content) throws WGAPIException;
        
        public boolean matchesWithWildcards(WGContent content) throws WGAPIException, UnsupportedEncodingException {
            Iterator contentTitles = getContentTitles(content).iterator();
            while (contentTitles.hasNext()) {
                String title = (String) contentTitles.next();
                if (matchesWithWildcards(title)) {
                    return true;
                }
            }

            return false;
        }
        
        public boolean matchesWithWildcards(String title) {
            Pattern titlePattern = getRegexPattern();
            return titlePattern.matcher(title).matches();
        }
        
        

        private Pattern getRegexPattern() {
            
            if (_regexPattern == null) {
                String regexPattern = getTitle();
                
                // Convert the * pattern to .* for regex
                regexPattern = WGUtils.strReplace(regexPattern, "*", ".*", true);
                
                // Escape all characters that have special meanings in RegEx
                regexPattern = WGUtils.strReplace(regexPattern, ".", "\\.", true);
                regexPattern = WGUtils.strReplace(regexPattern, "(", "\\(", true);
                regexPattern = WGUtils.strReplace(regexPattern, ")", "\\)", true);
                
                
                _regexPattern = Pattern.compile(regexPattern);
            }
            
            return _regexPattern;
            
        }
        
        public abstract int getIndex();
        
        public String toString() {

            if (getIndex() != -1) {
                return getNormalizedTitle() + INDEX_DIVIDER + getIndex();
            }
            else {
                return getNormalizedTitle();
            }

        }
        
        /**
         * Indirection method that should redirect to normalizeURLTitle or normalizeJCRTitle, what is appropriate for the concrete PathTitle implementation 
         * @param title
         * @return
         * @throws UnsupportedEncodingException 
         */
        public abstract String normalizeTitle(String title) throws UnsupportedEncodingException;
        
        

    }

    public class URLPathTitle extends PathTitle {

   

        private String _title;

        private int _index = -1;

        private String _normalizedTitle;

        public URLPathTitle(String title) {

            int tildePos = title.indexOf(INDEX_DIVIDER);
            if (tildePos != -1) {
                _title = title.substring(0, tildePos);
                _index = Integer.parseInt(title.substring(tildePos + 1));
            }
            else {
                _title = title;
            }

            _normalizedTitle = normalizeTitle(_title);
        }

        public URLPathTitle(String title, int index) {
            _title = title;
            _index = index;

            _normalizedTitle = normalizeTitle(_title);
        }

        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.url.PathTitle#getTitle()
         */
        public String getTitle() {
            return _title;
        }

        public int getIndex() {
            return _index;
        }

        public String getNormalizedTitle() {
            return _normalizedTitle;
        }



        public String normalizeTitle(String title) {
            return normalizeURLTitle(title);
        }

        protected List getContentTitles(WGContent content) throws WGAPIException {
            return Collections.singletonList(content.getTitle());
        }

    }
    
    public class SharePathTitle extends PathTitle {

        private String _title;

        private int _index = -1;

        private String _normalizedTitle;

        public SharePathTitle(String title) throws UnsupportedEncodingException {

            _title = title;
            _normalizedTitle = normalizeTitle(_title);
        }

        /* (non-Javadoc)
         * @see de.innovationgate.wgpublisher.url.PathTitle#getTitle()
         */
        public String getTitle() {
            return _title;
        }

        public int getIndex() {
            return _index;
        }

        public String getNormalizedTitle() {
            return _normalizedTitle;
        }

        public String normalizeTitle(String title) throws UnsupportedEncodingException {
            return normalizeShareTitle(title);
        }

        protected List getContentTitles(WGContent content) throws WGAPIException {
            List titles = new ArrayList();
            titles.add(content.getTitle());
            if (content.getFileNames().size() > 0) {
                titles.add(content.getFileNames().get(0));
            }
            return titles;
            
        }

    }

    public class TitlePath {

        private TitlePathRoot _root;
        private String _rootKey;
        private String _mediaKey;
        private String _structKey = null;
        private String _language = null;
        private List<String> _titles;
        private List<String> _encodedTitles;

        public TitlePathRoot getTrigger() {
            return _root;
        }

        public String getMediaKey() {
            return _mediaKey;
        }

        public List<String> getTitles() {
            return _titles;
        }

        public void setRoot(TitlePathRoot trigger) {
            _root = trigger;
        }

        public void setMediaKey(String mediaKey) {
            _mediaKey = mediaKey;
        }

        public void setTitles(List<String> titles) {
            _titles = titles;
            _encodedTitles = new ArrayList();
            for (String title : titles) {
                try {
                    _encodedTitles.add(_core.getURLEncoder().encodePathPart(title));
                }
                catch (URIException e) {
                    _core.getLog().error("Exception encoding URL titles", e);
                }
            }
        }

        public String getPath() {
            
            if (_structKey != null) {
                return _structKey;
            }
            else {
                StringBuffer path = new StringBuffer();
                path.append(getRootKey().toString()).append("/");
                path.append(WGUtils.serializeCollection(_titles, "/"));
                return path.toString();
            }
        }

        public String getLanguage() {
            return _language;
        }

        protected void setLanguage(String language) {
            _language = language;
        }

        public String getRootKey() {
            return _rootKey;
        }

        public void setRootKey(String rootKey) {
            _rootKey = rootKey;
        }

        public String getStructKey() {
            return _structKey;
        }

        public void setStructKey(String structKey) {
            _structKey = structKey;
        }

        public List<String> getEncodedTitles() {
            return _encodedTitles;
        }

    }

    private Map triggerNames = new HashMap();

    private WGACore _core;

    private String _shortcutArea;
    
    private boolean _generateTitlePathURLs = false;

    private boolean _allowAllCharacters = true;
    private boolean _allowMixedLang = false;

    private boolean _contentIndexing;

    private WGDatabase _db;
    
    private boolean _allowUmlaute;

    private boolean _includeKeys;
    private boolean _useStructKeysInPath;
    private boolean _enhancedFormat;
    
    public PathTitle parseURLPathTitle(String title) throws UnsupportedEncodingException {
        return new URLPathTitle(title);
    }
    
    public PathTitle parseSharePathTitle(String title) throws UnsupportedEncodingException {
        return new SharePathTitle(title);
    }

    public TitlePathManager(WGDatabase db, WGACore core, boolean generateTitlePathURLs) throws WGAPIException, UnsupportedEncodingException {
        _core = core;
        _shortcutArea = (String) db.getAttribute(WGACore.DBATTRIB_TITLEPATHURL_SHORTCUTAREA);
        _allowAllCharacters = db.getBooleanAttribute(WGACore.DBATTRIB_SHARE_ALLOWALLCHARS, true);
        _allowMixedLang = db.getBooleanAttribute(WGACore.DBATTRIB_TITLEPATHURL_MIXEDLANGUAGES, false);
        _generateTitlePathURLs = generateTitlePathURLs;
        _contentIndexing = db.getBooleanAttribute(WGACore.DBATTRIB_TITLEPATHURL_CONTENTINDEXING, false);
        _includeKeys = db.getBooleanAttribute(WGACore.DBATTRIB_TITLEPATHURL_INCLUDEKEYS, false);
        _useStructKeysInPath = db.getBooleanAttribute(WGACore.DBATTRIB_TITLEPATHURL_USESTRUCTKEYS, false);
        _allowUmlaute = db.getBooleanAttribute(WGACore.DBATTRIB_TITLEPATHURL_ALLOW_UMLAUTE, false);
        _enhancedFormat = db.getBooleanAttribute(WGACore.DBATTRIB_TITLEPATHURL_ENHENCED_FORMAT, false);
        _db = db;
        
        if(_shortcutArea!=null && db.getArea(_shortcutArea)==null){
        	// this warning should only logged once.
            _core.getLog().warn("Title path URL shortcut area '" + _shortcutArea + "' does not (yet) exist in database " + db.getDbReference());
        }
        
        db.addDatabaseEventListener(this);
        
        if (generateTitlePathURLs) {
            if (db.isConnected()) {
                updateRootNames();
            }
            else {
                db.addDatabaseConnectListener(this);
            }
        }
    }

    public void close() {
    }

    public void databaseUpdate(WGDatabaseEvent event) {
        try {
            WGDatabase db = event.getDatabase();
            if (event.getEditedDocument() == null) {
                updateRootNames();
            }
            else if (event.getEditedDocument() instanceof WGArea) {
                updateRootNames();
            }
            else if (_shortcutArea != null) {
                if (event.getEditedDocument() instanceof WGStructEntry) {
                    WGStructEntry entry = (WGStructEntry) event.getEditedDocument();
                    if (entry.isRoot() && entry.getArea().getName().equals(_shortcutArea)) {
                        updateRootNames();
                    }
                }
                else if (event.getEditedDocument() instanceof WGContent) {
                    WGContent content = (WGContent) event.getEditedDocument();
                    WGStructEntry entry = content.getStructEntry();
                    if (entry.isRoot() && entry.getArea().getName().equals(_shortcutArea)) {
                        updateRootNames();
                    }
                }
            }
        }
        catch (Exception e) {
            Logger.getLogger("wga.titlepath").error("Error updating title path manager", e);
        }
    }

    private void updateRootNames() throws WGAPIException, UnsupportedEncodingException {

        Map newNames = new HashMap();
        Iterator namesIt = _db.getAreas().keySet().iterator();
        while (namesIt.hasNext()) {
            String name = (String) namesIt.next();
            newNames.put(normalizeURLTitle(name), new TitlePathRoot(_db.getArea(name).getDocumentKeyObj()));
        }

        // Eventually parse shortcut area
        if (_shortcutArea != null) {
            parseShortcutArea(_db, newNames);
        }

        triggerNames = newNames;

    }

    private void parseShortcutArea(WGDatabase db, Map newNames) throws WGAPIException, UnsupportedEncodingException {
        WGArea area = db.getArea(_shortcutArea);
        if (area == null) {
            return;
        }

        Iterator rootStructs = area.getRootEntries().iterator();
        while (rootStructs.hasNext()) {
            WGStructEntry root = (WGStructEntry) rootStructs.next();
            Iterator contents = root.getContentSet(false).getReleasedContent().values().iterator();
            while (contents.hasNext()) {
                WGContent content = (WGContent) contents.next();
                newNames.put(normalizeURLTitle(content.getTitle()) + "/" + content.getLanguage().getName(), new TitlePathRoot(content.getDocumentKeyObj()));
                newNames.put(normalizeURLTitle(content.getTitle()), new TitlePathRoot(content.getDocumentKeyObj()));
            }
        }
    }

    public String normalizeURLTitle(String title) {

        title = title.toLowerCase();

        if(_includeKeys || _enhancedFormat){
        	// OpenWGA 7.9.3
        	// #00005772
    		if(!_allowUmlaute){
    			title = title.replaceAll("ä", "ae")
    					.replaceAll("ö", "oe")
    					.replaceAll("ü", "ue")
    					.replaceAll("ß", "ss");
    		}
    		return title.replaceAll("[^äöüßa-z0-9_()\\[\\]]+", "-");
    	}
    	

        StringBuffer name = new StringBuffer();
        for (int i = 0; i < title.length(); i++) {
            char c = title.charAt(i);

            // Modified chars
            if (c == ' ') {
                name.append(_includeKeys ? '-' : '_');
                // In openwga 7.5.1 we changed this from '_' to '-'. But we only do it if keys are included to avoid 404s from search engines.
            }
            
            else if (c == 'ä' && !_allowUmlaute) {
                name.append("ae");
            }
            else if (c == 'ö' && !_allowUmlaute) {
                name.append("oe");
            }
            else if (c == 'ü' && !_allowUmlaute) {
                name.append("ue");
            }
            else if (c == 'ß' && !_allowUmlaute) {
                name.append("ss");
            }

            // Unmodified chars
            else if (Character.isLetterOrDigit(c)) {
                name.append(c);
            }
            else if (c=='-' || c=='_') {
                name.append(c);
            }

            // All other chars are omitted

        }

        try {
            return _core.getURLEncoder().encodePathPart(name.toString());
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    public String normalizeShareTitle(String title) throws UnsupportedEncodingException {

        // Need to "normalize" String cause MacOS-DAV uses "decomposed" UTF-8 which breaks strings on database storage
        // title = com.ibm.icu.text.Normalizer.compose(title, true);
        title = WGUtils.normalizeUnicode(title);
        
        StringBuffer name = new StringBuffer();
        
        for (int i = 0; i < title.length(); i++) {
            char c = title.charAt(i);
            
            // Unmodified chars
            if (Character.isLetterOrDigit(c)) {
                name.append(c);
            }
            else if (_allowAllCharacters || SHARE_NAME_SPECIAL_CHARS.indexOf(c) != -1) {
                name.append(c);
            }
            else {
                name.append('?');
            }
        }

        return name.toString();
    }

    public boolean isTemporary() {
        return false;
    }

    public TitlePath parseTitlePathURL(List path) throws UnsupportedEncodingException, WGAPIException {

        // Look if minimum path length is reached
        if (path.size() < 1) {
            return null;
        }

        // Look if we have a valid URLID on the last element
        String lastElement = (String) path.get(path.size() - 1);
        WGPDispatcher.URLID urlid = new WGPDispatcher.URLID(lastElement, _db, _includeKeys);
        if (urlid.getSuffix() == null || _core.getMediaKey(urlid.getSuffix()) == null) {
            return null;
        }
        
        if (urlid.isCompleteFormat()) {
            WGLanguage lang = _db.getLanguage(urlid.getLanguage());
            if (lang == null) {
                urlid.setCompleteFormat(false);
            }
        }
        
        // Look if an extra key is provided. If so we must skip the whole rest, as there is no need for a (valid) trigger title
        if (urlid.getResourceExtraKey() != null) {
            TitlePath url = new TitlePath();
            url.setMediaKey(urlid.getSuffix());
            url.setStructKey(urlid.getResourceExtraKey());
            url.setTitles(path);
            if (urlid.isCompleteFormat()) {
                url.setLanguage(urlid.getLanguage());
            }
            return url;
        }

        // Look if first element matches a trigger, first language qualified (contents of shortcut URLs) then unqualified
        String triggerText;
        if (path.size() == 1) {
            triggerText = urlid.getResourceId();
        }
        else {
            triggerText = (String) path.get(0);    
        }
        
        TitlePathRoot trigger = null;
        if (urlid.getLanguage() != null && path.size()==1) {
            trigger = (TitlePathRoot) triggerNames.get(normalizeURLTitle(triggerText) + "/" + urlid.getLanguage());
        }
        else trigger = (TitlePathRoot) triggerNames.get(normalizeURLTitle(triggerText));
        
        if (trigger == null) {
            return null;
        }
        
        // We have found a non-content trigger but have no further path to specify: This is no titlepath URL
        if (path.size() == 1 && trigger.getType() != WGDocument.TYPE_CONTENT) {
            return null;
        }

        // So we have a title path URL. Now we collect the data
        TitlePath url = new TitlePath();
        url.setRoot(trigger);
        url.setRootKey(triggerText);
        url.setMediaKey(urlid.getSuffix());
        url.setStructKey(urlid.getResourceExtraKey());
        if (urlid.isCompleteFormat()) {
            url.setLanguage(urlid.getLanguage());
        }

        // Build list of pure document titles, excluding area and mediaKey/language suffix
        List docTitles = new ArrayList();
        if (path.size() > 1) {
            if (path.size() > 2) {
                docTitles.addAll(path.subList(1, path.size() - 1));
            }
            docTitles.add(urlid.getResourceId());
        }
        url.setTitles(docTitles);
        return url;

    }

    public static WGContent findContent(PathTitle title, WGDocument parent, WGLanguageChooser chooser) throws WGAPIException, UnsupportedEncodingException {

        WGStructEntryIterator entries = fetchCandidateEntriesIterator(parent);

        WGContent foundContent = null;
        while (entries.hasNext()) {
            WGStructEntry entry = (WGStructEntry) entries.next();
            WGContent content = chooser.selectContentForPage(entry, false);
            if (content != null) {
                if (title.matches(content)) {
                    foundContent = content;
                    break;
                }
            }
        }

        return foundContent;

    }

    public PathTitle createPathTitle(WGContent content) throws WGAPIException {

        String title = content.getTitle();
        String normalizedTitle = normalizeURLTitle(title);
        int index = -1;

        if (_contentIndexing) {
            // Search if there are other contents on earlier siblings with identical
            // title
            // If so we must add the sibling index to the path title to make it
            // unique
            WGContent prevContent = content.getPreviousContent();
            while (prevContent != null) {
                if (normalizeURLTitle(prevContent.getTitle()).equals(normalizedTitle)) {
                    index = content.getStructEntry().getSiblingIndex();
                    break;
                }
    
                prevContent = prevContent.getPreviousContent();
            }
        }

        return new URLPathTitle(title, index);

    }

    protected String getShortcutArea() {
        return _shortcutArea;
    }

    public WGContent findContentByTitlePath(TitlePath titlePathURL, WGDatabase database, WGLanguageChooser chooser, String mode) throws WGAPIException,
            UnsupportedEncodingException, RemainingPathElementException {
        
        
        // If we have a struct key (by tilde suffix) we just will load the doc by its content key
        if (titlePathURL.getStructKey() != null) {
            WGStructEntry entry = database.getStructEntryByKey(database.parseStructKey(titlePathURL.getStructKey()));
            if(entry==null){
            	// may be a sequence?
            	try{
	            	long seq = Long.parseLong(titlePathURL.getStructKey(), 16);
	            	entry = database.getStructEntryBySequence(seq);
            	}
            	catch(Exception e){
            		// may be unable to parse structkey as long. Ignore any errors here.
            	}
            }
            if (entry != null) {
                WGContent content = LanguageBehaviourTools.getRelevantContent(entry, titlePathURL.getLanguage(), false);
                if (content != null) {
                    return content;
                }
            }
            // We may not use the other information. We would eventually land on the wrong document by titlepaths.
            return null;
        }
       
        // Find trigger document
        WGDocument triggerDoc = titlePathURL.getTrigger().getDocument(database);
        if (triggerDoc == null) {
            return null;
        }
    
        // No further path. Return trigger document (if it can be used)
        if (titlePathURL.getTitles().isEmpty()) {
            if (triggerDoc instanceof WGContent) {
                return (WGContent) triggerDoc;
            }
            else {
                return null;
            }
        }
        
        // Otherwise find entries by titles, relative to the trigger doc: Mode JCR
        else if (mode.equals(MODE_SHARE)) {
            return findSharePathContent(titlePathURL, chooser, mode, triggerDoc);
        }
        
        // Mode URL
        else {
            return findURLPathContent(titlePathURL, 0, chooser, triggerDoc);
        }

    }

    /**
     * Recursive variant to parse an Title Path URL. We must try each and every possible variant, so we must proceed recursively until we find a complete match
     * @param pathTitles
     * @param index
     * @param acceptedLanguages
     * @param parent
     * @return
     * @throws WGAPIException
     * @throws UnsupportedEncodingException
     * @throws RemainingPathElementException
     */
    private WGContent findURLPathContent(TitlePath titlePath, int index, WGLanguageChooser chooser, WGDocument parent) throws WGAPIException, UnsupportedEncodingException, RemainingPathElementException {
        
        PathTitle pathTitle = parseURLPathTitle((String) titlePath.getTitles().get(index));
        WGContent foundContent = null;
        
        // First try: Search for specific language if available
        if (titlePath.getLanguage() != null) {
            WGStructEntryIterator entries = fetchCandidateEntriesIterator(parent);
            while (entries.hasNext()) {
                WGStructEntry entry = (WGStructEntry) entries.next();
                
                // If language available: Try the addressed language from the path
                WGContent content = null;
                content = LanguageBehaviourTools.getRelevantContent(entry, titlePath.getLanguage(), false);
                if (content != null && pathTitle.matches(content)) {
                    WGContent descent = descendOnContent(titlePath, index, chooser, content);
                    if (descent != null) {
                        return descent;
                    }
                }
                
            }
        }
        
        // Second try: Search all languages, if no language given or we may have mixed paths
        if ((_allowMixedLang && index != titlePath.getTitles().size() - 1) || titlePath.getLanguage() == null) {
            WGStructEntryIterator entries = fetchCandidateEntriesIterator(parent);
            while (entries.hasNext()) {
                WGStructEntry entry = (WGStructEntry) entries.next();
    
                // Try all languages, priorized by language chooser
                for (WGContent content : chooser.selectContentPriorityOrder(entry, false)) {
                    if (pathTitle.matches(content)) {
                        WGContent descent = descendOnContent(titlePath, index, chooser, content);
                        if (descent != null) {
                            return descent;
                        }
                    }
                }
            }
        }

        // Nothing found on this sub-path. Calling methods should try remaining paths
        return null;

    }

    private static WGStructEntryIterator fetchCandidateEntriesIterator(WGDocument parent) throws WGAPIException {
        WGStructEntryIterator entries = null;
        if (parent instanceof WGArea) {
            entries = ((WGArea) parent).getRootEntryIterator(10);
        }
        else if (parent instanceof WGStructEntry) {
            entries = ((WGStructEntry) parent).getChildEntryIterator(10);
        }
        else if (parent instanceof WGContent) {
            entries = ((WGContent) parent).getStructEntry().getChildEntryIterator(10);
        }
        return entries;
    }

    private WGContent descendOnContent(TitlePath titlePath, int index, WGLanguageChooser chooser, WGContent content) throws WGAPIException, UnsupportedEncodingException, RemainingPathElementException {
        // We have a match. Look if the path goes further and we must recurse deeper
        if (index < (titlePath.getTitles().size()-1)) {
            return findURLPathContent(titlePath, index+1, chooser, content.getStructEntry());
        }
        
        // Else we have found a matching content for the complete path and we return it
        else {
            return content;
        }
    }

    private WGContent findSharePathContent(TitlePath titlePathURL, WGLanguageChooser chooser, String mode, WGDocument triggerDoc) throws UnsupportedEncodingException,
            WGAPIException, RemainingPathElementException {
        
        WGContent pathContent = null;
        Iterator pathTitles = titlePathURL.getTitles().iterator();
        WGDocument parent = null;
        
        
        while (pathTitles.hasNext()) {
            
            PathTitle pathTitle = parseSharePathTitle((String) pathTitles.next());
            
            if (pathContent != null) {
                parent = pathContent.getStructEntry();
            }
            else {
                parent = triggerDoc;
            }
            
            WGContent newContent = TitlePathManager.findContent(pathTitle, parent, chooser);
            if (newContent == null) {
                // In JCR mode the last element may address a JCR property. We throw a RemainingPathElementException then
                if (pathTitles.hasNext()) {
                    pathContent = null;
                }
                else {
                    throw new RemainingPathElementException(pathContent, pathTitle);
                }
                break;
            }
            else {
                pathContent = newContent;
            }
            
        }
        return pathContent;
    }

    public boolean isGenerateTitlePathURLs() {
        return _generateTitlePathURLs;
    }



    public void databaseConnected(WGDatabaseEvent event) {
        try {
            updateRootNames();
        }
        catch (Exception e) {
            _core.getLog().error("Exception updating root names for title path manager", e);
        }
    }

    public void databaseConnectionError(WGDatabaseEvent event) {
       
    }
    
    public List<String> buildTitlePath(WGContent baseContent, String mediaKey, WGLanguageChooser chooser) throws WGAPIException {
        List<String> path = new ArrayList<String>();
        
        // Build a path of all titles up to the root content
        WGContent content = baseContent;
        while (true) {
            
            // Title path conflict marked. We cannot use titlepath
            if (Boolean.TRUE.equals(content.getExtensionData(TitlePathManager.EXTDATA_TITLEPATH_CONFLICT))) {
                path = null;
                break;
            }
            
            String title = createPathTitle(content).toString();
            // Add the language and media key suffix if we are on first level
            String language = content.getLanguage().getName();
            if (!LanguageBehaviourTools.isMultiLanguageDB(content.getDatabase()) && language.equals(content.getDatabase().getDefaultLanguage())) {
                language = WGPDispatcher.DEFAULT_LANGUAGE_TOKEN;
            }
            if (content == baseContent) {
            	String customTitlepath = (String)content.getMetaData("titlepath");
            	boolean hasCustomTitlepath = (customTitlepath!=null && !customTitlepath.equals("")); 
                StringBuffer baseContentSuffix = new StringBuffer();
                
                if (_includeKeys) {
                	if(hasCustomTitlepath){
                		title = normalizeURLTitle(customTitlepath);
                	}
                	
                	if(_useStructKeysInPath)
                		baseContentSuffix.append("~").append(String.valueOf(content.getStructKey()));
                	else{
                		// use page sequence as key
                		// #00005772: use dot instead of tilde now
	                	long seq = content.getStructEntry().getPageSequence();
	                	if(seq!=0)
	                		baseContentSuffix.append(".").append(Long.toHexString(seq));
	                	else baseContentSuffix.append(".").append(String.valueOf(content.getStructKey()));
                	}
                	
                }
                baseContentSuffix.append(".").append(language).append(".").append(mediaKey);
                
                title += baseContentSuffix.toString();
                
                // if custom titlepath is set, we are finished:
                if(hasCustomTitlepath && _includeKeys){
                	path.add(title);
                	break;
                }
                
            }
            path.add(title);
            
            if (content.getStructEntry().isRoot()) {
                break;
            }
            
            // Go up in hierarchy
            // If we cannot build a complete hierarchy path (b.c. of no released contents on a node) we must cancel the title path creation
            WGContent parentContent = content.getParentContent(false);
            
            // Fallback to a language chosen by the language chooser
            if (parentContent == null && _allowMixedLang && chooser != null) {
                parentContent = chooser.selectContentForPage(content.getStructEntry().getParentEntry(), false);
            }
            
            // Cannot find parent content bc. no valid language. We cannot use titlepath.
            if (parentContent == null) {
                path = null;
                break;
            }
            
            content = parentContent;
            
        }
        
        if (path != null) {
            // Add area if the content is not a member of the shortcut area
            String areaName = baseContent.getStructEntry().getArea().getName();
            if (getShortcutArea() == null || !getShortcutArea().equals(areaName)) {
                path.add(normalizeURLTitle(areaName));
            }
            
            Collections.reverse(path);
        }
        
        return path;
    }
    
    public boolean isTitlePathAllowed(WGContent content) throws WGAPIException {
        
        // Deactivated title path
        if (!isGenerateTitlePathURLs()) {
            return false;
        }
        
        // Non-released content
        if (!content.getStatus().equals(WGContent.STATUS_RELEASE)) {
            return false;
        }
        
        return true;
        
        
        
    }

    public boolean isIncludeKeys() {
        return _includeKeys;
    }
}
