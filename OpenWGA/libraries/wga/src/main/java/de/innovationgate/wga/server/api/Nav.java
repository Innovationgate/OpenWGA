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

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import de.innovationgate.utils.ConversionUtils;
import de.innovationgate.utils.IteratorWrapper;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGColumnSet;
import de.innovationgate.webgate.api.WGColumnSet.Term;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentList;
import de.innovationgate.webgate.api.WGContentNavigator;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGPageOrderSet;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * A tool object to create WebTML navigator results in Java.
 * 
 * Many methods to create navigator results receive a {@link Map} parameter "attributes" which can be used to parametrize the created result. It uses the names of <tml:navigator>-Attributes as keys and interprets them the same way. So filling it with an entry of key "role" and a value of "none" will disable the filtering for contents that are invisible to navigators. Specifying or omitting an attribute here has generally the same effect as it would have on <tml:navigator>. There are however these exceptions:
 * <ul>
 *   <li>Attribute "type" is not permitted. The navigator type is either determined by the method you call or its type parameter.
 *   <li>Attribute "exclude" is not permitted. Instead use the method exclude on created NavigatorResults
 *   <li>Attibute "pagesize" does not really limit the retrieved documents but only determines the fetch size of result pages from the backend. To limit your document output use skip() and/or getPage() on created NavigatorResults
 * </ul>
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Nav {
    
    public static final String NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS = "sourcesofincomingrelationgroups";
    public static final String NAVTYPE_SOURCESOFINCOMINGRELATIONS = "sourcesofincomingrelations";
    public static final String NAVTYPE_RELATIONGROUPTARGETS = "relationgrouptargets";
    public static final String NAVTYPE_VERSIONS = "versions";
    public static final String NAVTYPE_AREAS = "areas";
    public static final String NAVTYPE_LANGUAGES = "languages";
    public static final String NAVTYPE_SITEMAP = "sitemap";
    public static final String NAVTYPE_PATH = "path";
    public static final String NAVTYPE_PARENTS = "parents";
    public static final String NAVTYPE_SIBLINGS = "siblings";
    public static final String NAVTYPE_CHILDREN = "children";
    
    /**
     * An iterator implementation counting the index of returned results
     * @param <T> Type returned by the iterator
     */
    public class IndexCountingIterator<T> implements SkippingIterator<T>, IteratorWrapper<T> {

        private SkippingIterator<T> _parent;
        private int _index=0;
        private boolean _fetched = false;

        public int getIndex() {
            return _index;
        }

        public IndexCountingIterator(SkippingIterator<T> parent) {
            _parent = parent;
        }
        
        @Override
        public Iterator<T> getWrappedIterator() {
            return _parent;
        }

        @Override
        public boolean hasNext() {
            _fetched  = true;
            return _parent.hasNext();
        }

        @Override
        public T next() {
            _fetched = true;
            T next = _parent.next();
            _index++;
            return next;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int skip(int nrOfElements) {
            if (_fetched) {
                throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
            }
            
            int notSkipped = _parent.skip(nrOfElements);
            _index+=(nrOfElements- notSkipped);
            return notSkipped;
        }
        
    }

    /**
     * An iterator converting {@link WGRelationData} into {@link WGContent}
     */
    public static class RelationDataIterator implements SkippingIterator<WGContent> {

        private Iterator<WGRelationData> _it;
        private boolean _fetched = false;
        private boolean _returnsSources;

        public RelationDataIterator(Iterator<WGRelationData> it, boolean returnsSources) {
            _it = it;
            _returnsSources = returnsSources;
        }

        @Override
        public boolean hasNext() {
            _fetched  = true;
            return _it.hasNext();
        }

        @Override
        public WGContent next() {
            _fetched = true;
            try {
                WGRelationData relData = _it.next();
                if (_returnsSources) {
                    return relData.getParentContent();
                }
                else {
                    return relData.getTargetContent();
                }
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception retrieving target content of relation", e);
            }
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();

        }

        @Override
        public int skip(int nrOfElements) {
            if (_fetched) {
                throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
            }
            int skipped = 0;
            while (_it.hasNext() && skipped < nrOfElements) {
                _it.next();
                skipped++;
            }
            return nrOfElements - skipped;
        }

    }

    public static final String NAVATT_PAGESIZE = "pagesize";
    public static final String NAVATT_ALLAREAS = "allareas";
    public static final String NAVATT_MAXLEVEL = "maxlevel";
    public static final String NAVATT_ALLLANGUAGES = "alllanguages";
    public static final String NAVATT_ROLE = "role";
    public static final String NAVATT_RELATIONGROUP = "relationgroup";
    public static final String NAVATT_RELATION = "relation";
    public static final String NAVATT_CONTENTCLASS = "contentclass";
    public static final String NAVATT_ONLYPUBLISHED = "onlypublished";
    public static final String NAVATT_ORDER = "order";
    
    /**
     * An iterable converting {@link WGRelationData} into {@link WGContent}
     */
    protected static class RelationDataIterable implements Iterable<WGContent> {

        private List<WGRelationData> _rels;
        private boolean _returnsSources;

        public RelationDataIterable(List<WGRelationData> rels, boolean returnsSources) {
            _rels = rels;
            _returnsSources = returnsSources;
        }

        @Override
        public RelationDataIterator iterator() {
            return new RelationDataIterator(_rels.iterator(), _returnsSources);
        }
        
        public int getSize() {
            return _rels.size();
        }
        
    }

    protected class ResultFromContentNavigator extends NavigatorResult {
        
        private WGContentNavigator.IteratorCreator _it;
        private String _navType;
        private Context _reference;
        private boolean _onlyPublished;

        protected ResultFromContentNavigator(WGA wga, WGContentNavigator.IteratorCreator it, String navType, Context reference, boolean onlyPublished) {
            super(wga, null);
            _it = it;
            _navType = navType;
            _reference = reference;
            _onlyPublished = onlyPublished;
            
        }
        
        @Override
        public boolean isOnlyPublished() {
            return _onlyPublished;
        }

        @Override
        public SkippingIterator<Context> iterator() {
            WGContentNavigator.Iterator iterator = _it.iterator();
            return _wga.wrapIntoTMLContextIterator((SkippingIterator<WGContent>) iterator);
        }
        
        @Override
        protected String getDescription() {
            try {
                return _navType + " of " + _reference.getpath();
            }
            catch (WGAPIException e) {
                return _navType + " of unknown document";
            }
        }
        
        @Override
        public int getSize() throws WGException {
            if (_it instanceof WGContentNavigator.IteratorCreator) {
                return ((WGContentNavigator.IteratorCreator) _it).getSize();
            }
            else if (_it instanceof List<?>) {
                return ((List<?>) _it).size();
            }
            else {
                return -1;
            }
        }
        
    }
    
    private WGA _wga;
    private Context _context;
    
    
    protected static class NavTypeMetadata {
        
        private String _defaultRole;
        public String getDefaultRole() {
            return _defaultRole;
        }

        public boolean isDefaultOnlyPublished() {
            return _defaultOnlyPublished;
        }

        private boolean _defaultOnlyPublished;
        
        public NavTypeMetadata(String defaultRole, boolean defaultOnlyPublished) {
            _defaultRole = defaultRole;
            _defaultOnlyPublished = defaultOnlyPublished;
        }
        
        
        
    }
    
    protected class ResultFromContentList extends NavigatorResult {
        
        private List<WGContent> _list;
        private String _navType;
        private Context _reference;
        private boolean _onlyPublished;
    
        protected ResultFromContentList(WGA wga, List<WGContent> list, String navType, Context reference, boolean onlyPublished) {
            super(wga, null);
            _list = list;
            _navType = navType;
            _reference = reference;
            _onlyPublished = onlyPublished;
            
        }
        
        @Override
        public boolean isOnlyPublished() {
            return _onlyPublished;
        }
    
        @Override
        public IndexCountingIterator<Context> iterator() {
            Iterator<WGContent> iterator = new SkippingIteratorWrapper<WGContent>(_list.iterator());
            return new IndexCountingIterator<Context>(_wga.wrapIntoTMLContextIterator((SkippingIterator<WGContent>) iterator));
        }
        
        @Override
        protected String getDescription() {
            try {
                return _navType + " of " + _reference.getpath();
            }
            catch (WGAPIException e) {
                return _navType + " of unknown document";
            }
        }
        
        @Override
        public int getSize() throws WGException {
            return _list.size();
        }
        
    }

    protected class ResultFromRelationDataIterable extends NavigatorResult {
        
        private RelationDataIterable _iterable;
        private String _navType;
        private Context _reference;
        private boolean _onlyPublished;
    
        protected ResultFromRelationDataIterable(WGA wga, RelationDataIterable iterable, String navType, Context reference, boolean onlyPublished) {
            super(wga, null);
            _iterable = iterable;
            _navType = navType;
            _reference = reference;
            _onlyPublished = onlyPublished;
            
        }
        
        @Override
        public boolean isOnlyPublished() {
            return _onlyPublished;
        }
    
        @Override
        public SkippingIterator<Context> iterator() {
            RelationDataIterator iterator = _iterable.iterator();
            return new IndexCountingIterator<Context>(_wga.wrapIntoTMLContextIterator(iterator));
        }
        
        @Override
        protected String getDescription() {
            try {
                return _navType + " of " + _reference.getpath();
            }
            catch (WGAPIException e) {
                return _navType + " of unknown document";
            }
        }
        
        @Override
        public int getSize() throws WGException {
            return _iterable.getSize();
        }
        
    }

    public static final Map<String,NavTypeMetadata> NAVTYPE_METADATA = new HashMap<String, NavTypeMetadata>();
    static {
        NAVTYPE_METADATA.put(NAVTYPE_CHILDREN, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_SIBLINGS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put("parent", new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_PARENTS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_PATH, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_SITEMAP, new NavTypeMetadata(WGContent.DISPLAYTYPE_SITEMAP, true));
        NAVTYPE_METADATA.put(NAVTYPE_LANGUAGES, new NavTypeMetadata(WGContent.DISPLAYTYPE_NONE, true));
        NAVTYPE_METADATA.put(NAVTYPE_AREAS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_VERSIONS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NONE, false));
        NAVTYPE_METADATA.put(NAVTYPE_RELATIONGROUPTARGETS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_SOURCESOFINCOMINGRELATIONS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        NAVTYPE_METADATA.put(NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS, new NavTypeMetadata(WGContent.DISPLAYTYPE_NAVIGATOR, true));
        
    }

    protected Nav(WGA wga, Context context) {
        _wga = wga;
        _context = context;
    }
    
    
    /**
     * Returns a NavigatorResult for a children navigator, just like <tml:navigator type="children"/>
     * @throws WGException
     */
    public NavigatorResult children() throws WGException {
        return navigate(NAVTYPE_CHILDREN, null);
    }
    
    /**
     * Returns a NavigatorResult for a children navigator, just like <tml:navigator type="children"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult children(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_CHILDREN, atts);
    }
    
    /**
     * Returns a NavigatorResult for a siblings navigator, just like <tml:navigator type="siblings"/>
     * @throws WGException
     */
    public NavigatorResult siblings() throws WGException {
        return navigate(NAVTYPE_SIBLINGS, null);
    }
    
    /**
     * Returns a NavigatorResult for a siblings navigator, just like <tml:navigator type="siblings"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult siblings(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_SIBLINGS, atts);
    }

    /**
     * Returns a NavigatorResult for a path navigator, just like <tml:navigator type="path"/>
     * @throws WGException
     */
    public NavigatorResult path() throws WGException {
        return navigate(NAVTYPE_PATH, null);
    }

    /**
     * Returns a NavigatorResult for a path navigator, just like <tml:navigator type="path"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */    
    public NavigatorResult path(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_PATH, atts);
    }
    
    /**
     * Returns a NavigatorResult for a sitemap navigator, just like <tml:navigator type="sitemap"/>
     * @throws WGException
     */
    public NavigatorResult sitemap() throws WGException {
        return navigate(NAVTYPE_SITEMAP, null);
    }
    
    /**
     * Returns a NavigatorResult for a sitemap navigator, just like <tml:navigator type="sitemap"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult sitemap(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_SITEMAP, atts);
    }
    
    /**
     * Returns a NavigatorResult for a languages navigator, just like <tml:navigator type="languages"/>
     * @throws WGException
     */
    public NavigatorResult languages() throws WGException {
        return navigate(NAVTYPE_LANGUAGES, null);
    }
    
    /**
     * Returns a NavigatorResult for a languages navigator, just like <tml:navigator type="languages"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult languages(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_LANGUAGES, atts);
    }

    /**
     * Returns a NavigatorResult for an areas navigator, just like <tml:navigator type="areas"/>
     * @throws WGException
     */
    public NavigatorResult areas() throws WGException {
        return navigate(NAVTYPE_AREAS, null);
    }

    /**
     * Returns a NavigatorResult for an areas navigator, just like <tml:navigator type="areas"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult areas(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_AREAS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a versions navigator, just like <tml:navigator type="versions"/>
     * @throws WGException
     */    
    public NavigatorResult versions() throws WGException {
        return navigate(NAVTYPE_VERSIONS, null);
    }

    /**
     * Returns a NavigatorResult for a versions navigator, just like <tml:navigator type="versions"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */    
    public NavigatorResult versions(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_VERSIONS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a navigator iterating over the targets of a relation group, just like <tml:navigator type="relationGroupTargets"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */    
    public NavigatorResult relationGroupTargets(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_RELATIONGROUPTARGETS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a navigator iterating over the targets of a relation group, just like <tml:navigator type="relationGroupTargets"/>
     * @param relationGroup Name of the relation group
     * @throws WGException
     */
    public NavigatorResult relationGroupTargets(String relationGroup) throws WGException {
        Map<String,Object> atts = new HashMap<String,Object>();
        atts.put(NAVATT_RELATIONGROUP, relationGroup);
        return navigate(NAVTYPE_RELATIONGROUPTARGETS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relations, just like <tml:navigator type="sourcesOfIncomingRelations"/>
     * This variant iterates over all source contents no matter the relation name or content class.
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelations() throws WGException {
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONS, new HashMap<String,Object>());
    }

    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relations, just like <tml:navigator type="sourcesOfIncomingRelations"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelations(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relations, just like <tml:navigator type="sourcesOfIncomingRelations"/>
     * @param relation Name of the relation which must point to the context document in order to be retrieved 
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelations(String relation) throws WGException {
        Map<String,Object> atts = new HashMap<String,Object>();
        atts.put(NAVATT_RELATION, relation);
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONS, atts);
    }

    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relations, just like <tml:navigator type="sourcesOfIncomingRelations"/>
     * @param relation Name of the relation which must point to the context document in order to be retrieved
     * @param contentClass Content class that a source content must belong to in order to be retrieved 
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelations(String relation, String contentClass) throws WGException {
        Map<String,Object> atts = new HashMap<String,Object>();
        atts.put(NAVATT_RELATION, relation);
        atts.put(NAVATT_CONTENTCLASS, contentClass);
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relation groups, just like <tml:navigator type="sourcesOfIncomingRelationGroups"/>
     * This variant iterates over all source contents no matter the relation group name or content class.
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelationGroups() throws WGException {
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS, new HashMap<String,Object>());
    }

    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relation groups, just like <tml:navigator type="sourcesOfIncomingRelationGroups"/>
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelationGroups(Map<String,Object> atts) throws WGException {
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relation groups, just like <tml:navigator type="sourcesOfIncomingRelationGroups"/>
     * @param relationGroup Name of the relation group which must point to the context document in order to be retrieved 
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelationGroups(String relationGroup) throws WGException {
        Map<String,Object> atts = new HashMap<String,Object>();
        atts.put(NAVATT_RELATIONGROUP, relationGroup);
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS, atts);
    }

    /**
     * Returns a NavigatorResult for a navigator iterating over the sources of incoming relation groups, just like <tml:navigator type="sourcesOfIncomingRelationGroups"/>
     * @param relationGroup Name of the relation group which must point to the context document in order to be retrieved
     * @param contentClass Content class that a source content must belong to in order to be retrieved 
     * @throws WGException
     */
    public NavigatorResult sourcesOfIncomingRelationGroups(String relationGroup, String contentClass) throws WGException {
        Map<String,Object> atts = new HashMap<String,Object>();
        atts.put(NAVATT_RELATIONGROUP, relationGroup);
        atts.put(NAVATT_CONTENTCLASS, contentClass);
        return navigate(NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS, atts);
    }
    
    /**
     * Returns a NavigatorResult for a WebTML navigator, just like <tml:navigator>
     * @param navType Type of the navigator, use constants NAVTYPE_...
     * @throws WGException
     */
    public NavigatorResult navigate(String navType) throws WGException {
        return navigate(navType, null);
    }

    /**
     * Returns a NavigatorResult for a WebTML navigator, just like <tml:navigator>
     * @param navType Type of the navigator, use constants NAVTYPE_...
     * @param atts Attributes to influence the navigator result. Use attribute names of <tml:navigator> as keys and the respective attribute values as values. Note however the exceptions to this usage documented on class level.
     * @throws WGException
     */
    public NavigatorResult navigate(String navType, Map<String,Object> atts) throws WGException {
        
        if (atts == null) {
            atts = Collections.emptyMap();
        }
        
        WGContentNavigator nav = createContentNavigator(navType, atts);
        int pageSize = ConversionUtils.getInteger(atts.get(NAVATT_PAGESIZE), 10);
        if (pageSize == 0) { // As pagesize 0 actually means: No paging. We nevertheless need a backend paging size.
            pageSize = 10; 
        }
        
        NavigatorResult result;
        navType = navType.toLowerCase();
        if (navType.equals(NAVTYPE_CHILDREN)) {
            result = new ResultFromContentNavigator(_wga, nav.createChildContentIterable(_context.content(), pageSize, getPageOrderExpression(atts)), "Children", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_SIBLINGS)) {
            result = new ResultFromContentNavigator(_wga, nav.createSiblingContentIterable(_context.content(), pageSize, getPageOrderExpression(atts)), "Siblings", _context, nav.isOnlyPublished());
        }
        else if (navType.equals("parent") || navType.equals(NAVTYPE_PARENTS)) {
            result = new ResultFromContentNavigator(_wga, nav.createParentContentIterable(_context.content(), pageSize, getPageOrderExpression(atts)), "Parents", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_PATH) ) {
            result = new ResultFromContentList(_wga, getPathListFor(_context.content(), nav, atts), "Path", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_SITEMAP)) {
            result = new ResultFromContentList(_wga, getSitemapListFor(_context.content(), nav, atts), "Sitemap", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_LANGUAGES)) {
            result = new ResultFromContentList(_wga, getLanguagesListFor(_context.content(), nav, atts), "Languages", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_AREAS)) {
            result = new ResultFromContentList(_wga, getAreasListFor(_context.content(), nav, atts), "Areas", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_VERSIONS)) {
            result = new ResultFromContentList(_wga, getVersionsListFor(_context.content(), atts), "Versions", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_RELATIONGROUPTARGETS)) {
            result = new ResultFromContentList(_wga, getRelGroupTargetsFor(_context.content(), atts), "Relation group targets", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_SOURCESOFINCOMINGRELATIONS)) {
            result = new ResultFromRelationDataIterable(_wga, getIncomingRelationSourcesFor(_context.content(), atts), "Sources of incoming relations", _context, nav.isOnlyPublished());
        }
        else if (navType.equals(NAVTYPE_SOURCESOFINCOMINGRELATIONGROUPS)) {
            result = new ResultFromRelationDataIterable(_wga, getIncomingRelationGroupSourcesFor(_context.content(), atts), "Sources of incoming relation groups", _context, nav.isOnlyPublished());
        }
        else {
            throw new WGAServerException("Unknown navigator type '" + navType + "'");
        }
        
        String exclude = (String) atts.get("exclude");
        if (exclude != null) {
            throw new WGAServerException("Attribute 'exclude' invalid for TMLScript navigators. Please use method filter() on the CollectionResult object instead");
        }
        
        return result;
        
    }


    private String getPageOrderExpression(Map<String, Object> atts) throws WGException {
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        if (orderExpression != null && _wga.isTMLContextAvailable()) {
            return "[" + _wga.tmlcontext().content().getLanguage().getName() + "]" + orderExpression;
        }
        else {
            return null;
        }
    }
    
    private RelationDataIterable getIncomingRelationSourcesFor(WGContent content, Map<String, Object> atts) throws WGAPIException, WGException {

        String relation = (String) atts.get(NAVATT_RELATION);
        String contentClass = (String) atts.get(NAVATT_CONTENTCLASS);
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        return new RelationDataIterable(content.getIncomingRelations(contentClass, relation, !isCollectionsShowReleasedOnly(), orderExpression), true);
        
    }
    
    private RelationDataIterable getIncomingRelationGroupSourcesFor(WGContent content, Map<String, Object> atts) throws WGAPIException, WGException {

        String relgroup = (String) atts.get(NAVATT_RELATIONGROUP);
        String contentClass = (String) atts.get(NAVATT_CONTENTCLASS);
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        return new RelationDataIterable(content.getIncomingRelationsOfGroup(contentClass, relgroup, !isCollectionsShowReleasedOnly(), orderExpression), true);
        
    }

    private List<WGContent> getRelGroupTargetsFor(WGContent content, Map<String, Object> atts) throws WGException {

        String relGroup = (String) atts.get(NAVATT_RELATIONGROUP);
        if (relGroup == null) {
            throw new WGAServerException("Attribute 'relationgroup' mandatory for navigation type 'relgrouptargets'");
        }
        
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        return content.getRelationsOfGroup(relGroup, orderExpression);
    }

    private WGContentList getPathListFor(WGContent content, WGContentNavigator navigator, Map<String,Object> atts) throws WGAPIException {
        
        List<WGContent> contents = new ArrayList<WGContent>();
        
        while (content != null) {
            if (content.isVisibleFor(navigator.getDisplayType())) { // Necessary for first content
                contents.add(content);
            }
            content = navigator.getParentContent(content);
        }
        
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        if (orderExpression != null) {
            sortByOrderExpression(contents, orderExpression);
        }
        else {
            java.util.Collections.reverse(contents);
        }
        return WGContentList.create(contents);
    }
    
    private WGContentList getSitemapListFor(WGContent content, WGContentNavigator navigator, Map<String,Object> atts) throws WGAPIException {
        
        int maxLevel = ConversionUtils.getInteger(atts.get(NAVATT_MAXLEVEL), Integer.MAX_VALUE);
        boolean allAreas = ConversionUtils.getBoolean(atts.get(NAVATT_ALLAREAS), false);
        
        List<WGContent> contents = new ArrayList<WGContent>();
        
        if (allAreas == false) {
            WGContent rootContent;
            
            if (content.isDummy() || !content.hasCompleteRelationships()) {
                rootContent = content.getDatabase().getFirstReleasedContent(content.getLanguage().getName(), true);
            }
            else {
                rootContent = navigator.getRootContent(content.getStructEntry().getArea());
            }
            
            if (rootContent == null) {
                return new WGContentList();
            }
            
            Iterator<WGContent> rootSiblings = navigator.collectRelevantContents(rootContent.getStructEntry().getSiblingEntries()).iterator();
            while (rootSiblings.hasNext()) {
                WGContent siblingContent = (WGContent) rootSiblings.next();
                retrieveHierarchy(siblingContent, contents, navigator, 1, maxLevel);
            }
            
        }
        else {

            Iterator<WGArea> areas = content.getDatabase().getAreas().iterator();
            WGArea area;
            while (areas.hasNext()) {
                area = areas.next();
                if (area.isSystemArea()) {
                    continue;
                }
                
                WGContent rootContent = navigator.getRootContent(area);
                if (rootContent == null) {
                    continue;
                }
                
                Iterator<WGContent> rootSiblings = navigator.collectRelevantContents(rootContent.getStructEntry().getSiblingEntries()).iterator();
                while (rootSiblings.hasNext()) {
                    WGContent siblingContent = (WGContent) rootSiblings.next();
                    retrieveHierarchy(siblingContent, contents, navigator, 1, maxLevel);
                }
            }
        }
        
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        if (orderExpression != null) {
            sortByOrderExpression(contents, orderExpression);
        }
        
        return WGContentList.create(contents);
        
    }
    
    private void retrieveHierarchy(WGContent content, List<WGContent> contents, WGContentNavigator navigator, int level, int maxLevel) throws WGAPIException {
        contents.add(content);
        if (level < maxLevel) {
            WGContent childContent = navigator.getChildContent(content, 0);
            while (childContent != null) {
                retrieveHierarchy(childContent, contents, navigator, level + 1, maxLevel);
                childContent = navigator.getNextSibling(childContent);
            }
        }
    }
    
    private WGContentList getLanguagesListFor(WGContent relContent, WGContentNavigator navigator, Map<String,Object> atts) throws WGAPIException {
        boolean allLanguages = ConversionUtils.getBoolean(atts.get(NAVATT_ALLLANGUAGES), false);
        return navigator.getLanguagesContent(relContent, allLanguages);
    }
    
    private WGContentList getAreasListFor(WGContent content, WGContentNavigator navigator, Map<String,Object> atts) throws WGAPIException {
        
        List<WGContent> contents = new ArrayList<WGContent>();
        
        Iterator<WGArea> areas = content.getDatabase().getAreas().values().iterator();
        WGArea area;
        while (areas.hasNext()) {
            area = (WGArea) areas.next();
            content = navigator.getRootContent(area);
            if (content != null) {
                contents.add(content);
            }
            
        }
        
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        if (orderExpression != null) {
            sortByOrderExpression(contents, orderExpression);
        }
        
        return WGContentList.create(contents);
    }
    
    private WGContentList getVersionsListFor(WGContent content, Map<String,Object> atts) throws WGAPIException {
        WGContentList contents = content.getStructEntry().getAllContent(true);
        
        String orderExpression = (String) atts.get(NAVATT_ORDER);
        if (orderExpression != null) {
            sortByOrderExpression(contents, orderExpression);
        }
        
        return contents;
    }
    
    private WGContentNavigator createContentNavigator(String navType, Map<String,Object> atts) throws WGException {
        
        String theRole = (String) atts.get(NAVATT_ROLE);
        String defaultRole;
        NavTypeMetadata navtypeMetadata = NAVTYPE_METADATA.get(navType);
        if (navtypeMetadata != null) {
            defaultRole = navtypeMetadata.getDefaultRole();
        }
        else {
            defaultRole = WGContent.DISPLAYTYPE_NAVIGATOR;
        }
        
        WGContentNavigator navigator = new WGContentNavigator((theRole != null ? theRole : defaultRole), new WebTMLLanguageChooser(_context.db(), (TMLContext) _context));
        
        // Set if this navigator should only return published documents
        Object onlyPublished = atts.get(NAVATT_ONLYPUBLISHED);
        if (onlyPublished != null) {
            navigator.setOnlyPublished(ConversionUtils.getBoolean(onlyPublished, true));
        }
        if (!isCollectionsShowReleasedOnly()) {
            navigator.setOnlyPublished(false);
        }
        else {
            if (navtypeMetadata != null) {
                navigator.setOnlyPublished(NAVTYPE_METADATA.get(navType).isDefaultOnlyPublished());
            }
            else {
                navigator.setOnlyPublished(true);
            }
        }

        return navigator;
    }
    
    private boolean isCollectionsShowReleasedOnly() throws WGException {
        
        if (_context.iswebenvironment()) {
            if (!_context.isbrowserinterface() && !WGPDispatcher.isAuthoringMode(_context.db().getDbReference(), _wga.getRequest().getSession())) {
                return true;
            }
            
            Boolean colReleasedOnly = (Boolean) _wga.getHttpSession().getAttribute(WGACore.ATTRIB_BI_COLLECTIONS_SHOW_RELEASED_ONLY);
            if (colReleasedOnly != null && colReleasedOnly.booleanValue() == true) {
                return true;
            }
            else {
                return false;
            }
        }
        else {
            return true;
        }
        
        
    }
    
    private void sortByOrderExpression(List<WGContent> contents, String orderExpression) throws WGAPIException {
        
        final WGPageOrderSet order = WGPageOrderSet.parse(orderExpression); 
        final Collator collator;
        if(order.getContentLanguage() != null) {
            collator = Collator.getInstance(WGLanguage.languageNameToLocale(order.getContentLanguage()));
        }
        else {
            collator = Collator.getInstance(WGLanguage.languageNameToLocale(_context.db().getDefaultLanguage()));
        }
        Collections.sort(contents, new Comparator<WGContent>() {

            @Override
            public int compare(WGContent o1, WGContent o2) {

                for (WGColumnSet.Term term : order.getTerms()) {
                    
                    try {
                        Object value1 = fetchTermValue(o1, term);
                        Object value2 = fetchTermValue(o2, term);
                        int comparison = 0;
                        if (value1 instanceof String && value2 instanceof String) {
                            
                            if ("true".equals(term.getFlags().get("ci"))) {
                                value1 = ((String) value1).toLowerCase();
                                value2 = ((String) value2).toLowerCase();
                            }
                            
                            comparison = collator.compare((String) value1, (String) value2);
                            
                        }
                        else if (value1 instanceof Number && value2 instanceof Number) {
                            comparison = new Double(((Number) value1).doubleValue()).compareTo(((Number) value2).doubleValue());
                        }
                        else if (value1 instanceof Date && value2 instanceof Date) {
                            comparison = ((Date) value1).compareTo((Date) value2);
                        }
                        else if (value1 instanceof Boolean && value2 instanceof Boolean) {
                            comparison = ((Boolean) value1).compareTo((Boolean) value2);
                        }
                        
                        if (comparison != 0) {
                            if ("true".equals(term.getFlags().get("desc"))) {
                                comparison *= -1;
                            }
                            return comparison;
                        }
                    }
                    catch (WGAPIException e) {
                        throw new RuntimeException("Exception sorting by order expression", e);
                    }
                    
                    
                }
                
                return 0;
                
                
                
            }

            private Object fetchTermValue(WGContent o, Term term) throws WGAPIException {

                if (!Character.isUpperCase(term.getName().charAt(0))) {
                    return o.getItemValue(term.getName());
                }
                
                WGColumnSet.ColumnMeta meta = WGColumnSet.COLUMN_METAS.get(term.getName());
                if (meta == null) {
                    throw new WGIllegalArgumentException("Unknown column set meta: " + term.getName());
                }
                for (Class<? extends WGDocument> location : meta.getLocations()) {
                    if (location == WGContent.class) {
                        return o.getMetaData(meta.getMetaName());
                    }
                    else if (location == WGStructEntry.class) {
                        return o.getStructEntry().getMetaData(meta.getMetaName());
                    }
                    else if (location == WGContentType.class) {
                        return o.getStructEntry().getContentType().getMetaData(meta.getMetaName());
                    }
                    else if (location == WGLanguage.class) {
                        return o.getLanguage().getMetaData(meta.getMetaName());
                    }
                }
                
                return null;
                
            }
        });
        
        
    }

}
 