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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.IteratorWrapper;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentNavigator;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.Nav.IndexCountingIterator;
import de.innovationgate.wga.server.api.tml.Context;

/**
 * Represents an object carrying all resulting data of a document collection.
 */
public abstract class CollectionResult implements Iterable<Context> {
    
    /**
     * A filter to be applied to a CollectionResult via method {@link CollectionResult#filter(Filter)}
     */
    public interface Filter {
        
        /**
         * Checks if a document passes the filter
         * @param context A context pointing to the document
         * @return true if the document passes the filter, false if it should be omitted
         * @throws WGException
         */
        public boolean passesFilter(Context context) throws WGException;
    }
    
    /**
     * A wrapper that transforms Contextes in other result objects
     * @param <T> The target type for wrapping
     */
    public interface Wrapper<T> {
        
        /**
         * Wrap a context
         * @param context The context to wrap
         * @return The result object
         */
        public T wrap(Context context);
        
    }
    
    protected class WrappedIterator<T> implements SkippingIterator<T>, IteratorWrapper<Context> {

        private SkippingIterator<Context> _it;
        private Wrapper<T> _wrapper;

        public WrappedIterator(SkippingIterator<Context> it, Wrapper<T> wrapper) {
            _it = it;
            _wrapper = wrapper;
        }

        @Override
        public boolean hasNext() {
            return _it.hasNext();
        }

        @Override
        public T next() {
            return _wrapper.wrap(_it.next());
        }

        @Override
        public void remove() {
            _it.remove();
            
        }

        @Override
        public int skip(int nrOfElements) {
            return _it.skip(nrOfElements);
        }

        @Override
        public Iterator<? extends Context> getWrappedIterator() {
            return _it;
        }
        
    };
    
    protected static class FilteredResult extends CollectionResult {
        
        private Filter _filter;
        
        protected FilteredResult(WGA wga, CollectionResult parent, Filter filter) {
            super(wga, parent);
            _filter = filter;
        }
        
        @Override
        public SkippingIterator<Context> iterator() {
            return new ContextFilterIterator(_parent.iterator(), _filter);
        }
        
        @Override
        protected String getDescription() {
            return "Filter \"" + _filter.toString() + "\""; 
        }
        
    }

    protected static class SkippedResult extends CollectionResult {
    
        private int _start;
    
        protected SkippedResult(WGA wga, CollectionResult parent, int start) {
            super(wga, parent);
            _start = start;
        }
    
        @Override
        public SkippingIterator<Context> iterator() {
            SkippingIterator<Context> it = _parent.iterator();
            it.skip(_start);
            return it;
        }
        
        @Override
        protected String getDescription() {
            return "Skip " + WGUtils.DECIMALFORMAT_STANDARD.format(_start) + " elements";
        }
    
    }

    protected static class ContextFilterIterator extends PrefetchingIterator<Context> implements SkippingIterator<Context>, IteratorWrapper<Context> {
        
        private SkippingIterator<Context> _it;
        private CollectionResult.Filter _filter;
        private boolean _fetched = false;
    
        protected ContextFilterIterator(SkippingIterator<Context> it, CollectionResult.Filter filter) {
            _it = it;
            _filter = filter;
        }
    
        @Override
        protected Context fetchNextValue() {
           
            _fetched  = true;
            try {
                Context valueToReturn = null;
                while(_it.hasNext()) {
                    Context currentValue = _it.next();
                    if (_filter.passesFilter(currentValue)) {
                        valueToReturn = currentValue;
                        break;
                    }
                } 
                
                return valueToReturn;
                
            }
            catch (WGException e) {
                throw new RuntimeException("Exception filtering values", e);
            }
            
        }
    
        @Override
        public int skip(int nrOfElements) {
            
            if (_fetched) {
                throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
            }            
    
            return _it.skip(nrOfElements);
        }
    
        @Override
        public Iterator<Context> getWrappedIterator() {
            return _it;
        }
    
    }

    protected WGA _wga;
    protected CollectionResult _parent;

    protected CollectionResult(WGA wga, CollectionResult parent) {
        _wga = wga;
        _parent = parent;
    }
    
    /**
     * Creates a new {@link CollectionResult} containing only documents that pass the given filter
     * @param filter A collection result filter
     * @throws WGException
     */
    public CollectionResult filter(Filter filter) throws WGException {
        return new FilteredResult(_wga, this, filter);
    }
    
    /**
     * Creates a {@link CollectionResult} which skips the first documents in the current collection result 
     * @param size Number of documents to skip
     * @throws WGException
     */
    public CollectionResult skip(int size) throws WGException {
        return new SkippedResult(_wga, this, size);
    }
    
    /**
     * Returns the original {@link QueryResult} or {@link NavigatorResult}t of this collection result
     * @throws WGException
     */
    public CollectionResult getOriginalResult() throws WGException {
        if (_parent != null) {
            return _parent.getOriginalResult();
        }
        else {
            return this;
        }
    }
    
    /**
     * Returns a page of contents from the navigator result
     * @param start The starting number from which to retrieve, 1 being the first content. Note that this is the start position from the offset that may already been chosen by previous actions, like {@link #skip(int)}
     * @param size The number of contents to retrieve
     * @return List of contents as {@link Context} objects
     */
    public CollectionPage getPage(int start, int size) throws WGException {
        
        SkippingIterator<Context> it = iterator();
        it.skip(start-1);
        CollectionPage docs = new CollectionPage();
        for (int idx=0; idx<size; idx++) {
            if (!it.hasNext()) {
                break;
            }
            docs.add(it.next());
        }
        docs.setEndReached(!it.hasNext());
        docs.setEndIndex(determineCurrentIndex(it));
        return docs;
        
    }
    
    /**
     * Returns the index of the last returned result document from the given iterator that this document had on the original collection result. Index 1 is the first document.
     * @param it The iterator. Must be an iterator returned from a {@link CollectionResult} object
     */
    public int determineCurrentIndex(Iterator<?> it) {

        while (true) {
            if (it instanceof IndexCountingIterator<?>) {
               return (((IndexCountingIterator<?>) it).getIndex());
            }
            if (it instanceof WGContentNavigator.Iterator) {
                return (((WGContentNavigator.Iterator) it).getIndex());
            }
            else if (it instanceof CountReportingIterator<?>) {
                return (((CountReportingIterator<?>) it).getCurrentOffset());
            }
            
            if (it instanceof IteratorWrapper) {
                return determineCurrentIndex(((IteratorWrapper<?>) it).getWrappedIterator());
            }
            else {
                return -1;
            }
            
        }
        
    }

    /**
     * Returns a page of contents from the navigator result on its current offset (being 1 if no offset-modifying actions like {@link #skip(int)} have been used)
     * @param size The number of contents to retrieve
     * @return List of contents as {@link Context} objects
     */
    public CollectionPage getPage(int size) throws WGException {
        return getPage(1, size);
    }
    
    protected abstract String getDescription();
    
    /**
     * Returns the number of result documents in the collection 
     *  This is the number of results in the original collection, containing all documents that may be unreadable, not visible or skipped. The actual returned results may be lower.
     * @throws WGException
     */
    public int getSize() throws WGException {
        if (_parent != null) {
            return _parent.getSize();
        }
        else {
            return -1;
        }
    }
    
    @Override
    public abstract SkippingIterator<Context> iterator();
    
    @Override
    public String toString() {
        List<String> descriptions = new ArrayList<String>();
        CollectionResult nav = this;
        while (nav != null) {
            descriptions.add(nav.getDescription());
            nav = nav._parent;
        }
        Collections.reverse(descriptions);
        return WGUtils.serializeCollection(descriptions, " >> ");
        
    }
    
    /**
     * Creates a new {@link CollectionResult} omitting the given document
     * @param cx Context of the document to omit
     */
    public CollectionResult exclude(final Context cx) throws WGException {
        return filter(new Filter() {
            @Override
            public boolean passesFilter(Context context) throws WGException {
                return !context.equals(cx);
            }
        });
    }
    

    /**
     * Returns the first result content of the query
     * This is an effective way of returning the very first content document that is returned by the query if only this is important.
     * If the result contains no contents this method returns null.
     */

    public Context getFirstResult() throws WGException {
        WGContent con = getFirstResultContent();
        if (con != null) {
            return _wga.tmlcontext().context(con);
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns a single value of the first result content of the query.
     * This may be useful if your query only really returns a a single value (for example a document count, a sum etc.) which this method can directly retrieve
     * @param itemName The name of the item on the first result content, containing the desired value
     * @return The desired value or null if the item does not exist or there is no result content
     * @throws WGException
     */
    public Object getSingleValue(String itemName) throws WGException {
        WGContent con = getFirstResultContent();
        if (con != null) {
            return con.getItemValue(itemName);
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns the first result content of the query as WGAPI content object
     * This is an effective way of returning the very first content document that is returned by the query if only this is important.
     * If the result contains no contents this method returns null.
     */
    public WGContent getFirstResultContent() throws WGException {
        Iterator<Context> it = iterator();
        if (it.hasNext()) {
            return it.next().content();
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns an iterable whose iterators wrap/transform the returned documents into some custom object
     * @param wrapper A wrapping implementation
     * @return The wrapping iterable
     * @throws WGException
     */
    public <T> Iterable<T> wrap(final Wrapper<T> wrapper) throws WGException {
        
        return new Iterable<T>() {
            @Override
            public Iterator<T> iterator() {
                return new WrappedIterator<T>(CollectionResult.this.iterator(), wrapper);
            }
        };
        
    }
 

}
 