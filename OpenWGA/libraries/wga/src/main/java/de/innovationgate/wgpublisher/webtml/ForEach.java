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
package de.innovationgate.wgpublisher.webtml;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import de.innovationgate.utils.ObjectComparator;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAbstractResultSetIterator;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentNavigator;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.wga.server.api.CollectionResult;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.webtml.utils.CompareExpressionComparator;
import de.innovationgate.wgpublisher.webtml.utils.ContentComparator;
import de.innovationgate.wgpublisher.webtml.utils.ExpressionComparator;
import de.innovationgate.wgpublisher.webtml.utils.IterationTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.ResultIterator;
import de.innovationgate.wgpublisher.webtml.utils.ResultSetTagStatus;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;

public class ForEach extends Base implements IterationTag {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    public class FilterIterator<T> extends PrefetchingIterator<T>  {
	    
	    private Iterator<? extends T> _it;
        private TMLContext _dummyContext;
        private String _expression;
        private String _currentValueVar = null;

        public FilterIterator(String expression, String currentValueVar, Iterator<? extends T> it) throws WGAPIException {
	        _it = it;
	        _expression = expression;
	        if (currentValueVar != null) {
	            _currentValueVar = currentValueVar;
	        }
	        _dummyContext = getTMLContext().dbContext(getTMLContext().db());
	    }

        @Override
        protected T fetchNextValue() {
           
            try {
                
                T valueToReturn = null;
                while(_it.hasNext()) {
                    T currentValue = _it.next();
                    
                    TMLContext filterContext = _dummyContext;
                    Map<String, Object> additionalObjects = new HashMap<String, Object>();
                    if (currentValue instanceof WGContent) {
                        filterContext = getTMLContext().context((WGContent) currentValue);
                    }
                    else {
                        additionalObjects.put("currentValue", currentValue);
                        if (_currentValueVar != null) {
                            additionalObjects.put(_currentValueVar, currentValue);
                        }
                    }
                    
                    ExpressionResult result = ExpressionEngineFactory.getTMLScriptEngine().evaluateExpression(_expression, filterContext, ExpressionEngine.TYPE_EXPRESSION, additionalObjects);
                    if (result.isTrue()) {
                        valueToReturn = currentValue;
                        break;
                    }
                    else if (result.isError()) {
                        getTMLContext().getlog().error("Error executing filter expression", result.getException());
                        addExpressionWarning(_expression, result);
                    }
                } 
                
                return valueToReturn;
                
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception filtering values", e);
            }
            
        }

    }

    public static final String TAGINFO_CURRENTPAGE = "currentpage";
	public static final String TAGINFO_PAGES = "pages";
	public static final String TAGINFO_CURRENTVALUE = "currentvalue";
	public static final String TAGINFO_HASPREVIOUSPAGE= "haspreviouspage";
	public static final String TAGINFO_HASNEXTPAGE= "hasnextpage";
	public static final String TAGINFO_STARTINDEX = "startindex";
	public static final String TAGINFO_ENDINDEX = "endindex";
	public static final String TAGINFO_COUNT = "count"; 

	// attributes
	private String type;
	private String item;
	private String expression;
	private String relationgroup;
	private String count;
	private String sourceTag;
	private String pageSize;
	private String page;
	private String sortOrder;
	private String sortExpression;
	private String sortcomparison;
	
    public String getSortcomparison() {
        return getTagAttributeValue("sortcomparison", sortcomparison, null);
    }

    public void setSortcomparison(String sortcomparison) {
        this.sortcomparison = sortcomparison;
    }

    private String sortLanguage;
	private String currentvalue;
	private String filllastpage;
    private String onlypublished;
    private String linear;
	private String sortmeta;
	private String sortitem;
	private String filter;
	private String offset;
	
	public static class Status extends BaseTagStatus implements IterationTagStatus, DirectOutputCapableTag {
	    
    	int currentPage = 1;
    	Integer determinedPages = null;
    	int offset = 0;
    	Integer determinedLastDisplayedRow = null;
    	protected String forEachType;
    	
    	ResultIterator<? extends Object> resultIterator;
    	Object currentObject;
    	Integer determinedResultSize = null;
        String contentLanguage = null;
        
    	Object _varOverriddenValue = null;
    	boolean _varOverridden = false;
        int intPageSize = -1;
        boolean endReached;
        boolean onlyPublished;
        
        Boolean hasMoreResults = null;
        
        public int baseOffset = 0;
        Set<WGContentKey> docsToExclude = new HashSet<WGContentKey>();
        
        @Override
        public boolean isDirectOutputCapable() {
            return true;
        }
        
        /**
         * @see IterationTag#hasNextPage()
         */
        public boolean hasNextPage() {
            
            if (this.hasMoreResults != null) { // After the iteration the iterator was asked if there is really more (#00003231)
                return this.hasMoreResults;
            }
            
            if (this.currentPage < getPages() && !this.endReached) { // While still in the iteration we can only narrow on the answer to this question with math
                return true;
            }
            else {
                return false;
            }
        }

        /**
         * @see IterationTag#hasPreviousPage()
         */
        public boolean hasPreviousPage() {
            if (this.currentPage > 1) {
                return true;
            }
            else {
                return false;
            }
        }

        /**
         * @see IterationTag#isLastIteration()
         */
        public boolean isLastIteration() {
            return !this.resultIterator.hasNext() || (intPageSize > 0 && (iteration+1) > intPageSize);
        }

        /**
         * @see IterationTag#getIterationIndex()
         */
        public int getIterationIndex() {
        	return this.iteration;
        }
        
        @Override
        public Object getTagInfo(String name) throws WGAPIException {

            
            if (name.equals(TAGINFO_CURRENTPAGE)) {
                return new Integer(currentPage);
            }
            else if (name.equals(TAGINFO_PAGES)) {
                return getPages();
            }
            else if (name.equals(TAGINFO_CURRENTVALUE)) {
                return currentObject;
            }
            else if (name.equals(TAGINFO_HASNEXTPAGE)) {
                return new Boolean(hasNextPage());
            }
            else if (name.equals(TAGINFO_HASPREVIOUSPAGE)) {
                return new Boolean(hasPreviousPage());
            }
            else if (name.equals(TAGINFO_STARTINDEX)) {
                if (resultIterator.hasNext() || iteration > 0) {
                    return new Integer(offset + 1);
                }
                else {
                    return new Integer(0);
                }
            }
            else if (name.equals(TAGINFO_ENDINDEX)) {
                if (determinedLastDisplayedRow != null) {
                    return determinedLastDisplayedRow;
                }
                else {
                    return getResultSize();
                }
            }
            else if (name.equals(TAGINFO_COUNT)) {
                return getResultSize();
            }
            
            return super.getTagInfo(name);
        }

        public int getResultSize() {
            if (determinedResultSize != null) {
                return new Integer(determinedResultSize);
            }
            else {
                return resultIterator.getResultSize();
            }
        }

        public int getPages() {
            if (determinedPages != null) {
                return determinedPages;
            }
            else {
                return (int) Math.ceil((double) getResultSize() / intPageSize);
            }
        }
        
        @Override
        public void initAttributeDelegates(Base tag) {

            ForEach feTag = (ForEach) tag;
            this.forEachType = feTag.getType();
            
            // Autodetect some foreach types
            if (this.forEachType == null) {
                if (!WGUtils.isEmpty(feTag.getItem()) || !WGUtils.isEmpty(feTag.getExpression())) {
                    this.forEachType = "itemvalue";
                }
                else if (!WGUtils.isEmpty(feTag.getCount())) {
                    this.forEachType = "loop";
                }
                else {
                    this.forEachType = "content";
                }
            }
            
            this.forEachType = this.forEachType.toLowerCase().trim();
            
            String onlyPublishedStr = feTag.getOnlypublished();
            if (onlyPublishedStr != null) {
                this.onlyPublished = tag.stringToBoolean(onlyPublishedStr);
            }
            
            String strPageSize = feTag.getPageSize();
            if (strPageSize != null) {
                try {
                    this.intPageSize = WGUtils.parseInt(strPageSize);
                }
                catch (NumberFormatException exc) {
                    this.addWarning("Could not parse pageSize as number: " + strPageSize, true);
                }
            }
            
            super.initAttributeDelegates(tag);
        }

	}
	
	public static class OnlyVisibleIterator extends WGAbstractResultSetIterator<WGContent> {
        
        private Status _status;
        private TMLContext _context;

        public OnlyVisibleIterator(ResultIterator<WGContent> it, Status status, TMLContext context) {
            super(it);
            _status = status;
            _context = context;
        }
    
    
        @Override
        protected WGContent fetchContentForResult(WGContent result) throws WGAPIException {
            return result;
        }
    
        @Override
        protected boolean passesFilter(WGContent content) {
            try  {
                
                if (_status.docsToExclude.contains(content.getContentKey())) {
                    return false;
                }
                
                if (_status.onlyPublished && !content.mayBePublished(_context.isbrowserinterface(), WGContent.DISPLAYTYPE_NONE)) {
                    return false;
                }
                
                return true;
                
                
            }
            catch (WGAPIException e) {
                WGFactory.getLogger().error("Exception determining visibility state of " + content.getDocumentKey(), e);
                return false;
            }
        }
        
    }

    /**
     * This iterator will unwrap WGContent-containing objects into WGContent, leave everything else untouched
     */
    public static class ContentUnwrappingIterator implements SkippingIterator<Object> {
    
        private SkippingIterator<?> _it;
    
        public ContentUnwrappingIterator(Iterator<?> it) {
            _it = new SkippingIteratorWrapper<Object>(it);
        }
    
        @Override
        public boolean hasNext() {
            return _it.hasNext();
        }
    
        @Override
        public Object next() {
            Object obj = null;
            while (obj == null && _it.hasNext()) {
                obj = _it.next();
            } 
            if (obj == null && !_it.hasNext()) {
                throw new NoSuchElementException();
            }
            
            if (obj instanceof Context) {
                return ((Context) obj).content();
            }
            else {
                return obj;
            }
        }
    
        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    
        @Override
        public int skip(int nrOfElements) {
            return _it.skip(nrOfElements);
        }
    
    }

    @Override
	public BaseTagStatus createTagStatus() {
	    Status status = new Status();
	    return status;
	}
	
	   public Status getStatus() {
	        return (Status) super.getStatus();
	    }

	/**
	 * @throws WGAPIException 
	 * @see Base#tmlStartTag(TMLContext)
	 */
	public void tmlStartTag() throws WGException {
		
	    Status status = (Status) getStatus();
		// store tmlvariable with same name than currentvalue - used for restore in end tag
		if (!getTMLContext().getDesignContext().getVersionCompliance().isAtLeast(7,2) && getCurrentvalue() != null && getTMLContext().hasVariable(getCurrentvalue())) {
			status._varOverridden  = true;
			status._varOverriddenValue = getTMLContext().getvar(getCurrentvalue());
		}
		
		// Init private members
		status.currentPage = 1;
		status.determinedPages = null;
		status.offset = 0;
		status.baseOffset = 0;
		status.determinedResultSize = null;
		status.determinedLastDisplayedRow = null;
		status.endReached = false;
		status.resultIterator = null;
		status.currentObject = null;
		status.contentLanguage = null;
		
		// Get base offset
        String offsetStr = getOffset();
        if (offsetStr != null) {
            try {
                status.baseOffset  = WGUtils.parseInt(offsetStr) - 1;
                status.offset += status.baseOffset;
            }
            catch (NumberFormatException e) {
                this.addWarning("Could not parse offset attribute as number: " + offsetStr);
            }
        }
		
		// calculate page, pagesize and number of objects to display
		if (status.intPageSize > 0) {
			calcCurrentPage(status.intPageSize);
		}

		// get iteration items
		ResultIterator<? extends Object> objectIterator = retrieveResultIterator();
		
		// If no objects retrieved, exit tag
		if (objectIterator == null) {
			this.setEvalBody(false);
			return;
		}
		
		// sorting of entries - We must prefetch all contents here to be able to sort them in WebTML
		String sortOrder = this.getSortorder();
		if (sortOrder != null && !sortOrder.equals("none")) {
		    List<Object> objects = objectIterator.extractCompleteList();
			if (!status.forEachType.equals("content")) {
				valueSort(objects);
			}
			else {
				contentSort(objects);
			}
			
			if (sortOrder.startsWith("desc")) {
				Collections.reverse(objects);
			}
			
			objectIterator = new ResultIterator<Object>(objects);
		}
		
		// In case of type content wrap the iterator inside a FilterVisibleIterator() to filter out hidden docs and custom docs to exclude
		if (status.forEachType.equals("content")) {
		    @SuppressWarnings("unchecked")
            OnlyVisibleIterator onlyVisibleIterator = new OnlyVisibleIterator((ResultIterator<WGContent>) objectIterator, status, getTMLContext());
            objectIterator = new ResultIterator<WGContent>(onlyVisibleIterator, objectIterator);
		}
		
		// In case of a filter condition wrap it into a filter iterator
        String filterExpression = getFilter();
        if (filterExpression != null) {
            FilterIterator<Object> filterIterator = new FilterIterator<Object>(filterExpression, getCurrentvalue(), objectIterator);
            objectIterator = new ResultIterator<Object>(filterIterator, objectIterator);
        }

        // Define the result fetch strategy. Default is to ask the iterator.
        final ResultIterator<? extends Object> innerIterator = objectIterator;
        ResultIterator.ResultCount realResultSize = new ResultIterator.ResultCount() {
            @Override
            public int results() {
                return innerIterator.getResultSize();
            }
        };

        
        // Fill last page behaviour - Fetch the double pagesize and only use the last pageSize of docs of it. This should in most situations ensure that enough docs are retrieved
        
        if (status.intPageSize > 0 && stringToBoolean(getFilllastpage())) {
            int pageOffset = status.offset - status.baseOffset;
            int goBack = (pageOffset >= status.intPageSize ? status.intPageSize : pageOffset);
            int newOffset = status.offset - goBack;
            objectIterator.proceedToOffset(newOffset, stringToBoolean(getLinear()));
            
            List<Object> results = new ArrayList<Object>();
            for (int docsToFetch = 1; docsToFetch <= goBack + status.intPageSize; docsToFetch++) {
                if (!objectIterator.hasNext()) {
                    final int calculatedResultSize = newOffset + (docsToFetch - 1);
                    realResultSize = new ResultIterator.ResultCount() {
                        @Override
                        public int results() {
                            return calculatedResultSize;
                        }
                    };
                    break;
                }
                results.add(objectIterator.next());
            }
            status.hasMoreResults = objectIterator.hasNext();
            
            List<Object> subList;
            if (results.size() > status.intPageSize) {
                int startIndex = results.size() - status.intPageSize;
                subList = results.subList(startIndex, results.size());
                status.offset = newOffset + startIndex;
            }
            else {
                subList = results;
                status.offset = newOffset;
            }
            
            objectIterator = new ResultIterator<Object>(subList.iterator(), realResultSize);
        }
        
        // Else proceed directly to offset
        else {
            objectIterator.proceedToOffset(status.offset, stringToBoolean(getLinear()));
        }
        
        status.resultIterator = objectIterator;
		if (status.intPageSize > 0) {
		    status.determinedLastDisplayedRow = status.offset + status.intPageSize;
		}
 
		// Set the first value
		this.setEvalBody(this.proceedValues());

	}

    private void valueSort(List<Object> objects) {
        
        Collator collator = retrieveCollator();
        Comparator<Object> comparator = new ObjectComparator(collator);
        String currentValueName = getCurrentvalue();
        
        String sortExpression = getSortexpression();
        if (sortExpression != null) {
            ExpressionComparator exComparator = new ExpressionComparator(this, getSortExpressionEngine(), sortExpression, collator);
            exComparator.getValueInjectionNames().add("$VALUE");
            if (currentValueName != null) {
                exComparator.getValueInjectionNames().add(currentValueName);
            }
            
            comparator = exComparator;
        }
        
        String comparisonExpression = getSortcomparison();
        if (comparisonExpression != null) {
            List<String> names = new ArrayList<String>();
            names.add("$VALUE");
            if (currentValueName != null) {
                names.add(currentValueName);
            }
            
            comparator = new CompareExpressionComparator(this, getSortExpressionEngine(), comparisonExpression, collator, names);
        }
        
        java.util.Collections.sort(objects, comparator);
    }

    @SuppressWarnings("unchecked")
    private ResultIterator<? extends Object> retrieveResultIterator() throws WGException {
        ResultIterator<? extends Object> objectIterator = null;
        
        Status status = (Status) getStatus();
		if (status.forEachType.equals("content")) {
			ResultSetTagStatus tag;

			if (this.getSourcetag() != null) {
				tag = (ResultSetTagStatus) this.getTagStatusById(this.getSourcetag(), ResultSetTagStatus.class);
				if (tag == null) {
					throw new TMLException("Could not find content list tag with id " + this.getSourcetag(), true);
				}
			}
			else if (this.getRelationgroup() != null && getClass().equals(ForEach.class)) { // Backward compatibilty for legacy foreach type to iterate relation targets. May only be used on pure tml:foreach.
			    String group = getRelationgroup();
			    status.contentLanguage = getTMLContext().content().getLanguage().getName();
	            return new ResultIterator<WGContent>(getTMLContext().content().getRelationsOfGroup(group));
			}
			else if (getStatus() instanceof ResultSetTagStatus) {
				tag = (ResultSetTagStatus) getStatus();
			}
			else {
				tag = (ResultSetTagStatus) getStatus().getAncestorTag(ResultSetTagStatus.class);
				if (tag == null) {
					throw new TMLException("No content list tag specified", true);
				}
			}

			objectIterator = tag.getResultIterator();
            status.contentLanguage = tag.getResultLanguage();
			
		}
		else if (status.forEachType.equals("loop") || status.forEachType.equals("level")) {

			int count = 0;
			if (status.forEachType.equals("loop")) {
				String strCount = this.getCount();
				if (strCount == null) {
					throw new TMLException("Foreach with type loop but without attribute count", true);
				}
				count = stringToInteger(strCount, 0);
			}
			else if (status.forEachType.equals("level")) {
			    WGContentNavigator nav = new WGContentNavigator(null, new WebTMLLanguageChooser(getTMLContext().db(), getTMLContext()));
				count = nav.getContentLevel(this.getTMLContext().content()).intValue();
			} 
			List<Object> objects = new ArrayList<Object>();
			for (int idx = 0; idx < count; idx++) {
				objects.add(new Integer(idx + 1));
			}
			objectIterator = new ResultIterator<Object>(objects.iterator(), objects.size());
		}
		else if (status.forEachType.equals("itemvalue")) {
		    Iterable<?> values = null;
		    
		    String itemName = getItem();
		    if (itemName != null) {
    			values = this.getTMLContext().itemiterable(itemName);
    			if (values == null) {
    				throw new TMLException("Could not retrieve item " + itemName, true);
    			}
		    }
		    
		    String expression = getExpression();
		    if (expression != null) {
		        Object result = WGA.get(getTMLContext()).tmlscript().runExpression(getTMLContext(), expression);
		        if (result != null && result instanceof Iterable<?>) {
		            values = (Iterable<?>) result;		            
		        }
		        else {
		            throw new TMLException("Result of tml:foreach expression not suitable for iteration");
		        }
		    }
		    
		    if (values == null) {
		        throw new TMLException("Could not retrieve values for iteration");
		    }
            
		    final Iterable<?> theIterable = values;			
			objectIterator = new ResultIterator<Object>(new ContentUnwrappingIterator(theIterable.iterator()), new ResultIterator.ResultCount() {
                @Override
                public int results() {
                    try {
                        if (theIterable instanceof CollectionResult) {
                            return ((CollectionResult) theIterable).getSize();
                        }
                        else if (theIterable instanceof Collection<?>) {
                            return ((Collection<?>) theIterable).size();
                        }
                        else {
                            return 0;
                        }
                    }
                    catch (WGException e) {
                        return 0;
                    }
                }
            });
			
		}
		else if (status.forEachType.equals("fieldvalue")) {
			Object obj=null;
			// Register with form parent (if present) and retrieve item values from it;
			FormInputRegistrator form = (FormInputRegistrator) getStatus().getAncestorTag(FormBase.class);	
			if (form != null) 
				obj=form.getFieldValue(this.getItem(), false, "", false);
			else obj = this.getTMLContext().itemlist(this.getItem());
			
			if (obj == null) {
				throw new TMLException("Could not retrieve item " + this.getItem(), true);
			}
			if (obj instanceof List) {
				objectIterator = new ResultIterator<Object>((java.util.List<Object>) obj);
			}
			else {
				objectIterator = new ResultIterator<Object>(java.util.Collections.singletonList(obj));
			}
			
		}
		else if (status.forEachType.equals("tagresult")) {
			if (this.getSourcetag() == null) {
				throw new TMLException("No tag specified to take tagresult from", true);
			}
			BaseTagStatus tag = this.getTagStatusById(this.getSourcetag());
			if (tag == null) {
				throw new TMLException("Could not find tag with id " + this.getSourcetag(), true);
			}
			Object result = tag.result;
			if (de.innovationgate.utils.WGUtils.isCollection(result)) {
				objectIterator = new ResultIterator<Object>((java.util.Collection<Object>) result);
			}
			else {
				objectIterator = new ResultIterator<Object>(java.util.Collections.singletonList(result));
			}
		}
		else {
			throw new TMLException("Unknown type for foreach tag: " + status.forEachType, true);
		}
        return objectIterator;
    }

    private void calcCurrentPage(int pageSize) {
        
        Status status = (Status) getStatus();
        
        // Page attribute
        String strPage = this.getPage();
        if (strPage != null && strPage!="") {
        	try {
        		status.currentPage = WGUtils.parseInt(strPage);
        		status.offset += pageSize * (status.currentPage - 1);
        		return;
        	}
        	catch (NumberFormatException exc) {
        		this.addWarning("Could not parse page attribute as number: " + strPage);
        	}
        	
        }
        
        // WebTML-Variable
        String currentPageParam = this.getId() + "Page";
        try {
            String varName = "$" + currentPageParam;
            if (!getTMLContext().isempty(varName)) {
                Object pageObj = getTMLContext().item(varName);
                if (pageObj instanceof String) {
                    try {
                        status.currentPage = Integer.parseInt((String) pageObj);
                        status.offset += pageSize * (status.currentPage - 1);
                        return;
                    }
                    catch (NumberFormatException exc) {
                        this.addWarning("Could not parse page variable '" + varName + "' as number: " + String.valueOf(pageObj));
                    }
                }
                else if (pageObj instanceof Number) {
                    status.currentPage = ((Number) pageObj).intValue();
                    status.offset += pageSize * (status.currentPage - 1);
                    return;
                }
                else {
                    addWarning("Invalid data type for item '$" + currentPageParam + "' to specify foreach page: " + pageObj.getClass().getName());
                }
            }
        }
        catch (WGAPIException e) {
            log.error("Error retrieving foreach page item '$" + status.currentPage + "'", e);
        }
        
        // URL-Parameter
        strPage = this.pageContext.getRequest().getParameter(currentPageParam);
        if (strPage != null) {
            try {
                status.currentPage = Integer.parseInt(strPage);
                status.offset += pageSize * (status.currentPage - 1);
                return;
            }
            catch (NumberFormatException exc) {
                this.addWarning("Could not parse page url parameter as number: " + strPage);
            }
            
        }
    }

	private boolean proceedValues() throws WGException {
	    
	    Status status = (Status) getStatus();

	    if (status.intPageSize > 0 && (this.getIteration()+1) > status.intPageSize) {
	        return false;
	    }
	    
	    boolean hasValue = false;
		while (status.resultIterator.hasNext()) {
			status.currentObject = status.resultIterator.next();

			// Convert placeholder objects for contents
			if ("content".equals(status.forEachType) && status.currentObject instanceof WGRelationData) {
                status.currentObject = ((WGRelationData) status.currentObject).getParentContent();
            }
			
			try {
                this.getTMLContext().setvar("index", new Integer(this.getIteration()), false);
            }
            catch (WGAPIException e) {
                this.addWarning("Unable to set var 'index': " + e.getMessage(), false);
                this.getCore().getLog().error("Unable to set var 'index'.", e);
            }
            
			// Try to use the current object as a context, for changing the context inside the loop
			TMLContext targetContext = retrieveTargetContext(status.currentObject);
			if (targetContext instanceof TMLContext) {
				this.setChildTagContext(getTMLContextForDocument(targetContext.content())); // Convert to "native" TMLContext for the current design context
			}
			
			if (this.getCurrentvalue() != null) {
                try {
                    if (getTMLContext().getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
                        getTMLContext().setLocalVar(this.getCurrentvalue(), status.currentObject);
                    }
                    else {
                        getTMLContext().setvar(this.getCurrentvalue(), status.currentObject);
                    }
                }
                catch (WGAPIException e) {
                    this.addWarning("Unable to set currentvalue to var '"+ this.getCurrentvalue() + "': " + e.getMessage(), false);
                    this.getCore().getLog().error("Unable to set currentvalue to var '"+ this.getCurrentvalue() + "'.", e);
                }
			}
			
			hasValue = true;
			break;
			
		}
		
		if (!hasValue) {
		    status.determinedLastDisplayedRow = status.offset + getIteration();
		    status.determinedResultSize = status.determinedLastDisplayedRow;
		    status.determinedPages = status.currentPage;
		    status.endReached = true;
			return false;
		}
		else {
		    return true;
		}

	}

	private TMLContext retrieveTargetContext(Object currentObject) throws WGException {

	    if (currentObject instanceof TMLContext) {
	        return (TMLContext) currentObject;
	    }
	    
	    if (currentObject instanceof WGDocument) {
	        return getTMLContextForDocument((WGDocument) currentObject);
	    }
	    
	    TMLScript tmlScript = WGA.get(getTMLContext()).tmlscript();
	    if (tmlScript.isNativeObject(currentObject)) {
	        try {
	            if (tmlScript.hasProperty(currentObject, "context")) {
	                Object contextObj = tmlScript.callMethod(currentObject, "context");
	                if (contextObj instanceof TMLContext) {
	                    return (TMLContext) contextObj;
	                }
	            }
	            
	            return (TMLContext) tmlScript.descriptify(currentObject, Context.class);
	        }
	        catch (WGException e) {
	        }
	    }
	    
	    return null;
	    
    }

    /**
	 * Gets the count
	 * @return Returns a String
	 */
	public String getCount() {
		return this.getTagAttributeValue("count", count, null);
	}
	/**
	 * Sets the count
	 * @param count The count to set
	 */
	public void setCount(String count) {
		this.count = count;
	}

	/**
	 * Gets the item
	 * @return Returns a String
	 */
	public String getItem() {
		return this.getTagAttributeValue("item", item, "");
	}
	/**
	 * Sets the item
	 * @param item The item to set
	 */
	public void setItem(String item) {
		this.item = item;
	}

	/**
	 * Gets the pageSize
	 * @return Returns a String
	 */
	public String getPageSize() {
		return this.getTagAttributeValue("pagesize", pageSize, null);
	}
	/**
	 * Sets the pageSize
	 * @param pageSize The pageSize to set
	 */
	public void setPagesize(String pageSize) {
		this.pageSize = pageSize;
	}

	/**
	 * Gets the page
	 * @return Returns a String
	 */
	public String getPage() {
		return this.getTagAttributeValue("page", page, null);
	}
	/**
	 * Sets the pageSize
	 * @param pageSize The pageSize to set
	 */
	public void setPage(String page) {
		this.page = page;
	}

	/**
	 * Gets the type
	 * @return Returns a String
	 */
	public String getType() {
		return this.getTagAttributeValue("type", type, null);
	}
	/**
	 * Sets the type
	 * @param type The type to set
	 */
	public void setType(String type) {
		this.type = type;
	}

	/**
	 * Gets the sourceTag
	 * @return Returns a String
	 */
	public String getSourcetag() {
		return this.getTagAttributeValue("sourcetag", sourceTag, null);
	}
	/**
	 * Sets the sourceTag
	 * @param sourceTag The sourceTag to set
	 */
	public void setSourcetag(String sourceTag) {
		this.sourceTag = sourceTag;
	}

	/**
	 * @throws WGAPIException 
	 * @see Base#tmlAfterBody(TMLContext)
	 */
	public void tmlAfterBody() throws WGException {
		this.setEvalBody(this.proceedValues());
	}

	/**
	 * Gets the sortLanguage
	 * @return Returns a String
	 */
	public String getSortlanguage() {
		return this.getTagAttributeValue("sortlanguage", sortLanguage, null);
	}
	
	public String getXplanguage() {
		return this.getSortlanguage();
	}
	/**
	 * Sets the sortLanguage
	 * @param sortLanguage The sortLanguage to set
	 */
	public void setSortlanguage(String sortLanguage) {
		this.sortLanguage = sortLanguage;
	}
	
	public void setXplanguage(String sortLanguage) {
		this.setSortlanguage(sortLanguage);
	}

	/**
	 * Gets the sortExpression
	 * @return Returns a String
	 */
	public String getSortexpression() {
		return this.getTagAttributeValue("sortexpression", sortExpression, null);
	}
	/**
	 * Sets the sortExpression
	 * @param sortExpression The sortExpression to set
	 */
	public void setSortexpression(String sortExpression) {
		this.sortExpression = sortExpression;
	}

	/**
	 * Gets the sortOrder
	 * @return Returns a String
	 */
	public String getSortorder() {
		return this.getTagAttributeValue("sortorder", sortOrder, null);
	}
	/**
	 * Sets the sortOrder
	 * @param sortOrder The sortOrder to set
	 */
	public void setSortorder(String sortOrder) {
		this.sortOrder = sortOrder;
	}

	/**
	 * Gets the currentvalue
	 * @return Returns a String
	 */
	public String getCurrentvalue() {
		return this.getTagAttributeValue("currentvalue", currentvalue, null);
	}
	/**
	 * Sets the currentvalue
	 * @param currentvalue The currentvalue to set
	 */
	public void setCurrentvalue(String currentvalue) {
		this.currentvalue = currentvalue;
	}



	/**
	 * @throws WGException 
	 * @see de.innovationgate.wgpublisher.webtml.Base#tmlEndTag()
	 */
	public void tmlEndTag() throws TMLException, WGException {
		super.tmlEndTag();
		
		Status status = (Status) getStatus();
		if (status.hasMoreResults == null) {
		    status.hasMoreResults = status.resultIterator != null && status.resultIterator.hasNext();
		}
		status.currentObject = null;
		
		// restore tmlvariable with same name than currentvalue
		if (getCurrentvalue() != null) {
			if (status._varOverridden) {
				getTMLContext().setvar(getCurrentvalue(), status._varOverriddenValue);
			} else {
				getTMLContext().removevar(getCurrentvalue());
			}
		}
	}

	/**
	 * Returns the filllastpage.
	 * @return String
	 */
	public String getFilllastpage() {
		return this.getTagAttributeValue("filllastpage", filllastpage, "true");
	}

	/**
	 * Sets the filllastpage.
	 * @param filllastpage The filllastpage to set
	 */
	public void setFilllastpage(String filllastpage) {
		this.filllastpage = filllastpage;
	}

	/**
	 * @param string
	 */
	public void setSortmeta(String string) {
		sortmeta = string;
	}	/**
	 * @param string
	 */
	public void setSortitem(String string) {
		sortitem = string;
	}	/**
	 * @return
	 */
	public String getSortmeta() {
		return getTagAttributeValue("sortmeta", sortmeta, null);
	}	/**
	 * @return
	 */
	public String getSortitem() {
		return getTagAttributeValue("sortitem", sortitem, null);
	}	
	
	private void contentSort(List<Object> objects) {
        
        // Determine collator based on content language
        Collator collator = retrieveCollator();
        
        
		Comparator<Object> comparator = null;

		// Sort by expression, evaluated on both objects and returning something that is sorted
		String sortExpression = getSortexpression();
		if (sortExpression != null) {
			comparator = new ExpressionComparator(this, getSortExpressionEngine(), sortExpression, collator);
		}
		
		// Sort by comparision expression, which is evaluated on every  comparision
		String comparisonExpression = getSortcomparison();
		if (comparisonExpression != null) {
		    List<String> names = new ArrayList<String>();
		    names.add("$CONTEXT");
            comparator = new CompareExpressionComparator(this, getSortExpressionEngine(), comparisonExpression, collator, names);
		}

		// Sort by item
		String sortItem = getSortitem();
		if (sortItem != null) {
			comparator = new ContentComparator(this, false, sortItem, collator);
		}

		// Sort by meta
		String sortMeta = getSortmeta();
		if (sortMeta != null) {
			comparator = new ContentComparator(this, true, sortMeta, collator);
		}

		if (comparator != null) {
			java.util.Collections.sort(objects, comparator);
		}
		else {
			addWarning("Unable to sort content because no sort item/meta/expression was specified", false);
		}
		
	}

    private de.innovationgate.wgpublisher.expressions.ExpressionEngine getSortExpressionEngine() {
        de.innovationgate.wgpublisher.expressions.ExpressionEngine engine = null;
        if (this.getSortlanguage() != null) {
        	engine = de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory.getEngine(this.getSortlanguage());
        }
        return engine;
    }

    private Collator retrieveCollator() {
        Status status = (Status) getStatus();
        Collator collator;
        if (status.contentLanguage != null && status.contentLanguage != ResultSetTagStatus.MULTILANGUAGE_RESULT) {
            collator = Collator.getInstance(getCore().languageCodeToLocale(status.contentLanguage));
        }
        else {
            collator = Collator.getInstance(getTMLContext().getPreferredLanguageLocale());
        }
        return collator;
    }

    public String getOnlypublished() {
        return getTagAttributeValue("onlypublished", onlypublished, null);
    }

    public void setOnlypublished(String onlypublished) {
        this.onlypublished = onlypublished;
    }

    public String getLinear() {
        return getTagAttributeValue("linear", linear, "false");
    }

    public void setLinear(String linear) {
        this.linear = linear;
    }

    public String getFilter() {
        return getTagAttributeValue("filter", filter, null);
    }

    public void setFilter(String filter) {
        this.filter = filter;
    }

    public String getRelationgroup() {
        return getTagAttributeValue("group", relationgroup, null);
    }

    public void setRelationgroup(String group) {
        this.relationgroup = group;
    }

    public String getOffset() {
        return getTagAttributeValue("offset", offset, null);
    }

    public void setOffset(String offset) {
        this.offset = offset;
    }

    public String getExpression() {
        return getTagAttributeValue("expression", expression, null);
    }

    public void setExpression(String expression) {
        this.expression = expression;
    }}
