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
package de.innovationgate.webgate.api;

import java.util.ArrayList;
import java.util.List;

import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;


/**
 * <p>Utility object to navigate the content hierarchy. The WGContentNavigator in most methods takes a content as parameter and determines a content position in the site hierarchy relative to this content, e.g. the parent content or a sibling content.
 * The return is always a valid and released content object of the same language (except in those cases, where something different is configured - as described below). So this object is used by the navigation elements of WebTML to quickly determine the content links to display.</p>
 * 
 * <p>If a content position does not provide a valid content as requested, the navigator searches further along the given direction (e.g. when searching parent, goes further up the hierarchy, when searching sibling, goes to the nex sibling) and tries to return a valid content from there.
 * If the hierarchy ends in that direction without finding valid content, methods return null.</p>
 * 
 * <p>WGContentNavigator also takes into account visibility settings and automatically ignores contents that are not visible. 
 * Therefor the constructor takes a displaytype parameter, telling the navigator to behave like a (WebTML-Navigator), a sitemap or a search, so contents hidden from one of these facilities can be ignored.</p>
 * 
 */
public class WGContentNavigator {
    
    /**
     * Class capable of producing iterators for a content collection
     */
    public interface IteratorCreator extends Iterable<WGContent> {
        
        WGContentNavigator.Iterator iterator();
        
        int getSize() throws WGAPIException;
        
    }
   
    public class Iterator extends PrefetchingIterator<WGContent> implements SkippingIterator<WGContent> {
        
        private WGStructEntryIterator _structs;
        private int _index = 0;
        private int _nextElementIndex = 0;
        private boolean _fetched = false;

        private Iterator(WGStructEntryIterator structs) {
            _structs = structs;
        }
        
        @Override
        public WGContent next() {
            WGContent next = super.next();
            _index = _nextElementIndex;
            return next;
        }

        @Override
        protected WGContent fetchNextValue() {
            
            _fetched = true;
            try {
                _nextElementIndex = _index;
                while (_structs.hasNext()) {
                    WGStructEntry struct = _structs.next();
                    _nextElementIndex++;
                    WGContent content = getRelevantContent(struct);
                    if (content != null) {
                        return content;
                    }
                }
                
                return null;
            }
            catch (WGAPIException e) {
                throw new RuntimeException("Exception fetching next value", e);
            }
            
        }

        @Override
        public int skip(int nrOfElements) {
            if (_fetched) {
                throw new IllegalStateException("Cannot skip elements after next() or hasNext() has been used");
            }
            int notSkipped = _structs.skip(nrOfElements);
            _index+=(nrOfElements - notSkipped);
            return notSkipped;
        }
        
        public int getResults() throws WGAPIException {
            
            WGDocument parent = _structs.getParent();
            if (parent instanceof WGArea) {
                return ((WGArea) parent).getRootEntryCount();
            }
            else {
                return ((WGStructEntry) parent).getChildEntryCount();
            }
            
        }

        public int getIndex() {
            return _index;
        }
        
    }
	
	private String displayType;

	private boolean onlyPublished = true;
	private WGLanguageChooser chooser = null;
	

	
	/**
	 * Public constructor for WGContentNavigator.
	 * @param displayType The display type (i.e. role in WebTML) this navigator works for. Use constants WGContent.DISPLAYTYPE_...
	 * @param chooser A language chooser choosing the relevant language version for pages
	 * @throws WGAPIException 
	 */
	public WGContentNavigator(String displayType,WGLanguageChooser chooser) throws WGAPIException {
		this.displayType = displayType;
		this.chooser = chooser;
	} 
	

	

	
	/**
	 * Returns the root content for a given content in the same language
	 * @param relContent The content to find root content for
	 * @return The root content. Can be the same content as the given one, if this was a root content itself.
	 * @throws WGAPIException 
	 */
	public WGContent getRootContent(WGContent relContent) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return null;
		}
		
		WGStructEntry root = relContent.getStructEntry().getRootEntry();
		
	    WGContent content = getRelevantContent(root);
	    if (content != null) {
	        return content;
	    }
		
		
		return null;
	}
	
	/**
	 * Returns the first root content of an area in the given language.
	 * @param relArea The area to search for the first root content.
	 * @return WGContent
	 * @throws WGAPIException 
	 */
	public WGContent getRootContent(WGArea relArea) throws WGAPIException {
	    
	    Iterator it =createContentIterator(relArea.getRootEntryIterator(10));

	    if (it.hasNext()) {
	        return it.next();
	    }
	    else {
	        return null;
	    }
	    
	}
	
	/**
	 * Returns the "relevant" content of a struct entry. That is, a content that is released and that matches one relevant language. 
	 * Takes into account the eventually given draft content of this navigator and returns this one, if it should.
	 * @param entry Entry to search for relevant content
	 * @return WGContent
	 * @throws WGAPIException 
	 */
	public WGContent getRelevantContent(WGStructEntry entry) throws WGAPIException {
		
	    if (entry == null) {
	        return null;
	    }
	    

	   	WGContent content = this.chooser.selectContentForPage(entry, !onlyPublished);    
        if (content != null && content.mayBePublished(!onlyPublished, displayType)) {
            return content;
        }
	                    
		return null;
	}

	/**
	 * Gets the display type (i.e. role in WebTML), that this content navigator uses to determine contents to show.
	 */
	public String getDisplayType() {
		return displayType;
	}
	/**
	 * Sets the display type (i.e. role in WebTML) that this navigator should use.
	 * @param displayType The display type. Use constants WGContent.DISPLAYTYPE....
	 */

	public void setDisplayType(String displayType) {
		this.displayType = displayType;
	}
	
	/**
	 * Returns a sibling content of the given content that is at a certain index position in the siblings list and has the same language.
	 * @param relContent The content whose sibling is to be found.
	 * @param idx The index position at which to find the sibling.
	 * @param relative If true, the index position is regarded relative, i.e. +1 will fetch the next sibling, -1 the previous sibling. If false, the index position is regarded absolute, where 0 denotes the first sibling
	 * @return The content at that index position.
	 * @throws WGAPIException 
	 */
	public WGContent getSiblingContent(WGContent relContent, int idx, boolean relative) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return null;
		}
		
		if (relative == true) {
			if (idx == 0) {
				return relContent;
			}
			else if (idx > 0) {
				while (idx > 0 && relContent != null) {
					relContent = this.getNextSibling(relContent);
					idx--;
				}
				return relContent;
			}
			else if (idx < 0) {
				while (idx < 0 && relContent != null) {
					relContent = this.getPreviousSibling(relContent);
					idx++;
				}
				return relContent;
			}
		}
		
		else {
			java.util.Iterator<WGStructEntry> siblings = relContent.getStructEntry().getSiblingEntryIterator(10);
			WGStructEntry entry;
			WGContent content = null;
			
			while (siblings.hasNext() ) {
				entry = (WGStructEntry) siblings.next();
				content = this.getRelevantContent(entry);
			
				if (content != null ) {
					idx--;
					if (idx < 0) {
						return content;
					}
				}
				
			}
		}
		
		return null;
	}
	
	/**
	 * Gets the previous siblings content with the same language.
	 * @param relContent The content to find the sibling for
	 * @return WGContent
	 * @throws WGAPIException 
	 */
	public WGContent getPreviousSibling(WGContent relContent) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return null;
		}
		
		WGStructEntryList siblings = relContent.getStructEntry().getSiblingEntries();
		int index = siblings.getIndexOfEntry(relContent.getStructEntry());
		if (index == -1) {
			return null;
		}
		
		index--;
		
		WGStructEntry entry;
		WGContent content;
	
		while (index >= 0) {
			entry = siblings.getByIndex(index);
			content = this.getRelevantContent(entry);
			if (content != null) {
				return content;
			}
			
			index--;
		}
		
		return null;
	}
	
	/**
	 * Gets the next siblings content with the same language.
	 * @param relContent The content to find the sibling for
	 * @return WGContent
	 * @throws WGAPIException 
	 */
	public WGContent getNextSibling(WGContent relContent) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return null;
		}
		
		WGStructEntryList siblings = relContent.getStructEntry().getSiblingEntries();
		int index = siblings.getIndexOfEntry(relContent.getStructEntry());
		if (index == -1) {
			return null;
		}
		
		
		index++;
		
		WGStructEntry entry;
		WGContent content;
	
		while (index < siblings.size()) {
			entry = siblings.getByIndex(index);
			content = this.getRelevantContent(entry);
			if (content != null) {
			    if (!content.equals(relContent)) {
			        return content;
			    }
			    else {
			        // Something wrong here. Were returning the same content again.
			        return null;
			    }
			}
			
			index++;
		}
		
		return null;
		
	}
	
	/**
	 * Returns the nth child content of a parent content with the same language.
	 * @param relContent The parent content
	 * @param idx The index position of the needed child content (!= the struct entries position metadata!)
	 * @param searchOrder The search order. Use constants WGContent.SEARCHORDER_.... INEFFECTIVE SINCE OPENWGA 6.1
	 * @deprecated searchOrder argument is ineffective. Use {@link #getChildContent(WGContent, int)} instead.
	 * @return The child content at that index position
	 * @throws WGAPIException 
	 */
	public WGContent getChildContent(WGContent relContent, int idx, int searchOrder) throws WGAPIException {
		return getChildContent(relContent, idx);
	}
	
	  
	/**
     * Returns the nth child content of a parent content with the same language.
     * @param relContent The parent content
     * @param idx The index position of the needed child content (!= the struct entries position metadata!), 0 denoting the first one
     * @return The child content at that index position
     * @throws WGAPIException 
     */
	public WGContent getChildContent(WGContent relContent, int idx) throws WGAPIException {
	        
	        if (relContent.isDummy()) {
	            return null;
	        }
	        
	        WGStructEntryIterator children = relContent.getStructEntry().getChildEntryIterator(10);

	        WGContent childContent = null;
	        while (children.hasNext()) {
	            WGStructEntry child = children.next();
	            childContent = this.getRelevantContent(child);
	            if (childContent != null) {
                    idx--;
                    if (idx < 0) {
                        return childContent;
                    }
	            }
	            
	        }
	        return null;
	        
	    }
	
	/**
	 * Returns all available languages for the the given content as content list.
	 * @param relContent The content to retrieve languages for.
	 * @param includeThisOne If true, the given content will be included in the list, if false won't
	 * @return WGContentList
	 * @throws WGAPIException 
	 */
	public WGContentList getLanguagesContent(WGContent relContent, boolean includeThisOne) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return null;
		}
		
		WGStructEntry struct = relContent.getStructEntry();
		java.util.ArrayList<WGContent> contents = new java.util.ArrayList<WGContent>();
		
		java.util.Iterator<WGLanguage> languages = relContent.getDatabase().getLanguages().values().iterator();
		WGLanguage language = null;
		
		
		while (languages.hasNext()) {
		    WGContent content = null;
			language = (WGLanguage) languages.next();
			if (!onlyPublished) {
			    content = struct.getContent(language.getName(), WGContent.STATUS_DRAFT);
			}
			
			if (!onlyPublished && content == null) {
			    content = struct.getContent(language.getName(), WGContent.STATUS_REVIEW);
            }
			
			if (content == null) {
			    content = struct.getReleasedContent(language.getName());
			}
			
			if (content != null && content.mayBePublished(!onlyPublished, displayType)) {
				if (includeThisOne == true || !content.getLanguage().getName().equals(relContent.getLanguage().getName())) {
					contents.add(content);
				}
			}
		}
		
		return WGContentList.create(contents);
	}
	
	/**
	 * Gets the parent content to the given content with the same language.
	 * @param relContent content to find parent for
	 * @return WGContent
	 * @throws WGAPIException 
	 */
	public WGContent getParentContent(WGContent relContent) throws WGAPIException {

		if (relContent.isDummy()) {
			return null;
		}

		WGStructEntry parent = relContent.getStructEntry();
		WGContent parentContent = null;
		
		while (parent != null && !parent.isRoot()) {
			parent = parent.getParentEntry();
			parentContent = this.getRelevantContent(parent);
			if (parentContent != null) {
				return parentContent;
			}
		}

		return null;
	}
	
	/**
	 * Returns the absolute index position of a given content among its siblings
	 * @param relContent The context, whose index position is evaluated
	 * @return Integer
	 * @throws WGAPIException 
	 */
	public Integer getSiblingsIndex(WGContent relContent) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return null;
		}

		java.util.Iterator<WGStructEntry> siblings = relContent.getStructEntry().getSiblingEntries().iterator();
		WGStructEntry sibling;
		WGContent content;
		int index = 0;
		
		Object key = relContent.getStructKey();
		while (siblings.hasNext()) {
			sibling = (WGStructEntry) siblings.next();
			content = getRelevantContent(sibling);

			if (content != null) {
				if (content.getStructKey().equals(key)) {
					return new Integer(index);
				}
				else {
					index++;
				}
			}
		}

		return null;
	}
	
	/**
	 * Returns the number of siblings (of the same language) of a content.
	 * @param relContent The content to find the siblings count for.
	 * @return The count of siblings EXCLUDING the given content
	 * @throws WGAPIException 
	 */
	public int getSiblingsCount(WGContent relContent) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return 0;
		}
		
		java.util.Iterator<WGStructEntry> siblings = relContent.getStructEntry().getSiblingEntries().iterator();
		WGStructEntry sibling;
		WGContent content = null;
		int index = 0;
		Object key = relContent.getStructKey();
		int sibCount = 0;
		while (siblings.hasNext()) {
			sibling = (WGStructEntry) siblings.next();
			content = getRelevantContent(sibling);

			if (content != null) {
				if (!(content.getStructKey().equals(key))) {
					sibCount++;
				}
				else {
					index++;
				}
			}
		}
		return sibCount;	
	}
	
	/**
	 * Chooses the relevant content between a retrieved content, and a content currently previewed as draft, review or archived doc.
	 * When previewing a content at non-published state, all links that go back to it's struct entry position should not point to the currently released content but to the previewed version..
	 * Therefor this method should be called in preview mode with each content to show links for. It will tes, when this case occurs and return the preview content instead.
	 * @param retrievedContent The retrieved content, that under normal conditions should be shown
	 * @param previewContent The previewed content, that should be shown when the retrieved content is the released version of it
	 * @return WGContent The content to show
	 * @throws WGAPIException 
	 */
	public WGContent chooseRelevantContent(WGContent retrievedContent, WGContent previewContent) throws WGAPIException {
	    
	    // If we are allowed to serve non-published docs and the preview doc is not in status release, we must test if it is an alternative version of the retrieved content
	    if (!isOnlyPublished() && !previewContent.getStatus().equals(WGContent.STATUS_RELEASE)) {

	        // If so we return the main content
	        if (previewContent.getStructKey().equals(retrievedContent.getStructKey()) && previewContent.getLanguage().getName().equals(retrievedContent.getLanguage().getName())) {
	            return previewContent;
	        }
	    }
	    
	    return retrievedContent;
		
	}
	
	/**
	 * Retrieves the hierarchical level of a content document.
	 * @param relContent The content, whose level is requested.
	 * @return The hierarchical level, 1 being the main document level.
	 * @throws WGAPIException 
	 */
	public Integer getContentLevel(WGContent relContent) throws WGAPIException {
		if(relContent.isDummy())
			return 0;
	    return relContent.getStructEntry().getLevel();
	}
	
	/**
	 * Returns, if the given context has released child contexts in the same language.
	 * @param relContent
	 * @return boolean
	 * @throws WGAPIException 
	 */
	public boolean hasContentChildren(WGContent relContent) throws WGAPIException {
		
		if (relContent.isDummy()) {
			return false;
		}
		
		WGContent child = this.getChildContent(relContent, 0);
		return (child != null ? true : false);
	}

    /**
     * Returns if only published contents are to be retrieved
     */
    public boolean isOnlyPublished() {
        return onlyPublished;
    }

    /**
     * Sets if only published contents are to be retrieved
     */
    public void setOnlyPublished(boolean onlyPublished) {
        this.onlyPublished = onlyPublished;
    }
    

    

    


    /**
     * Given a list of struct entry siblings this method retrieves the relevant contents for all siblings.
     * @param siblings The siblings of type {@link WGStructEntry}
     * @return A list of {@link WGContent} objects
     * @throws WGAPIException
     */
    public List<WGContent> collectRelevantContents(WGStructEntryList siblings) throws WGAPIException {
        int index = 0;
        WGStructEntry entry;
        WGContent content;
        List<WGContent> siblingContents = new ArrayList<WGContent>();
    
        while (index < siblings.size()) {
            entry = siblings.getByIndex(index);
            content = this.getRelevantContent(entry);
            if (content != null) {
                siblingContents.add(content);
            }
            
            index++;
        }
        
        return siblingContents;
    }
    
    /**
     * Creates a content iterator for contents of the struct entries, returned by the given {@link WGStructEntryIterator}
     * The iterator will choose contents based on the settings in this content navigator.
     * @param structs The struct entry iterator
     * @return The content iterator
     */
    public Iterator createContentIterator(WGStructEntryIterator structs) {
        return new Iterator(structs);
    }

    /**
     * Creates an {@link Iterable} for the child contents of the given content
     * @param relContent The content
     * @param pageSize The fetch size for the iterable, determining how many contents will be fetched at once
     * @return The iterable
     */
    public IteratorCreator createChildContentIterable(final WGContent relContent, final int pageSize) throws WGAPIException {
        return createChildContentIterable(relContent, pageSize, null);
    }
    
    /**
     * Creates an {@link Iterable} for the child contents of the given content
     * @param relContent The content
     * @param pageSize The fetch size for the iterable, determining how many contents will be fetched at once
     * @param orderExpression A column set expression denoting the order in which to return contents, null for default oder (position, title, both ascending)
     * @return The iterable
     */
    public IteratorCreator createChildContentIterable(final WGContent relContent, final int pageSize, final String orderExpression) throws WGAPIException {
        
        if (!relContent.hasCompleteRelationships()) {
            throw new WGIllegalArgumentException("Cannot create navigator iterable for content without complete relationships: " + relContent.getContentKey().toString());
        }
        
        return new IteratorCreator() {
            
            @Override
            public Iterator iterator() {

                try {
                    if (orderExpression != null) {
                        return WGContentNavigator.this.createContentIterator(relContent.getStructEntry().getOrderedChildEntryIterator(pageSize, orderExpression));
                    }
                    else {
                        return WGContentNavigator.this.createContentIterator(relContent.getStructEntry().getChildEntryIterator(pageSize));
                    }
                }
                catch (WGAPIException e) {
                    throw new RuntimeException("Exception creating content iterator", e);
                }
                
            }
            
            @Override
            public int getSize() throws WGAPIException {
                WGDocument parent = relContent.getStructEntry();
                if (parent instanceof WGArea) {
                    return ((WGArea) parent).getRootEntryCount();
                }
                else {
                    return ((WGStructEntry) parent).getChildEntryCount();
                }
                
            }
            
        };
        
    }

    /**
     * Creates an {@link Iterable} for the sibling contents of the given content
     * @param relContent The content
     * @param pageSize The fetch size for the iterable, determining how many contents will be fetched at once
     * @return The iterable
     */
    public IteratorCreator createSiblingContentIterable(final WGContent relContent, final int pageSize) throws WGAPIException {
        return createSiblingContentIterable(relContent, pageSize, null);
    }
    
    /**
     * Creates an {@link Iterable} for the sibling contents of the given content
     * @param relContent The content
     * @param pageSize The fetch size for the iterable, determining how many contents will be fetched at once
     * @param orderExpression A column set expression denoting the order in which to return contents, null for default oder (position, title, both ascending)
     * @return The iterable
     */
    public IteratorCreator createSiblingContentIterable(final WGContent relContent, final int pageSize, final String orderExpression) throws WGAPIException {
        
        if (relContent.getStructEntry() == null || relContent.getLanguage() == null) {
            throw new WGIllegalArgumentException("Cannot create navigator iterable for content without complete relationships: " + relContent.getContentKey().toString());
        }
        
        return new IteratorCreator() {
            
            @Override
            public Iterator iterator() {

                try {
                    if (orderExpression != null) {
                        return WGContentNavigator.this.createContentIterator(relContent.getStructEntry().getOrderedSiblingEntryIterator(pageSize, orderExpression));
                    }
                    else {
                        return WGContentNavigator.this.createContentIterator(relContent.getStructEntry().getSiblingEntryIterator(pageSize));
                    }
                }
                catch (WGAPIException e) {
                    throw new RuntimeException("Exception creating content iterator", e);
                }
                
            }
            
            @Override
            public int getSize() throws WGAPIException {
                WGDocument parent = (WGDocument) relContent.getStructEntry().getParentNode();
                if (parent instanceof WGArea) {
                    return ((WGArea) parent).getRootEntryCount();
                }
                else {
                    return ((WGStructEntry) parent).getChildEntryCount();
                }
                
            }
        };
        
    }

    /**
     * Creates an {@link Iterable} for the parent contents of the given content
     * @param relContent The content
     * @return The iterable
     */
    public IteratorCreator createParentContentIterable(final WGContent relContent, final int pageSize) throws WGAPIException {
        return createParentContentIterable(relContent, pageSize, null);
    }
    
    /**
     * Creates an {@link Iterable} for the parent contents of the given content
     * @param relContent The content
     * @param pageSize The fetch size for the iterable, determining how many contents will be fetched at once
     * @return The iterable
     */
    public IteratorCreator createParentContentIterable(final WGContent relContent, final int pageSize, final String orderExpression) throws WGAPIException {
        
        if (!relContent.hasCompleteRelationships()) {
            throw new WGIllegalArgumentException("Cannot create navigator iterable for content without complete relationships: " + relContent.getContentKey().toString());
        }
        
        return new IteratorCreator() {
            
            @Override
            public Iterator iterator() {

                try {
                    if (orderExpression != null) {
                        return WGContentNavigator.this.createContentIterator(relContent.getStructEntry().getParentEntry().getOrderedSiblingEntryIterator(pageSize, orderExpression));
                    }
                    else {
                        return WGContentNavigator.this.createContentIterator(relContent.getStructEntry().getParentEntry().getSiblingEntryIterator(pageSize));
                    }
                }
                catch (WGAPIException e) {
                    throw new RuntimeException("Exception creating content iterator", e);
                }
                
            }
            
            @Override
            public int getSize() throws WGAPIException {
                WGDocument parent = (WGDocument) relContent.getStructEntry().getParentEntry().getParentNode();
                if (parent instanceof WGArea) {
                    return ((WGArea) parent).getRootEntryCount();
                }
                else {
                    return ((WGStructEntry) parent).getChildEntryCount();
                }
                
            }
            
        };
        
    }
    
    

}
