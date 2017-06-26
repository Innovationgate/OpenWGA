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
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.PageRightsFilter.Right;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;

/**
 * A language definion. This language can be used for content.
 */
public class WGLanguage extends WGSchemaDocument implements PageHierarchyNode {
	
	public static final String META_TITLE = "TITLE";
    public static final MetaInfo METAINFO_TITLE = new MetaInfo(META_TITLE, String.class, "");
    
	public static final String META_EDITORS = "EDITORS";
    public static final MetaInfo METAINFO_EDITORS = new MetaInfo(META_EDITORS, String.class, Collections.EMPTY_LIST);
    static { METAINFO_EDITORS.setMultiple(true); };
    
    /**
     * Converts a WGAPI language name to a corresponding java locale
     * @param lang
     * @return A locale object representing the same language as the WGAPI language code
     */
    public static Locale languageNameToLocale(String lang) {
        
        List langElements = WGUtils.deserializeCollection(lang, "_");
        switch (langElements.size()) {
            case 3:
                return new Locale((String) langElements.get(0), (String) langElements.get(1), (String) langElements.get(2));
                
            case 2:
                return new Locale((String) langElements.get(0), (String) langElements.get(1));
                
            case 1:
                return new Locale((String) langElements.get(0));
            
            default:
                return null;
        }
        
    }
    
	
	/**
	 * Constructor. Should not be used outside WGAPI.
	 * @param db
	 * @param doc
	 * @throws WGAPIException 
	 */
	public WGLanguage(WGDatabase db, WGDocumentCore doc) throws WGAPIException {
		this(db, doc,  new WGDocumentObjectFlags());
	}
	 
	public WGLanguage(WGDatabase wgDatabase, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
	    super(wgDatabase, doc, flags);
    }


    /**
	 * Returns the name of the language, i.e. the code
	 * @throws WGAPIException 
	 */
	public String getName() throws WGAPIException {
		return String.valueOf(this.getMetaData(META_NAME)).toLowerCase();
	}
	
	/**
	 * Returns the title of the language, i.e. the human readable description
	 * @throws WGAPIException 
	 */
	public String getTitle() throws WGAPIException {
		return this.getMetaData(META_TITLE).toString();
	}
	
	/**
	 * Set the title of this language.
	 * @throws WGAPIException 
	 */
	public boolean setTitle(String value) throws WGAPIException {
		return setMetaData(META_TITLE, value);
	}


  

	
	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.webgate.api.WGDesignDocument#setName(String)
	 */
	protected boolean setName(String name) throws WGAPIException {
		return setMetaData(META_NAME, name);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
	 */
	public WGDocument createClone(WGDatabase db) throws WGAPIException {
		
		if (isDummy()) {
			return null;
		}
		
		WGLanguage newLanguage = db.createLanguage(getName(), getTitle());
		pushData(newLanguage);
		
		newLanguage.saveWithGivenTimestamps(getCreated(), getLastModified());
		return newLanguage;

	}
	
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGLanguage lang = (WGLanguage) doc;
        lang.setTitle(getTitle());
        lang.setDescription(getDescription());
		lang.setEditors(new ArrayList(getEditors()));
		
		super.pushData(doc);
    }

    /**
	 * Returns a list of names allowed to edit content for this language. If empty, anyone is allowed.
     * @throws WGAPIException 
	 */
	public List getEditors() throws WGAPIException {
		return (List) this.getMetaData(WGLanguage.META_EDITORS);
	}
	
	/**
	 * Old version of {@link #getEditors()}. Avoid in new developments.
	 * @throws WGAPIException
	 */
	public List getContentCreators() throws WGAPIException {
	    return getEditors();
	}
	
	/**
	 * Set the people allowed to edit content of this language
	 * @throws WGAPIException 
	 */
	public void setEditors(List cc) throws WGAPIException {
		setMetaData(META_EDITORS, cc);
	}
	
	/**
	 * Old version of {@link #setEditors(List)}. Avoid in new developments.
	 * @param cc
	 * @throws WGAPIException
	 * @deprecated
	 */
	public boolean setContentCreators(List cc) throws WGAPIException {
	    setEditors(cc);
	    return true;
	}
	
	/**
	 * Tests if the current user is allowed to create content in this language
	 * - asks pageRightsFilter
	 * - checks struct edit rights (hierarchicaly)
	 * @throws WGAPIException 
	 */
	public boolean mayCreateContent() throws WGAPIException {
		List users = this.getEditors();
		if (WGDatabase.anyoneAllowed(users)) {
			return true;
		}
		else if( this.db.getSessionContext().getAccessLevel() >= WGDatabase.ACCESSLEVEL_CHIEF_EDITOR){
			return true;
		}		
		else {
			return this.db.isMemberOfUserList(users);
		}
	}

	public boolean mayCreateContent(WGStructEntry page) throws WGAPIException {
		
		Right right = this.db.getPageRightsFilter().mayEditContent(page, this.db.getSessionContext().getUserAccess(), this);
		if(right==Right.DENIED)
			return false;
		else if (right == Right.ALLOWED_SKIP_DEFAULT_CHECKS)
			return true;

		return mayCreateContent();
		
	} 

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#remove()
	 */
	protected boolean remove(WGDocument deletionRoot) throws WGAPIException {
		return innerRemove(deletionRoot, true);
	}





    @Override
    protected void performRemoveCheck(boolean deepCheck, WGDocument deletionRoot) throws WGAPIException {
        
        super.performRemoveCheck(deepCheck, deletionRoot);
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_LANGUAGE)) {
            throw new WGIllegalStateException("Removing languages via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
        
        if (deepCheck) {
            if (getDatabase().getCore().isLanguageUsed(this)) {
                throw new WGIllegalStateException("The language definition '" + getName() + "' cannot be removed because there are existing content documents of this language.", WGIllegalStateException.ERRORCODE_SCHEMA_DOCUMENT_IN_USE);
            }
        }
        
        
        
    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#dropRelations()
	 */
	protected void dropRelations() {}
    
    /**
     * Returns a java locale that matches the language code represented by this language definition
     * @throws WGAPIException 
     */
    public Locale getLocale() throws WGAPIException {
        return languageNameToLocale(getName());
    }


    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException {
        super.performSaveCheck();
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_LANGUAGE)) {
            throw new WGIllegalStateException("Updating languages via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
    }


    public List<PageHierarchyNode> getChildNodes() throws WGAPIException {
        return null;
    }
    
    @Override
    public SkippingIterator<? extends PageHierarchyNode> getChildNodeIterator(int pageSize) throws WGAPIException {
        return new SkippingIteratorWrapper(Collections.EMPTY_LIST.iterator());
    }


    public Class getChildNodeType() {
        return null;
    }


    public String getNodeKey() throws WGAPIException {
        return getDocumentKey();
    }


    public String getNodeTitle(String language) throws WGAPIException {
        return getName();
    }


    public PageHierarchyNode getParentNode() throws WGAPIException {
        return getDatabase().getAllDocumentsHierarchy().getCollectionForType(WGDocument.TYPE_LANGUAGE);
    }


    @Override
    public int getType() {
        return WGDocument.TYPE_LANGUAGE;
    }

}

