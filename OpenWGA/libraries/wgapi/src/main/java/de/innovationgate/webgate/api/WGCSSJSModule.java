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

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;

/**
 * Represents a Module containing various Non-WebTML-Code, e.g. for Web-Scripting- or Formatting-Languages
 */
public abstract class WGCSSJSModule extends WGDesignResourceDocument implements PageHierarchyNode {
    
	/**
	 * A module with XML-Code
	 */
	public static String CODETYPE_XML = "xml";
	/**
	 * A module with JavaScript-Code
	 */
	public static final String CODETYPE_JS = "js";
	/**
	 * A module with CSS-Code
	 */
	public static final String CODETYPE_CSS = "css";
	/**
	 * A module with VBScript-Code
	 * @deprecated
	 */
	public static final String CODETYPE_VBS = "vbs";
	/**
	 * A Module with TMLScript-Code
	 */
	public static final String CODETYPE_TMLSCRIPT = "tmlscript";
	

	public static final String META_CODE = "CODE";
    public static final MetaInfo METAINFO_CODE = new MetaInfo(META_CODE, String.class, "");
        
	public static final String META_CODETYPE = "CODETYPE";
    public static final MetaInfo METAINFO_CODETYPE = new MetaInfo(META_CODETYPE, String.class, CODETYPE_CSS);
    static { 
        METAINFO_CODETYPE.addAllowedValue(CODETYPE_CSS);
        METAINFO_CODETYPE.addAllowedValue(CODETYPE_JS);
        METAINFO_CODETYPE.addAllowedValue(CODETYPE_TMLSCRIPT);
        METAINFO_CODETYPE.addAllowedValue(CODETYPE_VBS);
        METAINFO_CODETYPE.addAllowedValue(CODETYPE_XML);
    };
    
    public static final String META_CODEOFFSET = "CODEOFFSET";
    public static final MetaInfo METAINFO_CODEOFFSET = new MetaInfo(META_CODEOFFSET, Integer.class, new Integer(0));
    static {
        METAINFO_CODEOFFSET.setExtdata(true);
        METAINFO_CODEOFFSET.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    public static final String METADATA_MODULE_QUALIFIER = "__";

    /**
     * Constructor. Should not be used outside the WGAPI.
     * @throws WGAPIException 
     */
    public WGCSSJSModule(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
        super(db, doc, flags);
    }
    
 
	/**
	 * Returns the name of this library
	 * @throws WGAPIException 
	 */
	public String getName() throws WGAPIException {
		return String.valueOf(this.getMetaData(META_NAME)).toLowerCase();
	}
	
	/**
	 * Returns the program code of this library.
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getCode() throws WGAPIException {
		return (String) this.getMetaData(META_CODE);
	}
	
	/**
	 * Sets the program code of this library.
	 * @param code
	 * @throws WGAPIException 
	 */
	public boolean setCode(String code) throws WGAPIException{
		return setMetaData(META_CODE, code);
	}
	 
	/**
	 * Returns the library type. Uses constants WGCSSJSLibrary.TYPE_...
	 * @return String
	 * @throws WGAPIException 
	 */
	public String getCodeType() throws WGAPIException {
		return this.getMetaData(META_CODETYPE).toString();
	}
	
	/**
	 * Sets the code type of this library. Must be a constant WGCSSJSModule.CODETYPE_...
	 * @param type
	 * @throws WGAPIException 
	 */
	public boolean setCodeType(String type) throws WGAPIException {
		return setMetaData(META_CODETYPE, type);
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

		WGCSSJSModule newModules = db.createCSSJSModule(getName(), getCodeType());
		try {
            pushData(newModules);
            newModules.saveWithGivenTimestamps(getCreated(), getLastModified());
            return newModules;
        }
        catch (WGAPIException e) {
            throw e;
        }
	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGCSSJSModule module = (WGCSSJSModule) doc;
        module.setDescription(getDescription());
		module.setCode(getCode());
		
		super.pushData(doc);
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

        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_CSSJS)) {
            throw new WGIllegalStateException("Removing script libraries via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }

    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#dropRelations()
	 */
	protected void dropRelations() {}
	
	/**
	 * If codetype is XML, returns the contained XML-code as dom Document.
	 * @throws DocumentException
	 * @throws WGAPIException 
	 */
	public Document getDOMDocument() throws DocumentException, WGAPIException {
		
		if (!getCodeType().equals(CODETYPE_XML)) {
			return DocumentHelper.createDocument();
		}
		
		return DocumentHelper.parseText(getCode());
		
		
	}
	
	/**
	 * Converts the given DOM-Document to XML sets it as code for this module. Additionally changes the codetype of this module to "xml".
	 * @param doc The DOM document that will be converted to XML
	 * @throws WGAPIException 
	 */
	public void setDOMDocument(Document doc) throws WGAPIException {
		
		setCodeType(CODETYPE_XML);
		setCode(doc.asXML());
		
	}



    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException, WGAuthorisationException {
        super.performSaveCheck();
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_CSSJS)) {
            throw new WGIllegalStateException("Updating script libraries via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }

    }

    /**
     * Determines if the module is a metadata module
     */
    public boolean isMetadataModule() throws WGAPIException {
        return getName().startsWith(WGCSSJSModule.METADATA_MODULE_QUALIFIER);
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
        return getDatabase().getAllDocumentsHierarchy().getCollectionForType(WGDocument.TYPE_CSSJS);
    }


    @Override
    public String getMediaKey() throws WGAPIException {
        return getCodeType();
    }
    
    /**
     * Returns the line number that the first line of code in this module had in its source medium.
     * Metadata headers might have been removed from the code. This metadata field reports the "real" number of the first line so OpenWGA can correctly report line numbers on errors
     * @throws WGAPIException
     */
    public int getCodeOffset() throws WGAPIException {
        return (Integer) getMetaData(META_CODEOFFSET);
    }


    @Override
    public int getType() {
        return WGDocument.TYPE_CSSJS;
    }

} 
