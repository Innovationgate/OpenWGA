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

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;

/**
 * A TML module containing WebTML Source code.
 */
public class WGTMLModule extends WGDesignResourceDocument implements PageHierarchyNode {
	
	public static final String META_CODE = "CODE";
    public static final MetaInfo METAINFO_CODE = new MetaInfo(META_CODE, String.class, "");
    
	public static final String META_MEDIAKEY = "MEDIAKEY";
    public static final MetaInfo METAINFO_MEDIAKEY = new MetaInfo(META_MEDIAKEY, String.class, null);
    static { METAINFO_MEDIAKEY.setLowerCase(true); };
    
	public static final String META_DIRECTACCESS = "DIRECTACCESS";
    public static final MetaInfo METAINFO_DIRECTACCESS = new MetaInfo(META_DIRECTACCESS, Boolean.class, Boolean.TRUE);
    
	public static final String META_CACHEABLE = "CACHEABLE";
    public static final MetaInfo METAINFO_CACHEABLE = new MetaInfo(META_CACHEABLE, Boolean.class, Boolean.FALSE);
    
    public static final String META_CODEOFFSET = "CODEOFFSET";
    public static final MetaInfo METAINFO_CODEOFFSET = new MetaInfo(META_CODEOFFSET, Integer.class, new Integer(0));

    public static final String META_PREPROCESS = "PREPROCESS";
    public static final MetaInfo METAINFO_PREPROCESS = new MetaInfo(META_PREPROCESS, Boolean.class, Boolean.FALSE);
    
    static {
        METAINFO_CODEOFFSET.setExtdata(true);
        METAINFO_CODEOFFSET.setMinCsVersion(WGDatabase.CSVERSION_WGA5);
    }
    
    /**
     * Creates a key, unique for one tml module, to be used when deploying it as JSP
     * @param dbKey The database of the module
     * @param moduleName The name of the module
     * @param mediaKey The media key of the module
     * @return The deployment key
     */
    public static String createDeploymentKey(String dbKey, String moduleName, String mediaKey) {
        
        StringBuffer buf = new StringBuffer();
        buf.append(dbKey).append("/").append(mediaKey.toLowerCase()).append("/").append(moduleName.toLowerCase());
        return buf.toString();
        
    }
    


    private String _deploymentKey;
    
	/**
	 * Constructor. Not to be used outside WGAPI
	 * @param db
	 * @param doc
	 * @throws WGAPIException 
	 */
	public WGTMLModule(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
		super(db, doc, flags);
	}
	 
	/**
	 * Returns the name of this TML module.
	 * @throws WGAPIException 
	 */
	public String getName() throws WGAPIException {
		return String.valueOf(this.getMetaData(META_NAME)).toLowerCase();
	}
	
	/**
	 * returns the WebTML-Code
	 * @throws WGAPIException 
	 */
	public String getCode() throws WGAPIException {
	    return (String) this.getMetaData(META_CODE);
	}
	
	/**
	 * Sets the WebTML-Code for this module
	 * @param code
	 * @throws WGAPIException 
	 */
	public boolean setCode(String code) throws WGAPIException {
		
		return setMetaData(META_CODE, code);
		 
	}
	
	/**
	 * Returns the media key of this module.
	 * @throws WGAPIException 
	 */
	public String getMediaKey() throws WGAPIException {

		return this.getMetaData(META_MEDIAKEY).toString();
		
	}
	
	/**
	 * Returns if this module should be directly accessible via URL
	 * @throws WGAPIException 
	 */
	public boolean isDirectAccessAllowed() throws WGAPIException {
		
		return ((Boolean)this.getMetaData(WGTMLModule.META_DIRECTACCESS)).booleanValue();

	}
	
	/**
	 * Sets if this module should be directly accessible via URL
	 * @param value
	 * @throws WGAPIException 
	 */
	public boolean setDirectAccessAllowed(boolean value) throws WGAPIException {
		return setMetaData(META_DIRECTACCESS, new Boolean(value));
	}


	public boolean isPreprocess() throws WGAPIException {
		return ((Boolean)this.getMetaData(META_PREPROCESS)).booleanValue();
	}
	public boolean setPreprocess(boolean value) throws WGAPIException {
		return setMetaData(META_PREPROCESS, new Boolean(value));
	}




	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.webgate.api.WGDesignDocument#setName(String)
	 */
	protected boolean setName(String name) throws WGAPIException {
		return setMetaData(META_NAME, name);
	}

	/**
	 * @throws WGAPIException 
	 * @see de.innovationgate.webgate.api.WGDesignDocument#setMediaKey(String)
	 */
	protected boolean setMediaKey(String key) throws WGAPIException {
		return setMetaData(META_MEDIAKEY, key);
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#createClone(de.innovationgate.webgate.api.WGDatabase)
	 */
	public WGDocument createClone(WGDatabase db) throws WGAPIException {

		WGTMLModule newModule = db.createTMLModule(getName(), getMediaKey());
		pushData(newModule);
		
		newModule.saveWithGivenTimestamps(getCreated(), getLastModified());
		return newModule;

	}
	
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument)
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGTMLModule module = (WGTMLModule) doc;
        module.setDescription(getDescription());
		module.setCode(getCode());
		module.setDirectAccessAllowed(isDirectAccessAllowed());
		module.setCacheable(isCacheable());
		
		if (doc.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
		    module.setMetaData(META_CODEOFFSET, getCodeOffset());
		}
		
		super.pushData(doc);
    }
	
	/**
	 * Returns if this TML modules results should be browser-cacheable
	 * @throws WGAPIException 
	 */
	public boolean isCacheable() throws WGAPIException {
		return ((Boolean) this.getMetaData(META_CACHEABLE)).booleanValue();
	}
	
	/**
	 * Sets if the result of this modules code should be browser-cacheable
	 * @throws WGAPIException 
	 */
	public boolean setCacheable(boolean cacheable) throws WGAPIException {
		return setMetaData(META_CACHEABLE, new Boolean(cacheable));
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

        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_TML)) {
            throw new WGIllegalStateException("Removing WebTML modules via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
            
        
        
    }
	
	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#dropRelations()
	 */
	protected void dropRelations() {}


    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException, WGAuthorisationException {
        super.performSaveCheck();

        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_TML)) {
            throw new WGIllegalStateException("Updating WebTML modules via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }

    }

    public void dropCache() throws WGAPIException {
        super.dropCache();
        _deploymentKey = null;
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
        return getName() + " (" + getMediaKey() + ")";
    }

    public PageHierarchyNode getParentNode() throws WGAPIException {
        return getDatabase().getAllDocumentsHierarchy().getCollectionForType(WGDocument.TYPE_TML);
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
        return WGDocument.TYPE_TML;
    }

}

