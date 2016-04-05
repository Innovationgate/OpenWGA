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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;

/**
 * A file container, whose main duty is to carry file attachments.
 */
public class WGFileContainer extends WGDesignResourceDocument implements PageHierarchyNode {

	/**
	 * Constructor. Should not be used outside the WGAPI.
	 * @param db
	 * @param doc
	 * @throws WGAPIException 
	 */
	public WGFileContainer(WGDatabase db, WGDocumentCore doc, WGDocumentObjectFlags flags) throws WGAPIException {
		super(db, doc, flags);
	}
	 


	/**
	 * Retrieves the name of this file container
	 * @throws WGAPIException 
	 */
	public String getName() throws WGAPIException {
		return String.valueOf(this.getMetaData(META_NAME)).toLowerCase();
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

		WGFileContainer newContainer = db.createFileContainer(getName());
		pushData(newContainer);
		
		newContainer.saveWithGivenTimestamps(getCreated(), getLastModified());
		return newContainer;

	}

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#pushData(de.innovationgate.webgate.api.WGDocument
     */
    public void pushData(WGDocument doc) throws WGAPIException {
        
        WGFileContainer container = (WGFileContainer) doc;
		container.setDescription(getDescription());
		
		// Clear files in target container
		Iterator oldFiles = container.getFileNames().iterator();
		while (oldFiles.hasNext()) {
		    String fileName = (String) oldFiles.next();
		    container.removeFile(fileName);
		}
		
		// Must save bc. some implementations (e.g. jdbc) won't allow to attach the same filenames again without saving inbetween
	    container.save();
		
	    
	    // Files		
		try {
			Iterator fileNames = getFileNames().iterator();
			while (fileNames.hasNext()) {
				String fileName = (String) fileNames.next();
				TemporaryFile tempFile = new TemporaryFile(fileName, getFileData(fileName), WGFactory.getTempDir());
				tempFile.deleteOnEviction(getDatabase().getSessionContext());
				container.attachFile(tempFile.getFile());
				
				// Copy meta and extension data
                if (container.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1 &&
                        getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA4_1) {
                    
                    WGFileMetaData sourceMeta = getFileMetaData(fileName);
                    WGFileMetaData targetMeta = container.getFileMetaData(fileName);
                    sourceMeta.pushData(targetMeta);
                }
				
			}
		} catch (IOException e) {
			WGFactory.getLogger().error("Error cloning file container", e);
			throw new WGCreationException("IO Error copying files for file container clone", e);
		}		
		
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
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_FILECONTAINER)) {
            throw new WGIllegalStateException("Removing file containers via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }

    }

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.WGDocument#dropRelations()
	 */
	protected void dropRelations() {}




    public void performSaveCheck() throws ResourceIsLockedException, WGAPIException, WGAuthorisationException {
        super.performSaveCheck();
        
        if (!getDatabase().isDoctypeModifiable(WGDocument.TYPE_FILECONTAINER)) {
            throw new WGIllegalStateException("Updating file containers via WGAPI is not permitted in this database", WGIllegalStateException.ERRORCODE_DOCTYPE_READONLY);
        }
            
            

    }



    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGDocument#removeFile(java.lang.String)
     */
    public boolean removeFile(String name) throws WGAPIException {
        return super.removeFile(name);
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
        return getDatabase().getAllDocumentsHierarchy().getCollectionForType(WGDocument.TYPE_FILECONTAINER);
    }



    @Override
    public int getType() {
        return WGDocument.TYPE_FILECONTAINER;
    }

}

