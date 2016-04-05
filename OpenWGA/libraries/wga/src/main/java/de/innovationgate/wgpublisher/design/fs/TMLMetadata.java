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

package de.innovationgate.wgpublisher.design.fs;

import java.util.ArrayList;
import java.util.Collections;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.wga.model.TMLMetadataInfo;

public class TMLMetadata extends DesignMetadata {
   
   public TMLMetadata() {
       super();
       _info = new TMLMetadataInfo();
   }
   
   public TMLMetadata(WGTMLModule mod) throws WGAPIException {
       super(mod); 
       _info = new TMLMetadataInfo();
       setDirectAccess(mod.isDirectAccessAllowed());
       setCacheable(mod.isCacheable());
   }
   
   public void writeToDocument(WGTMLModule mod) throws WGAPIException {
       super.writeToDocument(mod); 
       mod.setDirectAccessAllowed(isDirectAccess());
       mod.setCacheable(isCacheable());
       if (mod.getDatabase().getContentStoreVersion() >= WGDatabase.CSVERSION_WGA5) {
           mod.setMetaData(WGTMLModule.META_CODEOFFSET, getHeaderLines());
       }
   }
   
    /**
     * @return Returns the cacheable.
     */
    public boolean isCacheable() {
    	return ((TMLMetadataInfo)_info).isCacheable();
    }
    /**
     * @param cacheable The cacheable to set.
     */
    public void setCacheable(boolean cacheable) {
    	((TMLMetadataInfo)_info).setCacheable(cacheable);
    }
    /**
     * @return Returns the directAccess.
     */
    public boolean isDirectAccess() {
    	return ((TMLMetadataInfo)_info).isDirectAccess();
    }
    /**
     * @param directAccess The directAccess to set.
     */
    public void setDirectAccess(boolean directAccess) {
    	((TMLMetadataInfo)_info).setDirectAccess(directAccess);
    }
    
    /**
     * @return Returns the category.
     */
    public String getCategory() {
    	String cat =  ((TMLMetadataInfo)_info).getCategory();
        return (cat == null || cat.trim().equals("") ? null : cat);
    }
    
    /**
     * @param category The category to set.
     */
    public void setCategory(String category) {    	
        if (category == null) {
        	((TMLMetadataInfo)_info).setCategory("");
        } else {
        	((TMLMetadataInfo)_info).setCategory(category);	
        }
    }
}
