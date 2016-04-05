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

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.wga.model.FCMetadataInfo;

public class FCMetadata extends DesignMetadata {
    
    public FCMetadata() {
        super();
        _info = new FCMetadataInfo();
    }
    
    public FCMetadata(WGFileContainer con) throws WGAPIException {
        super(con);
        _info = new FCMetadataInfo();
    }

    /**
     * @return Returns the category.
     */
    public String getCategory() {
    	String cat = ((FCMetadataInfo)_info).getCategory();
        return (cat == null || cat.trim().equals("") ? null : cat); 
    }

    /**
     * @param category The category to set.
     */
    public void setCategory(String category) {
    	if (category == null) {
    		((FCMetadataInfo)_info).setCategory("");
    	} else {
    		((FCMetadataInfo)_info).setCategory(category);
    	}
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.designsync.DesignMetadata#writeToDocument(de.innovationgate.webgate.api.WGDesignDocument)
     */
    public void writeToDocument(WGFileContainer doc) throws WGAPIException {
        super.writeToDocument(doc);
    }
    

    
    
    
}
