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

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.wga.model.DesignMetadataInfo;

public class DesignMetadata {
	
	protected DesignMetadataInfo _info;
	private int _headerLines;
    
    public DesignMetadata() {
    	_info = new DesignMetadataInfo();
    }
    
    public DesignMetadata(WGDesignDocument doc) throws WGAPIException {
    	_info = new DesignMetadataInfo();
        setDescription(doc.getDescription());
    } 

    /**
     * @return Returns the description.
     */
    public String getDescription() {
    	String desc = _info.getDescription();
        return (desc == null || desc.trim().equals("") ? null : desc);
    }

    /**
     * @param description The description to set.
     */
    public void setDescription(String descriptionParam) {
    	_info.setDescription(descriptionParam);
        if (_info.getDescription() == null) {
            _info.setDescription("");
        }
    }

    public void writeToDocument(WGDesignDocument doc) throws WGAPIException {
        doc.setDescription(getDescription());
         
    }
    
    public void setMetadataFromHeader(String property, String value) {
        _info.setMetadataFromHeader(property, value);
    }

	public DesignMetadataInfo getInfo() {
		return _info;
	}

	public void setInfo(DesignMetadataInfo info) {
		_info = info;
	}

    public int getHeaderLines() {
        return _headerLines;
    }

    public void setHeaderLines(int headerLines) {
        _headerLines = headerLines;
    }

}
