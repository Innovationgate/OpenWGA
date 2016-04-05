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

package de.innovationgate.wgaservices.types;

import java.util.ArrayList;
import java.util.List;

/**
 * Information about a configured database server
 */
public class DatabaseServerInfo {

    private String _uid;
    private String _title;
    private String _description;
    
    private List<String> _createableContentStoreImplemenations = new ArrayList<String>();
    
    /**
     * Returns the server UID
     */
    public String getUid() {        
        return _uid;
    }
    /**
     * * Sets the server UID
     */
    public void setUid(String uid) {
        _uid = uid;
    }
    /**
     * Returns the server title
     */
    public String getTitle() {
        return _title;
    }
    /**
     * Sets the server title
     */
    public void setTitle(String title) {
        _title = title;
    }
    /**
     * Returns the server description
     */
    public String getDescription() {
        return _description;
    }
    /**
     * Sets the server description
     */
    public void setDescription(String description) {
        _description = description;
    }
    /**
     * Sets the list of database implementations createable on this server
     */
    public void setCreateableContentStoreImplemenations(List<String> createableContentStoreImplemenations) {
        _createableContentStoreImplemenations = createableContentStoreImplemenations;
    }
    /**
     * Returns the list of database implementations createable on this server
     */
    public List<String> getCreateableContentStoreImplemenations() {
        return _createableContentStoreImplemenations;
    }       
}
