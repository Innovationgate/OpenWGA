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

package de.innovationgate.webgate.api.jdbc;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import de.innovationgate.webgate.api.PortletItemStorageData;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGPortletDefaultBean;
import de.innovationgate.webgate.api.WGSystemException;

public class UserProfilePortlet extends Entity implements WGPortlet, PortletItemStorageData {
    
    private String key;
    private UserProfile parentprofile;
    private String designdb;
    private String design;
    private UserProfilePortlet parentportlet;
    private String name;
    private String appdb;
    private Map<String,UserProfileItem> items;

    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#getDb()
     */
    public String getDesigndb() {
        return designdb;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#setDb(java.lang.String)
     */
    public void setDesigndb(String db) {
        this.designdb = db;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#getDesign()
     */
    public String getDesign() {
        return design;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#setDesign(java.lang.String)
     */
    public void setDesign(String design) {
        this.design = design;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#setParent(java.lang.String)
     */
    public void setParentportlet(UserProfilePortlet parent) {
        this.parentportlet = parent;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#getTitle()
     */
    public String getName() {
        return name;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#setTitle(java.lang.String)
     */
    public void setName(String title) {
        this.name = title;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#getParentprofile()
     */
    public UserProfile getParentprofile() {
        return parentprofile;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.jdbc.WGPortlet#setParentprofile(de.innovationgate.webgate.api.jdbc.UserProfile)
     */
    public void setParentprofile(UserProfile parentprofile) {
        this.parentprofile = parentprofile;
    }
    public String getAppdb() {
        return appdb;
    }
    public void setAppdb(String rootdb) {
        this.appdb = rootdb;
    }
    public UserProfilePortlet getParentportlet() {
        return parentportlet;
    }
    public Map<String, UserProfileItem> getItems() {
        return items;
    }
    public void setItems(Map<String, UserProfileItem> items) {
        this.items = items;
    }
    public String getApplicationDb() {
        return getAppdb();
    }
    public String getDesignDb() {
        return getDesigndb();
    }
    public String getParentPortletKey() {
        if (parentportlet != null) {
            return parentportlet.getKey();
        }
        else {
            return null;
        }
    }
    public boolean isRoot() {
        return (parentportlet == null);
    }
    public void setDesignDb(String db) {
        setDesigndb(db);
    }
    public String getKey() {
        return key;
    }
    public void setKey(String key) {
        this.key = key;
    }
    
    @Override
    public Set<String> getItemNames() {
        return items.keySet();
    }
    
    @Override
    public Object getItemValue(String itemName) throws WGAPIException {
        
        String lcName = itemName.toLowerCase().trim(); 
        UserProfileItem item = getItems().get(lcName);
        if (item != null) {
            return WGDocumentImpl.readItemValue(null, null, item);
        }
        
        // Workaround for wrongly stored items due to bug #00000316
        item = getItems().get(itemName);
        if (item != null) {
            return WGDocumentImpl.readItemValue(null, null, item);
        }
        
        return null;
    }
    

}
