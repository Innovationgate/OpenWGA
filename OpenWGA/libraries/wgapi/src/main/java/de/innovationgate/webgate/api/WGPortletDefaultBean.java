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

/**
 * Object representing a portlet registration on {@link WGUserProfile}
 */
public class WGPortletDefaultBean implements WGPortlet {
    
    private String _key = null;
    private String _parentPortletId = null;
    
    private String _applicationDb = null;
    private String _designDb = null;
    private String _design = null;
    private String _name = null;

    public WGPortletDefaultBean(String appDb, String id, String parentPortletId) {
        _key = id;
        _applicationDb = appDb;
        _parentPortletId = parentPortletId;
    }
        
    public WGPortletDefaultBean(String appDb, String parentPortletId) {
        _applicationDb = appDb;
        _parentPortletId = parentPortletId;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#getDesignDb()
     */
    public String getDesignDb() {
        return _designDb;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#setDesignDb(java.lang.String)
     */
    public void setDesignDb(String db) {
        _designDb = db;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#getDesign()
     */
    public String getDesign() {
        return _design;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#setDesign(java.lang.String)
     */
    public void setDesign(String design) {
        _design = design;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#getName()
     */
    public String getName() {
        return _name;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#setName(java.lang.String)
     */
    public void setName(String title) {
        _name = title;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#getId()
     */
    public String getKey() {
        return _key;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#getParentPortletId()
     */
    public String getParentPortletKey() {
        return _parentPortletId;
    }
    
    /**
     * Sets the portlet key
     * @param key
     */
    public void setKey(String key) {
        _key = key;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#isRoot()
     */
    public boolean isRoot() {
        return (_parentPortletId == null);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.webgate.api.WGPortlet#getApplicationDb()
     */
    public String getApplicationDb() {
        return _applicationDb;
    }

}
