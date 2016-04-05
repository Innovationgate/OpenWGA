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
package de.innovationgate.webgate.api.templates;

import java.util.List;

/**
 * This object is used by SimpleContentSource implementations to tell the template which features to support.
 */
public class ContentSourceSpecs {

	private boolean _queryable = true;
	private boolean _browseable = false;
	private boolean _maintainsLastChanged = false;
	private boolean _writable = false;
	private boolean _useMasterLogin = true;
	private boolean _dynamicFolders = false;
	private boolean _calculatesKeys = false;
	private boolean _lowerCaseItems = false;
    private boolean _servePropertiesAsMetas = true;
    private boolean _contentReadProtected = true;
    
	private List _procedures = null;
    

	/**
	 * Returns, if this SCS implementation is browseable (supports browsing folders). 
	 */
	public boolean isBrowseable() {
		return _browseable;
	}

	/**
	 * Returns, if the SCS implementation maintains the last changed date.
	 */
	public boolean isMaintainsLastChanged() {
		return _maintainsLastChanged;
	}

	/**
	 * Retrieves the defined procedures. NOT YET IMPLEMENTED!
	 */
	public List getProcedures() {
		return _procedures;
	}

	/**
	 * Returns if this database is queryable.
	 */
	public boolean isQueryable() {
		return _queryable;
	}

	/**
	 * Returns if this SCS implementation is writable, i.e. supports creating, modifying and deleting beans
	 */
	public boolean isWritable() {
		return _writable;
	}

	/**
	 * Sets, if this SCS implementation is browseable (supports browsing folders).
	 * To support this, the implementation must implement the following methods:
	 * <ul>
	 * <li>{@link  de.innovationgate.webgate.api.templates.SimpleContentSource#browse browse}</li>
	 * <li>{@link  de.innovationgate.webgate.api.templates.SimpleContentSource#getContent getContent}</li>
	 * <li>{@link  de.innovationgate.webgate.api.templates.SimpleContentSource#browse browse}</li> 
	 * </ul>
	 * @param b
	 */
	public void setBrowseable(boolean b) {
		_browseable = b;
	}

	/**
	 * Sets if the SCS implementation maintains the last changed date. This will be used for cache maintenance.
	 * To support this, the SCS implementation must implement the following method:
	 * 	<ul>
	 * <li>{@link de.innovationgate.webgate.api.templates.SimpleContentSource#getLastModified getLastModified}</li> 
	 * </ul>
	 */
	public void setMaintainsLastChanged(boolean b) {
		_maintainsLastChanged = b;
	}

	/**
	 * Sets the procedures that this SCS implementation should offer. NOT YET IMPLEMENTED!
	 * @param list
	 */
	public void setProcedures(List list) {
		_procedures = list;
	}

	/**
	 * Sets if this database is queryable, i.e. supports querying for content by a query language.
	 * To support this, the SCS implementation must implement the following method:
	 * 	<ul>
	 * <li>{@link de.innovationgate.webgate.api.templates.SimpleContentSource#find find}</li> 
	 * </ul>
	 */
	public void setQueryable(boolean b) {
		_queryable = b;
	}

	/**
	 * Returns if this SCS implementation is writable, i.e. supports creating, modifying and deleting beans.
	 * To support this the implementation must implement the following methods:
	 * <ul>
	 * <li>{@link de.innovationgate.webgate.api.templates.SimpleContentSource#createContent(String) createContent}</li>
	 * <li>{@link de.innovationgate.webgate.api.templates.SimpleContentSource#removeContent(String, Object) removeContent}</li>
	 * <li>{@link de.innovationgate.webgate.api.templates.SimpleContentSource#updateContent(String, Object, Object) updateContent}</li>
	 * </ul>
	 */
	public void setWritable(boolean b) {
		_writable = b;
	}

	/**
	 * Returns if this SCS implementation can use a master login. 
	 */
	public boolean isUseMasterLogin() {
		return _useMasterLogin;
	}

	/**
	 * Sets if this SCS implementation can use a master login.
	 * If so, master login information is passed to it's login method
	 * If not, using the master login automatically returns ACCESSLEVEL_READER without invoking the login-Method. 
	 * This might bypass necessary initializations in the login-Method, so disable this only with care.
	 */
	public void setUseMasterLogin(boolean b) {
		_useMasterLogin = b;
	}

	/**
	 * Returns, if the available SCS folders can change dynamically in WGA runtime.
	 */
	public boolean isDynamicFolders() {
		return _dynamicFolders;
	}

	/**
	 * Returns, if the available SCS folders can change dynamically in WGA runtime.
	 * It is not recommended to activate this feature because it may drain performance dramatically.
	 */
	public void setDynamicFolders(boolean b) {
		_dynamicFolders = b;
	}

	/**
	 * Returns, if this SCS implementation calculates it's bean keys itself. 
	 * If not so, the keys must be given from outside the WGAPI when creating them.
	 */
	public boolean isCalculatesKeys() {
		return _calculatesKeys;
	}

	/**
	 * Sets, if this SCS implementation calculates it's bean keys itself. 
	 * If not so, the keys must be given from outside the WGAPI when creating them.
	 */
	public void setCalculatesKeys(boolean b) {
		_calculatesKeys = b;
	}

	/**
	 * Returns, if all item names are converted to lower case automatically.
	 */
	public boolean isLowerCaseItems() {
		return _lowerCaseItems;
	}

	/**
	 * Sets if all item names are converted to lower case automatically before they are processed by the SCS implementation.
	 */
	public void setLowerCaseItems(boolean b) {
		_lowerCaseItems = b;
	}

    /**
     * Shows if this content source should return bean properties/map entries, whose names/keys match
     * metadata fieldnames, as metadata value. Defaults to true.
     */
    public boolean isServePropertiesAsMetas() {
        return _servePropertiesAsMetas;
    }

    /**
     * Determines, if this content source should return bean properties/map entries, whose names/keys match
     * metadata fieldnames, as metadata value. Defaults to true.
     */
    public void setServePropertiesAsMetas(boolean servePropertiesAsMetas) {
        _servePropertiesAsMetas = servePropertiesAsMetas;
    }

    public boolean isContentReadProtected() {
        return _contentReadProtected;
    }

    public void setContentReadProtected(boolean contentReadProtected) {
        _contentReadProtected = contentReadProtected;
    }

}
