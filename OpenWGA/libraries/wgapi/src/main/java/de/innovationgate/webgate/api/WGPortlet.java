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
 * Registration data of a WebTML portlet
 */
public interface WGPortlet {

    /**
     * Returns the database key of the application containing the portlet design
     */
    public abstract String getDesignDb();

    /**
     * Sets the database key of the application containing the portlet design
     */
    public abstract void setDesignDb(String db);


    /**
     * Returns the name of the WebTML module used as portlet design
     */
    public abstract String getDesign();

    /**
     * Sets the name of the WebTML module used as portlet design
     */
    public abstract void setDesign(String design);

    /**
     * Returns the registration name of the portlet
     */
    public abstract String getName();

    /**
     * Sets the registration name of the portlet
     */
    public abstract void setName(String title);

    /**
     * Returns the registration key of the portlet
     */
    public abstract String getKey();

    /**
     * Returns the registration key of the parent portlet
     */
    public abstract String getParentPortletKey();

    /**
     * Determines if this is the root portlet
     */
    public abstract boolean isRoot();

    /**
     * Returns the database key of the application using this portlet registration
     */
    public abstract String getApplicationDb();

}
