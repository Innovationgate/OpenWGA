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

package de.innovationgate.wga.server.api.tml;

import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;

/**
 * Represents a WebTML portlet and its configuration.
 * This object is the same as the TMLPortlet object in TMLScript. Documentation on this object and its methods in more detail can therefor be found on the TMLScript reference of the OpenWGA documenation library of the respective OpenWGA version for Object "TMLPortlet".
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_INCLUDE)
public interface Portlet {

    /**
     * Returns a child portlet of the given name
     * @param name The portlet name
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Portlet child(String name) throws WGAPIException;

    /**
     * Clears the portlet configuration and state
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract void cleanup() throws WGAPIException;

    /**
     * Clears the state of a child portlet
     * @param name The portlet name
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="clearChildState")
    public abstract void clearchildstate(String name) throws WGAPIException;

    /**
     * Clears the portlet state
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="clearState")
    public abstract void clearstate() throws WGAPIException;

    /**
     * Fires a portlet event
     * @param event The portlet event
     */
    @CodeCompletion(preferredCase="fireEvent")
    public abstract void fireevent(PortletEvent event);

    /**
     * Fires a new portlet event of the given name
     * @param eventname The name of the portlet event
     */
    @CodeCompletion(preferredCase="fireEvent")
    public abstract void fireevent(String eventname);
    
    /**
     * Fires a new portlet event of the given name with the given parameters
     * @param eventName The name of the portlet event
     * @param params Parameters added to the event
     */
    @CodeCompletion(preferredCase="fireEvent")
    void fireevent(String eventName, Map<String, Object> params);

    /**
     * Forces a state on a portlet
     */
    @CodeCompletion(preferredCase="forceState")
    public abstract void forcestate() throws WGAPIException;

    /**
     * Returns the names of registered child portlets
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="childrenNames",isProperty=true)
    public abstract List<String> getchildrennames() throws WGAPIException;

    /**
     * Gets the portlet context 
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="context",isProperty=true)
    public abstract Context getcontext() throws WGAPIException;

    /**
     * Returns the names of items on this portlet configuration
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="itemNames",isProperty=true)
    public abstract List<String> getitemnames() throws WGAPIException;

    /**
     * Returns the current portlet mode
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="mode",isProperty=true)
    public abstract String getmode() throws WGAPIException;

    /**
     * Returns the name of the portlet
     */
    @CodeCompletion(preferredCase="name",isProperty=true)
    public abstract String getname();

    /**
     * Returns the absolute path of this portlet, consisting of a path of from names of all portlets up to the root 
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="portletPath",isProperty=true)
    public abstract String getportletpath() throws WGAPIException;

    /**
     *  Retrieves the root portlet
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="root",isProperty=true)
    public abstract Portlet getroot() throws WGAPIException;

    /**
     * Retrieves the value of a WebTML portlet session variable
     * @param name Name of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="getSessionVar")
    public abstract Object getsessionvar(String name) throws WGAPIException;

    /**
     * Returns the portlet that threw a portlet event
     * @param event The portlet event
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="getSourcePortlet")
    public abstract Portlet getsourceportlet(PortletEvent event) throws WGAPIException;

    /**
     * Returns the name of the layout WebTML module for this portlet
     */
    @CodeCompletion(preferredCase="tml",isProperty=true)
    public abstract String gettml();

    /**
     * Returns the key of the application containing the layout module of this portlet
     */
    @CodeCompletion(preferredCase="tmlDB", isProperty=true)
    public abstract String gettmldb();

    /**
     * Retrieves the value of a normal WebTML portlet variable
     * @param name Name of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="getVar")
    public abstract Object getvar(String name) throws WGAPIException;

    /**
     * Tests if an item exists on the portlet configuration
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="hasItem")
    public abstract boolean hasitem(String name) throws WGAPIException;

    /**
     * Shows if this portlet is the root portlet
     */
    @CodeCompletion(preferredCase="isRoot")
    public abstract boolean isroot();

    /**
     * Returns the value of an item from portlet configuration as a single value
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Object item(String name) throws WGAPIException;

    /**
     * Returns the value of an item from portlet configuration as a list value
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="itemList")
    public abstract List<Object> itemlist(String name) throws WGAPIException;

    /**
     * Returns the parent portlet. If this is the root portlet returns null. 
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract Portlet parent() throws WGAPIException;

    /**
     * Registers a new child portlet
     * @param name Name by which the portlet is registered. May consist of alphanumeric characters, dots, underlines and colons.
     * @param module Name of the WebTML module to use for portlet layout.
     * @return The internal portlet key of the created portlet
     * @throws WGException
     */
    @CodeCompletion(preferredCase="registerPortletForName")
    public abstract String registerportletforname(String name, String module) throws WGException;

    /**
     * Registers a new child portlet
     * @param name Name by which the portlet is registered. May consist of alphanumeric characters, dots, underlines and colons.
     * @param module Name of the WebTML module to use for portlet layout
     * @param overwrite Controls if an already existing portlet registration may be overwritten. If false an error is thrown when the given name is already registered.
     * @return The internal portlet key of the created portlet
     * @throws WGException
     */
    @CodeCompletion(preferredCase="registerPortletForName")
    public abstract String registerportletforname(String name, String module, boolean overwrite) throws WGException;

    /**
     * Registers a new child portlet
     * @param name Name by which the portlet is registered. May consist of alphanumeric characters, dots, underlines and colons.
     * @param moduleDb Database key of the application that hosts the WebTML module to use for portlet layout. Omit to use the current WebTML design application.
     * @param module Name of the WebTML module to use for portlet layout
     * @return The internal portlet key of the created portlet
     * @throws WGException
     */
    @CodeCompletion(preferredCase="registerPortletForName")
    public abstract String registerportletforname(String name, String moduleDb, String module) throws WGException;

    /**
     * Registers a new child portlet
     * @param name Name by which the portlet is registered. May consist of alphanumeric characters, dots, underlines and colons.
     * @param moduleDb Database key of the application that hosts the WebTML module to use for portlet layout. Omit to use the current WebTML design application.
     * @param module Name of the WebTML module to use for portlet layout
     * @param overwrite Controls if an already existing portlet registration may be overwritten. If false an error is thrown when the given name is already registered.
     * @return The internal portlet key of the created portlet
     * @throws WGException
     */
    @CodeCompletion(preferredCase="registerPortletForName")
    public abstract String registerportletforname(String name, String moduleDb, String module, boolean overwrite) throws WGException;

    /**
     * Removes an item from portlet configuration
     * @param name Name of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="removeItem")
    public abstract void removeitem(String name) throws WGAPIException;

    /**
     * Removes a WebTML portlet session variable
     * @param name Name of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="removeSessionVar")
    public abstract void removesessionvar(String name) throws WGAPIException;

    /**
     * Removes a WebTML portlet variable
     * @param name Name of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="removeVar")
    public abstract void removevar(String name) throws WGAPIException;

    /**
     * Sets the portlet context 
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="context",isProperty=true)
    public abstract void setcontext(Context context) throws WGAPIException;

    /**
     * Sets an item in portlet configuration
     * @param name Name of the item
     * @param value Value of the item
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="setItem")
    public abstract boolean setitem(String name, Object value) throws WGAPIException;

    /**
     * Sets the current portlet mode
     * @param mode The mode to set
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="mode",isProperty=true)
    public abstract void setmode(String mode) throws WGAPIException;

    /**
     * Sets a WebTML portlet session variable. This variant creates session variables that are allowed for serialisation in a cluster..
     * @param name Name of the variable
     * @param value Value of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="setSessionVar")
    public abstract void setsessionvar(String name, Object value) throws WGAPIException;

    /**
     * Sets a WebTML portlet session variable
     * @param name Name of the variable
     * @param value Value of the variable
     * @param allowSerialization Controls if this variable should be serialized to other cluster nodes in a server cluster. This is only possible if the value type is serializable.
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="setSessionVar")
    public abstract void setsessionvar(String name, Object value, boolean allowSerialization) throws WGAPIException;

    /**
     * Sets a WebTML portlet variable
     * @param name Name of the variable
     * @param value Value of the variable
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="setVar")
    public abstract void setvar(String name, Object value) throws WGAPIException;

    /**
     * Removes the current portlet from portlet registry. 
     * @throws WGAPIException
     */
    @CodeCompletion
    public abstract void unregister() throws WGAPIException;

    /**
     * Removes all child portlets from portlet registry
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="unregisterChildPortlets")
    public abstract void unregisterchildportlets() throws WGAPIException;

    /**
     * Removes a child portlet from portlet registry 
     * @param name Name of the child portlet
     * @throws WGAPIException
     */
    @CodeCompletion(preferredCase="unregisterPortletForName")
    public abstract void unregisterportletforname(String name) throws WGAPIException;

    @CodeCompletion(preferredCase="getController")
    public abstract Object getcontroller() throws WGAPIException, WGException;
    
}