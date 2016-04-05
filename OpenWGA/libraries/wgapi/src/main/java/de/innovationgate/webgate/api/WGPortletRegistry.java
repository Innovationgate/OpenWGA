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

import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Base class for WebTML portlet registry implementations, defining shared functionality
 */
public abstract class WGPortletRegistry {

    /**
     * Inserts a new portlet to the registry
     * @param portlet The portlet to insert. Must have been created via {@link #createPortlet(String, WGPortlet)} 
     * @throws WGAPIException
     */
    public abstract void insertPortlet(WGPortlet portlet) throws WGAPIException;
    
    /**
     * Returns the portlet for the given key. Should also be able to retrieve root portlets by their key.
     * @param appDb Dbkey of the current application database
     * @param key The portlet key
     * @throws WGAPIException
     */
    public abstract WGPortlet getPortlet(String appDb, String key) throws WGAPIException;
    
    /**
     * Returns the portlet for the given name under the given parent portlet.
     * @param appDb current application database
     * @param parentPortlet The parent portlet of the portlet to retrieve
     * @param name The name of the portlet
     * @throws WGAPIException
     */
    public abstract WGPortlet getPortletByName(String appDb, WGPortlet parentPortlet, String name) throws WGAPIException;
    
    /**
     * Updates an already registered portlet with the information given in the parameter portlet object
     * @param portlet The portlet object whose data is used to update the registration
     * @throws WGAPIException
     */
    public abstract void updatePortlet(WGPortlet portlet) throws WGAPIException;
    
    /**
     * Removes the portlet of the given key from registry
     * This method should:
     * <ul>
     * <li> Remove all items registered for this portlet
     * <li> Remove the registration of the portlet from portlet registry
     * <li> Remove all child portlets the same way that this portlet was removed (including items and their child portlets)
     * </ul>
     * @param portlet The portlet to be removed
     * @throws WGAPIException
     */
    public abstract void removePortlet(WGPortlet portlet) throws WGAPIException;
    
    /**
     * Returns the root portlet for the database of the given dbkey. May creates it if an id is given and the portlet does not yet exist. However that is a decision up to the registry.
     * @param appDb The database whose root portlet is to be retrieved
     * @throws WGAPIException
     */
    public abstract WGPortlet getOrCreateRootPortlet(String appDb) throws WGAPIException;
    
    
    /**
     * Takes a newly created user profile to do some eventually neccessary initialisations on it
     * This is called on each new user profile so the registry has a chance to initialise it if neccessary.
     * Registries who have no need for this may implement this method empty.
     * @param profile The new profile
     * @throws WGAPIException
     */
    public abstract void prepareNewProfile(WGUserProfile profile) throws WGAPIException;
    
    /**
     * Called immediately before a user profile is saved, so that the portlet registry may do some work necessary before saving
     * @param profile
     * @throws WGAPIException
     */
    public abstract void preSaveProfile(WGUserProfile profile) throws WGAPIException;
    
    /**
     * Returns the child portlets for the given parent portlet 
     * @param portlet The parent portlet
     * @throws WGAPIException
     */
    public abstract List<WGPortlet> getChildPortlets(WGPortlet portlet) throws WGAPIException;
    
    
    /**
     * Returns the names of all portlet items for the given portlet
     * @param portlet The portlet
     */
    public abstract List<String> getItemNames(WGPortlet portlet) throws WGAPIException;
    
    /**
     * Tests if the given portlet has a portlet item of the given name
     * @param portlet The portlet
     */
    public abstract boolean hasItem(WGPortlet portlet, String name) throws WGAPIException;
    
    /**
     * Returns the value of a portlet item
     * @param portlet The portlet whose item is to be retrieved
     * @param name The name of the item
     * @return The item value
     */
    public abstract Object getItemValue(WGPortlet portlet, String name) throws WGAPIException;;
    
    /**
     * Sets the value of a portlet item, eventually creating it
     * @param portlet The portlet whose item is to be set
     * @param name The name of the item
     * @param value The value of the item
     */
    public abstract void setItemValue(WGPortlet portlet, String name, Object value) throws WGAPIException;;
    
    /**
     * Removes a portlet item
     * @param portlet The portlet whose item is to be removed
     * @param name The name of the item
     */
    public abstract void removeItem(WGPortlet portlet, String name) throws WGAPIException;;
    
    /**
     * Returns if the registry is transient, and it is not to expect that a new request will reproduce once registered portlets
     * @throws WGAPIException
     */
    public abstract boolean isTransient() throws WGAPIException;
    
    
    /**
     * Removes all portlet items for the given portlet
     * @param portlet The portlet whose items are to be cleared
     * @throws WGAPIException
     */
    public void clearItems(WGPortlet portlet) throws WGAPIException {
        
        Iterator<String> names = getItemNames(portlet).iterator();
        while (names.hasNext()) {
            String name = names.next();
            removeItem(portlet, name);            
        }
        
    }
    
    /**
     * Creates a new portlet object that is not yet registered.
     * To register portlets created by this method call {@link #insertPortlet(WGPortlet)} giving it as parameter.
     * @param appDb The application database of the portlet
     * @param parent The parent of the portlet to be created
     */
    public abstract WGPortlet createPortlet(String appDb, WGPortlet parent);
    
    /**
     * Returns the string ID that should be passed to the methods of this portlet registry to identify the given database
     * @param appDb The application database
     */
    public abstract String getApplicationId(WGDatabase appDb);

}
