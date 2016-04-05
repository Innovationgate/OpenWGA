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

package de.innovationgate.wgpublisher.webtml.portlet;


import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext;

/**
 * A storage solution for portlet states
 */
public interface TMLPortletStateStorage {

    /**
     * Retrieves the state for the given portlet
     * @throws WGAPIException
     */
    public abstract TMLPortletState getState(TMLPortlet portlet) throws WGAPIException;

    /**
     * Removes the state of the addressed portlet from the storage
     * @param appDb App id of the portlet, like given from the portlet registry
     * @param portletKey Key of the portlet
     * @throws WGAPIException
     */
    public abstract void disposeState(String appDb, String portletKey) throws WGAPIException;
    
    /**
     * Removes the state of a child portlet of addressed portlet from the storage, based on its name. 
     * This is an optional operations, only possible if the child name can be used to resolve its state.
     * Implementations not supporting this should throw WGNotSupportedException.
     * @param appDb App id of the portlet, like given from the portlet registry
     * @param portletKey Key of the parent portlet
     * @param childName Name of the child portlet whose state is to be dropped
     * @throws WGAPIException
     */
    public abstract void disposeChildState(String appDb, String portletKey, String childName) throws WGAPIException;
    
    /**
     * Returns if portlet states in this storage should store their session vars at the {@link TMLPortletState} object.
     * Otherwise they are stored at accompanying {@link TMLFormProcessContext} objects on the server.
     */
    public boolean isStoreSessionVarsAtState();
    
}