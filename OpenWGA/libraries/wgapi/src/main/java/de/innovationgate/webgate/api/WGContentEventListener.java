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

import java.util.EventListener;

/**
 * A listener for content events in a database
 */
public interface WGContentEventListener extends EventListener {

	/**
	 * Triggered when a content is created.
	 * @param contentEvent Event informations
	 */
	public void contentCreated(WGContentEvent contentEvent) throws WGAPIException;
	
	/**
	 * Triggered just before a content is saved
	 * @param contentEvent Event information
	 * @return true, when the operation should continue. false if the saving should be canceled
	 */
	public boolean contentSaved(WGContentEvent contentEvent) throws WGAPIException;

    /**
     * Triggered after a content has been saved.
     * @param event Event information
     */
    public void contentHasBeenSaved(de.innovationgate.webgate.api.WGContentEvent event) throws WGAPIException;

    /**
     * Triggered after a content has been deleted.
     * @param event Event information
     */
    public void contentHasBeenDeleted(de.innovationgate.webgate.api.WGContentEvent event) throws WGAPIException;
    
    /**
     * Triggered after a content has been moved.
     * @param event Event information
     */
    public void contentHasBeenMoved(de.innovationgate.webgate.api.WGContentEvent event) throws WGAPIException;
    
    
    /**
     * Triggered after the status a content has been changed
     * @param event Event information
     */
    public void contentStatusChanged(de.innovationgate.webgate.api.WGContentEvent event) throws WGAPIException;
}
