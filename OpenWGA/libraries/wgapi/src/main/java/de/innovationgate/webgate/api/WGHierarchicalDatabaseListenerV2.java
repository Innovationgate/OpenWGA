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
 * extends the {@link WGHierarchicalDatabaseListener}
 * - methods for content move operations
 * - name
 *
 */
public interface WGHierarchicalDatabaseListenerV2 extends
		WGHierarchicalDatabaseListener {

    
    /**
     * Called before a content is moved on the target hierarchy
     * @param event The event context
     * @throws Throwable
     */
    public void preMoveContentTo(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called after the content was moved on the target hierarchy
     * @param event The event context
     * @throws Throwable
     */
    public void postMoveContentTo(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called before the content is moved on the source hierarchy
     * @param event The event context
     * @throws Throwable
     */
    public void preMoveContentFrom(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called after the content is moved on the source hierarchy
     * @param event The event context
     * @throws Throwable
     */
    public void postMoveContentFrom(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * returns the name of the listener
     */
    public String getName();
	
}
