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
 * Interface for listener for HDB content events
 */
public interface WGHierarchicalDatabaseListener {
    
    /**
     * Called before a content is created
     * @param event The event context
     * @throws Throwable
     */
    public void preCreateContent(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called after a content has been created
     * @param event The event context. Does not yet contain the context document under {@link WGHierarchicalDatabaseEvent#getContent()}
     * @throws Throwable
     */
    public void postCreateContent(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called before a content is updated
     * @param event The event context
     * @throws Throwable
     */
    public void preUpdateContent(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called after a content has been updated
     * @param event The event context
     * @throws Throwable
     */
    public void postUpdateContent(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called before a content is deleted
     * @param event The event context
     * @throws Throwable
     */
    public void preDeleteContent(WGHierarchicalDatabaseEvent event) throws Throwable;
    
    /**
     * Called after a content has been deleted
     * @param event The event context. Does no more contain the context document  under {@link WGHierarchicalDatabaseEvent#getContent()}
     * @throws Throwable
     */
    public void postDeleteContent(WGHierarchicalDatabaseEvent event) throws Throwable;
                
}
