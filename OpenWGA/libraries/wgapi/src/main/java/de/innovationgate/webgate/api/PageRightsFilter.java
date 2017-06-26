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
 * A filter to do custom determination of rights on pages
 */
public interface PageRightsFilter {
    
    public enum Right {
        /**
         * Permits the operation and also skips all default checks for permission that the WGAPI normally performs,
         * so the user is effectively allowed.
         */
        ALLOWED_SKIP_DEFAULT_CHECKS,
        
        /**
         * Permits the operation if the default permission checks of the WGAPI also allow it
         */
        ALLOWED,
        
        /**
         * Denies the operation
         */
        DENIED
    }
    
    public void init(WGDatabase db);
    
    /**
     * Called to determine if the current user may read contents on the given page
     * @param page The page
     * @param userAccess User information
     */
    public Right mayReadContent(WGStructEntry page, WGUserAccess userAccess);
    
    /**
     * Called to determine if the current user may edit data on the given page
     * @param page The page
     * @param userAccess User information
     */
    public Right mayEditPage(WGStructEntry page, WGUserAccess userAccess);
    
    /**
     * Called to determine if the current user may edit data on child pages of the given page
     * @param page The page
     * @param userAccess User information
     */
    public Right mayEditChildPages(WGStructEntry page, WGUserAccess userAccess);

    /**
     * Called to determine if the current user may edit content of a given language on this page
     * @param page The page
     * @param userAccess User information
     */
	public Right mayEditContent(WGStructEntry page, WGUserAccess userAccess, WGLanguage wgLanguage);

}
