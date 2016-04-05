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

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;


/**
 * An object implementing a language choosage functionality. Usable everywhere in the WGAPI where a language version of a content is to be chosen automatically.
 */
public interface WGLanguageChooser {
    
    /**
     * Chooses the language version of a content document for a given unique name
     * @param db The database containing the document
     * @param name The unique name
     * @param authoring True if authoring mode is enabled, i.E. draft documents may be returned
     * @return The content or null if no appropriate was found
     * @throws WGAPIException
     */
    public WGContent selectContentForName(WGDatabase db, String name, boolean authoring) throws WGAPIException;
    
    /**
     * Chooses the language version of a content document for a given pagee 
     * @param page The page
     * @param authoring True if authoring mode is enabled, i.E. draft documents may be returned
     * @return The content or null if no appropriate was found
     * @throws WGAPIException
     */
    public WGContent selectContentForPage(WGStructEntry page, boolean authoring) throws WGAPIException;
    
    /**
     * Chooses a language to use for the given database in some contextless functionality
     * @param db The database
     * @return The language or null if no appropriate was found
     * @throws WGAPIException
     */
    public WGLanguage selectDatabaseLanguage(WGDatabase db) throws WGAPIException;
    
    /**
     * Chooses languages to select in a query. Optional feature
     * The returned list is a languages list with descending order of priority.
     * Queries that only fetch content keys will be able to retrieve results in multiple languages and then filter the best match by this list.
     * @param db The database
     * @return The language or null if no appropriate was found
     * @throws WGAPIException
     * @throws WGNotSupportedException If the chooser does not support this functionality
     */
    public List<WGLanguage> getQueryLanguages(WGDatabase db) throws WGAPIException, WGNotSupportedException;
    
    /**
     * Orders the content of a struct entry in the order they should be priorized language-wise. A single content of every publishable language must be included in the result list.
     * @param entry The entry whose publishable contents to order
     * @param authoring True if authoring mode is enabled, i.E. draft documents may be returned
     * @return A list giving the contents in the determined priority order
     * @throws WGAPIException
     */
    public List<WGContent> selectContentPriorityOrder(WGStructEntry entry, boolean authoring) throws WGAPIException;

}
