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

package de.innovationgate.wgpublisher.lang;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * An object implementing a language behaviour, deciding what language to choose in diverse situations.
 */
public interface LanguageBehaviour {
    
    /**
     * Selects a content from a page on based on request locales
     * @param page The page
     * @param req The current request
     * @param isBI Is true when the user is logged in to authoring
     * @return The content to choose or null if none is appropriate
     * @throws WGAPIException
     */
    public WGContent requestSelectContentForPage(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException;
    
    /**
     * Selects a content for an unique name based on request locales
     * @param db The database holding the content
     * @param req The current request
     * @param name The unique name
     * @param isBI Is true when the user is logged in to authoring
     * @return The content for the name or null if none is appropriate
     * @throws WGAPIException
     */
    public WGContent requestSelectContentForName(WGDatabase db, HttpServletRequest req, String name, boolean isBI) throws WGAPIException;

    /**
     * Selects a language from a database based on request locales
     * @param db The database
     * @param req The current request
     * @return The language to choose or null if none is appropriate
     * @throws WGAPIException
     */
    public WGLanguage requestSelectDatabaseLanguage(WGDatabase db, HttpServletRequest req) throws WGAPIException;
    
    /**
     * Selects a content from a page inside processing of a WebTML request
     * @param page The page
     * @param sourceContext The context from whom to the page is addressed. Note that this may be a context from another database.
     * @param isBI Is true when the user is logged in to authoring
     * @return The content to choose or null if none is appropriate
     * @throws WGAPIException
     */
    public WGContent webtmlSelectContentForPage(WGStructEntry page, TMLContext sourceContext, boolean isBI) throws WGAPIException;
    
    /**
     * Selects a content for an unique name inside processing of a WebTML request
     * @param db The database
     * @param context The context from whom the function is called. Note that this may be a context from another database.
     * @param name The unique name
     * @param isBI Is true when the user is logged in to authoring
     * @return The content to choose or null if none is appropriate
     * @throws WGAPIException
     */
    public WGContent webtmlSelectContentForName(WGDatabase db, TMLContext context, String name, boolean isBI) throws WGAPIException;
    
    /**
     * Selects a language from a database inside processing of a WebTML request
     * @param db The database
     * @param sourceContext The context from whom the function is called. Note that this may be a context from another database. 
     * @return The language to choose or null if none is appropriate
     * @throws WGAPIException
     */
    public WGLanguage webtmlSelectDatabaseLanguage(WGDatabase db, TMLContext sourceContext) throws WGAPIException;
    
    /**
     * Fetches an WebTML label inside processing of a WebTML request
     * @param manager The resource bundle manager for those labels
     * @param context The context from whom the function is called. Note that this may be a context from another database.
     * @param container The label container name
     * @param file The label file name
     * @param key The label key
     * @return A label or null if no appropriate was found.
     * @throws WGAPIException
     */
    public String webtmlFetchLabel(WGAResourceBundleManager manager, TMLContext context, String container, String file, String key) throws WGAPIException;
    
    /**
     * Returns the languages to return in a WebTML query
     * @param db The database running the query
     * @param context The context from whom the query is executed. Note that this may be a context from another database.
     * @return A list of languages to include in the result list
     * @throws WGAPIException
     */
    public List<WGLanguage> webtmlQueryLanguages(WGDatabase db, TMLContext context) throws WGAPIException;
    


    /**
     * Returns a language to prefer in the current request for the given database. Used to fill this.preferredLanguage for functionalities still using this.
     * @param context The database for whom the preferred language is to be determined
     * @param context The context from whom the function is called. Note that this may be a context from another database.
     * @return Name of a language to prefer or null if no appropriate was found
     * @throws WGAPIException
     */
    public String webtmlGetPreferredLanguage(WGDatabase db, TMLContext context) throws WGAPIException;

}
