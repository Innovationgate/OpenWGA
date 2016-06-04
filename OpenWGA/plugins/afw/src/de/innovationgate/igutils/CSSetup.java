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
/*
 * Created on 13.03.2008 from oliverweise
 *
 */
package de.innovationgate.igutils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;

/**
 * Class to comfortably setup a WGA Content Store with neccessary documents in a
 * connection script. The methods of this class ensure the existence of various
 * documents and creates them if they do not exist.
 */
public class CSSetup {

    /**
     * Interface to issue a special initialisation function to a getOrCreate...
     * method, to be implemented as method {@link #init(WGDocument)}. 
     * The function is only called if the document is created. The created document is passed as parameter.
     */
    public interface InitFunction {
        /**
         * Method that is called for initialisation
         * @param doc The document that is to be initialized
         * @throws WGAPIException
         */
        public void init(WGDocument doc) throws WGAPIException;
    }

    private WGDatabase _db;
    private String _adminRole = "#admin";
    private boolean _logOutput = false;
    private Logger _log = Logger.getLogger("wga.igutils.cssetup");

    /**
     * Public constructor
     * 
     * @param db
     *            Database to setup
     */
    public CSSetup(WGDatabase db) {
        _db = db;
    }

    /**
     * Gets or creates a content type of the given name
     * 
     * @param name
     *            The content type name
     * @param initFunction
     *            An initialisiation function that is called when the document
     *            is created
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGContentType getOrCreateContentType(String name, InitFunction initFunction) throws WGAPIException {

        WGContentType ct = _db.getContentType(name);
        if (ct != null) {
            return ct;
        }

        log("No content type \"" + name + "\" found. Creating content type.");
        ct = _db.createContentType(name);
        if (initFunction != null) {
            log("Running initialization of content type \"" + name + "\".");
            initFunction.init(ct);
        }
        ct.save();

        return ct;

    }

    /**
     * Gets or creates a content type of the given name
     * 
     * @param name
     *            The content type name
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGContentType getOrCreateContentType(String name) throws WGAPIException {
        return getOrCreateContentType(name, null);
    }

    /**
     * Gets or creates an area of the given name
     * 
     * @param name
     *            The area name
     * @param initFunction
     *            An initialisiation function that is called when the document
     *            is created
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGArea getOrCreateArea(String name, InitFunction initFunction) throws WGAPIException {

        WGArea area = _db.getArea(name);
        if (area != null) {
            return area;
        }

        log("No area \"" + name + "\" found. Creating area.");
        area = _db.createArea(name);
        if (initFunction != null) {
            log("Running initialization of area \"" + name + "\".");
            initFunction.init(area);
        }
        area.save();

        return area;

    }

    /**
     * Gets or creates an areae of the given name
     * 
     * @param name
     *            The area name
     *            is created
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGArea getOrCreateArea(String name) throws WGAPIException {
        return getOrCreateArea(name, null);
    }

    /**
     * Gets or creates a language definition of the given name
     * @param name The language code
     * @param initFunction An initialisiation function that is called when the document is created
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGLanguage getOrCreateLanguage(String name, InitFunction initFunction) throws WGAPIException {

        WGLanguage lang = _db.getLanguage(name);
        if (lang != null && !lang.isDummy()) {
            return lang;
        }

        log("No language definition \"" + name + "\" found. Creating language.");
        lang = _db.createLanguage(name, name);
        if (initFunction != null) {
            log("Running initialization of language definition \"" + name + "\".");
            initFunction.init(lang);
        }
        lang.save();

        return lang;

    }

    /**
     * Gets or creates a language definition of the given name
     * @param name The language code
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGLanguage getOrCreateLanguage(String name) throws WGAPIException {
        return getOrCreateLanguage(name, null);
    }

    /**
     * Gets or creates a language definition for the default language of this database.
     * @param initFunction An initialisiation function that is called when the document is created
     * @return The existent or just created document
     * @throws WGAPIException
     */
    public WGLanguage getOrCreateDefaultLanguage(InitFunction initFunction) throws WGAPIException {
        return getOrCreateLanguage(_db.getDefaultLanguage(), initFunction);
    }
    
    /**
     * Gets or creates a language definition for the default language of this database.
     * @throws WGAPIException
     */
    public WGLanguage getOrCreateDefaultLanguage() throws WGAPIException {
        return getOrCreateLanguage(_db.getDefaultLanguage());
    }

    /**
     * Convenience method to create a content document to be used as application profile.
     * Such an application profile can be used to store global settings for the content store.
     * It is automatically hidden from all navigation structures and invisible to publishing.
     * It gets the unique name "$profile", which can be used for retrieving it, and is created 
     * in an area "system", which is also automatically created. It receives the, also automatically
     * created, content type "AppProfile" and database default language.
     * @param userInitFunction An initialisiation function that is called when the profile is created
     * @return The existent or just created application profile.
     * @throws WGAPIException
     */
    public WGContent getOrCreateAppProfile(final InitFunction userInitFunction) throws WGAPIException {

        WGContentType profileCT = getOrCreateContentType("AppProfile", 
                new InitFunction() {
                    public void init(WGDocument doc) throws WGAPIException {
                        WGContentType ct = (WGContentType) doc;
                        ct.setPositioning(WGContentType.POSITIONING_ROOTENTRIES);
                        ct.setContentCreators(Collections.singletonList(getAdminRole()));
                        ct.setDescription("Application profile document storing global settings");
                    }
                }
        );
        
        WGArea systemArea = getOrCreateArea("system");
        WGLanguage defaultLanguage = getOrCreateDefaultLanguage();
                
        return getOrCreateNamedPage(systemArea, profileCT, defaultLanguage, "$profile",
                new InitFunction() {
                    public void init(WGDocument doc) throws WGAPIException {
                        WGContent content = (WGContent) doc;
                        content.setTitle("Application profile");
                        List hiddenFrom = new ArrayList();
                        hiddenFrom.add(WGContent.DISPLAYTYPE_NAVIGATOR);
                        hiddenFrom.add(WGContent.DISPLAYTYPE_SEARCH);
                        hiddenFrom.add(WGContent.DISPLAYTYPE_SITEMAP);
                        content.setHiddenFrom(hiddenFrom);
                        content.setVisible(false);
                        
                        if (userInitFunction != null) {
                            userInitFunction.init(content);
                        }
                    }
                }
        );
    }

    /**
     * Gets or creates a named page, i.e. a struct entry plus content with a given unique name.
     * If the method does not find a named page in the given language, it searches all other defined languages of the database
     * for that name. If it finds a content there, which has the same parent as the one given as parameter, it will use it's struct entry
     * to create the named content. That way, multiple calls of this method with equal name and parent but different languages 
     * will create contents under the same struct entry.
     * The created content will be published when this method exits.
     * @param parent The parent of the page. Either a {@link WGArea} for roots or a {@link WGStructEntry} for child pages
     * @param contentType The content type to use for the page
     * @param lang The language for the content of the page
     * @param name The unique name that the content of the page gets.
     * @param initFunction An initialisiation function that is called when the content is created
     * @return The {@link WGContent} of the created page
     * @throws WGAPIException
     */
    public WGContent getOrCreateNamedPage(WGDocument parent, WGContentType contentType, WGLanguage lang, String name, InitFunction initFunction) throws WGAPIException {
        
        if (!(parent instanceof WGArea) && !(parent instanceof WGStructEntry)) {
            throw new IllegalArgumentException("Argument parent must be of type WGArea or WGStructEntry");
        }
        
        WGContent content = _db.getContentByName(name, lang.getName());
        if (content != null) {
            return content;
        }        
        
        log("No page of name \"" + name + "\" for language \"" + lang.getName() + "\" found. Creating named page.");
        
        
        // Try to find the same name in another language. If we find one that has the our needed parent, we will use it's struct entry for creating ours
        WGStructEntry entry = null;
        Iterator langs = _db.getLanguages().values().iterator();
        while (langs.hasNext()) {
            WGLanguage otherLang = (WGLanguage) langs.next();
            WGContent otherLangContent = _db.getContentByName(name, otherLang.getName());
            if (otherLangContent != null) {
                if (parent instanceof WGArea) {
                    if (otherLangContent.getStructEntry().isRoot() && otherLangContent.getStructEntry().getArea().equals(parent)) {
                        entry = otherLangContent.getStructEntry();
                        break;
                    }
                    else if (!otherLangContent.getStructEntry().isRoot() && otherLangContent.getStructEntry().getParentEntry().equals(parent)) {
                        entry = otherLangContent.getStructEntry();
                        break;
                    }
                }
            }
        }
        
        // If no entry was found we create a new one under the given parent
        if (entry == null) {
            entry = _db.createStructEntry(parent, contentType, name);
            entry.save();
        }

        content = entry.createContent(lang, name);
        content.setUniqueName(name);
        if (initFunction != null) {
            log("Running named page initialization.");
            initFunction.init(content);
        }

        content.publish("");
        return content;
    }
    
    /**
     * Gets or creates a named page, i.e. a struct entry plus content with a given unique name.
     * If the method does not find a named page in the given language, it searches all other defined languages of the database
     * for that name. If it finds a content there, which has the same parent as the one given as parameter, it will use it's struct entry
     * to create the named content. That way, multiple calls of this method with equal name and parent but different languages 
     * will create contents under the same struct entry.
     * The created content will be published when this method exits.
     * @param parent The parent of the page. Either a {@link WGArea} for roots or a {@link WGStructEntry} for child pages
     * @param contentType The content type to use for the page
     * @param lang The language for the content of the page
     * @param name The unique name that the content of the page gets.
     * @return The {@link WGContent} of the created page
     * @throws WGAPIException
     */
    public WGContent getOrCreateNamedPage(WGDocument parent, WGContentType contentType, WGLanguage lang, String name) throws WGAPIException {
        return getOrCreateNamedPage(parent, contentType, lang, name, null);
    }
    
    /**
     * This version of {@link #getOrCreateNamedPage(WGDocument, WGContentType, WGLanguage, String)} will create named pages
     * in all languages that are defined for the database.
     * @param parent The parent of the page. Either a {@link WGArea} for roots or a {@link WGStructEntry} for child pages
     * @param contentType The content type to use for the page
     * @param name The unique name that the content of the page gets.
     * @param initFunction
     *            An initialisiation function that is called when each content
     *            is created
     * @return The {@link WGStructEntry} that is the home of the created named pages
     * @throws WGAPIException
     */
    public WGStructEntry getOrCreateNamedPages(WGDocument parent, WGContentType contentType, String name, InitFunction initFunction) throws WGAPIException {
        
        Iterator langs = _db.getLanguages().values().iterator();
        WGContent content = null;
        while (langs.hasNext()) {
            WGLanguage lang = (WGLanguage) langs.next();
            content = getOrCreateNamedPage(parent, contentType, lang, name, initFunction);
        }
        
        return content.getStructEntry();
        
    }
    
    /**
     * This version of {@link #getOrCreateNamedPage(WGDocument, WGContentType, WGLanguage, String)} will create named pages
     * in all languages that are defined for the database.
     * @param parent The parent of the page. Either a {@link WGArea} for roots or a {@link WGStructEntry} for child pages
     * @param contentType The content type to use for the page
     * @param name The unique name that the content of the page gets.
     * @return The {@link WGStructEntry} that is the home of the created named pages
     * @throws WGAPIException
     */
    public WGStructEntry getOrCreateNamedPages(WGDocument parent, WGContentType contentType, String name) throws WGAPIException {
        return getOrCreateNamedPages(parent, contentType, name, null);
    }

    /**
     * Convenience method to create a content document to be used as application profile.
     * Such an application profile can be used to store global settings for the content store.
     * It is automatically hidden from all navigation structures and invisible to publishing.
     * It gets the unique name "$profile", which can be used for retrieving it, and is created 
     * in an area "system", which is also automatically created. It receives the, also automatically
     * created, content type "AppProfile" and database default language.
     * @return The existent or just created application profile.
     * @throws WGAPIException
     */
    public WGContent getOrCreateAppProfile() throws WGAPIException {
        return getOrCreateAppProfile(null);
    }

    /**
     * Returns the ACL role, that is set on places where access should be restricted for administrators
     * (for example: Creation of application profiles). The default is "#admin".
     */
    public String getAdminRole() {
        return _adminRole;
    }

    /**
     * Sets the ACL role, that is set on places where access should be restricted for administrators
     * (for example: Creation of application profiles). The default is "#admin".

     */
    public void setAdminRole(String adminRole) {
        _adminRole = adminRole;
    }

    /**
     * Returns if the CSSetup object is to write messages about its operations to the application log. Defaults to false.
     */
    public boolean isLogOutput() {
        return _logOutput;
    }

    /**
     * Sets if the CSSetup object is to write messages about its operations to the application log. Defaults to false.
     */
    public void setLogOutput(boolean logOutput) {
        _logOutput = logOutput;
    }
    
    private void log(String msg) {
        if (isLogOutput()) {
            _log.info(msg);
        }
    }

}
