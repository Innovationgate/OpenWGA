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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.labels.WGAResourceBundle;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class StaticLanguageBehaviour implements LanguageBehaviour, PriorizingLanguageBehaviour {
    
    public WGContent requestSelectContentForName(WGDatabase db, HttpServletRequest req, String name, boolean isBI) throws WGAPIException {
        WGContent content = LanguageBehaviourTools.chooseNamedContentByRequestLocales(db, name, req, isBI);
        if (content != null) {
            return content;
        }
        else {
            return db.getContentByName(name, db.getDefaultLanguage());
        }
    }

    public WGContent requestSelectContentForPage(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {
        WGContent content = LanguageBehaviourTools.chooseContentByRequestLocales(page, req, isBI);
        if (content != null) {
            return content;
        }
        else {
            return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
        }
    }

    public WGLanguage requestSelectDatabaseLanguage(WGDatabase db, HttpServletRequest req) throws WGAPIException {
        WGLanguage lang =  LanguageBehaviourTools.chooseLanguageByRequestLocales(db, req);
        if (lang != null) {
            return lang;
        }
        else {
            return db.getLanguage(db.getDefaultLanguage());
        }
    }

    public List<WGLanguage> webtmlQueryLanguages(WGDatabase db, TMLContext context) throws WGAPIException {
        Set<WGLanguage> langs = new LinkedHashSet<WGLanguage>();
        
        if (!LanguageBehaviourTools.isMultiLanguageContext(context)) {
            if (context.iswebenvironment()) {
                langs.add(requestSelectDatabaseLanguage(db, (HttpServletRequest) context.getrequest()));
            }
            else {
                langs.add(db.getLanguage(db.getDefaultLanguage()));
            }
        }
        else {
            String currentLangName = context.content().getLanguage().getName();
            WGLanguage lang = db.getLanguage(currentLangName);
            if (lang != null && !lang.isDummy()) {
                langs.add(lang);
            }
        }
        
        return new ArrayList(langs);
    }

    public WGContent webtmlSelectContentForPage(WGStructEntry page, TMLContext context, boolean isBI) throws WGAPIException {
        
        // If neither current nor main context are multilanguage then we might just choose a content from the request locales
        if (!LanguageBehaviourTools.isMultiLanguageContext(context)) {
            if (context.iswebenvironment()) {
                return requestSelectContentForPage(page, (HttpServletRequest) context.getrequest(), isBI);
            }
            else {
                return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
            }
        }
        
        String sourceLangName = context.content().getLanguage().getName();
        WGContent content = LanguageBehaviourTools.getRelevantContent(page, sourceLangName, isBI);
        if (content != null) {
            return content;
        }
        
        // If the source context is a dummy content we may ignore its language and choose from request locales (#00000543)
        if (context.content().isDummy() && context.iswebenvironment()) {
            content = requestSelectContentForPage(page, (HttpServletRequest) context.getrequest(), isBI);
            if (content != null) {
                return content;
            }
        }
        
        return null;
    }

    public String webtmlFetchLabel(WGAResourceBundleManager manager, TMLContext context, String container, String file, String key) throws WGAPIException {
        
        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            String currentLangName = context.content().getLanguage().getName();
            Locale mainLangLocale = WGLanguage.languageNameToLocale(currentLangName);
            try {
                String label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, mainLangLocale);
                if (label != null) {
                    return label;
                }
            }
            catch (IOException e) {
                context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + mainLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
            }
        }
        
        // If context is non-multilang the we see if we can take a language decision from the main context
        else if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            String currentLangName = context.getmaincontext().content().getLanguage().getName();
            Locale mainLangLocale = WGLanguage.languageNameToLocale(currentLangName);
            try {
                String label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, mainLangLocale);
                if (label != null) {
                    return label;
                }
            }
            catch (IOException e) {
                context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + mainLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
            }
        }
        
        // If neither current nor main context are multilanguage then we might just choose a content from the request locales
        else {
            WGLanguage lang = webtmlSelectDatabaseLanguage(manager.getDb(), context);
            if (lang != null) {
                Locale mainLangLocale = WGLanguage.languageNameToLocale(lang.getName());
                try {
                    String label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, mainLangLocale);
                    if (label != null) {
                        return label;
                    }
                }
                catch (IOException e) {
                    context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + mainLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
                }
            }
        }
        
        // Try fallback label language
        try {
            String label = LanguageBehaviourTools.fetchLabelForFallbackLanguage(manager, container, file, key, false);
            if (label != null) {
                return label;
            }
        }
        catch (IOException e) {
            throw new WGBackendException("Exception retrieving label", e);
        }
        
        return null;
        
     }

    public WGLanguage webtmlSelectDatabaseLanguage(WGDatabase db, TMLContext context) throws WGAPIException {
        

        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            String sourceLangName = context.content().getLanguage().getName();
            WGLanguage sourceLang = db.getLanguage(sourceLangName);
            if (sourceLang != null && !sourceLang.isDummy()) {
                return sourceLang;
            }
        }
        
        // If context is non-multilang the we see if we can take a language decision from the main context
        else if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            String sourceLangName = context.getmaincontext().content().getLanguage().getName();
            WGLanguage sourceLang = db.getLanguage(sourceLangName);
            if (sourceLang != null && !sourceLang.isDummy()) {
                return sourceLang;
            }
        }
        
        // If neither current nor main context are multilanguage then we might just choose a content from the request locales
        else {
            if (context.iswebenvironment()) {
                return requestSelectDatabaseLanguage(db, context.getrequest());
            }
            else {
                return db.getLanguage(db.getDefaultLanguage());
            }
        }
        
        return null;
        
    }

    public WGContent webtmlSelectContentForName(WGDatabase db, TMLContext context, String name, boolean isBI) throws WGAPIException {
        
        // If the current context is not multilanguage then we might just choose a language from the request locales
        if (!LanguageBehaviourTools.isMultiLanguageContext(context)) {
            if (context.iswebenvironment()) {
                return requestSelectContentForName(db, (HttpServletRequest) context.getrequest(), name, isBI);
            }
            else {
                return db.getContentByName(name, db.getDefaultLanguage());
            }
        }
        
        String sourceLangName = context.content().getLanguage().getName();
        WGContent content=null;
        
    	// first try: find by PAGENAME
    	WGStructEntry struct = db.getStructEntryByName(name);
    	if(struct!=null){
    		content = LanguageBehaviourTools.getRelevantContent(struct, sourceLangName, isBI);
            if (content != null) {
                return content;
            }
    	}
    	// second try: find by (deprecated) CONTENTNAME 
        content = db.getContentByName(name, sourceLangName);
        if (content != null) {
            return content;
        }
        
        // If the source context is a dummy content we may ignore its language and choose from request locales (#00000543, only on web requests #00001106)
        if (context.iswebenvironment() && context.content().isDummy()) {
            content = requestSelectContentForName(db, (HttpServletRequest) context.getrequest(), name, isBI);
            if (content != null) {
                return content;
            }
        }
        
        return null;
    }

    public String webtmlGetPreferredLanguage(WGDatabase db, TMLContext context) throws WGAPIException {
        
        // If neither current nor main context are multilanguage then we might just choose a content from the request locales
        if (!LanguageBehaviourTools.isMultiLanguageContext(context)) {
            if (context.iswebenvironment()) {
                return requestSelectDatabaseLanguage(db, context.getrequest()).getName();
            }
            else {
                return db.getDefaultLanguage();
            }
        }
        
        return (String) context.getmaincontext().meta("LANGUAGE");
    }

    @Override
    public List<WGContent> webtmlSelectPriorityOrder(WGStructEntry page, TMLContext context, boolean isBI) throws WGAPIException {

        // If neither current nor main context are multilanguage then we might just priorize by request locales
        if (!LanguageBehaviourTools.isMultiLanguageContext(context)) {
            if (context.iswebenvironment()) {
                return requestSelectPriorityOrder(page, (HttpServletRequest) context.getrequest(), isBI);
            }
            else {
                return LanguageBehaviourTools.getRemainingLanguagesPriorityOrder(page, Collections.EMPTY_LIST, isBI);
            }
        }
        
        List<WGContent> contents = new ArrayList<WGContent>();
        String sourceLangName = context.content().getLanguage().getName();
        WGContent content = LanguageBehaviourTools.getRelevantContent(page, sourceLangName, isBI);
        if (content != null) {
            contents.add(content);
        }
        
        contents.addAll(LanguageBehaviourTools.getRemainingLanguagesPriorityOrder(page, contents, isBI));
        
        return contents;
        
    }

    @Override
    public List<WGContent> requestSelectPriorityOrder(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {
        return LanguageBehaviourTools.defaultSelectPriorityOrder(page, req, isBI);
    }



}
