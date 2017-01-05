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
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.MissingResourceException;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.labels.WGAResourceBundle;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class LanguageBehaviourTools {
    
    public static final String LABEL_DEFAULT_FALLBACK_LANGUAGE = "en";    
    
    static class ContentComparatorByStatus implements Comparator {

        public int compare(Object o1, Object o2) {
            
            try {
                if (!(o1 instanceof WGContent) || !(o2 instanceof WGContent)) {
                    throw new IllegalArgumentException();
                }

                WGContent content1 = (WGContent) o1;
                WGContent content2 = (WGContent) o2;

                Integer statValue1 = getStatusValue(content1.getStatus());
                Integer statValue2 = getStatusValue(content2.getStatus());
                
                return statValue1.compareTo(statValue2);
            }
            catch (WGAPIException e) {
                throw new IllegalArgumentException("Cannot retrieve content status for comparing");
            } 
                
        }
        
        public Integer getStatusValue(String status) {
            
            if (status.equals(WGContent.STATUS_DRAFT)) {
                return new Integer(10);
            }
            else if (status.equals(WGContent.STATUS_REVIEW)) {
                return new Integer(50);
            }
            else if (status.equals(WGContent.STATUS_RELEASE)) {
                return new Integer(100);
            }
            else {
                return new Integer(900);
            }
            
        }

        public boolean equals(Object o1) {
            return false;
        }
    }
    
    public static WGContent chooseContentByRequestLocales(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {
        
        WGDatabase db = page.getDatabase();
        Enumeration<Locale> locales = req.getLocales();
        while (locales.hasMoreElements()) {
            Locale locale = locales.nextElement();
            WGLanguage lang = db.getLanguageForLocale(locale);
            if (lang != null && !lang.isDummy()) {
                WGContent content = getRelevantContent(page, lang.getName(), isBI);
                if (content != null) {
                    return content;
                }
            }
        }
        
        return null;
        
    }
    
    public static WGLanguage chooseLanguageByRequestLocales(WGDatabase db, HttpServletRequest req) throws WGAPIException {
        
        Enumeration<Locale> locales = req.getLocales();
        while (locales.hasMoreElements()) {
            Locale locale = locales.nextElement();
            WGLanguage lang = db.getLanguageForLocale(locale);
            if (lang != null && !lang.isDummy()) {
                return lang;
            }
        }        

        return null;
    }
    
    public static String fetchLabelForLanguage(WGAResourceBundleManager manager, String container, String file, String key, Locale locale) throws WGAPIException, IOException {
        String label = null;
        WGAResourceBundle bundle = manager.getBundle(container, file, locale);
        if (bundle != null) {
            try {
                label = bundle.getString(key);
            }
            catch (MissingResourceException e) {
            }
        }
        
        return label;
    }
    
    public static String fetchLabelForFallbackLanguage(WGAResourceBundleManager manager, String container, String file, String key, boolean allowDefaultFallback) throws WGAPIException, IOException {
        
        String defaultLang = (String) manager.getDb().getAttribute(WGACore.DBATTRIB_FALLBACK_LABEL_LANGUAGE);
        if (defaultLang == null && allowDefaultFallback) {
            defaultLang = LABEL_DEFAULT_FALLBACK_LANGUAGE;
        }
        
        if (defaultLang != null) {
            Locale defaultLangLocale = WGLanguage.languageNameToLocale(defaultLang);
            return LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, defaultLangLocale);
        }
        
        return null;
    }
    
    public static String fetchLabelByRequestLocales(WGAResourceBundleManager manager, String container, String file, String key, TMLContext context) throws WGAPIException {
        
        List<Locale> locales;
        if (context.iswebenvironment()) {
            locales = Collections.list(context.getrequest().getLocales());
        }
        else if (context.getEnvironment().getRootEnvironmentUserData().getLocales() != null) {
            locales = context.getEnvironment().getRootEnvironmentUserData().getLocales();
        }
        else {
            return null;
        }
            
        
        for (Locale locale : locales) {
            try {
                String label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, locale);
                if (label != null) {
                    return label;
                }
            }
            catch (IOException e) {
                context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + locale.toString() + " from DB " + manager.getDb().getDbReference(), e);
            }
        }
        
        return null;
    }
    
    public static WGContent getRelevantContent(WGStructEntry page, String language, boolean isBI) throws WGAPIException {
        
        // A user below ACL level author cannot use BI mode
        if (isBI == true && page.getDatabase().getSessionContext().getAccessLevel() < WGDatabase.ACCESSLEVEL_AUTHOR) {
            isBI = false;
        }
        
        // Non-BI mode: Just return released content
        if (!isBI || !page.mayEditPage()) {
            return page.getReleasedContent(language);
        }
        
        
        // BI mode: Prefer drafts over released docs
        Iterator contents = page.getAllContent().iterator();
        ArrayList allContent = new ArrayList();
        WGContent content = null;

        // Filter documents of correct workflow status and language and put them to list
        while (contents.hasNext()) {
            content = (WGContent) contents.next();
            if (!language.equalsIgnoreCase(content.getLanguage().getName())) {
                continue;
            }
            
            allContent.add(content);
        }
        
        if (allContent.isEmpty()) {
            return null;
        }

        // Sort content list the workflow status preference
        java.util.Collections.sort(allContent, new ContentComparatorByStatus());
        return (WGContent) allContent.get(0);
        
    }

    public static WGContent chooseNamedContentByRequestLocales(WGDatabase db, String name, HttpServletRequest req, boolean isBI) throws WGAPIException {
        Enumeration<Locale> locales = req.getLocales();
        while (locales.hasMoreElements()) {
            Locale locale = locales.nextElement();
            WGLanguage lang = db.getLanguageForLocale(locale);
            if (lang != null && !lang.isDummy()) {
                WGContent content = db.getContentByName(name, lang.getName());
                if (content != null) {
                    return content;
                }
            }
        }
        
        return null;
    }
    
    public static LanguageBehaviour retrieve(WGDatabase db) {
        return (LanguageBehaviour) db.getAttribute(WGACore.DBATTRIB_LANGUAGEBEHAVIOUR_INSTANCE);
    }
    
    public static LanguageBehaviour retrieve(WGDatabase contentDB, TMLContext con) {
        
        WGDatabase designDB = con.getDesignContext().getDesignDB();
        if (LanguageBehaviourTools.isMultiLanguageDB(designDB) && LanguageBehaviourTools.isMultiLanguageDB(contentDB)) {
            return retrieve(designDB);
        }
        else {
            return retrieve(contentDB);
        }
        
    }
    
    public static WGLanguage getDBLocalLanguage(WGDatabase db, WGLanguage language) throws WGAPIException {
        if (!language.getDatabase().equals(db)) {
            language = db.getLanguage(language.getName());
            if (language != null && !language.isDummy()) {
                return language;
            }
            else {
                return null;
            }
        }
        else {
            return language;
        }
    }
    
    public static boolean isMultiLanguageContext(TMLContext context) {
        return (isMultiLanguageDB(context.db()) && context.content() != null);
    }
    
    public static boolean isMultiLanguageDB(WGDatabase db) {
        return db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES) && db.getBooleanAttribute(WGACore.DBATTRIB_MULTILANGUAGE_CONTENT, true);
    }

    public static List<WGContent> defaultSelectPriorityOrder(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {

        List<WGContent> contents = new ArrayList();
        
        
        if (req != null && isMultiLanguageDB(page.getDatabase())) {
            Enumeration<Locale> locales = req.getLocales();
            while (locales.hasMoreElements()) {
                Locale locale = locales.nextElement();
                WGLanguage lang = page.getDatabase().getLanguageForLocale(locale);
                if (lang != null && !lang.isDummy()) {
                    WGContent content = getRelevantContent(page, lang.getName(), isBI);
                    if (content != null) {
                        contents.add(content);
                    }
                }
            }
        }
        
        contents.addAll(getRemainingLanguagesPriorityOrder(page, contents, isBI));
        return contents;
        
    }

    public static List<WGContent> getRemainingLanguagesPriorityOrder(WGStructEntry page, List<WGContent> alreadyChosen, boolean isBI) throws WGAPIException {

        // List of existing languages
        List<String> alreadyChosenLanguages = new ArrayList<String>();
        for (WGContent alreadyChosenContent : alreadyChosen) {
            alreadyChosenLanguages.add(alreadyChosenContent.getLanguage().getName());
        }
        
        List<WGContent> contents = new ArrayList<WGContent>();
        
        // Add default language. Has higher priority than other yet unchosen languages
        if (!alreadyChosenLanguages.contains(page.getDatabase().getDefaultLanguage())) {
            WGContent defaultLangContent = getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
            if (defaultLangContent != null) {
                contents.add(defaultLangContent);
            }
            alreadyChosenLanguages.add(page.getDatabase().getDefaultLanguage());
        }
        
        // Go through remaining languages
        for (WGLanguage lang : page.getDatabase().getLanguages().values()) {
            
            if (!alreadyChosenLanguages.contains(lang.getName())) {
                WGContent otherContent = getRelevantContent(page, lang.getName(), isBI);
                if (otherContent != null) {
                    contents.add(otherContent);
                }        
            }
            
        }
        return contents;
        
    }


}
