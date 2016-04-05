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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.modules.RegistryAwareModule;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class DynamicLanguageBehaviour implements LanguageBehaviour, PriorizingLanguageBehaviour, InitializableLanguageBehaviour {
    
    public static final String PUBOPTION_CONFIG_FILE = "DynamicLanguageBehaviour.ConfigFile";
    
    public static final class RequestPattern {
        
        public RequestPattern(Pattern pattern, String language) {
            super();
            _pattern = pattern;
            _language = language;
        }
        private Pattern _pattern;
        private String _language;
        
        public Pattern getPattern() {
            return _pattern;
        }
        
        public String getLanguage() {
            return _language;
        }
        
        public boolean verify(HttpServletRequest req) {
            return _pattern.matcher(req.getServerName()).matches();
        }
        
    }
    
    private File _configFile = null;
    private long _configFileLastModified = Long.MIN_VALUE;
    private List<RequestPattern> _requestPatterns = new ArrayList<RequestPattern>();

    public WGContent requestSelectContentForName(WGDatabase db, HttpServletRequest req, String name, boolean isBI) throws WGAPIException {
        
        String prefLang = getRequestPreferredLanguage(req);
        if (prefLang != null) {
            WGContent content = db.getContentByName(name, prefLang);
            if (content != null) {
                return content;
            }
        }
        
        WGContent content = LanguageBehaviourTools.chooseNamedContentByRequestLocales(db, name, req, isBI);
        if (content != null) {
            return content;
        }
        else {
            return db.getContentByName(name, db.getDefaultLanguage());
        }
    }

    public WGContent requestSelectContentForPage(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {
        
        String prefLang = getRequestPreferredLanguage(req);
        if (prefLang != null) {
            WGContent content = LanguageBehaviourTools.getRelevantContent(page, prefLang, isBI);
            if (content != null) {
                return content;
            }
        }
        
        WGContent content =  LanguageBehaviourTools.chooseContentByRequestLocales(page, req, isBI);
        if (content != null) {
            return content;
        }
        else {
            return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
        }
    }

    public WGLanguage requestSelectDatabaseLanguage(WGDatabase db, HttpServletRequest req) throws WGAPIException {
        
        String prefLang = getRequestPreferredLanguage(req);
        if (prefLang != null) {
            WGLanguage lang = db.getLanguage(prefLang);
            if (lang != null && !lang.isDummy()) {
                return lang;
            }
        }
        
        WGLanguage lang =  LanguageBehaviourTools.chooseLanguageByRequestLocales(db, req);
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        
        return null;
    }

    public String webtmlFetchLabel(WGAResourceBundleManager manager, TMLContext context, String container, String file, String key) throws WGAPIException {
        
        String label = null;
        
        // First try: Default language determined by request data
        if (context.iswebenvironment()) {
            String prefLang = getRequestPreferredLanguage(context.getrequest());
            if (prefLang != null) {
                Locale currentLangLocale = WGLanguage.languageNameToLocale(prefLang);
                try {
                    
                    label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, currentLangLocale);
                    if (label != null) {
                        return label;
                    }
                }
                catch (IOException e) {
                    context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + currentLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
                }   
            }
        }

        // Second try: Current context locale
        String currentLangName = null;
        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            currentLangName = context.content().getLanguage().getName();
            Locale currentLangLocale = WGLanguage.languageNameToLocale(currentLangName);
            try {
                
                label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, currentLangLocale);
                if (label != null) {
                    return label;
                }
            }
            catch (IOException e) {
                context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + currentLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
            }   
        }
        
        // Third try: Main context locale
        if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            String mainLangName = context.getmaincontext().content().getLanguage().getName();
            if (!mainLangName.equals(currentLangName)) {
                Locale mainLangLocale = WGLanguage.languageNameToLocale(mainLangName);
                try {
                    
                    label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, mainLangLocale);
                    if (label != null) {
                        return label;
                    }
                }
                catch (IOException e) {
                    context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + mainLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
                }
            }
        }
        
        // Fourth try: Request locales
        if (context.iswebenvironment()) {
            Enumeration<Locale> locales = context.getrequest().getLocales();
            while (locales.hasMoreElements()) {
                Locale locale = locales.nextElement();
                try {
                    
                    label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, locale);
                    if (label != null) {
                        return label;
                    }
                }
                catch (IOException e) {
                    context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + locale.toString() + " from DB " + manager.getDb().getDbReference(), e);
                }
            }
        }

        // Fifth try: Database default language
        Locale defLangLocale = WGLanguage.languageNameToLocale(manager.getDb().getDefaultLanguage());
        try {
            
            label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, defLangLocale);
            if (label != null) {
                return label;
            }
        }
        catch (IOException e) {
            context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + defLangLocale.toString() + " from DB " + manager.getDb().getDbReference(), e);
        }
        
        // Sixth try: Label fallback language
        try {
            label = LanguageBehaviourTools.fetchLabelForFallbackLanguage(manager, container, file, key, true);
            if (label != null) {
                return label;
            }
        }
        catch (IOException e) {
            throw new WGBackendException("Exception retrieving label", e);
        }
        
        
        return null;
        
        
        
    }

    public String webtmlGetPreferredLanguage(WGDatabase db, TMLContext context) throws WGAPIException {
        
        // First try: Default language determined by request data
        if (context.iswebenvironment()) {
            String prefLang = getRequestPreferredLanguage(context.getrequest());
            if (prefLang != null) {
                return prefLang;  
            }
        }
        
        // If neither current nor main context are multilanguage then we might just choose a content from the request locales
        if (!LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            if (context.iswebenvironment()) {
                WGLanguage lang = requestSelectDatabaseLanguage(db, context.getrequest());
                if (lang != null) {
                    return lang.getName();
                }
                else {
                    return db.getDefaultLanguage();
                }
            }
            else {
                return db.getDefaultLanguage();
            }
        }
        
        return (String) context.getmaincontext().meta("LANGUAGE");
    }

    public List<WGLanguage> webtmlQueryLanguages(WGDatabase db, TMLContext context) throws WGAPIException {

        HashSet<WGLanguage> langs = new LinkedHashSet<WGLanguage>();
        
        if (context.iswebenvironment()) {
            String prefLang = getRequestPreferredLanguage(context.getrequest());
            if (prefLang != null) {
                langs.add(db.getLanguage(prefLang));
            }
        }
        
        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            WGLanguage contextLanguage = LanguageBehaviourTools.getDBLocalLanguage(db, context.content().getLanguage());
            if (contextLanguage != null) {
                langs.add(contextLanguage);
            }
        }
        
        if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            WGLanguage mainLanguage = LanguageBehaviourTools.getDBLocalLanguage(db, context.getmaincontext().content().getLanguage());
            if (mainLanguage != null) {
                langs.add(mainLanguage);
            }
        }
        
        Enumeration<Locale> locales = context.getrequest().getLocales();
        while (locales.hasMoreElements()) {
            Locale locale = locales.nextElement();
            WGLanguage lang = db.getLanguageForLocale(locale);
            if (lang != null && !lang.isDummy()) {
                langs.add(lang);
            }
        }
        
        langs.add(db.getLanguage(db.getDefaultLanguage()));
        
        return new ArrayList<WGLanguage>(langs);
        
    }

    public WGContent webtmlSelectContentForName(WGDatabase db, TMLContext context, String name, boolean isBI) throws WGAPIException {

        WGContent content = null;
        
        // First try: Request default language
        if (context.iswebenvironment()) {
            String prefLang = getRequestPreferredLanguage(context.getrequest());
            if (prefLang != null) {
                content = db.getContentByName(name, prefLang);
                if (content != null) {
                    return content;
                }
            }
        }
        
        // Second try: Current context language
        String currentLangName = null;
        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            currentLangName = context.content().getLanguage().getName();
            content = db.getContentByName(name, currentLangName);
            if (content != null) {
                return content;
            }
        }

        // Third try: Main context language
        if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            String mainLangName = context.getmaincontext().content().getLanguage().getName();
                if (!mainLangName.equals(currentLangName)) {
                content = db.getContentByName(name, mainLangName);
                if (content != null) {
                    return content;
                }
            }
        }
        
        // Fourth try: Request locales
        if (context.iswebenvironment()) {
            content = LanguageBehaviourTools.chooseNamedContentByRequestLocales(db, name, (HttpServletRequest) context.getrequest(), isBI);
            if (content != null) {
                return content;
            }
        }

        // Fifth try: Database default language
        String defaultLangName = db.getDefaultLanguage();
        content = db.getContentByName(name, defaultLangName);
        if (content != null) {
            return content;
        }
        
        return null;
    }

    public WGContent webtmlSelectContentForPage(WGStructEntry page, TMLContext context, boolean isBI) throws WGAPIException {

        WGContent content = null;
        
        // First try: Request default language
        if (context.iswebenvironment()) {
            String prefLang = getRequestPreferredLanguage(context.getrequest());
            if (prefLang != null) {
                content = LanguageBehaviourTools.getRelevantContent(page, prefLang, isBI);
                if (content != null) {
                    return content;
                }
            }
        }
        
        // Second try: Current context language
        String currentLangName = null;
        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            currentLangName = context.content().getLanguage().getName();
            content = LanguageBehaviourTools.getRelevantContent(page, currentLangName, isBI);
            if (content != null) {
                return content;
            }
        }

        // Third try: Main context language
        if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            String mainLangName = context.getmaincontext().content().getLanguage().getName();
            if (!mainLangName.equals(currentLangName)) {
                content = LanguageBehaviourTools.getRelevantContent(page, mainLangName, isBI);
                if (content != null) {
                    return content;
                }
            }
        }
                
        // Fourth try: Request locales
        if (context.iswebenvironment()) {
            content = LanguageBehaviourTools.chooseContentByRequestLocales(page, context.getrequest(), isBI);
            if (content != null) {
                return content;
            }
        }

        // Fifth try: Database default language
        String defaultLangName = page.getDatabase().getDefaultLanguage();
        content = LanguageBehaviourTools.getRelevantContent(page, defaultLangName, isBI);
        if (content != null) {
            return content;
        }
        
        return null;
        
    }

    public WGLanguage webtmlSelectDatabaseLanguage(WGDatabase db, TMLContext context) throws WGAPIException {
        
        WGLanguage lang = null;
        
        // First try: Request default language
        if (context.iswebenvironment()) {
            String prefLang = getRequestPreferredLanguage(context.getrequest());
            if (prefLang != null) {
                lang = db.getLanguage(prefLang);
                if (lang != null && !lang.isDummy()) {
                    return lang;
                }
            }
        }
        
        // Second try: Current context language
        String currentLangName = null;
        if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
            currentLangName = context.content().getLanguage().getName();
            if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
                lang = db.getLanguage(currentLangName);
                if (lang != null && !lang.isDummy()) {
                    return lang;
                }
            }
        }
        
        // Third try: Main context language
        if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
            String mainLangName = context.getmaincontext().content().getLanguage().getName();
            if (!mainLangName.equals(currentLangName)) {
                lang = db.getLanguage(mainLangName);
                if (lang != null && !lang.isDummy()) {
                    return lang;
                }
            }
        }
                
        // Fourth try: Request locales
        if (context.iswebenvironment()) {
            lang = LanguageBehaviourTools.chooseLanguageByRequestLocales(db, context.getrequest());
            if (lang != null && !lang.isDummy()) {
                return lang;
            }
        }

        // Fifth try: Database default language
        String defaultLangName = db.getDefaultLanguage();
        lang = db.getLanguage(defaultLangName);
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        
        return null;
    }
    
    @Override
    public List<WGContent> requestSelectPriorityOrder(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {
        
        List<WGContent> contents = new ArrayList<WGContent>();
        String prefLang = getRequestPreferredLanguage(req);
        if (prefLang != null) {
            WGContent content = LanguageBehaviourTools.getRelevantContent(page, prefLang, isBI);
            if (content != null) {
                contents.add(content);
            }
        }
        
        if (req != null && LanguageBehaviourTools.isMultiLanguageDB(page.getDatabase())) {
            @SuppressWarnings("unchecked")
            Enumeration<Locale> locales = req.getLocales();
            while (locales.hasMoreElements()) {
                Locale locale = locales.nextElement();
                WGLanguage lang = page.getDatabase().getLanguageForLocale(locale);
                if (lang != null && !lang.isDummy() && !lang.getName().equals(prefLang)) {
                    WGContent content = LanguageBehaviourTools.getRelevantContent(page, lang.getName(), isBI);
                    if (content != null) {
                        contents.add(content);
                    }
                }
            }
        }
        
        contents.addAll(LanguageBehaviourTools.getRemainingLanguagesPriorityOrder(page, contents, isBI));
        return contents;
        
    }

    @Override
    public List<WGContent> webtmlSelectPriorityOrder(WGStructEntry page, TMLContext context, boolean isBI) throws WGAPIException {
        
            List<WGContent> contents = new ArrayList();
            
            // Request default language
            if (context.iswebenvironment()) {
                String prefLang = getRequestPreferredLanguage(context.getrequest());
                if (prefLang != null) {
                    WGContent content = LanguageBehaviourTools.getRelevantContent(page, prefLang, isBI);
                    if (content != null) {
                        contents.add(content);
                    }
                }
            }
            
            // Current context language
            String currentLangName = null;
            if (LanguageBehaviourTools.isMultiLanguageContext(context)) {
                currentLangName = context.content().getLanguage().getName();
                WGContent content = LanguageBehaviourTools.getRelevantContent(page, currentLangName, isBI);
                if (content != null) {
                    contents.add(content);
                }
            }

            // Main context language
            if (LanguageBehaviourTools.isMultiLanguageContext(context.getmaincontext())) {
                String mainLangName = context.getmaincontext().content().getLanguage().getName();
                if (!mainLangName.equals(currentLangName)) {
                    WGContent content = LanguageBehaviourTools.getRelevantContent(page, mainLangName, isBI);
                    if (content != null) {
                        contents.add(content);
                    }
                }
            }
                    
            // Request locales
            if (context.iswebenvironment()) {
                Enumeration<Locale> locales = context.getrequest().getLocales();
                while (locales.hasMoreElements()) {
                    Locale locale = locales.nextElement();
                    WGLanguage lang = page.getDatabase().getLanguageForLocale(locale);
                    if (lang != null && !lang.isDummy()) {
                        WGContent content = LanguageBehaviourTools.getRelevantContent(page, lang.getName(), isBI);
                        if (content != null && !contents.contains(content)) {
                            contents.add(content);
                        }
                    }
                }
            }

            // Database default language and remainders
            contents.addAll(LanguageBehaviourTools.getRemainingLanguagesPriorityOrder(page, contents, isBI));
            
            return contents;
        
    }

    @Override
    public void init(WGA wga, WGDatabase db) throws WGException {
        
        String configFilePath = (String) db.getAttribute(PUBOPTION_CONFIG_FILE);
        if (configFilePath != null) {
            _configFile = wga.server().resolveSystemFile(configFilePath);
            updateConfig();
        }
        
    }

    private void updateConfig() {

        if (_configFile == null || _configFile.lastModified() == _configFileLastModified) {
            return;
        }
            
        try {
            long newLastModified = _configFile.lastModified();
            Properties props = new Properties();
            InputStream in = new FileInputStream(_configFile);
            props.load(in);
            in.close();
            
            List<RequestPattern> newPatterns = new ArrayList<DynamicLanguageBehaviour.RequestPattern>(); 
            
            List<String> langs = WGUtils.deserializeCollection(props.getProperty("languages"), ",");
            for (String lang : langs) {
                
                String code = props.getProperty(lang + ".code");
                if (code == null) {
                    code = lang;
                }
                
                int patternIdx = 1;
                while (true) {
                    String pattern = props.getProperty(lang + ".host." + patternIdx);
                    if (pattern == null) {
                        break;
                    }
                    
                    if (!pattern.startsWith("^") && !pattern.endsWith("$")) {
                        pattern = "\\Q" + pattern + "\\E";
                    }
                    
                    newPatterns.add(new RequestPattern(Pattern.compile(pattern), code));
                    patternIdx++;
                }
                
                synchronized(this) {
                    _requestPatterns = newPatterns;
                    _configFileLastModified = newLastModified;
                }
            }
        }
        catch (Exception e) {
            Logger.getLogger("wga.languagebehaviour").error("Exception loading dynamic language behaviour config from file " + _configFile.getAbsolutePath(), e);
        }
        
        
    }
    
    protected String getRequestPreferredLanguage(HttpServletRequest req) {
        
        updateConfig();
        for (RequestPattern p : _requestPatterns) {
            if (p.verify(req)) {
                return p.getLanguage();
            }
        }
        
        return null;
        
    }

}
