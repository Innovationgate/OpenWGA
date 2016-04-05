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
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class OnlyDefaultLanguageBehaviour implements LanguageBehaviour, PriorizingLanguageBehaviour {

    public WGContent requestSelectContentForPage(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {
        return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
    }

    public WGLanguage requestSelectDatabaseLanguage(WGDatabase db, HttpServletRequest req) throws WGAPIException {
        return db.getLanguage(db.getDefaultLanguage());
    }

    public String webtmlFetchLabel(WGAResourceBundleManager manager, TMLContext context, String container, String file, String key) throws WGAPIException {
        
        try {
            // If we are included and the main context is multilang we should try to take the language choice from there
            TMLContext mainContext = context.getmaincontext();
            if (LanguageBehaviourTools.isMultiLanguageContext(mainContext)) {
                Locale mainLangLocale = WGLanguage.languageNameToLocale((String) mainContext.meta(WGContent.META_LANGUAGE));
                String label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, mainLangLocale);
                if (label != null) {
                    return label;
                }
            }
            
            // Choice by request locales
            String label = LanguageBehaviourTools.fetchLabelByRequestLocales(manager, container, file, key, context);
            if (label != null) {
                return label;
            }
        
            // Choice by fallback
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

    public List<WGLanguage> webtmlQueryLanguages(WGDatabase db, TMLContext context) throws WGAPIException {
        List<WGLanguage> langs = new ArrayList<WGLanguage>();
        WGLanguage lang = db.getLanguage(db.getDefaultLanguage());
        if (lang != null) {
            langs.add(lang);
        }
        return langs;
    }

    public WGContent webtmlSelectContentForPage(WGStructEntry page, TMLContext sourceContext, boolean isBI) throws WGAPIException {
        return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
    }

    public WGLanguage webtmlSelectDatabaseLanguage(WGDatabase db, TMLContext sourceContext) throws WGAPIException {
        return db.getLanguage(db.getDefaultLanguage());
    }

    public WGContent requestSelectContentForName(WGDatabase db, HttpServletRequest req, String name, boolean isBI) throws WGAPIException {
        return db.getContentByName(name, db.getDefaultLanguage());
    }

    public WGContent webtmlSelectContentForName(WGDatabase db, TMLContext context, String name, boolean isBI) throws WGAPIException {
        return db.getContentByName(name, db.getDefaultLanguage());
    }

    public String webtmlGetPreferredLanguage(WGDatabase db, TMLContext context) throws WGAPIException {
        return db.getDefaultLanguage();
    }

    @Override
    public List<WGContent> webtmlSelectPriorityOrder(WGStructEntry entry, TMLContext context, boolean isBI) throws WGAPIException {
        return defaultLanguagePriorityOrder(entry, isBI);
    }

    @Override
    public List<WGContent> requestSelectPriorityOrder(WGStructEntry entry, HttpServletRequest req, boolean isBI) throws WGAPIException {
        return defaultLanguagePriorityOrder(entry, isBI);
    }

    private List<WGContent> defaultLanguagePriorityOrder(WGStructEntry entry, boolean isBI) throws WGAPIException {
        return LanguageBehaviourTools.getRemainingLanguagesPriorityOrder(entry, Collections.EMPTY_LIST, isBI);
    }



}
