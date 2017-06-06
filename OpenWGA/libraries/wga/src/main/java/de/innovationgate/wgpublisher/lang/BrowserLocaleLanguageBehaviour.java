package de.innovationgate.wgpublisher.lang;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class BrowserLocaleLanguageBehaviour implements LanguageBehaviour {

	@Override
	public WGContent requestSelectContentForPage(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {

		WGContent content =  LanguageBehaviourTools.chooseContentByRequestLocales(page, req, isBI);
        if (content != null) {
            return content;
        }
        else {
            return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);
        }
	}

	@Override
	public WGContent requestSelectContentForName(WGDatabase db, HttpServletRequest req, String name, boolean isBI) throws WGAPIException {
        
		WGContent content = LanguageBehaviourTools.chooseNamedContentByRequestLocales(db, name, req, isBI);
        if (content != null) {
            return content;
        }
        else {
            return db.getContentByName(name, db.getDefaultLanguage());
        }
	}

	@Override
	public WGLanguage requestSelectDatabaseLanguage(WGDatabase db, HttpServletRequest req) throws WGAPIException {
		
        WGLanguage lang =  LanguageBehaviourTools.chooseLanguageByRequestLocales(db, req);
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        lang = db.getLanguage(db.getDefaultLanguage());
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        return null;
	}

	@Override
	public WGContent webtmlSelectContentForPage(WGStructEntry page, TMLContext context, boolean isBI) throws WGAPIException {

		WGContent content = null;
		content = LanguageBehaviourTools.chooseContentByRequestLocales(page, context.getrequest(), isBI);
        if (content != null) {
            return content;
        }
        // Database default language
        String defaultLangName = page.getDatabase().getDefaultLanguage();
        content = LanguageBehaviourTools.getRelevantContent(page, defaultLangName, isBI);
        if (content != null) {
            return content;
        }
        return null;
	}

	@Override
	public WGContent webtmlSelectContentForName(WGDatabase db, TMLContext context, String name, boolean isBI) throws WGAPIException {

		WGContent content = null;
        content = LanguageBehaviourTools.chooseNamedContentByRequestLocales(db, name, (HttpServletRequest) context.getrequest(), isBI);
        if (content != null) {
            return content;
        }

        // Database default language
        content = db.getContentByName(name, db.getDefaultLanguage());
        if (content != null) {
            return content;
        }
        return null;
	}

	@Override
	public WGLanguage webtmlSelectDatabaseLanguage(WGDatabase db, TMLContext context) throws WGAPIException {

		WGLanguage lang = null;
		lang = LanguageBehaviourTools.chooseLanguageByRequestLocales(db, context.getrequest());
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        // Database default language
        lang = db.getLanguage(db.getDefaultLanguage());
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        return null;
	}

	@Override
	public String webtmlFetchLabel(WGAResourceBundleManager manager, TMLContext context, String container, String file, String key) throws WGAPIException {

		String label = null;
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
        
        // Database default language
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

        // Label fallback language
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

	@Override
	public List<WGLanguage> webtmlQueryLanguages(WGDatabase db, TMLContext context) throws WGAPIException {

		HashSet<WGLanguage> langs = new LinkedHashSet<WGLanguage>();
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

	@Override
	public String webtmlGetPreferredLanguage(WGDatabase db, TMLContext context) throws WGAPIException {

        WGLanguage lang = requestSelectDatabaseLanguage(db, context.getrequest());
        if (lang != null) {
            return lang.getName();
        }
        else {
            return db.getDefaultLanguage();
        }

	}

}
