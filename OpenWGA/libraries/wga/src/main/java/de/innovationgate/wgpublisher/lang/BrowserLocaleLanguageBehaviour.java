package de.innovationgate.wgpublisher.lang;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGBackendException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.filter.WGAVirtualHostingFilter;
import de.innovationgate.wgpublisher.labels.WGAResourceBundleManager;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class BrowserLocaleLanguageBehaviour implements LanguageBehaviour, InitializableLanguageBehaviour {

	private WGA _wga;

	@Override
	public WGContent requestSelectContentForPage(WGStructEntry page, HttpServletRequest req, boolean isBI) throws WGAPIException {

		WGContent content =  LanguageBehaviourTools.chooseContentByRequestLocales(page, req, isBI);
        if (content != null) {
            return content;
        }

        // v-host fallback language
		for(String lang: getVhostPreferedLanguages(req)){
			content = LanguageBehaviourTools.getRelevantContent(page, lang, isBI);
	        if (content != null) 
	            return content;
		}
		
       	return LanguageBehaviourTools.getRelevantContent(page, page.getDatabase().getDefaultLanguage(), isBI);

	}

	@Override
	public WGContent requestSelectContentForName(WGDatabase db, HttpServletRequest req, String name, boolean isBI) throws WGAPIException {
        
		WGContent content = LanguageBehaviourTools.chooseNamedContentByRequestLocales(db, name, req, isBI);
        if (content != null) 
            return content;
        
        // v-host language
		for(String lang: getVhostPreferedLanguages(req)){
			content = db.getContentByName(name, lang);
	        if (content != null) 
	            return content;
		}
		
        return db.getContentByName(name, db.getDefaultLanguage());
	}

	@Override
	public WGLanguage requestSelectDatabaseLanguage(WGDatabase db, HttpServletRequest req) throws WGAPIException {
		
        WGLanguage lang =  LanguageBehaviourTools.chooseLanguageByRequestLocales(db, req);
        if (lang != null && !lang.isDummy()) {
            return lang;
        }
        
        // v-host language
        for(String langname: getVhostPreferedLanguages(req)){
			lang = db.getLanguage(langname);
	        if (lang != null && !lang.isDummy()) 
	            return lang;        	
        }

		lang = db.getLanguage(db.getDefaultLanguage());
        if (lang != null && !lang.isDummy()) 
            return lang;

        return null;
	}

	@Override
	public WGContent webtmlSelectContentForPage(WGStructEntry page, TMLContext context, boolean isBI) throws WGAPIException {

		return requestSelectContentForPage(page, context.getrequest(), isBI);
		
	}

	@Override
	public WGContent webtmlSelectContentForName(WGDatabase db, TMLContext context, String name, boolean isBI) throws WGAPIException {

		return requestSelectContentForName(db, context.getrequest(), name, isBI);
		
	}

	@Override
	public WGLanguage webtmlSelectDatabaseLanguage(WGDatabase db, TMLContext context) throws WGAPIException {

		return requestSelectDatabaseLanguage(db, context.getrequest());
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
                context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + locale.toString() + " from app " + manager.getDb().getDbReference(), e);
            }
        }
        
        // v-host fallback language
        for(String langname: getVhostPreferedLanguages(context.getrequest())){
			Locale locale = WGLanguage.languageNameToLocale(langname);
	        try {
	            label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, locale);
	            if (label != null) 
	                return label;
	        }
	        catch (IOException e) {
	            context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + locale.toString() + " from app " + manager.getDb().getDbReference(), e);
	        }
		}

        // Database default language
        Locale defLangLocale = WGLanguage.languageNameToLocale(manager.getDb().getDefaultLanguage());
        try {
            label = LanguageBehaviourTools.fetchLabelForLanguage(manager, container, file, key, defLangLocale);
            if (label != null) 
                return label;
        }
        catch (IOException e) {
            context.getlog().error("Exception retrieving label " + container + "/" + file + "/" + key + " for language " + defLangLocale.toString() + " from app " + manager.getDb().getDbReference(), e);
        }

        // Label fallback language
        try {
            label = LanguageBehaviourTools.fetchLabelForFallbackLanguage(manager, container, file, key, true);
            if (label != null) 
                return label;
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

        // v-host fallback language
        for(String langname: getVhostPreferedLanguages(context.getrequest())){
        	WGLanguage lang = db.getLanguage(langname);
        	if(lang!=null && !lang.isDummy())
        		langs.add(lang);
        }
        
        langs.add(db.getLanguage(db.getDefaultLanguage()));
        
        return new ArrayList<WGLanguage>(langs);
		
	}

	@Override
	public String webtmlGetPreferredLanguage(WGDatabase db, TMLContext context) throws WGAPIException {

        WGLanguage lang = requestSelectDatabaseLanguage(db, context.getrequest());
        if (lang != null) 
            return lang.getName();
        
        return null;

	}

	@Override
	public void init(WGA wga, WGDatabase db) throws WGException {
		_wga = wga;		
	}

	private List<String> getVhostPreferedLanguages(HttpServletRequest request){
		WGAConfiguration config = _wga.getCore().getWgaConfiguration();
		VirtualHost vhost = WGAVirtualHostingFilter.findMatchingHost(config, request);
		if(vhost!=null){
			String langs = vhost.getPreferedLanguages();
			if(langs!=null)
				return Arrays.asList(langs.split("\\s*,\\s*")); 
		}
		return Collections.emptyList();
	}

}
