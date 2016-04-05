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

package de.innovationgate.wgpublisher;

import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.apache.log4j.Logger;

import de.innovationgate.utils.CookieValueEncoder;
import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.TransientObjectWrapper;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGTransientPortletRegistry;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wgpublisher.WGACore.UserAgentVerifier;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateSessionStorage;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateStorage;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class PersonalisationManager {
    
    public static final Logger LOG = Logger.getLogger("wga.pers");

    public static final String SESSION_PROFILENAME = "Profilename:";
    public static final String SESSION_PROFILENAME_INDIVIDUALDB = "ProfilenameDB:";
    private WGACore _core;
    public static final String COOKIE_SECURE_WGPID = "SecureWGPID";
    public static final String COOKIE_WGPID = "WGPID";
    
    public PersonalisationManager(WGACore core) {
        _core = core;
    }

    public TMLUserProfile fetchUserProfile(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, WGDatabase database)
            throws WGAPIException {
    
        HttpSession session = request.getSession(); 
        
        // Check request cache
        TMLUserProfile tmlProfile = getProfileFromRequestCache(request, database);
        if (tmlProfile != null && !tmlProfile.getprofile().isDeleted()) {
            return tmlProfile;
        }
        
        // Fetch some basic information
        WGDatabase persDB = fetchPersonalisationDatabase(database);
        if (persDB == null) { // Possible when a data source is in context
            return null;
        }
        Integer persMode = getPersonalisationMode(database);
        boolean secureMode = database.getBooleanAttribute(WGACore.DBATTRIB_SECURE_APP, false);
        String cookieName = determineWgpidCookieName(secureMode);

    
        // Check session cache. If found and valid, write the profile to request cache, eventually refresh client information of profile
        WGUserProfile userProfile = getProfileFromSessionCache(session, database, persDB);
       if (userProfile != null && !userProfile.isDeleted() && userProfile.getDatabase().isReady() && userProfile.getDatabase().equals(persDB)) {
            tmlProfile = new TMLUserProfile(userProfile, _core, persMode, session, database.getComplianceVersion());
            writeProfileToRequestCache(tmlProfile, database, request);
            
            // Look if we must now send the WGPID cookie, in case it was not already sent
            if (persMode == Constants.PERSMODE_AUTO || persMode == Constants.PERSMODE_LOGIN) {
                String wgpid = findCookie(request.getCookies(), cookieName);
                if (wgpid == null) {
                    maybeSendWgpidCookie(response, userProfile, secureMode);
                }
            }
            
            // Maybe mark for autosave on this request
            markProfileForAutoSave(userProfile, persMode, session);
            
            return tmlProfile;
        }
    
        // If in Mode AUTO, the User Profile is retrieved automatically
        // (otherwise it is set in the login process)
        boolean created = false;
        if (persMode == Constants.PERSMODE_AUTO) {
            String wgpid = this.findCookie(request.getCookies(), cookieName);
            if (wgpid != null) {
                userProfile = persDB.getUserProfile(wgpid);
            }
    
            if (userProfile == null) {
                userProfile = createUserProfile((String) request.getHeader("User-Agent"), persDB, wgpid, Constants.PERSMODE_AUTO, request);
                created = true;
            }
            maybeSendWgpidCookie(response, userProfile, secureMode);
        }
        
        else if (persMode == Constants.PERSMODE_SESSION) {
            userProfile = createUserProfile((String) request.getHeader("User-Agent"), persDB, "sessionprofile", Constants.PERSMODE_SESSION, request);
            created = true;
        }
    
        else if (persMode == Constants.PERSMODE_LOGIN) {
            if (database.getSessionContext().isAnonymous()) {
                // no profile for anonymous
                writeNoProfileInformationToRequestCache(database, request);
                return null;
            }
            String userName = database.getSessionContext().getUser();
            userProfile = persDB.getUserProfile(userName);
            if (userProfile == null) {
                userProfile = createUserProfile((String) request.getHeader("User-Agent"), persDB, userName, Constants.PERSMODE_LOGIN, request);
                created =  true;
            }
        }
    
        // If valid user profile, write to caches, mark for autosave, register session
        if (userProfile != null && !userProfile.isDeleted()) {
                // Drop cache so we can be sure to be up-to-date with the backend
            // database
            // (Since backend changes - maybe from other cluster nodes - are not
            // registered for user profiles)
            userProfile.dropCache();
            tmlProfile = new TMLUserProfile(userProfile, _core, persMode, session, database.getComplianceVersion());
            writeProfileToSessionCache(userProfile, database, session);
            writeProfileToRequestCache(tmlProfile, database, request);
            markProfileForAutoSave(userProfile, persMode, session);
            this.registerSession(userProfile, request, session, created);
            return tmlProfile;
        }
        else {
            session.removeAttribute(SESSION_PROFILENAME_INDIVIDUALDB + database.getDbReference());
            return null;
        }
    
    }

    public WGUserProfile getProfileFromSessionCache(HttpSession session, WGDatabase targetDB, WGDatabase persDB) throws WGAPIException {
        WGUserProfile userProfile = null;
        @SuppressWarnings("unchecked")
        TransientObjectWrapper<WGUserProfile> profileWrapper = (TransientObjectWrapper<WGUserProfile>) session.getAttribute(SESSION_PROFILENAME_INDIVIDUALDB + targetDB.getDbReference());
        if (profileWrapper != null) {
            userProfile = profileWrapper.get();
        }
        
        // If the database got reconnected while this session we need to re-fetch the doc from the current instance
        if (userProfile != null && !userProfile.getDatabase().isReady()) {
            userProfile = (WGUserProfile) persDB.getDocumentByKey(userProfile.getDocumentKey());
            profileWrapper.set(userProfile);
        }
        
        return userProfile;
    }

    protected void writeProfileToSessionCache(WGUserProfile userProfile, WGDatabase database, HttpSession session) {
        boolean serializableProfile = database.getBooleanAttribute(WGACore.DBATTRIB_SERIALIZABLE_USERPROFILE, false);
        TransientObjectWrapper<WGUserProfile> profileWrapper = new TransientObjectWrapper<WGUserProfile>(!serializableProfile);
        profileWrapper.set(userProfile);
        session.setAttribute(SESSION_PROFILENAME_INDIVIDUALDB + database.getDbReference(), profileWrapper);
    }

    private void writeNoProfileInformationToRequestCache(WGDatabase database, HttpServletRequest request) {
        Map<String,Object> fetchedProfiles = getFetchedUserProfiles(request);
        fetchedProfiles.put(database.getDbReference(), new NullPlaceHolder());
    }

    private void writeProfileToRequestCache(TMLUserProfile tmlProfile, WGDatabase database, HttpServletRequest request) {
        Map<String,Object> fetchedProfiles = getFetchedUserProfiles(request);
        fetchedProfiles.put(database.getDbReference(), tmlProfile);
    }

    public TMLUserProfile getProfileFromRequestCache(javax.servlet.http.HttpServletRequest request, WGDatabase database) throws WGAPIException {
        Map<String,Object> fetchedProfiles = getFetchedUserProfiles(request);
        if (fetchedProfiles.containsKey(database.getDbReference())) {
            return retrieveCachedUserProfile(fetchedProfiles, database);
        }
        else {
            return null;
        }
    }

    public WGDatabase fetchPersonalisationDatabase(WGDatabase contentDatabase) throws WGAPIException {
        String domain = (String) contentDatabase.getAttribute(WGACore.DBATTRIB_DOMAIN);
        if (domain == null) {
            return null;
        }
    
        // Try to get pers db from domain
        WGDatabase persDB = null;
        try {
            persDB = _core.openPersonalisationDB(domain);
        }
        catch (WGAPIException e1) {
            _core.getLog().error("Error opening personalisation db for domain '" + domain + "'", e1);
            throw e1;
        }
    
        // Try to use content database as pers db or use an external self
        // personalisation db
        if (persDB == null) {
            if (contentDatabase.hasFeature(WGDatabase.FEATURE_SELF_PERSONALIZABLE)) {
                persDB = contentDatabase;
            }
            else {
                persDB = (WGDatabase) contentDatabase.getAttribute(WGACore.DBATTRIB_EXTERNAL_SELF_PERSONALISATION_DB);
                if (persDB != null) {
                    if (!persDB.isSessionOpen()) {
                        persDB.openSession();
                    }
                }
                else {
                    return null;
                }
            }
        }
        return persDB;
    }

    public Integer getPersonalisationMode(WGDatabase database) {
        // Fetch user profile using database's persmode. If not available we
        // take the default value for the option
        String persModeStr = (String) _core.readPublisherOptionOrDefault(database, WGACore.DBATTRIB_PERSMODE);
        Integer persMode;
        if (persModeStr != null) {
            persMode = Integer.valueOf(persModeStr);
        }
        // Backup for sure, which should not happen as the option is defined for
        // all databases
        else {
            persMode = Constants.PERSMODE_AUTO;
        }
        return persMode;
    }

    private String determineWgpidCookieName(boolean secureMode) {
        String cookieName = COOKIE_WGPID;
        if (secureMode) {
            cookieName = COOKIE_SECURE_WGPID;
        }
        return cookieName;
    }

    private String findCookie(Cookie[] cookies, String cookieName) {
    
        if (cookies == null) {
            return null;
        }
    
        Cookie cookie;
        for (int idx = 0; idx < cookies.length; idx++) {
            cookie = cookies[idx];
            if (cookie.getName().equalsIgnoreCase(cookieName)) {
                return CookieValueEncoder.decode(cookie.getValue());
            }
        }
        return null;
    
    }

    public WGUserProfile createUserProfile(String userAgent, WGDatabase persDB, String wgpid, int persMode, HttpServletRequest request) throws WGAPIException {
    
        WGUserProfile userProfile;
        UserAgentVerifier userAgentVerifier = _core.getUserAgentVerifier();
        boolean forceNew = (persMode == Constants.PERSMODE_SESSION);
        
        if (userAgentVerifier.isValidUserAgent(userAgent) == true) {
            
            // If wgpid given sync the creation, so collisions are avoided (#00001334)
            if (wgpid != null) {
                String syncKey = "WGPDispatcher.createUserProfile#" + persDB.getDbReference() + "#" + wgpid;
                synchronized (syncKey.intern()) {
                    userProfile = persDB.createUserProfile(wgpid, persMode, forceNew);
                    if (userProfile != null && !userProfile.isDummy()) {
                        TMLUserProfile.prepareNewProfile(userProfile, request);
                        //userProfile.save(); // Conflicts with #00001412. Deactivating this may break #00001334 again. 
                    }
                }
            }
            
            else {
                userProfile = persDB.createUserProfile(null, persMode ,forceNew);
                if (userProfile != null && !userProfile.isDummy()) {
                    TMLUserProfile.prepareNewProfile(userProfile, request);
                    //userProfile.save();  // Conflicts with #00001412. Deactivating this may break #00001334 again.
                }
            }
            
        }
        else {
            userProfile = persDB.getDummyProfile(wgpid);
        }
    
        return userProfile;
    }

    private void maybeSendWgpidCookie(javax.servlet.http.HttpServletResponse response, WGUserProfile userProfile, boolean secureMode) throws WGAPIException {
        if (userProfile != null && !userProfile.isDummy() && userProfile.isSaved()) {
            String cookieName = determineWgpidCookieName(secureMode);
            WGCookie wgpidCookie = new WGCookie(cookieName, CookieValueEncoder.encode(userProfile.getName()));
            wgpidCookie.setPath("/");
            wgpidCookie.setSecure(secureMode);
            wgpidCookie.setMaxAge(60 * 60 * 24 * 365);
            wgpidCookie.addCookieHeader(response);
        }
    }

    /**
     * Method registerHit.
     * 
     * @param userProfile
     * @param request
     * @param path
     * @param database
     * @param content
     */
    public void registerHit(WGUserProfile userProfile, WGDatabase database, WGContent content) {
        try {
            Integer statisticsMode = Integer.valueOf((String) _core.readPublisherOptionOrDefault(userProfile.getDatabase(), WGACore.DBATTRIB_PERSSTATMODE));
            if (statisticsMode == null || statisticsMode.intValue() != Constants.PERSSTATMODE_HIT) {
                return;
            }
    
            userProfile.addHit();
            userProfile.setLastAccess(new Date());
    
            if (database != null) {
                userProfile.setDBLogin(database.getSessionContext().getUser());
            }
    
            userProfile.save();
        }
        catch (Exception e) {
            LOG.error("Unable to register hit in user profile", e);
        }
    }

    private void registerSession(WGUserProfile userProfile, HttpServletRequest request, HttpSession session, boolean created) {
    
        Integer statisticsMode = Integer.valueOf((String) _core.readPublisherOptionOrDefault(userProfile.getDatabase(), WGACore.DBATTRIB_PERSSTATMODE));
        if (!created && (statisticsMode == null || statisticsMode.intValue() == Constants.PERSSTATMODE_OFF)) {
            return;
        }
    
        try {
            userProfile.addNewSession(request, session);
            userProfile.setLastAccess(new Date());
            String userAgent = request.getHeader("User-Agent");
            if (userAgent != null) {
                userProfile.setClient(userAgent);
            }
    
            //userProfile.save();  // Conflicts with #00001412.
        }
        catch (WGAPIException e) {
            LOG.error("Unable to register session in user profile.", e);
        }
    }

    public TMLUserProfile prepareUserProfileForRequest(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, WGContent content, WGDatabase database,
            TMLForm.MultipartFormData formData, boolean isAjax) throws UnsupportedEncodingException, WGAPIException {
        
        // Fetch the profile
        PersonalisationManager persManager = _core.getPersManager();
        TMLUserProfile tmlUserProfile = persManager.fetchUserProfile(request, response, database);
        if (tmlUserProfile == null || tmlUserProfile.getprofile().isDeleted()) {
            return null;
        }
         
        // Register hit
        if (!WGPDispatcher.isBrowserInterface(request.getSession())) {
            persManager.registerHit(tmlUserProfile.getprofile(), database, content);
        }
    
        // Determine and set portlet environment
        String registryMode = determinePortletRegistryMode(database);
        TMLPortletStateStorage portletStateStorage = null;
        WGTransientPortletRegistry portletRegistry = null;
        
        if (WGDatabase.PORTLETREGISTRYMODE_TRANSIENT.equals(registryMode)) {
            portletRegistry = new WGTransientPortletRegistry(database, tmlUserProfile.getprofile());
            portletStateStorage = new TMLPortletStateTransientStorage(_core, tmlUserProfile.getprofile(), request, formData, isAjax);
        }
        else {
            portletStateStorage = new TMLPortletStateSessionStorage(request.getSession());
        }
        request.setAttribute(WGACore.ATTRIB_TRANSIENTPORTLETREGISTRY, portletRegistry);
        request.setAttribute(WGACore.ATTRIB_PORTLETSTATESTORAGE, portletStateStorage);
        
        return tmlUserProfile;
        
    }

    public void markProfileForAutoSave(WGUserProfile docProfile, int persMode, HttpSession session) throws WGAPIException {
        
        // Do never save for new sessions. If the user does not accept the session the profile will be lost forever.
        if (session != null && session.isNew()) {
            return;
        }
    
        // Never save for persmode session
        if (persMode == Constants.PERSMODE_SESSION) {
            return;
        }
        
        // In opt in mode we do not save the profile unless it already has been saved (which indicates that the opt-in has happened)
        boolean optInMode = (Boolean) _core.readPublisherOptionOrDefault(docProfile.getDatabase(), WGACore.DBATTRIB_PERSMODE_OPT_IN);
        if (optInMode && !docProfile.isSaved()) {
            return;
        }
        
        // If no non-save-clause applies we finally mark it for autosave
        docProfile.autoSave();
        
    }

    public static String determinePortletRegistryMode(WGDatabase database) {
        String regMode = (String) database.getAttribute(WGACore.DBATTRIB_PORTLETREGISTRYMODE); 
           if (regMode == null) {
                regMode = database.getComplianceVersion().isAtLeast(6, 2) ? WGDatabase.PORTLETREGISTRYMODE_TRANSIENT : WGDatabase.PORTLETREGISTRYMODE_PERSISTENT;
                if (regMode == null) {
                    regMode = WGDatabase.PORTLETREGISTRYMODE_PERSISTENT; // Sure fallback, works with all dbs
                }
            }
        return regMode;
    }

    public static TMLUserProfile retrieveCachedUserProfile(Map<String,Object> profiles, WGDatabase db) {
        Object profileObj =  profiles.get(db.getDbReference());
        if (profileObj != null && profileObj instanceof NullPlaceHolder) {
            return null;
        } else {
            return (TMLUserProfile) profileObj;
        }
    }

    private static Map<String, Object> getFetchedUserProfiles(HttpServletRequest request) {
    
        @SuppressWarnings("unchecked")
        Map<String, Object> profiles = (Map<String, Object>) request.getAttribute(WGACore.ATTRIB_FETCHED_USERPROFILES);
        if (profiles == null) {
            profiles = new HashMap<String,Object>();
            request.setAttribute(WGACore.ATTRIB_FETCHED_USERPROFILES, profiles);
        }
        return profiles;
        
    }

}
