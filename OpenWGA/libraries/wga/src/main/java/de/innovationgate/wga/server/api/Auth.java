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

package de.innovationgate.wga.server.api;

import java.util.List;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.CustomCredentials;
import de.innovationgate.webgate.api.auth.LabeledNamesProvider;
import de.innovationgate.webgate.api.auth.UserGroupInfo;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.LoginException;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.auth.CSAuthModule;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * An object for providing services regarding user authentication.
 * This is normally created on behalf of a domain. 
 * However the deprecated method {@link WGA#auth()} returns an object without a domain in context which therefor will not be able to provide domain-specific services. 
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class Auth {
    
    private WGA _wga;
    private String _domain;
    private WGADomain _config;

    protected Auth(WGA wga) {
        _wga = wga;
    }
    
    protected Auth(WGA wga, String domain) throws WGException {
        _wga = wga;
        _domain = domain;
        _config = _wga.getCore().getDomains(_domain);
        if (_config == null) {
            throw new WGAServerException("Unknown domain '" + domain + "'");
        }

    }
    
    /**
     * Login to the domain with plain user/password 
     * @param user The user name
     * @param password The password
     * @return true if the login succeeded
     * @throws LoginException
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public boolean login(String user, String password) throws WGException {
        if (_config == null) {
            throw new UnavailableResourceException("The generic Auth object cannot be used to perform this operation. Retrieve it via the Domain object to do so");
        }
        
        return login(user, password, _domain);
    }
    
    /**
     * Login to a domain with plain user/password
     * @param user The user name
     * @param password The password
     * @param domain The domain to login to
     * @return true if the login succeeded
     * @throws LoginException
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public boolean login(String user, String password, String domain) throws WGException {
        return _wga.getCore().login(user, password, domain, _wga.getRequest(), _wga.getResponse());
    }

    /**
     * Login to the domain with user/password credentials
     * @param user The user name
     * @param creds The credentials
     * @return true if the login succeeded
     * @throws LoginException
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public boolean login(String user, CustomCredentials creds) throws WGException {
        return _wga.getCore().login(user, creds, _domain, _wga.getRequest(), _wga.getResponse());
    }
    
    /**
     * Login to a domain with user/password credentials
     * @param user The user name
     * @param creds The credentials
     * @param domain The domain to login to
     * @return true if the login succeeded
     * @throws LoginException
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public boolean login(String user, CustomCredentials creds, String domain) throws WGException {
        return _wga.getCore().login(user, creds, domain, _wga.getRequest(), _wga.getResponse());
    }
    
    /**
     * Logout from a domain
     * @param domain The domain to logout from
     * @return Always true
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public boolean logout(String domain) throws WGException {
        return _wga.getCore().logout(domain, _wga.getHttpSession(), _wga.getRequest(), _wga.getResponse(), true);
    }

    /**
     * Logout from the domain
     * @return Always true
     * @throws WGAPIException
     * @throws UnavailableResourceException
     */
    public boolean logout() throws WGException {
        if (_config == null) {
            throw new UnavailableResourceException("The generic Auth object cannot be used to perform this operation. Retrieve it via the Domain object to do so");
        }
        
        return logout(_domain);
    }
    
    /**
     * Change a password that is used to authenticate on a domain for the current session
     * This method should be used on "password self service" applications where users are able to change their login password on the authentication backend. After that is done his old password, which he has used before to login to the system, is still stored and present on his web session but no longer matches the changed password.
     * Calling this method after the password change will also change the stored password on the web session and therefor prevent the user from being logged out.
     * @param domain The domain
     * @param newPassword The new password to use
     * @throws UnavailableResourceException
     */
    public void changeSessionPassword(String domain, String newPassword) throws WGException {
        ((TMLContext) _wga.tmlcontext()).changesessionpassword(domain, newPassword);
    }
    
    /**
     * Change a password that is used to authenticate on the domain for the current session
     * This method should be used on "password self service" applications where users are able to change their login password on the authentication backend. After that is done his old password, which he has used before to login to the system, is still stored and present on his web session but no longer matches the changed password.
     * Calling this method after the password change will also change the stored password on the web session and therefor prevent the user from being logged out.
     * @param newPassword The new password to use
     * @throws UnavailableResourceException
     */
    public void changeSessionPassword(String newPassword) throws WGException {
        if (_config == null) {
            throw new UnavailableResourceException("The generic Auth object cannot be used to perform this operation. Retrieve it via the Domain object to do so");
        }
        
        changeSessionPassword(_domain, newPassword);
    }
    
    /**
     * Waits until updates on an authentication content store are effective
     * This is a rather special method for use in the UI of authentication applications, whose content stores store data about users and groups for login. If a TMLScript process updates login data of the currently logged in user - for example a password - it should stop execution until the authentication backend picks up that change (which mostly happens after a short delay). Only after that it can continue safely, knowing that it can use the updated login data.
     * Therefor after change of authentication data this method should get called, which waits for the authentication backend of the current domain to pick up updates.
     * A timeout prevents this methods from waiting endlessly in the case of an error.
     * @param db The authentication content store
     * @param timeoutSeconds
     * @throws WGAServerException
     */
    public void waitForUpdates(WGDatabase db, int timeoutSeconds) throws WGException {
        ((TMLContext) _wga.tmlcontext()).waitforauthupdates(db, timeoutSeconds);
    }
    
    /**
     * Waits until updates on the authentication content store of the current domain are effective
     * This is a rather special method for use in the UI of authentication applications, whose content stores store data about users and groups for login. If a TMLScript process updates login data of the currently logged in user - for example a password - it should stop execution until the authentication backend picks up that change (which mostly happens after a short delay). Only after that it can continue safely, knowing that it can use the updated login data.
     * Therefor after change of authentication data this method should get called, which waits for the authentication backend of the current domain to pick up updates.
     * A timeout prevents this methods from waiting endlessly in the case of an error.
     * @param timeoutSeconds
     * @throws WGAServerException
     */
    public void waitForUpdates(int timeoutSeconds) throws WGException {
        
        AuthenticationModule authModule = _config.getAuthModule();
        if (authModule instanceof CSAuthModule) {
            String authDbKey = ((CSAuthModule) authModule).getDbkey();
            WGDatabase authDb = _wga.getCore().getContentdbs().get(authDbKey);
            if (authDb != null) {
                waitForUpdates(authDb, timeoutSeconds);
            }
            else {
                throw new WGAServerException("Unknown authentication database of key '" + authDbKey + "'");
            }
        }
        
    }
    
    /**
     * Looks up user information by a distinguished user name
     * @param dn The distinguished user name
     * @return Information bean about the user
     * @throws WGAServerException
     * @throws WGQueryException
     */
    public UserGroupInfo lookupDN(String dn) throws WGException {
        
        if (_config == null) {
            throw new UnavailableResourceException("The generic Auth object cannot be used to perform this operation. Retrieve it via the Domain object to do so");
        }
        
        AuthenticationModule mod = _config.getAuthModule();
        if (mod == null) {
            throw new WGAServerException("No auth module configured for domain " + _domain);
        }
        return (UserGroupInfo) mod.query(dn, AuthenticationModule.QUERY_USER_DN);
        
    }
    
    /**
     * Fetches the common name if available
     * @param dn
     * @return common name if available or dn if not available
     * @throws WGException
     */
    public String fetchCommonName(String dn) throws WGException{
    	if(dn.equalsIgnoreCase(WGDatabase.ANONYMOUS_USER))
    		return dn;	// don't lookup anonymous user
    	String cn=null;
    	UserGroupInfo info = lookupDN(dn);
    	if(info != null && info instanceof LabeledNamesProvider)
    		cn = (String)((LabeledNamesProvider)info).getLabeledNames().get(AuthenticationModule.USERLABEL_COMMONNAME);
    	return cn != null ? cn : dn;
    }
    
    /**
     * Fetches the eMail address for a user from the authentication backend
     * This is a service whose availability depends on the Authentication source in use but should be available for most. Given any user name it returns the eMail adress for that user from the underlying system.
     * @param user Any user name
     * @return The mail address when it could be found, otherwise null
     * @throws WGAServerException
     */
    public String fetchEMail(String user) throws WGException {
        
        if (_config == null) {
            throw new UnavailableResourceException("The generic Auth object cannot be used to perform this operation. Retrieve it via the Domain object to do so");
        }
        
        AuthenticationModule mod = _config.getAuthModule();
        if (mod == null) {
            throw new WGAServerException("No auth module configured for domain " + _domain);
        }
        return mod.getEMailAddress(user);
    }
    
    /**
     * Searches for users and groups on the authentication backend
     * This is an optional service of authentication sources that allows to search for users and groups that are known to the backend by providing a query. In most cases the query is interpreted as containing a port of the user/group name.
     * @param query The query
     * @return Result information of the query
     * @throws WGAServerException
     * @throws WGQueryException
     */
    @SuppressWarnings("unchecked")
    public List<UserGroupInfo> findUsersAndGroups(String query) throws WGException {
        AuthenticationModule mod = _config.getAuthModule();
        if (mod == null) {
            throw new WGAServerException("No auth module configured for domain " + _domain);
        }
        return (List<UserGroupInfo>) mod.query(query, AuthenticationModule.QUERY_USERS_AND_GROUPS);
    }
    
    /**
     * Returns the WGAPI authentication module backing this authentication
     */
    public AuthenticationModule getModule() {
        return _config.getAuthModule();
    }
    
    /**
     * Returns the user name by which the current user is logged in to the current domain
     * This is the user name as it came from the authentication process, for example via a manual login on some login page. This is not necessarily the qualified name.
     * In case of an anonymous user this is "anonymous".
     * @throws UnavailableResourceException
     */
    public String getSessionUser() throws WGException {
        
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo != null) {
            return loginInfo.getUserName();
        }
        
        return WGDatabase.ANONYMOUS_USER;
        
        
    }
    
    /**
     * Returns the credentials by which the current user is logged in to this domain
     * When using regular user/password authentication this will return a string password. If the user is anonymous this returns null.
     * @throws UnavailableResourceException
     */
    public Object getSessionCredentials() throws WGException {
        
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo != null) {
            return loginInfo.getCredentials();
        }
        else {
            return null;
        }
        
    }

    /**
     * Returns the WGA login info object for the current domain, storing the users credentials for reuse accross requests
     * @throws UnavailableResourceException
     */
    @CodeCompletion
    private DBLoginInfo getLoginInfo() throws WGException {
        
        if (_domain == null) {
            throw new WGAServerException("Unavailable operation for domain-independent Auth object");
        }
        
        if (_wga.session().isAvailable()) {
            Map<Object,DBLoginInfo> logins = _wga.session().getLogins();
            if (logins != null) {
                return logins.get(_domain);
            }
        }
        
        return null;
    }
    
    /**
     * Applies an access filter to the current user on the current domain.
     * This will implicitly reopen open sessions on all databases of the domain, so that the filter is immediately active
     * @param filter The uid of the filter do apply
     * @throws WGException
     */
    public void applyAccessFilter(String filter) throws WGException {
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo == null) {
            throw new WGAServerException("Cannot set access filter for anonymous user");
        }
        loginInfo.setAccessFilter(filter);
        _wga.domain(_domain).reopenSessions();
        
    }
    
    /**
     * Applies an access filter to the current user only on given database
     * This will implicitly reopen an open session on that database, so that the filter is immediately active
     * @param filter The uid of the filter do apply
     * @param db The database on which the filter should be applied
     * @throws WGException
     */
    public void applyAccessFilter(String filter, Database db) throws WGException {
        
        if (!db.domain().getName().equals(_domain)) {
            throw new WGIllegalArgumentException("Database '" + db.getDbKey() + "' does not belong to domain " + _domain);
        }
        
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo == null) {
            throw new WGAServerException("Cannot set access filter for anonymous user");
        }
        loginInfo.getDbAccessFilters().put(db.getDbKey(), filter);
        db.reopen();
    }
    
    /**
     * Returns the currently active access filter uid for the domain, null if there is none
     * @throws WGException
     */
    public String getAccessFilter() throws WGException {
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo == null) {
            return null;
        }
        return loginInfo.getAccessFilter();
    }
    
    /**
     * Returns the currently active access filter uid on the given database, null if there is none
     * @throws WGException
     */
    public String getAccessFilter(Database db) throws WGException {
        
        if (!db.domain().getName().equals(_domain)) {
            throw new WGIllegalArgumentException("Database '" + db.getDbKey() + "' does not belong to domain " + _domain);
        }
        
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo == null) {
            return null;
        }
        return loginInfo.getDbAccessFilters().get(db.getDbKey());
    }
    
    /**
     * Removes a currently active access filter on the domain
     */
    public void removeAccessFilter() throws WGException {
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo == null) {
            throw new WGAServerException("Cannot set access filter for anonymous user");
        }
        loginInfo.setAccessFilter(null);
        _wga.domain(_domain).reopenSessions();
    }
    
    /**
     * Removes a currently active access filter on the given database
     * @param db The database to remove the filter from
     */
    public void removeAccessFilter(Database db) throws WGException {
        
        if (!db.domain().getName().equals(_domain)) {
            throw new WGIllegalArgumentException("Database '" + db.getDbKey() + "' does not belong to domain " + _domain);
        }
        
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo == null) {
            throw new WGAServerException("Cannot set access filter for anonymous user");
        }
        loginInfo.getDbAccessFilters().remove(db.getDbKey());
        db.reopen();
    }
    
    /**
     * Returns the name by which the user is logged into this domain, null if he isn't
     * @throws WGException
     */
    public String getLoginName() throws WGException {
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo != null) {
            return loginInfo.getUserName();
        }
        else {
            return null;
        }
    }

    /**
     * Returns the distinguished name of the current user. This may differ from the login-name.
     * @return distinguished name (DN)
     * @throws WGException
     */
    public String getUserName() throws WGException {
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo != null) {
            return loginInfo.getDN();
        }
        else {
            return null;
        }
    }

    /**
     * Returns the type of authentication that the user used to authenticate to the current domain. null if the user is not logged in.
     * Valid values: "password" (classic login via username/password), "cert" (client certificate), "request" (request metadata, like on most SSO solutions)
     * @throws WGException
     */
    public String getAuthenticationType() throws WGException {
        DBLoginInfo loginInfo = getLoginInfo();
        if (loginInfo != null) {
            return loginInfo.getAuthenticationType().toString();
        }
        else {
            return null;
        }
        
    }
    
    /**
     * Returns if the user is currently anonymous
     * @throws WGException 
     */
    public boolean isAnonymous() throws WGException {
        String loginName = getLoginName();
        return (loginName == null || loginName.equals(WGDatabase.ANONYMOUS_USER));
    }

    
}