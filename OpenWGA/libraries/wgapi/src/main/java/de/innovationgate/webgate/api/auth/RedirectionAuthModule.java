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
package de.innovationgate.webgate.api.auth;

import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGQueryException;

/**
 * Auth module implementation that redirects all calls to some backend module, to be defined by subclasses.
 */
public abstract class RedirectionAuthModule implements AuthenticationModule, PasswordCachingAuthenticationModule {

    public void addAuthenticationSourceListener(AuthenticationSourceListener listener) {
        getBackendModule().addAuthenticationSourceListener(listener);
    }

    public void clearCache() {
        getBackendModule().clearCache();
    }

    public void destroy() {
        getBackendModule().destroy();
    }

    public Class[] getAllowedCredentialClasses() {
        return getBackendModule().getAllowedCredentialClasses();
    }

    public String getAuthenticationSource() {
        return getBackendModule().getAuthenticationSource();
    }

    public String getEMailAddress(String user) {
        return getBackendModule().getEMailAddress(user);
    }

    public void init(Map params, WGDatabase db) throws ConfigurationException {
        getBackendModule().init(params, db);
    }

    public boolean isGeneratesSessionToken() {
        return getBackendModule().isGeneratesSessionToken();
    }

    public boolean isQueryable(String queryType) {
        return getBackendModule().isQueryable(queryType);
    }

    public AuthenticationSession login(String user, Object credentials) throws AuthenticationException {
        return getBackendModule().login(user, credentials);
    }

    public Object query(Object query, String queryType) throws WGQueryException {
        return getBackendModule().query(query, queryType);
    }

    public void removeAuthenticationSourceListener(AuthenticationSourceListener listener) {
        getBackendModule().removeAuthenticationSourceListener(listener);
    }

    /**
     * Returns the the backend module
     */
    public abstract AuthenticationModule getBackendModule();
    
    public void dropPasswordCache(String loginName) {
        AuthenticationModule mod = getBackendModule();
        if (mod instanceof PasswordCachingAuthenticationModule) {
            ((PasswordCachingAuthenticationModule) mod).dropPasswordCache(loginName);
        }
    }

}
