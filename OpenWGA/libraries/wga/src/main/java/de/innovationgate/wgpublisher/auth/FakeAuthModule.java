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

package de.innovationgate.wgpublisher.auth;

import java.util.Map;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.auth.AnonymousAuthSession;
import de.innovationgate.webgate.api.auth.AuthenticationException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.webgate.api.auth.ConfigurationException;

/**
 * An authentication module that has the same behaviour as having no auth module at all
 * Only anonymous login will succeed.
 */
public class FakeAuthModule implements AuthenticationModule {
    
    public static final FakeAuthModule INSTANCE = new FakeAuthModule();
    
    private FakeAuthModule() {
    }

    public void addAuthenticationSourceListener(AuthenticationSourceListener listener) {
    }

    public void clearCache() {
    }

    public void destroy() {
    }

    public Class[] getAllowedCredentialClasses() {
        return new Class[] {String.class};
    }

    public String getAuthenticationSource() {
        return "No authentication";
    }

    public String getEMailAddress(String user) {
        return null;
    }

    public void init(Map params, WGDatabase db) throws ConfigurationException {
    }

    public boolean isGeneratesSessionToken() {
        return false;
    }

    public boolean isQueryable(String queryType) {
        return false;
    }

    public AuthenticationSession login(String user, Object credentials) throws AuthenticationException {

        if (user.equals(WGDatabase.ANONYMOUS_USER)) {
            return AnonymousAuthSession.getInstance();
        }
        else {
            return null;
        }
    }

    public Object query(Object query, String queryType) {
        return null;
    }

    public void removeAuthenticationSourceListener(AuthenticationSourceListener listener) {
    }

}
