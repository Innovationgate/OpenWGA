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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hibernate.QueryException;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.auth.AnonymousAuthSession;
import de.innovationgate.webgate.api.auth.AuthenticationException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.AuthenticationSession;
import de.innovationgate.webgate.api.auth.AuthenticationSourceListener;
import de.innovationgate.webgate.api.auth.ConfigurationException;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreEvent;
import de.innovationgate.wgpublisher.WGACoreEventListener;

/**
 * An authentication module that delegates everything to the auth module of a given domain
 * 
 * Unlike {@link DomainRedirectionAuthModule} this module is capable of being created even before the domain is completely setup
 * and therefor can be used for OpenWGA plugins that want to authenticate against a domain. If the auth module is created before
 * the auth domain is ready (on OpenWGA startup) then this module caches all auth listeners that register. Once startup has finished
 * it will delegate all those listeners to the backend module.
 */
public class DelegatingAuthModule implements CoreAwareAuthModule, WGACoreEventListener {
    
    public static final String COPTION_DOMAIN = "delegate.domain";
    
    private String _domainName;
    
    public String getDomainName() {
        return _domainName;
    }

    private WGACore _core;
    private List _cachedListeners = new ArrayList();
    private boolean _ready = false;

    public DelegatingAuthModule() {
    }

    public void setCore(WGACore core) {
        _core = core;
    }
    
    private AuthenticationModule getAuthModule()  {
        
        if (!_ready) {
            return null;
        }
        
        WGADomain conf = _core.getDomains(_domainName);
        if (conf != null) {        
            return conf.getAuthModule();
        }
        else {
            return null;
        }
    }

    public synchronized void addAuthenticationSourceListener(AuthenticationSourceListener listener) {
        
        if (!_ready) {
            _cachedListeners.add(listener);
            return;
        }
        
        
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return;
        }
        
        mod.addAuthenticationSourceListener(listener);
    }

    public void clearCache() {

        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return;
        }
        
        mod.clearCache();

    }

    public void destroy() {
        
        if (!_ready) {
            return;
        }
        
        _core.removeEventListener(this);
        // No, we won't pass this on. Target module should destroy itself
    }

    public Class[] getAllowedCredentialClasses() {
        
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return new Class[] {};
        }
        
        return mod.getAllowedCredentialClasses();

    }

    public String getAuthenticationSource() {
        
        if (!_ready) {
            return "Delegate Auth Module, not yet ready";
        }
        
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return "No authentication";
        }
        
        return mod.getAuthenticationSource() + " (delegated)";
    }

    public String getEMailAddress(String user) {
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return null;
        }
        
        return mod.getEMailAddress(user);

    }

    public synchronized void init(Map params, WGDatabase db) throws ConfigurationException {
        _domainName = (String) params.get(COPTION_DOMAIN);
        
        if (_core.getStatus() < WGACoreEvent.TYPE_STARTUP_POST_CONNECT) {
            _ready = false;
            _core.addEventListener(this);
        }
        else {
            enable();
        }
    }

    private void enable() {
        AuthenticationModule mod = getAuthModule();
        if (mod != null) {
            Iterator listeners = _cachedListeners.iterator();
            while (listeners.hasNext()) {
                AuthenticationSourceListener listener = (AuthenticationSourceListener) listeners.next();
                mod.addAuthenticationSourceListener(listener);
            }
        }
        _cachedListeners.clear();
        _ready = true;
    }

    public boolean isGeneratesSessionToken() {
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return false;
        }
        
        return mod.isGeneratesSessionToken();

    }

    public boolean isPoolable() {
        return true;
    }

    public boolean isQueryable(String queryType) {
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return false;
        }
        
        return mod.isQueryable(queryType);
    }

    public AuthenticationSession login(String user, Object credentials) throws AuthenticationException {
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return AnonymousAuthSession.getInstance();
        }
        
        return mod.login(user, credentials);

    }

    public Object query(Object query, String queryType) throws WGQueryException {
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return null;
        }
        
        return mod.query(query, queryType);

    }

    public void removeAuthenticationSourceListener(AuthenticationSourceListener listener) {
        
        if (!_ready) {
            _cachedListeners.remove(listener);
        }
        
        AuthenticationModule mod = getAuthModule();
        if (mod == null) {
            return;
        }
        
        mod.removeAuthenticationSourceListener(listener);
    }

    public void contentStoreConnected(WGACoreEvent event) {
    }

    public void contentStoreDisconnected(WGACoreEvent event) {
    }

    public void shutdownPostDisconnect(WGACoreEvent event) {
    }

    public void shutdownPreDisconnect(WGACoreEvent event) {
    }

    public void startupPostConnect(WGACoreEvent event) {

        if (!_ready) {
            enable();
        }
        
    }

    public void startupPreConnect(WGACoreEvent event) {
    }
    

}
