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

import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.RedirectionAuthModule;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGACore;

public class DomainRedirectionAuthModule extends RedirectionAuthModule {
    
    private WGACore _core;
    private String _domain;

    public String getDomain() {
        return _domain;
    }

    public DomainRedirectionAuthModule(WGACore core, String domain) {
        _core = core;
        _domain = domain;
    }

    @Override
    public AuthenticationModule getBackendModule() {
        WGADomain domainConfig = _core.getDomains(_domain);
        if (domainConfig != null) {
            return domainConfig.getAuthModule();
        }
        else {
            return FakeAuthModule.INSTANCE;
        }
    }

    @Override
    public void destroy() {
        // We do not pass on destroyal of database auth modules to the domain module in backend
    }
    

}
