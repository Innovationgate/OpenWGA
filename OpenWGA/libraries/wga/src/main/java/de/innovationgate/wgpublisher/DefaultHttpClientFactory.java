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

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;

import de.innovationgate.webgate.api.HttpClientFactory;
import de.innovationgate.wga.config.WGAConfiguration;

public class DefaultHttpClientFactory implements HttpClientFactory {
    
    private WGAConfiguration _config;

    public DefaultHttpClientFactory(WGAConfiguration config) {
        _config = config;
    }

    public HttpClient createHttpClient() {
        HttpClient client = new HttpClient();
        client.getHttpConnectionManager().getParams().setConnectionTimeout(10 * 1000);
        client.getHttpConnectionManager().getParams().setSoTimeout(60 * 1000);
        
        if (_config != null && _config.getProxyConfiguration() != null && _config.getProxyConfiguration().isEnabled()) {
            if (_config.getProxyConfiguration().getHttpProxy() != null) {
                client.getHostConfiguration().setProxy(_config.getProxyConfiguration().getHttpProxy(), _config.getProxyConfiguration().getHttpProxyPort());
                
                if (_config.getProxyConfiguration().getUser() != null) {
                    client.getState().setProxyCredentials(AuthScope.ANY, new UsernamePasswordCredentials(_config.getProxyConfiguration().getUser(), _config.getProxyConfiguration().getPassword()));
                }
            }
            
        }
        
        return client;
    }

}
