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

package de.innovationgate.wgpublisher.cache;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpMethodBase;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.GetMethod;
import org.cyberneko.html.parsers.DOMParser;
import org.dom4j.io.DOMReader;
import org.xml.sax.InputSource;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.WGACore;

public class HttpCachePreloader implements CachePreloader {
    
    public interface PrepareTask {
        
        public void prepare(HttpClient client, HttpMethodBase method);

    }
    
    private String _url;
    private Credentials _credentials;
    private PrepareTask _prepareTask = null;
    
    public HttpCachePreloader(String url) {
        _url = url;
    }
    
    public HttpCachePreloader(String url, Credentials creds) {
        _url = url;
        _credentials = creds;
    }
    
    public HttpCachePreloader(String url, String user, String pwd) {
        _url = url;
        _credentials = new UsernamePasswordCredentials(user, pwd); 
    }

    @Override
    public String preloadCache(WGACore core, WGDatabase db) throws CachePreloadException {

        try {
            HttpClient client = WGFactory.getHttpClientFactory().createHttpClient();
            
            if (_credentials != null) {
                client.getState().setCredentials(AuthScope.ANY, _credentials);
                client.getParams().setAuthenticationPreemptive(true);
            }
            
            GetMethod method = new GetMethod(_url);
            
            if (_prepareTask != null) {
                _prepareTask.prepare(client, method);
            }
            
            int status = client.executeMethod(method);
            
            if (method.getStatusCode() != 200) {
                if (method.getStatusCode() == 503 || method.getStatusCode() == 504 || method.getStatusCode() == 507 || method.getStatusCode() == 509) {
                    throw new CachePreloadException("HTTP Status Code is " + method.getStatusCode() + ". Server either timed out or not ready. Will requeue.", false);
                }
                else {
                    throw new CachePreloadException("HTTP Status Code is " + method.getStatusCode(), false);
                }
            }
            
            Reader reader;
            if (method.getResponseCharSet() != null) {
                reader = new InputStreamReader(method.getResponseBodyAsStream(), method.getResponseCharSet());
            }
            else {
                reader = new InputStreamReader(method.getResponseBodyAsStream(), "UTF-8");    
            }
            
            String contents = WGUtils.readString(reader);
            return contents;
        }
        catch (Exception e) {
            throw new CachePreloadException("Exception preloading cache", e, false);
        }
        
    }

    public PrepareTask getPrepareTask() {
        return _prepareTask;
    }

    public void setPrepareTask(PrepareTask prepareTask) {
        _prepareTask = prepareTask;
    }

}
