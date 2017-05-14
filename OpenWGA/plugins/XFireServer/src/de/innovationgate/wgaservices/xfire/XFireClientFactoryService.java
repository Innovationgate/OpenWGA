/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/

package de.innovationgate.wgaservices.xfire;

import java.net.MalformedURLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.httpclient.params.HttpClientParams;
import org.apache.commons.httpclient.params.HttpConnectionParams;
import org.codehaus.xfire.client.Client;
import org.codehaus.xfire.client.XFireProxyFactory;
import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.service.binding.ObjectServiceFactory;
import org.codehaus.xfire.transport.http.CommonsHttpMessageSender;
import org.codehaus.xfire.transport.http.HttpTransport;

import de.innovationgate.wgaservices.ClientFactoryService;
import de.innovationgate.wgaservices.WGACoreServices;
import de.innovationgate.wgaservices.WGACustomServices;
import de.innovationgate.wgaservices.WGAServiceException;

public class XFireClientFactoryService implements ClientFactoryService {

    public WGACoreServices createCoreServiceClient(String wgaRootURL, Map<String, Object> properties) throws MalformedURLException, WGAServiceException {

        Service serviceModel = new ObjectServiceFactory().create(WGACoreServices.class);
        
        WGACoreServices service = (WGACoreServices) new XFireProxyFactory().create(serviceModel, wgaRootURL + "/services/core");        
        
        Client client = Client.getInstance(service);
        if (properties == null) {
            properties = new HashMap<String, Object>();
        }
        addDefaultServiceProperties(properties);
        
        Iterator<String> propKeys = properties.keySet().iterator();
        while (propKeys.hasNext()) {
            String key = propKeys.next();
            Object value = properties.get(key);
            client.setProperty(key, value);
        }
        return service;
        
    }

    public WGACustomServices createCustomServiceClient(String wgaRootURL, Map<String, Object> properties) throws MalformedURLException, WGAServiceException {
        
        Service serviceModel = new ObjectServiceFactory().create(WGACustomServices.class);
        
        WGACustomServices service = (WGACustomServices) new XFireProxyFactory().create(serviceModel, wgaRootURL + "/services/custom");  
        
        Client client = Client.getInstance(service);
        if (properties == null) {
            properties = new HashMap<String, Object>();
        }
        addDefaultServiceProperties(properties);
        
        Iterator<String> propKeys = properties.keySet().iterator();
        while (propKeys.hasNext()) {
            String key = propKeys.next();
            Object value = properties.get(key);
            client.setProperty(key, value);
        }
        return service;
    }
    
    private static void addDefaultServiceProperties(Map<String, Object> props) {
        if (!props.containsKey("mtom-enabled")) {
            props.put("mtom-enabled", "true");  
        }
        
        if (!props.containsKey(HttpTransport.CHUNKING_ENABLED)) {
            props.put(HttpTransport.CHUNKING_ENABLED, "true");  
        }       

        if (!props.containsKey(CommonsHttpMessageSender.HTTP_CLIENT_PARAMS)) {
            HttpClientParams params = new HttpClientParams();
            params.setIntParameter(HttpConnectionParams.CONNECTION_TIMEOUT, 1000 * 5);
            params.setIntParameter(HttpConnectionParams.SO_TIMEOUT, 1000 * 60);
            props.put(CommonsHttpMessageSender.HTTP_CLIENT_PARAMS, params);
        }
    }

}
