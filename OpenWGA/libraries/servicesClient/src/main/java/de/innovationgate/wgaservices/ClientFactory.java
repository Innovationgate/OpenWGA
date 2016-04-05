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
package de.innovationgate.wgaservices;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.MalformedURLException;
import java.nio.charset.Charset;
import java.util.Map;

import de.innovationgate.wgaservices.types.RemoteSession;

/**
 * A factory for WGA web service clients
 */
public class ClientFactory {

    public static final String SERVICE_PROVIDER_PATH = "META-INF/services/" + ClientFactoryService.class.getName(); 
	
    /**
     * Finds a client factory service on a class loader
     * @param loader
     * @return Client factory service
     * @throws WGAServiceException
     */
    public static ClientFactoryService findService(ClassLoader loader) throws WGAServiceException {
        
        InputStream serviceStream = loader.getResourceAsStream(SERVICE_PROVIDER_PATH);
        if (serviceStream == null) {
            throw new WGAServiceException("No ClientFactory service provider for WGAServices is in classpath");
        }
        
        try {
            LineNumberReader reader = new LineNumberReader(new InputStreamReader(serviceStream, Charset.forName("UTF-8")));
            String providerClassStr = null;
            do {
                providerClassStr = reader.readLine(); 
            } while (providerClassStr != null && providerClassStr.startsWith("#"));
            
            if (providerClassStr == null) {
                throw new WGAServiceException("Resource " + SERVICE_PROVIDER_PATH + " in classpath does not contain service provider information for WGAServices");
            }
            
            Class<ClientFactoryService> providerClass = (Class<ClientFactoryService>) loader.loadClass(providerClassStr);
            ClientFactoryService provider = providerClass.newInstance();
            return provider;
        }
        catch (Exception e) {
            throw new WGAServiceException("Exception creating ClientFactory service provider", e);
        }
        
        
    }
    
	
    /**
     * Creates a client for core services
     * @param wgaRootURL The root url of the OpenWGA server
     * @return A services client
     * @throws MalformedURLException
     * @throws WGAServiceException
     */
	public static WGACoreServices createCoreServiceClient(String wgaRootURL) throws MalformedURLException, WGAServiceException {
		return createCoreServiceClient(wgaRootURL, null);
	}
	
	/**
     * Creates a client for custom services
     * @param wgaRootURL The root url of the OpenWGA server
     * @return A services client
     * @throws MalformedURLException
     * @throws WGAServiceException
     */
	public static WGACustomServices createCustomServiceClient(String wgaRootURL) throws MalformedURLException, WGAServiceException {
		return createCustomServiceClient(wgaRootURL, null);
	}
	
	/**
	 * Creates a client for core services
	 * @param wgaRootURL The root url of the OpenWGA server
	 * @param properties Properties passed to the client factory service
	 * @return A services client
	 * @throws MalformedURLException
	 * @throws WGAServiceException
	 */
	public static WGACoreServices createCoreServiceClient(String wgaRootURL, Map<String,Object> properties) throws MalformedURLException, WGAServiceException {
	    return findService(ClientFactory.class.getClassLoader()).createCoreServiceClient(wgaRootURL, properties);
	}
	
	/**
     * Creates a client for custom services
     * @param wgaRootURL The root url of the OpenWGA server
     * @param properties Properties passed to the client factory service
     * @return A services client
     * @throws MalformedURLException
     * @throws WGAServiceException
     */
	public static WGACustomServices createCustomServiceClient(String wgaRootURL, Map<String, Object> properties) throws MalformedURLException, WGAServiceException {
	    return findService(ClientFactory.class.getClassLoader()).createCustomServiceClient(wgaRootURL, properties);
	}
	
    /**
     * Creates a helper object for calling remote actions on a WGA server.
     * An ActionCaller object can be used to call remote actions that are stored on a specific db on the WGA server.
     * @param services The WGAServices object that the caller will use
     * @param session The remote session that the caller will need to login to the server
     * @param db The database whose actions are to be called
     * @return An action caller object
     */
    public static ActionCaller createActionCaller(WGAServices services, RemoteSession session, String db) {
        return new ActionCaller(services, session, db);
    }
	
}
