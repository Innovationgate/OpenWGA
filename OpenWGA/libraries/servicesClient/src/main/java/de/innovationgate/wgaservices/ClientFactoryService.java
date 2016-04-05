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

import java.net.MalformedURLException;
import java.util.Map;

/**
 * Interface for client factory services
 */
public interface ClientFactoryService {
    
    /**
     * Creates a client for custom services
     * @param wgaRootURL The root url of the OpenWGA server
     * @param properties Properties passed to the client factory service
     * @return A services client
     * @throws MalformedURLException
     * @throws WGAServiceException
     */
    public WGACustomServices createCustomServiceClient(String wgaRootURL, Map<String, Object> properties) throws MalformedURLException, WGAServiceException;
    
    /**
     * Creates a client for core services
     * @param wgaRootURL The root url of the OpenWGA server
     * @param properties Properties passed to the client factory service
     * @return A services client
     * @throws MalformedURLException
     * @throws WGAServiceException
     */
    public WGACoreServices createCoreServiceClient(String wgaRootURL, Map<String,Object> properties) throws MalformedURLException, WGAServiceException;
    
}
