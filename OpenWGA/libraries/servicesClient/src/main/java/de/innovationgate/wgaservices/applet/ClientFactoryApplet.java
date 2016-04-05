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

package de.innovationgate.wgaservices.applet;

import java.applet.Applet;

import de.innovationgate.wgaservices.ClientFactory;
import de.innovationgate.wgaservices.WGACoreServices;
import de.innovationgate.wgaservices.WGACustomServices;
import de.innovationgate.wgaservices.WGAServices;


/**
 * A simple applet that has no GUI but offers a WGAService client to JavaScript via LiveConnect.
 */
public class ClientFactoryApplet extends Applet {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;

    public void init() {}
	public void destroy() {}
	public void start() {}
	public void stop() {}
	
	public WGACustomServices createCustomServiceClient(String url) throws Exception {
		return ClientFactory.createCustomServiceClient(url);
	}
	
	public WGACoreServices createCoreServiceClient(String url) throws Exception {
		return ClientFactory.createCoreServiceClient(url);
	}

}
