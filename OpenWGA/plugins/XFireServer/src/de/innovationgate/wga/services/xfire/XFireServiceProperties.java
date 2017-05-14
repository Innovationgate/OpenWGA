/*******************************************************************************
 * Copyright 2010 Innovation Gate GmbH
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

package de.innovationgate.wga.services.xfire;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.wga.modules.types.WGAWebServiceProperties;
import de.innovationgate.wgpublisher.services.PathHidingRequestFacade;
import de.innovationgate.wgpublisher.services.WGAWebServicesFilter;

public class XFireServiceProperties extends WGAWebServiceProperties {
    
    @Override
    public HttpServletRequest facadeRequest(HttpServletRequest req, String servicePath) {
        return new PathHidingRequestFacade(req, WGAWebServicesFilter.BASEPATH);
    }

}
