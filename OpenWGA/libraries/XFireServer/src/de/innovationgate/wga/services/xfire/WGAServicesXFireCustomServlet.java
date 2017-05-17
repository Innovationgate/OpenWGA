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

import java.util.Map;

import javax.servlet.ServletException;

import org.codehaus.xfire.service.Service;
import org.codehaus.xfire.service.binding.ObjectServiceFactory;
import org.codehaus.xfire.service.invoker.BeanInvoker;

import de.innovationgate.wgaservices.WGACustomServices;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.services.WGACustomServicesImpl;
import de.innovationgate.wgpublisher.services.WGAWebService;

public class WGAServicesXFireCustomServlet extends WGAServicesXFireServlet implements WGAWebService {
    
    private WGACustomServicesImpl _customServices;

    @Override
    public void init() throws ServletException {
        super.init();
        
        ObjectServiceFactory factory = new ObjectServiceFactory(getXFire().getTransportManager(), null);        
        factory.addIgnoredMethods("java.lang.Comparable");
        WGACore core = WGACore.retrieve(getServletContext());

        if (core.getWgaConfiguration().isWebservicesEnabled()) {
            // register custom service
            Service service = factory.create(WGACustomServices.class, "custom", null, null);
            service.setProperty("mtom-enabled", "true");        
            _customServices = new WGACustomServicesImpl(core, new WGAWebServicesXFireContextProvider());       
            service.setInvoker(new BeanInvoker(_customServices));
            getController().getServiceRegistry().register(service);
        }        


        
    }
    
    public String getName() {
        return getServletName();
    }
    
    public Map<String, String> getURLs() {
        return null;
    }

}
