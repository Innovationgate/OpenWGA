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

package de.innovationgate.wgpublisher.services;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.jdom.IllegalDataException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.types.WGAWebServiceModuleType;
import de.innovationgate.wga.modules.types.WGAWebServiceProperties;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.filter.WGAFilterConfig;

public class WGAWebServicesFilter implements Filter {
    
    public static class RegisteredService {
        
        private WGAWebService _service;
        private WGAWebServiceProperties _properties;
        
        protected WGAWebService getService() {
            return _service;
        }

        protected WGAWebServiceProperties getProperties() {
            return _properties;
        }

        public RegisteredService(WGAWebService service, WGAWebServiceProperties properties) {
            super();
            _service = service;
            _properties = properties;
        }
        
        
        
    }
    
    public static final Logger LOG = Logger.getLogger("wga.services");

    public static final String BASEPATH = "/services";
    
    
    private WGACore _core;
    private Map<String,RegisteredService> _services = new HashMap<String,RegisteredService>();

    public void destroy() {
        for (Map.Entry<String,RegisteredService> serviceEntry : _services.entrySet()) {
            try {
                serviceEntry.getValue().getService().destroy();
            }
            catch (Throwable e) {
                LOG.error("Exception destroying servlet for WGA service '" + serviceEntry.getKey() + "'", e);
            }
        }
    }

    public void doFilter(ServletRequest req, ServletResponse res, FilterChain filterChain) throws IOException, ServletException {
        HttpServletRequest httpReq = (HttpServletRequest) req;
        HttpServletResponse httpRes = (HttpServletResponse) res;
        String servicePath = httpReq.getServletPath().substring(BASEPATH.length());
        if ("".equals(servicePath) || "/".equals(servicePath)) {
            showServiceOverview(httpRes, httpReq);
        }
        else {
            List<String> pathParts = WGUtils.deserializeCollection(servicePath.substring(1), "/");
            String serviceName = pathParts.get(0);
            RegisteredService registeredService = _services.get(serviceName);
            if (registeredService != null) {
                httpReq = registeredService.getProperties().facadeRequest(httpReq, BASEPATH + "/" + serviceName);
                registeredService.getService().service(httpReq, res);
            }
            else {
                httpRes.sendError(404, "Unknown OpenWGA service '" + serviceName + "'");
            }
        }
    }

    private void showServiceOverview(HttpServletResponse httpRes, HttpServletRequest httpReq) throws IOException {

        PrintWriter out = httpRes.getWriter();
        out.write("<!doctype html>\n<html lang=\"en\">\n");
        out.write("<head><meta charset=\"UTF-8\"><title>OpenWGA Web Services</title></head>\n");
        out.write("<body>\n");
        out.write("<h1>OpenWGA Web Services</h1>\n");
        out.write("<ul>");
        
        for (Map.Entry<String, RegisteredService> serviceEntry : _services.entrySet()) {
            out.write("<li><b>" + serviceEntry.getKey() + "</b>: " + serviceEntry.getValue().getService().getName());
        }
        
        out.write("</ul>");
        out.write("\n</body>\n</html>");
        
    }

    public void init(FilterConfig fc) throws ServletException {
        
            WGAFilterConfig wgaFc = (WGAFilterConfig) fc;
            _core = WGACore.retrieve(wgaFc.getServletContext());
            
            Iterator<ModuleDefinition> impls = _core.getModuleRegistry().getModulesForType(WGAWebServiceModuleType.class).values().iterator();
            while (impls.hasNext()) {
                KeyBasedModuleDefinition serviceMod = (KeyBasedModuleDefinition) impls.next();
                try {
                    WGAWebService servlet = (WGAWebService) _core.getModuleRegistry().instantiate(serviceMod);
                    WGAWebServiceProperties props = (WGAWebServiceProperties) serviceMod.getProperties();
                    if (props == null) {
                        props = new WGAWebServiceProperties();
                    }
                    ServletConfig sc = props.buildServletConfig(fc.getServletContext(), serviceMod);
                    servlet.init(sc);
                    _services.put(serviceMod.getRegistrationKey().toLowerCase(), new RegisteredService(servlet, props));
                }
                catch (Throwable e) {
                    LOG.error("Exception creating servlet for WGA service '" + serviceMod.getRegistrationKey() + "'", e);
                }
            }
            
        
    }

}
