package de.innovationgate.wga.services.rest;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

import org.glassfish.jersey.servlet.ServletContainer;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.services.WGAWebService;

public class RestService implements WGAWebService {
    
    public static class EnforceEncodingRequest extends HttpServletRequestWrapper {

        public EnforceEncodingRequest(HttpServletRequest request) {
            super(request);
        }
        
        @Override
        public String getHeader(String name) {
            String value = super.getHeader(name);
            return convertHeader(name, value);
        }

        protected String convertHeader(String name, String value) {
            if ("Content-Type".equalsIgnoreCase(name)) {
                if (value.toLowerCase().indexOf("charset=") == -1) {
                    value = value + ";charset=UTF-8";
                }
            }
            return value;
        }
        
        @SuppressWarnings({ "rawtypes", "unchecked" })
        @Override
        public Enumeration getHeaders(final String name) {

            final Enumeration<String> rawHeaders = super.getHeaders(name);
            return new Enumeration<String>() {

                @Override
                public boolean hasMoreElements() {
                    return rawHeaders.hasMoreElements();
                }

                @Override
                public String nextElement() {
                    String value = rawHeaders.nextElement();
                    return convertHeader(name, value);
                }
            };
            
        }
        
    }
    
    private ServletContainer _jerseyServlet;
    private String _name;
    private WGA _wga;

    @Override
    public void init(ServletConfig cfg) throws ServletException {

        try {
            _wga = WGA.get(cfg.getServletContext());
            _jerseyServlet = new ServletContainer(new RestApplication(_wga));
            _jerseyServlet.init(cfg);
            _name = cfg.getServletName();
        }
        catch (WGException e) {
            throw new ServletException("Exception initializing REST service", e);
        }
        
    }

    @Override
    public void service(ServletRequest req, ServletResponse res) throws IOException, ServletException {
        
        // Inject HTTP login
        HttpServletRequest request = ((HttpServletRequest) req);

        String credentials = request.getHeader("Authorization");
        if (credentials != null && credentials.trim().toLowerCase().startsWith("basic")) {
            DBLoginInfo loginInfo = DBLoginInfo.createFromHttpCredentials(credentials);
            if (loginInfo != null) {
                request.setAttribute(WGPRequestPath.REQATTRIB_HTTPLOGIN, loginInfo);
                request.setAttribute(WGACore.ATTRIB_FORCEREGULARLOGIN, true);
            }
        }
        
        _jerseyServlet.service(new EnforceEncodingRequest(request), res);
    }
        
    @Override
    public void destroy() throws ServletException {
        _jerseyServlet.destroy();
    }
    
    @Override
    public String getName() {
        return _name;
    }
    
    @Override
    public Map<String, String> getURLs() {
        return null;
    }
    
}
