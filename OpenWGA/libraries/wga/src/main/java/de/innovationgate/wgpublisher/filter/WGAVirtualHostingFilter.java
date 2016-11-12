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

package de.innovationgate.wgpublisher.filter;

import java.io.IOException;
import java.net.IDN;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;

import de.innovationgate.utils.URLBuilder;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.config.ConfigBean;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.config.VirtualResource;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.WGPDispatcher.PathDispatchingOccasion;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;

public class WGAVirtualHostingFilter implements Filter , WGAFilterURLPatternProvider {
    
    public static final String REQUESTATTRIB_VIRTUAL_HOST = WGAVirtualHostingFilter.class.getName() + ":VirtualHost";
    
    private class DefaultDBRequestWrapper extends HttpServletRequestWrapper {

        private HttpServletRequest _request;
        private String _dbkey;

        public DefaultDBRequestWrapper(WGACore core, HttpServletRequest request, String dbkey) {
            super(request);
            _core = core;
            _request = request;
            _dbkey = dbkey;
        }

        @Override
        public String getRequestURI() {         
            String uri = super.getRequestURI();
            String path = uri.substring(_request.getContextPath().length());
            uri = _request.getContextPath() + "/" + _dbkey + path;
            return uri;
        }

        @Override
        public StringBuffer getRequestURL() {
            StringBuffer url = super.getRequestURL();           
            try {
                URLBuilder builder = new URLBuilder(new URL(url.toString()));
                String path = builder.getPath().substring(_request.getContextPath().length());
                builder.setPath(_request.getContextPath() + "/" + _dbkey + path);
                return new StringBuffer(builder.build(true));
            } catch (Exception e) {
                _core.getLog().error("Unable to compute request url.", e);
            }       
            return url;
        }
        
    }

    private static final List<String> BLACK_LIST = new ArrayList<String>();
    static {
        BLACK_LIST.add("/plugin-*");
        BLACK_LIST.add("/ajaxform*");
        BLACK_LIST.add("/contentmanager");
        BLACK_LIST.add("/tempdwn*");
        BLACK_LIST.add("/services");
        BLACK_LIST.add("/webdav/*");
        BLACK_LIST.add("/" + WGPRequestPath.PATHCMD_TMLFORM + "/*");
    }
    
    private static final List<String> WHITE_LIST = new ArrayList<String>();
    static {
        WHITE_LIST.add("/favicon.ico");
        WHITE_LIST.add("/login");
        WHITE_LIST.add("/logout");
    }
    
    private WGACore _core;
    

    public void init(FilterConfig config) throws ServletException {
        _core = WGACore.retrieve(config.getServletContext());
    }

    public void destroy() {
    }

    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;

        VirtualHost vHost = findMatchingHost(_core.getWgaConfiguration(), request);
        if (vHost != null) {
            // handle vhost
            request.setAttribute(REQUESTATTRIB_VIRTUAL_HOST, vHost);

            // determine default database key
            String defaultDBKey = getDefaultDBKey(_core, vHost);

            String uri = httpRequest.getRequestURI();
            int semiPos = uri.indexOf(";");
            if (semiPos != -1) {
                uri = uri.substring(0, semiPos);
            }

            if (uri.equalsIgnoreCase("/robots.txt") && findVirtualResource(vHost, "robots.txt")==null){
            	response.getWriter().print(vHost.getRobotsTxt());
            	return;
            }
            if (uri.equalsIgnoreCase("/login") && httpRequest.getMethod().equalsIgnoreCase("post")) {
                // skip post to login url - for this filter
            }
            else {
                String[] pathElements = uri.split("/");
                if (pathElements == null || pathElements.length < 1) {
                    // root url request - redirect to default database or hide db
                    if (defaultDBKey != null) {
                    	
                        if (vHost.isHideDefaultDatabaseInURL()) {
                        	//httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
                        	
                        	if(vHost.isHideHomepageURL()){
								try {
		                            WGDatabase db = (WGDatabase) _core.getContentdbs().get(defaultDBKey);
		                            WGAURLBuilder urlBuilder = _core.retrieveURLBuilder(httpRequest, db);	                            
									String homepage = "/" + defaultDBKey + urlBuilder.buildHomepageURL(db, httpRequest);
	                                httpRequest.getRequestDispatcher(homepage).forward(request, response);
	                                return;
								} catch (Exception e) {
									_core.getLog().error("v-host: unable to proxy homepage", e);
									// fallback:
									httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
								}    
                        	}
							else httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
							
                        }
                        else {
                            httpResponse.sendRedirect(httpResponse.encodeRedirectURL(uri + defaultDBKey));
                            return;
                        }
                    }
                }
                else if (pathElements.length == 2 && isRootResource(vHost, pathElements[1])) {
                    // handle root resource request
                    VirtualResource resource = findVirtualResource(vHost, pathElements[1]);
                    httpRequest.getRequestDispatcher(resource.getPath()).forward(request, response);
                    return;
                }
                else {
                    // normal db request
                    String requestedDBKey = pathElements[1];
                    if (vHost.isHideDefaultDatabaseInURL() && defaultDBKey != null) {
                        if (requestedDBKey.equalsIgnoreCase(defaultDBKey)) {
                            // if default db requested redirect to url without
                            // dbkey
                            URLBuilder builder = new URLBuilder(httpRequest, _core.getCharacterEncoding());
                            builder.setEncoding(_core.getCharacterEncoding());
                            String path = builder.getPath().substring(httpRequest.getContextPath().length());
                            builder.setPath(httpRequest.getContextPath() + path.substring(defaultDBKey.length() + 1));
                            httpResponse.sendRedirect(httpResponse.encodeRedirectURL(builder.build(true)));
                            return;
                        }

                        // we have to check if requestedDBKey is a valid content
                        // database - if not we use defaultDatabase
                        if (!_core.getContentdbs().containsKey(requestedDBKey.toLowerCase())) {
                            requestedDBKey = defaultDBKey;
                            httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
                        }

                    }
                    if (!requestedDBKey.equalsIgnoreCase("login") && !httpRequest.getMethod().equalsIgnoreCase("post")) {
                        if (!isDBKeyAllowed(_core.getWgaConfiguration(), vHost, requestedDBKey)) {
                            httpResponse.sendError(HttpServletResponse.SC_NOT_FOUND, "Resource '" + requestedDBKey + "' is unknown for the requested host.");
                            ProblemOccasion occ = new PathDispatchingOccasion(httpRequest, requestedDBKey);
                            _core.getProblemRegistry().addProblem(Problem.create(occ, "dispatching.vhostdenial#" + httpRequest.getRequestURI(), ProblemSeverity.LOW, Problem.var("vhost", vHost.getServername())));
                            return;
                        }
                    }

                }
            }
        }

        chain.doFilter(httpRequest, httpResponse);
    }

    private VirtualResource findVirtualResource(VirtualHost vHost, String path) {
        for (VirtualResource resource : vHost.getVirtualResources()) {
            if (resource.getName().equalsIgnoreCase(path)) {
                return resource;
            }
        }
        return null;
    }
    
    private boolean isRootResource(VirtualHost vHost, String path) {
        return findVirtualResource(vHost, path) != null;
    }

    public static VirtualHost findMatchingHost(WGAConfiguration config, ServletRequest request) {
        List<VirtualHost> vHosts = config.getVirtualHosts();        
        if (vHosts != null && !vHosts.isEmpty()) {
            String serverName = IDN.toUnicode(request.getServerName());
            return findMatchingHost(vHosts, serverName);
        }
        return null;        
    }

    public static VirtualHost findMatchingHost(List<VirtualHost> vHosts, String serverName) {
        for (VirtualHost vHost : vHosts) {
            if (vHost.isEnabled()) {
                if (Pattern.matches(convertToRegExp(vHost.getServername()), serverName)) {
                    return vHost;
                }
                else {
                    for (String alias : vHost.getServerAliases()) {
                        if (Pattern.matches(convertToRegExp(alias), serverName)) {
                            return vHost;
                        }
                    }
                }
            }
        }
        
        return null;
    }
    
    private static String convertToRegExp(String pattern) {
        StringBuffer regExp = new StringBuffer();
        StringTokenizer tokenizer = new StringTokenizer(pattern, "*", true);
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            if (token.equals("*")) {
                regExp.append(".*");
            } else {
                regExp.append("\\Q" + token + "\\E");
            }
        }
        return regExp.toString();
    }

    public static boolean isDBKeyAllowed(WGAConfiguration config, VirtualHost vHost, String dbkey) {
        return retrievePriorityForDatabase(config, vHost, dbkey) != -1;
    }
    
    /**
     * computes a priority flag for the given vHost and dbkey
     * @param config
     * @param vHost
     * @param dbkey
     * @return PRIORITY flag for the host, -1 == not allowed, 0 == is default database of host or index (1..n) in allowed dbs 
     */
    public static int retrievePriorityForDatabase(WGAConfiguration config, VirtualHost vHost, String dbkey) {

        // first try - check if default db is requested
        if (vHost.getDefaultDatabase() != null) {
            ConfigBean bean = (ConfigBean) config.getByUid(vHost.getDefaultDatabase());
            if (bean != null && bean instanceof ContentDatabase) {
                ContentDatabase database = (ContentDatabase) bean;
                if (dbkey.equalsIgnoreCase(database.getKey())) {
                    return 0;
                }
            }
        }
        
        // second try - check allowed db keys
        int i = 0;
        for (String uid : vHost.getAllowedDatabases()) {
            i++;
            if (uid.equals(VirtualHost.UID_ALL_DATABASES)) {
                return Integer.MAX_VALUE;
            }
            ConfigBean dbBean = (ConfigBean) config.getByUid(uid);
            if (dbBean != null && dbBean instanceof ContentDatabase) {
                ContentDatabase database = (ContentDatabase) dbBean;
                if (dbkey.equalsIgnoreCase(database.getKey())) {
                    return i;
                }       
            }
        }        
        
        return -1;
    }
    
    /**
     * returns the virtual host with the highest priority for the given dbkey or null if no host for the db is defined
     * hosts containing wildcards in the servername are excluded bc. we cannot generate an absolute URL for these hosts
     * @param config
     * @param dbkey
     * @return
     */    
    public static VirtualHost findPreferredHostForDatabase(WGAConfiguration config, String dbkey) {
       int priority = -1;
       VirtualHost preferredHost = null;
       for (VirtualHost vHost : config.getVirtualHosts()) {
           if (vHost.isEnabled() && !vHost.getServername().contains("*")) {
               int currentPrio = retrievePriorityForDatabase(config, vHost, dbkey);
               if (currentPrio != -1) {
                   if (priority == -1 || (priority > currentPrio)) {
                       priority = currentPrio;
                       preferredHost = vHost;
                   }
               }
           }
       }
       return preferredHost;
    }

    public List<String> getBlackListURLPatterns() {        
        return BLACK_LIST;
    }

    public List<String> getWhiteListURLPatterns() {        
        return WHITE_LIST;
    }
    
    public static String getDefaultDBKey(WGACore core, VirtualHost vHost) {
        String defaultDBKey = null;
        if (vHost.getDefaultDatabase() != null) {
            ConfigBean bean = (ConfigBean) core.getWgaConfiguration().getByUid(vHost.getDefaultDatabase());
            if (bean != null && bean instanceof ContentDatabase) {
                ContentDatabase database = (ContentDatabase) bean;
                defaultDBKey = database.getKey();
            }
        }
        return defaultDBKey;
    }
}
