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
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
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

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import de.innovationgate.utils.URLBuilder;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.config.ConfigBean;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.config.VirtualHostRedirect;
import de.innovationgate.wga.config.VirtualResource;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.WGPDispatcher.PathDispatchingOccasion;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.url.TitlePathManager;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;

public class WGAVirtualHostingFilter implements Filter , WGAFilterURLPatternProvider {
    
    public static final String REQUESTATTRIB_VIRTUAL_HOST = WGAVirtualHostingFilter.class.getName() + ":VirtualHost";
    public static final Logger LOG = Logger.getLogger("wga.vhost");
    
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
        BLACK_LIST.add("/ajaxform*");
        BLACK_LIST.add("/tempdwn*");
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
        LOG.setLevel(Level.INFO);
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

            String uri = httpRequest.getServletPath();
            int semiPos = uri.indexOf(";");
            if (semiPos != -1) {
                uri = uri.substring(0, semiPos);
            }

        	// check if redirects required
        	URLBuilder redirectUrlBuilder = new URLBuilder(httpRequest);
        	boolean redirectRequired = false;
        	boolean isPermanentRedirect = false;
        	boolean forwardRequest=false;

        	// set redirect path
        	Redirect redirect = findRedirect(vHost, request.getServerName(), uri);
            if(redirect!=null && !uri.equalsIgnoreCase(redirect.getPath())){
            	if(redirect.isForward()){
            		LOG.debug("v-host " + vHost.getServername() + ": Forward '" + uri + "' to path: " + redirect.getPath());
            		httpRequest.setAttribute(WGAFilterChain.FORWARD_URL, redirect.getPath());
            		forwardRequest=true;
            	}
            	else {
            		LOG.debug("v-host " + vHost.getServername() + ": Redirect '" + uri + "' to path: " + redirect.getPath());
            		String url = redirect.getPath();
            		if(!url.startsWith("/")){
            			// redirect to some external URL
                		if(isPermanentRedirect){
                    		httpResponse.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
                    		httpResponse.setHeader("Location", httpResponse.encodeRedirectURL(url));        			
                		}
                		else httpResponse.sendRedirect(httpResponse.encodeRedirectURL(url));
                		return;            			
            		}
            		redirectRequired = true;
            		redirectUrlBuilder.setPath(redirect.getPath());
            		isPermanentRedirect = redirect.isPermanentRedirect();
            	}
            }
            
        	// set redirect host
        	if(vHost.isForceDefaultHost() && !request.getServerName().equalsIgnoreCase(vHost.getServername())){
        		LOG.debug("v-host: Redirect '" + request.getServerName() + "' to default host: " + vHost.getServername());
    			redirectRequired = true;
    			redirectUrlBuilder.setHost(vHost.getServername());
    		}
        	
        	// set redirect protocol
            if(vHost.isForceSSL() && !request.isSecure()){
            	LOG.debug("v-host " + vHost.getServername() + ": Redirect to use SSL");
            	redirectRequired = true;
            	redirectUrlBuilder.setProtocol("https");
            }
            
        	if(redirectRequired){
        		if(isPermanentRedirect){
            		httpResponse.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
            		httpResponse.setHeader("Location", httpResponse.encodeRedirectURL(redirectUrlBuilder.build(true)));        			
        		}
        		else httpResponse.sendRedirect(httpResponse.encodeRedirectURL(redirectUrlBuilder.build(true)));
        		return;
        	}

            if (uri.equalsIgnoreCase("/robots.txt") && findVirtualResource(vHost, "robots.txt")==null){
            	response.getWriter().print(vHost.getRobotsTxt());
            	return;
            }

            // check for virtual root resource request (old style)
            String resource_path = uri;
            if(resource_path.startsWith("/"))
            	resource_path = resource_path.substring(1);
            VirtualResource resource = findVirtualResource(vHost, resource_path);
            if(resource!=null){
            	httpRequest.setAttribute(WGAFilterChain.FORWARD_URL, resource.getPath());
            	forwardRequest=true;
            }
            
            if(!forwardRequest){
                // determine default database key
                String defaultDBKey = getDefaultDBKey(_core, vHost);
                
                String[] pathElements = uri.split("/");
                if (pathElements == null || pathElements.length < 1) {
                    // root url request - redirect to default database or hide db
                    if (defaultDBKey != null) {
                    	
                        if (vHost.isHideDefaultDatabaseInURL()) {
                        	if(vHost.isHideHomepageURL()){
								try {
		                            WGDatabase db = (WGDatabase) _core.getContentdbs().get(defaultDBKey);
		                            if(db==null){
		                            	_core.getLog().error("v-host '" + vHost.getServername() + "': default app '" + defaultDBKey + "' is not connected.");
										// Generate 404 for the user:
										httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
		                            }
		                            else{
			                            WGAURLBuilder urlBuilder = _core.retrieveURLBuilder(httpRequest, db);	 
			                            String homepage = urlBuilder.buildHomepageURL(db, httpRequest);
			                            homepage = homepage.substring(httpRequest.getContextPath().length());
										homepage = "/" + defaultDBKey + homepage;
										/*
										 * #00005217
										 * forwarding must be done after all filters had a change to get the request.
										 * Otherwise we might have problems f. e. with request based authentications. 
										 * Therefore we put an attribute to the request and let WGAFilterChain.doFilter() do the job. 
										 */
										httpRequest.setAttribute(WGAFilterChain.FORWARD_URL, homepage);
		                            }
								} catch (Exception e) {
									_core.getLog().error("v-host '" + vHost.getServername() + "': unable to proxy homepage", e);
									// fallback:
									httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
								}    
                        	}
							else httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
							
                        }
                        else {
                            httpResponse.sendRedirect(httpResponse.encodeRedirectURL(httpRequest.getContextPath() + uri + defaultDBKey));
                            return;
                        }
                    }
                }
                else {
                    // normal db request
                    String requestedDBKey = pathElements[1];
                    if (vHost.isHideDefaultDatabaseInURL() && defaultDBKey != null && !requestedDBKey.startsWith("plugin-")) {
                    	// we need to know if requestedDBKey really is a DBKEY or part of a content path in a title path url 
                    	boolean hasValidTitlePath = false;
                    	try{
                        	WGDatabase database = _core.getContentdbs().get(defaultDBKey);
                        	if(database!=null){
		                    	database = _core.openContentDB(database, httpRequest, false);
		                		TitlePathManager tpm = (TitlePathManager) database.getAttribute(WGACore.DBATTRIB_TITLEPATHMANAGER);
		                		if (tpm != null && tpm.isGenerateTitlePathURLs() && database.isSessionOpen()) {
		                			ArrayList<String> elements = new ArrayList<String>(Arrays.asList(pathElements));
		                			elements.remove(0); // remove empty first element produced by "/"
		                			TitlePathManager.TitlePath title_path_url = tpm.parseTitlePathURL(elements);
		                			if(title_path_url!=null && title_path_url.getStructKey()!=null){
		                				String key = title_path_url.getStructKey();
		                				WGStructEntry entry = database.getStructEntryByKey(key);
		                				if(entry==null){
		                					// may be a sequence?
			                            	try{
			                	            	long seq = Long.parseLong(key, 16);
			                	            	entry = database.getStructEntryBySequence(seq);
			                            	}
			                            	catch(Exception e){
			                            		// may be unable to parse structkey as long. Ignore any errors here.
			                            	}
		                				}
		                				if(entry!=null){
			                				// we have a structkey or sequence pointing to a content in this database
			                				// requestedDBKey should be ignored in this case: it's part of the title path
			                                hasValidTitlePath = true;
		                				}
		                			}
		                		}
                        	}
						} catch (WGException e) {
							_core.getLog().info("Unable to determine title path", e);
						}
                    	
            		    if (hasValidTitlePath) {
            		    	// valid title path for default db of this host
            		    	//_core.getLog().info("valid title path: " + uri);
            		    	requestedDBKey = defaultDBKey;
            		    	httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
            		    }

            		    else {
            		    	// requestedDBKey really is a dbkey
            		    	if (requestedDBKey.equalsIgnoreCase(defaultDBKey)) {
	                            // if default db requested redirect to url without dbkey
	                            URLBuilder builder = new URLBuilder(httpRequest, _core.getCharacterEncoding());
	                            builder.setEncoding(_core.getCharacterEncoding());
	                            String path = builder.getPath().substring(httpRequest.getContextPath().length());
	                            builder.setPath(httpRequest.getContextPath() + path.substring(defaultDBKey.length() + 1));
	                            httpResponse.sendRedirect(httpResponse.encodeRedirectURL(builder.build(true)));
	                            return;
	                        }
	            		    
	                        // we have to check if requestedDBKey is a valid content database
	                        // - if not we use defaultDatabase
	                        if (!_core.getContentdbs().containsKey(requestedDBKey.toLowerCase())) {
	                            requestedDBKey = defaultDBKey;
	                            httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
	                        }
            		    }

                    }
                    
                    // Bug #00005171
                    //if (!requestedDBKey.equalsIgnoreCase("login") && !httpRequest.getMethod().equalsIgnoreCase("post")) {
                    // I believe the above code is logically meant as
                    //	if (!(requestedDBKey.equalsIgnoreCase("login") && httpRequest.getMethod().equalsIgnoreCase("post")))
                    //	-> if not a post to /login
                    // as effect ALL post requests are not handled here wich leads to unwanted behaviour.
                    // however this case is already tested at the beginning of the method so it's not nessessarry to test it again here.
                    // We leave the test to /login
                    
                    if (!requestedDBKey.equalsIgnoreCase("login")) {
                    	if (!isDBAllowed(vHost, requestedDBKey)) {
                        	if(defaultDBKey != null)
                        		httpRequest = new DefaultDBRequestWrapper(_core, httpRequest, defaultDBKey);
                        	else{
	                            httpResponse.sendError(HttpServletResponse.SC_NOT_FOUND, "Resource '" + requestedDBKey + "' is unknown for the requested host.");
	                            ProblemOccasion occ = new PathDispatchingOccasion(httpRequest, requestedDBKey);
	                            _core.getProblemRegistry().addProblem(Problem.create(occ, "dispatching.vhostdenial#" + httpRequest.getRequestURI(), ProblemSeverity.LOW, Problem.var("vhost", vHost.getServername())));
	                            return;
                        	}
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
                        	LOG.debug("v-host: Requested host '" + serverName + "' matches host alias '" + alias + "' of v-host '" + vHost.getServername() + "'");
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

    private boolean isDBAllowed(VirtualHost vHost, String dbkey) {
    	WGDatabase database = _core.getContentdbs().get(dbkey);
    	if(database!=null){
	        boolean adminApp = database.getBooleanAttribute(WGACore.DBATTRIB_ADMIN_APP, false);
	        if (adminApp && !vHost.isAllowAdminApps()) {
	        	return false;
	        }
	        boolean authoringApp = database.getBooleanAttribute(WGACore.DBATTRIB_AUTHORING_APP, false);
	        if (authoringApp && !vHost.isAllowAuthoringApps()) {
	        	return false;
	        }
    	}
    	if(dbkey.startsWith("plugin-"))
    		return true;	// allow all other plugins
        return isDBKeyAllowed(_core.getWgaConfiguration(), vHost, dbkey);
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

    private class Redirect{
    	boolean _forward;
    	boolean _permanent;
    	String _url;
    	public Redirect(String url, boolean forward, boolean permanent) {
			_forward = forward;
			_url = url;
			_permanent = permanent;
		}
    	String getPath(){
    		return _url;
    	}
    	boolean isForward(){
    		return _forward;
    	}
    	boolean isPermanentRedirect(){
    		return _permanent;
    	}
    }
    
    private Redirect findRedirect(VirtualHost vHost, String serverName, String path){

    	List<VirtualHostRedirect> redirects = vHost.getRedirects();
    	if(redirects==null || redirects.size()==0)
    		return null;

    	for(VirtualHostRedirect redirect: redirects){
    		if(!redirect.isEnabled())
    			continue;
    		
    		// Check Request Hosts
    		boolean hostMatch = false;
    		List<String> hosts = redirect.getRequestHosts();
    		if(hosts!=null && hosts.size()>0){
    			for(String host: hosts){
    				if (Pattern.matches(convertToRegExp(host), serverName)) {
    					hostMatch=true;
    					break;
    				}
    			}
    		}
    		else hostMatch=true;	// no hosts: any serverName matches
    		
    		if(hostMatch){
    			String redirectPath = redirect.getPath();
    			if(redirectPath.isEmpty())
    				redirectPath = ".*";	// any path
	    		Pattern pattern = Pattern.compile(redirectPath, Pattern.CASE_INSENSITIVE);
	        	Matcher matcher= pattern.matcher(path);
	            if(matcher.find()) {
	            	LOG.debug("v-host " + vHost.getServername() + ": '" + path + "' matches VirtualHostRedirect-Path: " + redirectPath);
	                String redirectURL = redirect.getRedirect();
	                Integer count = matcher.groupCount();
	                if(count > 0) {
	                    for(Integer i = 1; i <= count; i++) {
	                    	redirectURL = redirectURL.replace("$"+i, matcher.group(i));
	                    }
	                }
	                return new Redirect(redirectURL, redirect.isForward(), redirect.isPermanentRedirect());
	            }
    		}
    	}
    	return null;
    }
    
}
