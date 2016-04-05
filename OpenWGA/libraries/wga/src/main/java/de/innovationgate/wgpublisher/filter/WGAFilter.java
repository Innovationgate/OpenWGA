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
import java.security.cert.CertificateEncodingException;
import java.security.cert.X509Certificate;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.WeakHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import javax.security.auth.x500.X500Principal;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.ArrayUtils;
import org.mockito.Mockito;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGCookie;
import de.innovationgate.wgpublisher.WGPDeployer;
import de.innovationgate.wgpublisher.log.WGALoggerWrapper;
import de.innovationgate.wgpublisher.log.WGARequestInformation;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.LongRequestProblemType;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.sessions.AbstractWGAHttpSessionManager;
import de.innovationgate.wgpublisher.sessions.WGAHttpSession;

public class WGAFilter implements Filter {
    

    private static final String COOKIE_NAME_WGSESSION = "WGSESSIONID";
    private static final String COOKIE_NAME_WGSECURESESSION = "WGSECURESESSIONID";
    private static final String COOKIE_NAME_LBROUTE = "WGLBROUTE";
	
	public static final String REQATTRIB_ORIGINAL_URI = WGAFilter.class.getName() + ".originalRequestURI";
	public static final String REQATTRIB_ORIGINAL_URL = WGAFilter.class.getName() + ".originalRequestURL";
	public static final String REQATTRIB_ORIGINAL_QUERYSTRING = WGAFilter.class.getName() + ".originalQueryString";
	public static final String REQATTRIB_REQUEST_WRAPPER = WGAFilter.class.getName() + ".requestWrapper";
	public static final String REQATTRIB_RESPONSE_WRAPPER = WGAFilter.class.getName() + ".responseWRapper";
	public static final String SESATTRIB_MOCK_CERT = WGAFilter.class.getName() + ".mockCert";
    public static final long REQUEST_LENGTH_NOTIFICATION_THRESHOLD = 1000 * 10;

    private WGACore _core;
	private WGAFilterChain _wgaFilterChain;
    private ServletContext _servletContext;
    private Map<ServletRequest, WGARequestInformation> _currentRequests = new WeakHashMap<ServletRequest, WGARequestInformation>();
    
    private ReadWriteLock _holdRequestsLock = new ReentrantReadWriteLock();
    
    public static class FilterRequestOccasion implements ProblemOccasion {

        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return LongRequestProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return WGAFilter.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return false;
        }
        
        @Override
        public int hashCode() {
            return getClass().hashCode();
        }
        
        @Override
        public boolean equals(Object obj) {
            return (obj instanceof FilterRequestOccasion);
        }
        
        
        
    }

    
    /**
     * wrapper for HttpServletResponse
     * - ignores all calls to setCharacterEncoding
     * - replace parameter charset=<XY> during setContentType with <wrappedResponse>.getCharacterEncoding()
     * Therefore the output encoding can be final set on the original response. 
     *
     */
    public class FinalCharacterEncodingResponseWrapper extends HttpServletResponseWrapper {

        public static final String SYSPROP_ENCODING_ON_CONTENTTYPE = "de.innovationgate.wga.encoding.oncontenttype";

    	private boolean _encodingOnContentType = false;

		private int _statusCode = HttpServletResponse.SC_OK;

		private String _statusMessage;


    	
        public FinalCharacterEncodingResponseWrapper(RequestWrapper reqWrapper, HttpServletResponse arg0) {
            super(arg0);
            _encodingOnContentType = Boolean.getBoolean(SYSPROP_ENCODING_ON_CONTENTTYPE);
        }
        
        public void setCharacterEncoding(String arg0) {
            // ignore
        }
        
        public void setContentType(String arg0) {
            String contentType = arg0;
            if (contentType != null) {
                if (contentType.indexOf(";") != -1) {
                    List<String> parameters = WGUtils.deserializeCollection(contentType, ";");
                    boolean charsetParamSet = false;
                    for (int i=1; i < parameters.size(); i++) {
                        String parameter = (String) parameters.get(i);
                        parameter = parameter.trim();
                        if (parameter.toLowerCase().startsWith("charset")) {
                            parameters.set(i, "charset=" + getResponse().getCharacterEncoding());
                            charsetParamSet = true;
                        }
                    }
                    
                    
                    if (_encodingOnContentType && !charsetParamSet && contentType.toLowerCase().trim().startsWith("text/")) {
	                    // this is for a websphere bug
	                    // if character encoding is set before content type and content type
	                    // is given without charset websphere resets the charset to platform default
	                    // therefore we ensure here that all of our content types have the correct 
	                    // charset parameter
	                    parameters.add("charset=" + getResponse().getCharacterEncoding());
                    }
                    contentType = WGUtils.serializeCollection(parameters, ";");
                 } else if (_encodingOnContentType && contentType.toLowerCase().trim().startsWith("text/")) {                	 
                	 // see above about websphere bug
                	contentType += ";charset=" + getResponse().getCharacterEncoding(); 
                 }                 
           }
           getResponse().setContentType(contentType);
        }

		@Override
		public void sendError(int sc, String msg) throws IOException {
			super.sendError(sc, msg);
			_statusCode = sc;
			_statusMessage = msg;
		}

		@Override
		public void sendError(int sc) throws IOException {
			super.sendError(sc);
			_statusCode = sc;
			_statusMessage = null;
		}

		@SuppressWarnings("deprecation")
        @Override
		public void setStatus(int sc, String sm) {
			super.setStatus(sc, sm);
			_statusCode = sc;
			_statusMessage = sm;
		}

		@Override
		public void setStatus(int sc) {
			super.setStatus(sc);
			_statusCode = sc;
			_statusMessage = null;
		}

		public int getStatusCode() {
			return _statusCode;
		}

		public String getStatusMessage() {
			return _statusMessage;
		}

		@Override
		public void sendRedirect(String location) throws IOException {
			super.sendRedirect(location);
			setStatus(SC_MOVED_TEMPORARILY);
		}



//        @Override
//        public String encodeRedirectUrl(String url) {
//            return encodeRedirectURL(url);
//        }
//        
//
//        @Override
//        public String encodeUrl(String url) {
//            return encodeRedirectURL(url);
//        }
//
//        @Override
//        public String encodeURL(String url) {
//            if (_core.getHttpSessionManager() == null) {
//                return super.encodeURL(url);
//            }
//            
//            HttpSession session = _reqWrapper.getSession(false);
//            if (session != null && session.isNew()) {
//                return url + ";jsessionid=" + session.getId();
//            }
//            return url;
//        }
//
//
//        @Override
//        public String encodeRedirectURL(String url) {
//            if (_core.getHttpSessionManager() == null) {
//                return super.encodeRedirectUrl(url);
//            }
//            
//            return encodeURL(url);
//        }
        
    }
    
    /**
     * a wrapper for WGA requests
     * - supports decoding of url parameters (parameters of the querystring) in the correct encoding
     * - supports setting defaults for url parameters if they are empty (not submitted)
     *
     */
    public class RequestWrapper extends HttpServletRequestWrapper {


        

        private Map<String,String[]> _parameters = new HashMap<>();
		
		private HttpServletRequest _wrappedRequest;
        private HttpServletResponse _response;

        private WGAHttpSession _wgaHttpSession;

		public RequestWrapper(HttpServletRequest request, HttpServletResponse reponse) {			
			super(request);
			_wrappedRequest = request;
			_response = reponse;
		
			String queryString = request.getQueryString();
			// decode query string parameters with wga character encoding
			if (queryString != null) {
				StringTokenizer keyValuePairs = new StringTokenizer(queryString, "&");
				while (keyValuePairs.hasMoreTokens()) {
					String keyValuePair = keyValuePairs.nextToken();
					
					String key = null;
					String value = null;
					int pos = keyValuePair.indexOf("=");
					if (pos != -1) {
						key = keyValuePair.substring(0, pos);
						if (pos  < keyValuePair.length() - 1) {
							value = keyValuePair.substring(pos + 1);
						} else {
							value = "";
						}
						
						try {
							String keyDecoded = _core.getURLEncoder().decode(key);
							String valueDecoded = _core.getURLEncoder().decode(value);
							String values[] = (String[]) _parameters.get(keyDecoded);
							values = (String[])ArrayUtils.add(values, valueDecoded);
							_parameters.put(keyDecoded, values);
						} catch (Exception e) {
							WGFactory.getLogger().warn("Unable to decode request parameter '" + key + "'.", e);
						}
					}
											
				}
			}

		}

        public String getParameter(String name) {
			copyNoneExistingParams(_wrappedRequest.getParameterMap());
			String values[] = (String[]) _parameters.get(name);
			if (values == null || values.length == 0) {
				return null;
			} else {
				return values[0];
			}
		}

        public Map<String,String[]> getParameterMap() {
			copyNoneExistingParams(_wrappedRequest.getParameterMap());
			return new HashMap<String,String[]>(_parameters);
		}

		public String[] getParameterValues(String name) {
			copyNoneExistingParams(_wrappedRequest.getParameterMap());
			return (String[]) _parameters.get(name);
		}

    	public Enumeration<String> getParameterNames() {
    		copyNoneExistingParams(_wrappedRequest.getParameterMap());
    		return Collections.enumeration(_parameters.keySet());
		}
    	
    	public void setParameterIfEmpty(String name, String value) {
    		String[] values = getParameterValues(name);
    		if (values == null || values.length == 0 || (values.length == 1 && values[0].equals(""))) {    		
    			String valueArr[] = new String[1];
    			valueArr[0] = value;
    			_parameters.put(name, valueArr);
    		}
    	}
    	
    	public void setParameterIfEmpty(String name, String[] values) {
    		String[] existingValues = getParameterValues(name);
    		if (existingValues == null || existingValues.length == 0 || (existingValues.length == 1 && existingValues[0].equals(""))) {   
    			_parameters.put(name, values);
    		}
    	}
    	
		private void copyNoneExistingParams(Map<String,String[]> from) {
			// add all parameters from the request to local map, which where not already parsed from querystring or added via setParameterIfEmpty
			// necessary for jsp-param parsing
			HashMap<String,String[]> temp = new HashMap<>(from);
			Iterator<String> localParams = _parameters.keySet().iterator();
			while (localParams.hasNext()) {
				String localParam = (String) localParams.next();
				temp.remove(localParam);	
			}
			_parameters.putAll(temp);
		}
		
		@Override
		public HttpSession getSession() {
		    return getSession(true);
		}
		

        @Override
        public HttpSession getSession(boolean create) {
            if (_core.getHttpSessionManager() == null) {
                return super.getSession(create);
            }
            
            if (_wgaHttpSession == null) {
                AbstractWGAHttpSessionManager manager = _core.getHttpSessionManager();            
                String requestedSessionID = getRequestedSessionId();
                getOrCreateManagedHttpSession(create, manager, requestedSessionID);
            }
            return _wgaHttpSession;            
        }

        private void getOrCreateManagedHttpSession(boolean create, AbstractWGAHttpSessionManager manager, String requestedSessionID) {
            
            // Try to fetch existing session for given ID
            if (requestedSessionID != null) {                               
                _wgaHttpSession = manager.getSession(requestedSessionID);
                if (_wgaHttpSession != null) {
                    _wgaHttpSession.setNew(false);
                    // check if session id has changed - might be the case on replicated sticky sessions where jvmRoute changed due to a cluster failover
                    if (!_wgaHttpSession.getId().equals(requestedSessionID)) {
                        if (manager.isDebug()) {
                            _core.getLog().info("session manager has changed session id for existing session '" + requestedSessionID + "' -> '" + _wgaHttpSession.getId() + "' - updating cookie information");
                        }
                        addSessionCookie(_wgaHttpSession.getId());    
                    }
                    return;
                } 
            }

            // Creating new session, cancel if not allowed
            if (!create) {
                return;
            }

            if (manager.isDebug()) {
                _core.getLog().info("no session id yet - creating new session");
            }
            _wgaHttpSession = manager.createSession();

            // For a secure request check if an unsecure session is available and push existing session data to new session
            if (_wrappedRequest.isSecure()) {
                String requestedUnSecureSessionID = getRequestedUnsecureSessionId();
                if (requestedUnSecureSessionID != null) {
                    WGAHttpSession unsecureSession = manager.getSession(requestedUnSecureSessionID);
                    if (unsecureSession != null) {
                        if (manager.isDebug()) {
                            _core.getLog().info("existing unsecure session found with id '" + requestedUnSecureSessionID + "' - pushing data to new secure session");
                        }
                        unsecureSession.pushData(_wgaHttpSession);
                    }
                }
            }
            
            // Write session cookie
            addSessionCookie(_wgaHttpSession.getId());
            
        }
                        
        @Override
        public boolean isRequestedSessionIdValid() {
            if (_core.getHttpSessionManager() == null) {
                return _wrappedRequest.isRequestedSessionIdValid();
            }
            
            WGAHttpSession session = _core.getHttpSessionManager().getSession(getRequestedSessionId());
            return session != null;
        }
        
        

        @Override
        public String getRequestedSessionId() {
            if (_core.getHttpSessionManager() == null) {
                return _wrappedRequest.getRequestedSessionId();
            }

            Cookie[] cookies = _wrappedRequest.getCookies();            
            if (cookies != null) {
                for (Cookie cookie : cookies) {
                    
                    if (cookie.getName().equals(getSessionCookieName())) {
                        return cookie.getValue();
                    }
                }
            }
            return null;
        }
        
        private String getSessionCookieName() {
            if (_wrappedRequest.isSecure()) {
                return COOKIE_NAME_WGSECURESESSION;
            } else {
                return COOKIE_NAME_WGSESSION;
            }
        }
        
        private String getRequestedUnsecureSessionId() {
            Cookie[] cookies = _wrappedRequest.getCookies();            
            if (cookies != null) {
                for (Cookie cookie : cookies) {                    
                    if (cookie.getName().equals(COOKIE_NAME_WGSESSION)) {
                        return cookie.getValue();
                    }
                }
            }
            return null;
        }
        


        @Override
        public boolean isRequestedSessionIdFromCookie() {
            if (_core.getHttpSessionManager() == null) {
                return _wrappedRequest.isRequestedSessionIdFromCookie();
            }
            return true;
        }

        @Override
        public boolean isRequestedSessionIdFromURL() {
            if (_core.getHttpSessionManager() == null) {
                return _wrappedRequest.isRequestedSessionIdFromURL();
            }
            return false;
        }

        @Override
        public boolean isRequestedSessionIdFromUrl() {
            return isRequestedSessionIdFromURL();
        }

        private void addSessionCookie(String sessionId) {
            if (_wrappedRequest.getAttribute("SESSIONCOOKIE_SET") == null || !(Boolean)_wrappedRequest.getAttribute("SESSIONCOOKIE_SET")) {
                WGCookie sessionCookie = new WGCookie(getSessionCookieName(), sessionId);
                sessionCookie.setPath("/");
                sessionCookie.setMaxAge(-1);
                sessionCookie.setSecure(_wrappedRequest.isSecure());
                sessionCookie.addCookieHeader(_response);
                if (_core.getHttpSessionManager() != null && _core.getHttpSessionManager().isDebug()) {
                    _core.getLog().info("adding '" + getSessionCookieName() + "' " +  sessionCookie.getValue() + " - cookie.path " + sessionCookie.getPath() + " - secure " + sessionCookie.getSecure());
                }
                _wrappedRequest.setAttribute("SESSIONCOOKIE_SET", true);
            }
        }

        public HttpServletRequest getWrappedRequest() {
            return _wrappedRequest;
        }
    }

	/* (Kein Javadoc)
	 * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
	 */
	public void init(FilterConfig arg0) throws ServletException {
	    _servletContext = arg0.getServletContext();
		_core = WGACore.retrieve(_servletContext);
		if (_core != null) {
		    _core.setFilter(this);
		}
		setupFilterChain();
	}

    public void setupFilterChain() {
        
        _holdRequestsLock.writeLock().lock();
        try {
            if (_wgaFilterChain != null) {
                _core.getLog().info("Shutting down filter chain");
                _wgaFilterChain.destroy();
            }
            
            _core.getLog().info("Initializing filter chain");
            _wgaFilterChain = new WGAFilterChain(_core, _servletContext);
        }
        finally {
            _holdRequestsLock.writeLock().unlock();
        }
        
    }

	/* (Kein Javadoc)
	 * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest, javax.servlet.ServletResponse, javax.servlet.FilterChain)
	 */
	public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
		WGARequestInformation info = new WGARequestInformation();
		try {
			info.setStartTime(System.currentTimeMillis());
			info.setThread(Thread.currentThread());
			request.setAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME, info);
			
			_wgaFilterChain.init(request, chain);									
			
			_currentRequests.put(request, info);
			
	        // F000037B2
	        if (_core.getCharacterEncoding() != null) {
	            request.setCharacterEncoding(_core.getCharacterEncoding());
	            // B000041DA
	            response.setCharacterEncoding(_core.getCharacterEncoding());            
	        }
	        
	        HttpServletRequest httpReq = (HttpServletRequest) request;
	        request.setAttribute(REQATTRIB_ORIGINAL_URI, httpReq.getRequestURI());
	        request.setAttribute(REQATTRIB_ORIGINAL_URL, httpReq.getRequestURL());
	        request.setAttribute(REQATTRIB_ORIGINAL_QUERYSTRING, httpReq.getQueryString());
	       
	        // add/ delete jvmRoute
	        String lbRoute = _core.getClusterService().getLBRoute();
	        if (lbRoute != null && !lbRoute.trim().equals("")) {
    	        WGCookie jvmRouteCookie = new WGCookie(COOKIE_NAME_LBROUTE, lbRoute);
    	        jvmRouteCookie.setPath("/");
    	        jvmRouteCookie.setMaxAge(-1);
    	        jvmRouteCookie.addCookieHeader((HttpServletResponse)response);
	        } else {
	           Cookie cookie = getCookie(httpReq, COOKIE_NAME_LBROUTE);
	           if (cookie != null) {
	               WGCookie jvmRouteCookie = WGCookie.from(cookie);
	               jvmRouteCookie.setMaxAge(0);
	               jvmRouteCookie.addCookieHeader((HttpServletResponse)response);   
	           }
	        }
	        
	        RequestWrapper wrappedRequest = createRequestWrapper(response, httpReq);
	        FinalCharacterEncodingResponseWrapper wrappedResponse =  createResponseWrapper(response, wrappedRequest);
	        
	        // Probably mock request certificate
            if (WGACore.isDevelopmentModeEnabled()) {
                mockX509Cert(wrappedRequest);
            }
	        
	        _holdRequestsLock.readLock().lock();
	        try {
	            _wgaFilterChain.doFilter(wrappedRequest, wrappedResponse);
	        }
	        finally {
	            _holdRequestsLock.readLock().unlock();
	        }
	      
	       info.setStatusCode(wrappedResponse.getStatusCode());
	       info.setStatusMessage(wrappedResponse.getStatusMessage());	
	       
	       AbstractWGAHttpSessionManager sessionManager = _core.getHttpSessionManager();
           if (sessionManager != null) {
               sessionManager.requestFinished(wrappedRequest, wrappedResponse);
           }
		}
		catch (ServletException e) {
			info.setStatusCode(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		    info.setStatusMessage(e.getMessage());
		    throw e;
		}
		finally {
			info.setEndTime(System.currentTimeMillis());	
	       try {
				WGALoggerWrapper logger = _core.getAccessLogger();
				   if (logger != null) {
					   logger.logRequest(request);
				   }
			} catch (Exception e) {
				_core.getLog().error("Unable to log request.", e);
			}
			
	       WGFactory.getInstance().closeSessions();
	       
	       if (!Boolean.TRUE.equals(request.getAttribute(WGPDeployer.REQATTRIB_TML_DEPLOYED)) && info.getEndTime() > info.getStartTime() + REQUEST_LENGTH_NOTIFICATION_THRESHOLD) {
	           
	           String uri = ((HttpServletRequest) request).getRequestURI();
	           Problem.Vars vars = Problem
	                   .var("reqinfo", info)
	                   .var("uri", uri);
	           
	           String problemKey = "requestProblem.longRequest#" + uri;
	           
	           if (info.getDatabase() != null) {
	               _core.getProblemRegistry().addProblem(Problem.create(new FilterRequestOccasion(), new DatabaseScope(info.getDatabase().getDbReference()), problemKey, ProblemSeverity.LOW, vars));
	           }
	           else {
	               _core.getProblemRegistry().addProblem(Problem.create(new FilterRequestOccasion(), problemKey, ProblemSeverity.LOW, vars));
	           }
	       }
	       
	       // close opened lucene-resultsets
	       LuceneManager luceneManager = _core.getLuceneManager();
	       if (luceneManager != null) {
	           luceneManager.closeOpenedResultSets();
	       }	       	       
	       
	       _currentRequests.remove(request);
       }
	}

    public void mockX509Cert(HttpServletRequest request) {
        X509Certificate cert = (X509Certificate) request.getSession().getAttribute(WGAFilter.SESATTRIB_MOCK_CERT);
        if (cert == null) {
            String mockCert = request.getParameter("mockX509Cert");
            if (mockCert != null) {
                try {
                    cert = Mockito.mock(X509Certificate.class);
                    X500Principal principal = new X500Principal(mockCert);
                    Mockito.when(cert.getSubjectDN()).thenReturn(principal);
                    Mockito.when(cert.getSubjectX500Principal()).thenReturn(principal);
                    Mockito.when(cert.getEncoded()).thenReturn(new byte[] {1,1,1});
                    request.getSession().setAttribute(WGAFilter.SESATTRIB_MOCK_CERT, cert);
                }
                catch (CertificateEncodingException e) {
                    _core.getLog().error("Exception mocking X509 certificate", e);
                }
            }
        }
        
        if (cert != null) {
            request.setAttribute("javax.servlet.request.X509Certificate", new X509Certificate[] {cert});
        }
    }

    protected FinalCharacterEncodingResponseWrapper createResponseWrapper(ServletResponse response, RequestWrapper wrappedRequest) {
        FinalCharacterEncodingResponseWrapper responseWrapper =  new FinalCharacterEncodingResponseWrapper(wrappedRequest, (HttpServletResponse)response);
        wrappedRequest.getWrappedRequest().setAttribute(REQATTRIB_RESPONSE_WRAPPER, responseWrapper);
        return responseWrapper;
    }

    protected RequestWrapper createRequestWrapper(ServletResponse response, HttpServletRequest httpReq) {
        RequestWrapper wrapper = new RequestWrapper(httpReq, (HttpServletResponse)response);
        httpReq.setAttribute(REQATTRIB_REQUEST_WRAPPER, wrapper);
        return wrapper;
    }
	

	/* (Kein Javadoc)
	 * @see javax.servlet.Filter#destroy()
	 */
	public void destroy() {
	    _wgaFilterChain.destroy();
	}

    public Map<ServletRequest, WGARequestInformation> getCurrentRequests() {
        return _currentRequests;
    }

    private Cookie getCookie(HttpServletRequest request, String name) {
        Cookie[] cookies = request.getCookies();
        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if (cookie.getName().equals(name)) {
                    return cookie;
                }
            }
        }
        return null;
    }
    
    public void initFilterChain() {
        
        Executors.newSingleThreadExecutor().execute(new Runnable() {
            @Override
            public void run() {
                setupFilterChain();
                
            }
        });
        
    }

}
