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
package de.innovationgate.wgpublisher;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.httpclient.URIException;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wgpublisher.WGPDispatcher.URLID;
import de.innovationgate.wgpublisher.filter.WGAFilter;
import de.innovationgate.wgpublisher.lang.LanguageBehaviour;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;
import de.innovationgate.wgpublisher.lang.RequestLanguageChooser;
import de.innovationgate.wgpublisher.url.TitlePathManager;
import de.innovationgate.wgpublisher.url.TitlePathManager.RemainingPathElementException;
import de.innovationgate.wgpublisher.url.TitlePathManager.TitlePath;
import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;

public class WGPRequestPath {

	
    public static final String PATHCMD_JOBLOG = "joblog";
    public static final String PATHCMD_WEBTML_DEBUGGER = "tmldebug";
    public static final String PATHCMD_ADMIN_TML = "admintml";
    public static final String PATHCMD_STATIC_TML = "statictml";
    public static final String PATHCMD_STATIC_RESOURCE = "static";
    public static final String PATHCMD_BROWSER_INTERFACE = "bi";
    public static final String PATHCMD_STARTPAGE = "start";
    public static final String PATHCMD_TMLFORM = "tmlform";
    public static final int TYPE_INVALID = -1;
	public static final int TYPE_INVALID_DB = -2;
	public static final int TYPE_UNAVAILABLE_DB = -3;

    public static final int TYPE_UNKNOWN = 0;
	public static final int TYPE_TML = 2;
	public static final int TYPE_RESOURCE = 3;
	public static final int TYPE_FILE = 4;
	public static final int TYPE_TMLFORM = 5;
	public static final int TYPE_CSS = 6;
	public static final int TYPE_REDIRECT = 7;
	public static final int TYPE_STATICTML = 8;
	public static final int TYPE_LOGOUT = 9;
	public static final int TYPE_TMLDEBUG = 10;
	public static final int TYPE_JOBLOG = 11;
    public static final int TYPE_GOTO_HOMEPAGE = 12;
    public static final int TYPE_FAVICON = 13;
    public static final int TYPE_TITLE_PATH = 14;
    public static final int TYPE_UNDEFINED_HOMEPAGE = 15;
    public static final int TYPE_JS = 16;
    public static final int TYPE_UNKNOWN_CONTENT = 17;
    // Working variables
	private URL completeURL = null;
	private String basePath = null;
	private List<String> pathElements = new ArrayList<String>();
	
	// Results
	private int pathType = 0;
	private String databaseKey = null;
	private MediaKey mediaKey = null;
	private String layoutKey = null;
	private String contentKey = null;
	private String cssjsKey = null;
	private String containerKey = null;
	private String fileName = null;
	private String derivateName = null;
	private String resourcePath = null;
	private WGDatabase database = null;
	private String queryString  = null;
	private String publisherURL = null;
	private TitlePathManager.TitlePath titlePathURL = null;
	private boolean appendQueryString = true;
	private boolean completePath = true;
	private String requestLanguage = null;
	private boolean permanentRedirect = false;

    private String pathCommand = null;
    private boolean masterLogin = false;
    private boolean proceedRequest = true;
    private WGACore core;
    private WGContent content;
    
    public static final String PATHCMD_TEMP_DOWNLOAD = "tempdwn";
	
    public static final String REQATTRIB_HTTPLOGIN = "HttpLogin";
    
	protected WGPRequestPath(HttpServletRequest request, HttpServletResponse response, WGPDispatcher dispatcher) throws HttpErrorException, WGException, IOException, URIException {

	    this.core = dispatcher.getCore();
		this.queryString = request.getQueryString();
		this.completeURL = buildCompleteURL(request);
		this.publisherURL = WGPDispatcher.getPublisherURL(request);
		
		// Extract the base part of the path - Redirect to start.jsp if no path information given
		this.basePath = this.getBasePath(request, dispatcher);
		if (this.basePath.equals("") || this.basePath.equals("/")) {
			if (core.getWgaConfiguration().getDefaultDatabase() == null) {
				this.pathType = TYPE_REDIRECT;
				this.resourcePath = this.publisherURL + this.core.getStartPageURL();
				return;
			}
			else {
				this.basePath = "/" + core.getWgaConfiguration().getDefaultDatabase();
			}
		}

		// Tokenize Path
		int tildeTokenPos = -1;
		java.util.StringTokenizer pathTokens = new StringTokenizer(this.basePath, "/");
		String token;
		while (pathTokens.hasMoreTokens()) {
			token = pathTokens.nextToken();
			this.pathElements.add(token);
			if (token.charAt(0) == '~') {
				tildeTokenPos = this.pathElements.size() - 1;
			}
			
		}

		if (this.pathElements.size() < 1) {
			this.pathType = TYPE_INVALID;
			return;
		}

		// Resolve database
		this.databaseKey = ((String) this.pathElements.get(0)).toLowerCase();;
		this.database = (WGDatabase) core.getContentdbs().get(this.databaseKey);
		
        // if no database under this key, try to recognize a special path command
		if (this.database == null) {
            determineSpecialPathCommand();
            if (this.database == null) {
                return;
            }
		}
		
		// Check if we need to enforce secure app mode
		URL secureURL = enforceSecureAppMode(database, request);
		if (secureURL != null) {
		    pathType = TYPE_REDIRECT;
		    resourcePath = secureURL.toString();
		    return;
		}
		
		
		// check if db is accessed via right protocol, host and port - Must be before login so it may get redirected to some certauth port
        URL currentURL = new URL(request.getRequestURL().toString());    
        URL redirectURL = enforceRedirectionRules(database, currentURL);                                    
        
        // currentURL differs from redirectURL - redirect necessary
        if (redirectURL != null && !dispatcher.isBrowserInterface(request.getSession())) {
            pathType = TYPE_REDIRECT;
            resourcePath = redirectURL.toString();
            return;
        }
        
        // Handle special db commands "login" and "logout". The only one not requiring to login to the database
        if (this.pathElements.size() == 2) {
            if ("login".equals(this.pathElements.get(1))) {
                this.pathType = TYPE_REDIRECT;
                String sourceURL =
                    (request.getParameter("redirect") != null
                        ? dispatcher.getCore().getURLEncoder().decode(request.getParameter("redirect"))
                        : WGPDispatcher.getPublisherURL(request) + "/" + this.databaseKey);
                this.resourcePath = dispatcher.getLoginURL(request, database, sourceURL);
                this.appendQueryString = false;
                return;
            }
            else if ("logout".equals(this.pathElements.get(1))) {
                this.pathType = TYPE_LOGOUT;
                
                if (request.getParameter("redirect") != null) {
                    this.resourcePath = request.getParameter("redirect");
                    this.appendQueryString = false;
                }
                else {
                    this.resourcePath = WGPDispatcher.getPublisherURL(request) + "/" + this.databaseKey;
                }
                return;
            }
                
        }

		
		// Open the database
        try {
            if (pathType == TYPE_STATICTML && "admintml".equals(getPathCommand()) && dispatcher.isAdminLoggedIn(request)) {
                this.masterLogin =  true;
            }
            
            
            // Prepare HTTP credentials if available
            String credentials = request.getHeader("Authorization");
            if (credentials != null && credentials.trim().toLowerCase().startsWith("basic")) {
                DBLoginInfo loginInfo = DBLoginInfo.createFromHttpCredentials(credentials);
                if (loginInfo != null) {
                    // Look if ANY media key uses HTTP login. Only if so we accept this login
                    if (isHttpLoginUsed(database)) {
                        request.setAttribute(REQATTRIB_HTTPLOGIN, loginInfo);
                    }
                }
            }
            
            this.database = core.openContentDB(database, request, this.masterLogin);
        }
        catch (WGUnavailableException e) {
            throw new HttpErrorException(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "The website is currently unavailable: " + e.getMessage(), getDatabaseKey());
        }
        catch (de.innovationgate.wgpublisher.AuthenticationException e) {
            throw new HttpErrorException(HttpServletResponse.SC_UNAUTHORIZED, e.getMessage(), null);
        }
        catch (AccessException e) {
            throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, e.getMessage(), null);
        }
        
        
        if (!database.isSessionOpen()) {
            handleLoginFailure(request, response, dispatcher);
            this.proceedRequest = false;
            return;
        }
        
        // If request is static/admin tml we are done here
        if (pathType == TYPE_STATICTML) {
            return;
        }

		// If only database given, go to home page
		if (this.pathElements.size() == 1) {
			    this.pathType = TYPE_GOTO_HOMEPAGE;
		        setPermanentRedirect(core.getWgaConfiguration().isUsePermanentRedirect());
                return;
		}

		// Process tilde tokens
		if (tildeTokenPos != -1) {
			String tildeToken = (String) this.pathElements.get(tildeTokenPos);
            
			// Url to file attachment via ~file-Syntax
			if (tildeToken.equalsIgnoreCase("~file")) {
			    this.pathType = TYPE_FILE;
			    List<String> preTildeTokenElems = this.pathElements.subList(1, tildeTokenPos);
			    this.containerKey = (String) preTildeTokenElems.get(preTildeTokenElems.size() - 1);
			    this.fileName = WGUtils.serializeCollection(this.pathElements.subList(tildeTokenPos + 1, this.pathElements.size()), "/");
			    return;
			}
            
		}

		// Catch special db-related urls
		String elem1 = ((String) this.pathElements.get(1)).toLowerCase();
		int elementsSize = this.pathElements.size();
		if (elementsSize >= 3 && (elem1.equals("css"))) {
			this.pathType = TYPE_CSS;
			this.cssjsKey = this.database.toLowerCaseMeta(WGUtils.serializeCollection(this.pathElements.subList(2, this.pathElements.size()), "/"));
			return;
		}
		else if (elementsSize >= 3 && (elem1.equals("js"))) {
            this.pathType = TYPE_JS;
            this.cssjsKey = this.database.toLowerCaseMeta(WGUtils.serializeCollection(this.pathElements.subList(2, this.pathElements.size()), "/"));
            return;
        }
		else if (elementsSize >= 3 && elem1.equals("file")) {
			this.pathType = TYPE_FILE;
			int fileNameIndex = determineFileNameIndex(this.pathElements, 2);
			this.containerKey = this.database.toLowerCaseMeta(WGUtils.serializeCollection(this.pathElements.subList(2, fileNameIndex), ":"));			
			this.fileName = WGUtils.serializeCollection(this.pathElements.subList(fileNameIndex, elementsSize), "/");			
			return;
		}
		
		// Find out if we have a title path URL
		TitlePathManager tpm = (TitlePathManager) database.getAttribute(WGACore.DBATTRIB_TITLEPATHMANAGER);
		if (tpm != null && tpm.isGenerateTitlePathURLs()) {
		    TitlePathManager.TitlePath url = tpm.parseTitlePathURL(pathElements.subList(1, pathElements.size()));
		    if (url != null) {
    		    this.pathType = TYPE_TITLE_PATH;
    		    this.titlePathURL = url;
    		    this.mediaKey = core.getMediaKey(url.getMediaKey());
    		    if (url.getLanguage() == null) {
    		        completePath = false;
    		    }
		    }
		}
		
		// Path identified as normal TML request, read media and layout key
		if (pathType == TYPE_UNKNOWN) {
    		pathType = TYPE_TML;
            int elementIdx = this.readMediaKey(core, this.pathElements, 1);
    		elementIdx = this.readLayoutKey(core, this.pathElements, elementIdx);
            if (elementIdx < this.pathElements.size()) {
                this.contentKey = this.database.toLowerCaseMeta((String) this.pathElements.get(elementIdx));
    		}
            if (this.layoutKey == null && this.contentKey == null) {
                this.pathType = TYPE_INVALID;
            }
		}
		
		// Retrieve the content
		if (getPathType() == WGPRequestPath.TYPE_TITLE_PATH) {
            this.content = getContentByTitlePath(request);
            if (this.content == null) {
                pathType = TYPE_UNKNOWN_CONTENT;
            }
            
            // If content was retrieved with struct key we check if the title path is correct. If not we force redirection to the correct version (#00003145)
            else if (getTitlePathURL().getStructKey() != null) {
                List<String> correctTitlePath = tpm.buildTitlePath(this.content, mediaKey.getKey(), new RequestLanguageChooser(this.database, request));
                if (!correctTitlePath.equals(getTitlePathURL().getEncodedTitles())) {
                    completePath = false;
                }
            }
            
            // If title path is configured to include keys but the current tp has no key we force redirection to the version including a key (#00003304).
            else if (tpm.isIncludeKeys()) {
                completePath = false;
            }
        }
        else if (getPathType() == TYPE_TML) {
            if (this.contentKey != null) {
                URLID contentid = new URLID(this.contentKey, this.database);
                boolean isBI = WGPDispatcher.isBrowserInterface(request.getSession()) || WGPDispatcher.isAuthoringMode(database.getDbReference(), request.getSession());
                this.content = WGPDispatcher.getContentByAnyKey(contentid, database, new RequestLanguageChooser(database, request), isBI);
                if (this.content != null) {
                    // Look if we really used the parsed content URLID information.
                    if (!contentid.isCompleteFormat()) {
                        completePath = false;
                    }
                    this.requestLanguage = content.getLanguage().getName();
                }
                else {
                    pathType = TYPE_UNKNOWN_CONTENT;
                }
            }
            
            // Contextless request. If we have no request language we have no complete path and we must determine a language
            else {
                if (requestLanguage == null) {
                    completePath = false;
                    LanguageBehaviour langBehaviour = LanguageBehaviourTools.retrieve(database);
                    WGLanguage lang = langBehaviour.requestSelectDatabaseLanguage(database, request);
                    if (lang != null) {
                        this.requestLanguage = lang.getName();
                    }
                    
                    // Fallback to the database default language
                    else {
                        this.requestLanguage = database.getDefaultLanguage();
                    }
                }
            }
        }
	}

    public static URL buildCompleteURL(HttpServletRequest request) throws MalformedURLException, URIException {
        
        StringBuffer url = new StringBuffer();
        url.append(((StringBuffer) request.getAttribute(WGAFilter.REQATTRIB_ORIGINAL_URL)).toString());
        String qs = request.getQueryString();
        if (qs != null) {
            url.append("?").append(qs);
        }
        return new URL(url.toString());
    }

    private int determineFileNameIndex(List<String> pathElements, int startElement) {
        
        // The path element denoting the file name is either the one having ".zip" suffix (for links into the zip archive) or the last element 
        for (int idx=startElement; idx < pathElements.size(); idx++) {
            String elem = pathElements.get(idx);
            if (elem.toLowerCase().endsWith(".zip")) {
                return idx;
            }
        }
        
        return pathElements.size() - 1;
        
    }

    private void handleLoginFailure(HttpServletRequest request, HttpServletResponse response, WGPDispatcher dispatcher) throws HttpErrorException, IOException,
            UnsupportedEncodingException, WGException {

        // Look if there is a login for that domain. If so, user has no
        // access to this db
        if (!this.core.allowLoginRetry(database, request.getSession())) {
            throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, "You have no access to application '" + getDatabaseKey() + "'", getDatabaseKey());
        }

        // Try to login, if no login for this domain
        String dbAttribHttpLogin = (String) database.getAttribute(WGACore.DBATTRIB_HTTPLOGIN);
        if (dbAttribHttpLogin == null) {
            dbAttribHttpLogin = "false";
        }
        
        // Try to determine media key from URL, to see if we should do HTTP Login. This will only work with standard URLs.
        if (this.pathElements.size() >= 2) {
            readMediaKey(core, this.pathElements, 1);
	        if (mediaKey != null && mediaKey.isHttpLogin() || dbAttribHttpLogin.equals("true")) {
	            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
	            response.setHeader("WWW-Authenticate", "Basic realm=\"" + database.getAttribute(WGACore.DBATTRIB_DOMAIN) + "\"");
	            return;
	        }
        }
        else if(dbAttribHttpLogin.equals("true")){
            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            response.setHeader("WWW-Authenticate", "Basic realm=\"" + database.getAttribute(WGACore.DBATTRIB_DOMAIN) + "\"");
            return;        	
        }
        
        // Redirect to login facility
        if (request.getParameter("$ajaxInfo") == null) {
            dispatcher.sendRedirect(response, dispatcher.getLoginURL(request, database, getCompleteURL()));
        } else {
            // this is an ajax call without login information - redirect to jsp and fire event LoginRequired
            String loginRequiredEvent = de.innovationgate.wgpublisher.webtml.portlet.PortletEvent.LOGIN_REQUIRED_EVENT.toJavaScriptObject();
            String encodedEvent = Base64.encodeWeb(loginRequiredEvent.getBytes());
            dispatcher.sendRedirect(response, this.publisherURL + "/fireSystemEvent.jsp?event="  + encodedEvent);
        }
    }

    private void determineSpecialPathCommand() {
        this.pathCommand = databaseKey;
        this.databaseKey = null;
        
        if (this.pathCommand.equalsIgnoreCase(PATHCMD_TMLFORM)) {
            if (this.pathElements.size() >= 3) {
                this.pathType = TYPE_TMLFORM;
                this.containerKey = this.pathElements.get(1);
                this.fileName = this.pathElements.get(2);
            }
            else {
                this.pathType = TYPE_INVALID;
            }
        }
        else if (this.pathCommand.equalsIgnoreCase(PATHCMD_STARTPAGE)) {
        	this.pathType = TYPE_REDIRECT;
        	this.resourcePath = this.publisherURL + core.getStartPageURL();
        }
        else if (this.pathCommand.equalsIgnoreCase("wgadmin") || this.pathCommand.equalsIgnoreCase("admin")) {
        	this.pathType = TYPE_REDIRECT;
        	this.resourcePath = this.publisherURL + "/plugin-admin";
        }
        else if (this.pathCommand.equalsIgnoreCase("wga4admin")) {
            this.pathType = TYPE_REDIRECT;
            this.resourcePath = this.publisherURL + "/wgadmin.jsp";
        }
        else if (this.pathCommand.equals(PATHCMD_BROWSER_INTERFACE) || this.pathCommand.equals(PATHCMD_STATIC_RESOURCE) || this.pathCommand.equals(PATHCMD_TEMP_DOWNLOAD)) {
        	this.pathType = TYPE_RESOURCE;
        	this.resourcePath = this.basePath;
        }
        else if (this.pathCommand.equals(PATHCMD_STATIC_TML) || this.pathCommand.equals(PATHCMD_ADMIN_TML)) {
        	this.pathType = TYPE_STATICTML;
            if (this.pathElements.size() >= 3) {
                this.databaseKey = (String) this.pathElements.get(1);
                this.database = core.getContentdbs().get(this.databaseKey);
                this.resourcePath = "/static/tml/" + ((String) this.pathElements.get(2));
                if (this.pathElements.size() >= 4) {
                    this.contentKey = (String) this.pathElements.get(3);
                }
            } else {
                this.pathType = TYPE_INVALID;
            }
        }
         else if (this.pathCommand.equals(WGACore.WGASERVICES_WSDL_URL)) {
            this.pathType = TYPE_STATICTML;
            this.resourcePath = "/wgaservices";
            this.databaseKey = null;
        }
        else if (this.pathCommand.equals(PATHCMD_WEBTML_DEBUGGER)) {
        	this.pathType = TYPE_TMLDEBUG;
        }
        else if (this.pathCommand.equals(PATHCMD_JOBLOG)) {
        	this.pathType = TYPE_JOBLOG;
        } 
        else if (this.pathCommand.equals("favicon.ico")) {
            this.pathType = TYPE_FAVICON;
        }
        else if (this.pathCommand.equalsIgnoreCase("contentmanager")) {
            this.pathType = TYPE_REDIRECT;
            this.resourcePath = this.publisherURL + "/plugin-contentmanager";
        }
        else {
            this.databaseKey = this.pathCommand;
        	this.pathType = TYPE_INVALID_DB;
        }
    }
	
	private int readMediaKey(WGACore core, List<String> elements, int elementIdx) {
	
			String mediaKeyCandidate = null;
			int contentKeyCutPosition = -1;
			int addToIdx = 0;
			
			mediaKeyCandidate = ((String) elements.get(elementIdx)).toLowerCase();
			addToIdx = 1;
		
			if (core.isMediaKeyDefined(mediaKeyCandidate)) {
				elementIdx += addToIdx;
			}
			else {
				mediaKeyCandidate = null;
				this.completePath = false;
			}
			
			if (mediaKeyCandidate != null) {
			    this.mediaKey = core.getMediaKey(mediaKeyCandidate);
			}
			
			return elementIdx;
			
	}
	
	private int readLayoutKey(WGACore core, List<String> elements, int elementIdx) throws WGAPIException {
		
		if (elementIdx >= elements.size()) {
			return elementIdx;
		}
        
        String designDBKey = core.getDesignDatabaseKey(this.databaseKey);
        WGDatabase originalDesignDB = (WGDatabase) core.getContentdbs().get(designDBKey);
		
		String layoutKeyCandidate = ((String) elements.get(elementIdx)).toLowerCase();
		
		if (layoutKeyCandidate.equals("default")) {
			layoutKeyCandidate = null;
			elementIdx++;
		}
		else {
			String mediaKeyStr = this.mediaKey != null ? this.mediaKey.getKey() : (String) this.database.getAttribute(WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
			String requestLanguageCandidate = null;
			boolean moduleFound = false;
			
            // Try to interpret as complete URLID
	        if (elementIdx == elements.size() - 1) {

	            WGPDispatcher.URLID urlid = new URLID(layoutKeyCandidate, database);
	            if (urlid.getSuffix() != null && urlid.getSuffix().equals(mediaKeyStr)) {
	                if (urlid.getLanguage() != null) {
	                    requestLanguageCandidate = urlid.getLanguage();
	                }
	                moduleFound = isTMLModuleName(originalDesignDB, mediaKeyStr, urlid.getResourceId());
	                if (moduleFound) {
	                    layoutKeyCandidate = urlid.getResourceId();
	                }
	            }
	        }
			
	        // Try again with full layout key candidate
	        if (!moduleFound) {
	            moduleFound = isTMLModuleName(originalDesignDB, mediaKeyStr, layoutKeyCandidate);
	        }
			
			if (moduleFound) {
			    elementIdx++;
			    this.requestLanguage = requestLanguageCandidate;
			}
			else {
                layoutKeyCandidate = null;
                completePath = false;    
			}
			
		}
				
		this.layoutKey = layoutKeyCandidate;
		return elementIdx;
		
	}

    private boolean isTMLModuleName(WGDatabase db, String mediaKeyStr, String layoutKeyCandidate) throws WGAPIException {
        
        if (db == null) {
            return false;
        }
        
        String deploymentKey = WGTMLModule.createDeploymentKey(db.getDbReference(), layoutKeyCandidate, mediaKeyStr);
        
        // We use this as layout key, if there is a layout of this name known to the deployer
        // Or if the design db is not yet connected (what would make it impossible for the deployer to know the layout)
        boolean moduleFound = false;
        if (this.core.isDeploymentKeyDefined(deploymentKey) || (db != null  && !db.isConnected())) {
        	moduleFound = true;
        }
        
        // Try to retrieve the module directly from the open database
        else {
            WGTMLModule module = database.getTMLModule(layoutKeyCandidate, mediaKeyStr);
            if (module != null) {
                moduleFound = true;
            }
            
            WGScriptModule scriptModule = database.getScriptModule(layoutKeyCandidate + ".renderer", WGScriptModule.CODETYPE_TMLSCRIPT);
            if (scriptModule != null) {
                moduleFound = true;
            }
                
        }
        return moduleFound;
    }

	/**
	 * Gets the basePath
	 * @return Returns a String
	 */
	public String getBasePath() {
		return basePath;
	}


	/**
	 * Gets the containerKey
	 * @return Returns a String
	 */
	public String getContainerKey() {
		return containerKey;
	}


	/**
	 * Gets the contentKey
	 * @return Returns a String
	 */
	public String getContentKey() {
		return contentKey;
	}

	/**
	 * Gets the fileName
	 * @return Returns a String
	 */
	public String getFileName() {
		return fileName;
	}

	/**
	 * Gets the queryString
	 * @return Returns a String
	 */
	public String getQueryString() {
		return queryString;
	}


	/**
	 * Gets the layoutKey
	 * @return Returns a String
	 */
	public String getLayoutKey() {
		return layoutKey;
	}



	/**
	 * Gets the pathType
	 * @return Returns a int
	 */
	public int getPathType() {
		return pathType;
	}


	/**
	 * Gets the resourcePath
	 * @return Returns a String
	 */
	public String getResourcePath() {
		return resourcePath;
	}


	/**
	 * Gets the databaseKey
	 * @return Returns a String
	 */
	public String getDatabaseKey() {
		return databaseKey;
	}


	/**
	 * Gets the mediaKey
	 * @return Returns a String
	 */
	public String getMediaKey() {
	    if (mediaKey != null) {
	        return mediaKey.getKey();
	    }
	    else {
	        return null;
	    }
	}
	
	public MediaKey getMediaKeyDefinition() {
	    return mediaKey;
	}


	/**
	 * Returns the complete and original URL that was used to call this request
	 */
	public String getCompleteURL() {
		return completeURL.toExternalForm();
	}


	/**
	 * Gets the cssjsKey
	 * @return Returns a String
	 */
	public String getCssjsKey() {
		return cssjsKey;
	}

	private String getBasePath(javax.servlet.http.HttpServletRequest request, WGPDispatcher dispatcher) throws URIException {
	    
	    // Build basepath from servlet path and path info
        // getServletPath/getPathInfo is decoded by the container. Since WGA encodes Links with UTF-8 (per default)
        // this will lead to wrong results with containers, that decode by some other scheme (e.g. Tomcat5).
        // Encoding of Links is configurable via manager
	    /*StringBuffer pathBuf = new StringBuffer();
	    
	    if (request.getServletPath() != null) {
	        pathBuf.append(request.getServletPath());
	    }

	    if (request.getPathInfo() != null) {
	        pathBuf.append(request.getPathInfo());
	    }
        String path = pathBuf.toString();
        */
        String undecodedPath = request.getRequestURI();
        String contextPath = request.getContextPath();
        if (!contextPath.equals("")) {
            undecodedPath = undecodedPath.substring(contextPath.length());
        }
        String path = dispatcher.getCore().getURLEncoder().decode(undecodedPath);
        
	    int semiPos = path.indexOf(";");
	    if (semiPos != -1) {
	        path = path.substring(0, semiPos);
	    }
	    return path;
        
	    
	}
	/**
	 * Returns the appendQueryString.
	 * @return boolean
	 */
	public boolean appendQueryString() {
		return appendQueryString;
	}
    
    public void setAppendQueryString(boolean appendQueryString) {
        this.appendQueryString = appendQueryString;
    }
	
	public String expandToCompletePath(HttpServletRequest req) throws WGAPIException, UnsupportedEncodingException {
		
	    StringBuffer path = new StringBuffer(this.publisherURL);
	    path.append("/").append(this.databaseKey).append("/");
	    
	    if (this.pathType != TYPE_TITLE_PATH && this.pathType != TYPE_TML) {
	        return null;
	    }
	    
	    // Determine the path to create from the title path configuration of the app (#00004183)
	    TitlePathManager tpm = (TitlePathManager) this.database.getAttribute(WGACore.DBATTRIB_TITLEPATHMANAGER);
	    if (tpm != null && this.content != null && tpm.isTitlePathAllowed(this.content) && (this.mediaKey == null || this.mediaKey.getKey().equals("html")) && (layoutKey == null || layoutKey.equals("default"))) {
	        this.pathType = TYPE_TITLE_PATH;
	    }
	    else {
	        this.pathType = TYPE_TML;
	    }

	    // Title path URL
	    if (this.pathType == TYPE_TITLE_PATH) {
	        
           if (mediaKey == null) {
               mediaKey = core.getMediaKey("html");
           }
           
	        List<String> titlePath = tpm.buildTitlePath(this.content, mediaKey.getKey(), new RequestLanguageChooser(this.database, req));
            if (titlePath != null) {
                path.append(WGUtils.serializeCollection(titlePath, "/"));
            }
            // Cannot build a title path. We use a normal content path instead.
            else {
                this.pathType = TYPE_TML;
            }
        }
	    
	    // Regular URL 
	    if (this.pathType == TYPE_TML) {
    	    MediaKey mediaKey = (this.mediaKey != null ? this.mediaKey : core.getMediaKey((String) this.database.getAttribute(WGACore.DBATTRIB_DEFAULT_MEDIAKEY)));
            String layoutKey = (this.layoutKey != null ? this.layoutKey : "default");
    		
    		path.append(mediaKey.getKey()).append("/");
    		
    		if (this.content != null) {
    	        path.append(layoutKey).append("/");
    	        path.append(WGPDispatcher.buildContentURLID(content, mediaKey.getKey(), WGPDispatcher.isBrowserInterface(req.getSession())));
    		    
    		}
    		else {
    		    path.append(WGPDispatcher.buildLayoutURLID(this.database, layoutKey, this.requestLanguage, mediaKey.getKey()));
    		}
	    }
		
        if (this.queryString != null) {
            path = path.append("?").append(this.queryString);
        }

        return path.toString();
		
	}

	/**
	 * Returns the completePath.
	 * @return boolean
	 */
	public boolean isCompletePath() {
		return completePath;
	}

	/**
	 * Returns the publisherURL.
	 * @return String
	 */
	public String getPublisherURL() {
		return publisherURL;
	}

	/**
	 * Sets the resourcePath.
	 * @param resourcePath The resourcePath to set
	 */
	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}
	
    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return getCompleteURL();
    }

    /**
     * @return Returns the pathCommand.
     */
    public String getPathCommand() {
        return pathCommand;
    }

    protected TitlePathManager.TitlePath getTitlePathURL() {
        return titlePathURL;
    }

    public boolean isPermanentRedirect() {
        return permanentRedirect;
    }

    public void setPermanentRedirect(boolean permanentRedirect) {
        this.permanentRedirect = permanentRedirect;
    }

    public boolean isMasterLogin() {
        return masterLogin;
    }

    public boolean isProceedRequest() {
        return proceedRequest;
    }

    public WGDatabase getDatabase() {
        return database;
    }

    private void testGeneralAccess(javax.servlet.http.HttpServletRequest request) throws HttpErrorException, AdminLoginNeededException {
        
        Boolean updated = (Boolean) database.getAttribute(WGDatabase.ATTRIB_UPDATED);
        if (updated != null) {
            if (updated.booleanValue() == true) {
                throw new HttpErrorException(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "This application is currently being updated. Please try again later.", getDatabaseKey());
            }
        }
        
        boolean allowPublishing = database.getBooleanAttribute(WGACore.DBATTRIB_ALLOW_PUBLISHING, true);
        if (!allowPublishing) {
            throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, "The application '" + database.getDbReference() + "' is not published", null);
        }
    
        boolean authoringApp = database.getBooleanAttribute(WGACore.DBATTRIB_AUTHORING_APP, false);
        if (authoringApp && !core.isAuthoringPort(request.getLocalPort())) {
            throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, "Access to authoring applications is disabled", null);
        }
        
        boolean adminApp = database.getBooleanAttribute(WGACore.DBATTRIB_ADMIN_APP, false);
        if (adminApp) {
            if (!core.isAdministrativePort(request.getLocalPort())) {
                throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, "Access to administrative applications is disabled", null);
            }
            
            if (!core.isAdminLoggedIn(request)) {
                throw new AdminLoginNeededException();
            }
        }
        
        if (this.pathType != TYPE_CSS && this.pathType != TYPE_JS && this.pathType != TYPE_FILE) {
            if (!database.getSessionContext().getUserAccess().mayAccessDirectly()) {
                throw new IllegalDirectAccessException(database.getDbReference());
            }
        }
        
    }

    private URL enforceSecureAppMode(WGDatabase db, HttpServletRequest req) throws MalformedURLException {
        
        boolean secureAppMode = db.getBooleanAttribute(WGACore.DBATTRIB_SECURE_APP, false);
        if (secureAppMode && !req.isSecure()) {
            URL currentURL = new URL(req.getRequestURL().toString());
            
            int redirectPort = 443;
            String defaultPortHTTPS = (String) db.getAttribute(WGACore.DBATTRIB_DEFAULTPORT_HTTPS);
            if (defaultPortHTTPS != null) {
                try {
                    redirectPort = Integer.valueOf(defaultPortHTTPS);
                }
                catch (NumberFormatException e) {
                    core.getLog().error("Exception parsing default HTTPS port as integer: " + defaultPortHTTPS + ". Will use 443 instead.");
                }
            }
            
            URL redirectURL;
            if (redirectPort != 443) {
                redirectURL = new URL("https", currentURL.getHost(), redirectPort, currentURL.getFile());
            }
            else {
                redirectURL = new URL("https", currentURL.getHost(), currentURL.getFile());
            }
            return redirectURL;
        }

        return null;
        
    }
    
    
    /**
     * 
     * @param db
     * @param currentURL
     * @return the redirect url if redirect is necessary - null otherwise
     * @throws MalformedURLException
     */
    private URL enforceRedirectionRules(WGDatabase db, URL currentURL) throws MalformedURLException {
        
        String redirectProtocol = (String) db.getAttribute(WGACore.DBATTRIB_REDIRECTPROTOCOL);
        String redirectHost = (String) db.getAttribute(WGACore.DBATTRIB_REDIRECTHOST);
        String redirectPort = (String) db.getAttribute(WGACore.DBATTRIB_REDIRECTPORT);
        
        if (redirectProtocol == null && redirectHost == null && redirectPort == null) {
            // no redirect configured
            return null;
        }
        
        return determineRedirectionURL(currentURL, redirectProtocol, redirectHost, redirectPort);
    }

    public static URL determineRedirectionURL(URL currentURL, String redirectProtocol, String redirectHost, String redirectPort) throws MalformedURLException {
        // determine current protocol, host and port
        String currentProtocol = currentURL.getProtocol();
        String currentHost = currentURL.getHost();
        String currentPort = null;
        if (currentURL.getPort() != -1) {
            currentPort = new Integer(currentURL.getPort()).toString();
        }
        else if ("http".equals(currentProtocol)) {
            currentPort = "80";
        }
        else if ("https".equals(currentProtocol)) {
            currentPort = "443";
        }
        
        //build redirectURL
        boolean redirectNecessary = false;
        StringBuffer redirectURLBuffer = new StringBuffer();
        if (redirectProtocol != null) {                        
            if (!currentProtocol.equalsIgnoreCase(redirectProtocol)) {
                redirectURLBuffer.append(redirectProtocol);
                redirectNecessary = true;
            } else {
                redirectURLBuffer.append(currentProtocol);
            }
        } 
        else {
            redirectURLBuffer.append(currentProtocol);
        }
        
        redirectURLBuffer.append("://");    
        
        if (redirectHost != null) {                        
            if (!currentHost.equalsIgnoreCase(redirectHost)) {
                redirectURLBuffer.append(redirectHost);
                redirectNecessary = true;                        	
            } 
            else {
                redirectURLBuffer.append(currentHost);
            }                        
        }  
        else {
            redirectURLBuffer.append(currentHost);
        }                   
        
        if (redirectPort != null && currentPort != null){        	
            if (!currentPort.equalsIgnoreCase(redirectPort)) {
                redirectURLBuffer.append(":" + redirectPort);
                redirectNecessary = true;
            } 
            else {
                redirectURLBuffer.append(":" + currentPort);
            }                        
        } 
        else if (currentPort != null) {
            redirectURLBuffer.append(":" + currentPort);
        }
        
        redirectURLBuffer.append(currentURL.getPath());
        
        if (currentURL.getQuery() != null) {
            redirectURLBuffer.append("?").append(currentURL.getQuery());
        }
        
        if (redirectNecessary) {
        	URL redirectURL = new URL(redirectURLBuffer.toString());        
        	return redirectURL;
    	} else {
    		return null;
    	}
    }

    protected void setTitlePathURL(TitlePathManager.TitlePath titlePathURL) {
        this.titlePathURL = titlePathURL;
    }

    protected String getRequestLanguage() {
        return requestLanguage;
    }
    
    public WGContent getContentByTitlePath(HttpServletRequest request) throws WGAPIException, UnsupportedEncodingException {

        TitlePathManager tpm = (TitlePathManager) database.getAttribute(WGACore.DBATTRIB_TITLEPATHMANAGER);
        TitlePath titlePathURL = getTitlePathURL();
        
        WGContent pathContent = null;
        try {
            pathContent = tpm.findContentByTitlePath(titlePathURL, database, new RequestLanguageChooser(database, request), TitlePathManager.MODE_URL);
        }
        catch (RemainingPathElementException e) {
            // Cannot happen with MODE_URL of title path manager
        }
        
        return pathContent;
        
    }

    protected WGContent getContent() {
        return content;
    }
    
    private boolean isHttpLoginUsed(WGDatabase db) {
        if (db.getAttribute(WGACore.DBATTRIB_HTTPLOGIN) != null && db.getAttribute(WGACore.DBATTRIB_HTTPLOGIN).equals("true")) {
            return true;
        } else {
            for (String mediaKeyStr : core.getMediaKeys()) {
                MediaKey mediaKey = core.getMediaKey(mediaKeyStr);
                if (mediaKey != null && mediaKey.isHttpLogin()) {
                    return true;
                }
            }
        }        
        return false;        
    }
    
    public static WGPRequestPath parseRequest(WGPDispatcher dispatcher, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws HttpErrorException, WGException, IOException {
        WGPRequestPath path = new WGPRequestPath(request, response, dispatcher);
        
        if (!path.isMasterLogin() && path.getDatabase() != null && path.getDatabase().isSessionOpen()) {
            try {
                path.testGeneralAccess(request);
            }
            catch (AdminLoginNeededException e) {
                if (request.getParameter("$ajaxInfo") == null) {
                    dispatcher.sendRedirect(response, dispatcher.getAdminLoginURL(request, path.getCompleteURL()));
                }
                else {
                    // this is an ajax call without login information - redirect to jsp and fire event LoginRequired
                    String loginRequiredEvent = de.innovationgate.wgpublisher.webtml.portlet.PortletEvent.SESSION_IS_NEW_EVENT.toJavaScriptObject();
                    String encodedEvent = Base64.encodeWeb(loginRequiredEvent.getBytes());
                    dispatcher.sendRedirect(response, path.publisherURL + "/fireSystemEvent.jsp?event="  + encodedEvent);
                }
                
                path.proceedRequest = false;
            }
            catch (IllegalDirectAccessException e) {
                path.handleLoginFailure(request, response, dispatcher);
                path.proceedRequest = false;
            }
        }
        
        return path;
    }

    public String getDerivateName() {
        return derivateName;
    }

    protected void setPathType(int pathType) {
        this.pathType = pathType;
    }

    protected void setMediaKey(MediaKey mediaKey) {
        this.mediaKey = mediaKey;
    }

}

