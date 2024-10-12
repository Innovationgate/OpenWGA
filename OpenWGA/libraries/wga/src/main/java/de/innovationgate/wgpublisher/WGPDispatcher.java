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

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.SocketException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.activation.DataSource;
import javax.activation.URLDataSource;
import javax.servlet.GenericServlet;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionBindingEvent;
import javax.servlet.http.HttpSessionBindingListener;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.log4j.Logger;

import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.TransientWrappedMap;
import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.io.ByteArrayDataSource;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGTransientPortletRegistry;
import de.innovationgate.webgate.api.WGUnresolveableVirtualLinkException;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.model.BrowsingSecurity;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wgpublisher.auth.LoginAttemptInformation;
import de.innovationgate.wgpublisher.cache.FileCache;
import de.innovationgate.wgpublisher.cache.PostprocessedResourcesCache;
import de.innovationgate.wgpublisher.design.conversion.PostProcessData;
import de.innovationgate.wgpublisher.design.conversion.PostProcessResult;
import de.innovationgate.wgpublisher.design.conversion.PostProcessor;
import de.innovationgate.wgpublisher.design.fs.DesignFileDocument;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptAppGlobalRegistry;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQueryTerm;
import de.innovationgate.wgpublisher.files.derivates.TypeQueryTermProcessor;
import de.innovationgate.wgpublisher.files.derivates.WGFailedDerivateQueryException;
import de.innovationgate.wgpublisher.files.derivates.WGInvalidDerivateQueryException;
import de.innovationgate.wgpublisher.filter.WGAFilter;
import de.innovationgate.wgpublisher.lang.LanguageBehaviour;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;
import de.innovationgate.wgpublisher.lang.RequestLanguageChooser;
import de.innovationgate.wgpublisher.log.WGARequestInformation;
import de.innovationgate.wgpublisher.problems.DatabaseScope;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.HTTPProblemType;
import de.innovationgate.wgpublisher.problems.MessageVariableProvider;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.scheduler.Job;
import de.innovationgate.wgpublisher.so.SessionScopeResolver;
import de.innovationgate.wgpublisher.url.TitlePathManager;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.vlink.VirtualLinkTarget;
import de.innovationgate.wgpublisher.vlink.VirtualLinkTarget.Type;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext;
import de.innovationgate.wgpublisher.webtml.form.TMLFormProcessContext.PCFile;
import de.innovationgate.wgpublisher.webtml.form.TMLFormPublishingFile;
import de.innovationgate.wgpublisher.webtml.init.WebTMLEnvironmentBuilder;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateStorage;
import de.innovationgate.wgpublisher.webtml.utils.AjaxActionDefinition;
import de.innovationgate.wgpublisher.webtml.utils.AjaxInfo;
import de.innovationgate.wgpublisher.webtml.utils.HttpErrorException;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public class WGPDispatcher extends HttpServlet {

    public static final String SESSION_AJAX_GENERAL_FAILURE_MESSAGE_SHOWN = "de.innovationgate.ajax.GeneralFailureMessageShown";

    public static final String COOKIE_LASTREDIRECT = "WGLastRedirectHex";

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public static final String URLPARAM_MAXWIDTH = "maxwidth";

    public static final String URLPARAM_MAXHEIGHT = "maxheight";
    
    public static final String URLPARAM_DERIVATE = "derivate";
    
    private static final String BYTERANGE_BOUNDARY = "##BYTERANGE##";

    public static final String REQUESTTYPE_TML = "tml";

    public static final String REQUESTTYPE_STATICTML = "statictml";

    private static final Pattern ACCEPT_RANGE_PATTERN = Pattern.compile("(\\d+)?\\-(\\d+)?");

    private static class AcceptRange {
        public long from;

        long to;

        public AcceptRange(int from, int to) {
            this.from = from;
            this.to = to;
        }
    }
    
    public static class TemporaryDownloadsMap extends TransientWrappedMap<String,TemporaryDownload> {
        @Override
        protected Map<String, TemporaryDownload> initWrappedMap() {
            return new ConcurrentHashMap<String, WGPDispatcher.TemporaryDownload>();
        }
    }
    
    public static class PathDispatchingOccasion implements ProblemOccasion, MessageVariableProvider {
        
        private String _uri;
        private String _dbKey;
        private String _completeUrl;
        private String _host;

        public PathDispatchingOccasion(HttpServletRequest req, String dbKey) {
            _uri = req.getRequestURI();
            _completeUrl = req.getRequestURL().toString();
            _host = req.getServerName();
            _dbKey = dbKey;
        }

        @Override
        public ProblemScope getDefaultScope() {
            if (_dbKey != null) {
                return new DatabaseScope(_dbKey);
            }
            else {
                return GlobalScope.INSTANCE;
            }
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return HTTPProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return WGPDispatcher.class;
        }

        @Override
        public Problem.Vars getDefaultMessageVariables() {
            return Problem.var("uri", _uri).var("completeurl", _completeUrl).var("host", _host);
        }

        @Override
        public boolean isClearedAutomatically() {
            return false;
        }

        protected String getUri() {
            return _uri;
        }

        protected String getDbKey() {
            return _dbKey;
        }

        protected String getCompleteUrl() {
            return _completeUrl;
        }

        protected String getHost() {
            return _host;
        }
        
    }

    public static class URLID {

        private String _resourceId = null;
        
        private String _resourceExtraKey = null;

        private String _language = null;

        private String _suffix = null;

        private String _completeId;

        private boolean _completeFormat = true;

        private WGDatabase _db;
        
        public URLID(String id, WGDatabase contentDb) throws WGAPIException {
        	this(id, contentDb, false);
        }
        public URLID(String id, WGDatabase contentDb, boolean titlepath_with_key) throws WGAPIException {

            _db = contentDb;

            if (id == null || id.trim().equals("")) {
                throw new WGIllegalArgumentException("The given id is empty or null");
            }

            /*
             * #00005772: id now has format 
             * path/to/content[.key].language.mediakey
             * or
             * structkey.language.version
             */
            List<String> tokens = WGUtils.deserializeCollection(id, WGContentKey.TOKEN_DIVIDER);
            Collections.reverse(tokens);
            if (tokens.size() < 3) {
                _completeFormat = false;
            }

            int contentStartsAt = 0;
            if (tokens.size() >= 2) {
                _suffix = (String) tokens.get(0);
                contentStartsAt = 1;
            }

            if (tokens.size() >= 3) {
                _language = (String) tokens.get(1);
                contentStartsAt = 2;
                if (_language.equals(DEFAULT_LANGUAGE_TOKEN)) {
                    _language = contentDb.getDefaultLanguage();
                }
            }

            // openwga 7-9-3
            // #00005772
            if (titlepath_with_key && tokens.size() >= 4) {
            	
            	String resourceKeyCandidat = (String) tokens.get(2);
            	// test if we have a structkey or sequence
            	WGStructEntry entry = contentDb.getStructEntryByKey(resourceKeyCandidat);
            	if(entry==null){
            		try{
		            	long seq = Long.parseLong(resourceKeyCandidat, 16);
		            	entry = contentDb.getStructEntryBySequence(seq);
            		}
            		catch(Exception e){}	// ignore number format exceptions
            	}
            	if(entry!=null){
	                _resourceExtraKey = (String) tokens.get(2);
	                contentStartsAt = 3;
            	}
            
            }

            List<String> contentIdTokens = tokens.subList(contentStartsAt, tokens.size());
            Collections.reverse(contentIdTokens);
            _resourceId = WGUtils.serializeCollection(contentIdTokens, WGContentKey.TOKEN_DIVIDER);

            if (titlepath_with_key){
            	// #00005772
            	// compatibility with oder title path urls:
            	// find ~ char and interpret as key
	            int tildePos = _resourceId.indexOf("~");
	            if (tildePos != -1) {
	                _resourceExtraKey = _resourceId.substring(tildePos + 1);
	                _resourceId = _resourceId.substring(0, tildePos);
	            }
            }
            
            StringBuffer completeId = new StringBuffer();
            completeId.append(_resourceId);
            if (_language != null) {
                completeId.append(".");
                completeId.append(_language);
            }
            if (_suffix != null) {
                completeId.append(".");
                completeId.append(_suffix);
            }
            _completeId = completeId.toString();

        }

        public boolean isCompleteFormat() {
            return _completeFormat;
        }

        public void setCompleteFormat(boolean complete) {
            _completeFormat = complete;
        }

        public String getResourceId() {
            return _resourceId;
        }

        public String getLanguage() {
            return _language;
        }

        public String getSuffix() {
            return _suffix;
        }

        public String getCompleteId() {
            return _completeId;
        }

        public WGContentKey asContentKey() throws WGAPIException {
            if (isCompleteFormat()) {
                return WGContentKey.parse(_completeId, _db);
            }
            else {
                return null;
            }
        }

        public int getSuffixVersion() {

            int version = 0;
            try {
                version = Integer.parseInt(_suffix);
            }
            catch (NumberFormatException e) {
                // Everything ok. All non-numeric versions (p, mediakey) will
                // get parsed as version 0
            }
            return version;

        }

        @Override
        public String toString() {
            return _completeId;
        }

        public String getResourceExtraKey() {
            return _resourceExtraKey;
        }

    }

    public static class TemporaryDownload implements HttpSessionBindingListener {

        private String _name;

        private TemporaryFile _tempFile;

        public TemporaryDownload(String name, TemporaryFile file) {
            super();
            this._name = name;
            this._tempFile = file;
        }

        public TemporaryFile getTempFile() {
            return _tempFile;
        }

        public String getName() {
            return _name;
        }

        protected void finalize() throws Throwable {
            _tempFile.delete();
        }

        public void valueBound(HttpSessionBindingEvent arg0) {
        }

        public void valueUnbound(HttpSessionBindingEvent arg0) {
            _tempFile.delete();
        }

    }

    WGACore _core;

    private Logger _log;

    public static final String SESSION_VARS = "Vars";

    // Session attributes set by this program
    public static final String SESSION_LOGINS = "Logins";

    // Session attribute where temporary downloads are registered
    public static final String SESSION_TEMPORARYDOWNLOADS = "TemporaryDownloads";

    // Flag if pages are to be served
    private volatile boolean _servePages = false;

    private int _listenPort = -1;

    private String _contextPath = null;
    

    private WebTMLDebugger _tmlDebugger;

    public static final String URLPARAM_VARS = "$vars";

    public static final String URLPARAM_ACTION = "$action";

    public static final String DEFAULT_LANGUAGE_TOKEN = "int";

    

    

    // Config flags and infos

    public void doPut(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws javax.servlet.ServletException, java.io.IOException {
        this.doPost(request, response);
    }

    public void doPost(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws javax.servlet.ServletException, java.io.IOException {

        if (!isServePages()) {
            response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "Website is currently updating configuration. Please try again later.");
            return;
        }

        String path = request.getServletPath() + (request.getPathInfo() != null ? request.getPathInfo() : "");

        try {
            

            if (path.equals("/login")) {

                doLogin(request, response);
                return;

            }
            else if (path.equals("/logout")) {

                String domain = request.getParameter("domain");
                _core.logout(domain, request.getSession(), request, response, true);
                sendRedirect(request, response, request.getParameter("redirect"));

            }
            else if (path.equals("/ajaxform")) {
                this.dispatchAjaxForm(request, response);
            }
            else {
                this.doGet(request, response);
            }

        }
        catch (AjaxFailureException exc) {
            handleAjaxFailure(exc, request, response);
        }
        catch (Exception exc) {
            _log.error("Exception in processing of request URL " + String.valueOf(request.getRequestURL()), exc);
            request.setAttribute(WGACore.ATTRIB_EXCEPTION, exc);
            throw new ServletException(exc);
        }
        catch (Error err) {
            _log.error("Error in processing of request URL " + String.valueOf(request.getRequestURL()), err);
            request.setAttribute(WGACore.ATTRIB_EXCEPTION, err);
            throw new ServletException(err);
        }
    }

    /**
     * Handles AJAX failures where ajaxinfo could not even be parsed, therefor it is unknown if an action is regular or norefresh
     * @param exc
     * @param request
     * @param response
     */
    private void handleAjaxFailure(AjaxFailureException exc, HttpServletRequest request, HttpServletResponse response) {

        try {
            
            String messageShown = (String) request.getSession().getAttribute(SESSION_AJAX_GENERAL_FAILURE_MESSAGE_SHOWN);
                    
            StringBuffer js = new StringBuffer();
            String errorMsg = getCore().getSystemLabel("tml", exc.getLabelKey(), request);
            if (!"true".equals(messageShown)) {
                request.getSession().setAttribute(SESSION_AJAX_GENERAL_FAILURE_MESSAGE_SHOWN, "true");
                StringBuffer text = new StringBuffer();
                text.append(getCore().getSystemLabel("tml", "ajax.failure.intro", request));
                text.append("\n\n");
                text.append(errorMsg);
                if (exc.getDetailMessage() != null) {
                    text.append(":\n\n").append(exc.getDetailMessage());
                }
                text.append("\n\n");
                text.append(getCore().getSystemLabel("tml", "ajax.failure.reload", request));
                
                WGA wga = WGA.get(request, response, getCore());
                js.append("WGA.util.showReloadMessage(\"" + wga.encode("javascript", text.toString()) + "\");\n");

	            Writer writer = response.getWriter();
	            
	            if (exc.getAjaxType() == AjaxFailureException.AJAXTYPE_FORMPOST) {
	                // The formpost request only executes a single script tag without surroundings
	                writer.write("<script>\n");
	                writer.write(js.toString());
	                writer.write("\n</script>");
	            }
	            else {
	                // For AJAX norefresh. Protect from view via HTML Comment
	                writer.write("//" + errorMsg + "\n<!--\n");
	                writer.write(js.toString());
	                writer.write("\n//-->\n");
	                
	                // For regular AJAX, protect from norefresh via JS comment
	                writer.write("/*" + errorMsg + "<script>\n");
	                writer.write(js.toString());
	                writer.write("\n</script> */");
	            }
	            
	            writer.flush();
            }
            
        }
        catch (Exception e) {
            getCore().getLog().error("Exception handling ajax failure", e);
        }
        
    }

    /**
     * retrieve posted formdata, parse request, store posted data in
     * temp-sessionkey, render javascript-callbackfunction to submit sessionkey
     * to clientside ajax call
     * 
     * @param request
     * @param response
     * @throws HttpErrorException 
     */
    private void dispatchAjaxForm(HttpServletRequest request, HttpServletResponse response) throws AjaxFailureException {
        // skip logging
        WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
        if (info != null) {
            info.setType(WGARequestInformation.TYPE_AJAXFORM);
        }

        if (ServletFileUpload.isMultipartContent(request)) {
            
            try {
                TMLForm.MultipartFormData formData = new TMLForm.MultipartFormData(request, getCore());

                if (formData.getFormActionItem() == null || formData.getAjaxCallIdItem() == null) {
                    getCore().getLog().error("Could not retrieve 'formaction' or 'ajaxcallid' from ajaxform-data.");
                    return;
                }
                
                // generate temp session key
                String sessionKey = UIDGenerator.generateUID();

                request.getSession().setAttribute(sessionKey, formData);

                if (request.getSession().isNew()) {
                    // server create new session in an ajax form call
                    // set flag on session and later render
                    // Event.SESSION_IS_NEW_EVENT
                    // in ajax call to notify client
                    // the sessionflag is handled and removed in root-tag
                    request.getSession().setAttribute(sessionKey + ".event.SessionWasNew", new Boolean(true));
                }

                // create action definition
                String formaction = formData.getDecodedItemValue(formData.getFormActionItem());
                String ajaxcallid = formData.getDecodedItemValue(formData.getAjaxCallIdItem());
                String ajaxgraydiv = formData.getDecodedItemValue(formData.getAjaxGrayDivItem());
                String ajaxmode = formData.getDecodedItemValue(formData.getAjaxModeItem());
                
                AjaxActionDefinition actionDef = new AjaxActionDefinition(formaction, ajaxcallid);
                actionDef.setTmlformSessionKey(sessionKey);
                if (ajaxgraydiv != null && !ajaxgraydiv.equals("#null#")) {
                    actionDef.setGraydiv(Boolean.valueOf(ajaxgraydiv).booleanValue());
                }
                if (ajaxmode != null && !ajaxmode.equals("#null#")) {
                    actionDef.setMode(ajaxmode);
                }

                // render response
                response.setContentType("text/html");
                PrintWriter out = response.getWriter();
                out.write("<script>");
                out.write("parent.WGA.ajax.formCallback(" + actionDef.toJavaScriptObject() + ");");
                out.write("</script>");
                
            }
            catch (InvalidEncryptionException e) {
                throw new AjaxFailureException("ajax.problem.changedencryption", AjaxFailureException.AJAXTYPE_FORMPOST);
            }
            catch (Exception e) {
                getCore().getLog().error("Error creating ajaxcall response.", e);
                throw new AjaxFailureException("ajax.problem.unknown", AjaxFailureException.AJAXTYPE_FORMPOST, e);
            }
        }

    }

    private void doLogin(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws HttpErrorException, IOException, MalformedURLException,
            ServletException, WGException {

        // Get the necessary fields
        String domain = request.getParameter("domain");
        String username = request.getParameter("username");
        String password = request.getParameter("password");
        String redirect = request.getParameter("redirect");
        String referer = request.getHeader("Referer");

        // Some basic validating
        if (domain == null) {
            throw new HttpErrorException(500, "Missing parameter \"domain\". (Hint: Field name must be lower case)", null);
        }
        if (username == null) {
            throw new HttpErrorException(500, "Missing parameter \"username\". (Hint: Field name must be lower case)", null);
        }
        if (password == null) {
            throw new HttpErrorException(500, "Missing parameter \"password\". (Hint: Field name must be lower case)", null);
        }

        boolean isLoginSuccessful = false;
        try {
            if (WGACore.DOMAIN_ADMINLOGINS.equals(domain)) {
                if (!_core.isAdministrativePort(request.getLocalPort())) {
                    throw new HttpErrorException(HttpServletResponse.SC_FORBIDDEN, "Access to administrative applications is disabled", null);
                }
                if (!username.isEmpty() && !password.isEmpty())
                	isLoginSuccessful = _core.doAdminLogin(username, password, request);
            }
            else if (!username.isEmpty() && !password.isEmpty()) 
           		isLoginSuccessful = _core.login(username, password, domain, request, response);
        }
        catch (LoginException e) {
            throw new ServletException("Login Error", e);
        }

        // React on login success
        if (isLoginSuccessful) {

            if (redirect != null) {
                sendRedirect(request, response, redirect);
            }
            else if (referer != null) {
                request.setAttribute(WGACore.ATTRIB_LOGINERROR, "No redirect specified.");
                de.innovationgate.utils.URLBuilder builder = new de.innovationgate.utils.URLBuilder(new java.net.URL(referer));
                builder.setParameter("loginerror", "2");
                sendRedirect(request, response, builder.build(false));
            }
            else {
                response.setStatus(HttpServletResponse.SC_OK);
            }
        }
        else {

        	LoginAttemptInformation inf = _core.getBruteForceLoginBlocker().getLoginAttemptInformation(domain, username); 
        	if(inf!=null && inf.isBlocked()){
        		long now = System.currentTimeMillis();
        		Float minutes = (float)LoginAttemptInformation.BLOCKED_MINUTES - ((now - inf.getBlockedDate().getTime())/(1000*60)); 
        		request.setAttribute(WGACore.ATTRIB_LOGINERROR, "Login for username '" + username + "' is blocked for the next " + minutes.intValue() + " minutes because of too many wrong login attempts.");
        	}
        	
        	else if (WGACore.DOMAIN_ADMINLOGINS.equals(domain)) {
                if (username.isEmpty() || password.isEmpty())
                    request.setAttribute(WGACore.ATTRIB_LOGINERROR, "Invalid administrative login: Empty username or password.");
                else request.setAttribute(WGACore.ATTRIB_LOGINERROR, "Invalid administrative login. Please verify username and password.");
            }
            else {
            	if (username.isEmpty() || password.isEmpty()) 
            		request.setAttribute(WGACore.ATTRIB_LOGINERROR, "Invalid login to domain " + domain + ": Empty username or password.");
            	else request.setAttribute(WGACore.ATTRIB_LOGINERROR, "Invalid login to domain " + domain + ". Please verify username and password.");
            }

            if (request.getParameter("flag") != null) {
                response.setContentType("text/html");
                request.getRequestDispatcher("login.jsp").include(request, response);
            }
            else if (referer != null) {
                de.innovationgate.utils.URLBuilder builder = new de.innovationgate.utils.URLBuilder(new java.net.URL(referer));
                builder.setParameter("loginerror", "1");
                sendRedirect(request, response, builder.build(false).toString());
            }
            else {
                response.sendError(HttpServletResponse.SC_FORBIDDEN, "Invalid login");
            }
        }
        return;
    }

    public void doGet(javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws javax.servlet.ServletException, java.io.IOException {
        
    	long start = System.currentTimeMillis();
    	
        if (!isServePages()) {
            response.sendError(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "Website is currently updating configuration. Please try again later.");
            return;
        }

        Date startDate = new Date();

        if (this._contextPath == null) {
            this._contextPath = request.getContextPath();
            this._listenPort = request.getServerPort();
        }
        
        WGARequestInformation reqInfo = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);

        try {

            // Parse request
            WGPRequestPath path = WGPRequestPath.parseRequest(this, request, response);
            request.setAttribute(WGACore.ATTRIB_REQUESTPATH, path);

            // If database login failed or access was denied exit immediately
            if (!path.isProceedRequest()) {
                return;
            }

            // Set access logging for this request
            if (path.getDatabase() != null) {
                String accessLoggingEnabled = (String) path.getDatabase().getAttribute(WGACore.DBATTRIB_ENABLE_ACCESSLOGGING);
                if (accessLoggingEnabled != null) {
                    if (reqInfo != null) {
                        reqInfo.setLoggingEnabled(Boolean.parseBoolean(accessLoggingEnabled));
                    }
                }
            }

            int iPathType = path.getPathType();

            // Treatment of special URL types
            String dbKey = path.getDatabaseKey();
            if (iPathType == WGPRequestPath.TYPE_INVALID) {
                throw new HttpErrorException(404, "Invalid path: " + path.getBasePath(), dbKey);
            }
            if (iPathType == WGPRequestPath.TYPE_INVALID_DB) {
                throw new HttpErrorException(404, "Specified application '" + dbKey + "' is unknown", null);
            }

            if (iPathType == WGPRequestPath.TYPE_UNKNOWN_CONTENT) {
                sendNoContentNotification(path, request, response, path.getDatabase());
                return;
            }

            if (iPathType == WGPRequestPath.TYPE_GOTO_HOMEPAGE) {
                iPathType = determineHomepage(request, path, iPathType);
            }

            if (iPathType == WGPRequestPath.TYPE_UNAVAILABLE_DB) {
                throw new HttpErrorException(HttpServletResponse.SC_SERVICE_UNAVAILABLE, "The website is currently unavailable", path.getDatabaseKey());
            }

            if (iPathType == WGPRequestPath.TYPE_UNDEFINED_HOMEPAGE) {
                throw new HttpErrorException(HttpServletResponse.SC_NOT_FOUND, "No home page was defined for app '" + path.getDatabaseKey() + "'. Please specify an explicit content path.", path
                        .getDatabaseKey());
            }

            if (iPathType == WGPRequestPath.TYPE_TMLDEBUG) {
                _tmlDebugger.performDebugMode(request, response, request.getSession());
                return;
            }

            if (iPathType == WGPRequestPath.TYPE_JOBLOG) {
                sendJobLog(request, response, request.getSession());
                return;
            }

            if (iPathType == WGPRequestPath.TYPE_LOGOUT) {
                WGDatabase db = (WGDatabase) _core.getContentdbs().get(dbKey);
                String domain = (String) db.getAttribute(WGACore.DBATTRIB_DOMAIN);
                _core.logout(domain, request.getSession(), request, response, true);
                removeSessionCookie(response, request.getSession(), db);

                iPathType = WGPRequestPath.TYPE_REDIRECT;
            }

            if (iPathType == WGPRequestPath.TYPE_FORWARD) {
            	String url = path.getResourcePath();
            	request.getRequestDispatcher(url).forward(request, response);
            	return;
            }
            
            if (iPathType == WGPRequestPath.TYPE_FAVICON) {
                String faviconPath = determineFavicon(request);
                if (faviconPath != null) {
                	request.getRequestDispatcher(faviconPath).forward(request, response);
                	return;
                }
                else {
                	// should not happen since we have a default favicon
                    response.sendError(HttpServletResponse.SC_NOT_FOUND, "Favicon not defined");
                    return;
                }
            }
            
            if (iPathType == WGPRequestPath.TYPE_TMLFORM) { 
                dispatchTmlFormRequest(path, request, response);
                return;
            }          

            if (iPathType == WGPRequestPath.TYPE_APP_DISPATCHER) { 
                appDispatcher(path, request, response);
                return;
            }          


            // Treatment of base URL Types
            if (iPathType == WGPRequestPath.TYPE_REDIRECT) {
                String url = path.getResourcePath();
                if (path.appendQueryString() == true && request.getQueryString() != null && !request.getQueryString().equals("")) {
                    if (url.indexOf("?") != -1) {
                        url += "&" + request.getQueryString();
                    }
                    else {
                        url += "?" + request.getQueryString();
                    }
                }

                if (path.isPermanentRedirect()) {
                    sendPermanentRedirect(response, url);
                }
                else {
                    sendRedirect(request, response, url);
                }
            }
            else if (iPathType != WGPRequestPath.TYPE_RESOURCE && iPathType != WGPRequestPath.TYPE_STATICTML && !_core.getContentdbs().containsKey(path.getDatabaseKey())) {
                throw new HttpErrorException(404, "Database '" + dbKey + "' is unknown", null);
            }
            else {

                String requestMethod = request.getMethod().toLowerCase();
                switch (iPathType) {
                    case (WGPRequestPath.TYPE_TML):
                    case (WGPRequestPath.TYPE_TITLE_PATH):
                        
                        // Fetch the redirect cookie
                        Cookie lastRedirectCookie = null;
                        Cookie[] cookies = request.getCookies();
                        if (cookies != null) {
                            for (Cookie cookie : cookies) {
                                if (cookie.getName().equals(COOKIE_LASTREDIRECT)) {
                                    lastRedirectCookie = cookie;
                                    break;
                                }
                            }
                        }
                        
                        // If path is not complete redirect it to the complete path, if possible. Set redirect cookie to prevent endless redirections
                        if (!path.isCompletePath()) {
                            String redirectPath = path.expandToCompletePath(request);
                            if (isRedirectable(request, redirectPath, lastRedirectCookie)) {
                                lastRedirectCookie = new WGCookie(COOKIE_LASTREDIRECT, Hex.encodeHexString(redirectPath.getBytes("UTF-8")));
                                lastRedirectCookie.setMaxAge(-1);
                                lastRedirectCookie.setPath("/");
                                lastRedirectCookie.setSecure(request.isSecure());
                                ((WGCookie)lastRedirectCookie).addCookieHeader(response);
                                sendRedirect(request, response, redirectPath);
                                break;
                            }
                        }
                        
                        // Delete redirect cookie when exists on normal dispatching
                        if (lastRedirectCookie != null) {
                            lastRedirectCookie = new WGCookie(COOKIE_LASTREDIRECT, "");
                            lastRedirectCookie.setMaxAge(0);
                            lastRedirectCookie.setPath("/");
                            ((WGCookie)lastRedirectCookie).addCookieHeader(response);
                        }
                    
                        // Dispatch
                        dispatchTmlRequest(path, request, response, startDate);
                        break;

                    case (WGPRequestPath.TYPE_FILE):
                        dispatchFileRequest(path, request, response);
                        break;

                    case (WGPRequestPath.TYPE_CSS):
                    case (WGPRequestPath.TYPE_JS):
                        dispatchCssjsRequest(path, request, response);
                        break;

                    case (WGPRequestPath.TYPE_RESOURCE):
                        dispatchResourceRequest(path, request, response);
                        break;

                    case (WGPRequestPath.TYPE_STATICTML):
                        dispatchStaticTmlRequest(path, request, response);
                        break;
                        
                    default:
                        throw new HttpErrorException(500, "Invalid url format", dbKey);
                }
            }

            // moved from finally block to ensure errorpage can be displayed
            commitResponse(response);
            _log.debug("doGet :" + (System.currentTimeMillis()-start) + "ms");
        }
        catch(ClientAccessException exc){
        	response.sendError(403, exc.getMessage());
        }
        catch (AjaxFailureException exc) {
            handleAjaxFailure(exc, request, response);   
        }
        catch (HttpErrorException exc) {
            request.setAttribute(WGACore.ATTRIB_EXCEPTION, exc);
            ProblemOccasion occ = new PathDispatchingOccasion(request, exc.getDbHint());
            _core.getProblemRegistry().addProblem(Problem.create(occ, "dispatching.http404#" + request.getRequestURL(), ProblemSeverity.LOW));
            if (!response.isCommitted()) {
                // throw exception to display errorpage - with senderror() the
                // applicationserver use the buildin errorpage
                if (exc.getCode() == HttpServletResponse.SC_NOT_FOUND || exc.getCode() == HttpServletResponse.SC_FORBIDDEN || exc.getCode() == HttpServletResponse.SC_PRECONDITION_FAILED) {
                    response.sendError(exc.getCode(), exc.getMessage());
                }
                else {
                	_log.error("Exception in processing request from " + request.getRemoteAddr() 
                		+ " to URL " + String.valueOf(request.getRequestURL())
                		+ (request.getQueryString()!=null ? " with query " + String.valueOf(request.getQueryString()) : "")
                	);
                    throw new ServletException(exc);
                }
            }
        }
        catch (SocketException exc) {
            _log.warn("Socket Exception: " + exc.getMessage());
        }
        catch (Exception exc) {
            _log.error("Exception in processing of request URL " + String.valueOf(request.getRequestURL()), exc);
            request.setAttribute(WGACore.ATTRIB_EXCEPTION, exc);
            throw new ServletException(exc);
        }
        catch (Error err) {
            _log.error("Error in processing of request URL " + String.valueOf(request.getRequestURL()), err);
            request.setAttribute(WGACore.ATTRIB_EXCEPTION, err);
            throw new ServletException(err);
        }
        finally {
            if (reqInfo != null) {
                reqInfo.setCommited(true);
            }
        }
    }

    private void appDispatcher(WGPRequestPath path, HttpServletRequest request, HttpServletResponse response) throws Exception {
    	
        WGA wga = WGA.get(request, response, getCore());
        
        // Set some basic attributes for WebTML processing
        request.setAttribute(WGACore.ATTRIB_WGPPATH, path.getPublisherURL());
        request.setAttribute(WGACore.ATTRIB_TAGIDS, new ConcurrentHashMap<String,BaseTagStatus>());
        request.setAttribute(WGACore.ATTRIB_REQUESTURL, path.getCompleteURL());
        request.setAttribute(WGACore.ATTRIB_REQUESTTYPE, REQUESTTYPE_TML);
        request.setAttribute(WGACore.ATTRIB_URI_HASH, WGUtils.createMD5HEX(request.getRequestURI().getBytes("UTF-8")));
        request.setAttribute(WGACore.ATTRIB_COOKIES, fetchHttpCookies(request));

        // Determine requested mime type and type key
        String mediaKey = path.getMediaKey();
        if (mediaKey == null) {
            mediaKey = path.getDatabase().getAttribute(WGACore.DBATTRIB_DEFAULT_MEDIAKEY).toString();
        }
        // Set these here for once, so the following scripts can fetch them. Might be changed by the renderer.
        MediaKey mediaKeyObj = _core.getMediaKey(mediaKey);
        request.setAttribute(WGACore.ATTRIB_MIMETYPE, mediaKeyObj.getMimeType());
        request.setAttribute(WGACore.ATTRIB_MEDIAKEY, mediaKeyObj.getKey());

        // Read ajax information, to be able to determine AJAX requests
        String encAjaxInfo = request.getParameter("$ajaxInfo");
        AjaxInfo ajaxInfo = null;
        if (encAjaxInfo != null) {
        	ajaxInfo = readAjaxInformation(request, path.getDatabase(), encAjaxInfo);
        }

    	try{
        	// Prepare WebTML environment
        	WGContent content = path.getDatabase().getDummyContent(path.getRequestLanguage());
            TMLUserProfile tmlUserProfile = null;
            try {
                tmlUserProfile = getCore().getPersManager().prepareUserProfileForRequest(request, response, content, path.getDatabase(), null, ajaxInfo != null);
            } 
            catch (Exception e) {
                _log.error("Unable to personalize WebTML request " + path.getCompleteURL(), e);
            }
	    	TMLContext mainContext = new WebTMLEnvironmentBuilder(getCore(), content, request, response, tmlUserProfile, ajaxInfo).prepareWebTmlEnvironment();
	    	
	    	if (ajaxInfo != null) {
	            // If AJAX read the AJAX data and use WebTML module from there
	        	WGTMLModule tmlLib = getAjaxTMLModule(request, path.getDatabase(), ajaxInfo);
	        	Design outerLayout = wga.design(tmlLib.getDatabase()).resolve(tmlLib.getName());
	        	mediaKey = tmlLib.getMediaKey();
	        	rootDispatch(wga, outerLayout, mainContext, mediaKey);
	    	}
	    	else{
		    	TMLScript tmlscript = wga.tmlscript();
		    	ArrayList<Object> params = new ArrayList<Object>();
		    	params.add(path);
		    	params.add(path.appDispatcher.getPathElements());
	    		tmlscript.callMethod(path.appDispatcher.getScriptObject(), path.appDispatcher.getLayoutKey(), null, params);
	    	}
            // Eventually do redirect
            if (request.getAttribute(WGACore.ATTRIB_REDIRECT) != null) {
            	response.sendRedirect(String.valueOf(request.getAttribute(WGACore.ATTRIB_REDIRECT)));
            }
    	}
    	catch (WGExpressionException e){
    		TMLScriptException scriptException = WGUtils.getCauseOfType(e, TMLScriptException.class);
    		if(scriptException!=null){
	    		@SuppressWarnings("rawtypes")
				Map result = (Map)scriptException.getErrorValue();
	    		int status = (int)result.get("code");
	    		String msg = (String)result.get("message");
	    		throw new HttpErrorException(status, msg, path.getDatabaseKey());
    		}
    		else throw e;
    	}
        catch (Exception e) {
            throw e;
        }
        finally {
            TMLContext.clearThreadMainContext();
        }
	}

	public boolean isRedirectable(HttpServletRequest request, String redirectPath, Cookie lastRedirectCookie) {
        
        // Only redirect GET and HEAD requests
        if (!request.getMethod().equalsIgnoreCase("get") && !request.getMethod().equalsIgnoreCase("head")) {
            //getCore().getLog().warn("Unredirectable request with incomplete URL: " + request.getRequestURI() + ". Reason: No GET or HEAD request. Request method was " + request.getMethod());
            return false;
        }
        
        // A potential endless redirect. We already redirected to the path we are going to redirect now. Cancel it.
        if (lastRedirectCookie != null) {
            try {
                String lastRedirectValue = new String(Hex.decodeHex(lastRedirectCookie.getValue().toCharArray()), "UTF-8");
                if (redirectPath.equals(lastRedirectValue)) {
                    getCore().getLog().warn("Unredirectable request with incomplete URL: " + request.getRequestURI() + ". Reason: Endless redirect.");
                    return false;
                }
            }
            catch (Exception e) {
                getCore().getLog().error("Unredirectable request with incomplete URL: " + request.getRequestURI() + ". Reason: Exception on comparison to previous redirect.", e);
                return false;
            }
            
        }
        
        return true;
        
    }
    
    private void dispatchTmlFormRequest(WGPRequestPath path, HttpServletRequest request, HttpServletResponse response) throws HttpErrorException, IOException, WGException {

        PCFile file = null;
        ProcessContextRegistration contexts = (ProcessContextRegistration) request.getSession().getAttribute(TMLContext.SESSIONATTRIB_PROCESSCONTEXTS);
        if (contexts != null) {
            TMLFormProcessContext pc = (TMLFormProcessContext) contexts.getProcessContext(path.getContainerKey());
            if (pc != null) {
                file = pc.getFiles().get(path.getFileName().toLowerCase());
            }
        }
        
        if (file == null) {
            throw new HttpErrorException(404, "No WebTML form file of name " + path.getFileName(), null);
        }
        
        file.openSession(request, _core);
        response.setDateHeader("Date", System.currentTimeMillis());

        // Create publishing file object, which will provide the data
        PublishingFile publishingFile = new TMLFormPublishingFile(this, file);
        if (!publishingFile.isPublishable()) {
            throw new HttpErrorException(403, "This WebTML form file may not be published", null);
        }
        
        dispatchPublishingFile(publishingFile, request, response, _core.getCharacterEncoding(), null, path);

    }

    private void sendPermanentRedirect(HttpServletResponse response, String url) {
        response.setStatus(301);
        response.setHeader( "Location", url);
        response.setHeader( "Connection", "close");
    }

    private int determineHomepage(javax.servlet.http.HttpServletRequest request, WGPRequestPath path, int iPathType) throws UnsupportedEncodingException, HttpErrorException {
        WGDatabase db = (WGDatabase) _core.getContentdbs().get(path.getDatabaseKey());

        WGAURLBuilder urlBuilder = _core.retrieveURLBuilder(request, db);

        String homepage;
        try {
            homepage = urlBuilder.buildHomepageURL(db, request);
            if (homepage != null) {
                path.setResourcePath(homepage);
                return WGPRequestPath.TYPE_REDIRECT;
            }
        }
        catch (WGException e) {
            getCore().getLog().error("Error determining homepage", e);
            throw new HttpErrorException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, e.getMessage(), path.getDatabaseKey());
        }

        return WGPRequestPath.TYPE_UNDEFINED_HOMEPAGE;
    }

    public String determineFavicon(HttpServletRequest request) {

        String faviconPath = getCore().getWgaConfiguration().getFavicon();
        if (faviconPath != null) {
            if (faviconPath.indexOf("://") == -1 && !faviconPath.startsWith("/")) {
                faviconPath = "/" + faviconPath;
            }
            return faviconPath;
        }
        else {
            return getPublisherURL(request) + "/static/images/brand/favicon.ico";
        }

    }

    private void commitResponse(javax.servlet.http.HttpServletResponse response) {
        if (!response.isCommitted()) {
            try {
                response.flushBuffer();
            }
            catch (IOException e) {
            }
        }
    }

    /**
     * @param request
     * @param response
     * @param session
     */
    private void sendJobLog(HttpServletRequest request, HttpServletResponse response, HttpSession session) throws IOException {

        if (!isAdminLoggedIn(request)) {
            response.sendError(HttpServletResponse.SC_FORBIDDEN, "You must be logged in as WGA administrator!");
            return;
        }

        String jobToShow = request.getParameter("name");
        Job job = getCore().getScheduler().getJob(jobToShow);
        if (job == null) {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Unknown job: " + jobToShow);
            return;
        }

        response.setContentType("text/html");
        Writer out = response.getWriter();
        out.write("<!DOCTYPE HTML>\n");
        out.write("<html>\n");
        out.write("<head><script>var running=" + Boolean.valueOf(job.isRunning()).toString() + ";</script></head>\n");
        
        out.write("<body style=\"background-color:white; font-family:courier; font-size:10pt\">\n");
        String log = job.getLog();
        LineNumberReader reader = new LineNumberReader(new StringReader(log));
        String line;
        while ((line = reader.readLine()) != null) {
            out.write(line);
            out.write("<br>\n");
        }
        if (!job.isRunning() && job.getEndMessage() != null) {
            out.write("<hr>\n");
            out.write(job.getEndMessage());
        }
        out.write("\n</body>\n</html>");
    }

    private void dispatchTmlRequest(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, Date startDate) throws java.lang.Exception {

        WGContent content = path.getContent();
        WGDatabase database = path.getDatabase();
        if (database == null) {
            throw new HttpErrorException(404, "No database of key " + path.getDatabaseKey(), null);
        }
        
        WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
        if (info != null) {
            info.setDatabase(database);
            info.setContent(content);
            if (content != null) {
                info.setType(WGARequestInformation.TYPE_CONTENT);
            }
            else {
                info.setType(WGARequestInformation.TYPE_TML);
            }
        }

        boolean ajax = false;

        HttpSession session = request.getSession();

        // Determine requested mime type and type key
        String mediaKey = path.getMediaKey();
        if (mediaKey == null) {
            mediaKey = database.getAttribute(WGACore.DBATTRIB_DEFAULT_MEDIAKEY).toString();
        }
        
        // Ensure existing SO registry for this session/database
        SessionScopeResolver.getRegistry(request.getSession(), database.getDbReference(), true);    

        // Look if a session cookie has to be set
        _core.setSessionCookie(request, response, database);

        // Context request, if content key filled or we have a title path
        if (content != null) {

            if (!content.mayBePublished(isBrowserInterface(session) || isAuthoringMode(database.getDbReference(), session), WGContent.DISPLAYTYPE_NONE)) {
                sendNoContentNotification(path, request, response, database);
                return;
            }

            if (content.isVirtual()) {

            	if (isBrowserInterface(session)) {
                    if (!content.getStatus().equals(WGContent.STATUS_DRAFT) && request.getParameter("forceVLink") == null) {
                        String url = getVirtualContentURL(request, database, path, content);
                        if(url!=null){
	                        sendRedirect(request, response, url);
	                        return;
                        }
                    }
                }
                else {
                    String vLink = buildVirtualLink(WGA.get(request, response, getCore()), content, path.getMediaKey(), path.getLayoutKey());
                    if (vLink != null) {
                        sendRedirect(request, response, vLink);
                        return;
                    }
                    else {
                        throw new HttpErrorException(HttpServletResponse.SC_NOT_FOUND, "Unresolveable virtual link on content " + content.getContentKey().toString(), path.getDatabaseKey());
                    }
                }
            }

        }

        // Contextless request. We use a dummy content
        else {
            content = database.getDummyContent(path.getRequestLanguage());
        }

        // Test browsability of content
        if (!content.isDummy() && getBrowsingSecurity(database) <= BrowsingSecurity.NO_BROWSING) {
            throw new HttpErrorException(java.net.HttpURLConnection.HTTP_FORBIDDEN, "Browsing not allowed in database '" + path.getDatabaseKey() + "'", path.getDatabaseKey());
        }

        // Drop cache if requested by url param
        if (request.getQueryString() != null && request.getQueryString().toLowerCase().indexOf("dropcache") != -1 && isAdminLoggedIn(request)) {
            content.dropCache();
        }
        
        // Preparse TMLForm data into multipart form data
        TMLForm.MultipartFormData formData = null;
        String ajaxFormdataSessionKey = request.getParameter("$ajaxformkey");
        if (ajaxFormdataSessionKey != null && !ajaxFormdataSessionKey.trim().equals("")) {
            formData = (TMLForm.MultipartFormData) request.getSession().getAttribute(ajaxFormdataSessionKey);                
            if (formData != null && !formData.isValid()) {
                formData = null;
            }
            request.getSession().removeAttribute(ajaxFormdataSessionKey);
        }
        else if (ServletFileUpload.isMultipartContent(request) && request.getContentLength() != -1) {
            try {
                formData = new TMLForm.MultipartFormData(request, getCore());
            }
            catch (IOException e) {
                getCore().getLog().warn("Could not parse multipart form data because of IO exception: " + WGUtils.getRootCause(e));
            }
            catch (Exception e) {
                getCore().getLog().error("Exception parsing multipart form data", e);
            }
        }

        // Read ajax information, to be able to determine AJAX requests
        String encAjaxInfo = request.getParameter("$ajaxInfo");
        boolean isAjax = (encAjaxInfo != null); 
        
        TMLUserProfile tmlUserProfile = null;
        try {
            tmlUserProfile = getCore().getPersManager().prepareUserProfileForRequest(request, response, content, database, formData, isAjax);
            if (info != null && tmlUserProfile!=null) {
                info.setProfile(tmlUserProfile);
            }
        } 
        catch (Exception e) {
            _log.error("Unable to personalize WebTML request " + path.getCompleteURL(), e);
        }

        // Set some basic attributes for WebTML processing
        request.setAttribute(WGACore.ATTRIB_WGPPATH, path.getPublisherURL());
        request.setAttribute(WGACore.ATTRIB_TAGIDS, new ConcurrentHashMap<String,BaseTagStatus>());
        request.setAttribute(WGACore.ATTRIB_REQUESTURL, path.getCompleteURL());
        request.setAttribute(WGACore.ATTRIB_REQUESTTYPE, REQUESTTYPE_TML);
        request.setAttribute(WGACore.ATTRIB_URI_HASH, WGUtils.createMD5HEX(request.getRequestURI().getBytes("UTF-8")));
        request.setAttribute(WGACore.ATTRIB_FORMDATA, formData);
        request.setAttribute(WGACore.ATTRIB_COOKIES, fetchHttpCookies(request));

        // Determine tml design for this request
        WGA wga = WGA.get(request, response, getCore());
        Design outerLayout = null;
        if (path.getLayoutKey() != null) {
            outerLayout = wga.design(database).resolve(path.getLayoutKey());
            WGTMLModule tmlLib = outerLayout.getTMLModule(mediaKey);
            if (tmlLib != null && tmlLib.isDirectAccessAllowed() == false) {
                throw new HttpErrorException(java.net.HttpURLConnection.HTTP_FORBIDDEN, "This design is not allowed for direct access: " + tmlLib.getName() + " (" + tmlLib.getMediaKey() + ")", path
                        .getDatabaseKey());
            }
        }
        else {
            WGStructEntry entry = content.getStructEntry();
            if (entry == null) {
                throw new HttpErrorException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Content " + content.getContentKey().toString() + " has no struct entry", path.getDatabaseKey());
            }
            outerLayout = wga.design(database).resolve(entry.getContentType().getOuterLayoutName());
            WGTMLModule tmlLib = outerLayout.getTMLModule(mediaKey);
            if(tmlLib==null){
                throw new HttpErrorException(java.net.HttpURLConnection.HTTP_NOT_FOUND, "Design not found for mediakey '" + mediaKey +"': " + outerLayout.toString(), path.getDatabaseKey());
            }
        }

        request.setAttribute(WGACore.ATTRIB_OUTER_DESIGN, outerLayout.getBaseReference().getResourceName());


        // If AJAX read the AJAX data and use WebTML module from there
        AjaxInfo ajaxInfo = null;
        if (encAjaxInfo != null) {
            ajaxInfo = readAjaxInformation(request, database, encAjaxInfo);
            ajax = true;
            WGTMLModule tmlLib = getAjaxTMLModule(request, database, ajaxInfo);
            outerLayout = wga.design(tmlLib.getDatabase()).resolve(tmlLib.getName());
            mediaKey = tmlLib.getMediaKey();
        }

        // Set these here for once, so the following scripts can fetch them. Might be changed by the renderer.
        MediaKey mediaKeyObj = _core.getMediaKey(mediaKey);
        request.setAttribute(WGACore.ATTRIB_MIMETYPE, mediaKeyObj.getMimeType());
        request.setAttribute(WGACore.ATTRIB_MEDIAKEY, mediaKeyObj.getKey());

        // Update usage statistics
        getCore().getUsageStatistics().addRequestStatistic(request, session, database, tmlUserProfile);
        
        // Prepare WebTML environment
        TMLContext mainContext = new WebTMLEnvironmentBuilder(getCore(), content, request, response, tmlUserProfile, ajaxInfo).prepareWebTmlEnvironment();
        
        // Dispatch
        try {
            rootDispatch(wga, outerLayout, mainContext, mediaKey);

            // Eventually do redirect
            if (request.getAttribute(WGACore.ATTRIB_REDIRECT) != null) {
                if (!ajax) { // On AJAX requests the redirect is performed by Root.tmlEndTag()
                    if (!response.isCommitted()) { 
                        if (!session.isNew()) { // on new sessions we must not reset the response (#00000147)
                            response.reset();
                        }
                        response.sendRedirect(String.valueOf(request.getAttribute(WGACore.ATTRIB_REDIRECT)));
                    }
                    else { // Out of luck for redirect. We only can log that we could not redirect
                        getCore().getLog().warn("Unable to perform redirect to '" + String.valueOf(request.getAttribute(WGACore.ATTRIB_REDIRECT)) +  "' because response was already committed");
                    }
                }
            }

        }
        catch (Exception t) {
            throw t;
        }
        finally {
            if (info != null) {
                info.setMimeType((String) request.getAttribute(WGACore.ATTRIB_MIMETYPE));
                info.setPath(path);
                info.setDesign(outerLayout.getTMLModule((String) request.getAttribute(WGACore.ATTRIB_MEDIAKEY)));
                info.setAjax(ajax);
            }
            
            TMLContext.clearThreadMainContext();
        }
    }

    public void rootDispatch(WGA wga, Design outerLayout, TMLContext mainContext, String mediaKey) throws WGException {
        
        // Optionally prepare websocket
        if (!wga.call().isAjax()) {
            TMLScriptAppGlobalRegistry appGlobalRegistry = wga.getCore().getTmlscriptGlobalRegistry().getAppGlobalRegistry(outerLayout.db());
            if (appGlobalRegistry.getUsedManagedGlobalScopes().contains(ObjectScope.TMLPAGE)) {
                String mediakey = wga.call().getMediaKey();
                MediaKey mediaKeyDef = wga.getCore().getMediaKey(mediaKey);
                if (mediaKeyDef != null) {
                    String mimeType = mediaKeyDef.getMimeType();
                    if (mimeType != null && mimeType.startsWith("text/html")) {
                        ((TMLPageImpl) wga.tmlPage()).prepareWebSocket();
                    }
                }
            }
        }
        
        dispatchToRenderer(wga, outerLayout, mainContext, mediaKey);
    }

    public void dispatchToRenderer(WGA wga, Design layout, TMLContext context, String mediaKey) throws WGException {

        // Optionally dispatch to custom renderer
        Design rendererDesign = layout.resolve("::" + layout.getBaseReference().getResourceLocalName() + ".renderer");
        WGScriptModule rendererModule = rendererDesign.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT);
        if (rendererModule != null) {
            TMLContext rendererContext = context.designContext(rendererDesign.getBaseReference().toString());
            wga = WGA.get(rendererContext);
            TMLScript tmlscript = wga.tmlscript();
            Object renderer = wga.tmlscript().createObject(rendererDesign, TMLScript.ObjectType.V2, null, null);
            if (tmlscript.hasProperty(renderer, "render")) {
                tmlscript.callMethod(renderer, "render", null, null);
                return;
            }
        }
        
        // Default render
        TMLPageImpl page = (TMLPageImpl) wga.tmlPage();
        page.render(layout, mediaKey, context);
        
        
    }

    private WGTMLModule getAjaxTMLModule(javax.servlet.http.HttpServletRequest request, WGDatabase database, AjaxInfo ajaxInfo) throws AjaxFailureException, WGAPIException {
        WGTMLModule tmlLib;
        // fetch correct db for ajax (if designdb on include tag is present use
        // design db, if not use current db)
        WGDatabase ajaxDB = null;
        String ajaxDesignDBKey = ajaxInfo.getDesignDB();
        if (ajaxDesignDBKey != null) {
            // fetch design db
            try {
                ajaxDB = _core.openContentDB(ajaxDesignDBKey, request);
            }
            catch (Exception e) {
                throw new AjaxFailureException("Unable to open database '" + ajaxDesignDBKey + "'", AjaxFailureException.AJAXTYPE_ACTION, e);
            }

            if (ajaxDB == null) {
                throw new AjaxFailureException("No database of key '" + ajaxDesignDBKey + "'", AjaxFailureException.AJAXTYPE_ACTION);
            }
            else if (ajaxDB.isSessionOpen() == false) {
                throw new AjaxFailureException("Database '" + ajaxDesignDBKey + "' could not be opened by ajaxCall.", AjaxFailureException.AJAXTYPE_ACTION);
            }
        }
        else {
            ajaxDB = database;
        }
        
        tmlLib = ajaxDB.getTMLModule(ajaxInfo.getTmlmodule(), ajaxInfo.getMediaKey());
        if (tmlLib == null) {
            throw new AjaxFailureException("No WebTML layout '" + ajaxInfo.getTmlmodule() + "' for media key '" + ajaxInfo.getMediaKey() + "' available in app '" + ajaxDB.getDbReference() + "'", AjaxFailureException.AJAXTYPE_ACTION);
        }
        return tmlLib;
    }

    public Map<String, de.innovationgate.wga.server.api.Cookie> fetchHttpCookies(javax.servlet.http.HttpServletRequest request) {
        Map<String,de.innovationgate.wga.server.api.Cookie> cookies = new HashMap<String, de.innovationgate.wga.server.api.Cookie>();
        Cookie[] rawCookies = request.getCookies();
        if (rawCookies != null) {
            for (javax.servlet.http.Cookie c : rawCookies) {
               cookies.put(c.getName(), new de.innovationgate.wga.server.api.Cookie(c)); 
            }
        }
        return cookies;
    }



    public static void saveUserProfile(WGACore core, TMLUserProfile tmlUserProfile, HttpServletRequest request) throws WGAPIException {
        
        if (!tmlUserProfile.isSavedOnEnd()) {
            return;
        }
        
        // Do not save if session not open
        WGUserProfile profile = tmlUserProfile.getprofile();
        if (!profile.getDatabase().isSessionOpen()) {
            return;
        }

        // Do not save if nothing changed, and the profile is already persisted
        if (!profile.isEdited() && profile.isSaved()) {
            return;
        }
        
        // Do never save for new sessions. If the user does not accept the session the profile will be lost forever.
        if (request != null && request.getSession().isNew()) {
            return;
        }

        // Never save for persmode session
        if (tmlUserProfile.getPersMode() == Constants.PERSMODE_SESSION) {
            return;
        }
        
        // In opt in mode we do not save the profile unless it already has been saved (which indicates that the opt-in has happened)
        boolean optInMode = (Boolean) core.readPublisherOptionOrDefault(profile.getDatabase(), WGACore.DBATTRIB_PERSMODE_OPT_IN);
        if (optInMode && !profile.isSaved()) {
            return;
        }
        
        // If no non-save-clause applies we finally save
        profile.save();
        
        
    }

    public static boolean isAuthoringMode(String dbReference, HttpSession session) {

        if (session == null) {
            return false;
        }

        String attName = WGACore.ATTRIB_AUTHORINGMODE + dbReference;
        Boolean bi = (Boolean) session.getAttribute(attName);
        if (bi != null && bi.booleanValue() == true) {
            return true;
        }
        else {
            return false;
        }

    }

    private AjaxInfo readAjaxInformation(javax.servlet.http.HttpServletRequest request, WGDatabase database, String encAjaxInfo) throws WGException {
        WGTMLModule tmlLib;
        AjaxInfo ajaxInfo = null;
        try {
            // decrypt
            byte[] zipped = null;
            try {
                zipped  = this.getCore().getSymmetricEncryptionEngine().decryptBase64Web(encAjaxInfo);
            }
            catch (GeneralSecurityException e) {
                throw new InvalidEncryptionException("AjaxInfo could not be decrypted");
            }
            if (zipped == null) {
                this.getCore().getLog().error("Retrieved AjaxInfo could not be decrypted. Maybe the ajax call was defined for another WGA instance.");
                throw new InvalidEncryptionException("AjaxInfo could not be decrypted");
            }
            // unzip
            String unzipped = WGUtils.unzipString(zipped);
            //String unzipped = new String(zipped, "UTF-8");
            // deserialize
            ajaxInfo = (AjaxInfo) getCore().getDefaultSerializer().fromXML(unzipped);
            // put ajaxInfo on request so root tag can retrieve it
            request.setAttribute(WGACore.ATTRIB_AJAXINFO, ajaxInfo);
        }
        catch (InvalidEncryptionException e) {
            throw new AjaxFailureException("ajax.problem.changedencryption", AjaxFailureException.AJAXTYPE_ACTION);
        }
        catch (Exception e) {
            this.getCore().getLog().error("Retrieved AjaxInfo could not be restored.", e);
            throw new AjaxFailureException("ajax.problem.unknown", AjaxFailureException.AJAXTYPE_ACTION, e);
        }

        return ajaxInfo;
    }

    private void sendNoContentNotification(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, WGDatabase database)
            throws IOException, HttpErrorException, WGException {
    	if(request.getParameter("login")!=null && (!database.isSessionOpen() || database.getSessionContext().isAnonymous())){
            sendRedirect(request, response, getLoginURL(request, database, path.getCompleteURL()));
        }
        else {

            if (isBrowserInterface(request.getSession()) && (path.getPathType() == WGPRequestPath.TYPE_TML || path.getPathType() == WGPRequestPath.TYPE_UNKNOWN_CONTENT)) {
                String url = getNoContentNotificationURL(request, database, path);
                if(url!=null){
                	sendRedirect(request, response, url);
                	return;
                }
            }
            if (path.getTitlePathURL() != null) {
                throw new HttpErrorException(404, "Content '" + path.getTitlePathURL().getPath() + "' does not exist or is not visible for user '"
                        + database.getSessionContext().getUser() + "'", path.getDatabaseKey());
            }
            else {
                throw new HttpErrorException(404, "Content of name/id '" + path.getContentKey() + "' does not exist or is not visible for user '" + database.getSessionContext().getUser() + "'",
                        path.getDatabaseKey());
            }

        }
    }

    private void sendNoFileContainerNotification(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, WGDatabase database)
            throws IOException, HttpErrorException, WGException {

        if (request.getQueryString() != null && request.getQueryString().toLowerCase().indexOf("login") != -1) {
            sendRedirect(request, response, getLoginURL(request, database, path.getCompleteURL()));
        }
        else {
            throw new HttpErrorException(404, "No file container of name " + path.getContainerKey(), path.getDatabaseKey());
        }

    }

    private void removeSessionCookie(javax.servlet.http.HttpServletResponse response, HttpSession session, WGDatabase database) {
        if (!database.hasFeature(WGDatabase.FEATURE_SESSIONTOKEN)) {
            return;
        }

        String cookieName = (String) database.getAttribute(WGACore.DBATTRIB_SESSIONCOOKIE);
        if (cookieName == null) {
            return;
        }

        WGCookie sessionCookie = new WGCookie(cookieName, "");
        sessionCookie.setMaxAge(0);
        sessionCookie.setPath("/");
        String sessionCookieDomain = (String) database.getAttribute(WGACore.DBATTRIB_SESSIONCOOKIEDOMAIN);
        if (sessionCookieDomain != null) {
            sessionCookie.setDomain(sessionCookieDomain);
        }
        sessionCookie.addCookieHeader(response);
        session.removeAttribute(WGACore.SESSION_COOKIESET + cookieName);

    }

    protected void sendRedirect(HttpServletRequest request, javax.servlet.http.HttpServletResponse response, String virtualLink) throws IOException {
        // send redirect always via j2ee method
        // B00004862
    	
    	try {
    		// #00005082: try to use absolute URLs for redirects if possible
			String url = WGA.get(request, response, _core).urlBuilder(virtualLink).build(true);
			response.sendRedirect(response.encodeRedirectURL(url));
		} catch (WGException e) {
			_log.warn("Unable to create absolute Redirect URL. Using relative URL", e);
			response.sendRedirect(response.encodeRedirectURL(virtualLink));
		}
    	
    }

    /**
     * Method isBrowserInterface.
     * 
     * @param request
     * @return boolean
     */
    public static boolean isBrowserInterface(HttpSession session) {

        if (session == null) {
            return false;
        }

        Boolean bi = (Boolean) session.getAttribute(WGACore.ATTRIB_BROWSERINTERFACE);
        if (bi != null && bi.booleanValue() == true) {
            return true;
        }
        else {
            return false;
        }
    }

    public static String buildContentURLID(WGContent content, String mediaKey, boolean isBI) throws WGAPIException {

        String language = content.getLanguage().getName();
        if (!LanguageBehaviourTools.isMultiLanguageDB(content.getDatabase()) && language.equals(content.getDatabase().getDefaultLanguage())) {
            language = DEFAULT_LANGUAGE_TOKEN;
        }

        StringBuffer idBuffer = new StringBuffer();
        String uniqueName = content.getStructEntry().getUniqueName();
        if (WGUtils.isEmpty(uniqueName)) {
            uniqueName = content.getUniqueName();
        }

        if (!WGUtils.isEmpty(uniqueName) && !isBI && content.getStatus().equals(WGContent.STATUS_RELEASE) && content.getDatabase().getBooleanAttribute(WGACore.DBATTRIB_CREATE_NAME_URLS, true) && isValidURLName(uniqueName)) {
            idBuffer.append(uniqueName).append(".");
            idBuffer.append(language).append(".");
            idBuffer.append(mediaKey);
        }
        else {
            idBuffer.append(String.valueOf(content.getStructKey())).append(".");
            idBuffer.append(language).append(".");
            if (content.getStatus().equals(WGContent.STATUS_RELEASE)) {
                idBuffer.append(mediaKey);
            }
            else {
                idBuffer.append(content.getVersion());
            }
        }
        return idBuffer.toString();
    }

    private static boolean isValidURLName(String uniqueName) {

        return (!uniqueName.contains("~"));
        
    }

    public static String buildLayoutURLID(WGDatabase db, String layoutKey, String language, String mediaKey) throws WGAPIException {

        if (db != null && !LanguageBehaviourTools.isMultiLanguageDB(db) && language.equals(db.getDefaultLanguage())) {
            language = DEFAULT_LANGUAGE_TOKEN;
        }

        StringBuffer idBuffer = new StringBuffer();
        idBuffer.append(layoutKey).append(".");
        idBuffer.append(language).append(".");
        idBuffer.append(mediaKey);
        return idBuffer.toString();

    }

    /**
     * retrieves a relative url from session attribute WGACore.ATTRIB_NO_CONTENT_NOTIFCATION_URL to use as
     * NoContentNotificationURL in sendNoContentNotification(...)
     * parameters #DBKEY# and #KEY# are replaced by the corresponding values.
     * @param request
     * @param database
     * @param path
     * @return URL starting with getContextPath();
     */
    private String getNoContentNotificationURL(HttpServletRequest request, WGDatabase database, WGPRequestPath path) {

    	String relativeURL = (String) request.getSession().getAttribute(WGACore.ATTRIB_NO_CONTENT_NOTIFCATION_URL);

    	if (relativeURL == null) {
    		return null;
    	}
    	
        // replace variables in relative URL
        relativeURL = relativeURL.replaceAll("#DBKEY#", database.getAttribute(WGACore.DBATTRIB_DBKEY).toString());
        String contentKey = path.getContentKey();
        if(contentKey!=null)
        	relativeURL = relativeURL.replaceAll("#KEY#", contentKey.toString());
        else relativeURL = relativeURL.replaceAll("#KEY#", "");

        StringBuffer url = new StringBuffer();
        url.append(getContextPath());
        url.append(relativeURL);
        
        return url.toString();
    }

    /**
     * retrieves a relative url from session attribute
     * WGACore.ATTRIB_VIRTUAL_CONTENT_URL to use for virtual content in BI 
     * parameters #DBKEY#, #CONTENTKEY_URL_MODE#, #CONTENTKEY_UNIQUE_MODE# and #STRUCTKEY#
     * are replaced by the corresponding values
     * 
     * @param request
     * @param database
     * @param path
     * @param content
     * @return URL starting with getContextPath();
     * @throws WGAPIException
     */
    private String getVirtualContentURL(HttpServletRequest request, WGDatabase database, WGPRequestPath path, WGContent content) throws WGAPIException {

        String relativeURL = (String)request.getSession().getAttribute(WGACore.ATTRIB_VIRTUAL_CONTENT_URL);
        if(relativeURL==null)
        	return null;
        
        // replace variables in relative URL
        relativeURL = relativeURL.replaceAll("#DBKEY#", database.getAttribute(WGACore.DBATTRIB_DBKEY).toString());
        relativeURL = relativeURL.replaceAll("#CONTENTKEY_URL_MODE#", content.getContentKey(false).toString());
        relativeURL = relativeURL.replaceAll("#CONTENTKEY_UNIQUE_MODE#", content.getContentKey(true).toString());
        relativeURL = relativeURL.replaceAll("#STRUCTKEY#", content.getStructKey().toString());

        StringBuffer url = new StringBuffer();
        url.append(getContextPath());
        url.append(relativeURL);
        return url.toString();
    }

    public static String getCompleteRequestURL(javax.servlet.http.HttpServletRequest request) {
        StringBuffer requestURL = request.getRequestURL();
        String queryString = request.getQueryString();
        return getCompleteRequestURL(requestURL, queryString);
    }

    public static String getCompleteRequestURL(StringBuffer requestURL, String queryString) {
        if (queryString != null) {
            requestURL.append("?").append(queryString);
        }
        return requestURL.toString();
    }

    public static WGContent getContentByAnyKey(String contentKey, WGDatabase database, HttpServletRequest req) throws WGAPIException {

        if (contentKey == null || contentKey.trim().equals("")) {
            return null;
        }

        HttpSession session = req.getSession();
        boolean isBI = isBrowserInterface(session) || isAuthoringMode(database.getDbReference(), session);

        // Inner getContentByAnyKey
        return getContentByAnyKey(contentKey, database, new RequestLanguageChooser(database, req), isBI);

    }

    public static WGContent getContentByAnyKey(String contentKey, WGDatabase database, WGLanguageChooser languageChooser, boolean isBI) throws WGAPIException {
        try {
            URLID urlid = new URLID(contentKey, database);
            return getContentByAnyKey(urlid, database, languageChooser, isBI);
        }
        catch (WGIllegalArgumentException e) {
            return null;
        }
    }

    public static WGContent getContentByAnyKey(URLID id, WGDatabase database, WGLanguageChooser languageChooser, boolean isBI) throws WGAPIException {

        WGLanguage langObj;
        // Block with various attempts to search content by this key
        WGContent content = null;

        // Try to retrieve content via content key - Fastest way
        WGContentKey key = id.asContentKey();
        if (key != null) {
            content = database.getContentByKey(key);
            if (content != null) {
                return content;
            }
        }

        // Try to interpret struct key as unique name
        if (id.getSuffixVersion() == 0 && id.getLanguage() != null) {
            content = database.getContentByName(id.getResourceId(), id.getLanguage());
            if (content != null) {
                return content;
            }
        }

        // From here on we are sure to have no URLID of complete format. We will
        // use the complete id string from here on to make any sense of it.
        id.setCompleteFormat(false);
        String completeId = id.getCompleteId();

        // Try to use key as struct key only. If found, will try to find a
        // content for this struct from the languages list
        WGStructEntry entry = null;
        Object structKey = database.parseStructKey(completeId);
        if (structKey != null) {
            entry = database.getStructEntryByKey(structKey);
            if (entry != null) {
                content = languageChooser.selectContentForPage(entry, isBI);
                if (content != null) {
                    return content;
                }
            }
        }
        
        // Try sequence (hex)
    	try{
        	long seq = Long.parseLong(completeId, 16);
        	if(seq!=0){
        		entry = database.getStructEntryBySequence(seq);
            	if(entry!=null){
                    content = languageChooser.selectContentForPage(entry, isBI);
                    if (content != null)
                        return content;
            	}
        	}
    	}
    	catch(NumberFormatException e){}	// ignore format errors
    
        // Try to use key as name only
        content = languageChooser.selectContentForName(database, completeId, isBI);
        if (content != null) {
            return content;
        }

        // Try to retrieve via Name-Language syntax
        int dividerPos = completeId.lastIndexOf("-");
        if (content == null && dividerPos != -1) {
            String realName = completeId.substring(0, dividerPos);
            String realLanguage = completeId.substring(dividerPos + 1).toLowerCase();

            // Cutoff trailing suffix which might be there for browser
            // compatibility
            if (realLanguage.lastIndexOf(".") != -1) {
                realLanguage = realLanguage.substring(0, realLanguage.lastIndexOf("."));
            }

            if (realLanguage.equals(DEFAULT_LANGUAGE_TOKEN)) {
                realLanguage = database.getDefaultLanguage();
            }

            content = database.getContentByName(realName, realLanguage);
            if (content != null) {
                return content;
            }
        }

        // Last Try :-) Fetch by name only, so browser language
        // configuration cannot prevent you from fetching by name only
        // However, when the name is used by multiple language versions, the
        // first one is returned
        if (content == null) {
            content = database.getAnyContentByName(completeId);
            if (content != null) {
                return content;
            }
        }

        return null;

    }
    


    private void dispatchFileRequest(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws java.lang.Exception {

        WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);

        // Fetch database
        WGDatabase database = path.getDatabase();
        if (info != null) {
            info.setType(WGARequestInformation.TYPE_FILE);
            if (database != null) {
                info.setDatabase(database);
            }
        }

        // Parse container key - This may be a path when we have the /~file/
        // syntax. We use different parts depending on the type of container
        String containerKey = path.getContainerKey();
        List<String> containerKeyElems = WGUtils.deserializeCollection(containerKey, "/");
        String containerKeyLastElement = (String) containerKeyElems.get(containerKeyElems.size() - 1);

        // Fetch the file container
        WGDocument fileContainer = database.getFileContainer(containerKeyLastElement);
        if (fileContainer == null) {

            // Container is content, addressed by some kind of content key?
            WGContent content = getContentByAnyKey(containerKeyLastElement, database, request);

            // Container is content, addressed by Title path?
            if (content == null) {
                TitlePathManager tpm = (TitlePathManager) database.getAttribute(WGACore.DBATTRIB_TITLEPATHMANAGER);
                if (tpm != null && tpm.isGenerateTitlePathURLs()) {
                    TitlePathManager.TitlePath url = tpm.parseTitlePathURL(containerKeyElems);
                    if (url != null) {
                        path.setTitlePathURL(url);
                        content = path.getContentByTitlePath(request);
                    }
                }
            }

            // Nothing found.
            if (content == null || !content.mayBePublished(isBrowserInterface(request.getSession()) || isAuthoringMode(database.getDbReference(), request.getSession()), WGContent.DISPLAYTYPE_NONE)) {
                sendNoFileContainerNotification(path, request, response, database);
                return;
            }

            fileContainer = content;
        }

        if (info != null && fileContainer != null && fileContainer instanceof WGContent) {
            info.setContent((WGContent) fileContainer);
        }

        String fileName = path.getFileName();
        if (fileName == null || fileName.isEmpty()) {
        	path.setFileName("index.html");
        }
        
        if(!fileContainer.hasFile(fileName)){
        	// check, if fileName is a directory and therefore it is part of the container name and should address file 'index.html'
        	WGDocument fileContainer2 = database.getFileContainer(containerKeyLastElement.isEmpty() ? fileName : containerKeyLastElement+":"+fileName);
        	if(fileContainer2!=null){
        		fileContainer = fileContainer2;
        		path.setFileName("index.html");
        	}
        }
        
        String headerModuleName = "wga:header:files";
    	WGCSSJSModule headerModule = database.getScriptModule(headerModuleName, WGCSSJSModule.CODETYPE_TMLSCRIPT);	    	
    	if (headerModule != null) {
    		try {
	    		WGA wga = WGA.get(request, response, _core);
	    		Context headerModuleContext = null;
	    		if (fileContainer instanceof WGContent) {
	    			headerModuleContext = wga.createTMLContext((WGContent) fileContainer, wga.design(database.getDbReference()));
	    		} else {
	    			headerModuleContext = wga.createTMLContext(database, wga.design(database.getDbReference()));
	    		}
	    		wga.tmlscript().runScript(headerModuleContext, headerModule.getCode());
    		} catch (Exception e) {
            	_log.error("Failed to execute header module '" + headerModuleName + "'", e);
            }
    	}        

        ExternalFileServingConfig externalFileServingConfig = _core.getExternalFileServingConfig();

        if (externalFileServingConfig.isEnabled() && database.getBooleanAttribute(WGACore.DBATTRIB_EXTERNAL_FILE_SERVING_ENABLED, false)) {

            // check if file is anonymous accessible
            boolean isAnonymousAccessible = database.isAnonymousAccessible();

            if (isAnonymousAccessible) {
                // perform further document level checks
                if (fileContainer != null && fileContainer instanceof WGContent) {
                    // check status
                    if (!((WGContent) fileContainer).getStatus().equals(WGContent.STATUS_RELEASE)) {
                        isAnonymousAccessible = false;
                    }
                    else {
                    	// check readers
                    	isAnonymousAccessible = ((WGContent) fileContainer).isPublic();
                    }
                }
            }

            if (isAnonymousAccessible) {
                dispatchFileExternalImpl(externalFileServingConfig, path, request, response, database, fileContainer);
            }
            else {
                dispatchFileDefaultImpl(path, request, response, database, fileContainer);
            }
        }
        else {
            dispatchFileDefaultImpl(path, request, response, database, fileContainer);
        }

    }



    private void dispatchFileExternalImpl(ExternalFileServingConfig config, WGPRequestPath path, HttpServletRequest request, HttpServletResponse response, final WGDatabase database,
            final WGDocument fileContainer) throws WGAPIException, IOException, HttpErrorException, WGException {

    	final String filename = path.getFileName();
    	
    	PublishingFile publishingFile = new DocumentPublishingFile(this, fileContainer, filename);
    	String derivate = request.getParameter(URLPARAM_DERIVATE);
        if (derivate != null) {
            DerivateQuery derivateQuery = getCore().getFileDerivateManager().parseDerivateQuery(derivate);
            DocumentPublishingFile docPublishingFile = (DocumentPublishingFile) publishingFile;
            WGFileDerivateMetaData derivateMd = docPublishingFile.queryDerivate(derivateQuery, new ClientHints());
            if (derivateMd != null) {
                publishingFile = new DerivatePublishingFile(this, docPublishingFile.getContainer(), derivateMd);
            }
            else if (!isFallbackToOriginalOnDerivateQuery(derivateQuery, publishingFile)) {
                throw new WGNotSupportedException("Derivate queries are not supported on this file type");
            }                                                
        }
        
        if(publishingFile.getFileSize()<config.getThreshold()){
        	dispatchFileDefaultImpl(path, request, response, database, fileContainer);
        	return;
        }
        	
        final PublishingFile thePublishingFile = publishingFile;	// make it final for thread handling
        final File dbRoot = config.getRootForDB(database.getDbReference());
        if (!dbRoot.exists()) {
            dbRoot.mkdir();
        }
        
        File externalCacheFolder = new File(dbRoot, fileContainer.getDocumentKey().replaceAll("/", ":"));
        if (!externalCacheFolder.exists()) {
            externalCacheFolder.mkdir();
        }

        if (!externalCacheFolder.exists()) {
            // fallback to normal dispatch
            _log.warn("Unable to create external file cache directory '" + externalCacheFolder.getAbsolutePath() + "'. Performing file dispatch in default mode.");
            dispatchFileDefaultImpl(path, request, response, database, fileContainer);
            return;
        }
        
    	String cacheFileName = "[" + filename + "]";
    	if(path.getQueryString()!=null)
    		cacheFileName += path.getQueryString().hashCode();
    	String mimeType = publishingFile.getContentType();
    	cacheFileName += "." + WGFactory.getMimetypeDeterminationService().determineSuffixByMimeType(mimeType);
    	
    	final File cachedFile = new File(externalCacheFolder, cacheFileName);

        if (cachedFile.exists()) {
            // perform redirect
        	//_log.info("redirect " + filename + " (" + path.getQueryString() + ") " + publishingFile.getContentType() + ") to " + cacheFileName);
        	response.sendRedirect(config.getRootURL() + database.getDbReference() + "/" + fileContainer.getDocumentKey().replaceAll("/", ":") + "/" + cacheFileName);
        }
        else {
            // cache file
            Thread cacheTask = new Thread() {

                @Override
                public void run() {
                    synchronized (WGPDispatcher.class.getName() + ".filecache." + cachedFile.getAbsolutePath().intern()) {
                        try {
                            database.openSession();

                            File temp = new File(cachedFile.getParent(), UIDGenerator.generateUID() + ".tmp");
                            InputStream in = null;
                            OutputStream out = null;
                            try {
                                in = thePublishingFile.getInputStream();
                                out = new FileOutputStream(temp);
                                WGUtils.inToOut(in, out, 1024);
                            }
                            finally {
                                if (in != null) {
                                    try {
                                        in.close();
                                    }
                                    catch (IOException e) {}
                                }
                                if (out != null) {
                                    try {
                                        out.close();
                                    }
                                    catch (IOException e) {}
                                }
                            }
                            temp.renameTo(cachedFile);
                        }
                        catch (Throwable e) {
                            _log.error("Unable to create cache for external file serving.", e);
                        }
                        finally {
                            WGFactory.getInstance().closeSessions();
                        }
                    }
                }

            };

            cacheTask.start();
            

            dispatchFileDefaultImpl(path, request, response, database, fileContainer);
        }

    }

    private void dispatchFileDefaultImpl(WGPRequestPath path, HttpServletRequest request, HttpServletResponse response, WGDatabase database, WGDocument fileContainer) throws IOException,
            WGAPIException, HttpErrorException, WGException {
        response.setDateHeader("Date", System.currentTimeMillis());

        // Set expiration time
        int fileExpirationMinutes = ((Integer) _core.readPublisherOptionOrDefault(database, WGACore.DBATTRIB_FILEEXPIRATION_MINUTES)).intValue();
        if (fileContainer instanceof WGContent && ((WGContent) fileContainer).getStatus().equals(WGContent.STATUS_DRAFT)) {
            fileExpirationMinutes = 0;
        }
        
        if (fileExpirationMinutes > 0) {
            int fileExpirationSeconds = fileExpirationMinutes * 60;

            // check if file is anonymous accessible
            boolean isAnonymousAccessible = database.isAnonymousAccessible();
            if (isAnonymousAccessible) {
                // perform further document level checks
                if (fileContainer != null && fileContainer instanceof WGContent) {
                    // check status
                    if (!((WGContent) fileContainer).getStatus().equals(WGContent.STATUS_RELEASE)) {
                        isAnonymousAccessible = false;
                    }                    
                    else {
                    	// check readers
                        isAnonymousAccessible = ((WGContent) fileContainer).isPublic(); 
                    }
                }
            }

            if (isAnonymousAccessible) {
            	response.setHeader("Cache-Control", "public, max-age=" + fileExpirationSeconds);
            }
            else response.setHeader("Cache-Control", "private, max-age=" + fileExpirationSeconds);
        }

        // Create publishing file object, which will provide the data
        PublishingFile publishingFile = new DocumentPublishingFile(this, fileContainer, path.getFileName());
        if (!publishingFile.isPublishable()) {
            throw new HttpErrorException(403, "Files from this container may not be published", path.getDatabaseKey());
        }
        if (!publishingFile.isAvailable()) {
            throw new HttpErrorException(404, "File not found: " + publishingFile.getName(), path.getDatabaseKey());
        }

        String designEncoding = (String) database.getAttribute(WGACore.DBATTRIB_DESIGN_ENCODING);
        FileCache fileCache = (FileCache) database.getAttribute(WGACore.DBATTRIB_FILECACHE);
        
        dispatchPublishingFile(publishingFile, request, response, designEncoding, fileCache, path);
    }

    private void dispatchPublishingFile(PublishingFile publishingFile, HttpServletRequest request, HttpServletResponse response, String textOutputEncoding, FileCache fileCache, WGPRequestPath path)
            throws WGException, HttpErrorException, IOException {
        
        // Collect HTTP client hints (if enabled)
        ClientHints clientHints = new ClientHints();
        boolean useHttpClientHints = false;
        if (publishingFile.getDatabase() != null) {
            Database database = WGA.get(request, response, getCore()).database(publishingFile.getDatabase());
            if (database instanceof App && ((Boolean) database.getPublisherOption(WGACore.DBATTRIB_USE_NONFINAL_HT_FEATURES)) == true) {
                useHttpClientHints = true;
            }
        }
        
        if (useHttpClientHints) {
            String dprStr = request.getHeader("CH-DPR");
            if (dprStr != null) {
                try {
                    clientHints.setDevicePixelRatio(Float.valueOf(dprStr));
                }
                catch (NumberFormatException e) {
                    getCore().getLog().warn("Client uses unparseable device pixel ratio: " + dprStr);
                }
            }
        }
        
        // Optionally select derivate
        Float usedDevicePixelRatio = null;
        try {
        
            String derivate = request.getParameter(URLPARAM_DERIVATE);
            if (derivate != null) {
            	
                DerivateQuery derivateQuery=null;
                try{
					derivateQuery = FileDerivateManager.parseDerivateQuery(derivate);
                }
                catch(WGInvalidDerivateQueryException e){
                	// parameter may be encoded. Try with decoded version
					derivateQuery = FileDerivateManager.parseDerivateQuery(URIUtil.decode(derivate));
                }
                
                if (publishingFile instanceof DocumentPublishingFile) {
                    DocumentPublishingFile docPublishingFile = (DocumentPublishingFile) publishingFile;
                    WGFileDerivateMetaData derivateMd = docPublishingFile.queryDerivate(derivateQuery, clientHints);
                    if (derivateMd != null) {
                        usedDevicePixelRatio = docPublishingFile.getUsedDevicePixelRatio();
                        publishingFile = new DerivatePublishingFile(this, docPublishingFile.getContainer(), derivateMd);
                        
                    }
                }
                else if (!isFallbackToOriginalOnDerivateQuery(derivateQuery, publishingFile)) {
                    throw new WGNotSupportedException("Derivate queries are not supported on this file type");
                }
                
            }
        
        }
        catch(WGNotSupportedException | WGInvalidDerivateQueryException | WGFailedDerivateQueryException e) {
        	_log.error("Request " + request.getRequestURI() + " from " + request.getRemoteAddr() + ": Invalid derivate query '" + request.getParameter(URLPARAM_DERIVATE) + "': " + e.getMessage());
        }
        /*
        catch (WGNotSupportedException e) {
            throw new HttpErrorException(412, e.getMessage(), path.getDatabaseKey());
        }
        catch (WGInvalidDerivateQueryException e) {
            //throw new HttpErrorException(400, "Invalid derivate query: " + e.getMessage(), path.getDatabaseKey());
        	_log.error(path.getDatabaseKey() + ": Invalid derivate query '" + request.getParameter(URLPARAM_DERIVATE) + "': " + e.getMessage());
        }
        catch (WGFailedDerivateQueryException e) {
            throw new HttpErrorException(412, "No derivate of file '" + publishingFile.getFileName() + "' matches the derivate query", path.getDatabaseKey());
        }
        */
        
        // Put out the used device pixel ratio, if any
        if (usedDevicePixelRatio != null) {
            response.setHeader("Vary", "CH-DPR");
            response.setHeader("DPR", usedDevicePixelRatio.toString());
        }

        // Handle browser cache
        long lastModified = determinePublishingFileLastModified(publishingFile, request, response);

        if (browserCacheIsValid(request, lastModified, publishingFile.getETag())) {
            response.setStatus(HttpServletResponse.SC_NOT_MODIFIED);
            return;
        }
        else {
            response.setDateHeader("Last-Modified", lastModified);
            response.setHeader("ETag", '"' + publishingFile.getETag() + '"');
        }

        
        // Optionally inject online scaling information to file object
        String maxHeightStr = request.getParameter(URLPARAM_MAXHEIGHT);
        String maxWidthStr = request.getParameter(URLPARAM_MAXWIDTH);
        if (maxHeightStr != null || maxWidthStr != null) {
            try {
                int maxHeight = -1;
                if (maxHeightStr != null) {
                    maxHeight = Integer.parseInt(maxHeightStr);
                }
                int maxWidth = -1;
                if (maxWidthStr != null) {
                    maxWidth = Integer.parseInt(maxWidthStr);
                }
                publishingFile.setOnlineScaling(maxWidth, maxHeight, clientHints);
            }
            catch (NumberFormatException e) {
                getCore().getLog().error("Unparseable online scaling metrics", e);
            }
            catch (Exception e) {
                getCore().getLog().error("Exception setting online scaling information", e);
            }
        }
        
        // Put out content type
        String contentType = publishingFile.getContentType();
        if (contentType == null) {
            contentType = "application/octet-stream";
        }
        response.setContentType(contentType);

        // Content Disposition Header - Either if download forced or a disposition filename has been specified
        Boolean forceDownload = Boolean.parseBoolean(request.getParameter("forcedownload"));
        if (forceDownload) {
            response.setHeader("Content-disposition", "attachment" + (publishingFile.getDispositionFileName() != null ? ";filename=" + publishingFile.getDispositionFileName() : ""));
        }
        else if (publishingFile.getDispositionFileName() != null) {
            response.setHeader("Content-disposition", "inline;filename=" + publishingFile.getDispositionFileName());
        }

        // Allow accept ranges on all CS with optimized file handling and binary responses
        if (publishingFile.isAllowAcceptRanges() && isBinary(request, response)) {
            response.setHeader("Accept-Ranges", "bytes");
        }

        try {
            // Look if file is cached - If so, send it and exit
            if (fileCache != null) {
                boolean outputHandled = dispatchFileWithCache(publishingFile, request, response, fileCache, lastModified, textOutputEncoding, contentType, path);
                if (outputHandled) {
                    return;
                }
            }
            
            // We serve from cache so must retrieve the file. Test for availability now which may load the document.
            if (!publishingFile.isAvailable()) {
                throw new HttpErrorException(404, "File not found: " + publishingFile.getName(), path.getDatabaseKey());
            }
            
            // File is above threshold and not in cache - serve from database
            writeData(publishingFile, request, response, publishingFile.getTextEncoding(), publishingFile.getFileSize(), publishingFile.getSourceHint(),  publishingFile.isAllowAcceptRanges() && isBinary(request, response));
        }
        catch (java.net.SocketException exc) {
            _log.warn("Dispatch of file request failed bc. of socket error: " + exc.getMessage());
        }
        catch (java.io.IOException exc) {
            if (!exc.getClass().getName().equals("org.apache.catalina.connector.ClientAbortException")) {
                _log.warn("Dispatch of file request failed bc. of IO error: " + exc.getMessage());
            }
        }
        catch (HttpErrorException exc) {
            throw exc;
        }
        catch (Exception exc) {
            _log.error("Exception dispatching file " + publishingFile.getName(), exc);
        }
    }
    
    /**
     * Determine if we may fallback to the original because the backend does not support derivates
     * @param derivateQuery The derivate query
     * @return true if the original should be served as fallback, false otherwise
     */
    protected boolean isFallbackToOriginalOnDerivateQuery(DerivateQuery derivateQuery, PublishingFile publishingFile) {
        String contentType = publishingFile.getContentType();
        
        
        // If the usage is "poster" and the original is an image we may return it
        DerivateQueryTerm usage  = derivateQuery.get(DerivateQuery.QUERYTERM_USAGE);
        if (usage != null && usage.getValue().equals(WGFileDerivateMetaData.USAGE_POSTER) && contentType.startsWith("image/")) {
            return true;
        }
        
        // If the original has the same mime as request we may return it
        DerivateQueryTerm type  = derivateQuery.get(DerivateQuery.QUERYTERM_TYPE);
        if (type != null && TypeQueryTermProcessor.testMimeTypeMatch(contentType, type.getValue()) > 0) {
            return true;
        }

        // All others: Do not serve
        return false;
    }

    /**
     * Determine lastModified for a publishing file, respecting also changes of server output and design encoding 
       Last modified of binary response depends only on resource change date.
       Last change date of textual response additionally depends on character encoding change date.
     * @param publishingFile
     * @param request
     * @param response
     * @return
     * @throws WGAPIException
     */
    private long determinePublishingFileLastModified(PublishingFile publishingFile, HttpServletRequest request, HttpServletResponse response) throws WGAPIException {
        long lastModified;
        if (isBinary(request, response)) {
            lastModified = publishingFile.getLastModifiedTime();
        }
        else {
            lastModified = Math.max(publishingFile.getLastModifiedTime(), _core.getCharacterEncodingLastModified());
            WGDatabase fileDb = publishingFile.getDatabase();
            if (fileDb != null) {
                lastModified = Math.max(lastModified, _core.getDesignEncodingLastModified(fileDb.getDbReference()));
            }
        }
        return lastModified;
    }



    private boolean dispatchFileWithCache(PublishingFile publishingFile, HttpServletRequest request, HttpServletResponse response, FileCache fileCache, long lastModified, 
            String designEncoding, String contentType, WGPRequestPath path) throws HttpErrorException, IOException, WGAPIException {
        boolean outputHandled = false;
        byte[] data = fileCache.getFile(publishingFile, lastModified);
        
        // Serve from cache
        if (data != null) {
            try {
                // B000041DA
                ByteArrayDataSource dataIn = new ByteArrayDataSource(data, publishingFile.getFileName(), contentType);
                writeData(dataIn, request, response, publishingFile.getTextEncoding(), data.length, publishingFile.getSourceHint(), true);
            }
            catch (java.net.SocketException exc) {
                _log.warn("Dispatch of cached file request failed bc. of socket error: " + exc.getMessage());
            }
            catch (java.io.IOException exc) {
                if (!exc.getClass().getName().equals("org.apache.catalina.connector.ClientAbortException")) {
                    _log.warn("Dispatch of cached file request failed bc. of IO error: " + exc.getMessage());
                }
            }
            outputHandled = true;
        }

        // Try to put to cache, if below threshold
        else {
            
            // First test if the file is available
            if (!publishingFile.isAvailable()) {
                throw new HttpErrorException(404, "File not found: " + publishingFile.getName(), path.getDatabaseKey());
            }
            
            // Look if file size is below cache threshold - if so, collect data and
            // put into cache, then serve
            long threshold = fileCache.getThreshold();
            long fileSize = publishingFile.getFileSize();
            if (fileSize > 0 && fileSize <= threshold) {
       
                // Put into cache
                InputStream inputStream = publishingFile.getInputStream();
                try {
                    ByteArrayOutputStream outCache = new ByteArrayOutputStream(2048);
                    WGUtils.inToOut(inputStream, outCache, 2048);
                    data = outCache.toByteArray();
                    fileCache.putFile(publishingFile, data, lastModified);
                }
                catch (java.net.SocketException exc) {
                    _log.warn("Caching of file request failed bc. of socket error: " + exc.getMessage());
                }
                catch (java.io.IOException exc) {
                    if (!exc.getClass().getName().equals("org.apache.catalina.connector.ClientAbortException")) {
                        _log.warn("Caching of file request failed bc. of IO error: " + exc.getMessage());
                    }
                }
                finally {
                    if (inputStream != null) {
                        try {
                            inputStream.close();
                        }
                        catch (Exception e) {
                        }
                    }
                }
       
                // Writing from cache to out
                try {
                    // B000041DA
                    ByteArrayDataSource dataIn = new ByteArrayDataSource(data, publishingFile.getFileName(), contentType);
                    if (designEncoding != null) {
                        writeData(dataIn, request, response, designEncoding, data.length, publishingFile.getSourceHint(), true);
                    }
                    else {
                        writeData(dataIn, request, response, null, data.length, publishingFile.getSourceHint(), true);
                    }
                }
                catch (java.net.SocketException exc) {
                    _log.warn("Dispatch of file request failed bc. of socket error: " + exc.getMessage());
                }
                catch (java.io.IOException exc) {
                    if (!exc.getClass().getName().equals("org.apache.catalina.connector.ClientAbortException")) {
                        _log.warn("Dispatch of file request failed bc. of IO error: " + exc.getMessage());
                    }
                }
       
                outputHandled = true;
                
            }
        }
        return outputHandled;
    }

    private void dispatchResourceRequest(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws Exception {

        WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
        if (info != null) {
            info.setType(WGARequestInformation.TYPE_RESOURCE);
        }

        // determine mimeType
        String resourcePath = path.getResourcePath();
        String mimeType = this.getServletContext().getMimeType(resourcePath);
        if (mimeType == null) {
            mimeType = "application/octet-stream";
        }
        response.setContentType(mimeType);

        File file = null;
        boolean isTemporaryDownload = false;
        if (path.getPathCommand().equals(WGPRequestPath.PATHCMD_TEMP_DOWNLOAD)) {
            String tempFileName = path.getResourcePath().substring(path.getResourcePath().lastIndexOf("/") + 1);
            TemporaryDownloadsMap temporaryDownloads = (TemporaryDownloadsMap) request.getSession().getAttribute(SESSION_TEMPORARYDOWNLOADS);
            if (temporaryDownloads != null) {
                TemporaryDownload tempDownload = (TemporaryDownload) temporaryDownloads.get(tempFileName);
                if (tempDownload != null) {
                    file = tempDownload.getTempFile().getFile();
                    isTemporaryDownload = true;
                }
            }
        }
        else {
            file = new File(this.getServletContext().getRealPath("/"), path.getResourcePath());
        }

        if (file == null || !file.exists() || !file.isFile()) {
            throw new HttpErrorException(404, "File not found: " + path.getResourcePath(), null);
        }

        // / Set expiration time
        int fileExpirationMinutes = getCore().getWgaConfiguration().getCacheExpirationForStaticResources();
        if (fileExpirationMinutes > 0) {
            int fileExpirationSeconds = fileExpirationMinutes * 60;
            response.setHeader("Cache-Control", "max-age=" + fileExpirationSeconds + ", must-revalidate");
        }

        // determine lastModified
        // - last modified of binary response depends only on resource change
        // date
        // - last change date of textual response additionally depends on
        // character encoding change date
        long lastModified;
        if (isBinary(request, response)) {
            lastModified = file.lastModified();
        }
        else {
            lastModified = Math.max(file.lastModified(), _core.getCharacterEncodingLastModified());
        }
        if (browserCacheIsValid(request, lastModified, String.valueOf(lastModified))) {
            response.setStatus(HttpServletResponse.SC_NOT_MODIFIED);
            return;
        }
        else {
            response.setDateHeader("Last-Modified", lastModified);
            response.setHeader("ETag", '"' + String.valueOf(lastModified) + '"');
        }

        if (mimeType.startsWith("application/")) {
            response.setHeader("Cache-Control", "must-revalidate, post-check=0, pre-check=0");
        }

        // Dispatch to file resource
        try {
            if (!"HEAD".equalsIgnoreCase(request.getMethod())) {
                DataSource in = null;
                int fileSize = 0;

                // We allow direct file dispatching only for temporary files. In
                // all other cases we use the servlet
                // context which will take care of file system security.
                if (isTemporaryDownload) {
                    response.setHeader("Content-Disposition", "attachment;filename=" + file.getName());
                    in = new URLDataSource(file.toURI().toURL());
                    fileSize = (int) file.length();
                }
                else {
                    in = new URLDataSource(this.getServletContext().getResource(resourcePath));
                }

                // B000041DA
                // write data to response - use UTF-8 when reading characters -
                // WGA resources are UTF-8 encoded
                writeData(in, request, response, "UTF-8", fileSize, null, true);
            }
        }
        catch (java.net.SocketException exc) {
            _log.warn("Dispatch of resource request failed bc. of socket error: " + exc.getMessage());
        }
        catch (java.io.IOException exc) {
            _log.warn("Dispatch of resource request failed bc. of IO error: " + exc.getMessage());
        }
    }

    /**
     * writes the given data to the response based on response.getContentType()
     * either reader/writer (for textual content) or binary streams are used
     * 
     * @param data
     *            - the data as inputstream to write - implicit closed after
     *            method call
     * @param request
     * @param response
     * @param characterEncoding
     *            - encoding to use when reading character based from given
     *            data, if null platform encoding is used
     * @throws IOException
     * @throws HttpErrorException
     */
    private void writeData(DataSource data, HttpServletRequest request, HttpServletResponse response, String characterEncoding, long size, String sourceHint, boolean allowRequestRanges) throws IOException,
            HttpErrorException {
        

        // Parse accept ranges if requested
        List<AcceptRange> ranges = new ArrayList<AcceptRange>();
        String rangeHeader = request.getHeader("Range");
        if (rangeHeader != null && allowRequestRanges && size > 0) {
            if (rangeHeader.startsWith("bytes=")) {
                rangeHeader = rangeHeader.substring("bytes=".length());
                String[] subranges = rangeHeader.split("\\s*,\\s*");
                for (String r : subranges) {
                    Matcher matcher = ACCEPT_RANGE_PATTERN.matcher(r);
                    if (matcher.matches()) {
                        String from = matcher.group(1);
                        String to = matcher.group(2);
                        AcceptRange newRange;

                        try {
                            newRange = new AcceptRange(from != null ? Integer.parseInt(from) : -1, to != null ? Integer.parseInt(to) : -1);
                            if (newRange.from == -1) {
                                newRange.from = size - newRange.to;
                                newRange.to = size - 1;
                            }
                            else if (newRange.to == -1) {
                                newRange.to = size - 1;
                            }
                        }
                        catch (NumberFormatException e) {
                            response.addHeader("Content-Range", "bytes */" + size);
                            response.sendError(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
                            return;
                        }

                        if ((newRange.from == -1 && newRange.to == -1) || newRange.to > size) {
                            response.addHeader("Content-Range", "bytes */" + size);
                            response.sendError(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
                            return;
                        }
                        ranges.add(newRange);
                    }
                    else {
                        response.addHeader("Content-Range", "bytes */" + size);
                        response.sendError(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
                        return;
                    }
                }
            }
            else {
                response.addHeader("Content-Range", "bytes */" + size);
                response.sendError(HttpServletResponse.SC_REQUESTED_RANGE_NOT_SATISFIABLE);
                return;
            }
        }

        if (ranges.isEmpty()) {
            // We can only set the raw length on binary resources bc.
            // transcoding textual response can change the size
            if (isBinary(request, response) && size > 0) {
                response.setContentLength((new Long(size)).intValue());
            }

            if (!"HEAD".equalsIgnoreCase(request.getMethod())) {
                writeWholeData(data, request, response, characterEncoding, sourceHint);
            }
            
        }
        else {
            response.setStatus(HttpServletResponse.SC_PARTIAL_CONTENT);
            if (ranges.size() == 1) {
                AcceptRange r = ranges.get(0);
                response.addHeader("Content-Range", "bytes " + r.from + "-" + r.to + "/" + size);
                response.setContentLength((new Long(r.to - r.from + 1)).intValue());
            }
            else {
                response.setContentType("multipart/byteranges; boundary=" + BYTERANGE_BOUNDARY);
            }
            
            if (!"HEAD".equalsIgnoreCase(request.getMethod())) {
                writeRangesData(data, ranges, response, sourceHint, size);
            }
        }

    }

    private void writeWholeData(DataSource data, HttpServletRequest request, HttpServletResponse response, String characterEncoding, String dbHint) throws IOException, HttpErrorException, UnsupportedEncodingException {

        Reader reader = null;
        InputStream in = data.getInputStream();
        if (in == null) {
            throw new HttpErrorException(404, "File not found: " + data.getName(), dbHint);
        }

        try {
            if (!isBinary(request, response)) {
                // this is a textual reponse
                if (characterEncoding != null && !characterEncoding.equals(response.getCharacterEncoding())) {
                    reader = new InputStreamReader(in, characterEncoding);
                    WGUtils.inToOut(reader, response.getWriter(), 2048);
                }
                else {
                    WGUtils.inToOut(in, response.getOutputStream(), 2048);
                }
            }
            else {
                WGUtils.inToOut(in, response.getOutputStream(), 2048);
            }
        }
        finally {
            if (reader != null) {
                try {
                    reader.close();
                }
                catch (IOException e) {
                }
            }
            if (in != null) {
                try {
                    in.close();
                }
                catch (IOException e) {
                }
            }
        }
    }

    private void writeRangesData(DataSource data, List<AcceptRange> ranges, HttpServletResponse response, String dbHint, long size) throws IOException, HttpErrorException, UnsupportedEncodingException {

        ServletOutputStream out = response.getOutputStream();
        for (AcceptRange range : ranges) {
            InputStream in = data.getInputStream();
            if (in == null) {
                throw new HttpErrorException(404, "File not found: " + data.getName(), dbHint);
            }
            
            if (ranges.size() != 1) {
                out.println();
                out.println("--" + BYTERANGE_BOUNDARY);
                out.println("Content-Type: " + data.getContentType());
                out.println("Content-Range: bytes " + range.from + "-" + range.to + "/" + size);
                out.println();
            }
            
            if (range.from > 0) {
                in.skip(range.from);
            }
    
            try {
                WGUtils.inToOutLimited(in, out, (new Long(range.to - range.from + 1)).intValue(), 2048);
                out.flush();
            }
            finally {
                if (in != null) {
                    try {
                        in.close();
                    }
                    catch (IOException e) {
                    }
                }
            }
        }
        
        if (ranges.size() != 1) {
            out.println();
            out.print("--" + BYTERANGE_BOUNDARY + "--");
            out.flush();
        }
        
    }

    private void dispatchCssjsRequest(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws java.lang.Exception {

        WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
        if (info != null) {
            info.setType(WGARequestInformation.TYPE_SCRIPT);
        }

        // Fetch the database
        WGDatabase database = path.getDatabase();

        // Fetch the library
        String codeType;
        switch (path.getPathType()) {

            case WGPRequestPath.TYPE_CSS:
                codeType = WGScriptModule.CODETYPE_CSS;
                break;

            case WGPRequestPath.TYPE_JS:
                codeType = WGScriptModule.CODETYPE_JS;
                break;

            default:
                throw new HttpErrorException(500, "Invalid path type to dispatch a css/js request: " + path.getPathType(), path.getDatabaseKey());

        }

        WGCSSJSModule lib = database.getScriptModule(path.getCssjsKey(), codeType);
        if (lib == null) {
            if (codeType.equals(WGScriptModule.CODETYPE_CSS) && path.getCssjsKey().endsWith(".css")) {
                lib = database.getScriptModule(path.getCssjsKey().substring(0, path.getCssjsKey().length() - 4), codeType);
            }
            else if (codeType.equals(WGScriptModule.CODETYPE_JS) && path.getCssjsKey().endsWith(".js")) {
                lib = database.getScriptModule(path.getCssjsKey().substring(0, path.getCssjsKey().length() - 3), codeType);
            }
        }
        
        if (lib == null) {
            throw new HttpErrorException(404, "No css/js resource of name " + path.getCssjsKey(), path.getDatabaseKey());
        }
        
        // determine mime type and encoding
        String mimeType;
        String libType = lib.getCodeType();
        if (libType.equals(WGCSSJSModule.CODETYPE_CSS)) {
            mimeType = "text/css";
        }
        else if (libType.equals(WGCSSJSModule.CODETYPE_JS)) {
            mimeType = "text/javascript";
        }
        else {
            mimeType = "text/" + libType;
        }
        
        // Reading and post processing of code
        PostProcessResult result = postProcessDesignResource(lib, request, response);
        String code;
        if (result != null) {
            code = result.getCode();
            if (result.getMimeType() != null) {
                mimeType = result.getMimeType();
            }
        }
        else {
            code = lib.getCode();
        }
        
        response.setContentType(mimeType);

        // Set expiration time
        int fileExpirationMinutes = ((Integer) _core.readPublisherOptionOrDefault(database, WGACore.DBATTRIB_FILEEXPIRATION_MINUTES)).intValue();
        if (fileExpirationMinutes > 0) {
            int fileExpirationSeconds = fileExpirationMinutes * 60;
            boolean isAnonymousAccessible = database.isAnonymousAccessible();
            if(isAnonymousAccessible)
            	response.setHeader("Cache-Control", "public, max-age=" + fileExpirationSeconds);
            else response.setHeader("Cache-Control", "private, max-age=" + fileExpirationSeconds);
        }

        // determine lastModified
        // - last modified of binary response depends only on resource change
        // date
        // - last change date of textual response additionally depends on
        // character encoding change date
        long lastModified;
        if (isBinary(request, response)) {
            lastModified = lib.getLastModified().getTime();
        }
        else {
            lastModified = Math.max(lib.getLastModified().getTime(), _core.getCharacterEncodingLastModified());
            lastModified = Math.max(lastModified, _core.getDesignEncodingLastModified(database.getDbReference()));
        }
        
        String eTag = String.valueOf(code.hashCode()) + "-" + String.valueOf(_core.getDesignEncodingLastModified(database.getDbReference()));
        if (browserCacheIsValid(request, null, eTag)) {
            response.setStatus(HttpServletResponse.SC_NOT_MODIFIED);
            response.setHeader("ETag", '"' + eTag + '"');
            return;
        }
        else {
            response.setHeader("ETag", '"' + eTag + '"');
        }

        // If this is a head request we are finished now
        if ("HEAD".equalsIgnoreCase(request.getMethod())) {
            return;
        }

        try {
            java.io.Writer out = response.getWriter();
            out.write(code);
        }
        catch (java.net.SocketException exc) {
            _log.warn("Dispatch of css/js request failed bc. of socket error: " + exc.getMessage());
        }
        catch (java.io.IOException exc) {
            _log.warn("Dispatch of css/js request failed bc. of IO error: " + exc.getMessage());
        }
    }

    public PostProcessResult postProcessDesignResource(WGCSSJSModule lib, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws WGException, InstantiationException, IllegalAccessException {
    	return postProcessDesignResource(lib, request, response, true);
    }

    public PostProcessResult postProcessDesignResource(WGCSSJSModule lib, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response, Boolean compress) throws WGException, InstantiationException, IllegalAccessException {
        
        @SuppressWarnings("unchecked")
        Class<? extends PostProcessor> postProcessorClass = (Class<? extends PostProcessor>) lib.getExtensionData(DesignFileDocument.EXTDATA_POSTPROCESSOR);
        if (postProcessorClass == null) {
            return null;
        }
        
        PostProcessData data = new PostProcessData();
        data.setDocument(lib);
        data.setCacheQualifier(null);
        data.setCompress(compress);
        
        // Create processor and give it the opportunity to fill caching data
        PostProcessor processor = postProcessorClass.newInstance();
        
        WGA wga = WGA.get(request, response, _core);
        processor.prepare(wga, data);
        
        // Try to read cached result
        PostprocessedResourcesCache cache = null;
        if (data.isCacheable()) {
            cache = (PostprocessedResourcesCache) lib.getDatabase().getAttribute(WGACore.DBATTRIB_PPRCACHE);
        }
        
        if (cache != null) {
            PostProcessResult result = cache.getResource(wga, data);
            if (result != null) 
                return result;

        	synchronized (cache) {
        		// postProcess and update cache.
        		// read again because another thread may have filled the cache while I am blocked
	            result = cache.getResource(wga, data);
	            if (result != null) 
	                return result;
            
                // Process and optionally cache the result
                result = processor.postProcess(wga, data, lib.getCode());
                if (data.isCacheable()) {
                    cache.putResource(data, result); 
                    wga.getLog().info("Added to PostProcessResource Cache: " + lib.getDesignReference().toString() + ": " + result.getCode().length() + " Bytes");
                }
				return result;
			}
        }
        else return processor.postProcess(wga, data, lib.getCode());
    }

    /**
     * @see GenericServlet#init(ServletConfig)
     */
    public void init(ServletConfig config) throws ServletException {

        super.init(config);
        _log = Logger.getLogger("wga.publisher");

        this.getServletContext().setAttribute(WGACore.ATTRIB_DISPATCHER, this);
        this._core = (WGACore) getServletContext().getAttribute(WGACore.ATTRIB_CORE);
        if (this._core == null) {
            _log.fatal("Could not connect to WGACore");
        }

        _tmlDebugger = new WebTMLDebugger(this);

        this.setServePages(true);
        _log.debug("OpenWGA Publisher initialized");

    }

    public WGACore getCore() {
        return _core;
    }

    /**
     * @see GenericServlet#destroy()
     */
    public void destroy() {

        shutdown();
        super.destroy();
    }

    public void shutdown() {
        this.setServePages(false);

    }

    public String getLoginURL(HttpServletRequest req, WGDatabase database, String sourceURL) throws UnsupportedEncodingException, WGException {
        return _core.retrieveURLBuilder(req, database).buildLoginURL(database, req, sourceURL);
    }

    public String getAdminLoginURL(HttpServletRequest req, String sourceURL) throws UnsupportedEncodingException, URIException {

        StringBuffer loginURL = new StringBuffer();
        loginURL.append(getPublisherURL(req)).append("/login.jsp");

        String parameterIntroducor = "?";
        if (loginURL.indexOf("?") != -1) {
            parameterIntroducor = "&";
        }

        loginURL.append(parameterIntroducor).append("domain=").append(WGACore.DOMAIN_ADMINLOGINS).append("&redirect=").append(getCore().getURLEncoder().encodeQueryPart(sourceURL));

        return loginURL.toString();

    }

    /**
     * Takes a URL, and qualifies it to be a complete path, given the following
     * schema: - If the url contains double slashes "//" it is considered to be
     * a complete path already and returned unmodified - If the url starts with
     * a slash "/" (and param qualifyWithoutDBKey is true) it is considered to be starting with a database key and it
     * is only completed by the OpenWGA context path - If the url does not
     * start with a slash (or qualifyWithoutDBKey is false) it is considered to be a relative path for the given
     * database. It is completed to the context path and the dbs database key
     * 
     * @param url
     *            The url to qualify
     * @param req
     *            A request object, used to determine the application context
     *            path
     * @param database
     *            A database, used to determine a database key
     * @param qualifyWithoutDBKey
     *            If true interprets paths beginning with slash to already contain the database key. Otherwise paths beginning with slash are interpreted to be relative to the database URL.
     * @return A string buffer containing the qualified URL. It can be used to
     *         add further information to the path
     */
    public static StringBuffer qualifyWGAURL(String url, HttpServletRequest req, WGDatabase database, boolean qualifyWithoutDBKey) {
        StringBuffer loginURL = new StringBuffer();

        // If this is no complete url, we will prepend the publisher url and
        // conditionally dbkey, if the LoginPage does not begin with "/"
        if (url.indexOf("//") == -1) {

            loginURL.append(getPublisherURL(req, false));
            if (!url.startsWith("/") || !qualifyWithoutDBKey) {
                if (database == null) {
                    throw new IllegalArgumentException("Cannot qualify URL '" + url + "' when database parameter is not given");
                }
                loginURL.append("/").append(database.getDbReference()).append("/");
            }
        }

        loginURL.append(url);
        return loginURL;
    }

    public static String getPublisherURL(javax.servlet.http.HttpServletRequest request, boolean absolute) {
        
        if (absolute) {
            String protocol = request.getProtocol();
            // format should be something like http/1.1
        	int slash_index = protocol.indexOf("/");
            if(slash_index>=0)
            	protocol = protocol.substring(0, slash_index).toLowerCase();
            if (protocol.equals("http") && request.isSecure()) {
                protocol = "https";
            }
            
            int port = request.getServerPort();            
            if (port != -1) {                
                if (protocol.equals("http") && port == 80) {
                    port = -1;
                } else if (protocol.equals("https") && port == 443) {
                    port = -1;
                }
            }

            return protocol + "://" + request.getServerName() + (port != -1 ? ":" + port : "") + request.getContextPath();
        }
        else {
            return request.getContextPath();
        }

    }

    public static String getPublisherURL(javax.servlet.http.HttpServletRequest request) {
        return getPublisherURL(request, false);
    }

    private void dispatchStaticTmlRequest(WGPRequestPath path, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) throws java.lang.Exception {

        // do not log static requests
        WGARequestInformation info = (WGARequestInformation) request.getAttribute(WGARequestInformation.REQUEST_ATTRIBUTENAME);
        if (info != null) {
            info.setType(WGARequestInformation.TYPE_STATIC);
        }

        // Fetch the database
        WGDatabase database = path.getDatabase();
        String contentKey = path.getContentKey();
        String jspName = path.getResourcePath();

        WGContent content = null;
        if (database != null) {
            LanguageBehaviour langBehaviour = LanguageBehaviourTools.retrieve(database);

            // Determine the content for this request
            if (contentKey != null) {
                content = getContentByAnyKey(contentKey, database, request);
                if (content == null) {
                    if (request.getQueryString() != null && request.getQueryString().toLowerCase().indexOf("login") != -1) {
                        sendRedirect(request, response, getLoginURL(request, database, path.getCompleteURL()));
                    }
                    else {
                        throw new HttpErrorException(404, "No content of name/id " + contentKey, path.getDatabaseKey());
                    }
                    return;
                }
                if (!content.isVisible() && !isBrowserInterface(request.getSession())) {
                    throw new HttpErrorException(404, "No content of name/id " + contentKey, path.getDatabaseKey());
                }
            }
            else {
                WGLanguage lang = langBehaviour.requestSelectDatabaseLanguage(database, request);
                content = database.getDummyContent(lang.getName());
            }

            // Test browsability of content
            if (!content.isDummy() && getBrowsingSecurity(database) <= BrowsingSecurity.NO_BROWSING) {
                throw new HttpErrorException(java.net.HttpURLConnection.HTTP_FORBIDDEN, "Browsing not allowed in database '" + path.getDatabaseKey() + "'", path.getDatabaseKey());
            }

            request.setAttribute(WGACore.ATTRIB_MEDIAKEY, database.getAttribute(WGACore.DBATTRIB_DEFAULT_MEDIAKEY));

        }

        // Set context attributes for jsps
        request.setAttribute(WGACore.ATTRIB_WGPPATH, getPublisherURL(request));
        request.setAttribute(WGACore.ATTRIB_TAGIDS, new ConcurrentHashMap<String,BaseTagStatus>());
        request.setAttribute(WGACore.ATTRIB_REQUESTURL, path.getCompleteURL());
        request.setAttribute(WGACore.ATTRIB_REQUESTTYPE, REQUESTTYPE_STATICTML);
        
        // Prepare WebTML environment
        TMLContext mainContext = new WebTMLEnvironmentBuilder(getCore(), content, request, response, null, null).prepareWebTmlEnvironment();

        String mimeType = null;
        if (contentKey != null) {
            mimeType = this.getServletContext().getMimeType(jspName);
        }
        else {
            mimeType = this.getServletContext().getMimeType(request.getRequestURI());
        }
        if (mimeType == null) {
            mimeType = "text/html";
        }

        request.setAttribute(WGACore.ATTRIB_MIMETYPE, mimeType);
        response.setContentType(mimeType);

        String targetJSP = jspName + ".jsp";
        if (!"HEAD".equalsIgnoreCase(request.getMethod())) {
            this.getServletContext().getRequestDispatcher(targetJSP).include(request, response);
        }

        // Eventually do redirect
        if (request.getAttribute(WGACore.ATTRIB_REDIRECT) != null) {
            response.reset();
            response.sendRedirect(String.valueOf(request.getAttribute(WGACore.ATTRIB_REDIRECT)));
        }
    }
    
    public int getBrowsingSecurity(WGDatabase db) {
        
        String securityStr = (String) db.getAttribute(WGACore.DBATTRIB_BROWSING_SECURITY);
        if (securityStr != null) {
            return Integer.parseInt(securityStr);
        }
        
        String allowBrowsing = (String) db.getAttribute(WGACore.DBATTRIB_ALLOW_BROWSING);
        if (allowBrowsing != null && Boolean.parseBoolean(allowBrowsing) == false) {
            return BrowsingSecurity.NO_BROWSING;
        }
        
        return BrowsingSecurity.FULL_ACCESS;
        
                
    }



    /**
     * @see GenericServlet#getServletInfo()
     */
    public String getServletInfo() {
        return WGACore.getReleaseString();
    }

    /**
     * Gets the contextPath
     * 
     * @return Returns a String
     */
    public String getContextPath() {
        return _contextPath;
    }

    /**
     * Sets the contextPath
     * 
     * @param contextPath
     *            The contextPath to set
     */
    public void setContextPath(String contextPath) {
        this._contextPath = contextPath;
    }

    /**
     * Gets the listenPort
     * 
     * @return Returns a int
     */
    public int getListenPort() {
        return _listenPort;
    }

    /**
     * Returns the servePages.
     * 
     * @return boolean
     */
    public boolean isServePages() {
        return _servePages;
    }





    public boolean isAdminLoggedIn(HttpServletRequest request) {
        return getCore().isAdminLoggedIn(request);
    }

    /**
     * Sets the servePages.
     * 
     * @param servePages
     *            The servePages to set
     */
    public void setServePages(boolean servePages) {
        this._servePages = servePages;
    }

    public static String buildVirtualLink(WGA wga, WGContent content, String mediaKey, String layoutKey) throws WGException {
        return buildVirtualLink(wga, content, mediaKey, layoutKey, new LinkedHashSet<WGContentKey>());
    }
    
    private static String buildVirtualLink(WGA wga, WGContent content, String mediaKey, String layoutKey, Set<WGContentKey> contentStack) throws WGException {

        contentStack.add(content.getContentKey());
        
        // Resolve the virtual link
        VirtualLinkTarget target = wga.getCore().resolveVirtualLink(wga, content);
        
        // Virtual link is unresolveable (maybe because read-protected)
        if (target == null) {
            //throw new WGUnresolveableVirtualLinkException("Target document does not exist or is not readable");
        	wga.getLog().warn("Virtual link in " + content.getContentKey().toString() + ": Target " + content.getVirtualLink() + " does not exist or is not readable");
        	return null;
        }
        
        // If the target is again a virtual document descend into its resolving
        TMLContext cx = (TMLContext) (wga.isTMLContextAvailable() ? wga.tmlcontext() : wga.createTMLContext(content));
        if (target.getType() == Type.CONTENT) {
            TMLContext targetContext = cx.context("docid:" + target.getContainerKey(), false);
            if (targetContext != null) {
                
                if (targetContext.content().isVirtual()) {
                
                    // Circular link protection
                    if (contentStack.contains(targetContext.content().getContentKey())) {
                        throw new WGUnresolveableVirtualLinkException("Virtual link circulation detected. A virtual link points to itself across documents: " + WGUtils.serializeCollection(contentStack, ","));
                    }
                    
                    return buildVirtualLink(wga, targetContext.content(), mediaKey, layoutKey, contentStack);
                
                }
                
                String url = targetContext.contenturl(mediaKey, layoutKey);
                String anchor = content.getLinkAnchor();
                if(anchor!=null && !anchor.isEmpty())
                	url += "#" + anchor;
                return url;
            }
            else {
                return null;
            }
        }
        else if (target.getType() == Type.ATTACHMENT) {
            return cx.fileurl(target.getContainerKey(), target.getFileName());
        }
        else if (target.getType() == Type.LAYOUT) {
            return cx.layouturl(mediaKey, target.getLayoutName());
        }
        else if (target.getType() == Type.EXTERNAL) {
            return target.getExternalUrl();
        }
        else {
            throw new WGUnresolveableVirtualLinkException("Unknown virtual link target type: " + target.getType());
        }
        

    }



    /**
     * @param code
     * @param message
     * @param dbHint
     * @deprecated instead throw HttpErrorException directly
     * @throws HttpErrorException
     */
    public void sendError(int code, String message, String dbHint) throws HttpErrorException {
        throw new HttpErrorException(code, message, dbHint);
    }

    /**
     * @param code
     * @param message
     * @deprecated instead throw HttpErrorException directly
     * @throws HttpErrorException
     */
    public void sendError(int code, String message) throws HttpErrorException {
        sendError(code, message, null);
    }

    public synchronized String addTemporaryDownload(HttpSession session, TemporaryFile file) {
        String name = UIDGenerator.generateUID();
        TemporaryDownloadsMap temporaryDownloads = (TemporaryDownloadsMap) session.getAttribute(SESSION_TEMPORARYDOWNLOADS);
        if(temporaryDownloads==null){
        	temporaryDownloads = new WGPDispatcher.TemporaryDownloadsMap();
        	session.setAttribute(SESSION_TEMPORARYDOWNLOADS, temporaryDownloads);
        }
        TemporaryDownload tempDownload = new TemporaryDownload(name, file);
        temporaryDownloads.put(name, tempDownload);
        return name;
    }

    public synchronized String addTemporaryDownload(HttpSession session, File file) throws IOException {

        InputStream in = new FileInputStream(file);
        TemporaryFile tempFile = new TemporaryFile(file.getName(), in, WGFactory.getTempDir());
        return addTemporaryDownload(session, tempFile);

    }

    public boolean browserCacheIsValid(HttpServletRequest request, Long lastModified, String currentETag) {

        try {
            // Handle If-None-Match
            String browserETagsStr = (String) request.getHeader("If-None-Match");
            if (browserETagsStr != null) {
                Iterator<String> browserETags = WGUtils.deserializeCollection(browserETagsStr, ",", true, new Character('"')).iterator();
                while (browserETags.hasNext()) {
                    String rawETag = (String) browserETags.next();
                    if (rawETag.trim().equals("*")) {
                        return true;
                    }
                    
                    int startETag = rawETag.indexOf('"');
                    int endETag = rawETag.lastIndexOf('"');
                    if (startETag != -1 && endETag != -1 && startETag!=endETag) {
                        String eTag = rawETag.substring(startETag + 1, endETag);
	                    if (eTag.equals(currentETag)) {
	                        return true;
	                    }
	                }
                }
            }
    
            // Handle If-Modified-Since
            if (lastModified != null) {
                long modSince = request.getDateHeader("If-Modified-Since");
                if (modSince != -1 && modSince >= WGUtils.cutoffTimeMillis(lastModified)) {
                    return true;
                }
            }

        }
        catch (Exception e) {
            getCore().getLog().error("Exception evaluating browser cache HTTP headers", e);
        }


        return false;

    }

    public boolean isBinary(HttpServletRequest request, HttpServletResponse response) {
        String mimeType = response.getContentType();
        if (request.getParameterMap().containsKey("binaryOutput")) {
            return true;
        }
        else if (mimeType != null) {
            
            if (mimeType.toLowerCase().startsWith("text/")) {
            return false;
        }
        else {
            return true;
        }
            
        }
        else {
            return true;
        }
    }

    public boolean dispatchErrorTmlRequest(WGAError error, javax.servlet.http.HttpServletRequest request, javax.servlet.http.HttpServletResponse response) {

        try {
        
            if (error.getCausingDatabase() == null) {
                return false;
            }
            
            // don't touch session bc. JSESSIONID cookie will be creates otherwise even if we have a WGASessionManager
            //HttpSession session = request.getSession();
            
            // Parse request
            WGPRequestPath path = (WGPRequestPath) request.getAttribute(WGACore.ATTRIB_REQUESTPATH);
            if (path == null) {
            	path = WGPRequestPath.parseRequest(this, request, response);
                if(path==null)
                	return false;
                request.setAttribute(WGACore.ATTRIB_REQUESTPATH, path);
            }
            
            // Again fetch the wrapped request and response created by the filter, as we are outside it here and we have again the raw request/response object
            HttpServletRequest wrappedRequest = (HttpServletRequest) request.getAttribute(WGAFilter.REQATTRIB_REQUEST_WRAPPER);
            if (wrappedRequest != null) {
                request = wrappedRequest;
            }
            HttpServletResponse wrappedResponse = (HttpServletResponse)  request.getAttribute(WGAFilter.REQATTRIB_RESPONSE_WRAPPER); 
            if (wrappedResponse != null) {
                response = wrappedResponse;
            }
            

            // Determine tml design for this request
            WGA wga = WGA.get(request, response, getCore());
            WGDatabase errorDatabase = getCore().getContentdbs().get(error.getCausingDatabase());
            if (!errorDatabase.isSessionOpen() && !wga.openDatabase(errorDatabase)) {
                return false;
            }
            // #00005451: don't show custom error page if app is not direct accessable
            if(!errorDatabase.getSessionContext().getUserAccess().mayAccessDirectly()){
            	return false;
            }
            
            WGTMLModule tmlLib = determineErrorModule(errorDatabase, wga, error);
            if (tmlLib == null) {
                return false;
            }
    
            int pathType = path.getPathType(); 
            if(pathType==WGPRequestPath.TYPE_FILE
            		|| pathType==WGPRequestPath.TYPE_CSS
            		|| pathType==WGPRequestPath.TYPE_JS
            	)
            	return false;	// show error page only for normal pages
            
            WGContent content = errorDatabase.getDummyContent(path==null ? null : path.getRequestLanguage());
    
            // Personalize
            TMLUserProfile tmlUserProfile = null;
            try {
                tmlUserProfile = getCore().getPersManager().prepareUserProfileForRequest(request, response, content, errorDatabase, null, false);            
            }
            catch (Exception e) {
                _log.error("Unable to personalize WebTML request " + path.getCompleteURL(), e);
            }
    
            // Set context attributes for tml
            
            request.setAttribute(WGACore.ATTRIB_WGPPATH, path.getPublisherURL());
            request.setAttribute(WGACore.ATTRIB_TAGIDS, new ConcurrentHashMap<String, BaseTagStatus>());
            request.setAttribute(WGACore.ATTRIB_REQUESTURL, path.getCompleteURL());
            request.setAttribute(WGACore.ATTRIB_MIMETYPE, "text/html");
            request.setAttribute(WGACore.ATTRIB_MEDIAKEY, "html");
            request.setAttribute(WGACore.ATTRIB_REQUESTTYPE, REQUESTTYPE_TML);
            request.setAttribute(WGACore.ATTRIB_URI_HASH, WGUtils.createMD5HEX(request.getRequestURI().getBytes("UTF-8")));
            request.setAttribute(WGACore.ATTRIB_FORMDATA, null);
            request.setAttribute(WGACore.ATTRIB_OUTER_DESIGN, tmlLib.getName());
            request.setAttribute(WGACore.ATTRIB_OUTER_DESIGN_DB, tmlLib.getDatabase().getDbReference());
            request.setAttribute(WGACore.ATTRIB_COOKIES, fetchHttpCookies(request));
    
            // TML Cache control
            response.setHeader("Pragma", "No-Cache");
            response.setHeader("Cache-Control", "No-Cache");
            
            // Prepare WebTML environment
            TMLContext mainContext = new WebTMLEnvironmentBuilder(getCore(), content, request, response, tmlUserProfile, null).prepareWebTmlEnvironment();
            
            // Change path type to TML, so the WebTML rendering works normally
            path.setPathType(WGPRequestPath.TYPE_TML);
            path.setMediaKey(_core.getMediaKey("html"));
            
            // Dispatch to jsp
            rootDispatch(wga, wga.design(tmlLib.getDatabase()).resolve(tmlLib.getName()), mainContext, "html");

            // Eventually do redirect
            if (request.getAttribute(WGACore.ATTRIB_REDIRECT) != null) {

                if (!response.isCommitted()) {
//                    if (!session.isNew()) { // on new sessions we must not reset the response (#00000147)
//                        response.reset();
//                    }
                    response.sendRedirect(String.valueOf(request.getAttribute(WGACore.ATTRIB_REDIRECT)));
                }
            }

            return true;
            
        }
        catch (Exception t) {
            getCore().getLog().error("Exception handling error request", t);
            if (!response.isCommitted()) {
                response.reset();
            }
            return false;
        }
        finally {
            WGFactory.getInstance().closeSessions();
            TMLContext.clearThreadMainContext();
        }
    }

    public WGTMLModule determineErrorModule(WGDatabase errorDatabase, WGA wga, WGAError error) throws WGException {
        
        String incidentsDbKey = (String) errorDatabase.getAttribute(WGACore.DBATTRIB_INCIDENTS_APP);
        if (incidentsDbKey != null) {
            errorDatabase = wga.db(incidentsDbKey);
        }
        
        if (!errorDatabase.isSessionOpen() && !wga.openDatabase(errorDatabase)) {
            return null;
        }
        
        Design dbDesign = wga.design(errorDatabase);
        
        String errorCodeStr = wga.format(error.getErrorCode(), "000");
        
        // Error page with full error number
        String errorModuleName = "incidents:error_" + errorCodeStr;
        Design errorModule = dbDesign.resolveSystemTMLModule(errorModuleName, "html", false);
        if (errorModule != null) {
            return errorModule.getTMLModule("html");
        }
        
        // Error page with last digit x
        errorModuleName = "incidents:error_" + errorCodeStr.substring(0, 2) + "x";
        errorModule = dbDesign.resolveSystemTMLModule(errorModuleName, "html", false);
        if (errorModule != null) {
            return errorModule.getTMLModule("html");
        }
        
        // Error page with last two digits x
        errorModuleName = "incidents:error_" + errorCodeStr.substring(0, 1) + "xx";
        errorModule = dbDesign.resolveSystemTMLModule(errorModuleName, "html", false);
        if (errorModule != null) {
            return errorModule.getTMLModule("html");
        }
        
        // Generic error page
        errorModule = dbDesign.resolveSystemTMLModule("incidents:error", "html", false);
        if (errorModule != null) {
            return errorModule.getTMLModule("html");
        }
        
        return null;
        
    }

}
