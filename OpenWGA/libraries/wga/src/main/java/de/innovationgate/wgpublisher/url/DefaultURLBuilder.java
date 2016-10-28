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

package de.innovationgate.wgpublisher.url;

import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.httpclient.URIException;

import de.innovationgate.utils.URLBuilder;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.wga.config.ContentDatabase;
import de.innovationgate.wga.config.VirtualHost;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.auth.DelegatingAuthModule;
import de.innovationgate.wgpublisher.auth.DomainRedirectionAuthModule;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.filter.WGAVirtualHostingFilter;
import de.innovationgate.wgpublisher.lang.LanguageBehaviour;
import de.innovationgate.wgpublisher.lang.LanguageBehaviourTools;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

/**
 * Base implementation for creating regular OpenWGA URLs.
 * Subclasses of this implementation will not be used on independent TMLScript environments. If this is what you want subclass {@link RequestIndependentDefaultURLBuilder} instead.
 */
public class DefaultURLBuilder implements WGAURLBuilder, WGASpecificFileURLBuilder {
    
    public enum FileURLMode {
        
        /**
         * Retrieves only URLs to files attached to content
         */
        CONTENT,
        
        /**
         * Retrieves only URLs to files attached to file containers on design
         */
        DESIGN,
        
        /**
         * Retrieves URLs to both file types, priorizing design if existent (legacy mode up to compliance 7.1) 
         */
        BOTH
        
    }
    
    public class ContentURL {
        
        public ContentURL(String url, boolean external) {
            super();
            _url = url;
            _external = external;
        }
        private String _url;
        private boolean _external;
        public String getUrl() {
            return _url;
        }
        public boolean isExternal() {
            return _external;
        }

    }


    public DefaultURLBuilder() {
        _virtualLinkStack.set(new LinkedHashSet<WGContentKey>());
    }
    
    protected ThreadLocal<Set<WGContentKey>> _virtualLinkStack = new ThreadLocal<Set<WGContentKey>>();
    
    public static final String DEFAULT_LOGIN_URL = "/login.jsp";
    protected boolean _titlePathAllowed = true;
    protected WGACore _core;
    
    public String buildContentURL(TMLContext context, String mediaKey, String layoutKey, boolean ignoreVirtualLink) throws WGException {

    	String defaultMediaKey = (String) _core.readPublisherOptionOrDefault(context.db(), WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
    	HttpServletRequest request = context.getrequest();

        if (mediaKey == null) {
            mediaKey = context.getDesignContext().getMediaKey();
            if (mediaKey == null) {
                mediaKey = defaultMediaKey;
            }
        }

    	if(request!=null && mediaKey.equalsIgnoreCase(defaultMediaKey) && context.ishomepage()){
    		// check v-host config
            WGAConfiguration config = _core.getWgaConfiguration();
            VirtualHost vHost = WGAVirtualHostingFilter.findMatchingHost(config, request);
            if(vHost!=null){
            	String defaultDBKey = WGAVirtualHostingFilter.getDefaultDBKey(_core, vHost);
    	        if(vHost.isHideHomepageURL() && context.db().getDbReference().equalsIgnoreCase(defaultDBKey)){
    	        	return "/" + request.getContextPath();
    	        }
            }    		
    	}
    	
    	ContentURL url = innerBuildContentURL(context, mediaKey, layoutKey, ignoreVirtualLink);
        if (!url.isExternal()) { // Do not rewrite for external URLs (#00003853)
            return rewriteURL(url.getUrl(), context.getrequest(), context.getwgacore(), context.content().isVirtual());
        }
        else {
            return url.getUrl();
        }
    } 

    protected ContentURL innerBuildContentURL(TMLContext context, String mediaKey, String layoutKey, boolean ignoreVirtualLink) throws WGException {

        String completeUrl = null;
        boolean external = false;
        String defaultMediaKey = (String) _core.readPublisherOptionOrDefault(context.db(), WGACore.DBATTRIB_DEFAULT_MEDIAKEY);

        if (mediaKey == null) {
            mediaKey = context.getDesignContext().getMediaKey();
            if (mediaKey == null) {
                mediaKey = defaultMediaKey;
            }
        }

        // If content is dummy and this is the main context return the current layout url 
        if (context.content().isDummy()) {
            String dbKey = null;
            if (layoutKey == null || layoutKey.equals("default")) {
                if (context.db() != context.context("main").db()) {
                    throw new WGIllegalStateException("Cannot generate content url for dummy context when this is not the main context of the request");
                }
                layoutKey = (String) context.getrequest().getAttribute(WGACore.ATTRIB_OUTER_DESIGN);
                dbKey = context.db().getDbReference();
                
            }
            return new ContentURL(buildLayoutURL(context, dbKey, mediaKey, layoutKey), false);
        }

        // Virtual link
        if (context.content().isVirtual() && !ignoreVirtualLink && (layoutKey == null || layoutKey.equals("default"))) {
            completeUrl = WGPDispatcher.buildVirtualLink(WGA.get(context), context.content(), mediaKey, layoutKey);
            external = (WGContent.VIRTUALLINKTYPE_EXTERNAL.equals(context.content().getVirtualLinkType()) || WGContent.VIRTUALLINKTYPE_NOT_SPECIFIED.equals(context.content().getVirtualLinkType()));
            if (completeUrl == null) {
                return new ContentURL("", false);
            }
        }
        
        // Normal content URL
        else {

            // Default layoutkey or expand local references 
            if (layoutKey == null) {
                layoutKey = "default";
            }
            else {
                // We will ignore the possiblity here to switch design app, as this is not possible with mixed mode URLs
                layoutKey = context.resolveDesignResource(layoutKey).getResourceName();
            }
            
            // Title path URL if there is a title path manager, the document is released and default layout is used
            TitlePathManager tpm = (TitlePathManager) context.db().getAttribute(WGACore.DBATTRIB_TITLEPATHMANAGER);
            if (isTitlePathAllowed() && 
                    context.isbrowserinterface() == false &&
                    "default".equals(layoutKey) &&
                    tpm != null && tpm.isTitlePathAllowed(context.content())) {
                completeUrl = buildTitlePathURL(context, mediaKey, tpm);
            }
            
            // Standard URL if there is no title path manager or the title path creation failed/is not allowed
            if (completeUrl == null) {
                completeUrl = buildStandardContentURL(context, mediaKey, layoutKey);
            }
        }
        
        // Test, if there is a link action defined, that redirects content links to the specified action
        DesignResourceReference linkAction = (DesignResourceReference) context.option(Base.OPTION_LINK_ACTION);
        if (linkAction != null && !ignoreVirtualLink) {
            List<Object> params = new ArrayList<Object>();
            params.add("content");
            params.add(mediaKey);
            try {
                params.add(context.getwgacore().getURLEncoder().encodePathPart(layoutKey));
            }
            catch (URIException e) {
                throw new WGException("Exception encoding content URL path", e);
            }
            params.add(completeUrl);
            TMLAction tmlAction = context.getActionByID(linkAction.getResourceName(), linkAction.getDesignApp());
            if (tmlAction != null) {
                return new ContentURL(context.getURLBuilder().buildActionURL(context, tmlAction, null, params, null, context.getpath()), false);
            }
            else {
                return new ContentURL("", false);
            }
        }
        
        
        // Look if the user is logged in. In that case we add the "?login" parameter if the app is configured to do so
        boolean useLoginParameter = context.getDesignContext().getDesignDB().getBooleanAttribute(WGACore.DBATTRIB_USE_LOGINPARAMETER_ON_CONTENTURLS, false);
        if (useLoginParameter && context.content().getDatabase().getSessionContext().isAnonymous() == false) {
            try {
                de.innovationgate.wgpublisher.webtml.utils.URLBuilder builder = WGA.get(context).urlBuilder(completeUrl);
                builder.setParameter("login", null);
                completeUrl = builder.buildLikeGiven();
            }
            catch (Exception e) {
                throw new WGException("Exception adding 'login' URL parameter", e);
            }
        }
        
        return new ContentURL(completeUrl, external);
        
        
    }




    private String buildTitlePathURL(TMLContext context, String mediaKey, TitlePathManager tpm) throws WGAPIException {
        
        List<String> path = tpm.buildTitlePath(context.content(), mediaKey, new WebTMLLanguageChooser(context.db(), context));
        if (path == null) {
            return null;
        }
        
        // Add base path
        path.add(0, buildBasePath(context));
        return WGUtils.serializeCollection(path, "/");
        
    }



    private String buildStandardContentURL(TMLContext context, String mediaKey, String layoutKey) throws WGAPIException {
        StringBuffer url = new StringBuffer();
        
        // Base path up to layout key
        url.append(buildBasePath(context)).append("/");
        url.append(mediaKey).append("/");
        url.append(layoutKey).append("/");
        
        String contentKey = WGPDispatcher.buildContentURLID(context.content(), mediaKey, context.isbrowserinterface());
        url.append(contentKey);
        
        return url.toString();
    }

    public String buildLayoutURL(TMLContext context, String dbKey, String mediaKey, String layoutKey) throws WGException {
        String url = innerBuildLayoutURL(context, dbKey, mediaKey, layoutKey);
        return rewriteURL(url, context.getrequest(), context.getwgacore(), false);
    }

    protected String innerBuildLayoutURL(TMLContext context, String dbKey, String mediaKey, String layoutKey) throws WGException {

        // Defaulting of media and layout key
        if (mediaKey == null) {
            mediaKey = context.getDesignContext().getMediaKey();
            if (mediaKey == null) {
                mediaKey = (String) _core.readPublisherOptionOrDefault(context.db(), WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
            }
        }

        if (layoutKey == null) {
            layoutKey = "default";
        }
        else {
            DesignResourceReference layoutRef = context.resolveDesignResource(dbKey, layoutKey);
            layoutKey = layoutRef.getResourceName();
            dbKey = layoutRef.getDesignApp();
        }

        // Base path
        StringBuffer url = new StringBuffer();
        url.append(context.getEnvironment().getPublisherURL());
        url.append("/");
        url.append(dbKey).append("/");
        
        // Determining target language
        
        String targetLanguage = null;
        
        try {
            WGDatabase targetDB = context.db(dbKey);
            if (targetDB != null) {
                LanguageBehaviour langBehaviour = LanguageBehaviourTools.retrieve(targetDB);
                if (targetDB.isSessionOpen()) {
                    WGLanguage lang = langBehaviour.webtmlSelectDatabaseLanguage(targetDB, context);
                    if (lang != null) {
                        targetLanguage = lang.getName();
                    }
                }
               
            }
        }
        catch (WGException e) {
            context.getlog().error("Exception determining target language for layout URL", e);
        }
        
        if (targetLanguage == null) { // Fallback
            targetLanguage = new WebTMLLanguageChooser(context.db(), context).getPreferredLanguage(context.db());
        }
        
        // Media and layout key
        url.append(mediaKey).append("/");
        try {
            url.append(WGPDispatcher.buildLayoutURLID(context.getwgacore().getContentdbs().get(dbKey), context.getwgacore().getURLEncoder().encodePathPart(layoutKey), targetLanguage, mediaKey));
        }
        catch (URIException e) {
            throw new WGException("Exception encoding layout URL path", e);
        }

        // Test, if there is a link action defined, that redirects content links to the specified action
        DesignResourceReference linkAction = (DesignResourceReference) context.option(Base.OPTION_LINK_ACTION);
        if (linkAction != null) {
            List<Object> params = new ArrayList<Object>();
            params.add("layout");
            params.add(mediaKey);
            params.add(layoutKey);
            params.add(url.toString());
            TMLAction tmlAction = context.getActionByID(linkAction.getResourceName(), linkAction.getDesignApp());
            if (tmlAction != null) {
                return context.getURLBuilder().buildActionURL(context, tmlAction, null, params, null, null);
            }
            else {
                return "";
            }            
        }
        else {
            return url.toString();
        }
    }
    
    private String buildBasePath(TMLContext context) {
        StringBuffer url = new StringBuffer();
        url.append(context.getEnvironment().getPublisherURL());
        url.append("/");
        url.append(context.getdocument().getDatabase().getAttribute(WGACore.DBATTRIB_DBKEY).toString());
        return url.toString();
    }
    
    @Override
    public String buildActionURL(TMLContext context, TMLAction action, Map<String,Object> namedParams, List<Object> params, String portletMode, String portletContext) throws WGException {
        String url = innerBuildActionURL(context, action, namedParams, params, portletMode, portletContext);
        return rewriteURL(url, context.getrequest(), context.getwgacore(), false);
    }
    
    protected String innerBuildActionURL(TMLContext context, TMLAction action, Map<String,Object> namedParams, List<Object> params, String portletMode, String portletContext) throws WGException {
        
        TMLActionLink actionLink = action.createActionLink(namedParams, params, context);
        actionLink.setPortletmode(portletMode);
        if (portletContext != null) {
            actionLink.setPortletContextPath(context, portletContext);
        }
               
        String encodedActionLink = actionLink.getEncodedString(context.getwgacore());

        try {
            URLBuilder builder = WGA.get(context).urlBuilder(WGA.get(context).call().getOriginalURL());
            builder.setParameter("$action", encodedActionLink);
            return builder.build(false);
        }
        catch (Exception exc) {
            context.getlog().error("Error building action URL", exc);
            return null;
        }
        
       
    }

    public boolean isTitlePathAllowed() {
        return _titlePathAllowed;
    }

    public void setTitlePathAllowed(boolean titlePathAllowed) {
        _titlePathAllowed = titlePathAllowed;
    }

    public String buildHomepageURL(WGDatabase db, HttpServletRequest request) throws WGException {
        String url = innerBuildHomepageURL(db, request);
        if (url != null) {
	        return rewriteURL(url, request, _core, false);
	    }
        else {
           return null;
        }
    }
    
	protected String innerBuildHomepageURL(WGDatabase db, HttpServletRequest request) throws WGException {
        
        String homepage = (String) db.getAttribute(WGACore.DBATTRIB_HOME_PAGE);
        String homepageName = (String) db.getAttribute(WGACore.DBATTRIB_HOME_PAGE_NAME);

        // First try: if db homepage attribute set - redirect to homepage
        if (!WGUtils.isEmpty(homepage) && WGUtils.isEmpty(homepageName)) {
            return WGPDispatcher.qualifyWGAURL(homepage, request, db, false).toString();
        }

        // Second try: Try find home page by name
        db = _core.openContentDB(db, request, false);
        if (db.isSessionOpen()) {
            
            if (homepageName == null) {
                homepageName = "home";
            }
            LanguageBehaviour langBehaviour = LanguageBehaviourTools.retrieve(db);
            WGContent content = langBehaviour.requestSelectContentForName(db, request, homepageName, false);
                
            if (content != null && content.mayBePublished(false, WGContent.DISPLAYTYPE_NONE)) {
                TMLContext cx = new TMLContext(content, _core, null, null, request, null, request.getSession());
                String mediaKey = (String) _core.readPublisherOptionOrDefault(db, WGACore.DBATTRIB_DEFAULT_MEDIAKEY);
                return innerBuildContentURL(cx, mediaKey, null, false).getUrl();
            }
        }
        
        // Fallback: The home page might be able to be retrieved by just calling the database key. This will also enable the user to login if he has no access yet.
        else {
            StringBuffer url = new StringBuffer();
            url.append(WGPDispatcher.getPublisherURL(request)).append("/");
            url.append(db.getDbReference());
            return url.toString();
        }
        
		return null;
	
	}
	
    public String buildFileURL(TMLContext context, String dbKey, String containerName, String fileName) throws WGException {
        String url = innerBuildFileURL(FileURLMode.BOTH, context, dbKey, containerName, fileName);
        return rewriteURL(url, context.getrequest(), context.getwgacore(), false);
    }
    
    @Override
    public String buildContentFileURL(TMLContext context, String dbKey, String docKey, String fileName) throws WGException {
        String url = innerBuildFileURL(FileURLMode.CONTENT, context, dbKey, docKey, fileName);
        return rewriteURL(url, context.getrequest(), context.getwgacore(), false);
    }


    @Override
    public String buildDesignFileURL(TMLContext context, String dbKey, String containerName, String fileName) throws WGException {
        String url = innerBuildFileURL(FileURLMode.DESIGN, context, dbKey, containerName, fileName);
        return rewriteURL(url, context.getrequest(), context.getwgacore(), false);

    }



    protected String innerBuildFileURL(FileURLMode mode, TMLContext context, String dbKey, String containerName, String fileName) throws WGException {
        
        try {
            if (fileName == null) {
                context.addwarning("Missing file name for URL");
                return null;
            }
            
            // Basic path
            StringBuffer url = new StringBuffer();
            url.append(context.getEnvironment().getPublisherURL());
            url.append("/");
            
            // Ugly hack: We collect the db/container address in a DesignResourceReference
            // though in case of a content file it is not really a design resource
            DesignResourceReference containerRef;
            
            // Determine containing database
            boolean containerIsCurrentContext = false;
            try {
                
                switch (mode) {
                    
                    case CONTENT:
                        if (dbKey == null) {
                            dbKey = context.db().getDbReference();
                        }
                        
                        if (containerName == null || containerName.equals("this")) {
                            containerRef = new DesignResourceReference(dbKey, context.content().getContentKey(false).toString());
                            containerIsCurrentContext = true;
                        }
                        else {
                            containerRef = new DesignResourceReference(dbKey, containerName);
                        }
                        break;
                            
                    case DESIGN: 
                        if (dbKey == null) {
                            dbKey = context.getDesignContext().getBaseReference().getDesignApp();
                        }
                        containerRef = new DesignResourceReference(dbKey, containerName);
                        break;
                    
                    case BOTH:
            
                        // If no container name given we will use the current content document in context
                        if (containerName == null || containerName.equals("this")) {
                            containerRef = new DesignResourceReference(context.db().getDbReference(), context.content().getContentKey(false).toString());
                            containerIsCurrentContext = true;
                        }
                        
                        else {
                            containerRef = context.resolveDesignResource(dbKey, containerName);
                            WGDatabase designDB = context.db(containerRef.getDesignApp());
                            if (designDB == null) {
                                context.addwarning("Unknown design app: " + dbKey);
                                return null;
                            }
                            
                            // Try to determine if the container ref addresses a file container. Can only be done if we are actually able to open the design db
                            WGFileContainer container = null;
                            if (designDB.isSessionOpen()) {
                                container = designDB.getFileContainer(containerRef.getResourceName());
                                
                                // If the design app was not explicitly chosen and we cannot find the container in design db we we default to the context db and try to find the container there
                                if (container == null && !containerRef.isExplicitDesignAppChoice()) {
                                    containerRef.setDesignApp(context.db().getDbReference());
                                    container = context.db().getFileContainer(containerRef.getResourceName());
                                }
                                
                                // If still not available it must be a content id addressed.
                                // Reset resource name to container name, so we have no things special for design references in there (like overlay prefixes #00001158)
                                if (container == null) {
                                    containerRef.setResourceName(containerName);
                                }                        
                            }
                        }
                        
                        break;
                        
                    default:
                        throw new WGIllegalArgumentException("Unknown file URL mode: " + mode);
                
                }
                
            }
            catch (WGException e) {
                context.addwarning("Error opening design db: " + e.getClass().getName() + " - " + e.getMessage());
                context.getlog().error("Error opening design db", e);
                return null;
            }
            
            // Database key
            if (containerRef.getDesignApp() != null) {
                url.append(containerRef.getDesignApp());
            }
            else {
                return null;
            }
            
            // Address nested containers in URL form with slashes instead of colons, encode only the elements
            List<String> containerPath = new ArrayList<String>(); 
            for (String elem : containerRef.getResourceName().split(":")) {
                containerPath.add(context.getwgacore().getURLEncoder().encodePathPart(elem));
            }
            String containerURL = WGUtils.serializeCollection(containerPath, "/");
            
            // Encode file path elements, but not the slashes
            List<String> filePath = new ArrayList<String>();
            for (String elem : fileName.split("/")) {
                filePath.add(context.getwgacore().getURLEncoder().encodePathPart(elem));
            }
            String fileURL = WGUtils.serializeCollection(filePath, "/");
            
            // if content doc addressed and filename contains ".zip/", we use the ~file-Syntax relative to the content URL so file URL will be relative to the content document URL (if not titlepath) 
            if (containerIsCurrentContext && fileName.toLowerCase().indexOf(".zip/") != -1) {
                url = new StringBuffer();
                url.append(context.contenturl());
                url.append("/~file/");
                url.append(fileURL);            
            }
            
            // Normal syntax with qualifying "/file/" URL part
            else {
                url.append("/file/").append(containerURL).append("/").append(fileURL);
            }
            return url.toString();
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        
    }
    
    
    public String rewriteURL(String url, HttpServletRequest request, WGACore core) {
        return rewriteURL(url, request, core, true);
    }
    
    public String rewriteURL(String url, HttpServletRequest request, WGACore core, boolean isVirtual) {
        
        // Cannot rewrite if no request or url available
        if (request == null || url == null) {
            return url;
        }
        
        VirtualHost vHost = WGAVirtualHostingFilter.findMatchingHost(core.getWgaConfiguration(), request);
        if (vHost == null) {
            return url;
        } else {
            URLBuilder builder = parseURL(core, request, url);
            if (builder != null) {
                String path = builder.getPath().substring(request.getContextPath().length());
                if (path.startsWith("/")) {
                    path = path.substring(1);
                }
                if (path.indexOf("/") != -1) {
                    String targetDBKey = path.substring(0, path.indexOf("/")); 
                    
                    // skip vhosts processing for plugins
                    if (targetDBKey.startsWith("plugin-")) {
                        return url;
                    }
                    
                    // Is this actually a database key? If not there is no point in rewriting (#00003853)
                    ContentDatabase dbConfig = core.getWgaConfiguration().getContentDatabase(targetDBKey.toLowerCase());
                    if (dbConfig == null) {
                        return url;
                    }
                    
                    // first check if default database is requested and if we have to hide it from url
                    if (vHost.isHideDefaultDatabaseInURL() && vHost.getDefaultDatabase() != null) {
                        String defaultDBKey = WGAVirtualHostingFilter.getDefaultDBKey(core, vHost);
                        if (defaultDBKey != null && targetDBKey.equalsIgnoreCase(defaultDBKey)) {
                            // default db requested - remove from path
                            builder.setPath(path.substring(defaultDBKey.length()));
                            return builder.build(false);
                        }
                    }
                    
                    if (!WGAVirtualHostingFilter.isDBKeyAllowed(core.getWgaConfiguration(), vHost, targetDBKey)) {                      
                        // we have to find the best matching host for this db and create an absolute url here                        
                        VirtualHost preferredHost = WGAVirtualHostingFilter.findPreferredHostForDatabase(core.getWgaConfiguration(), targetDBKey);
                        if (preferredHost != null) {
                            builder.setHost(preferredHost.getServername());
                            if (preferredHost.isHideDefaultDatabaseInURL() && preferredHost.getDefaultDatabase() != null) {
                                String defaultDBKey = WGAVirtualHostingFilter.getDefaultDBKey(core, preferredHost);
                                if (defaultDBKey != null && targetDBKey.equalsIgnoreCase(defaultDBKey)) {
                                 // default db requested - remove from path
                                    builder.setPath(path.substring(defaultDBKey.length()));
                                }
                            }
                            return builder.build(true);
                        }
                    }
                }
            }            
        }        
        return url;
    }

    private URLBuilder parseURL(WGACore core, HttpServletRequest request, String url) {
        URLBuilder builder = null;
        try {
            if (url.contains("://")) {
                builder = new URLBuilder(new URL(url));
            } else {
                URL urlObj = new URL(request.getRequestURL().toString());
                String path = null;
                String query = null;
                if (url.contains("?")) {
                    String[] tokens = url.split("\\?");
                    path = tokens[0];
                    query = tokens[1];
                } else {
                    path = url;
                }
                builder = new URLBuilder(urlObj.getProtocol(), urlObj.getPort(), urlObj.getHost(), path, query, core.getCharacterEncoding());                
            }
        }
        catch (Exception e) {
            core.getLog().error("URLBuilder creating failed for url '" + url + "'.", e);
        }
        return builder;
    }


    public String buildScriptURL(TMLContext context, String dbKey, String scriptType, String scriptName) throws WGException {
        String url = innerBuildScriptURL(context, dbKey, scriptType, scriptName);
        return rewriteURL(url, context.getrequest(), context.getwgacore(), false);

    }


    private String innerBuildScriptURL(TMLContext context, String dbKey, String scriptType, String scriptName) throws WGException {

        DesignResourceReference scriptRef = context.resolveDesignResource(dbKey, scriptName);
        
        StringBuffer url = new StringBuffer();
        url.append(context.getEnvironment().getPublisherURL()).append("/");
        url.append(scriptRef.getDesignApp()).append("/");
        url.append(scriptType).append("/");
        url.append(scriptRef.getResourceName());
        return url.toString();
        
    }


    private String innerBuildLoginURL(WGDatabase db, HttpServletRequest request, String redirectURL) {

        
        // Determined by publisher option "LoginPage"
        String url = (String) db.getAttribute(WGACore.DBATTRIB_LOGIN_PAGE);

        // Determined by occasions:login.tml
        if (WGUtils.isEmpty(url)) {
            url = determineLoginTml(db, request, url);
        }
        
        // Default login URL
        if (WGUtils.isEmpty(url)) {
            url = DEFAULT_LOGIN_URL;
        }
        
        // Internal login URL: Check if this is anonymously/directly accessible. If not, return to default URL
        if (!url.contains("//") && !url.startsWith("/")) {
            
            Map<Object, DBLoginInfo> logins = WGACore.getSessionLogins(request.getSession());
            if (!logins.containsKey(_core.getDomainForDatabase(db).getName())) { // If a login is stored on the session for the dbs domain we assume it accessible
                
               if (!db.isSessionOpen()) {
                    try {
                        db.openSession(WGDatabase.ANONYMOUS_USER, null);
                    }
                    catch (WGAPIException e) {
                    }
                }
                
                if (!db.isSessionOpen() ||
                        !db.getSessionContext().getUserAccess().mayAccessDirectly()) {
                    _core.getLog().warn("App '" + db.getDbReference() + "': Internal login page '" + db.getAttribute(WGACore.DBATTRIB_LOGIN_PAGE) + "' cannot be used as the application is not directly accessible anonymously. Falling back to default login page");
                    url = DEFAULT_LOGIN_URL;
                }
            }
            
        }

        StringBuffer loginURLBuffer = WGPDispatcher.qualifyWGAURL(url, request, db, true);

        String parameterIntroducor = "?";
        if (url.indexOf("?") != -1) {
            parameterIntroducor = "&";
        }

        String authDomain = db.getAttribute(WGACore.DBATTRIB_DOMAIN).toString();
        if (authDomain != null && authDomain.startsWith("plugin-")) {
            if (db.getAuthenticationModule() instanceof DomainRedirectionAuthModule) {
                authDomain = ((DomainRedirectionAuthModule)db.getAuthenticationModule()).getDomain();
            }
            else if (db.getAuthenticationModule() instanceof DelegatingAuthModule) {
                authDomain = ((DelegatingAuthModule)db.getAuthenticationModule()).getDomainName();
            }
        }            
        
        try {
            loginURLBuffer.append(parameterIntroducor)
                .append("domain=").append(_core.getURLEncoder().encodeQueryPart(db.getAttribute(WGACore.DBATTRIB_DOMAIN).toString()))
                .append("&authDomain=").append(_core.getURLEncoder().encodeQueryPart(authDomain))
                .append("&redirect=").append(_core.getURLEncoder().encodeQueryPart(redirectURL));
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        
        String loginURL = loginURLBuffer.toString();
        
        // Eventually enforce login url restrictions
        try {
            URL completeURL = new URL(WGPRequestPath.buildCompleteURL(request), loginURL);
            Map<String,String> serverOptions = _core.getWgaConfiguration().getServerOptions();
            String redirectProtocol = serverOptions.get(WGACore.SERVEROPTION_LOGINREDIRECTPROTOCOL);
            String redirectHost = serverOptions.get(WGACore.SERVEROPTION_LOGINREDIRECTHOST);
            String redirectPort = serverOptions.get(WGACore.SERVEROPTION_LOGINREDIRECTPORT);
            URL enforcedURL = WGPRequestPath.determineRedirectionURL(completeURL, redirectProtocol, redirectHost, redirectPort);
            if (enforcedURL != null) {
                loginURL = enforcedURL.toString();
            }
        }
        catch (Exception e) {
            throw new RuntimeException("Exception enforcing restrictions on login url", e);
        }
        

        return loginURL;
        
    }


    public String determineLoginTml(WGDatabase db, HttpServletRequest request, String url) {
        
        try {
            WGA wga = WGA.get(request, null, _core);
            
            String incidentsDbKey = (String) db.getAttribute(WGACore.DBATTRIB_INCIDENTS_APP);
            if (incidentsDbKey != null) {
                db = wga.db(incidentsDbKey);
            }
            
            if (!db.isSessionOpen() && !wga.openDatabase(db)) {
                return null;
            }
        
            Design loginDesign = wga.design(db).resolveSystemTMLModule("incidents:login", "html", false);
            if (loginDesign != null) {
                url = "/" + loginDesign.getBaseReference().getDesignApp() + "/html/" + loginDesign.getBaseReference().getResourceName();
            }
            return url;
        }
        catch (WGException e) {
            _core.getLog().error("Exception determining login.tml", e);
            return null;
        }
        
    }
    
    public void newRequest(WGACore core, HttpServletRequest req) {
        _core = core;
    }


    private String innerBuildLogoutURL(WGDatabase db, HttpServletRequest request, String redirectURL) throws WGException {

        StringBuffer url = new StringBuffer();
        url.append(WGPDispatcher.getPublisherURL(request)).append("/");
        url.append(db.getDbReference()).append("/");
        url.append("logout");
        
        String parameterIntroducor = "?";
        if (url.indexOf("?") != -1) {
            parameterIntroducor = "&";
        }

        try {
            url.append(parameterIntroducor).append("&redirect=").append(
                    _core.getURLEncoder().encodeQueryPart(redirectURL));
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
        
        return url.toString();
        
        
    }


    public String buildLoginURL(WGDatabase db, HttpServletRequest request, String redirectURL) throws WGException {
        String url = innerBuildLoginURL(db, request, redirectURL);
        return rewriteURL(url, request, _core, false);
    }


    public String buildLogoutURL(WGDatabase db, HttpServletRequest request, String redirectURL) throws WGException {
        String url = innerBuildLogoutURL(db, request, redirectURL);
        return rewriteURL(url, request, _core, false);
    }


}
