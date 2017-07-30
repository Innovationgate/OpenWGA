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
package de.innovationgate.wgpublisher.webtml;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import net.sf.json.JSONSerializer;

import org.dom4j.Document;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAError;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata.PortletEventListener;
import de.innovationgate.wgpublisher.websockets.PageConnection;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionCallParameters;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletState;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateStorage;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.AjaxInfo;
import de.innovationgate.wgpublisher.webtml.utils.RootTagReceptor;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;

public class Root extends Base {
    
    private static final long serialVersionUID = 1L;

	private String resource = null;
	private String ref = null;
	private String modulename = null;
    private String modulemediakey = null;
    
    
    public static class Status extends BaseTagStatus implements DirectOutputCapableTag {
        private boolean _ajax;
        private AjaxInfo _ajaxInfo;
        private boolean _isAjaxNoRefreshCall = false;
        private boolean _isAjaxFailure = false;
        private RootTagReceptor receptorTag = null;
        public String resource;
        private String _upperTml;
        public String _upperTmlDb;
        @Override
        public boolean isDirectOutputCapable() {
            return (Boolean) WGA.get(tmlContext).server().getServerOption(WGACore.SERVEROPTION_WEBTML_DIRECT_OUTPUT);
        }
    }
    
    @Override
    public BaseTagStatus createTagStatus() {
        return new Status();
    }
	
	public void tmlStartTag() throws TMLException, WGException {

		Status status = (Status) getStatus();
		status.resource = getResource();
	    
		status.keepResult = true;
		
		// Set module info
        status.setOption(OPTION_TMLMODULE_NAME, getModulename(), null);
        status.setOption(OPTION_TMLMODULE_MEDIAKEY, getModulemediakey(), null);
		status.receptorTag = (RootTagReceptor) this.getParentTag();
		
		// Root tag was included by another tag
		if (status.receptorTag != null) {
			processIncludedRoot();            
		}
		
		// Root tag is absolute root tag of this request - do basic functionalities
		else {
            processAbsoluteRoot();
		}
        
		storeCurrentTMLAttributes();
		
		// If the rendered module is a portlet initialize it
		if (isPortletRendering(status)) {
		    TMLPortlet portlet = getTMLContext().getportlet();
		    if (portlet != null) { // May actually be null if the portlet got deregistered in the previous action. Only valid operation on AJAX norefresh action. 
		        processPortlet(portlet, status);
		    }
        }
        
	}

    private void processPortlet(TMLPortlet portlet, Status status) throws WGException {
        
        // Debug
        if (status.debugNode != null) {
            status.debugNode.addAttribute("portletkey", portlet.getportletkey());
            status.debugNode.addAttribute("portletname", portlet.getname());
            status.debugNode.addAttribute("portletmode", portlet.getmode());
            status.debugNode.addAttribute("portletcontext", portlet.getState().getContextPath());
        }
        
        // Create the javascript portlet registration
        if (!status._isAjaxNoRefreshCall) {
            String portletKey = portlet.getportletkey();
            StringBuffer javaScript = new StringBuffer();
             createPortletRegistrationJavaScript(javaScript, portletKey, getStatus().getRelevantForm(), false);
            if (javaScript.length() > 0)  {
                this.setPrefix(this.getPrefix() + javaScript.toString());
            }
        }
        
        // #00004841:
        // if the portlet state is "isOverwritableByClient" we render an empty portlet and let the client request it.
        // this prevents the portlet from being rendered twice and avoids "flickering" of the UI on the client side.
        TMLPortletState state = portlet.getState();
        if (state.isOverwritableByClient()) {
            setEvalBody(false);
            setResult(""); // Bypass stupid exiting behaviour of Base when result is null. Would prevent prefix/suffix from being rendered.
            state.setForceReload(true);
        }        	
        
        // Set portlet context.
        TMLContext portletContext = portlet.getcontext();
        
        // Reset context and mode if existing context is unretrievable (#00002299)
        if (portletContext == null && portlet.iscontextset()) {
            portlet.setcontext(null);
            AjaxInfo ajaxInfo = getCurrentPortletAjaxInfo(status);
            portlet.setmode(ajaxInfo.getInitialMode());
        }
        
        // Set options from the controller
        for (Map.Entry<String,Object> option : portlet.getState().getControllerOptions(getTMLContext()).entrySet()) {
            status.setOption(option.getKey(), option.getValue(), TMLOption.SCOPE_PORTLET);
        }
        
        // On AJAX calls: Without portlet context we use the context path of the ajax info as base context
        if (portletContext == null && status._ajaxInfo != null) {
            String contextPath = status._ajaxInfo.getContextPath();
            portletContext = getTMLContext().context(contextPath);
        }
        
        if (portletContext != null) {
            this.setTMLContext(portletContext);
            this.setChildTagContext(portletContext);
        }
        
        // Prepare for serverside portlet event processing inside the currently rendered portlet
        portlet.prepareEventProcessing(this);


    }

    private boolean isPortletRendering(Status status) {

        // On portlet inclusion (either regular or AJAX request)
        if (status.receptorTag != null) {
            if (status.receptorTag.isPortletInclude()) {
                return true;
            }
        }
        
        // On AJAX request
        else if (status._ajaxInfo != null && status._isAjaxFailure == false) {
            return true;
        }
        
        return false;
    
    }

    private void processIncludedRoot() throws TMLException {
        
        Status status = (Status) getStatus();
        
        this.setTMLContext(status.receptorTag.getChildTagContext());
        this.setChildTagContext(status.receptorTag.getChildTagContext());
        status.localVars.putAll(status.receptorTag.getLocalVarsToInherit());
        status.receptorTag.setRootTagStatus((Status) getStatus());
        
        // Test for inclusion depth. Evaluation will stop, if level 32 reached for stability reasons (stack overflow)
        Integer includeLevel = (Integer) getTMLContext().option(OPTION_INCLUDELEVEL);
        if (includeLevel.intValue() >= 64) {
        	throw new TMLException("Maximum tml:include level of 64 reached. No further inclusions allowed.", true);
        }
        else {
        	status.setOption(OPTION_INCLUDELEVEL, new Integer(includeLevel.intValue() + 1), null);
        }
        
        // Filter out local options
        if (status.receptorTag instanceof Include.Status) {
            Include.Status includeTag = (Include.Status) status.receptorTag;
            filterOptions(includeTag.getOptionsToFilter());
        }
    }

    private void processAbsoluteRoot() throws WGException, TMLException {
        
        HttpServletRequest request = (HttpServletRequest) getPageContext().getRequest();
        Status status = (Status) getStatus();
        
        // check for ajaxCall, cancel evaluation if we have no portlet
        status._ajax = false;
        status._ajaxInfo = (AjaxInfo) request.getAttribute(WGACore.ATTRIB_AJAXINFO);
        status._isAjaxNoRefreshCall = (Boolean) WGUtils.getValueOrDefault(request.getAttribute(WGACore.ATTRIB_AJAX_NOREFRESH), false);
        status._isAjaxFailure = (Boolean) WGUtils.getValueOrDefault(request.getAttribute(WGACore.ATTRIB_AJAX_FAILURE), false);

        if ( status._ajaxInfo != null) {
            status._ajax = true;
            TMLPortlet portlet = getTMLContext().getportlet();
            if (portlet == null && !status._isAjaxNoRefreshCall) { // For a norefresh call having no portlet after action call may be ok (might have been deregistered there)
                status._isAjaxFailure = true;
                setEvalBody(false);
                return;
            }
        }            
        
        // set currentMediakey to MainMediaKey
        status.setOption(OPTION_CURRENT_MEDIAKEY, getPageContext().getRequest().getAttribute(WGACore.ATTRIB_MEDIAKEY), null);
        
        // Set basic request attributes
        status.parentTag = status;
        setBasicRequestAttributes(request);
        
        /*
        // Cancel here if we have an ajax failure when restoring AJAX environment failed
         * removed bc. #00004828
        if (status._isAjaxFailure) {
            setEvalBody(false);
            return;
        }
        */
        
        // Eventually enable TML Debug
        HttpSession session = this.pageContext.getSession();
        Boolean tmlDebug = (Boolean) session.getAttribute(WGACore.ATTRIB_TMLDEBUG);
        if (tmlDebug != null && tmlDebug.booleanValue() == true) {
        	enableDebug(session, status);
        }
        
        // Determine, which (if any) document can be edited in this request
        if (getTMLContext().isbrowserinterface()) {
        	WGContent content = this.getTMLContext().content();				
        					
        	List<String> userNamesList = new ArrayList<String>();
        	userNamesList.add(content.getAuthor());
        	 
        	if ( content.getStatus().equals(WGContent.STATUS_DRAFT)
        		&& content.mayEditContent()
        		&& content.getDatabase().isMemberOfUserList( userNamesList )
        		&& !content.hasItem("remote_info")
        		) {			        		
        			pageContext.getRequest().setAttribute(WGACore.ATTRIB_EDITDOCUMENT, content.getContentKey().toString());
        	}
        }
        
        // Eventually load WGA error object as variable
        WGAError wgaError = (WGAError) getPageContext().getRequest().getAttribute(WGACore.ATTRIB_WGAERROR);
        if (wgaError != null) {
            WGA.get(getTMLContext()).tmlPage().setVar("wgaerror", wgaError);
        }

        // Deactivate body on AJAX no refresh call
        if (status._isAjaxNoRefreshCall) {
            appendResult("");
            setEvalBody(false);
        }
        
        // Deactivate body on redirect
        //TODO: This functionality should be moved to the dispatcher, as actions are no longer executed here, therefor no need to go into WebTML processing on redirect
        if (request.getAttribute(WGACore.ATTRIB_REDIRECT) != null) {
            appendResult("");
            setEvalBody(false);
        }
        
        if (status._ajax) {
            // if session is new throw corresponding system event
            if (getTMLContext().gethttpsession().isNew()) { 
                fireSessionIsNewPortletEvent();
            } else {
                // check if session was new - this might happen on ajax calls with form data
                // this session attribute is set in Dispatcher on ajax form call
                String ajaxSessionWasNewFlagSessionKey = request.getParameter("$ajaxformkey") + ".event.SessionWasNew";
                Boolean sessionWasNew = (Boolean) request.getSession().getAttribute(ajaxSessionWasNewFlagSessionKey);
                if (sessionWasNew != null && sessionWasNew.booleanValue()) {
                    // remove attribute
                    request.getSession().removeAttribute(ajaxSessionWasNewFlagSessionKey);
                    
                    // fire session is new event
                    fireSessionIsNewPortletEvent();
                }                
            }
        }
    }



    private void fireSessionIsNewPortletEvent() throws WGAPIException {        
        TMLPortlet portlet = getTMLContext().getportlet();
        if (portlet != null) {
            portlet.fireevent(de.innovationgate.wgpublisher.webtml.portlet.PortletEvent.SESSION_IS_NEW_EVENT);
        } else {
            String sessionIsNewEvent = de.innovationgate.wgpublisher.webtml.portlet.PortletEvent.SESSION_IS_NEW_EVENT.toJavaScriptObject();
            String encodedEvent = Base64.encodeWeb(sessionIsNewEvent.getBytes());
            try {
                getTMLContext().redirectto(getTMLContext().getrequest().getContextPath() + "/fireSystemEvent.jsp?event="  + encodedEvent);
            }
            catch (IOException e) {
                getTMLContext().addwarning("Unable to fire portlet event 'session is new'. Redirect to 'fireSystemEvent.jsp' failed bc. of exception '" + e.getMessage() + "'");           
            }
        }
            
 
    }
    
    private void enableDebug(HttpSession session, Status status) {
		Document doc = DocumentHelper.createDocument();
		Element rootElement = doc.addElement("tmldebugdocument");
		rootElement.addAttribute("url", WGUtils.reduce(getStatus().getRequestURL(), 100));
		rootElement.addAttribute("started", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
		rootElement.addAttribute("traceresults", String.valueOf(session.getAttribute(WGACore.ATTRIB_TMLDEBUG_TRACE_RESULTS)));
        rootElement.addAttribute("traceoptions", String.valueOf(session.getAttribute(WGACore.ATTRIB_TMLDEBUG_TRACE_OPTIONS)));
        rootElement.addAttribute("ajax", status._ajax ? "true" : "false");
		List<Document> debugDocuments = WGACore.getDebugDocumentsList(session);
		debugDocuments.add(doc);
		createDebugNode(rootElement);
		status.iterationDebugNode = status.debugNode.addElement("starttag");
		try {
            getStatus().debugNode.addAttribute("context", getTMLContext().getpath());
        }
        catch (WGAPIException e) {
            getStatus().debugNode.addAttribute("context", "Unable to retrieve contextpath bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
        }
	}



	private void setBasicRequestAttributes(HttpServletRequest request) {

	    Status status = (Status) getStatus();
	    
	    request.setAttribute(WGACore.ATTRIB_ROOT_TAG, status);
		request.setAttribute(WGACore.ATTRIB_REQUESTTIME, new Long(System.currentTimeMillis()));
		status.setOption(OPTION_INCLUDELEVEL, new Integer(0), null);
		
		if (status._ajaxInfo == null) {
	        String outerLayoutDb = (String) request.getAttribute(WGACore.ATTRIB_OUTER_DESIGN_DB);
		    String requestType = (String) request.getAttribute(WGACore.ATTRIB_REQUESTTYPE);
		    
			// if this is an static tml request we use a dummy designdbkey
		    if(requestType != null && requestType.equals(WGPDispatcher.REQUESTTYPE_STATICTML)) {
		    	status.setOption(OPTION_DESIGNDB, WGACore.STATICTML_DESIGNDBKEY, null);
		    }
		    else if (outerLayoutDb != null) {
		        status.setOption(OPTION_DESIGNDB, outerLayoutDb, null);
		    }
		    else {
		        status.setOption(OPTION_DESIGNDB, getTMLContext().db().getDbReference(), null);
		    }
		}
        
	}



    /**
	 * @throws WGAPIException 
	 * @throws  
	 * @see Base#tmlEndTag()
	 */
	public void tmlEndTag() throws WGException {
	    
        handlePortletSuffixes();
       	    
        Status status = (Status) getStatus();
        if (status._ajax == true) {
            handleAjaxSuffix(status);
        }
        
		if (status.debugNode != null) {
		    status.debugNode.addAttribute("ended", (new Date()).toString());
		    status.debugNode.addAttribute("backenddocs", String.valueOf(getMainContext().db().getSessionContext().getTotalFetchedCores()));
			Element rootElement = status.debugNode.getDocument().getRootElement();
            rootElement.addAttribute("ended", DEBUG_TIMESTAMP_FORMAT.format(new Date()));
            rootElement.addAttribute("tags", new Integer(getStatus().subtags).toString());
            rootElement.addAttribute("backenddocs", String.valueOf(getMainContext().db().getSessionContext().getTotalFetchedCores()));
            
		}

		if (status.receptorTag != null) {
		    if (!status.directOutput) {
		        this.setResultOutput(false);
		    }
		}
		else {
			this.pageContext.getRequest().setAttribute(WGACore.ATTRIB_TAGIDS, null);
		}
		
		if (status.receptorTag == null && status._ajax == false) {
		    TMLPageImpl page = (TMLPageImpl) WGA.get(getTMLContext()).tmlPage();
		    PageConnection pageConn = page.getPageConnection(false);
		    if (pageConn != null && !pageConn.isClientInited()) {
		        getTMLContext().addwarning("A WebSocket connection is needed by this page but was initialized too late, so it could not be established. Use Page.prepareSocket() either before <tml:htmlhead> is rendered or the last WebTML portlet ends.");
		    }
		}
		
		recoverCurrentTMLAttributes();
        

	}

    public void handleAjaxSuffix(Status status) throws TMLException {
        
        // Handle AJAX failure
        if (status._isAjaxFailure) {
            
            StringBuffer javaScript = new StringBuffer();
            
            if (!status._isAjaxNoRefreshCall) {
                javaScript.append("<script type=\"text/javascript\">");
            }
            
            if (!getTMLContext().isbotrequest()) {
                String message = getTMLContext().systemLabel("tml", "ajax.sessionexpired.message");
                try {
                    javaScript.append("WGA.util.showReloadMessage(\"" + getTMLContext().encode("javascript", message) +  "\");");
                }
                catch (FormattingException e) {
                    getTMLContext().getlog().error("Exception encoding AJAX error message", e);
                }
            }
            
            if (!status._isAjaxNoRefreshCall) {
                javaScript.append("</script>");
            }
            
            this.setSuffix(javaScript.toString());
            //this.setResult(""); // Neccessary to trigger any tag output
            //return;
            
        }
        
        // Handle reintegration of form part on AJAX subform request
        String ajaxSuperform = (String) getPageContext().getRequest().getAttribute(WGACore.ATTRIB_IS_AJAX_SUBFORM_OF);
        if (ajaxSuperform != null) {
        	TMLForm form = getTMLContext().tmlformbyid(ajaxSuperform);
        	if (form != null) {
        		try {
    				String serializedInfo = FormBase.serializeFormInfo(form.getforminfo(), getTMLContext());
    	            StringBuffer javaScript = new StringBuffer();
    	            if (!status._isAjaxNoRefreshCall) {
    	                javaScript.append("<script type=\"text/javascript\">");
    	            }
    			    javaScript.append("var form = document.forms['").append(form.getformid()).append("'];");
    			    javaScript.append("if( form ) {");
    			    javaScript.append("form." + TMLForm.SYSTEMFIELD_FORMINFO + ".value = \"").append(serializedInfo).append("\";");
    			    javaScript.append("form.removeAttribute('target');");
    			    javaScript.append("form.removeAttribute('action');");
    			    javaScript.append("}");
    			    javaScript.append("else { alert('Oops, there is no form!'); };");
    			    if (!status._isAjaxNoRefreshCall) {
    			        javaScript.append("</script>");
    			    }
    				this.setSuffix(getSuffix() + javaScript.toString());
    			} catch (Exception e) {
    				throw new TMLException("Unable to serialize form data.", e, false);
    			}
        	}
        }
        
        // Handle redirect from action inside AJAX request
        String redirectURL = (String) getPageContext().getRequest().getAttribute(WGACore.ATTRIB_REDIRECT);
        if (redirectURL != null) {
            StringBuffer javaScript = new StringBuffer();
            if (!status._isAjaxNoRefreshCall) {
                javaScript.append("<script type=\"text/javascript\">");
            }
            
            javaScript.append("location.href=\"" + WGUtils.encodeJS(redirectURL) + "\";");
            
            if (!status._isAjaxNoRefreshCall) {
                javaScript.append("</script>");
            }
            this.setSuffix(getSuffix() + javaScript.toString());
        }
    }



	private boolean isAlreadyWarnedAboutAJAXFailure(AjaxInfo ajaxInfo) {
	    
	    boolean warnGenerally = getTMLContext().db().getBooleanAttribute(WGACore.DBATTRIB_SHOW_SESSION_EXPIRED_MESSAGE, true);
	    if (!warnGenerally) {
	        return true;
	    }
	    
	    if (ajaxInfo.getProfileSessionId() == null) {
	        return false;
	    }

	    HttpSession session = getPageContext().getSession();
	    synchronized (session) {
	        
	        @SuppressWarnings("unchecked")
            Map<String, Boolean> alreadyWarnedSessions = (Map<String, Boolean>) session.getAttribute(WGACore.SESSION_WARNED_PROFILESESSIONS);
	        if (alreadyWarnedSessions == null) {
	            alreadyWarnedSessions = new ConcurrentHashMap<String, Boolean>();
	            session.setAttribute(WGACore.SESSION_WARNED_PROFILESESSIONS, alreadyWarnedSessions);
	        }
	        if (alreadyWarnedSessions.containsKey(ajaxInfo.getProfileSessionId())) {
	            return true;
	        }
	        else {
	            alreadyWarnedSessions.put(ajaxInfo.getProfileSessionId(), true);
	            return false;
	        }
	        
	    }
	    
	    
	
	}

    private AjaxInfo getCurrentPortletAjaxInfo(Status status) {

	    // Portlet include: Return the ajax info from include tag
	    if (status.receptorTag != null && status.receptorTag.isPortletInclude()) {
	        return status.receptorTag.getPortletAJAXInfo();
	    }
	    
	    // AJAX request. Return the ajax info retrieved by this root tag
	    if (status._ajaxInfo != null && !status._isAjaxFailure) {
	        return status._ajaxInfo;
	    }
	    
	    return null;
	    
    }

    /**
	 * Gets the resource
	 * @return Returns a String
	 */
	public String getResource() {
		return resource;
	}
	/**
	 * Sets the resource
	 * @param resource The resource to set
	 */
	public void setResource(String resource) {
		this.resource = resource;
	}

	/**
	 * Returns the db.
	 * @return String
	 */
	public String getRef() {
		return ref;
	}

	/**
	 * Sets the db.
	 * @param db The db to set
	 */
	public void setRef(String db) {
		this.ref = db;
	}

	/**
     * @param request
     */
    protected void storeCurrentTMLAttributes() {
        
        Status status = (Status) getStatus();
        Root.Status root;
        if (status instanceof Root.Status) {
            root = (Root.Status) status;
        }
        else {
            root = status.getRootTag();
        }
        
        HttpServletRequest request = (HttpServletRequest) getPageContext().getRequest();
        status._upperTml = (String) request.getAttribute(WGACore.ATTRIB_CURRENTTML);
        status._upperTmlDb = (String) request.getAttribute(WGACore.ATTRIB_CURRENTTMLDB);
        
        request.setAttribute(WGACore.ATTRIB_CURRENTTML, root.resource);
        request.setAttribute(WGACore.ATTRIB_CURRENTTMLDB, root.getDesignDBKey());
    }
    
    protected void recoverCurrentTMLAttributes() {
        
        Status status = (Status) getStatus();
        HttpServletRequest request = (HttpServletRequest) getPageContext().getRequest();
        request.setAttribute(WGACore.ATTRIB_CURRENTTML, status._upperTml);
        request.setAttribute(WGACore.ATTRIB_CURRENTTMLDB, status._upperTmlDb);
        
    }
    


    /**
     * @return Returns the modulename.
     */
    public String getModulename() {
        return modulename;
    }

    /**
     * @param modulename The modulename to set.
     */
    public void setModulename(String modulename) {
        this.modulename = modulename;
    }

    /**
     * @return Returns the modulemediakey.
     */
    public String getModulemediakey() {
        return modulemediakey;
    }

    /**
     * @param modulemediakey The modulemediakey to set.
     */
    public void setModulemediakey(String modulemediakey) {
        this.modulemediakey = modulemediakey;
    }
    
    
    private void filterOptions(Set<String> optionsToFilter) {
        
        Iterator<TMLOption> options = getStatus().getTagOptions().values().iterator();
        TMLOption option;
        while (options.hasNext()) {
            option = options.next();
            if (optionsToFilter.contains(option.getName())) {
                if (option.isLocalScope()) {
                    options.remove();
                }
                else if (option.isPortletScope() && isPortletRendering((Status) getStatus())) {
                    options.remove();
                }
            }
        }
        
    }

    public void handlePortletSuffixes() throws WGAPIException {        
        
        try {
            Status status = (Status) getStatus();
            AjaxInfo ajaxInfo = getCurrentPortletAjaxInfo(status);
            StringBuilder javaScript = new StringBuilder();
            boolean somethingDone = false;
            
            // Write changed portlet states to the client when they are transient: On the absolute root when AJAX request, on first level portlet includes on Non-AJAX requests
            TMLPortletStateStorage stateStorage = getTMLContext().getPortletStateStorage();
            if (stateStorage instanceof TMLPortletStateTransientStorage) {
                
                TMLPortletStateTransientStorage transientStorage = (TMLPortletStateTransientStorage) stateStorage;
                
                boolean writeStates = false;
                boolean writeReloadTrigger = false;
                if (status.receptorTag == null && status._ajaxInfo != null) {
                    writeStates = true;
                    writeReloadTrigger = true;
                }
                else if (status.receptorTag != null && status.receptorTag.isPortletInclude()) {
                    TMLPortlet portlet = getTMLContext().getportlet();
                    if (portlet.getparentportlet().isroot()) {
                        writeStates = true;
                    }
                }
                
                if (writeStates) {
                    Set<String> disposedStates = transientStorage.getDisposedStates();
                    for (TMLPortletState state : transientStorage.getAllStates()) {
                        if (state.isTransmitToClient()) {
                            state.storeScopeObjects();
                            createRegisterPortletStateJavascript(javaScript, transientStorage, state);
                            state.setTransmitToClient(false);
                            somethingDone = true;
                            disposedStates.remove(state.getPortletKey());
                        }
                    }
                    for (String disposedKey : disposedStates) {
                        createDisposePortletStateJavascript(javaScript, disposedKey);
                    }
                    
                    if (writeReloadTrigger) {
                        javaScript.append("WGA.portlet.performPortletReloads();\n");
                    }
                }

            }
            
            // check for portlet events that were issued inside the call and render if present (once for absolute root only)
            if (ajaxInfo != null) {
                @SuppressWarnings("unchecked")
                Set<PortletEvent> portletEvents = (Set<PortletEvent>) getPageContext().getRequest().getAttribute(TMLPortlet.REQATTRIB_FIRED_PORTLET_EVENTS); 
                if (portletEvents != null) {
                    Iterator<PortletEvent> it = portletEvents.iterator();
                    while (it.hasNext()) {
                        somethingDone = true;
                        PortletEvent event = it.next();
                        javaScript.append("WGA.event.dispatch(").append(event.toJavaScriptObject()).append(");\n");
                    }
                    getPageContext().getRequest().removeAttribute(TMLPortlet.REQATTRIB_FIRED_PORTLET_EVENTS);
                }
            }
            
            if (somethingDone) {
            	// Eventually init the page connection, if not already done
                if (status._ajax == false) {
                	initPageConnectionClient(javaScript);
                }
                
                String suffix;
                if (status._isAjaxNoRefreshCall)
                	suffix = javaScript.toString();
                else suffix = "<script type=\"text/javascript\">" + javaScript.toString() + "</script>";
                
                this.setSuffix(this.getSuffix() + suffix);
            }
        }
        catch (Exception e) {
            throw new RuntimeException("Exception creating portlet suffixes", e);
        }
        
        
    }

    private void createDisposePortletStateJavascript(StringBuilder out, String disposedKey) {
        out.append("WGA.portlet.disposeState('").append(disposedKey).append("');\n");
    }

    protected void createRegisterPortletStateJavascript(StringBuilder out, TMLPortletStateTransientStorage transientStorage, TMLPortletState state) throws IOException {
        out.append("WGA.portlet.registerState('")
        .append(state.getPortletKey()).append("', '")
        .append(transientStorage.serializePortletState(state)).append("', '")
        .append(state.getProcessContext().getProcessId()).append("', ")
        .append(String.valueOf(state.isOverwritableByClient())).append(", ")
        .append(String.valueOf(state.isForceReload())).append(", ")
        .append(String.valueOf(state.isDefaultState()))
        .append(");\n");
    }
    
    private void createPortletRegistrationJavaScript(StringBuffer javaScript, String portletKey, String parentForm, boolean skipScriptTags) throws WGException {
        // fetch parent keys
        List<String> parentPortletKeys = new ArrayList<String>();
        TMLPortlet portlet = getTMLContext().getportletbykey(portletKey);
        String parentKey = portlet.getparentkey();
        if (parentKey != null) {
            TMLPortlet parentPortlet = portlet.getparentportlet();
            while (parentKey != null && parentPortlet != null && !parentPortlet.isroot()) {
                parentPortletKeys.add(parentPortlet.getportletkey());
                parentKey = parentPortlet.getparentkey();
                parentPortlet = parentPortlet.getparentportlet();                
            }
        }
        
        // create javascript block for registration
        if (!skipScriptTags) {
            javaScript.append("<script type=\"text/javascript\">");
        }
        
        Map<String,Object> regParams = new HashMap<String,Object>();
        regParams.put("portletKey", portletKey);
        regParams.put("parentKeys", parentPortletKeys);
        if (parentForm != null) {
            regParams.put("parentForm", parentForm);
        }
        
        javaScript.append("WGA.portlet.register("  + JSONSerializer.toJSON(regParams) + ");");
        
        // Event registrations from controller
        TMLScriptObjectMetadata controllerMetaData = portlet.getState().getControllerMetaData(WGA.get(getTMLContext()));
        if (controllerMetaData != null) {
            for (PortletEventListener eventListener : controllerMetaData.getPortletEventListeners()) {
                TMLAction action = new TMLAction("pc." + eventListener.getMethod());
                TMLActionCallParameters callParams = new TMLActionCallParameters();
                callParams.setAjaxMode(eventListener.getAjaxMode());
                callParams.setRelevantForm(getStatus().getRelevantForm());
                writePortletEventRegistration(javaScript, action, callParams, isKeepParamsOnAJAX(), eventListener.getEventName());
            }
        }
        
        if (!skipScriptTags) {
            javaScript.append("</script>");
        }

    }


}

