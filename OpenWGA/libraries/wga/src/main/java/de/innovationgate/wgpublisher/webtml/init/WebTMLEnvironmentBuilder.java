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

package de.innovationgate.wgpublisher.webtml.init;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.httpclient.URIException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGPortletRegistry;
import de.innovationgate.wga.server.api.ApplicationEventBuilder;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.Root;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionException;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletState;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateStorage;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.AjaxInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLInvalidActionLinkException;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLOptionPreserver;
import de.innovationgate.wgpublisher.webtml.utils.TMLPageImpl;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;
import de.innovationgate.wgpublisher.webtml.utils.URLBuilder;
import de.innovationgate.wgpublisher.webtml.utils.VarParamsMap;

public class WebTMLEnvironmentBuilder {
    
    private WGACore _core;
    private boolean _debug;
    private WGContent _doc;
    private HttpServletRequest _request;
    private HttpServletResponse _response;
    private TMLUserProfile _userProfile;
    private AjaxInfo _ajaxInfo;

    public WebTMLEnvironmentBuilder(WGACore core, WGContent doc, HttpServletRequest request, HttpServletResponse response, TMLUserProfile tmlUserProfile, AjaxInfo ajaxInfo) {
        _core = core;
        _doc = doc;
        _request = request;
        _response = response;
        _userProfile = tmlUserProfile;
        _ajaxInfo = ajaxInfo;
        _debug = WGUtils.getValueOrDefault((Boolean) request.getSession().getAttribute(WGACore.ATTRIB_TMLDEBUG), false); 
    }

    public TMLContext prepareWebTmlEnvironment() throws WGException {
    
        if (_debug) {
            _core.getLog().info("Request: " + System.identityHashCode(_request) + 
                    ", Session: " + _request.getSession().getId() +
                    ", Ajax: " + (_ajaxInfo != null)); 
        }
        
        // Create WebTML context
        TMLContext context = new TMLContext(_doc, _core, _userProfile, null, _request, _response, _request.getSession());
        TMLContext.clearThreadMainContext();
        context.makeThreadMainContext();
        _request.setAttribute(WGACore.ATTRIB_MAINCONTEXT, context.content());
        
        // Process multipartformdata
        de.innovationgate.wgpublisher.webtml.form.TMLForm.MultipartFormData data = retrieveMultipartFormData(_request);
        
        // create TMLForm if data is available
        TMLForm form = null;
        if (data != null) {
            if (data.getFormInfoItem() != null) {
                form = processTMLForm(context, _request, data, _ajaxInfo);
                if(form!=null){
	                context.getEnvironment().setForm(form);
	                _request.setAttribute(WGACore.ATTRIB_POSTED_TMLFORM, form);
                }
            }
            else {
                WGA.get(context).tmlPage().setVar("multipartData", data.getFileItems());
            }
        }
        
        // Process WebTML var parameters
        String varParam = _request.getParameter(WGPDispatcher.URLPARAM_VARS);
        if (varParam != null) {
            try {
                processVarParams(_request, context, varParam);
            }
            catch (Exception e) {
                _core.getLog().error("Exception processing var params", e);
                context.addwarning("Unable to process var params: " + e.getMessage());          
            }       
        }
        
        // Set DefaultURLBuilder if no other is already set
        _core.retrieveURLBuilder(_request, _doc.getDatabase());
    
        // Restore AJAX environment on AJAX call
        if (_ajaxInfo != null) {
            restoreAjaxEnvironment(_ajaxInfo, context);
        }
        
        // Read transient portlet states
        TMLPortletStateStorage storage = context.getPortletStateStorage();
        if (storage instanceof TMLPortletStateTransientStorage) {
            TMLPortletStateTransientStorage transientStorage = (TMLPortletStateTransientStorage) storage;
            transientStorage.readStates(_core);
            reregisterChildPortlets(context.db(), transientStorage.getAllStates(), context.getPortletRegistry(), null);
        }
        
        // Process WebTML action
        try {
            if (processAction(context, _request, _response, form, _ajaxInfo != null)) {
                if (form != null) {
                    TMLFormInfo formInfo = form.getforminfo();
                    if (formInfo.isValidated() && formInfo.keepOnValidate() && !form.wasValidatedInThisRequest()) {
                        form.validate();
                    }
                }
            }
        }
        catch (Exception e) {
            _core.getLog().error("Exception processing WebTML action", e);
            context.addwarning("Unable to process WebTML action: " + e.getMessage());           
        }
        
        return context;
        
        
    }

    private TMLForm.MultipartFormData retrieveMultipartFormData(HttpServletRequest request) {
        return (TMLForm.MultipartFormData) request.getAttribute(WGACore.ATTRIB_FORMDATA);
    }

    private TMLForm processTMLForm(TMLContext context, HttpServletRequest request, TMLForm.MultipartFormData multipartData, AjaxInfo ajaxInfo) {
        
        TMLForm form = null;
        try {
            form = new TMLForm(_core, multipartData);
        }
        catch (Exception e) {
            _core.getLog().error("Exception creating TMLForm from multipart form data", e);
            context.addwarning("Error creating TMLForm object bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage(), true);
            return null;
        }
        
        // Eventually merge with persistent form
        TMLForm persistentForm = (TMLForm) context.getPersistentForms().get(form.getformid());
        if (persistentForm != null) {
            try {
                persistentForm.importForm(form);
            }
            catch (WGAPIException e) {
                _core.getLog().error("Exception importing form data into persistent form", e);
                context.addwarning("Error importing form data into persistent form: " + e.getClass().getName() + " message: " + e.getMessage(), true);
            }
            form = persistentForm;              
        }
        else {
            context.registerForm(form, false);          
        }
        request.setAttribute(WGACore.ATTRIB_LASTFORM, form);        
    
        if (form != null) {
            // reset validated in this request flag
            form.setWasValidatedInThisRequest(false);
        }
        
        // AJAX Request: Look if the form is our "superform". If so we switch to AJAX subform mode.
        if (ajaxInfo != null && ajaxInfo.getSuperform() != null && ajaxInfo.getSuperform().equals(form.getformid())) {
            request.setAttribute(WGACore.ATTRIB_IS_AJAX_SUBFORM_OF, form.getformid());
        }
        
        
        return form;
    }

    private void processVarParams(HttpServletRequest req, TMLContext context, String varParam) throws WGException, UnsupportedEncodingException, IOException, GeneralSecurityException {
        
        VarParamsMap varParams = URLBuilder.extractVarParameters(varParam, _core);
        if (varParams == null) {
            return;
        }
        
        // Wrong session? Discard var params
        if (!varParams.isValidRequest(req)) {
            URL requestUrl = new URL((String) req.getAttribute(WGACore.ATTRIB_REQUESTURL));
            _core.getLog().warn("Var params used by client " + req.getRemoteAddr() + " do not belong to the called URL. Not used. Called url path: " + requestUrl.getPath() + ", Params url path: " + varParams.getPath());
            return;
        }
        
        req.setAttribute(WGACore.ATTRIB_VAR_PARAMETERS, varParams);
        TMLPage page = WGA.get(context).tmlPage();
        for (Map.Entry<String,Object> param : varParams.entrySet()) {
            page.setVar(param.getKey(), param.getValue());
        }
         
    }

    private void restoreAjaxEnvironment(AjaxInfo ajaxInfo, TMLContext context) throws WGException {
        
        // restore options
        for (TMLOption option : ajaxInfo.getOptions().values()) {
            if (!Root.UNRECOVERABLE_OPTIONS.contains(option.getName())) {
                context.setoption(option.getName(), recoverOptionValue(context, option), option.getScope());
            }
        }
        
        // Restore design db option - must be here so following context change can use it
        context.setoption(Root.OPTION_DESIGNDB, ajaxInfo.getDesignDB(), null);
    
        // Restore profile store on end flag
        if (context.getprofile() != null) {
            context.getprofile().setSavedOnEnd(ajaxInfo.isSaveProfileOnEnd());
        }
        
        // Restore page id
        if (ajaxInfo.getPageId() != null) {
            TMLPageImpl page = (TMLPageImpl) WGA.get(context).tmlPage();
            page.restorePageConnectionForAJAX(ajaxInfo.getPageId());
        }
        
        // Restore portlet of called AJAX action, maybe need to reregister if registry is transient
        WGPortletRegistry reg = context.getPortletRegistry();
        if (reg.isTransient()) {
            WGPortlet portlet = reregisterPortletsFromPath(context.db(), reg, ajaxInfo);
            if (!portlet.getKey().equals(ajaxInfo.getId())) {
                _core.getLog().error("A reinstantiated transient portlet has a different key than the original. That is not good!");
            }
        }
        
    }

    public Object recoverOptionValue(TMLContext context, TMLOption option) {
        Object value = option.getValue();
        
        if (value instanceof TMLContext.Serialized) {
            value = ((TMLContext.Serialized) value).deserialize(context);
        }
        
        return value;
    }

    private boolean processAction(TMLContext context, HttpServletRequest request, HttpServletResponse response, TMLForm form, boolean ajax) {
        
    	boolean actioncalled = false;
    	// Location of Actionkey depends. When form available, then is located in form, else is URL parameter
    	String actionKey = null;
    	
    	try {
    	
    		if (form != null) {
    			actionKey = form.getformaction();
    		}
    		else {
    			actionKey = _core.getURLEncoder().decode(request.getParameter(WGPDispatcher.URLPARAM_ACTION));
    		}
            
    		// Parse the action link and call the action
    		TMLActionLink actionLink;
    		if (actionKey != null && !actionKey.trim().equals("")) {
    			//	actions
    			actionLink = TMLActionLink.read(request, actionKey, _core);
    			if (actionLink == null) {
    			    context.addwarning("Unable to read action link: " + actionKey);
    			    return false;
    			}
                
                long starttime = System.currentTimeMillis();
    			TMLAction action = callAction(context, actionLink, ajax);
                long endtime = System.currentTimeMillis();
                
                if (_debug && action != null) {
                    _core.getLog().info("Request: " + System.identityHashCode(request) + 
                            ", WebTML Action: " + action.getDescription() + 
                            ", Action Mode: " + actionLink.getMode() +
                            ", Portlet: " + (context.getportlet() != null ? context.getportlet().getportletpath() + " (" + context.getportlet().getportletkey() + ")" : "(none)") + 
                            ", Execution time: " + (endtime - starttime));
                    _response.addHeader("X-WGA-Request-DebugID", String.valueOf(System.identityHashCode(request)));
                }
                
                // We rebuild the session on the current context if the action was a master action
                // to be able to pick up modifications done there (F00004096) 
                if (action != null && action.isMaster()) {
                    context.db().reopenSession();
                }
                
                //F00004242
                if (actionLink.getMode() != null && actionLink.getMode().equals(TMLActionLink.MODE_AJAX_NO_PORTLET_REFRESH)) {
                    // this is an ajax action call do not render content result but portletEvents, response type "text/javascript" (#00004413)
                    request.setAttribute(WGACore.ATTRIB_AJAX_NOREFRESH,  true);
                    response.setContentType("text/javascript");
                }
                
                actioncalled = true;                                
                
    		}
    	}
    	catch (TMLInvalidActionLinkException e) {
    	    if ("true".equals(System.getProperty("de.innovationgate.license.DevelopmentModeEnabled"))) {
    	        context.addwarning("Invalid action link calling TML action: " + e.getMessage());
    	    }
    	}
        catch (TMLActionException e) {
            context.addwarning("Error calling TML action: " + e.getMessage());
        }
        catch (IOException e) {
            context.addwarning("Error decoding action link: " + e.getMessage());
    	}
        catch (WGException e) {
            context.addwarning("Error calling TML action: " + e.getMessage());
        }
    
    	
        return actioncalled;
        
    }

    private TMLAction callAction(TMLContext context, TMLActionLink actionLink, boolean ajax) throws WGException {
    
    	// Look for correct sequence number to prevent action execution by page reload. Fail silently
    	HttpSession session = context.gethttpsession();
        if (!context.isCurrentSequenceNumber(actionLink, session, ajax)) {
            return null;
        }
        
        // Look if action was rendered in this session - if not do not execute, 
        if (!actionLink.getSessionID().equals(session.getId())) {
            if (ajax) {
                context.getrequest().setAttribute(WGACore.ATTRIB_AJAX_FAILURE, true);
            }
            throw new TMLInvalidActionLinkException("The action was registered for a different session ID");
        }
    
    	// Try to retrieve action context - defaults to context of this tag
    	TMLContext actionContext = context;
    	if (actionLink.getContextPath() != null) {
    		actionContext = actionContext.context(actionLink.getContextPath(), false);
            
            // Could not retrieve context. Too dangerous to execute action under the wrong context.
            if (actionContext == null) {
                throw new TMLActionException("Unable to retrieve action context: " + actionLink.getContextPath());
            }            
    	}
    	else if (actionLink.getDbKey() != null) {
    		//B00004602
    		
    		// no context path - switch to dummy context of the given db  
    		actionContext = context.context("db:" + actionLink.getDbKey(), false);
    		
    		// Could not retrieve dummy context. Too dangerous to execute action under the wrong context.
    		if (actionContext == null) {
    			throw new TMLActionException("Unable to retrieve action context: db:" + actionLink.getDbKey());
    		}
    	}
    	
    	// Temporarily set portlet namespace, WebTML scope
    	TMLOptionPreserver preserver = new TMLOptionPreserver(context.getDesignContext());
        preserver.preserve(Base.OPTION_PORTLET_NAMESPACE, actionLink.getPortletKey());
        preserver.preserve(Base.OPTION_WEBTML_SCOPE, actionLink.getWebtmlScope());
    	
        try {
            
            // Additional objects for action: portletEvent
            Map<String,Object> objects = new HashMap<String,Object>();
            PortletEvent ev = context.fetchRequestPortletEvent();
            if (ev != null) {
                objects.put("portletEvent", ev);
            }
            
    		// Execute default action
    		if (actionLink.isDefaultAction()) {
    	        TMLAction action = new TMLAction(actionLink.getDefaultAction());
    			Object result = actionContext.callDefaultAction(action, actionLink, objects);
    			if (result == null || !result.equals(Boolean.FALSE)) {
        			performPostActionOperations(action, actionLink, context, actionContext, result);
    			}
    			return action;
    		}
    		
    		// Execute custom action
    		else {
    			TMLAction tmlAction = (TMLAction) context.getActionRegistration().get(actionLink.getActionKeyInteger());
    			if (tmlAction == null) {
    				throw new TMLActionException("Could not find action for key " + actionLink.getActionKey());
    			}
    			
    			// Execute, enforce portletmode and context on success if given
    			try {
                    ExpressionResult expressionResult = actionContext.callCustomAction(tmlAction, actionLink, objects);
                    if (expressionResult.getResult() == null || expressionResult.getResult() instanceof String || !expressionResult.isFalse()) {
                        performPostActionOperations(tmlAction, actionLink, context, actionContext, expressionResult.getResult());
                    }
                }
                catch (WGExpressionException e) {
                    context.addwarning(WGUtils.encodeHTML(e.getMessage() + (e.getExpression() != null ? "\nExpression line: " + e.getExpression() : "\nExpression:\n" + tmlAction.getCode())), false);
                    _core.getLog().error("Error executing " + tmlAction.getDescription() + " : " + e.getMessage(), e);
                }
    			return tmlAction;
    		}
    		
    		
        }
        finally {		    
            preserver.restore();
        }
    
    }

    private void performPostActionOperations(TMLAction action, TMLActionLink actionLink,  TMLContext requestContext, TMLContext actionContext, Object actionResult) throws WGException {
        
        WGA wga = WGA.get(requestContext);
        TMLPortlet portlet = actionContext.getportlet();
        
        if (actionResult != null) {
            processActionResult(action, actionResult, wga, portlet);
        }
        

        if (portlet != null) {
            // Set portlet mode
            if (actionLink.getPortletmode() != null) {
                portlet.setmode(actionLink.getPortletmode());
            }
            
            // Set portlet context
            String pContextPath = actionLink.getPortletContextPath();
            if (pContextPath != null) {
                if (pContextPath.trim().equalsIgnoreCase(TMLPortlet.PORTLETCONTEXT_NONE)) {
                    portlet.setcontext(null);    
                }
                else {
                    TMLContext portletContext = actionContext.context(pContextPath, false);
                    if (portletContext != null) {
                        portlet.getState().setContextPath(portletContext.getpath());
                    }
                    else {
                        actionContext.addwarning("Unable to retrieve portlet context " + pContextPath + " from base context " + actionContext.getpath());
                    }
                }
            }
        }
        
    }

    public void processActionResult(TMLAction action, Object actionResult, WGA wga, TMLPortlet portlet) throws WGException {
        TMLScript tmlscript = wga.tmlscript();
        
        // Set variables
        if (tmlscript.hasProperty(actionResult, "$vars")) {
            try {
                Object scriptVars = tmlscript.callMethod(actionResult, "$vars");
                if (scriptVars instanceof Map) {
                    @SuppressWarnings("unchecked")
                    Map<Object,Object> vars = (Map<Object,Object>) scriptVars;
                    for (Map.Entry<Object,Object> var : vars.entrySet()) {
                        wga.tmlcontext().setvar(String.valueOf(var.getKey()), var.getValue());
                    }
                }
            }
            catch (Exception e) {
                wga.getLog().error("Exception setting $vars of WebTML action '" + action.getDescription() + "' as Lookup table", e);
            }

        }
        
        // Set page variables
        if (tmlscript.hasProperty(actionResult, "$pageVars")) {
            try {
                Object scriptVars = tmlscript.callMethod(actionResult, "$pageVars");
                if (scriptVars instanceof Map) {
                    @SuppressWarnings("unchecked")
                    Map<Object,Object> vars = (Map<Object,Object>) scriptVars;
                    for (Map.Entry<Object,Object> var : vars.entrySet()) {
                        wga.tmlPage().setVar(String.valueOf(var.getKey()), var.getValue());
                    }
                }
            }
            catch (Exception e) {
                wga.getLog().error("Exception setting $pageVars of WebTML action '" + action.getDescription() + "' as Lookup table", e);
            }
        }
        
        // Set portlet variables
        if (tmlscript.hasProperty(actionResult, "$portletVars")) {
            if (portlet == null || portlet.isroot()) {
                wga.getLog().error("Cannot set $portletVars as the WebTML action '" + action.getDescription() + "' does not run in a portlets scope");
            }
            else {
                try {
                    Object scriptVars = tmlscript.callMethod(actionResult, "$portletVars");
                    if (scriptVars instanceof Map) {
                        @SuppressWarnings("unchecked")
                        Map<Object,Object> vars = (Map<Object,Object>) scriptVars;
                        for (Map.Entry<Object,Object> var : vars.entrySet()) {
                            portlet.setvar(String.valueOf(var.getKey()), var.getValue());
                        }
                    }
                }
                catch (Exception e) {
                    wga.getLog().error("Exception setting $portletVars of WebTML action '" + action.getDescription() + "' as Lookup table", e);
                }
            }
        }
        
        // Fire portlet events
        if (tmlscript.hasProperty(actionResult, "$portletEvents")) {
            Object portletEvents = tmlscript.callMethod(actionResult, "$portletEvents");
            try {
                @SuppressWarnings("unchecked")
                Map<String,Map<String,Object>> events = tmlscript.descriptify(portletEvents, Map.class);
                for (Map.Entry<String,Map<String,Object>> event : events.entrySet()) {
                    
                    PortletEvent ev = wga.tmlcontext().createevent(event.getKey());
                    for (Map.Entry<String,Object> param : event.getValue().entrySet()) {
                        ev.addParameter(param.getKey(), param.getValue());
                    }
                    portlet.fireevent(ev);
                }
            }
            catch (Exception e) {
                wga.getLog().error("Exception descriptifying $vars of WebTML action '" + action.getDescription() + "' as Lookup table", e);
            }
            
        }
        
        // Fire app events
        if (tmlscript.hasProperty(actionResult, "$appEvents")) {
            Object appEvents = tmlscript.callMethod(actionResult, "$appEvents");
            try {
                @SuppressWarnings("unchecked")
                Map<String,Map<String,Object>> events = tmlscript.descriptify(appEvents, Map.class);
                Event.Scope scope = Event.Scope.CLUSTER;
                for (Map.Entry<String,Map<String,Object>> event : events.entrySet()) {
                    ApplicationEventBuilder ev = wga.app().createEvent(event.getKey());
                    for (Map.Entry<String,Object> param : event.getValue().entrySet()) {
                        if (param.getKey().equals("$scope")) {
                            scope = (Event.Scope) param.getValue();
                        }
                        else {
                            ev = ev.param(param.getKey(), param.getValue());
                        }
                    }
                    switch (scope) {
                        case CLUSTER:
                            ev.fire();
                            break;
                        case SERVER:
                            ev.fireOnLocalServer();
                            break;
                        case SESSION:
                            ev.fireOnSession();
                            break;
                    }
                }
            }
            catch (Exception e) {
                wga.getLog().error("Exception descriptifying $vars of WebTML action '" + action.getDescription() + "' as Lookup table", e);
            }
            
        }
        
        // Redirect to some other page
        if (tmlscript.hasProperty(actionResult, "$redirect")) {
            Object redirectionScriptData = tmlscript.callMethod(actionResult, "$redirect");
            try {
            	if(redirectionScriptData instanceof String){
            		wga.redirectTo((String)redirectionScriptData);
            	}
            	else{
	                @SuppressWarnings("unchecked")
	                Map<Object,Object> redirectionData = tmlscript.descriptify(redirectionScriptData, Map.class);
	                String url = interpretRedirectionData((TMLContext) wga.tmlcontext(), redirectionData);
	                if (url != null) {
	                    wga.redirectTo(url);
	                }
            	}
            }
            catch (Exception e) {
                wga.getLog().error("Exception interpreting $redirect result of WebTML action '" + action.getDescription() + "' as String or Map", e);
            }
        }
    }


    @SuppressWarnings("unchecked")
    public static String interpretRedirectionData(TMLContext context, Map<Object, Object> redirectionData) throws WGException, URIException {

        
        // Read URL data from map
        TMLContext targetCx = null;
        String layout = null;
        String medium = "html";
        String db = null;
        String container = null;
        String formId = null;
        String file = null;
        
        if (redirectionData.containsKey("context")) {
            Object contextData = redirectionData.get("context");
            if (contextData instanceof TMLContext) {
                targetCx = (TMLContext) contextData;
            }
            else  {
                targetCx = context.context(String.valueOf(contextData), false);
                if (targetCx == null) {
                    context.addwarning("Unable to find target document of $redirect data: " + contextData);
                    return null;
                }
            }
        }
        

        String url = null;
        if (redirectionData.containsKey("url")) {
            url = String.valueOf(redirectionData.get("url"));
        }
        if (redirectionData.containsKey("layout")) {
            layout = String.valueOf(redirectionData.get("layout"));
        }
        if (redirectionData.containsKey("container")) {
            container = String.valueOf(redirectionData.get("container"));
        }
        if (redirectionData.containsKey("form")) {
            formId = String.valueOf(redirectionData.get("form"));
        }
        if (redirectionData.containsKey("file")) {
            file = String.valueOf(redirectionData.get("file"));
        }
        if (redirectionData.containsKey("medium")) {
            medium = String.valueOf(redirectionData.get("medium"));
        }
        if (redirectionData.containsKey("db")) {
            db = String.valueOf(redirectionData.get("db"));
        }
        
        // Types of redirection
        
        // - Redirect to content or content file
        if (targetCx != null) {
            if (file != null) {
                url = targetCx.fileurl(file);
            }
            else {
                url = targetCx.contenturl(medium, layout);
            }
        }
        
        // - Redirect to container file
        if (container != null) {
            if (file != null) {
                url = context.fileurl(db, container, file);
            }
            else {
                context.addwarning("Cannot use container property without file property $redirect data");
                return null;
            }
        }
        
        // - Redirect to form file
        if (formId != null) {
            if (file != null) {
                TMLForm tmlForm = context.tmlformbyid(formId);
                if (tmlForm != null) {
                    url =  tmlForm.fileurl(file);
                }
                else {
                    context.addwarning("Unknown form in $redirect data: " + formId);
                }
            }
            else {
                context.addwarning("Cannot use container property without file property $redirect data");
                return null;
            }
        }
        
        // - Redirect to layout url
        else if (layout != null) {
            url =  context.layouturl(db, medium, layout);
        }
        
        // Nothing matched
        if (url == null) {
            context.addwarning("Cannot interpret $redirect data with keys: " + redirectionData.keySet());
            return null;
        }
        
        
        // Enhance url: Absolute, URL params, var params
        Boolean absolute = null;
        if (redirectionData.containsKey("absolute")) {
            absolute = (Boolean) redirectionData.get("absolute");
        }
        
        Map<String,Object> urlParams = new HashMap<String,Object>();
        if (redirectionData.containsKey("urlParams")) {
            urlParams = (Map<String,Object>) redirectionData.get("urlParams");
        }
        Map<String,Object> varParams = new HashMap<String,Object>();
        if (redirectionData.containsKey("varParams")) {
            varParams = (Map<String,Object>) redirectionData.get("varParams");
        }
        
        if (absolute != null || urlParams != null) {
            URLBuilder builder = WGA.get(context).urlBuilder(url);
            if (urlParams != null) {
                for (Map.Entry<String,Object> param : urlParams.entrySet()) {
                    builder.setParameter(param.getKey(), String.valueOf(param.getValue()));
                }
            }
            if (varParams != null) {
                for (Map.Entry<String,Object> param : varParams.entrySet()) {
                    builder.setVarParameter(param.getKey(), param.getValue());
                }
            }
            
            if (absolute != null) {
                url = builder.build(absolute);
            }
            else {
                url = builder.buildLikeGiven();
            }
        }
        
        return url;

    }

    private WGPortlet reregisterPortletsFromPath(WGDatabase db, WGPortletRegistry reg, AjaxInfo ajaxInfo) throws WGAPIException {
    
        String appDb = reg.getApplicationId(db);
        WGPortlet portlet = reg.getOrCreateRootPortlet(appDb);
        for (String name : WGUtils.deserializeCollection(ajaxInfo.getPortletPath(), "/")) {
            if (!WGUtils.isEmpty(name)) {
                WGPortlet childPortlet = reg.createPortlet(appDb, portlet);
                childPortlet.setName(name);
                reg.insertPortlet(childPortlet);
                portlet = childPortlet;
            }
        }
        
        portlet.setDesign(ajaxInfo.getTmlmodule());
        portlet.setDesignDb(ajaxInfo.getDesignDB());
        return portlet;
        
    }

    private void reregisterChildPortlets(WGDatabase db, java.util.Collection<TMLPortletState> states, WGPortletRegistry reg, String parentKey) throws WGAPIException {
        
        String appDb = reg.getApplicationId(db);
        WGPortlet parentPortlet;
        if (parentKey == null) {
            parentPortlet = reg.getOrCreateRootPortlet(appDb);
        }
        else {
            parentPortlet = reg.getPortlet(appDb, parentKey);
        }
        
        if (parentPortlet == null) {
            _core.getLog().warn("Unable to re-register child portlets of parent " + parentKey + " because there was no state sent for it.");
            return;
        }
    
        for (TMLPortletState state : states) {
            if (WGUtils.nullSafeEquals(state.getParentPortletKey(), parentKey)) {
                WGPortlet portlet = reg.getPortlet(appDb, state.getPortletKey()); 
                if (portlet == null) {
                    WGPortlet childPortlet = reg.createPortlet(appDb, parentPortlet);
                    childPortlet.setName(state.getPortletName());
                    reg.insertPortlet(childPortlet);
                    reregisterChildPortlets(db, states, reg, state.getPortletKey());
                }
            }
        }
        
    }

}
