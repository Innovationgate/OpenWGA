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
package de.innovationgate.wgpublisher.webtml.utils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.collections.map.LinkedMap;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;
import org.apache.log4j.Logger;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.search.highlight.Fragmenter;
import org.apache.lucene.search.highlight.Highlighter;
import org.apache.lucene.search.highlight.InvalidTokenOffsetsException;
import org.apache.lucene.search.highlight.SimpleFragmenter;
import org.apache.lucene.search.highlight.SimpleHTMLFormatter;
import org.dom4j.Document;
import org.xml.sax.SAXException;

import com.google.gson.Gson;

import de.innovationgate.utils.Base64;
import de.innovationgate.utils.FormattingChain;
import de.innovationgate.utils.FormattingException;
import de.innovationgate.utils.ImageScaler;
import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.utils.ObjectFormatter;
import de.innovationgate.utils.TemporaryFile;
import de.innovationgate.utils.TransientObjectWrapper;
import de.innovationgate.utils.TransientWrappedMap;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.SearchDetails;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentEventListener;
import de.innovationgate.webgate.api.WGContentNavigator;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDesignDocument;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGFileAnnotations;
import de.innovationgate.webgate.api.WGFileContainer;
import de.innovationgate.webgate.api.WGFileDerivateMetaData;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGLanguageChooser;
import de.innovationgate.webgate.api.WGPortlet;
import de.innovationgate.webgate.api.WGPortletRegistry;
import de.innovationgate.webgate.api.WGQueryException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGTMLModule;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.webgate.api.auth.PasswordCachingAuthenticationModule;
import de.innovationgate.webgate.api.workflow.WGNamedWorkflow;
import de.innovationgate.webgate.api.workflow.WGWorkflow;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.common.beans.LuceneConfiguration;
import de.innovationgate.wga.common.beans.LuceneIndexItemRule;
import de.innovationgate.wga.common.beans.csconfig.v1.MediaKey;
import de.innovationgate.wga.common.beans.csconfig.v1.PluginID;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.TMLScript.ObjectType;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.TMLPage;
import de.innovationgate.wgpublisher.ClientHints;
import de.innovationgate.wgpublisher.DBLoginInfo;
import de.innovationgate.wgpublisher.PersonalisationManager;
import de.innovationgate.wgpublisher.RenderServletRequest;
import de.innovationgate.wgpublisher.RenderServletResponse;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.auth.CSAuthModule;
import de.innovationgate.wgpublisher.auth.Login;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager;
import de.innovationgate.wgpublisher.files.derivates.FileDerivateManager.DerivateQuery;
import de.innovationgate.wgpublisher.files.derivates.WGInvalidDerivateQueryException;
import de.innovationgate.wgpublisher.filter.WGAFilter;
import de.innovationgate.wgpublisher.filter.WGAFilter.RequestWrapper;
import de.innovationgate.wgpublisher.lang.WebTMLLanguageChooser;
import de.innovationgate.wgpublisher.lucene.LuceneManager;
import de.innovationgate.wgpublisher.lucene.LuceneSearchDetails;
import de.innovationgate.wgpublisher.mail.SmtpMail;
import de.innovationgate.wgpublisher.plugins.WGAPlugin;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemType;
import de.innovationgate.wgpublisher.so.ManagedObject;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.url.WGASpecificFileURLBuilder;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.ForEach;
import de.innovationgate.wgpublisher.webtml.IterationTag;
import de.innovationgate.wgpublisher.webtml.Query;
import de.innovationgate.wgpublisher.webtml.actions.MasterCustomAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionException;
import de.innovationgate.wgpublisher.webtml.actions.TMLActionLink;
import de.innovationgate.wgpublisher.webtml.env.IndependentDesignContext;
import de.innovationgate.wgpublisher.webtml.env.IndependentTMLScriptEnvironment;
import de.innovationgate.wgpublisher.webtml.env.IsolatedTMLContextEnvironment;
import de.innovationgate.wgpublisher.webtml.env.WebTMLContextEnvironment;
import de.innovationgate.wgpublisher.webtml.env.WebTMLDesignContext;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateStorage;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextEnvironment.RootEnvironmentUserData;

/**
 * Object representing a TML context, i.e. a pointer on a WGA Content Document that is used somewhere inside WebTML.
 * This object is identical to the "this" object in TMLScript and provides the same functionalities.
 */
@CodeCompletion (methodMode=CodeCompletion.MODE_EXCLUDE, beanMode=CodeCompletion.BEAN_MODE_LOWERCASED,delegate=Context.class)
public class TMLContext implements TMLObject, de.innovationgate.wga.server.api.tml.Context {
    
    public static final String SESSIONATTRIB_PROCESSCONTEXTS = "processContexts";
    
    public static class Serialized {
        
        private String _path;
        
        public Serialized(String path) {
            _path = path;
        }

        public Object deserialize(TMLContext context) {
            return context.context(_path);
        }
        
    }
    
    public static class PersistentFormsMap extends TransientWrappedMap<String,TMLForm> {
        @Override
        protected Map<String, TMLForm> initWrappedMap() {
            return new ConcurrentHashMap<String,TMLForm>();
        }
    }
    
    public static class WebTMLOccasion implements ProblemOccasion {

        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return TMLContext.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return false;
        }
        
    }
    
    @CodeCompletion
    public static final String ACTIONID_DIVIDER = "/";
    
    /**
     * Set as session attribute for tasks that execute master/async actions.
     * Contains the definition of the action that is executed in this task.
     */
    @CodeCompletion
    public static final Object SESSIONATTRIB_ASYNCACTION = "AsyncAction";
    
    @CodeCompletion
    public static ThreadLocal<LinkedList<TMLContext>> _threadMainContexts = new ThreadLocal<LinkedList<TMLContext>>();
    
    public static TMLContext getThreadMainContext() {
        
        LinkedList<TMLContext> contexts = _threadMainContexts.get();
        if (contexts != null && contexts.size() > 0) {
            return contexts.getLast();
        }
        else {
            return null;
        }
        
    }
    
    public void makeThreadMainContext() {
        
        LinkedList<TMLContext> contexts = _threadMainContexts.get();
        if (contexts == null) {
            contexts = new LinkedList<TMLContext>();
            _threadMainContexts.set(contexts);
        }
        
        if ("true".equals(System.getProperty("de.innovationgate.wga.threadmaincontexts.verbose"))) {
            try {
                StackTraceElement[] elements = Thread.currentThread().getStackTrace();
                getlog().info("Thread " + Thread.currentThread().hashCode() + " - Registering main context '" + getpath() + "' (" + System.identityHashCode(this) + ") at position " + contexts.size() + ", Stacktrace " + elements[2]);
            }
            catch (WGAPIException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        
        contexts.add(this);        
        
        if (contexts.size() == 100) {
            getlog().warn("Size of main contexts list on thread '" + Thread.currentThread().getName() + "' exceeds 100 entries. The main contexts management is most likely leaking!");
        }
        
    }
    
    public void removeThreadMainContext() {
        
        LinkedList<TMLContext> contexts = _threadMainContexts.get();
        
        
        if ("true".equals(System.getProperty("de.innovationgate.wga.threadmaincontexts.verbose"))) {
            try {
                StackTraceElement[] elements = Thread.currentThread().getStackTrace();
                getlog().info("Thread " + Thread.currentThread().hashCode() + " - Removing main context '" + getpath() + "' (" + System.identityHashCode(this) + ") at position " + (contexts.size() -  1) + ", Stacktrace " + elements[2]);
            }
            catch (WGAPIException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        
        if (contexts != null) {
            TMLContext lastContext = contexts.removeLast();
            if (lastContext != this) {
                try {
                    getlog().error("Error in WebTML context management: Thread main context mismatch on removal. Removed: " + this.getpath() + " (" + System.identityHashCode(this) + "), In stack: " + lastContext.getpath() + " (" + System.identityHashCode(lastContext) + ")");
                }
                catch (WGAPIException e) {
                }
            }
        }
        
    }
    
    public static void clearThreadMainContext() {
        LinkedList<TMLContext> contexts = _threadMainContexts.get();
        if (contexts != null) {
            contexts.clear();
        }
    }
    
    
    
    public static class ListVarContainer implements Serializable {

        private static final long serialVersionUID = -7274704419920612617L;
        
        private List<Object> _list;
        
        public ListVarContainer(List<Object> list) {
            _list = list;
        }

        /**
         * @return Returns the list.
         */
        public List<Object> getList() {
            return _list;
        }
        
    }
	
	private TMLContextEnvironment _environment = null;
	private TMLDesignContext _designContext = null;
    public TMLContextEnvironment getEnvironment() {
        return _environment;
    }


    public void importEnvironmentData(TMLContext context, boolean includeIntrusiveData) throws TMLException {
        _environment.importEnvironmentData(context, includeIntrusiveData);
    }
    
    public void importEnvironmentData(TMLContext context) throws TMLException {
        importEnvironmentData(context, true);
    }
    


    public Map getActionRegistration() {
        return _environment.getActionRegistration();
    }
    
    public Object callDefaultAction(TMLAction action, TMLActionLink link, Map<String, Object> objects) {
		
		try {
            TMLAction.DefaultAction defaultAction = TMLAction.getDefaultAction(action);
            if (defaultAction != null) {
                return defaultAction.call(action, this, link, objects);
            }
			else {
				addwarning("Unknown default action: " + link.getDefaultAction(), false);
				return null;
			}
		}
		catch (TMLException e) {
		    addwarning("Error executing default action " + link.getDefaultAction()  + ": " + e.getMessage(), false);
		    return false;
		}
		catch (Exception e) {
			addwarning("Error executing default action " + link.getDefaultAction()  + ": " + e.getMessage(), false);
			getlog().error("Error executing default action " + link.getDefaultAction(), e);
			return false;
		}
	
	}

	public ExpressionResult callCustomAction(TMLAction tmlAction, TMLActionLink actionLink, Map<String,Object> parentScopeObjects) throws WGAPIException {

		// Look if we must execute the action in a separate master action task.
	    // We must do so if
	    // a) we call a master action and are not already working under master rights
	    // b) we call an async action and are not already in a master action task to execute this action
	    boolean switchToMaster = tmlAction.isMaster() && !getdocument().getDatabase().getSessionContext().isMasterSession();
	    boolean switchToAsync = tmlAction.isAsync() && !WGUtils.nullSafeEquals(getdocument().getDatabase().getSessionContext().getAttribute(SESSIONATTRIB_ASYNCACTION), tmlAction);
		if (switchToMaster || switchToAsync) {
			MasterCustomAction masterAction = new MasterCustomAction(this, tmlAction, actionLink, parentScopeObjects);
			masterAction.start();
			if(masterAction.getResult() != null) {
			    return processActionResult(masterAction.getResult());
			}
			else if (tmlAction.isAsync()) {
			    return new ExpressionResult(null, false, false, null);
			}
			else {
			    return new ExpressionResult(null, false, false, new WGExpressionException("Master action did not return result", tmlAction.getCode()));
			}
		}

		// Set params
        
		for (int idx = 0; idx < actionLink.getUnnamedParams().size(); idx++) {
			setvar("tmlparam" + (idx + 1), actionLink.getUnnamedParams().get(idx));
		}
		
        // set additional objects and params. Keys must be lowercase so we have no doublettes in case-insensitive TMLScript
        Map<String,Object> additionalObjects = new HashMap<String,Object>();
        if (parentScopeObjects != null) {
            Iterator<Map.Entry<String,Object>> entries = parentScopeObjects.entrySet().iterator();
            while (entries.hasNext()) {
                Map.Entry<String,Object> entry = entries.next();
                additionalObjects.put(String.valueOf(entry.getKey()).toLowerCase(), entry.getValue());
            }
        }
        
        additionalObjects.put(RhinoExpressionEngine.PARAM_SCRIPTTIMEOUT, new Integer(tmlAction.getTimeout()));
        additionalObjects.put(RhinoExpressionEngine.PARAM_ACTIONDEFINITION, tmlAction);
        additionalObjects.put(TMLAction.OBJ_ACTION_PARAMS, actionLink.getUnnamedParams());
        
        // Temporarily set design db to originating db of action (if not default action)
        boolean designDBSwitched = false;
        TMLContext actionContext = this;
        if (tmlAction.getOrigin() != TMLAction.ORIGIN_DEFAULT) {
            try {
                TMLDesignContext actionDesignContext = tmlAction.createDesignContext(this, _designContext);
                actionContext = new TMLContext(this, actionDesignContext);               
            }
            catch (WGException e) {
                addwarning("Unable to switch design context bc. target database '" + tmlAction.getModuleDatabase() + "' cannot be opened", false);
            }
        }

        // Run action
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
        ExpressionResult result = engine.evaluateExpression(tmlAction.getCode(), actionContext, ExpressionEngine.TYPE_SCRIPT, additionalObjects);
		return processActionResult(result);
		
	}

	private ExpressionResult processActionResult(ExpressionResult result) throws WGExpressionException {
	    if (result.isError()) {
            WGExpressionException exc = result.getException();
            throw exc;                         
        }
        
        return result;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setvar(java.lang.String, java.lang.Object, boolean)
     */
    @SuppressWarnings("unchecked")
    public void setvar(String name, Object value, boolean keepList) throws WGAPIException {
		
        if (this.document.getDatabase().hasFeature(WGDatabase.FEATURE_STORESVARS) &&
            this.document.getDatabase().getBooleanAttribute(WGACore.DBATTRIB_VARPROVISIONING, true) == true) {
            
            try {
                this.document.setItemValue(name, value);
            }
            catch (WGAPIException e) {
                getlog().warn("Could not store WebTML variable as item on document bc. of exception. It will not be available in native expressions.", e);
            }
        } 
        
        name = convertVarName(name);
        
        if (value instanceof List && keepList) {
            value = new ListVarContainer((List<Object>) value);
        }
        
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
            getDesignContext().setLocalVarOnModule(name, value);
        }
        else {
            _environment.getPageVars().put(name, value);
        }	
		
	}
	
    public String convertVarName(String name){
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
        	return name;
        }
        else{
        	return name.toLowerCase();
        }
    }
    
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getvar(java.lang.String)
     */
	@Override
    public Object getvar(String name) throws WGAPIException {
		if (name == null) {
	        return null;
	    }	    
		
		name = convertVarName(name);
		
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
            Object value = getDesignContext().retrieveLocalVar(name);
            if (!(value instanceof NullPlaceHolder)) {
                return value;
            }
        }
		
		Map vars = _environment.getPageVars();
		if (vars.containsKey(name)) {
			return vars.get(name);
		}
		return null;
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getsessionvar(java.lang.String)
     */
	@Override
    public Object getsessionvar(String name) {
	    if (name == null) {
	        return null;
	    }	    
	    name = convertVarName(name);
		Map<String,TransientObjectWrapper<Object>> sessionVars = _environment.getSessionVars();
		if (sessionVars.containsKey(name)) {
			TransientObjectWrapper<Object> wrapper = sessionVars.get(name);
			if (wrapper != null) {
			    return wrapper.get();
			}
		}
		return null;
	}
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setvar(java.lang.String, java.lang.Object)
     */
    @Override
    public void setvar(String name, Object value) throws WGAPIException {
        setvar(name, value, true);
    }
	
	/**
	 * Tool method for the TMLScript engine defining the behaviour of setvar() short syntax
	 * Will look for possible existing variables to update before setting a new variable
	 * @param name Variable to set
	 * @param value Value to Set
	 * @throws WGAPIException
	 */
	public void updateOrSetVar(String name, Object value) throws WGException {
		
		String cname = convertVarName(name);
		
	    // Look for portlet vars and portlet session vars of that name (B00005E12)
        String portletNameSpace = (String) option(Base.OPTION_PORTLET_NAMESPACE);
            if (portletNameSpace != null) {
                TMLPortlet portlet = getportlet();
                if (portlet != null) {
    
                String pItemName = portlet.getVarPrefix() + cname;
                if (_environment.getPageVars().containsKey(pItemName)) {
                    portlet.setvar(name, value);
                    return;
                }
                
                Object pValue = portlet.retrieveSessionVar(name);
                if (pValue != null) {
                    portlet.setsessionvar(name, value);
                    return;
                }
            }
        }
	    
	    // Look for session var of that name
		if (_environment.getSessionVars().containsKey(cname)) {
		    this.setsessionvar(cname, value);
		    return;
		}
		
		// Compliance 7.2 - Look for local vars, then page vars of that name
		// Important to have this priority order for updating vars
		if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
		    
		    Object localVarValue = getDesignContext().retrieveLocalVar(cname);
		    if (!(localVarValue instanceof NullPlaceHolder)) {
		        setvar(cname, value);
		        return;
		    }
		    
		    TMLPage page = WGA.get(this).tmlPage();
		    if (page.hasVar(cname)) {
		        page.setVar(cname, value);
		        return;
		    }
		}

        // So we just set - (or maybe update, if compliance < 7.2) - a normal var
        this.setvar(cname, value);
		
	}

	private WGDocument document;
	private String role = null;
	private String lastError = null;
    
	/**
	 * Constructor for the main context of a WebTML request
	 * @param doc
	 * @param tag
	 */
	@CodeCompletion
    public TMLContext(WGDocument doc, de.innovationgate.wgpublisher.webtml.Base tag) {
	    
	    this.document = doc;
        _environment = new WebTMLContextEnvironment(this, tag.getPageContext());
        _designContext = new WebTMLDesignContext((WebTMLContextEnvironment) _environment, tag.getStatus(), null, null);
        
        if (this.document == null) {
            getwgacore().getLog().error("Request context with null content");
        }
    }
	
	
    	
    /**
     * Constructor to create new contextes that take environment and designcontext from a parent context
     * @param doc
     * @param parentContext
     */
    public TMLContext(WGDocument doc, TMLContext parentContext) {

        this.document = doc;
        this._environment = parentContext.getEnvironment();
        this._designContext = parentContext.getDesignContext();
        
		if (this.document == null) {
			getwgacore().getLog().error("Request context with null content");
		}
	}

    /**
     * Constructor to create clones of a context with a design context for a different WebTML tag 
     * @param doc
     * @param parentContext
     */
    public TMLContext(TMLContext parentContext, BaseTagStatus status) {
    
        this.document = parentContext.getdocument();
        _environment = parentContext.getEnvironment();
        if (!(_environment instanceof WebTMLContextEnvironment)) {
            throw new IllegalArgumentException("This constructor is only usable in WebTML context environments");
        }
	    
        _designContext = new WebTMLDesignContext((WebTMLContextEnvironment) _environment, status, status.getDesignDBKey(), status.getTMLModuleName());
        
        if (this.document == null) {
            throw new IllegalArgumentException("Request context with null content");
        }
    }
    	
    /**
     * Constructor to create clones of a context with the custom design context of a WebTML action 
     * @param doc
     * @param parentContext
     */
    private TMLContext(TMLContext parentContext, TMLDesignContext designContext) {

        this.document = parentContext.getdocument();
        _environment = parentContext.getEnvironment();
        _designContext = designContext;
        
		if (this.document == null) {
            throw new IllegalArgumentException("Request context with null content");
		}
	}
	
	public TMLContext(WGDocument doc, WGACore core, TMLUserProfile userProfile, TMLForm form, HttpServletRequest req, HttpServletResponse rsp, HttpSession session, TMLContext parentContext) {

	    this.document = doc;
	    
		
        // No environment given. This is a new independent TMLScript environmeent
        if (parentContext == null) {
            _environment = new IndependentTMLScriptEnvironment(this, core, userProfile, form, req, rsp, session);
            _designContext = new IndependentDesignContext((IndependentTMLScriptEnvironment) _environment, null, "");
        }
        // Derived context. We will load vars of the given environment
        else {
            _environment = parentContext.getEnvironment();
            _designContext =  parentContext.getDesignContext();
        }
        
        
	}
	
	@CodeCompletion
	public TMLContext(WGDocument doc, WGACore core, TMLUserProfile userProfile, TMLForm form) {
		this(doc, core, userProfile, form, null, null, null, null);
	}
	
	public TMLContext(WGDocument doc, WGACore core, TMLUserProfile userProfile, TMLForm form, HttpServletRequest req, HttpServletResponse rsp, HttpSession session) {
	    this(doc, core, userProfile, form, req, rsp, session, null);
	}

	public TMLContext getmaincontext() {
		return _environment.getMainContext();
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#item(java.lang.String)
     */
	@Override
    public Object item(String name) throws WGAPIException {
	    return item(name, Collections.<String,Object>emptyMap());
	}
	    
	public Object item(String name, Map<String,Object> params) throws WGAPIException {

		Object value = flattenList(retrieveItem(name, params), !getDesignContext().getVersionCompliance().isAtLeast(7,2));
        if (value instanceof NullPlaceHolder) {
            value = db().getNoItemBehaviour().getForTMLItem();
        }
        return value;

	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#item(java.lang.String, java.lang.String)
     */
    public Object item(String type, String name) throws WGAPIException {

		name = name.toLowerCase();

		if (type.equals("content")) {
			return item(name);
		}
		else if (type.equals("profile")) {
			TMLUserProfile profile = getprofile();
			if (profile == null) {
				addwarning("Current user has no profile", true);
				return "";
			}
			return profile.item(name);
		}
		else if (type.equals("portlet")) {
			TMLPortlet portlet = getportlet();
			if (portlet == null) {
			    if (!isbotrequest()) {
			        this.addwarning("Current user has no portlet registry", true);
			    }
				return "";
			}
			return portlet.item(name);
		}
		
		return "";
	}
	
	public boolean isbotrequest() {
        TMLUserProfile profile = getprofile();
        return (profile != null && profile.isdummy());
    }

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#itemlist(java.lang.String, java.lang.String)
     */
    public Object itemlist(String type, String name) throws WGAPIException {

		name = name.toLowerCase();

		if (type.equals("content")) {
			return itemlist(name);
		}
		else if (type.equals("profile")) {
			TMLUserProfile profile = getprofile();
			if (profile == null) {
				addwarning("Current user has no profile", true);
				return "";
			}
			return profile.itemlist(name);
		}
		else if (type.equals("portlet")) {
			TMLPortlet portlet = getportlet();
			if (portlet == null) {
			    if (!isbotrequest()) {
			        this.addwarning("Current user has no portlet registry", true);
			    }
				return "";
			}
			return portlet.itemlist(name);
		}
		
		return "";
	}
	


	public WGDocument getdocument() {
		return document;
	}
    
    public boolean hasVariable(String name) {
    	name = convertVarName(name);
        try {
            if (_environment.getPageVars().containsKey(name)) {
                return true;
            }
            if (_environment.getSessionVars().containsKey(name)) {
                return true;
            }
            if (!(_designContext.retrieveLocalVar(name) instanceof NullPlaceHolder)) {
                return true;
            }
        }
        catch (WGAPIException e) {
            addwarning("Exception testing for variable existence: " + name, false, e);
        }
        
        return false;
    }
    
    public boolean hasMappedItemValue(String name) {
    	name = convertVarName(name);
        if (this.getMappedItemValue(name) == null) {
            return false;
        } else {
            return true;
        }
    }

	private Object retrieveItem(String name, Map<String,Object> params) throws WGAPIException {
	    try {
            return new ItemExpression(this, name).retrieve(params, null, null);
        }
	    catch (WGAPIException e) {
	        throw e;
	    }
        catch (WGException e) {
            throw new WGAPIException(e);
        }
	}

	protected Object retrieveVar(String name) throws WGAPIException {
        
        if (name == null) {
            return null;
        }
        
        String cname = convertVarName(name);
        
		Map<String,Object> pageVars = _environment.getPageVars();
        
        // Portlet variables
        String portletNameSpace = (String) option(Base.OPTION_PORTLET_NAMESPACE);
        if (portletNameSpace != null) {
            TMLPortlet portlet = getportlet();
            if (portlet != null) {
            
                // Portlet variables. Stored along with regular WebTML variables plus prefix
                String pName = portlet.getVarPrefix() + cname;
                if (pageVars.containsKey(pName)) {
                    return pageVars.get(pName);
                }
    
                // Portlet session variables. Stored on the portlet session context, therefore retrieved via portlet object
                Object value = portlet.retrieveSessionVar(name);
                if (value != null) {
                    return value;
                }
                
            }
        }

        // Local variables (since 7.2)
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
            Object value = getDesignContext().retrieveLocalVar(cname);
            if (!(value instanceof NullPlaceHolder)) {
                return value;
            }
        }
        
		// Regular page (request) variables
		if (pageVars.containsKey(cname)) {
			return pageVars.get(cname);
		}

		// Session variables
		Map<String,TransientObjectWrapper<Object>> sessionVars = _environment.getSessionVars();
		if (sessionVars.containsKey(cname)) {			
			return sessionVars.get(cname).get();
		}
        
		return new NullPlaceHolder();
		
    }

	public static Object flattenList(Object object, boolean reduceList) {
	
		if (object == null) {
			return null;
		}

		if (object instanceof List) {
		    
		    if (!reduceList) {
		        return object;
		    }
		    
			List<?> list = (List<?>) object;
			if (list.size() == 0) {
				return null;
			}
			else {
				return ((List<?>) object).get(0);
			}

		}
        else if (object instanceof ListVarContainer) {
            return ((ListVarContainer) object).getList();
        }
		else {
			return object;
		}

	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#itemlist(java.lang.String)
     */
	@Override
    public List<Object> itemlist(String name) throws WGAPIException {
	    return itemlist(name, Collections.<String,Object>emptyMap());
	}
	    
	    
	public List<Object> itemlist(String name, Map<String,Object> params) throws WGAPIException {

        Object value = retrieveItem(name, params);
        if (value instanceof NullPlaceHolder) {
            return db().getNoItemBehaviour().getForTMLItemList();
        }
        else {
            return toList(value);
        }

	}
	
	public Iterable<Object> itemiterable(String name) throws WGException {
	    return itemiterable(name, Collections.<String,Object>emptyMap());
	}
	
	public Iterable<Object> itemiterable(String name, Map<String,Object> params) throws WGException {
	    Object value = retrieveItem(name, params);
        if (value instanceof NullPlaceHolder) {
            return db().getNoItemBehaviour().getForTMLItemList();
        }
        else {
            return toIterable(value);
        }
	}
	
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#highlightitem(java.lang.String, java.lang.String, java.lang.String)
     */
    @CodeCompletion
	public String highlightitem(String name, String prefix, String suffix) throws WGAPIException {
		return highlightitem(name, prefix, suffix, null);
	}
	
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#highlightitem(java.lang.String, java.lang.String, java.lang.String, java.lang.String)
     */
    @CodeCompletion
    public String highlightitem(String name, String prefix, String suffix, String encode) throws WGAPIException {
        if (name == null) {
            return itemTextValue(name, encode);
        }
        if (!getwgacore().isLuceneEnabled()) {
            addwarning("Unable to highlight item '" + name + "' bc. lucene is not enabled.");
            return itemTextValue(name, encode);
        }
        // try to retrieve last lucene query for highlighting
        org.apache.lucene.search.Query query = (org.apache.lucene.search.Query) getrequest().getSession().getAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY);
        if (query == null) {
            // no query in session - highlighting not possible
            addwarning("Lucene highlighting not possible because there is no query with enabled highlighting support");
            return itemTextValue(name, encode);
        }
        
        // lowercase name
        name = name.toLowerCase();           
        
        // create htmlformatter to highlight fragments with "$HIGHLIGHT_PREFIX$", "$HIGHLIGHT_SUFFIX$"
        // these placeholders are later on replaced by the given prefix and suffix
        // this additional step is necessary to encode the fragment text properly
        // see F00004C66
        String prefixPlaceholder = "$HIGHLIGHT_PREFIX$";
        String suffixPlaceholder = "$HIGHLIGHT_SUFFIX$";        
        SimpleHTMLFormatter formatter = new SimpleHTMLFormatter(prefixPlaceholder, suffixPlaceholder);

        // create highlighter
        Highlighter highlighter = getwgacore().getLuceneManager().createHighlighter(name, query, formatter);
        
        // retrieve itemtext
        String originalText = itemTextValue(name, encode);        
        if (originalText == null) {
            return null;
        }
        
        // create text to analyze
        LuceneConfiguration config = getwgacore().getLuceneManager().retrieveLuceneConfig(content().getDatabase().getDbReference());
        LuceneIndexItemRule rule = config.getMatchingItemRule(name);
        String analyzeText = rule.parseItemValue(originalText);

        // create tokenstream
        TokenStream tokenStream = getwgacore().getLuceneManager().createTokenStream(analyzeText, content());
        
        // create fragmenter and set fragmentsize to itemText.length to ensure only one fragments with the whole itemText is returned        
        Fragmenter fragmenter = new SimpleFragmenter(originalText.length() + 1); // if analyzeText.length == originalText.length we might get two fragments from lucene without the +1 (possible lucene bug)
        highlighter.setTextFragmenter(fragmenter);
                
        try {
            String highlighted = highlighter.getBestFragment(tokenStream, originalText.toString());
            if (highlighted != null) {
            	// replace highlight placeholders with correct prefix and suffix
            	highlighted = WGUtils.strReplace(highlighted, prefixPlaceholder, prefix, true);
            	highlighted = WGUtils.strReplace(highlighted, suffixPlaceholder, suffix, true);

            	return highlighted;
            } 
            else {
                return itemTextValue(name, encode);
            }
            }
        catch (IOException e) {
            addwarning("Unable to highlight item '" + name + "' bc. of exception '" + e.getMessage() + "'.");
            return itemTextValue(name, encode);
        } catch (InvalidTokenOffsetsException e) {
        	addwarning("Unable to highlight item '" + name + "' bc. of exception '" + e.getMessage() + "'.");
            return itemTextValue(name, encode);
		}                

    }

    /**
     * returns a singleton list with metavalues highlighted (surrounded with given <prefix> and <suffix>) based uppon the last lucene query with highlight attribute set to true
     * if highlighting is not possible this method returns metalist(<name>);
     * @param name
     * @param prefix
     * @param suffix
     * @return list 
     * @throws WGAPIException 
     * @throws WGAPIException
     */
	@CodeCompletion
    public List highlightMeta(String name, String prefix, String suffix) throws WGAPIException {
    	return highlightMeta(name, prefix, suffix, null);
    }

    

    /**
     * returns a singleton list with metavalues highlighted (surrounded with given <prefix> and <suffix>) based uppon the last lucene query with highlight attribute set to true
     * if highlighting is not possible this method returns metalist(<name>);
     * @param name
     * @param prefix
     * @param suffix
     * @param encode
     * @return list 
     * @throws WGAPIException
     */
	@CodeCompletion
    public List highlightMeta(String name, String prefix, String suffix, String encode) throws WGAPIException {
        if (name == null) {
            return metalist(name);
        }
        if (!getwgacore().isLuceneEnabled()) {
            addwarning("Unable to highlight meta '" + name + "' bc. lucene is not enabled.");
            return metalist(name);
        }
        // try to retrieve last lucene query for highlighting
        org.apache.lucene.search.Query query = (org.apache.lucene.search.Query) getrequest().getSession().getAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY);
        if (query == null) {
            // no query in session - highlighting not possible
            return metalist(name);
        }
                
        // create htmlformatter to highlight fragments with "$HIGHLIGHT_PREFIX$", "$HIGHLIGHT_SUFFIX$"
        // these placeholders are later on replaced by the given prefix and suffix
        // this additional step is necessary to encode the fragment text properly
        String prefixPlaceholder = "$HIGHLIGHT_PREFIX$";
        String suffixPlaceholder = "$HIGHLIGHT_SUFFIX$";
        SimpleHTMLFormatter formatter = new SimpleHTMLFormatter(prefixPlaceholder, suffixPlaceholder);

        // create highlighter
        Highlighter highlighter = getwgacore().getLuceneManager().createHighlighter(name.toUpperCase(), query, formatter);
        
        // retrieve metatext
        String originalText = metaTextValue(name, encode);        
        if (originalText == null) {
            return metalist(name);
        }
        
        // create tokenstream
        TokenStream tokenStream = getwgacore().getLuceneManager().createTokenStream(originalText, content());
        
        // create fragmenter and set fragmentsize to metaText.length to ensure only one fragments with the whole metaText is returned        
        Fragmenter fragmenter = new SimpleFragmenter(originalText.length() + 1); // +1 is necessary here 
        highlighter.setTextFragmenter(fragmenter);
                
        try {
            String highlighted = highlighter.getBestFragment(tokenStream, originalText);
            if (highlighted != null) {

                // replace highlight placeholders with correct prefix and suffix
            	highlighted = WGUtils.strReplace(highlighted, prefixPlaceholder, prefix, true);
            	highlighted = WGUtils.strReplace(highlighted, suffixPlaceholder, suffix, true);
            	            	
                return Collections.singletonList(highlighted);
            } else {
                return metalist(name);
            }
        } catch (IOException e) {
            addwarning("Unable to highlight meta '" + name + "' bc. of exception '" + e.getMessage() + "'.");
            return metalist(name);
        } catch (InvalidTokenOffsetsException e) {
        	addwarning("Unable to highlight meta '" + name + "' bc. of exception '" + e.getMessage() + "'.");
            return metalist(name);
		}                

    }
    

    /**
     * retrieves a list of the best fragments from the given contentItem based upon the last lucene query with highlight attribute set to true
     * query hits are not highlighted in fragments 
     * @param itemname the item fragements should be retrieved from
     * @param fragmentSize the number of characters for each fragment
     * @param maxFragments the maximum number of fragements returned
     * @return list of fragements (Strings) - if no lucene query present returns EMPTY_LIST
     * @throws WGAPIException
     */
    @CodeCompletion
    public List bestfragments(String itemname, int fragmentSize, int maxFragments) throws WGAPIException {
        return bestfragments(itemname, fragmentSize, maxFragments, "", "");
    }
    
    
    /**
     * retrieves a list of the best fragments from the given contentItem based upon the last lucene query with highlight attribute set to true
     * query hits are highlighted (surrounded by given <prefix> and <suffix>) in fragments
     * @param itemname the item fragements should be retrieved from
     * @param fragmentSize the number of characters for each fragment
     * @param maxFragments the maximum number of fragements returned
     * @param prefix the prefix for highlighting the search term in fragements
     * @param suffix the suffix for highlighting the search term in fragements
     * @return list of fragements (Strings) - if no lucene query present returns EMPTY_LIST
     * @throws WGAPIException
     */
    @CodeCompletion
    public List bestfragments(String itemname, int fragmentSize, int maxFragments, String prefix, String suffix) throws WGAPIException {
    	return bestfragments(itemname, fragmentSize, maxFragments, prefix, suffix, null);
    }
    
    /**
     * retrieves a list of the best fragments from the given contentItem based upon the last lucene query with highlight attribute set to true
     * query hits are highlighted (surrounded by given <prefix> and <suffix>) in fragments
     * @param itemname the item fragements should be retrieved from
     * @param fragmentSize the number of characters for each fragment
     * @param maxFragments the maximum number of fragements returned
     * @param prefix the prefix for highlighting the search term in fragements
     * @param suffix the suffix for highlighting the search term in fragements
     * @param encode encode each fragment in the given encoding - prefix and suffix will not be encoded
     * @return list of fragements (Strings) - if no lucene query present returns EMPTY_LIST
     * @throws WGAPIException
     */
    @CodeCompletion
    public List bestfragments(String itemname, int fragmentSize, int maxFragments, String prefix, String suffix, String encode) throws WGAPIException {
        // check preconditions for highlighting
        if (itemname == null) {
            throw new WGIllegalArgumentException("Unable to retrieve best fragments for item 'null'.");
        }        
        if (!getwgacore().isLuceneEnabled()) {
            addwarning("Unable to highlight item '" + itemname + "' bc. lucene is not enabled.");
            return Collections.EMPTY_LIST;
        }
        // try to retrieve last lucene query for highlighting
        org.apache.lucene.search.Query query = (org.apache.lucene.search.Query) getrequest().getSession().getAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY);
        if (query == null) {
            // no query in session - highlighting not possible
            return Collections.EMPTY_LIST;
        }
                        
        // lowercase name
        itemname = itemname.toLowerCase();           
        
        // create htmlformatter to highlight fragments with "$HIGHLIGHT_PREFIX$", "$HIGHLIGHT_SUFFIX$"
        // these placeholders are later on replaced by the given prefix and suffix
        // this additional step is necessary to encode the fragment text properly
        // see B00004BBE
        String prefixPlaceholder = "$HIGHLIGHT_PREFIX$";
        String suffixPlaceholder = "$HIGHLIGHT_SUFFIX$";
        SimpleHTMLFormatter formatter = new SimpleHTMLFormatter(prefixPlaceholder, suffixPlaceholder);

        // create highlighter
        Highlighter highlighter = getwgacore().getLuceneManager().createHighlighter(itemname, query, formatter);
        
        // retrieve itemtext
        String text = itemTextValue(itemname, "none");        
        if (text == null) {
            return Collections.EMPTY_LIST;
        }
        
        // remove html/xml from text
        // fragments should not contain html/xml bc. of design issues
        try {
            text = WGUtils.toPlainText(text, " ", false);
            // B000049EA
            // if the item value contains encoded html entities these entities has been converted to their characters
            // we should do an html encode for sure
            // text = WGUtils.encodeHTML(text); --> has side effects @see B00004BBE
        }
        catch (IOException e) {
            addwarning("Unable to highlight item '" + itemname + "' bc. of exception '" + e.getMessage() + "'.");
            return Collections.EMPTY_LIST;
        }
        
        // create tokenstream
        TokenStream tokenStream = getwgacore().getLuceneManager().createTokenStream(text, content());
                
        // create fragmenter
        Fragmenter fragmenter = new SimpleFragmenter(fragmentSize);
        highlighter.setTextFragmenter(fragmenter);
        
        try {
            String[] highlighted = highlighter.getBestFragments(tokenStream, text, maxFragments);            
            if (highlighted != null) {
                ArrayList list = new ArrayList();
                for (int i=0; i < highlighted.length; i++) {
                	// B00004BBE
                	// evtl. encode fragment
                	String fragment  = highlighted[i];              
                	if (encode != null) {
                		try {
							fragment = multiencode(encode, fragment);
						} catch (FormattingException e) {
							addwarning("Unable to highlight item '" + itemname + "' bc. of formating exception '" + e.getMessage() + "'.");
				            return Collections.EMPTY_LIST;
						}
                	}
                	// B00004BBE
                	// replace highlight placeholders with correct prefix and suffix
                	fragment = WGUtils.strReplace(fragment, prefixPlaceholder, prefix, true);
                	fragment = WGUtils.strReplace(fragment, suffixPlaceholder, suffix, true);
                    list.add(fragment);
                }
                return list;
            } else {
                return Collections.EMPTY_LIST;
            }
        } catch (IOException e) {
            addwarning("Unable to highlight item '" + itemname + "' bc. of exception '" + e.getMessage() + "'.");
            return Collections.EMPTY_LIST;
        } catch (InvalidTokenOffsetsException e) {
        	addwarning("Unable to highlight item '" + itemname + "' bc. of exception '" + e.getMessage() + "'.");
            return Collections.EMPTY_LIST;
		}                
    }
    
    /**
     * removes the last lucene query from session
     * should be called when you are sure you do not need further highlighting of content items
     * for the last executed lucene query with highlight=="true"
     */
    @CodeCompletion
    public void removelucenequery() {
        getrequest().getSession().removeAttribute(Query.SESSION_ATTRIBUTE_SIMPLIFIED_LUCENEQUERY);
    }
    
    /**
     * returns the itemvalue as text, concatenating eventual list elements and doing encoding
     * @param name Item name
     * @param Optional WebTML encoding. Use null to have none.
     * @return itemvalue as text, null if item is not found or itemvalue is null or itemvalues are no Strings
     * @throws WGAPIException
     */
    private String itemTextValue(String name, String encode) throws WGAPIException {
        if (name == null) {
            return null;
        }
        
        StringBuffer text = new StringBuffer();
        name = name.toLowerCase();
        List itemValues = itemlist(name);
        if (itemValues == null) {
            return null;
        }
        
        Iterator values = itemValues.iterator();
        boolean firstPart = true;
        while (values.hasNext()) {
            Object itemValue = values.next();            
            if (itemValue instanceof String) {
                String value = (String) itemValue;
                if (value != null) {
                    if (firstPart) {
                        firstPart = false;
                    }
                    else {
                        text.append(" ");
                    }
                    
                    text.append(value);
                } 
            }
        }
        
        
        // Optionally encode
        String finalText;
        if (encode != null && !encode.equals("none") && true) {
            try {
                finalText = multiencode(encode, text.toString());
            }
            catch (FormattingException e) {
                addwarning("Unable to highlight item '" + name + "' bc. of formating exception '" + e.getMessage() + "'.");
                return itemTextValue(name, null);
            }
        }
        else {
            finalText = text.toString();
        }
        
        
        return finalText;
        

    }
    
    
    /**
     * returns the metavalue as single text, concatenating eventual list elements and doing encoding
     * @param name
     * @return metavalue as text, null if item is not found or metavalue is null or metavalues are no Strings
     * @throws WGAPIException
     */
    private String metaTextValue(String name, String encode) throws WGAPIException {
        
        if (name == null) {
            return null;
        }
        
        StringBuffer text = new StringBuffer();
        name = name.toLowerCase();
        List metaValues = metalist(name);
        if (metaValues == null) {
            return null;
        }
        
        Iterator values = metaValues.iterator();
        boolean firstPart = true;
        while (values.hasNext()) {
            Object metaValue = values.next();            
            if (metaValue instanceof String) {
                String value = (String) metaValue;
                if (value != null) {
                    if (firstPart == true) {
                        firstPart = false;
                    }
                    else {
                        text.append(" ");
                    }
                    text.append(value);
                } 
            }
        }
        
        // Optionally encode
        String finalText;
        if (encode != null && !encode.equals("none") && true) {
            try {
                finalText = multiencode(encode, text.toString());
            }
            catch (FormattingException e) {
                addwarning("Unable to highlight meta '" + name + "' bc. of formating exception '" + e.getMessage() + "'.");
                return metaTextValue(name, null);
            }
        }
        else {
            finalText = text.toString();
        }
        
        return text.toString();
        
    }
    
    @SuppressWarnings("unchecked")
    public Iterable<Object> toIterable(Object value) throws WGException {
        
        if (value == null) {
            return new ArrayList<Object>();
        }
        
        
        if (value instanceof Iterable<?>) {
            return (Iterable<Object>) value;
        }
        
        
        if (value instanceof Object[]) {
            List<Object> list = new ArrayList<Object>();
            list.addAll(Arrays.asList((Object[]) value));
            return list;
        }
        
        if (value instanceof ListVarContainer) {
            return ((ListVarContainer) value).getList();
        }
        
        
        if (value instanceof java.util.Collection) {
            return new ArrayList<Object>((java.util.Collection<?>) value);
        }
        
        TMLScript tmlScript = WGA.get(this).tmlscript();
        if (tmlScript.isNativeObject(value)) {
            
            // Legacy functionality for deserializing native XML
            int scriptType = ExpressionEngineFactory.getTMLScriptEngine().determineTMLScriptType(value);
            if (scriptType != RhinoExpressionEngine.TYPE_NOTMLSCRIPT) {
                if (scriptType == RhinoExpressionEngine.TYPE_XMLLIST) {
                    return (List<Object>) ExpressionEngineFactory.getTMLScriptEngine().convertXMLListToList(value);
                }
            }
            
            // Modern functionality using descriptify
            try {
                return tmlScript.descriptify(value, Iterable.class);
            }
            catch (WGException e) {
                addwarning("Cannot use native TMLScript object as iterable: " + value.getClass().getName());
            }
            
        }
        
        
        List<Object> list = new ArrayList<Object>();
        list.add(value);
        return list;
        
        
    }

	@SuppressWarnings("unchecked")
    public static List<Object> toList(Object value) {

		if (value == null) {
			return new ArrayList<Object>();
		}
		else if (value instanceof Object[]) {
			List<Object> list = new ArrayList<Object>();
			list.addAll(Arrays.asList((Object[]) value));
			return list;
		}
		else if (value instanceof List<?>) {
			return (List<Object>) value;
		}
        else if (value instanceof ListVarContainer) {
            return ((ListVarContainer) value).getList();
        }
		else if (value instanceof java.util.Collection) {
			return new ArrayList<Object>((java.util.Collection<?>) value);
		}
        else if (ExpressionEngineFactory.getTMLScriptEngine().determineTMLScriptType(value) == RhinoExpressionEngine.TYPE_XMLLIST) {
            return (List<Object>) ExpressionEngineFactory.getTMLScriptEngine().convertXMLListToList(value);
        }
		else {
			List<Object> list = new ArrayList<Object>();
			list.add(value);
			return list;
		}
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#metalist(java.lang.String, java.lang.String)
     */
	@Override
    public List metalist(String type, String name) throws WGAPIException {

		return toList(retrieveMeta(type, name));

	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#metalist(java.lang.String)
     */
	@Override
    public List metalist(String name) throws WGAPIException {
		return metalist("content", name);
	}

	private Object getMappedMetaValue(String name) {

		String expression = getMetaMappingExpression(name);

		if (expression != null) {
			ExpressionEngine engine =
				ExpressionEngineFactory.getEngine(
					this.content().getDatabase().getAttribute(WGACore.DBATTRIB_EXPRESSION_DEFAULT).toString());
			ExpressionResult result = engine.evaluateExpression(expression, this, ExpressionEngine.TYPE_EXPRESSION, null);
			return result.getResult();
		}
		else {
			return null;
		}

	}

	
    private String getMetaMappingExpression(String name) {
	    @SuppressWarnings("unchecked")
        Map<String,String> mappings = (Map<String,String>) this.document.getDatabase().getAttribute(WGACore.DBATTRIB_META_MAPPINGS);
		return mappings.get(name.toLowerCase());
	}

	protected Object getMappedItemValue(String name) {
	
		String expression = getItemMappingExpression(name);

		if (expression != null) {
			ExpressionEngine engine =
				ExpressionEngineFactory.getEngine(this.content().getDatabase().getAttribute(WGACore.DBATTRIB_EXPRESSION_DEFAULT).toString());
			ExpressionResult result = engine.evaluateExpression(expression, this, ExpressionEngine.TYPE_EXPRESSION, null);
			return result.getResult();
		}
		else {
			return null;
		}

	}

	private String getItemMappingExpression(String name) {
		@SuppressWarnings("unchecked")
        Map<String,String> mappings = (Map<String,String>) this.getdocument().getDatabase().getAttribute(WGACore.DBATTRIB_ITEM_MAPPINGS);
		name = convertVarName(name);
		return mappings.get(name);
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setSessionVar(java.lang.String, java.lang.Object, boolean, boolean)
     */
    public void setSessionVar(String name, Object value, boolean allowSerialization, boolean keepList) {
        
    	name = convertVarName(name);
    	
        if (value instanceof List && keepList) {
            value = new ListVarContainer((List) value);
        }
        
        boolean makeTransient = false;
        if (!allowSerialization || (getwgacore().getVariousServerOptionReader().readOptionValueOrDefault(WGACore.SERVEROPTION_SERVER_TESTSESSIONVARSERIALIZABLE).equals(Boolean.TRUE) && !WGUtils.isSerializable(value))) {
            makeTransient = true;
        }
        
        TransientObjectWrapper wrapper = null;
        wrapper = _environment.getSessionVars().get(name);
        if (wrapper != null && wrapper.isTransient() == makeTransient) {
           wrapper.set(value);
        }
        else {
           wrapper = TransientObjectWrapper.create(value, makeTransient);
            _environment.getSessionVars().put(name, wrapper);
        }
	}
    
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setsessionvar(java.lang.String, java.lang.Object, boolean)
     */
	@Override
    public void setsessionvar(String name, Object value, boolean allowSerialisation) {
		setSessionVar(name, value, allowSerialisation, true);
	}
	
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setsessionvar(java.lang.String, java.lang.Object)
     */
    @Override
    public void setsessionvar(String name, Object value) {
        setSessionVar(name, value, true, true);
    }
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#removesessionvar(java.lang.String)
     */
	@Override
    public void removesessionvar(String name) {
		name = convertVarName(name);
		_environment.getSessionVars().remove(name);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#content()
     */
	@Override
    public WGContent content() {
		return getcontent();
	}
	
	@CodeCompletion
	public WGContent getcontent() {

		if (this.document instanceof WGContent) {
			return (WGContent) this.document;
		}
		else {
			return null;
		}

	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#context(java.lang.String, boolean)
     */
	@Override
    public TMLContext context(String expression, boolean returnContextOnError) {
	    try {
            TMLContextExpression expr = new TMLContextExpression(expression, this, db().getDefaultLanguage());
            return expr.processExpression(this, returnContextOnError);
        }
        catch (WGAPIException e) {
            this.setLastError("Unable to change context to (" + expression + ") bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage());
            return (returnContextOnError ? this : null);
        }
	}
	
	



    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#context(java.lang.String)
     */
    @Override
    public TMLContext context(String expression) {
        return context(expression, true);
    }

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#meta(java.lang.String, java.lang.String)
     */
	@Override
    public Object meta(String type, String name) throws WGAPIException {

		return flattenList(retrieveMeta(type, name), !getDesignContext().getVersionCompliance().isAtLeast(7,2));

	}

	private Object retrieveMeta(String type, String originalName) throws WGAPIException {

	    try {
	    
    		this.setLastError(null);
    
    		type = (type != null ? type.toLowerCase().trim() : "content");
    		String name = originalName.toLowerCase().trim();
    		if (type.equals("content")) {
    			return getContentMetaData(name);
    		}
    		else if (type.equals("profile")) {
    			if (this.getprofile() != null) {
    				return this.getprofile().metalist(name);
    			}
    			else {
    				this.addwarning("Current user has no profile", false);
    				return "";
    			}
    		}
    		else if (type.equals("database") || type.equals("db")) {
    			return getDatabaseMetaData(name, originalName);
    		}
    		else if (type.equals("session")) {
    			return getSessionMetaData(name);
    		}
    		else if (type.startsWith("domain")) {
    	        return getDomainMetaData(type, name); 
            }
    		else if (type.equals("request")) {
    			return getRequestMetaData(name);
    		}
    		else if (type.equals("response")) {
    			return getResponseMetaData(name);
    		}
    		else if (type.equals("taginfo")) {
    			this.setLastError("Taginfos not reachable via this.meta. Use this.taginfo instead!");
    			return null;
    		}
    		else {
    			return null;
    		}
		
	    }
	    catch (WGAPIException e) {
	        throw e;
	    }
	    catch (WGException e) {
	        throw new WGAPIException(e);
	    }

		
	}

	private Object getDomainMetaData(String type, String name) {
        
	    String domainName = (String) db().getAttribute(WGACore.DBATTRIB_DOMAIN);
	    int colonPos = type.indexOf(":");
	    if (colonPos != -1) {
	        domainName = type.substring(colonPos + 1).trim();
	    }
	    
	    if (name.equalsIgnoreCase("name")) {
	        return domainName;
	    }
	    
	    else if (name.equalsIgnoreCase("username")) {
	        DBLoginInfo loginInfo = WGACore.getSessionLogins(gethttpsession()).get(domainName);
	        if (loginInfo != null) {
	            return loginInfo.getUserName();
	        }
	        else {
	            return WGDatabase.ANONYMOUS_USER;
	        }
	    }
	    
	    else if (name.equalsIgnoreCase("defaultmanager")) {
	        WGADomain dc = getwgacore().getDomains(domainName);
	        if (dc == null) {
                addwarning("No configuration found for domain: " + domainName);
                return null;
            }
	        
	        return dc.getConfig().getDefaultManager();
	    }
	    
	    else if (name.equalsIgnoreCase("authentication") || name.equalsIgnoreCase("auth")) {
	        WGADomain dc = getwgacore().getDomains(domainName);
            if (dc == null) {
                addwarning("No configuration found for domain: " + domainName);
                return null;
            }
            
            AuthenticationModule authModule = dc.getAuthModule();
            if (authModule != null) {
                return authModule.getAuthenticationSource();
            }
            else {
                return null;
            }
	    }
	    
	    else if (name.equalsIgnoreCase("databases")) {
	        return getwgacore().getDatabasesForDomain(domainName);
	    }
	    
	    else {
    	    this.addwarning("Unknown domain metadata name: " + name, true);
            return "";
	    }
	    
    }


    private Object getRequestMetaData(String name) throws WGException {
		
        WGA wga = WGA.get(this);
        
        try {
    		if (name.equals("url")) {
    			return wga.call().getURL();
    		}
    		else if (name.equals("wgaurl")) {
    			return wga.urlBuilder(wga.server().getBaseURL()).build(false);
    		}
    		else if (name.equals("absolutewgaurl")) {
    			return wga.server().getBaseURL();
    		}
    		else if (name.equals("query_string")) {
    			return wga.call().getQueryString();
    		}
    		else if (name.equals("http_referer")) {
    			return wga.call().getReferrer();
    		}
    		else if (name.equals("http_user_agent")) {
    		    return wga.call().getUserAgent();
    		}
    		else if (name.equals("http_host") || name.equals("server_name")) {
    			return wga.call().getJavaRequest().getServerName();
    		}
    		else if (name.equals("path_info")) {
    			return wga.call().getJavaRequest().getPathInfo();
    		}
    		else if (name.equals("path_info_decoded")) {
    			return wga.call().getJavaRequest().getPathTranslated();
    		}
    		else if (name.equals("remote_addr")) {
    			return wga.call().getClient();
    		}
    		else if (name.equals("remote_host")) {
    			return wga.call().getJavaRequest().getRemoteHost();
    		}
    		else if (name.equals("remote_user")) {
    			return wga.call().getJavaRequest().getRemoteUser();
    		}
    		else if (name.equals("request_method")) {
    			return wga.call().getRequestMethod();
    		}
    		else if (name.equals("server_protocol")) {
    			return wga.call().getJavaRequest().getProtocol();
    		}
    		else if (name.equals("server_port")) {
    			return wga.call().getPort();
    		}
    		else if (name.equals("mainmedium")) {
    		    return wga.call().getMediaKey();
    		}
    		else if (name.equals("ajax")) {
    		    return wga.call().isAjax();
    		}
    		else {
    			return null;
    		}
        }
        catch (UnavailableResourceException e) {
            this.setLastError("Cannot retrieve request metadata because this script does not run inside a WebTML request");
            return null;
        }

	}
	
	private Object getResponseMetaData(String name) {
		
		if (!getEnvironment().isPageContextAvailable()) {
			this.setLastError("Cannot retrieve response metadata because this script does not run inside a WebTML page");
			return null;
		}

		if (name.equals("character_encoding")) {
			return this.getresponse().getCharacterEncoding();
		}
		else {
			return null;
		}

	}
	

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getprofile()
     */
	@Override
    public TMLUserProfile getprofile() {
	    return _environment.getUserProfile(getdocument().getDatabase());
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getportlet()
     */
	@Override
    public TMLPortlet getportlet() throws WGAPIException {
	    
	    // No portlets without request
	    if (_environment.getRequest() == null) {
	        return null;
	    }
	    
		String nameSpace = (String) option(Base.OPTION_PORTLET_NAMESPACE);
		return getportletbykey(nameSpace);

	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#portletbypath(java.lang.String)
     */
	@Override
    public TMLPortlet portletbypath(String path) throws WGException {
	    
	    List<String> elements = WGUtils.deserializeCollection(path, "/", true);
	    TMLPortlet p = getportlet();
	    if (p == null) {
	        setLastError("Cannot perform portletByPath(). No portlet in environment.");
	        return null;
	    }
	    
	    
	    boolean firstElement = true;
	    for (String elem : elements) {
	        
	        
	        if (elem.equals("")) {
	            if (firstElement) { // A starting slash means start from root
	                p = p.getroot();
	            }
	        }
	        
	        else if (elem.equals("..")) {
	            p = p.getparentportlet();
	        }
	        
	        else if (elem.startsWith("parent:")) {
	            String parentName = elem.substring(7).trim();
	            TMLPortlet parent = p;
	            while (parent != null && !parentName.equals(parent)) {
	                parent = parent.getparentportlet();
	                if (parent.isroot()) {
	                    parent = null;
	                }
	            }
	            
	            p = parent;
	            
	        }
	        
	        else if (elem.startsWith("name:")) {
	            String name = elem.substring(5);
	            p = p.getportletforname(name); 
	        }
	        
	        else if (elem.startsWith("key:")) {
                String key = elem.substring(4);
                p = p.getportlet(key); 
            }
	        
	        else {
	            p = p.getportletforname(elem);
	        }
	        
	        if (p == null) {
	            setLastError("Portlet path '" + path + "' unresolvable at element '" + elem + "'");
	            return null;
	        }
           
	        firstElement = false;

	    }
	    
	    return p;
	    
	}


    public TMLPortlet getportletbykey(String nameSpace) throws WGAPIException {
        TMLUserProfile profile = this.getmaincontext().getprofile();
        if (profile == null || !profile.getprofile().getDatabase().isSessionOpen()) {
            return null;
        }
        
        WGPortletRegistry portletRegistry = getPortletRegistry();
        if (portletRegistry == null) {
            return null;
        }
        
		WGPortlet portlet;
		String appDb = portletRegistry.getApplicationId(getmaincontext().db());
        if (nameSpace != null) {
		    portlet = portletRegistry.getPortlet(appDb, nameSpace);
		}
		else {
			portlet = portletRegistry.getOrCreateRootPortlet(appDb);
		}

		if (portlet != null) {
		    return new TMLPortlet(this, profile, portlet);
		}
		else {
		    return null;
		}
    }

    public WGPortletRegistry getPortletRegistry() throws WGAPIException {
        
        WGPortletRegistry pReg = null;
        
        if (_environment.getRequest() != null) {
            pReg = (WGPortletRegistry) _environment.getRequest().getAttribute(WGACore.ATTRIB_TRANSIENTPORTLETREGISTRY);
        }
        
        if (pReg == null) {
            pReg = getmaincontext().getprofile().getprofile().getPortletRegistry();
        }
        
        return pReg;
    }
    
    public TMLPortletStateStorage getPortletStateStorage() {
        return (TMLPortletStateStorage) getrequest().getAttribute(WGACore.ATTRIB_PORTLETSTATESTORAGE);
    }

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#meta(java.lang.String)
     */
	@Override
    public Object meta(String name) throws WGAPIException {
		return this.meta("content", name);
	}

	private Object getContentMetaData(String name) throws WGException {

		WGContent content = this.content();
		if (content == null) {
			return this.getdocument().getMetaData(name);
		}

		Object mappingresult = this.getMappedMetaValue(name);
		if (mappingresult != null) {
			return mappingresult;
		}

		if (name.equals("created")) {
			return content.getCreated();
		}
		else if (name.equals("modified")) {
			return content.getLastModified();
		}
		else if (name.equals("pagecreated")) {
            return content.getStructEntry().getCreated();
        }
		else if (name.equals("url")) {
			return contenturl();
		}
        else if (name.equals("titlepath")) {
            if (content.getStructEntry() != null)
	            return content.getStructEntry().getTitlePath();
            else return "";
        }
        else if (name.equals("pagetitle")) {
            return content.getStructEntry().getTitle();
        }
		else if (name.equals("uniquename") || name.equals("name") || name.equals("docname")) {
			return content.getUniqueName();
		}
		else if (name.equals("pageuniquename") || name.equals("pagename") || name.equals("pagedocname")) {
		    if (content.getStructEntry() != null)
		    	return content.getStructEntry().getUniqueName();
		    else return null;
        }
		else if (name.equals("position")) {
		    if (content.getStructEntry() != null) {
				return content.getStructEntry().getPosition();
			}
		    else {
		        return 0;
		    }
		}
		else if (name.equals("pagesequence")) {
			if (content.getStructEntry() != null) {
				return content.getStructEntry().getPageSequence();
			}
			else return 0;
		}
		else if (name.equals("key")) {
			return content.getContentKey(true).toString();
		}
		else if (name.equals("area")) {
            if (!content.isDummy()) {
                return content.getStructEntry().getArea().getName();
            }
            else {
                return "";
            }
		}
		else if (name.equals("level")) {
		    if (!content.isDummy()) {
		        WGContentNavigator nav = new WGContentNavigator(null, new WebTMLLanguageChooser(db(), this));
		        return nav.getContentLevel(content);
		    }
		    else {
		        return 0;
		    }
		}
		else if (name.equals("index")) {
		    if (!content.isDummy()) {
		        WGContentNavigator nav = new WGContentNavigator(null, new WebTMLLanguageChooser(db(), this));
		        return nav.getSiblingsIndex(content);
		    }
		    else {
		        return 0;
		    }
		}
        else if (name.equals("navindex")) {
            if (!content.isDummy()) {
                WGContentNavigator nav = new WGContentNavigator(WGContent.DISPLAYTYPE_NAVIGATOR, new WebTMLLanguageChooser(db(), this));
                return nav.getSiblingsIndex(content);
            }
            else {
                return 0;
            }
        }
		else if (name.equals("language")) {
			return content.getLanguage().getName();
		}
		else if (name.equals("languagetitle")) {
			return content.getLanguage().getTitle();
		}
		else if (name.equals("attachments")) {
			return content.getFileNames();
		}
		else if (name.equals("doctype") || name.equals("contenttype")) {
		    if (content.hasCompleteRelationships()) {
		        return content.getStructEntry().getContentType().getName();
		    }
		    else {
		        return null;
		    }
		}
        else if (name.equals("contenttypetitle")) {
            if (content.hasCompleteRelationships()) {
	            return content.getStructEntry().getContentType().getNameForLanguage(getpreferredlanguage());
	        }
            else {
                return "";
            }
        }
        else if (name.equals("contenttypedescription")) {
            if (content.hasCompleteRelationships()) {
                return content.getStructEntry().getContentType().getDescriptionForLanguage(getpreferredlanguage());
            }
            else {
                return "";
            }
        }
		else if (name.equals("siblings")) {
            if (!content.isDummy()) {
                WGContentNavigator nav = new WGContentNavigator(null, new WebTMLLanguageChooser(db(), this));
                return new Integer(nav.getSiblingsCount(content));
            }
		    else {
		        return 0;
		    }
		}
		else if (name.equals("workflow")) {
		    
		    if (content.getStatus().equals(WGContent.STATUS_DRAFT) || content.getStatus().equals(WGContent.STATUS_REVIEW)) {
		        WGWorkflow wf = content.getWorkflow();
		        if (wf instanceof WGNamedWorkflow) {
		            return ((WGNamedWorkflow) wf).getWorkflowName();
		        }
		    }
		    
			if (content.getStructEntry() != null) {
			    return content.getStructEntry().getWorkflowName();
			}
			
		    return null;
		}
		else if (name.equals("status")) {
			return content.getStatus();
		}
		else if (name.equals("structtitle")) {
		    if (content.getStructEntry() != null) {
		        return content.getStructEntry().getTitle();
		    }
		    else {
		       return "";
		    }
		}
		else if (name.equals("structkey") || name.equals("pagekey")) {
			return content.getContentKey().getStructKey();
		}
		else if (name.equals("itemnames")) {
			return content.getItemNames();
		}
		else if (name.equals("searchscore")) {
		    return new Float(content.getSearchScore());
		}
		else if (name.equals("searchexplanation")) {
			return content.getSearchExplanation();
		}
		else if (name.equals("searchdoctype")) {
		    SearchDetails sd = content.getSearchDetails();
		    if (sd instanceof LuceneSearchDetails) {
		        return ((LuceneSearchDetails) sd).getDoctype();
		    }
		    else {
		        return null;
		    }
		}
		else if (name.equals("searchfilename")) {
            SearchDetails sd = content.getSearchDetails();
            if (sd instanceof LuceneSearchDetails) {
                return ((LuceneSearchDetails) sd).getFilename();
            }
            else {
                return null;
            }
        }
        else if (name.equals("positionpath")) {
            if (content.getStructEntry() != null) {
                return content.getStructEntry().getPositionPath();
            }
            else {
                return "";
            }
        }
        else if (name.equals("pagepublished")) {
            if (content.getStructEntry() != null) {
                return content.getStructEntry().getPublished().get(content.getLanguage().getName());
            }
            else {
                return null;
            }
        }
        else if (name.equals("email")) {
            return content.getAuthorEMail();
        }
		else {
			Object result = null;
			String upperName = name.toUpperCase();
			boolean isNoValidMeta = true;

			if (content.getMetaInfo(upperName) != null) {
				result = content.getMetaData(upperName);
				isNoValidMeta = false;
			}

			else {
				WGStructEntry structEntry = content.getStructEntry();
				if (structEntry != null) {
					if (structEntry.getMetaInfo(upperName) != null) {
						result = structEntry.getMetaData(upperName);
						isNoValidMeta = false;
					}
					else {
						WGContentType docType = structEntry.getContentType();
						if (docType != null) {
							if (docType.getMetaInfo(upperName) != null) {
								result = docType.getMetaData(upperName);
								isNoValidMeta = false;
							}
						}
					}
				}
			}
			if (isNoValidMeta) {
				return "";
			}
			return result;
		}

	}

	private Object getDatabaseMetaData(String name, String originalName) throws WGAPIException {

		if (name.equals("title")) {
			return this.getdocument().getDatabase().getTitle();
		}
		else if (name.equals("path")) {
			return this.getdocument().getDatabase().getPath();
		}
		else if (name.equals("type")) {
			return this.getdocument().getDatabase().getType();
		}
		else if (name.equals("dbkey")) {
			return this.getdocument().getDatabase().getAttribute(WGACore.DBATTRIB_DBKEY);
		}
		else if (name.startsWith("user")) {
		    return getDatabaseUserMetaData(db().getSessionContext().getUserAccess(), name, originalName);
		}
		else if (name.startsWith("original_user")) {
		    WGUserAccess userAccess = getoriginaluserdata();
		    if (userAccess != null) {
		        return getDatabaseUserMetaData(userAccess, name.substring(9), originalName.substring(9));
		    }
		    else {
		        return null;
		    }
		}
        else if (name.equals("domain")) {
			return (String) this.getdocument().getDatabase().getAttribute(WGACore.DBATTRIB_DOMAIN);
		}
		else if (name.equals("filecontainers")) {
			return getDesignDocNames(this.getdocument().getDatabase().getFileContainers());
		}
		else if (name.equals("contenttypes")) {
			return getDesignDocNames(this.getdocument().getDatabase().getContentTypes());
		}		
		else if (name.equals("cssjsmodules")) {
			return getDesignDocNames(this.getdocument().getDatabase().getCSSJSModules());
		}
		else if (name.equals("tmlmodules")) {
			return getDesignDocNames(this.getdocument().getDatabase().getTMLModules());
		}	
		else if (name.equals("areas")) {
			return getDesignDocNames(this.getdocument().getDatabase().getAreas().values());
		}
		else if (name.equals("languages")) {
			return getDesignDocNames(this.getdocument().getDatabase().getLanguages().values());
		}		
		else if (name.equals("defaultlanguage")) {
		    return getdocument().getDatabase().getDefaultLanguage();
		}
		else if (name.equals("servername")) {
		    return getdocument().getDatabase().getServerName();   
		}
		else if (name.equals("lucene")) {
		    return getwgacore().getLuceneManager().indexIsEnabled(db().getDbReference());
		}
		else {
			this.addwarning("Unknown metadata name: " + name, true);
			return "";
		}

	}

	private Object getDatabaseUserMetaData(WGUserAccess userAccess, String name, String originalName) throws WGAPIException {
	    
	    // Look if we have detailed user information
	    WGUserDetails userDetails = null;
	    if (userAccess instanceof WGUserDetails) {
	        userDetails  = (WGUserDetails) userAccess;
	    }
	    
	    if (name.equals("username")) {
            return userAccess.getPrimaryName();
        }
	    else if (name.equals("useraccess")) {
            return new Integer(userAccess.getAccessLevel());
        }
	    else if (name.equals("userlabels")) {
	        if (userDetails != null) {
	            return new ArrayList(userDetails.getLabeledNames().keySet());
	        }
	        else {
	            return Collections.emptyList();
	        }
	    }
	    
        else if (name.startsWith("userlabel_") || name.startsWith("useralias_")) {
            if (userDetails != null) {
                int sublinePos = name.indexOf("_");
                String label = originalName.substring(sublinePos + 1);
                return userDetails.getLabeledNames().get(label);
            }
            else {
                return null;
            }
        }
        else if (name.equals("useraliases")) {
            if (userDetails != null) {
                return userDetails.getAliases();
            }
            else {
                return null;
            }
        }
        else if (name.equals("userroles")) {
            if (userDetails != null) {
                return userDetails.getRoles();
            }
            else {
                return null;
            }
        }
        else if (name.equals("usergroups")) {
            if (userDetails != null) {
                return userDetails.getGroups();
            }
            else {
                return null;
            }
        }
        else if (name.equals("useremail")) {
            if (userDetails != null) {
                return userDetails.getEMailAddress();
            }
            else {
                return null;
            }
        }
        else if (name.equals("usermaymovestructs")) {
            return new Boolean(userAccess.mayMoveStructEntries());
        }
        else if (name.equals("usermaydeletedocs")) {
            return new Boolean(userAccess.mayDeleteDocuments());
        }
        else {
    	    this.addwarning("Unknown metadata name: " + name, true);
            return "";
        }
	}

    private Object getDesignDocNames(Collection containers) throws WGAPIException {
		Iterator designDocs = containers.iterator();
		WGDesignDocument designDoc; 
		List designDocNames = new ArrayList();
		while (designDocs.hasNext()) { 
			designDoc = (WGDesignDocument) designDocs.next();
			designDocNames.add(designDoc.getName());
		}
		return designDocNames;
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#taginfo(java.lang.String)
     */
	@Override
    public TagInfo tag(String tagId) throws WGAPIException {
	    BaseTagStatus tag = _designContext.getTag();
	    
		if (tag == null)
			getlog().error("taginfo: no TML environment");

		if (tag!=null && tagId != null) {
			tag = tag.getTagStatusById(tagId);
			if (tag == null) {
				String msg = "taginfo: Could not find tag with id = " + tagId;
				getlog().error(msg);
				this.setLastError(msg);
			}
		}
		return  new TagInfo(tag);
	}
	
	public TagInfo tag() throws WGAPIException {
		return tag(null);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#taginfo(java.lang.String, java.lang.String)
     */
	@Override
	@Deprecated
    public Object taginfo(String tagId, String name) throws WGAPIException {
		
	    BaseTagStatus tag = _designContext.getTag();
		if (tag == null) {
			return null;
		}

		if (tagId == null) {
			this.setLastError("No WebTML tag environment available");
			return null;
		}

		BaseTagStatus refTag = tag.getTagStatusById(tagId);
		if (refTag != null) {
			Object result = refTag.getTagInfo(String.valueOf(name).toLowerCase());
			if (result == null) {
				this.setLastError("Tag " + tagId + " did not provide Info " + name);
			}
			return result;
		}
		else {
		    addwarning("Could not find tag for tagid " + tagId, false);
			this.setLastError("Could not find tag for tagid " + tagId);
			return null;
		}
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getlasterror()
     */
	@Override
    public String getlasterror() {
		return lastError;
	}

	public void setLastError(String lastError) {
		this.lastError = lastError;
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isselected()
     */
	@Override
    public boolean isselected() throws WGAPIException {
		
		TMLContext mainContext = getmaincontext();
		
		return isselected(mainContext);
	}

	@Override
    public boolean isselected(Context mainContext) throws WGAPIException {
        boolean isSelected = false;
		WGContent pageContent = mainContext.content();
		if (pageContent.getDatabase() != this.document.getDatabase()) {
			return false;
		}

		WGContentNavigator navigator = new WGContentNavigator(this.role, new WebTMLLanguageChooser(db(), this));
		navigator.setOnlyPublished(!isbrowserinterface());
		while (pageContent != null) {
			if (pageContent.getStructKey().equals(this.content().getStructKey())) {
				isSelected = true;
				break;
			}

			if (pageContent.hasCompleteRelationships() && !pageContent.getStructEntry().isRoot()) {
				pageContent = navigator.getParentContent(pageContent);
			}
			else {
				pageContent = null;
			}
		}
		return isSelected;
    }

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#haschildren()
     */
	@Override
    public boolean haschildren() throws WGAPIException {
		WGContentNavigator navigator = new WGContentNavigator(this.role, new WebTMLLanguageChooser(db(), this));
        return navigator.hasContentChildren(getcontent());
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#hassiblings()
     */
	@Override
    public boolean hassiblings() throws WGAPIException {
		WGContentNavigator navigator = new WGContentNavigator(this.role, new WebTMLLanguageChooser(db(), this));
        return navigator.getSiblingsCount(getcontent())!=0;
	}


	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isroot()
     */
	@Override
    public boolean isroot() throws WGAPIException {
		return getcontent().isDummy() || getcontent().getStructEntry().isRoot();
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#ismaindocument()
     */
	@Override
    public boolean ismaindocument() throws WGAPIException {
		return (document == getmaincontext().content());
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isbrowserinterface()
     */
	@Override
    public boolean isbrowserinterface() {
	    WGA wga = WGA.get(this);
	    if (wga.call().isAvailable()) {
	        try {
	        	if(wga.call().getParam(WGACore.URL_PARAM_CLEAN)!=null)
	        		return false;
                return WGPDispatcher.isBrowserInterface(wga.call().getJavaRequest().getSession());
            }
            catch (WGException e) {
                getlog().error("Exception determining browser interface", e);
                return false;
            }
	    }
	    else {
	        return false;
	    }
	    
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#contenturl(java.lang.String, java.lang.String)
     */
	@Override
    public String contenturl(String mediaKey, String layoutKey) throws WGException {
		return contenturl(mediaKey, layoutKey, false);
	}
     
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#contenturl()
     */
	@Override
    public String contenturl() throws WGException {
	    return contenturl(null, null, false);
	}
     
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#contenturl(java.lang.String, java.lang.String, boolean)
     */
	@Override
    public String contenturl(String mediaKey, String layoutKey, boolean ignoreVirtualLink) throws WGException {
	    return getURLBuilder().buildContentURL(toUnlockedVersion(), mediaKey, layoutKey, ignoreVirtualLink);
	}
    
    public String contentdataurl(String mediaKey, String layoutKey) throws ServletException, IOException, WGException {        
        return contentdataurl(mediaKey, layoutKey, false, null);
    }
    
    public String contentdataurl(String mediaKey, String layoutKey, String queryString) throws ServletException, IOException, WGException {        
        return contentdataurl(mediaKey, layoutKey, false, queryString);
    }

    
    public String contentdataurl(String mediaKey, String layoutKey, boolean ignoreVirtualLink, String queryString) throws ServletException, IOException, WGException {
        
        BaseTagStatus tag = _designContext.getTag();
        if (tag ==  null) {
            return null;
        }
        
        TMLContext unlockedCx = toUnlockedVersion();
        
        
        // retrieve standard contenturl
        String url = contenturl(mediaKey, layoutKey, ignoreVirtualLink);
        // cut off WGPath
        url = url.substring(url.indexOf(_environment.getPublisherURL()) + _environment.getPublisherURL().length());
        // append query string
        if (queryString != null) {
            url = url + "?" + queryString;
        }
        
        MediaKey key = getwgacore().getMediaKey(mediaKey);        
        String contentType = key.getMimeType();
        
        /** this works on WAS - on tomcat the result is an endless loop         
        // create wrapper for request and response
        RenderServletRequestWrapper req = new RenderServletRequestWrapper(this.getrequest(), url);        
        RenderServletResponseWrapper res = new RenderServletResponseWrapper(this.getresponse());
        **/
        RenderServletRequest req = new RenderServletRequest(unlockedCx.getrequest(), url);        
        RenderServletResponse res = new RenderServletResponse(unlockedCx.getresponse());
        
        
        // call include
        this.getrequest().getRequestDispatcher(url).include(req, res);

        // process response
        if (res.isBinary()) {
            byte data[] = res.getBinaryData();
            return createDataURL(data, contentType);
        } else {
            String data = res.getStringData();
            return createDataURL(data, contentType);
        }
    }
    
    
	
	public int getDummy() {
	    return 1;
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#layouturl(java.lang.String, java.lang.String, java.lang.String)
     */
    @CodeCompletion
	public String layouturl(String dbKey, String mediaKey, String layoutKey) throws WGException {
	    return getURLBuilder().buildLayoutURL(toUnlockedVersion(), resolveDBKey(dbKey), mediaKey, layoutKey);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#layouturl(java.lang.String, java.lang.String)
     */
    @CodeCompletion
	public String layouturl(String mediaKey, String layoutKey) throws WGException {
	    return layouturl(null, mediaKey, layoutKey);
	}

	public void setrole(String role) {
		this.role = role;
	}

	public String getrole() {
		return this.role;
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#addwarning(java.lang.String, boolean)
     */
	@Override
    public void addwarning(String msg, boolean severe) {
	    _designContext.addWarning(this, msg, severe, null);
	}
	
   @Override
    public void addwarning(String msg, boolean severe, Throwable cause) {
        _designContext.addWarning(this, msg, severe, cause);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#addwarning(java.lang.String)
     */
    @Override
    public void addwarning(String msg) {
        addwarning(msg, false);
    }
    


	private Object getSessionMetaData(String name) {
		
		if (!getEnvironment().isPageContextAvailable()) {
			this.setLastError("Cannot retrieve session metadata because this script does not run inside a WebTML page");
			return null;
		}

		HttpSession session = gethttpsession();
		if (name.equals("start")) {
			return new Date(session.getCreationTime());
		}
		else if (name.equals("lastaccess")) {
			return new Date(session.getLastAccessedTime());
		}
		else if (name.equals("id")) {
			return session.getId();
		}
		else if (name.equals("language")) {
			return getpreferredlanguage();
		}
		else {
			return null;
		}
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isnewsession()
     */
	@Override
    public boolean isnewsession() {
		
		if (!getEnvironment().isPageContextAvailable()) {
			return false;
		}
		
		return gethttpsession().isNew();
	}

	public javax.servlet.http.Cookie[] getcookies() {
		
		if (!_environment.isPageContextAvailable()) {
			return null;
		}
		
		return getrequest().getCookies();
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getrequest()
     */
	@Override
    @CodeCompletion
	public javax.servlet.http.HttpServletRequest getrequest() {
		return _environment.getRequest();
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#istrue(java.lang.String)
     */
	@Override
    public boolean istrue(String varname) throws WGAPIException {

		Object value = this.item(varname);

		// Enhanced "true", using JavaScript rules to determine true/false.
		if (isEnhancedItemExpressions()) {
		    
		    if (value == null) {
		        return false;
		    }
		    
		    if (value instanceof Boolean) {
		        return ((Boolean) value).booleanValue();
		    }
		    
		    if (value instanceof Number && (((Number) value).intValue() == 0 || ((Number) value).doubleValue() == Double.NaN)) {
		        return false;
		    }
		    
		    if (value.equals("")) {
		        return false;
		    }
		    
		    int scriptType = ExpressionEngineFactory.getTMLScriptEngine().determineTMLScriptType(value);
		    if (scriptType == RhinoExpressionEngine.TYPE_UNDEFINED || scriptType == RhinoExpressionEngine.TYPE_NAN) {
		        return false;
		    }
		    
		    return true;
		    
		    
		}
		
		// Legacy "true", using boolean values, numbers 1 or -1 and string literals "true"/"false"
		else {
		
    		if (value instanceof Boolean) {
    			return ((Boolean) value).booleanValue();
    		}
    		else if (value instanceof Number) {
    			int valueNumber = ((Number) value).intValue();
    			if (valueNumber == 1 || valueNumber == -1) {
    				return true;
    			}
    			else {
    				return false;
    			}
    		}
    		else if (value instanceof String) {
    			return Boolean.valueOf((String) value).booleanValue();
    		}
    		else {
    			return false;
    		}
    		
		}

	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isdefined(java.lang.String)
     */
	@Override
    public boolean isdefined(String name) throws WGAPIException {
		
		name = convertVarName(name);
        TMLPortlet portlet = getportlet();
        
        Map<String,Object> vars = _environment.getPageVars();
        
        // Portlet variables
        if (portlet != null && !portlet.isroot()) {
            
            String pName = portlet.getVarPrefix() + name;
            
            // Request variables
            if (vars.containsKey(pName)) {
                return true;
            }

            // Session variables
            if (portlet.hassessionvar(name)) {
                return true;
            }
            
        }

        // Local variables (since 7.2)
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
            Object value = getDesignContext().retrieveLocalVar(name);
            if (!(value instanceof NullPlaceHolder)) {
                return true;
            }
        }
        
        // Request variables
		if (vars.containsKey(name)) {
			return true;
		}

		// Session variables
		Map<String,TransientObjectWrapper<Object>> sessionVars = _environment.getSessionVars();
		if (sessionVars.containsKey(name) && sessionVars.get(name).get() != null) {
			return true;
		}

		// Mapped item values
		Object result = this.getItemMappingExpression(name);
		if (result != null) {
			return true;
		}

		// Content items
		return this.document.hasItem(name);

	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isfalse(java.lang.String)
     */
    public boolean isfalse(String varname) throws WGAPIException {
		return !this.istrue(varname);
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#option(java.lang.String, java.lang.Object)
     */
	@Override
    public Object option(String optionName, Object defaultValue) {

	    try {
            TMLOption option = _designContext.getOption(optionName);
            if (option == null || option.getValue() == null) {
                return defaultValue;
            }
            else {
                return option.getValue();
            }
        }
        catch (WGException e) {
            addwarning("Error retrieving WebTML option '" + optionName + "': " + e.getMessage());
            getlog().error("Error retrieving WebTML option '" + optionName + "'", e);
            return null;
        }

       
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#option(java.lang.String)
     */
	@Override
    public Object option(String option) {
	    return option(option, null);
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#hasoption(java.lang.String)
     */
	@Override
    public boolean hasoption(String option){
		return (this.option(option) != null);
	}
	
	public Date dateonly(Date date) {
		return WGUtils.dateOnly(date);
	}
	
	public Date timeonly(Date date) {
		return WGUtils.timeOnly(date);
	}

	@CodeCompletion
	public WGDocument getapiobject() {
		return this.document;
	}

	public Date stringtodate(String text) {

		if (text == null) {
			return null;
		}

		if (text.indexOf(":") == -1) {
			text += " 0:00";
		}

		SimpleDateFormat dateFormat = new SimpleDateFormat();
		dateFormat.setLenient(true);
		return dateFormat.parse(text, new ParsePosition(0));

	}

	@CodeCompletion
	public List createlist() {
	    return WGA.get(this).createList();

	}
	
	@CodeCompletion
	public List createlist(Object[] objects) {
        if (objects != null) {
            return WGA.get(this).createList(objects);
        }
        else {
            return createlist();
        }
	}
	
	@CodeCompletion
	public List createlist(String listString, String delimiter) {
	    return WGA.get(this).createList(listString, delimiter);
	}

	public int createuserprofile(String name, String password)  throws WGAPIException {
		return this.createuserprofile((String) this.content().getDatabase().getAttribute(WGACore.DBATTRIB_DOMAIN), name, password);
	}

	public int createuserprofile(String domain, String name, String password)  throws WGAPIException{
		return createuserprofile(domain, name, password, true);
	}

	public int createuserprofile(String domain, String name, String password, boolean forCurrentUser) throws WGAPIException {
	    
        if (getEnvironment().getRequest() == null) {
            return TMLUserProfile.RC_METHOD_UNAVAILABLE;
        }
	    
		// Get pers db. Ensure that there is one
		if (domain == null || domain.equals("")) {
			return TMLUserProfile.RC_NO_DOMAIN;
		}
		HttpSession session = getEnvironment().getRequest().getSession();
		if (session == null) {
			return TMLUserProfile.RC_NO_SESSION;
		}

		WGDatabase persDB = getwgacore().openPersonalisationDB(domain);
		if (persDB == null) {
			return TMLUserProfile.RC_NOT_PERSONALIZED;
		}

		//  ensure persdb is in correct mode (custom)
		Integer persMode = Integer.valueOf((String) getwgacore().readPublisherOptionOrDefault(getDesignContext().getDesignDB(), WGACore.DBATTRIB_PERSMODE));
		if (persMode.intValue() != Constants.PERSMODE_CUSTOM) {
			return TMLUserProfile.RC_WRONG_PERSMODE;
		}

		// Ensure, there is no profile yet with this name
		WGUserProfile profile = persDB.getUserProfile(name);
		if (profile != null) {
			return TMLUserProfile.RC_PROFILE_EXISTS;
		}

		// Try to create the profile
		try {
			profile = persDB.createUserProfile(name, Constants.PERSMODE_CUSTOM);
		}
		catch (WGException e) {
			this.addwarning("Exception creating user profile: " + e.getMessage(), true);
		}
		if (profile == null) {
			return TMLUserProfile.RC_NOT_CREATABLE;
		}
		TMLUserProfile.prepareNewProfile(profile, getrequest());

		// Set password eventually
		if (password != null) {
			profile.setMetaData(WGUserProfile.META_PASSWORD, password);
		}

		try {
			profile.save();
		}
		catch (WGAPIException e) {
			this.addwarning("Unable to create user profile: " + e.getMessage(), true);
            return TMLUserProfile.RC_NOT_CREATABLE;
		}

		// Attach to session
		if (forCurrentUser) {
			attachProfileToUser(domain, profile);
		}

		return TMLUserProfile.RC_OK;
	}

	public int assignuserprofile(String name, String password)  throws WGAPIException {
		return this.assignuserprofile((String) this.content().getDatabase().getAttribute(WGACore.DBATTRIB_DOMAIN), name, password);
	}

	public int assignuserprofile(String domain, String name, String password)  throws WGAPIException{

        if (getEnvironment().getRequest() == null) {
            return TMLUserProfile.RC_METHOD_UNAVAILABLE;
        }
	    
		password = (password == null ? "" : password);

		// Get pers db. Ensure that there is one
		if (domain == null || domain.equals("")) {
			return TMLUserProfile.RC_NO_DOMAIN;
		}
		HttpSession session = getEnvironment().getRequest().getSession();
		if (session == null) {
			return TMLUserProfile.RC_NO_SESSION;
		}
 
		WGDatabase persDB = getwgacore().openPersonalisationDB(domain);
		if (persDB == null) {
			return TMLUserProfile.RC_NOT_PERSONALIZED;
		}

		//  ensure database is in correct mode (custom)
		Integer persMode = Integer.valueOf((String) getwgacore().readPublisherOptionOrDefault(getDesignContext().getDesignDB(), WGACore.DBATTRIB_PERSMODE));
		if (persMode.intValue() != Constants.PERSMODE_CUSTOM) {
			return TMLUserProfile.RC_WRONG_PERSMODE;
		}

		// Try to fetch profile
		WGUserProfile profile = persDB.getUserProfile(name);
		if (profile == null) {
			return TMLUserProfile.RC_NO_PROFILE;
		}

		// Test password
		if (!password.equals(profile.getPassword())) {
			return TMLUserProfile.RC_WRONG_PASSWORD;
		}

		// Attach to session & request and register hit
		attachProfileToUser(domain, profile);
		if (!isbrowserinterface()) {
			getwgacore().getPersManager().registerHit(profile, this.content().getDatabase(), this.content());
		}

		return TMLUserProfile.RC_OK;

	}

	private void attachProfileToUser(String domain, WGUserProfile profile) throws WGAPIException {

		if (!getEnvironment().isPageContextAvailable()) {
			addwarning("Cannot use this method in this TMLScript runtime", false);
			return;
		}

		if (profile != null) {
			gethttpsession().setAttribute(PersonalisationManager.SESSION_PROFILENAME + domain, profile.getName());
		}
		else {
			gethttpsession().removeAttribute(PersonalisationManager.SESSION_PROFILENAME + domain);
		}
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#gettmlform()
     */
	@Override
    public TMLForm gettmlform() {
		
	    TMLForm form = null;
	    
	    // Use form predefined by environment
		if (_environment != null && _environment.getForm() != null) {
			form = _environment.getForm();
		}

		// Use last defined form on the WebTML request
		if (form == null && getrequest() != null) {
		    form = (TMLForm) getrequest().getAttribute(WGACore.ATTRIB_LASTFORM);
        }
        
		return form;

	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#tmlformbyid(java.lang.String)
     */
	@Override
    public TMLForm tmlformbyid(String id) {

		TMLForm form = (TMLForm) getTransientForms().get(id);
		if (form != null) {
			return form;
		}

		form = (TMLForm) getPersistentForms().get(id);
		if (form != null) {
			return form;
		}

		return null;
	}

	private Map getTransientForms() {
        return _environment.getTransientForms();
    }

	@CodeCompletion
    public WGDatabase db(String key) throws WGException {
	    return WGA.get(this).db(key);
	}
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#db()
     */
    @Override
    public WGDatabase db() {
        return document.getDatabase();
    }
    
    @CodeCompletion
    public WGDatabase designdb() throws WGException {
        WGDatabase designDB = _designContext.getDesignDB();
        return _environment.openDB(designDB);
    }

    @CodeCompletion
    public WGHierarchicalDatabase hdb(String key) throws WGException {
        return WGA.get(this).hdb(key);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#hdb()
     */
    @Override
    public WGHierarchicalDatabase hdb() throws WGException {
    	return hdb(db().getDbReference());
    }
    
	public int closeuserprofile(String domain) throws WGAPIException {

		HttpSession session = gethttpsession();
		if (session == null) {
			return TMLUserProfile.RC_NO_SESSION;
		}

		TMLUserProfile profile = this.getprofile();
		if (profile == null) {
			return TMLUserProfile.RC_NO_PROFILE;
		}

		Integer mode = Integer.valueOf((String) getwgacore().readPublisherOptionOrDefault(profile.getprofile().getDatabase(), WGACore.DBATTRIB_PERSMODE));
		if (mode.intValue() != Constants.PERSMODE_CUSTOM) {
			return TMLUserProfile.RC_WRONG_PERSMODE;
		}

		attachProfileToUser(domain, null);
		return TMLUserProfile.RC_OK;

	}

	public int closeuserprofile() throws WGAPIException {
		return this.closeuserprofile((String) this.content().getDatabase().getAttribute(WGACore.DBATTRIB_DOMAIN));
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#hasprofile()
     */
	@Override
    public boolean hasprofile() {
		return (this.getprofile() != null);
	}

	
	// For binary backward compatibility to WGA 6.0 (#00002388)
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#context(de.innovationgate.webgate.api.WGContent)
     */
	@Override
    @CodeCompletion
	public TMLContext context(WGContent content) throws WGAPIException {
	    return getTMLContextForDocument(content);
	}
	
	public TMLContext context(WGDocument doc) throws WGAPIException {
		return getTMLContextForDocument(doc);
	}

	@CodeCompletion
	public SmtpMail createmail(String smtpHost, String username, String password) throws UnsupportedEncodingException {
		try {
            return WGA.get(this).createMail(smtpHost, username, password);
        }
        catch (WGException e) {
            throw new RuntimeException(e);
        }
	}
	
	@CodeCompletion
	public SmtpMail createmail() throws TMLException {
		try {
            return WGA.get(this).createMail();
        }
        catch (UnsupportedEncodingException e) {
            throw new TMLException("Unsupported encoding: " + e.getMessage());
        }
		catch (WGException e) {
		    throw new TMLException(e.getMessage());
		}
	}

	@CodeCompletion
    public ImageScaler createimagescaler(File imageFile) throws WGException, IOException {
	    return WGA.get(this).createImageScaler(imageFile);
	}
    
	@CodeCompletion
    public ImageScaler createimagescaler(InputStream is) throws WGException, IOException {
	    return WGA.get(this).createImageScaler(is);
    }

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getpath()
     */
	@Override
    public String getpath() throws WGAPIException {

		StringBuffer path = new StringBuffer();
		WGDocument doc = getdocument();
        
		path.append("db:" + doc.getDatabase().getAttribute(WGACore.DBATTRIB_DBKEY));

		switch (doc.getType()) {

			case WGDocument.TYPE_CONTENT :
				if (!getcontent().isDummy()) {
					path.append("/docid:" + getcontent().getContentKey().toString());
				}
				else {
				    path.append("<").append(getcontent().getLanguage().getName()).append(">");
				}
				break;

			case WGDocument.TYPE_STRUCTENTRY :
				path.append("/$struct:" + ((WGStructEntry) doc).getStructKey());
				break;

			case WGDocument.TYPE_AREA :
				path.append("/$area:" + ((WGArea) doc).getName());
				break;

			case WGDocument.TYPE_CONTENTTYPE :
				path.append("/$contenttype:" + ((WGContentType) doc).getName());
				break;

			case WGDocument.TYPE_LANGUAGE :
				path.append("/$language:" + ((WGLanguage) doc).getName());
				break;

			case WGDocument.TYPE_TML :
				path.append("/$tml:" + ((WGTMLModule) doc).getName() + "," + ((WGTMLModule) doc).getMediaKey());
				break;

			case WGDocument.TYPE_CSSJS :
				path.append("/$cssjs:" + ((WGCSSJSModule) doc).getName());
				break;

			case WGDocument.TYPE_FILECONTAINER :
				path.append("/$filecontainer:" + ((WGFileContainer) doc).getName());

		}

		return path.toString();

	}

	public void registerForm(TMLForm form, boolean persist) {
	    _environment.setForm(form);
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getresponse()
     */
	@Override
    @CodeCompletion
	public HttpServletResponse getresponse() {
	    return (HttpServletResponse) _environment.getResponse();
	}

	@CodeCompletion
	public void redirectto(String url) throws IOException {
		getrequest().setAttribute(WGACore.ATTRIB_REDIRECT, getresponse().encodeURL(url));
	}
	
	public TMLContext getTMLContextForDocument(WGDocument doc) {
	    return _environment.getTMLContextForDocument(this, doc);
	}

	public static String createContextKey(WGDocument doc) {
		return doc.getDatabase().getDbReference() + "//" + doc.getDocumentKey().toString();
	}
	
	public Cookie createcookie(String name, String value) {
		return new Cookie(name, value);
	}	public Cookie createCookie(String name, String value) {
		return new Cookie(name, value);
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#getlog()
     */
	@Override
    public Logger getlog() {
		return getEnvironment().getLog();
	}
	
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#removevar(java.lang.String)
     */
	@Override
    public void removevar(String name) throws WGAPIException {
		String lcName = convertVarName(name);
		// Local variables (since 7.2)
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2)) {
            Object value = getDesignContext().retrieveLocalVar(lcName);
            if (!(value instanceof NullPlaceHolder)) {
                getDesignContext().removeLocalVar(lcName);
                return;
            }
        }
	    
		_environment.getPageVars().remove(lcName);
		
		if (this.document.getDatabase().hasFeature(WGDatabase.FEATURE_STORESVARS)) {
			this.document.removeItem(name);
		}
	}

	@CodeCompletion
	public WGACore getwgacore() {
	    return _environment.getCore();
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#gethttpsession()
     */
	@Override
    public HttpSession gethttpsession() {
		return _environment.getSession();
	}
	
	/**
	 * @deprecated
	 */
	public void setpreferredlanguage(String lang) {
	    addwarning("Method TMLContext.setPreferredLanguage() is deprecated and inoperable since OpenWGA 5.1");
	}
	
	/**
	 * Returns the preferred language for the given content context
	 * This equals calling {@link #getpreferredlanguage(WGDatabase, boolean)} with the current context database and "true" for the exhaustive parameter
	 * @throws WGAPIException 
	 * @deprecated
	 */
	public String getpreferredlanguage() {
	    return getpreferredlanguage(db(), true);
	}
	
	/**
	 * Determines the preferred language for the given database
	 * @param The database for whom the preferred language is determined. It is used to evaluate the available languages and to read it's language behaviour setting
	 * @param exhaustive Set to true if the method should try to find a appropriate language even if the pref language was not yet set for the user session
	 * @return The preferred language or null if none could be determined
	 * @deprecated
	 */
	protected String getpreferredlanguage(WGDatabase db, boolean exhaustive) {
        WebTMLLanguageChooser chooser = new WebTMLLanguageChooser(db, this);
        try {
            String lang = chooser.getPreferredLanguage(db);
            if (lang != null) {
                return lang;
            }
            else {
                return getmaincontext().content().getLanguage().getName();
            }
        }
        catch (WGAPIException e) {
            getlog().error("Exception retrieving preferred language for database " + db.getDbReference(), e);
            return null;
           
        }
	}
    
    public Locale getPreferredLanguageLocale() {
        
        String prefLang = getpreferredlanguage();
        if (prefLang == null) {
            return null;
        }
        
        return getwgacore().languageCodeToLocale(prefLang);
        
    }
    
    @CodeCompletion
    public Date parsedate(String date, String format) throws ParseException, WGException {
        return WGA.get(this).getDateFormat(format, null).parse(date);
    }
    
    @CodeCompletion
    public Number parsenumber(String number, String format) throws ParseException, WGException {
    	return WGA.get(this).getNumberFormat(format,null).parse(number);
    }
    
    @CodeCompletion
    public Date createdate() {
        return createdate(true);
    }
    
    @CodeCompletion
    public Date now() {
    	return createdate();
    }
    
    @CodeCompletion
    public Date createdate(boolean includeMillis) {
        try {
            return WGA.get(this).createDate(includeMillis);
        }
        catch (WGException e) {
            throw new RuntimeException(e);
        }
    }
    
    public Calendar createcalendar(Date date) {
        try {
            return WGA.get(this).createCalendar(date);
        }
        catch (WGException e) {
            throw new RuntimeException(e);
        }
    }
    
    public Calendar createcalendar() {
        try {
            return WGA.get(this).createCalendar(null);
        }
        catch (WGException e) {
            throw new RuntimeException(e);
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setoption(java.lang.String, java.lang.Object, java.lang.String)
     */
    @Override
    public void setoption(String name, Object value, String scope) {
        try {
            _designContext.setOption(name, value, scope);
        }
        catch (WGException e) {
            addwarning("Error setting WebTML option '" + name + "': " + e.getMessage());
            getlog().error("Error setting WebTML option '" + name + "'", e);
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#setoption(java.lang.String, java.lang.Object)
     */
    @Override
    public void setoption(String name, Object value) {
        setoption(name, value, TMLOption.SCOPE_GLOBAL);
    }
    
    public Login createlogin(String username, String password) {
        return new Login(username, password);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String, java.lang.String, java.lang.String, java.util.List, boolean)
     */
    @Override
    public String label(String containerName, String fileName, String key, List params, boolean usePlaceholder) {
        try {
            return WGA.get(this).design().label(containerName, fileName, key, params, usePlaceholder);
        }
        catch (WGException e) {
           getlog().error("Unable to open design db", e);
           return "(Error retrieving label)";
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String, java.lang.String, java.lang.String, java.util.List)
     */
    @Override
    public String label(String containerName, String fileName, String key, List params) {
        return label(containerName, fileName, key, params, true);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(de.innovationgate.webgate.api.WGDatabase, java.lang.String, java.lang.String, java.lang.String, java.util.List, boolean)
     */
    @Override
    public String label(WGDatabase designDB, String containerName, String fileName, String key, List<String> params, boolean usePlaceholder) throws WGException {
        return WGA.get(this).design(designDB).label(containerName, fileName, key, params, usePlaceholder);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(de.innovationgate.webgate.api.WGDatabase, java.lang.String, java.lang.String, java.lang.String, java.util.List)
     */
    @Override
    public String label(WGDatabase designDB, String containerName, String fileName, String key, List params) throws WGException {
        return label(designDB, containerName, fileName, key, params, true);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public String label(String containerName, String fileName, String key) {
        return label(containerName, fileName, key, null);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String, java.lang.String, java.util.List)
     */
    @Override
    public String label(String fileName, String key, List params) {
        return label(null, fileName, key, params);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String, java.lang.String)
     */
    @Override
    public String label(String fileName, String key) {
        return label(null, fileName, key, null);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String, java.util.List)
     */
    @Override
    public String label(String key, List params) {
        return label(null, null, key, params);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#label(java.lang.String)
     */
    @Override
    public String label(String key) {
        return label(null, null, key, null);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#haslabel(java.lang.String)
     */
    @Override
    public boolean haslabel(String key) {
        return (label(null, null, key, null, false) != null);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#haslabel(java.lang.String, java.lang.String)
     */
    @Override
    public boolean haslabel(String fileName, String key) {
        return (label(null, fileName, key, null, false) != null);
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#haslabel(java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public boolean haslabel(String containerName, String fileName, String key) {
        return (label(containerName, fileName, key, null, false) != null);
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#iswebenvironment()
     */
    @Override
    public boolean iswebenvironment() {
        return getEnvironment().isWebEnvironment();
    }
    
    /**
     * wrapper for TestCore.assertTrue(...)
     * @param title assertion-title
     * @param category assertion-category
     * @param condition tmlscript-condition to test for true
     */
    @CodeCompletion
    public void asserttrue(String title, String category, String condition) {
        this.getwgacore().getTestCore().assertTrue(title, category, condition, this);
    }
    
    /**
     * wrapper for TestCore.assertEquals(...)
     * @param title assertion-title
     * @param category assertion-category
     * @param obj1 Object1 for compare
     * @param obj2 Object2 for compare
     */
    @CodeCompletion
    public boolean assertequals(String title, String category, Object obj1, Object obj2) {
        return this.getwgacore().getTestCore().assertEquals(title, category, obj1, obj2, this);
    }
    
    @CodeCompletion
    public boolean assertlarger(String title, String category, Number obj1, Number obj2) {
        return this.getwgacore().getTestCore().assertLarger(title, category, (Comparable) obj1, (Comparable) obj2, this);
    }
    
    @CodeCompletion
    public boolean assertlarger(String title, String category, Date obj1, Date obj2) {
        return this.getwgacore().getTestCore().assertEquals(title, category, obj1, obj2, this);
    }
    
    /**
     * wrapper for TestCore.assertNotEquals(...)
     * @param title assertion-title
     * @param category assertion-category
     * @param obj1 Object1 for compare
     * @param obj2 Object2 for compare
     */
    @CodeCompletion
    public boolean assertnotequals(String title, String category, Object obj1, Object obj2) {
        return this.getwgacore().getTestCore().assertNotEquals(title, category, obj1, obj2, this);
    }

    /**
     * Compares two lists based on their contents but not on their order
     * @param title assertion-title
     * @param category assertion-category
     * @param l1 List 1 for compare
     * @param l2 List 2 for compare
     * @param ignoreOrder Set to true to ignore list order
     */
    @CodeCompletion
    public boolean assertequallists(String title, String category, List l1, List l2) {
        return assertequallists(title, category, l1, l2, true);
    }        
    
    /**
     * Compares two lists based on their contents
     * @param title assertion-title
     * @param category assertion-category
     * @param l1 List 1 for compare
     * @param l2 List 2 for compare
     * @param ignoreOrder Set to true to ignore list order
     */
    @CodeCompletion
    public boolean assertequallists(String title, String category, List l1, List l2, boolean ignoreOrder) {
        
        List sorted1 = (l1 != null ? new ArrayList(l1) : Collections.emptyList());
        List sorted2 = (l2 != null ? new ArrayList(l2) : Collections.emptyList());
        if (ignoreOrder) {
            Collections.sort(sorted1);
            Collections.sort(sorted2);
        }
        
        return this.getwgacore().getTestCore().assertEquals(title, category, sorted1, sorted2, this);
        
    }

    /**
     * wrapper for TestCore.assertTrue(...)
     * @param id of preregistered assertion
     * @param condition tmlscript-condition to test for true
     */
    @CodeCompletion
    public void asserttrue(String id, String condition) {
        this.getwgacore().getTestCore().assertTrue(id, condition, this);
    }
    
    /**
     * wrapper for TestCore.assertEquals(...)
     * @param id of preregistered assertion
     * @param obj1 Object1 for compare
     * @param obj2 Object2 for compare
     */
    @CodeCompletion
    public void assertequals(String id, Object obj1, Object obj2) {
        this.getwgacore().getTestCore().assertEquals(id, obj1, obj2, this);
    }    
    
    /**
     * wrapper for TestCore.registerAssertTrue()
     * @param id unqiue id for the assertion
     * @param title assertion-title
     * @param category assertion-category
     */
    public void registerasserttrue(String id, String title, String category) {
        this.getwgacore().getTestCore().registerAssertTrue(id, title, category);
    }
    

    
    /**
     * wrapper for TestCore.registerAssertEquals()
     * @param id unqiue id for the assertion
     * @param title assertion-title
     * @param category assertion-category
     */
    public void registerassertequals(String id, String title, String category) {
        this.getwgacore().getTestCore().registerAssertEquals(id, title, category);
    }

    /**
     * wrapper for TestCore.assertIsRegisteredOrExecuted()
     * @param id unique id for the assertion
     * @return true/false
     */
    @CodeCompletion
    public boolean assertisregisteredorexecuted(String id) {
        return this.getwgacore().getTestCore().assertIsRegisteredOrExecuted(id);
    }    
    
    /**
     * enable or disable TestCore.assertionDebugMode
     */
    @CodeCompletion
    public void assertiondebug(boolean enabled) {
        this.getwgacore().getTestCore().setDebugAssertions(enabled);
    }
    
    @CodeCompletion
    public boolean isassertiondebug() {
        return this.getwgacore().getTestCore().isDebugAssertions();
    }    
    
    
    @CodeCompletion
    public void changesessionpassword(String domain, String newPassword) {
        
        // Fetch the login info for the current user
        DBLoginInfo loginInfo = (DBLoginInfo) getwgacore().getSessionLogins(gethttpsession()).get(domain);
        if (loginInfo == null) {
            return;
        }
       
        // Change password on login info
        loginInfo.setCredentials(newPassword);
        
        // Change cached password on domain auth module, if it is password-caching
        if (!WGDatabase.SESSIONTOKEN_USER.equals(loginInfo.getUserName())) {
            WGADomain domConfig = getwgacore().getDomains(domain);
            if (domConfig != null && domConfig.getAuthModule() instanceof PasswordCachingAuthenticationModule) {
                PasswordCachingAuthenticationModule module = (PasswordCachingAuthenticationModule) domConfig.getAuthModule();
                module.dropPasswordCache(loginInfo.getUserName());        
    }
        }
        
        
    }
    
    @CodeCompletion
    public void changesessionpassword(String newPassword) throws WGAPIException {
        changesessionpassword((String) meta("db", "domain"), newPassword);
    }
    



    public TMLAction getActionByID(String id, String dbkey) {
        return getActionByID(id, dbkey, null);
    }
    
    /**
     * Retrieves an TMLAction definition from all valid sources
     * @param id The reference of the action
     * @param dbkey An explicitly chosen dbkey where to lookup the action. Give null if nothing was chosen.
     * @param baseRef The base design reference of the current design context, used to resolve the action.
     * @return
     */
    public TMLAction getActionByID(String id, String dbkey, DesignResourceReference baseRef) {

        // Default action
        if (id.startsWith("$")) {
            return new TMLAction(id.substring(1));
        }

        // If tag is present (WebTML-Environment) we can use action registration
        Map actionRegistration = getActionRegistration();
            
        // First try: Search for registered action qualified by design db
        // If there are multiple actions with the same id (from different databases), we need to find the one defined for the current design database first
        Integer actionKey;
        String currentDesignKey = dbkey;
        if (currentDesignKey == null && baseRef != null) {
            currentDesignKey = baseRef.getDesignApp();
        }
        if (currentDesignKey == null) {
            currentDesignKey = getDesignDBKey();
        }
        TMLAction action = fetchActionByQualifiedId(id, currentDesignKey, actionRegistration);
        if (action != null) {
            return action;
        }
        
        
        // Second try: Search for module action in the current design db
        try {
            
            // Interpret id as module id
            DesignResourceReference moduleId = resolveDesignResource(dbkey, id, baseRef);
            
            // If the resolved id differs we will again try to fetch it from registration 
            if (!moduleId.getResourceName().equals(id)) {
                action = fetchActionByQualifiedId(id, dbkey, actionRegistration);
                if (action != null) {
                    return null;
                }
            }
            
            action = getModuleActionByID(moduleId.getResourceName(), moduleId.getDesignApp());
            if (action != null) {
                return action;
            }
        }
        catch (Exception e) {
            getlog().error("Exception retrieving WebTML action " + currentDesignKey + "/" + id, e);
            addwarning(e.getMessage());
        }
        
        
        // Third try: search for registered action, not qualified by dbkey. We can find actions registered by other DBs that way.
        actionKey = (Integer) actionRegistration.get("ID#" + id.toLowerCase());
        if (actionKey != null) {
            action = (TMLAction) actionRegistration.get(actionKey);
            if (action != null && action.upToDate(this)) {
                return action;
            }
        }

        // No action found
        return null;
    }

    private TMLAction fetchActionByQualifiedId(String id, String dbkey, Map actionRegistration) {
        String qualifiedID = id;
        if (qualifiedID.indexOf("/") == -1) {
            qualifiedID = "ID#" + (dbkey + ACTIONID_DIVIDER + id).toLowerCase();
        }
        TMLAction action = null;
        Integer actionKey = (Integer) actionRegistration.get(qualifiedID);
        if (actionKey != null) {
            TMLAction theAction = (TMLAction) actionRegistration.get(actionKey);
            if (theAction != null && theAction.upToDate(this)) {
                action = theAction;
            }
        }
        return action;
    }
    
    public String getDesignDBKey() {
        return _designContext.getDesignDB().getDbReference();
    }

    public TMLAction getModuleActionByID(String id, String dbKey) throws TMLActionException {
        try {
            
            
            // Determine if there is dbkey information in the id
            List idParts = WGUtils.deserializeCollection(id, "/");
            if (idParts.size() == 2) {
                dbKey = (String) idParts.get(0);
                id = (String) idParts.get(1);
            }
            
            // Get the design db. Either by dbkey in id, by parameter dbkey, by current design db of WebTML or just the current context db
            WGDatabase designDB;
            if (!WGUtils.isEmpty(dbKey)) {
                designDB = db(dbKey);
            }
            else {
                designDB = designdb();
            }
            
            if (designDB == null) {
                throw new TMLActionException("Could not open design db to load tmlscript action module, because it does not exist.");
            }
            if (!designDB.isSessionOpen()) {
                throw new TMLActionException("Could not open design db '" + designDB.getDbReference() + "' to load tmlscript action module, because  you have no access.");
            }
            
            WGCSSJSModule mod = designDB.getCSSJSModule(id, WGScriptModule.CODETYPE_TMLSCRIPT);
            if (mod == null) { 
                return null;
            }
            
            TMLAction action =  TMLAction.buildActionFromScriptModule(mod);
            registerAction(action, id, designDB.getDbReference());
            return action;
            
        }
        catch (WGException e) {
           throw new TMLActionException("Could not open design db to load tmlscript action module", e);
        }
    }
    
    @CodeCompletion
    public TMLForm createform(TMLFormInfo formInfo) throws WGException {
        
        TMLForm form = new TMLForm(getwgacore(), formInfo, this);
       _environment.setForm(form);
        return form;
        
    }
    
    @CodeCompletion
    public TMLFormInfo createforminfo(String id) {
        return new TMLFormInfo(id, TMLFormInfo.HTMLINPUT_FALSE, false, getDesignContext().getVersionCompliance());
    }
    
    @CodeCompletion
    public TMLFormInfo createforminfo(String id, boolean htmlInput, boolean persistent) {
        return new TMLFormInfo(id, String.valueOf(htmlInput), true, getDesignContext().getVersionCompliance());
    }


    /**
     * Takes a reference name that might be a local name (starting with ":").
     * If it is a local name it will get expanded by the directory name of the currently executed module.
     * If it is not it will just get returned unmodified.
     * @param ref The reference name to expand
     * @param currentModuleRef The name of the currently executed module
     * @return The expanded name
     */
    public static String processReferencePath(String ref, String currentModuleRef) {
        
        if (ref == null) {
            return "";
        }
        
        // Local reference: Start with parent folder
        String startPath = "";
        if (ref.startsWith("::")) { // Local reference, start with folder of current module ref
            if (currentModuleRef != null) {
                int colonPos =currentModuleRef.lastIndexOf(":");
                if (colonPos != -1) {
                    currentModuleRef = currentModuleRef.substring(0, colonPos);
                    startPath = currentModuleRef;
                }
            }
            ref = ref.substring(2);
        }
        
        // Process path parts;
        List<String> refParts = WGUtils.deserializeCollection(ref, ":");
        List<String> currentParts = "".equals(startPath) ? new ArrayList<String>() : WGUtils.deserializeCollection(startPath, ":");
        int idx = -1;
        for (String part : refParts) {
            idx++;
            
            if (part.equals("")) { 
                if (idx == 0) { // A single colon at the start? Go up to the root. Else ignore.
                    currentParts.clear();
                }
            }
            else if (part.equals("..")) { // Go up one level
                if (idx == 0) { // At start: Use the parent folder of the base reference, otherwise just go up from wherever we are 
                    currentParts = "".equals(currentModuleRef) ? new ArrayList<String>() : WGUtils.deserializeCollection(currentModuleRef, ":");
                }
                
                if (currentParts.size() > 0) {
                    currentParts.remove(currentParts.size() - 1);
                }
            }
            else if (part.equals(".")) { // Stay on the current level
                if (idx == 0) { // At start: Use the current reference. Otherwise do nuffin
                    currentParts = "".equals(currentModuleRef) ? new ArrayList<String>() : WGUtils.deserializeCollection(currentModuleRef, ":");
                }
            }
            else {
                currentParts.add(part);
            }
        }
        
        // Return
        if (currentParts.size() == 0) {
            return "";
        }
        else {
            return WGUtils.serializeCollection(currentParts, ":");
        }

    }
    
    @CodeCompletion
    public WGResultSet lucenesearch(String phrase, String scope) throws WGQueryException {
        try {
            Map params = new HashMap();
            
            scope = scope.toLowerCase();
            if (scope.equals("wga")) {
                params.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, LuceneManager.SEARCHSCOPE_WGA);
            }
            else if (scope.equals("domain")) {
                params.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, LuceneManager.SEARCHSCOPE_DOMAIN);  
            }
            else {
                params.put(LuceneManager.QUERYOPTION_SEARCHSCOPE, LuceneManager.SEARCHSCOPE_DB);
            }
            
            return getwgacore().getLuceneManager().search(db(), phrase, params, WGA.get(this));
        }
        catch (WGException e) {
            if (e instanceof WGQueryException) {
                throw (WGQueryException) e;
            }
            else {
                throw new RuntimeException(e);
            }
        }
    }
    
    @CodeCompletion
    public WGResultSet lucenesearch(String phrase) throws WGQueryException {
        return lucenesearch(phrase, "db");
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#fileurl(java.lang.String, java.lang.String)
     */
    @Override
    public String fileurl(String containerName, String fileName) throws WGException {
        return fileurl(null, containerName, fileName);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#fileurl(java.lang.String, java.lang.String, java.lang.String)
     */
    @Override
    public String fileurl(String dbKey, String containerName, String fileName) throws WGException {
        
        WGAURLBuilder builder = getURLBuilder();
        
        if (getDesignContext().getVersionCompliance().isAtLeast(7,2) && builder instanceof WGASpecificFileURLBuilder) {
            return ((WGASpecificFileURLBuilder) builder).buildContentFileURL(toUnlockedVersion(), dbKey, containerName, fileName);
        }
        else {
            return builder.buildFileURL(toUnlockedVersion(), dbKey, containerName, fileName);
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#fileurl(java.lang.String)
     */
    @Override
    public String fileurl(String fileName) throws WGException {
        return fileurl(null, null, fileName);
    }
    
    public String filedataurl(String fileName) throws WGAPIException {
    	return filedataurl(null, null, fileName, null, null);
    }

    public String filedataurl(String containerName, String fileName) throws WGAPIException {
        return filedataurl(null, containerName, fileName, null, null);
    }
    
    @Deprecated
    public String filedataurl(String containerName, String fileName, String contentType) throws WGAPIException {
        return filedataurl(null, containerName, fileName, contentType);
    }
    @Deprecated
    public String filedataurl(String dbKey, String containerName, String fileName, String contentType) throws WGAPIException {
    	return filedataurl(dbKey, containerName, fileName, contentType, null);
    }

    public String filedataurl(String fileName, Map<String,String> config) throws WGAPIException {
    	return filedataurl(config.get("designdb"), config.get("doc"), fileName, config.get("mimetype"), config.get("derivate"));
    }

    /**
     * creates a RFC2397 data url from the given file
     * @param dbKey
     * @param containerName
     * @param fileName
     * @param contentType
     * @return The url
     * @throws WGAPIException 
     */
    public String filedataurl(String dbKey, String containerName, String fileName, String contentType, String derivate) throws WGAPIException {
        
        // retrieve input stream
        InputStream fileIn = null;
        int fileSize = 0;
        try {

            // If containerName null get file from current content
            if (containerName == null) {

                fileIn = content().getFileData(fileName);
                fileSize = content().getFileSize(fileName);

                if(derivate!=null){
                	// search for derivate:
                	FileDerivateManager fdm = getwgacore().getFileDerivateManager();
	            	DerivateQuery derivateQuery = fdm.parseDerivateQuery(derivate);
	                WGFileAnnotations md = fdm.queryDerivate(content(), fileName, derivateQuery, new ClientHints(), true);
	                if(md!=null && md instanceof WGFileDerivateMetaData){
	                	fileIn = content().getFileDerivateData(((WGFileDerivateMetaData)md).getId());
	                	fileSize = (int)md.getSize();
	                }
                }
                if (fileIn == null) {
                    addwarning("File '" + fileName + "' is not attached to content '" + content().getContentKey().toString() + "'.");
                    return null;
                }
            }
            
            // Else try to find container
            else {
                DesignResourceReference containerRef = resolveDesignResource(dbKey, containerName);
                
                // get file from filecontainer
                WGDatabase designDB = db(containerRef.getDesignApp());
                if (designDB == null) {
                    addwarning("Unknown design db: " + dbKey);
                    return null;
                }
                    
                // first try - lookup file container of designDB or explicit given db
                WGDocument container = designDB.getFileContainer(containerRef.getResourceName());
                if (container == null) {
    
                    // second try - lookup file container in current context db
                	container = db().getFileContainer(containerRef.getResourceName());
                	
                	if (container == null) {
                		// third try - use container name as key and try to find a content document
                		container = WGPDispatcher.getContentByAnyKey(containerRef.getResourceName(), db(), new WebTMLLanguageChooser(db(), this), isbrowserinterface());
                	}
                }
                    
                if (container == null) {
                    addwarning("No file container or content found for '" + containerName + "'.");
                    return null;                    
                }
                
                fileIn = container.getFileData(fileName);
                fileSize = container.getFileSize(fileName);
                if (fileIn == null) {
                    addwarning("File '" + fileName + "' not found in filecontainer '" + containerName + "'.");
                    return null;
                }                
            }
            
        }
        catch (WGException e) {
            addwarning("Error opening design db " + getDesignDBKey() + ": " + e.getClass().getName() + " - " + e.getMessage());
            getwgacore().getLog().error("Error opening design db " + getDesignDBKey(), e);
            return null;
        }
        
        
        // Create the byte array to hold the data
        byte[] fileData = new byte[fileSize];  
        // read filedata in byte-array
        try {            
            // Read in the bytes
            int offset = 0;
            int numRead = 0;
            while (offset < fileData.length
                   && (numRead=fileIn.read(fileData, offset, fileData.length-offset)) >= 0) {
                offset += numRead;
            }
            // Ensure all the bytes have been read
            if (offset < fileData.length) {
                addwarning("Not all data could be readed from file '" + fileName + "'.");
                return null;
            }
            // close the stream
            fileIn.close();
        } catch (IOException e) {
            addwarning("Unable to read file '" + fileName + "' from filecontainer '" + containerName + "' - " + e.getMessage());
            getwgacore().getLog().error("Unable to read file '" + fileName + "' from filecontainer '" + containerName + "'.", e);
            return null;
        }
        
        if (contentType == null && getEnvironment().isPageContextAvailable()) {
            // try to determine mime type
            contentType = getwgacore().getServletContext().getMimeType(fileName);            
        }        
        return createDataURL(fileData, contentType);        
    }
    
    /**
     * creates a base64 encoded RFC2396 dataurl
     * @param data
     * @param contentType
     * @return
     */
    private String createDataURL(byte data[], String contentType) {
        StringBuffer url = new StringBuffer();
        url.append("data:");
        if (contentType != null) {
            url.append(contentType);            
        }
        url.append(";base64");
        url.append("," + Base64.encode(data));
        return url.toString();        
    }
    
    /**
     * creates an urlencoded RFC2396 dataurl
     * @param data
     * @return
     * @throws UnsupportedEncodingException 
     * @throws URIException 
     */
    private String createDataURL(String data, String contentType) throws URIException {
        StringBuffer url = new StringBuffer();
        url.append("data:");
        if (contentType != null) {
            url.append(contentType);            
        }
        // RFC2396 defines that data is encoded in US-ASCII  
        url.append("," + URIUtil.encodeWithinPath(data, "UTF-8"));
        return url.toString(); 
    }
    
    public void saveprofileonend() {
        if (!iswebenvironment()) {
            return;
        }
        
        TMLUserProfile prof = getprofile();
        if (prof == null) {
            return;
        }
        
        prof.setSavedOnEnd(true);
    }
    
    @CodeCompletion
    public Map createlookuptable() {
        return WGA.get(this).createLookupTable();
    }
    
    @CodeCompletion
    public Map createlookuptable(Map map) {
        return WGA.get(this).createLookupTable(map);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#createevent(java.lang.String)
     */
    @Override
    public PortletEvent createevent(String name) {
        return new PortletEvent(name, _designContext.getVersionCompliance());
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isempty(java.lang.String)
     */
    @Override
    public boolean isempty(String itemName) throws WGAPIException {
        
        Object item = item(itemName);
        return isemptyvalue(item);
        
    }


    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isemptyvalue(java.lang.Object)
     */
    @Override
    public boolean isemptyvalue(Object value) {
        if (value == null) {
            return true;
        }
        
        if (value instanceof List) {
            List list = (List) value;
            if (list.size() == 0) {
                return true;
            }
        }
        
        if (value instanceof String) {
            String string = (String) value;
            if (string.trim().equals("")) {
                return true;
            }
            
            // RTF-Editor sets a single <br> to a field when empty
            if (string.trim().equals("<br>")) {
                return true;
            }
        }
        
        return false;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isfilled(java.lang.String)
     */
    @Override
    public boolean isfilled(String attValue) throws WGAPIException {
        
        boolean isEditmode = (isbrowserinterface() && content().getStatus().equals(WGContent.STATUS_DRAFT));
        if (!isEditmode) {
            return !isempty(attValue);
        }
        else {
            return true;
        }
        
    }



    
    @CodeCompletion
    public boolean login(String user, String password, String domain) throws WGException {
        return getwgacore().login(user, password, domain, getrequest(), getresponse());
    }
    
    public boolean login(String user, String password) throws WGException {
        return login(user, password, (String) db().getAttribute(WGACore.DBATTRIB_DOMAIN));
    }
    
    @CodeCompletion
    public boolean logout(String domain) throws WGException {
        return getwgacore().logout(domain, getrequest().getSession(), getrequest(), getresponse(), true);
    }


    
    @CodeCompletion
    public WGDatabase plugindb(String pluginUniqueName) throws WGException {
        return db(plugindbkey(pluginUniqueName));
    }
    
    @CodeCompletion
    public PluginID pluginid(String pluginUniqueName) throws WGException {
        
        WGAPlugin plugin = getwgacore().getPluginSet().getPluginByUniqueName(pluginUniqueName);
        if (plugin != null) {
            return plugin.getCsConfig().getPluginConfig().getId();
        }
        else {
            return null;
        }

    }
    
    @CodeCompletion
    public String plugindbkey(String pluginUniqueName) {
        
        WGAPlugin plugin = getwgacore().getPluginSet().getPluginByUniqueName(pluginUniqueName);
        if (plugin != null) {
            return plugin.buildDatabaseKey();
        }
        else {
            return null;
        }
        
    }
    
    @CodeCompletion
    public String encode(String encode, Object input) throws FormattingException {
    	ObjectFormatter formatter = getwgacore().getEncodingFormatter(encode, this);
    	if (formatter != null) {
    		return formatter.format(input);
    	}
    	else {
    		addwarning("No encoding formatter registered under encoding key '" + encode + "'");
    		return input.toString();
    	}    	
    }
    
    public String multiencode(String encode, Object input) throws FormattingException {
        
        FormattingChain formatters = new FormattingChain();
        Iterator encoders = WGUtils.deserializeCollection(encode, ",", true).iterator();
        while (encoders.hasNext()) {
            String encoder = (String) encoders.next();
            ObjectFormatter formatter = getwgacore().getEncodingFormatter(encoder, this);
            formatters.addFormatter(formatter);
        }
        
        return formatters.format(input);
        
    }
    
    public WGAURLBuilder getURLBuilder() {
        return getEnvironment().getURLBuilder();
    }
    
    @CodeCompletion
    public Document loadhtml(HttpClient client, String url) throws UnsupportedEncodingException, WGAPIException, IOException, HttpException, SAXException {
        try {
            return WGA.get(this).html().load(client, url);
        }
        catch (WGException e) {
            if (e instanceof WGAPIException) {
                throw (WGAPIException) e;
            }
            else {
                throw new WGAPIException(e);
            }
        }
    }
    
    @CodeCompletion
    public Document loadhtml(String url) throws UnsupportedEncodingException, WGAPIException, HttpException, IOException, SAXException {
        try {
            return WGA.get(this).html().load(url);
        }
        catch (WGException e) {
            if (e instanceof WGAPIException) {
                throw (WGAPIException) e;
            }
            else {
                throw new WGAPIException(e);
            }
        }
    }
    
    @CodeCompletion
    public Document parsehtml(String html) throws SAXException, IOException {
        try {
            return WGA.get(this).html().parse(html);
        }
        catch (WGException e) {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Registers an action for the current Http-Session, if an action with this code is not already registered.
     * Further code about this action object should use the object returned by this method, since that may be
     * a previous registered instance of it with the correct sequence number.
     * @param currentAction The action to register
     * @param id The id of the action. Specify null if the action has no id.
     * @return The action that is registered. Either the action given as parameter, or another one with identical code that already was registered
     */
    public TMLAction registerAction(TMLAction currentAction, String id, String designDB) {
        
        TMLAction tmlAction;
        
        // Check if action registration available. If not, fail silent.
        Map actions = getActionRegistration();
        if (actions == null) {
            return currentAction;
        }
        
        // check if action already exists
        if (!actions.containsKey(currentAction.getKey())) {
            // register new action
            actions.put(currentAction.getKey(), currentAction);
            tmlAction = currentAction; // store new action under tmlAction for further operations
        } 
        else {
            // get registered action for further opertations
            tmlAction = (TMLAction) actions.get(currentAction.getKey());
        }
        
        // register action id mapping
        if (id != null) {
            // Map with design db qualifier
            actions.put("ID#" + (designDB + TMLContext.ACTIONID_DIVIDER + id).toLowerCase(), currentAction.getKey());
            
            // Map without design db qualifier
            actions.put("ID#" + id.toLowerCase(), currentAction.getKey());

            currentAction.setID(id.toLowerCase());
        }
        return tmlAction;
    }
    
    public void removePortletVariables(String prefix) {
        
        // Request variables
        Iterator varsIt = _environment.getPageVars().keySet().iterator();
        String key;
        while (varsIt.hasNext()) {
            key = (String) varsIt.next();
            if (key.startsWith(prefix)) {
                varsIt.remove();
            }
            
        }

        // No longer necessary to clear session variables as they are now stored on portlet state
        
        
        
    }
	
    
    public String geturlparameter(String name) {
    	return getrequest().getParameter(name);
    }
    
	public List geturlparameterlist(String name) {
		String[] values = getrequest().getParameterValues(name);
		List list = new ArrayList();
		if (values != null) {
			for (int i = 0; i < values.length; i++) {
				list.add(values[i]);
			}
		}
		return list;
	}

	public List geturlparameternames() {
		return new ArrayList(getrequest().getParameterMap().keySet());
	}
	
	public void seturlparameterifempty(String name, String value) throws WGIllegalStateException {
		if (getrequest() instanceof WGAFilter.RequestWrapper) {
			RequestWrapper wrapper = (WGAFilter.RequestWrapper) getrequest();							
			wrapper.setParameterIfEmpty(name, value);										
		} else {
			throw new WGIllegalStateException("method setUrlParameterIfEmtpy() is only supported on GET requests.");
		}
	}
	
	public void seturlparameterifempty(String name, List values) throws WGIllegalStateException {
		if (getrequest() instanceof WGAFilter.RequestWrapper) {
			RequestWrapper wrapper = (WGAFilter.RequestWrapper) getrequest();
			wrapper.setParameterIfEmpty(name, (String[]) values.toArray(new String[values.size()]));	
		} else {
			throw new WGIllegalStateException("method setUrlParameterIfEmtpy() is only supported on GET requests.");
		}
	}
	
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isfirstloop()
     */
	@Override
    public boolean isfirstloop() {
	    
	    BaseTagStatus tag = getDesignContext().getTag();
        if (tag == null) {
            return false;
        }
	    
		ForEach.Status targetTag = (ForEach.Status) tag.getAncestorTag(IterationTag.class);
		if (targetTag == null) {
			addwarning("Could not find ancestor iteration tag for method isfirstloop().");
			return false;
		}
		return targetTag.getIterationIndex() == 1; 
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#islastloop()
     */
	@Override
    public boolean islastloop() {
	    
	    BaseTagStatus tag = getDesignContext().getTag();
        if (tag == null) {
            return false;
        }
	    
		ForEach.Status targetTag = (ForEach.Status) tag.getAncestorTag(IterationTag.class);
		if (targetTag == null) {
			addwarning("Could not find ancestor iteration tag for method islastloop().");
			return false;
		}
		
		return targetTag.isLastIteration();
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isfirstloop(java.lang.String)
     */
	@Override
    public boolean isfirstloop(String tagId) {
	    
	    BaseTagStatus tag = getDesignContext().getTag();
        if (tag == null) {
            return false;
        }
	        
		IterationTagStatus targetTag = (IterationTagStatus) tag.getTagStatusById(tagId, IterationTagStatus.class);
		if (targetTag == null) {
			addwarning("Could not find iteration tag with id '" + tagId + "'.");
			return false;
		}
		return targetTag.getIterationIndex() == 1;
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#islastloop(java.lang.String)
     */
	@Override
    public boolean islastloop(String tagId) {
		BaseTagStatus tag = getDesignContext().getTag();
		if (tag == null) {
		    return false;
		}
		
        IterationTagStatus targetTag = (IterationTagStatus) tag.getTagStatusById(tagId, IterationTagStatus.class);
		if (targetTag == null) {
			addwarning("Could not find iteration tag with id '" + tagId + "'.");
			return false;
		}
		return targetTag.isLastIteration();
	}	
	

	

	
	public String resolveDBKey(String dbkey) {
	    try {
            return WGA.get(this).design().resolveDbKey(dbkey);
        }
        catch (Exception e) {
            throw new RuntimeException("Exception resolving database key", e);
        }
	}



    /**
	 * Ugly duplicate of {@link #pluginid(WGDatabase)}, kept for historical reasons. See B00005E92
	 * @param db
	 * @return
	 */
	public PluginID getpluginid(WGDatabase db) {
	    return pluginid(db);
	}
	
	@CodeCompletion
	public PluginID pluginid(WGDatabase db) {
	    return (PluginID) db.getAttribute(WGACore.DBATTRIB_PLUGIN_ID);
	}


    public Map getPersistentForms() {
        return _environment.getPersistentForms();
    }


    public TMLDesignContext getDesignContext() {
        return _designContext;
    }
	
    
    public ExpressionResult evaluateExpression(String expression, Map additionalObjects) {
    	ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
    	return engine.evaluateExpression(expression, this, ExpressionEngine.TYPE_EXPRESSION, additionalObjects);
    }
    
    public ExpressionResult evaluateExpression(String expression) {
    	return evaluateExpression(expression, null);
    }
    
    public ExpressionResult evaluateScript(String script, Map additionalObjects) {
    	ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
    	return engine.evaluateExpression(script, this, ExpressionEngine.TYPE_SCRIPT, additionalObjects);
    }
    
    public ExpressionResult evaluateScript(String script) {
    	return evaluateScript(script, null);
    } 
    
    @CodeCompletion
    public void waitforauthupdates(WGDatabase db, int timeoutSeconds) throws WGAServerException {
        
        if (db == null) {
            db = db();
        }
        
        // Collect the CSAuthModules listening to content save events on this database
        List listeners = db.getContentEventListeners();
        List modules = new ArrayList();
        synchronized (listeners) {
            Iterator it = listeners.iterator();
            while (it.hasNext()) {
               WGContentEventListener listener = (WGContentEventListener) it.next();
               if (listener instanceof CSAuthModule) {
                   modules.add(listener);
               }
            }
        }
        
        // Look thru the modules until their currently running update threads have finished
        long timeout = System.currentTimeMillis() + (timeoutSeconds * 1000);
        Map moduleThreads = new HashMap();
        do {
            
            // Idle. Check timeout
            try {
                Thread.sleep(100);
            }
            catch (InterruptedException e) {
            }
            if (System.currentTimeMillis() > timeout) {
                throw new WGAServerException("Method waitForAuthUpdates() encountered timout of " + timeoutSeconds +  "seconds");
            }
            
            // Iterate modules
            Iterator modIt = modules.iterator();
            while (modIt.hasNext()) {
                CSAuthModule mod = (CSAuthModule) modIt.next();
                
                // Look if a collector runs right now. If not, then this module is thru
                if (mod.getCurrentCollectorThread() == null) {
                    modIt.remove();
                }
                
                // A collection runs right now.
                else {

                    // Retrieve an maybe recorded earlier thread
                    Thread earlierThread = (Thread) moduleThreads.get(mod);
                    
                    // if there is no earlier thread then we just haven't recorded it yet
                    if (earlierThread == null) {
                        moduleThreads.put(mod, mod.getCurrentCollectorThread());
                    }
                    
                    // If there was an earlier thread, we look if it is still the same. If not, the earlier thread is finished and our update most likely thru
                    else if (!earlierThread.equals(mod.getCurrentCollectorThread())) {
                        modIt.remove();
                    }
                    
                }
            }
            
            
        } while (modules.size() > 0);
        
        
    }
    
    /**
     * Retrieves a WGA internal system label from the properties files lying under {@link WGACore#SYSTEMLABEL_BASE_PATH} in the language of the current request locale
     * @param systemBundleName Name of the bundle file
     * @param labelKey Key of the label
     * @param params A list of parameters that replace placeholders {1}...{n} in the label 
     * @return The label or an empty string if none was found
     */
    public String systemLabel(String systemBundleName, String labelKey, List params) {
                
        try{
            return WGA.get(this).design().systemLabel(systemBundleName, labelKey, params);
        }
        catch(WGException e){    
            this.addwarning(e.getMessage(), false);
            return "";
        }

    }
    
    public String systemLabel(String systemBundleName, String labelKey) {
        return systemLabel(systemBundleName, labelKey, null);
    }


    public static TMLContext createMasterSessionContext(TMLContext context) throws WGAPIException {
        
        if (context.getdocument().isTemporary() && !context.getdocument().isDummy()) {
            throw new WGIllegalArgumentException("Context document '" + context.getdocument().getDocumentKey() + "' is temporary and cannot be used as context when switching to master session");
        }
        
        TMLContext mainContext = context.getmaincontext();
        TMLContext masterMainContext = new TMLContext(mainContext.getdocument(), mainContext.getwgacore(), context.getprofile(), context.gettmlform()).designContext(context.getDesignContext());
        
        
        TMLContext masterContext;
        if (!context.getdocument().equals(masterMainContext.getdocument())) { 
            masterContext = masterMainContext.context(context.getdocument());
            if (masterContext == null) {
                throw new WGIllegalDataException("Unretrievable master action context: " + context.getpath());
            }
        }
        else {
            masterContext = masterMainContext;
        }
        
        try {
            masterContext.importEnvironmentData(context);
        }
        catch (TMLException e) {
            context.getlog().error("Error importing TMLScript environment settings", e);
        }
        
        return masterContext;
    }
    
    public static Map<String, TransientObjectWrapper<Object>> fetchSessionVars(HttpSession session) {
        @SuppressWarnings("unchecked")
        Map<String,TransientObjectWrapper<Object>> sessionVars =  (Map<String,TransientObjectWrapper<Object>>) session.getAttribute(WGPDispatcher.SESSION_VARS);
        if (sessionVars == null) {
            sessionVars = new ConcurrentHashMap<String, TransientObjectWrapper<Object>>();
            session.setAttribute(WGPDispatcher.SESSION_VARS, sessionVars);
        }
        return sessionVars;
    }

    public static ProcessContextRegistration fetchProcessContextRegistration(HttpSession session) {
        return (ProcessContextRegistration) session.getAttribute(SESSIONATTRIB_PROCESSCONTEXTS);
    }

    public static Map<String, TMLAction> fetchActionRegistration(HttpSession session) {
        @SuppressWarnings("unchecked")
        Map<String,TMLAction> actionRegistration = (Map<String,TMLAction>) session.getAttribute(WGACore.SESSION_ACTIONS);
        if (actionRegistration == null) {
            actionRegistration = new ConcurrentHashMap<String, TMLAction>();
            session.setAttribute(WGACore.SESSION_ACTIONS, actionRegistration);
        }
        return actionRegistration;
    }

    public static TMLUserProfile fetchUserProfile(WGACore core, WGDatabase db, HttpServletRequest req, HttpServletResponse res) {
        try {
           return core.getPersManager().fetchUserProfile(req, res, db);
        }
        catch (WGAPIException e) {
            core.getLog().error("Unable to retrieve user profile", e);
        }
        return null;
    }

    public static List<Warning> fetchWarnings(HttpServletRequest req) {
        @SuppressWarnings("unchecked")
        List<Warning> warnings = (List<Warning>) req.getAttribute(Base.class.getName() + ":Warnings");
        if (warnings == null) {
            warnings = new ArrayList<Warning>();
            req.setAttribute(Base.class.getName() + ":Warnings", warnings);
        }
        return warnings;
    }

    public static Map<String,TMLForm> fetchTransientForms(HttpServletRequest req) {
        @SuppressWarnings("unchecked")
        Map<String,TMLForm> forms = (Map<String,TMLForm>) req.getAttribute(WGACore.ATTRIB_TMLFORM);
        if (forms == null) {
            forms = new HashMap<String,TMLForm>();
            req.setAttribute(WGACore.ATTRIB_TMLFORM, forms);
        }
        return forms;
    }

    @SuppressWarnings("unchecked")
    public static Map<String,TMLForm> fetchPersistentForms(HttpSession session) {
        return (Map<String,TMLForm>) session.getAttribute(WGACore.ATTRIB_TMLFORM);
    }

    public WGUserAccess getoriginaluserdata() {
        
        RootEnvironmentUserData userData = getEnvironment().getRootEnvironmentUserData();
        
        // We have no root environment data = we are in root environment. So we return the current db user access
        if (userData == null) {
            return db().getSessionContext().getUserAccess();
        }
        
        // We return root environment data only if it matches the database currently in context
        if (userData.getDbkey().equals(db().getDbReference())) {
            return userData.getUserAccess();
        }
        
        // We have root environment data, but it does not match the current context db = we have no data for the current db. So return null.
        else {
            return null;
            
        }
        
    }

	/* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#removetmlform(java.lang.String)
     */
	@Override
    public void removetmlform(String formid) {
		_environment.removeForm(formid);		
	}
	
	public void attachscaledimage(WGDocument doc, ImageScaler scaler, String targetFileName) throws IOException, WGAPIException {               
        // Determine final file name
        String finalFileName = null;
        if( targetFileName != null ) {
            finalFileName = targetFileName + scaler.getFormatSuffix();
        }
        else {
            String fileName = scaler.getSourceFileName();
            if (fileName == null) {
                throw new IllegalArgumentException("Unable to determine file name. Please specify as targetFileName.");
            }
            
            String suffix = null;
            if (scaler.getFormat().equals("JPEG")) {
                suffix = ".jpg";
            }
            else if (scaler.getFormat().equals("PNG")) {
                suffix = ".png";
            }
            finalFileName = fileName.substring(0, fileName.lastIndexOf(".")) + suffix;
        }
        
        TemporaryFile targetFile = new TemporaryFile(finalFileName, null, WGFactory.getTempDir());
        targetFile.deleteOnEviction(doc.getDatabase().getSessionContext());
        
        // Write scaled image to target file
        scaler.writeImage(targetFile.getFile());       
            
        // Attach scaled image
        doc.attachFile(targetFile.getFile());                  
    }
	
	public void attachscaledimage(WGDocument doc, ImageScaler scaler) throws IOException, WGAPIException {
        attachscaledimage(doc, scaler, null);
    }
	
	public TMLContext dbContext(WGDatabase dbTarget, WGLanguageChooser chooser) throws WGAPIException {
	    
	    WGLanguage lang = chooser.selectDatabaseLanguage(dbTarget);
	    
	    // We want to create a db context even when there is no matching language
	    // so we just create dummy content on the source context language
	    if (lang == null) {
	        lang = dbTarget.getLanguage(content().getLanguage().getName());
	    }
	    
        WGContent dummyContent = dbTarget.getDummyContent(lang.getName());
        if (dummyContent != null) {
            return getTMLContextForDocument(dummyContent);
        }
        else {
            return null;
        }
        
	}
	
	public TMLContext dbContext(WGDatabase dbTarget) throws WGAPIException {
	   try {
        return (TMLContext) WGA.get(this).createTMLContext(dbTarget);
        }
        catch (WGException e) {
            // Tedious and ugly cause determination to keep exception signature of this legacy method
            if (e.getCause() instanceof WGAPIException) {
                throw (WGAPIException) e.getCause();
            }
            else {
                throw new WGAPIException("Exception creating db context", e);
            }
            
            
        }
	}
	
	public BaseTagStatus gettag() {
	    return getDesignContext().getTag();
	}
	
	public List<Warning> getWarnings() {
	    return getEnvironment().getWarnings();
	}
	
	public DesignResourceReference resolveDesignResource(String ref) throws WGException {
	    return resolveDesignResource(null, ref, null);
	}
	
	
	public DesignResourceReference resolveDesignResource(String designDB, String refString) throws WGException {
	    return resolveDesignResource(designDB, refString, null);	    
	}
	
	public DesignResourceReference resolveDesignResource(String refString, DesignResourceReference baseReference) throws WGException {
	    return resolveDesignResource(null, refString, baseReference);
	}
	
	/**
	 * Resolves a design reference to find the corresponding resource.
	 * Takes care of all special addressation forms:
	 * <ul>
	 * <li>Local references
	 * <li>dbkeys addressed inside the refString with "dbkey/" syntax
	 * <li>@base
	 * @param refDB dbkey of an explicitly chosen target app. Give null if nothing was chosen.
	 * @param refString The reference string addressing the resource
	 * @param baseReference The base reference used to resolve relative addressation
	 * @return A design resource reference object, with resolved appdb/resource name
	 * @throws WGException 
	 */
	public DesignResourceReference resolveDesignResource(String refDB, String refString, DesignResourceReference baseReference) throws WGException {
	    
	    Design design = WGA.get(this).design();
	    
	    if (baseReference != null) {
	        design = design.resolve(baseReference);
	    }
	    	    
	   return design.resolveReference(new DesignResourceReference(refDB, refString));
	}
	
    public String getScopedString(String str, String scope) {
        try {
            return WGA.get(this).scoped(str, scope);
        }
        catch (WGException e) {
            // Cannot happen when scope is provided
            return str;
        }
    }
    
    public List<String> buildOptions(Iterable<WGContent> contents, String titleExpr, String emptyTitle) throws WGAPIException  {
        try {
            return WGA.get(this).buildOptions(contents, titleExpr, emptyTitle);
        }
        catch (WGException e) {
            // Cannot happen
            return null;
        }
    }

    public String scripturl(String dbKey, String type, String doc) throws WGException {
        return getURLBuilder().buildScriptURL(this, dbKey, type, doc);
    }
    
    public String scripturl(String type, String doc) throws WGException {
        return getURLBuilder().buildScriptURL(this, getDesignDBKey(), type, doc);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#istagidvalid(java.lang.String)
     */
    @Override
    public boolean istagidvalid(String tagid) {
        BaseTagStatus status = getDesignContext().getTag();
        if (status != null) {
            return (status.getTagStatusById(tagid) != null ? true : false);
        }
        else {
            return false;
        }
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#hasrole(java.lang.String)
     */
    @Override
    public boolean hasrole(String role) throws WGAPIException {
        return metalist("db", "userroles").contains(role);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#hasgroup(java.lang.String)
     */
    @Override
    public boolean hasgroup(String group) throws WGAPIException {
        return metalist("db", "usergroups").contains(group);
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#isanonymous()
     */
    @Override
    public boolean isanonymous() throws WGAPIException {
        return db().getSessionContext().isAnonymous();
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.Context#appendvar(java.lang.String, java.lang.Object)
     */
    @Override
    public void appendvar(String name, Object value) throws WGAPIException {

        
        try {
            Object oldValue = retrieveVar(name);
            List newValue;
            
            if (oldValue instanceof NullPlaceHolder) {
                newValue = new ArrayList();
            }
            else {
                newValue = toList(oldValue);
            }
            
            if (value instanceof Collection) {
                newValue.addAll((Collection) value);
            }
            else {
                newValue.add(value);
            }
            updateOrSetVar(name, newValue);
        }
        catch (WGException e) {
            throw new WGAPIException(e);
        }
        
    }
    
    @Override
    public String toString() {
        try {
            return getpath();
        }
        catch (WGAPIException e) {
            return super.toString();
        }
    }

    public String getUriHash() {
        return (String) getrequest().getAttribute(WGACore.ATTRIB_URI_HASH);
    }
    

    public DerivateQuery enhanceFileDerivateQuery(String fileDerivates) throws WGInvalidDerivateQueryException {
        String existingDerivates = (String) option(Base.OPTION_IMAGE_DERIVATES);
        DerivateQuery derivateQuery;
        if (existingDerivates != null) {
            derivateQuery = getwgacore().getFileDerivateManager().mergeDerivateQueries(fileDerivates, existingDerivates);
        }
        else {
            derivateQuery = getwgacore().getFileDerivateManager().parseDerivateQuery(fileDerivates);
        }
        return derivateQuery;
    }
    
    public TMLContext designContext(TMLDesignContext designContext) {
        return new TMLContext(this, designContext);
    }
    
    public TMLContext designContext(String baseReference) {
        return designContext(_designContext.createContextDelegate(_designContext.getDesignDB(), baseReference));
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((document == null) ? 0 : document.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TMLContext other = (TMLContext) obj;
        if (document == null) {
            if (other.document != null)
                return false;
        }
        else if (!document.equals(other.document))
            return false;
        return true;
    }

    public PortletEvent fetchRequestPortletEvent() {
        
        // Event originally thrown from server
        String eventIdxStr = getrequest().getParameter("$portletEvent");
        if (!WGUtils.isEmpty(eventIdxStr)) {
            Long eventIdx = Long.parseLong(eventIdxStr);
            PortletEvent event = findEventByIndex(eventIdx);
            if (event != null) {
                return event;
            }
        }
        
        // Event originally thrown by browser client. We just instantiate an event of the given name, but have no parameters and such
        String eventName = getrequest().getParameter("$portletEventName");
        if (!WGUtils.isEmpty(eventName)) {
            PortletEvent event = new PortletEvent(eventName, getDesignContext().getVersionCompliance());
            try {
                String eventParams = getwgacore().getURLEncoder().decode(getrequest().getParameter("$portletEventParams"));
                if (eventParams != null) {
                    event.setParameters(new Gson().fromJson(eventParams, Map.class));
                }
            }
            catch (Exception e) {
                getlog().error("Exception decoding portlet event parameters from JSON", e);
            }
            
            return event;
        }
        
        return null;
    }

    private PortletEvent findEventByIndex(Long eventIdx) {
    
        LinkedMap events = TMLPortlet.getFiredEventsQueue(gethttpsession());
        return (PortletEvent) events.get(eventIdx);
        
    }

    public boolean isCurrentSequenceNumber(TMLActionLink actionLink, HttpSession session, boolean ajax) {
        
        boolean correctSequence = true;
        
        // In case of norefresh actions we do not care about debouncing
        if (actionLink.getMode() != null && actionLink.getMode().equals(TMLActionLink.MODE_AJAX_NO_PORTLET_REFRESH)) {
           return true;
        }
        
        if (actionLink.isDefaultAction()) {
            
            TMLAction action = new TMLAction(actionLink.getDefaultAction());
            TMLAction.DefaultAction defaultAction = TMLAction.getDefaultAction(action);
            
            // When default action is unknown return true, 
            // so the action execution runs and issues warning "unknown default action"
            if (defaultAction == null) {
                return true;
            }
            
            // if debouncing is disabled we do not care about sequencenumber
            if (!action.isDebounce()) {
                return true;
            }
            
            if (!getwgacore().defaultActionSequenceIdAlreadyUsed(actionLink.getSequenceId())) {
                getwgacore().defaultActionCalledWithSequenceId(actionLink.getSequenceId());
            }
            else {
                correctSequence = false;
            }
        }
        else {
            TMLAction action = (TMLAction) getActionRegistration().get(actionLink.getActionKeyInteger());
            if (action == null) {
                // Will fail later when action will be called. More speaking error message can be put out there.
                return true;
            }
            
            // if debouncing is disabled we do not care about sequencenumber
            if (!action.isDebounce() ) {
                return true;
            }
            
            if (!action.sequenceIdAlreadyUsed(actionLink.getSequenceId())) {
                // action was not called with this sequenceId
                action.calledWithSequenceId(actionLink.getSequenceId());
            } else {
                correctSequence = false;
            }
        }
        return correctSequence;
    }
    
    public ManagedObject fetchModuleController() throws WGException {
        
        DesignResourceReference currentModule = getDesignContext().getBaseReference();
        final Design mcDesign = WGA.get(this).design().resolve(currentModule).resolve("::" + currentModule.getResourceLocalName() + ".controller");
        
        // Inside WebTML page return the existing module controller from option
         ManagedObject mc = (ManagedObject) option(Base.OPTION_MODULE_CONTROLLER);
        if (mc != null && mc.getDesign().equals(mcDesign.getBaseReference())) {
            return mc;
        }
        
        TMLContext controllerContext = designContext(mcDesign.getBaseReference().toString());
        WGA wga = WGA.get(controllerContext);

        // If the design is the same as for the portlet then the portlet controller is module controller
        TMLPortlet portlet = getportlet();
        if (portlet != null && !portlet.isroot()) {
            DesignResourceReference portletControllerDesign = portlet.getState().getControllerDesign();
            if (mcDesign.getBaseReference().equals(portletControllerDesign)) {
                ScopeObject portletController = portlet.getState().fetchController(wga);
                if (portletController != null) {
                    return portletController;
                }
            }
        }
        
        // Create module controller
        WGScriptModule rendererModule = mcDesign.getScriptModule(WGScriptModule.CODETYPE_TMLSCRIPT);
        if (rendererModule == null) {
            return null;
        }
        TMLScript tmlscript = wga.tmlscript();
        final Object createdMc = wga.tmlscript().createObject(mcDesign, ObjectType.V2_ISOLATED);
        return new ManagedObject() {
            
            @Override
            public void afterUsage() {
            }
            
            @Override
            public void beforeUsage() throws WGException {
            }

            @Override
            public Object getObject() {
                return createdMc;
            }
            
            @Override
            public DesignResourceReference getDesign() {
                return mcDesign.getBaseReference();
            }
            
        };
        
    }
    
    public TMLContext toIsolatedVersion() {
        if (!(_environment instanceof IsolatedTMLContextEnvironment)) {
            TMLContext isolatedContext =  new TMLContext(getcontent(), getwgacore(), getprofile(), gettmlform(), getrequest(), getresponse(), gethttpsession());
            isolatedContext.isolate();
            return isolatedContext;
        }
        else {
            return this;
        }
    }
    
    private TMLContext toUnlockedVersion() {
        if (_environment instanceof IsolatedTMLContextEnvironment) {
            TMLContextEnvironment unlockedEnv = ((IsolatedTMLContextEnvironment) _environment).getParentEnvironment();
            return new TMLContext(getcontent(), getwgacore(), getprofile(), unlockedEnv.getForm(), unlockedEnv.getRequest(), unlockedEnv.getResponse(), unlockedEnv.getSession());
        }
        else {
            return this;
        }
    }
    
    protected boolean isEnhancedItemExpressions() {
        return getDesignContext().getVersionCompliance().isAtLeast(7,2) || getDesignContext().getDesignDB().getBooleanAttribute(WGACore.DBATTRIB_ENHANCED_ITEMEXPRESSIONS, false);
    }
    
    public void setLocalVar(String name, Object value) throws WGException {
    	name = convertVarName(name);
        getDesignContext().setLocalVar(name, value);
    }
    
    private void isolate() {
        if (!(_environment instanceof IsolatedTMLContextEnvironment)) {
            _environment = new IsolatedTMLContextEnvironment(this, getEnvironment().getCore(), getprofile(), getEnvironment().getForm(), getEnvironment().getRequest(), getEnvironment().getResponse(), getEnvironment().getSession());
        }
    }

    @Override
    public boolean ishomepage(){
    	if(getcontent().isDummy())
    		return false;
    	String homepageName = (String) db().getAttribute(WGACore.DBATTRIB_HOME_PAGE_NAME);
    	if(homepageName!=null){
			try {
				String name = getcontent().getStructEntry().getUniqueName();
				if(name==null)
					name = getcontent().getUniqueName();
				if(name!=null && name.equalsIgnoreCase(homepageName))
					return true;
			} catch (WGAPIException e) {}
    	}
    	return false;
    }
	
} 
