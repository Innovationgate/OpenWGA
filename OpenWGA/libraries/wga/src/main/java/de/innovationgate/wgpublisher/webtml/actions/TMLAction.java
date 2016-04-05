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
package de.innovationgate.wgpublisher.webtml.actions;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.DocumentHelper;
import org.dom4j.Element;
import org.dom4j.Namespace;
import org.dom4j.QName;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGCancelledException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGIllegalStateException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.common.DesignDirectory;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.LoginException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.form.FieldReg;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.portlet.PortletEvent;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletProcessContext;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateStorage;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.ItemExpression;
import de.innovationgate.wgpublisher.webtml.utils.ObjectStrategy;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContext;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLDesignContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

public class TMLAction implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * The action is defined on a WebTML page, either inline or via <tml:action id="actionid">
     */
    public static final int ORIGIN_TML = 1;

    /**
     * The action is defined by a TMLScript module
     */
    public static final int ORIGIN_SCRIPT_MODULE = 2;

    /**
     * The action is a default action
     */
    public static final int ORIGIN_DEFAULT = 3;

    public static final String DEFAULTACTION_RESET = "reset";

    public static final String DEFAULTACTION_REFRESH = "refresh";

    public static final String DEFAULTACTION_STORE = "store";
    
    public static final String DEFAULTACTION_REMOVE = "delete";

    public static final String DEFAULTACTION_ATTACH = "attach";

    public static final String DEFAULTACTION_LOGIN = "login";

    public static final String DEFAULTACTION_SETSESSIONVAR = "setsessionvar";

    public static final String DEFAULTACTION_SETPSESSIONVAR = "setpsessionvar";

    public static final String DEFAULTACTION_SETVAR = "setvar";

    public static final String DEFAULTACTION_SETPVAR = "setpvar";

    public static final String DEFAULTACTION_HDBSTORE = "hdbstore";

    public static final String DEFAULTACTION_INCVAR = "incvar";
    
    public static final String DEFAULTACTION_FIREEVENT = "fireevent";
    
    public static final String DEFAULTACTION_CALLPCMETHOD = "pc";
    
    public static final String DEFAULTACTION_CALLMCMETHOD = "mc";
    
    public static final int FLAG_DEBOUNCED = 1;

    public abstract static class DefaultAction {

        private String _name;

        private boolean _debounced;
        
        public DefaultAction(String name, int flags) {
            _name = name;
            _debounced = (flags & FLAG_DEBOUNCED) == FLAG_DEBOUNCED;
        }

        public DefaultAction() {
        }

        /**
         * @return Returns the debounced.
         */
        public boolean isDebounced() {
            return _debounced;
        }

        /**
         * @return Returns the name.
         */
        public String getName() {
            return _name;
        }

        public abstract Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, TMLScriptException, WGException, IOException;

    }

    public static Map<String,DefaultAction> _defaultActions = new HashMap<String,DefaultAction>();

    static {

        DefaultAction action;

        action = new DefaultAction(DEFAULTACTION_ATTACH, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws WGAPIException, IOException, TMLException {
                return TMLAction.defaultActionAttach(context);
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_REFRESH, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws WGException {
                return TMLAction.defaultActionRefresh(context);
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_RESET, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException {
                return TMLAction.defaultActionReset(context);
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_STORE, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws WGException, IOException, WGIllegalStateException {
                return TMLAction.defaultActionStore(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);
        
        action = new DefaultAction(DEFAULTACTION_REMOVE, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException, IOException, WGIllegalStateException {
                return TMLAction.defaultActionRemove(context);
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_SETSESSIONVAR, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException, IOException, WGIllegalStateException {
                return TMLAction.defaultActionSetSessionVar(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_SETVAR, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws WGException, IOException, WGIllegalStateException {
                return TMLAction.defaultActionSetVar(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_SETPVAR, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException, IOException, WGIllegalStateException {
                return TMLAction.defaultActionSetPortletVar(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_SETPSESSIONVAR, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException, IOException, WGIllegalStateException {
                return TMLAction.defaultActionSetPortetSessionVar(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_HDBSTORE, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws WGException, IOException, WGIllegalStateException, TMLScriptException {
                return TMLAction.defaultActionHDBStore(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_INCVAR, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException, IOException, WGIllegalStateException, TMLScriptException {
                return TMLAction.defaultActionIncVar(context, link.getUnnamedParams());
            }
        };
        _defaultActions.put(action.getName(), action);

        action = new DefaultAction(DEFAULTACTION_LOGIN, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws WGException, IOException {
                TMLAction.defaultActionLogin(context, link.getUnnamedParams());
                return null;
            }
        };
        _defaultActions.put(action.getName(), action);
        
        action = new DefaultAction(DEFAULTACTION_FIREEVENT, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException, WGAPIException, IOException, WGIllegalStateException, TMLScriptException, LoginException {
                TMLAction.defaultActionFireEvent(context, link.getUnnamedParams());
                return null;
            }
        };
        _defaultActions.put(action.getName(), action);
        
        action = new DefaultAction(DEFAULTACTION_CALLPCMETHOD, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException ,TMLScriptException ,WGException ,IOException {
                return TMLAction.defaultActionCallPCMethod(action, context , link.getNamedParams(), link.getUnnamedParams(), objects);
            };
        };
        _defaultActions.put(action.getName(), action);
        
        action = new DefaultAction(DEFAULTACTION_CALLMCMETHOD, FLAG_DEBOUNCED) {
            public Object call(TMLAction action, TMLContext context, TMLActionLink link, Map<String, Object> objects) throws TMLException ,TMLScriptException ,WGException ,IOException {
                return TMLAction.defaultActionCallMCMethod(action, context , link.getDefinitionModule(), link.getNamedParams(), link.getUnnamedParams(), objects);
            };
        };
        _defaultActions.put(action.getName(), action);

    }

    public static final String CUSTOMACTION = "custom";

    private boolean _async;

    private boolean _master;

    private String _code;

    private Date _moduleDate;

    private String _ID;
    
    private DesignResourceReference _designReference;
    
    private String _objectName = null;

    public DesignResourceReference getDesignReference() {
        return _designReference;
    }

    public void setDesignReference(DesignResourceReference designReference) {
        _designReference = designReference;
    }

    // private int _sequence;
    private String _type;

    private int _origin;

    private boolean _debounce;

    private int _timeout;
    
    private ObjectStrategy _objectStrategy = ObjectStrategy.SINGLE;

    private Set<String>  _calledSequenceIds = Collections.synchronizedSet(new LinkedHashSet<String>());

    private String _qualifier = null;

    static final String ACTION_XML_PREFIX = "<doc xmlns:tml=\"urn:webtml\">";

    static final String ACTION_XML_SUFFIX = "</doc>";

    public static final String OBJ_ACTION_PARAMS = "actionparams";

    public TMLAction() {
        _debounce = true;
        _timeout = RhinoExpressionEngine.DEFAULT_SCRIPTTIMEOUT;
    }

    public static Object defaultActionCallMCMethod(TMLAction action, TMLContext context, DesignResourceReference definitionModule, Map<String, Object> namedParams, List<Object> unnamedParams, Map<String,Object> objects) throws WGException {
        return new ItemExpression(context.designContext(definitionModule.toString()), "$mc." + action.getQualifier()).retrieve(namedParams, unnamedParams, objects);
    }

    public static Object defaultActionCallPCMethod(TMLAction action, TMLContext context, Map<String,Object> namedParams, List<Object> unnamedParams, Map<String, Object> objects) throws WGException {
        return new ItemExpression(context, "$pc." + action.getQualifier()).retrieve(namedParams, unnamedParams, objects);        
    }
    
    protected static void defaultActionFireEvent(TMLContext context, List<Object> params) throws TMLException, WGAPIException {

        if (context.getportlet() == null) {
            throw new TMLException("Default action $" + DEFAULTACTION_FIREEVENT + " can only be called inside a WebTML portlet", false);
        }
        
        if (params.size() <= 1) {
            throw new TMLException("Default action $" + DEFAULTACTION_FIREEVENT + " needs the name of the portlet event as first parameter", false);
        }
        
        PortletEvent event = context.createevent(String.valueOf(params.get(0)));
        context.getportlet().fireevent(event);
        
    }

    protected static Boolean defaultActionRemove(TMLContext context) throws WGAPIException {

        WGContent content = context.content();
        
        // Move a portlet context positioned on the document to delete to its parent, if available, es to none
        TMLPortlet portlet = context.getportlet();
        if (portlet != null && portlet.getcontext() != null && portlet.getcontext().getpath().equals(context.getpath())) {
            TMLContext parentContext = portlet.getcontext().context("parent", false);
            if (parentContext != null) {
                portlet.setcontext(parentContext);
            }
            else {
                portlet.setcontext(null);
            } 
        }
        
        // Look if we delete via HDBModel
        if (!content.isDummy() && content.getStructEntry().getContentType().getName().equals(WGHierarchicalDatabase.CONTENT_TYPE)) {
            HDBModel model = HDBModel.getModel(content.getDatabase());
            if (model != null && HDBModel.isContent(content)) {
                model.deleteContent(content);
                return true;
            }
        }
        
        // Delete normally
        content.remove();
        return true;
        
    }

    protected static void defaultActionLogin(TMLContext context, List<Object> params) throws WGException, IOException {

        String username = null;
        String password = null;
        String domain = (String) context.meta("db", "domain");
        String redirect = null;
        TMLForm form = context.gettmlform();
        if (form != null) {
            if (form.hasfield("user")) {
                username = (String) form.field("user");
            }
            else if (form.hasfield("username")) {
                username = (String) form.field("username");
            }

            if (form.hasfield("password")) {
                password = (String) form.field("password");
            }
            else if (form.hasfield("pwd")) {
                password = (String) form.field("pwd");
            }

            if (form.hasfield("domain")) {
                domain = (String) form.field("domain");
            }

            if (form.hasfield("redirect")) {
                redirect = (String) form.field("redirect");
            }
        }

        if (username == null && params.size() >= 1 && params.get(0) != null) {
            username = String.valueOf(params.get(0));
        }

        if (password == null && params.size() >= 2 && params.get(1) != null) {
            password = String.valueOf(params.get(1));
        }

        if (domain == null && params.size() >= 3 && params.get(2) != null) {
            domain = String.valueOf(params.get(2));
        }

        if (redirect == null && params.size() >= 4 && params.get(3) != null) {
            redirect = String.valueOf(params.get(3));
        }

        if (context.login(username, password, domain)) {
            if (redirect != null) {
                context.redirectto(redirect);
            }
        }
        else {
            WGA.get(context).tmlPage().setVar("loginerror", 1);
            throw new TMLException("Login failed", false);
        }
    }

    public static Object defaultActionSetSessionVar(TMLContext context, List<Object> params) throws TMLException {

        if (params.size() >= 2 && !WGUtils.isEmpty(params.get(0)) && !WGUtils.isEmpty(params.get(1))) {

            context.setsessionvar(String.valueOf(params.get(0)), params.get(1));
            try {
                return context.item("tmlparam2");
            }
            catch (WGAPIException e) {
                throw new TMLException("Error retrieving item tmlparam2", false);
            }
        
        }
        
        else if (context.gettmlform() != null && context.gettmlform().issubmitted()) {
            
            TMLForm form = context.gettmlform();
            for (String fieldName : form.getfieldnames()) {
                
                Object value;
                FieldReg fieldReg = form.getforminfo().getFieldRegistration(fieldName);
                if (fieldReg != null && fieldReg.isMultiple()) {
                    value = form.fieldlist(fieldName);
                }
                else {
                    value = form.field(fieldName);
                }
                
                context.setsessionvar(fieldName, value);
            }
            
            return null;

            
        }
        
        else {
            throw new TMLException("Default action $" + DEFAULTACTION_SETSESSIONVAR + " needs either 2 action parameters or a submitted WebTML Form", false);
        }
        
    }

    public static Object defaultActionSetPortetSessionVar(TMLContext context, List<Object> params) throws TMLException, WGAPIException {

        if (params.size() >= 2 && !WGUtils.isEmpty(params.get(0)) && !WGUtils.isEmpty(params.get(1))) { 
    
            if (context.getportlet() == null) {
                throw new TMLException("Default action $" + DEFAULTACTION_SETPSESSIONVAR + " can only be called inside a WebTML portlet", false);
            }
    
            context.getportlet().setsessionvar(String.valueOf(params.get(0)), params.get(1));
    
            try {
                return context.item("tmlparam2");
            }
            catch (WGAPIException e) {
                throw new TMLException("Error retrieving item tmlparam2", false);
            }
            
        }
        
        else if (context.gettmlform() != null && context.gettmlform().issubmitted()) {
            
            TMLForm form = context.gettmlform();
            for (String fieldName : form.getfieldnames()) {
                
                Object value;
                FieldReg fieldReg = form.getforminfo().getFieldRegistration(fieldName);
                if (fieldReg != null && fieldReg.isMultiple()) {
                    value = form.fieldlist(fieldName);
                }
                else {
                    value = form.field(fieldName);
                }
                
                context.getportlet().setsessionvar(fieldName, value);
            }
            
            return null;

            
        }
        
        else {
            
            throw new TMLException("Default action $" + DEFAULTACTION_SETPSESSIONVAR + " needs either 2 action parameters or a submitted WebTML Form", false);
            
        }

    }

    public static Object defaultActionSetVar(TMLContext context, List<Object> params) throws WGException {

        if (params.size() >= 2 && !WGUtils.isEmpty(params.get(0)) && !WGUtils.isEmpty(params.get(1))) {

            WGA.get(context).tmlPage().setVar(String.valueOf(params.get(0)), params.get(1));
            try {
                return context.item("tmlparam2");
            }
            catch (WGAPIException e) {
                throw new TMLException("Error retrieving item tmlparam2", false);
            }
            
        }
        
        else if (context.gettmlform() != null && context.gettmlform().issubmitted()) {
            
            TMLForm form = context.gettmlform();
            for (String fieldName : form.getfieldnames()) {
                
                Object value;
                FieldReg fieldReg = form.getforminfo().getFieldRegistration(fieldName);
                if (fieldReg != null && fieldReg.isMultiple()) {
                    value = form.fieldlist(fieldName);
                }
                else {
                    value = form.field(fieldName);
                }
                
                context.setvar(fieldName, value);
            }
            
            return null;

            
        }
        
        else {
            
            throw new TMLException("Default action $" + DEFAULTACTION_SETVAR + " needs either 2 action parameters or a submitted WebTML Form", false);            

        }
    }

    public static Object defaultActionIncVar(TMLContext context, List<Object> params) throws TMLException, WGAPIException {

        if (params.size() < 1) {
            throw new TMLException("Default action $" + DEFAULTACTION_INCVAR + " needs 1 arguments while only " + params.size() + " were provided", false);
        }

        try {
            String varName = String.valueOf(params.get(0));
            Number num = (Number) context.item(varName);
            if (num == null) {
                num = 0;
            }
            num = num.intValue() + 1;
            context.updateOrSetVar(varName, num);
            return num;
        }
        catch (Exception e) {
            throw new TMLException("Error executing default action 'incvar'", e, false);
        }
    }

    public static Object defaultActionSetPortletVar(TMLContext context, List<Object> params) throws TMLException, WGAPIException {

        if (params.size() >= 2 && !WGUtils.isEmpty(params.get(0)) && !WGUtils.isEmpty(params.get(1))) {

            if (context.getportlet() == null) {
                throw new TMLException("Default action $" + DEFAULTACTION_SETPVAR + " can only be called inside a WebTML portlet", false);
            }
    
            context.getportlet().setvar(String.valueOf(params.get(0)), params.get(1));
    
            try {
                return context.item("tmlparam2");
            }
            catch (WGAPIException e) {
                throw new TMLException("Error retrieving item tmlparam2", false);
            }
        
        }
        
        else if (context.gettmlform() != null && context.gettmlform().issubmitted()) {
            
            TMLForm form = context.gettmlform();
            for (String fieldName : form.getfieldnames()) {
                
                Object value;
                FieldReg fieldReg = form.getforminfo().getFieldRegistration(fieldName);
                if (fieldReg != null && fieldReg.isMultiple()) {
                    value = form.fieldlist(fieldName);
                }
                else {
                    value = form.field(fieldName);
                }
                
                context.getportlet().setvar(fieldName, value);
            }
            
            return null;

            
        }
        
        else {
            throw new TMLException("Default action $" + DEFAULTACTION_SETPVAR + " needs either 2 action parameters or a submitted WebTML Form", false);
        }
    }

    public TMLAction(String code, boolean master, boolean async, boolean debounce, int origin) {
        _type = CUSTOMACTION;
        _code = code;
        _master = master;
        _async = async;
        _debounce = debounce;
        _origin = origin;
        _timeout = RhinoExpressionEngine.DEFAULT_SCRIPTTIMEOUT;
    }

    /**
     * Constructor for default actions.
     * 
     * @param defaultActionName
     *            The name of the default action, already stripped of the $ qualifier
     */
    public TMLAction(String defaultActionName) {
        _code = "";
        _master = false;
        _async = false;
        _debounce = true;
        _timeout = RhinoExpressionEngine.DEFAULT_SCRIPTTIMEOUT;
        _origin = ORIGIN_DEFAULT;

        // Split up default action name and qualifier
        _type = defaultActionName;
        int dotPos = _type.indexOf(".");
        if (dotPos != -1) {
            _qualifier  = _type.substring(dotPos + 1);
            _type = _type.substring(0, dotPos);
        }
        
        
        // check for supported action and set
        if (!_defaultActions.containsKey(_type)) {
            throw new IllegalArgumentException("Unsupported defaultaction '" + defaultActionName + "'");
        }

        setID(defaultActionName);
    }
    
    public boolean isDefaultAction() {
        return (!_type.equals(CUSTOMACTION));
    }

    /**
     * Returns the code.
     * 
     * @return String
     */
    public String getCode() {
        return _code;
    }

    /**
     * Returns the key. hashcode from all properties
     * 
     * @return Integer
     */
    public Integer getKey() {
        return new Integer(new HashCodeBuilder().append(_async).append(_master).append(_code).append(_type).append(_qualifier).append(_debounce).append(_timeout).append(
                getModuleDatabase() != null ? getModuleDatabase() : "nodb").toHashCode());
    }

    public static Boolean defaultActionAttach(TMLContext context) throws WGAPIException, IOException, TMLException {

        TMLForm tmlForm = context.gettmlform();

        if (!context.getdocument().getDatabase().getSessionContext().isMasterSession()) {
            TMLAction action = new TMLAction(TMLAction.DEFAULTACTION_ATTACH);
            MasterDefaultAction masterAction = new MasterDefaultAction(context, action, action.createActionLink(Collections.<String,Object>emptyMap(), Collections.emptyList(), context), null);
            masterAction.start();
            context.getdocument().getDatabase().reopenSession();
        }
        else {
            WGDocument doc = context.getdocument();

            tmlForm.pushFiles(doc);

            doc.save();
        }

        return new Boolean(true);
    }

    public static Boolean defaultActionRefresh(TMLContext context) throws WGException {
        
        WGA wga = WGA.get(context);
        List<String> dropProcessIds = wga.call().getParamValues("$reloadProcessContexts");
        dropOverloadedProcessContexts(context, dropProcessIds);
        
        return true;
    }

    private static void dropOverloadedProcessContexts(TMLContext context, List<String> dropProcessIds) {
        if (dropProcessIds.size() == 0) {
            return;
        }
        
        ProcessContextRegistration reg = context.getEnvironment().getProcessContextRegistration();
        TMLPortletStateStorage stateStorage = context.getPortletStateStorage();
        if (!(stateStorage instanceof TMLPortletStateTransientStorage)) {
            return;
        }
        
        TMLPortletStateTransientStorage transientStorage = (TMLPortletStateTransientStorage) stateStorage;
      
        // Drop those process contextes whose portlets were "overloaded", and where no current portlet state was sent for.
        // So these were temporary instances of those portlets, no longer active (#00004513) 
        for (String id : dropProcessIds) {
            
            // Just for sure: We only want to drop portlet PCs
            ProcessContext pc = reg.getProcessContext(id);
            if (!(pc instanceof TMLPortletProcessContext)) {
                continue;
            }
            
            if (!transientStorage.getProcessIdsSentByClient().contains(id)) {
                reg.removeProcessContext(id);
            }
        }
              
    }
    
    /**
     * @param context
     * @throws IOException
     * @throws WGAPIException
     * @throws WGIllegalStateException
     */
    public static Boolean defaultActionStore(TMLContext context, List<Object> params) throws WGException, IOException, WGIllegalStateException {

        try {
            
            if (context.isbrowserinterface()) {
                context.getcontent().getDatabase().getSessionContext().setClient(WGACore.getBIClientString());
            }
            else {
                context.getcontent().getDatabase().getSessionContext().setClient(WGACore.getWebformClientString());
            }

            TMLForm tmlForm = context.gettmlform();
            
            if (tmlForm == null) {
                throw new TMLException("Cannot store form because there is no form", false);
            }
            
            return (tmlForm.store());

        }
        catch (WGCancelledException e) {
            if (context.gettmlform() != null) {
                context.gettmlform().addmessage(e.getMessage());
            }
            return false;
        }
        catch (TMLScriptException e) {
            throw new TMLException("TMLScript exception creating HDB content via $store", e, false);
        }

    }

    public static Boolean defaultActionHDBStore(TMLContext context, List<Object> params) throws WGException, IOException, WGIllegalStateException, TMLScriptException {

        TMLForm tmlForm = context.gettmlform();
        if (tmlForm == null) {
            throw new TMLException("Cannot store form because there is no form", false);
        }
        String formSource = tmlForm.getforminfo().getSource();
        if (WGUtils.isEmpty(formSource)) {
            throw new TMLException("Cannot store form because the form source is empty", false);
        }

        if (context.isbrowserinterface()) {
            context.getcontent().getDatabase().getSessionContext().setClient(WGACore.getBIClientString());
        }
        else {
            context.getcontent().getDatabase().getSessionContext().setClient(WGACore.getWebformClientString());
        }

        boolean result = false;
        if (formSource.equals("content") || formSource.equals("none") || formSource.equals("newcontent")) {
            Object param = null;
            if (params.size() >= 1) {
                param = params.get(0);
            }
            result = tmlForm.storeinhdb(param);
        }
        else {
            throw new TMLException("Cannot store form in HDB because the form source is invalid for this action: " + String.valueOf(formSource), false);
        }

        // There are files to attach
        if (result == true && tmlForm.getfilenames().size() > 0) {
            defaultActionAttach(context);
        }

        return result;

    }

    public static Boolean defaultActionReset(TMLContext context) throws TMLException, WGAPIException {
        
        TMLForm tmlForm = context.gettmlform();
        if (tmlForm == null) {
            throw new TMLException("Cannot reset form because there is no form", false);
        }

        tmlForm.reset();

        return new Boolean(true);

    }

    public boolean isAsync() {
        return _async;
    }

    /**
     * @return
     */
    public boolean isMaster() {
        return _master;
    }

    /**
     * @param b
     */
    public void setAsync(boolean b) {
        _async = b;
    }

    /**
     * @param string
     */
    public void setCode(String string) {
        _code = string;
    }

    /**
     * @param b
     */
    public void setMaster(boolean b) {
        _master = b;
    }

    public TMLActionLink createActionLink(Map<String,Object> namedParams, List<Object> unnamedParams, TMLContext context) throws WGAPIException {

        TMLActionLink actionLink = new TMLActionLink(context.gethttpsession());

        actionLink.setSequenceId(isDebounce() ? UIDGenerator.generateUID() : TMLActionLink.EMPTY_PARAM);
        if (_type.equals(CUSTOMACTION)) {
            actionLink.setActionKey(getKey());
        }
        else {
            actionLink.setDefaultAction(getID());
        }

        // Determine db key and context path
        actionLink.setDbKey((String) context.getdocument().getDatabase().getAttribute(WGACore.DBATTRIB_DBKEY));
        if (!context.getdocument().isTemporary()) {
            actionLink.setContextPath(context.getpath());
        }

        // Determine portlet key, if present
        TMLPortlet portlet = context.getportlet();
        if (portlet != null) {
            actionLink.setPortletKey(portlet.getportletkey());
        }

        // Determine WebTML scope
        String scope = (String) context.option(Base.OPTION_WEBTML_SCOPE);
        if (scope != null) {
            actionLink.setWebtmlScope(scope);
        }

        // Set action params
        if (unnamedParams != null) {
            actionLink.getUnnamedParams().addAll(unnamedParams);
        }
        if (namedParams != null) {
            actionLink.getNamedParams().putAll(namedParams);
        }
        
        // Set definition module
        actionLink.setDefinitionModule(context.getDesignContext().getBaseReference());
        
        return actionLink;
    }

    public String getType() {
        return _type;
    }

    public void setType(String type) {
        _type = type;
    }

    public boolean isDebounce() {
        return _debounce;
    }

    public void setDebounce(boolean debounce) {
        _debounce = debounce;
    }

    public int getTimeout() {
        return _timeout;
    }

    public void setTimeout(int timeout) {
        _timeout = timeout;
    }

    public boolean sequenceIdAlreadyUsed(String sequenceId) {
        return _calledSequenceIds.contains(sequenceId);
    }

    public void calledWithSequenceId(String sequenceId) {
        // add sequence Id to set
        _calledSequenceIds.add(sequenceId);
        if (_calledSequenceIds.size() > 1000) {
            // if size exceeded remove first
            synchronized (_calledSequenceIds) {
                Iterator<String> it = _calledSequenceIds.iterator();
                it.next();
                it.remove();
            }
        }
    }

    public static DefaultAction getDefaultAction(TMLAction action) {
        return (DefaultAction) _defaultActions.get(action.getType());
    }

    /**
     * @return Returns the moduleName.
     */
    public String getModuleName() {
        DesignResourceReference ref = getDesignReference();
        if (ref != null) {
            return ref.getResourceName();
        }
        else {
            return null;
        }
    }

    /**
     * @return Returns the moduleDatabase.
     */
    public String getModuleDatabase() {
        DesignResourceReference ref = getDesignReference();
        if (ref != null) {
            return ref.getDesignApp();
        }
        else {
            return null;
        }
    }


    private WGCSSJSModule retrieveModule(TMLContext context) throws WGException {
        WGDatabase db = context.db(getModuleDatabase());
        if (db == null) {
            throw new WGException("Database " + getModuleDatabase() + " not found");
        }

        if (!db.isSessionOpen()) {
            throw new WGException("User has no access to database " + getModuleDatabase());
        }

        WGCSSJSModule mod = db.getCSSJSModule(getModuleName(), WGScriptModule.CODETYPE_TMLSCRIPT);
        // Module not available or was deleted. We just return null.
        if (mod == null) {
            return null;
        }

        if (!mod.getCodeType().equals(WGCSSJSModule.CODETYPE_TMLSCRIPT)) {
            throw new WGException("Script module '" + getModuleName() + "' in database " + getModuleDatabase() + " is no TMLScript module");
        }
        return mod;
    }

    /**
     * @return Returns the iD.
     */
    public String getID() {
        return _ID;
    }

    /**
     * @param id
     *            The iD to set.
     */
    public void setID(String id) {
        _ID = id;
    }

    /**
     * @return Returns the moduleDate.
     */
    public Date getModuleDate() {
        return _moduleDate;
    }

    /**
     * @param moduleDate
     *            The moduleDate to set.
     */
    public void setModuleDate(Date moduleDate) {
        _moduleDate = moduleDate;
    }

    /**
     * Tests if this action definition is up-to-date. This only applies to
     * actions that base on TMLScript modules, which might have been updated.
     * 
     * @param context
     *            A context that this method needs to test for up-to-date-ness
     * @return true, if the action definition is up-to-date, false (surprise) if
     *         not
     */
    public boolean upToDate(TMLContext context) {

        if (_origin != ORIGIN_SCRIPT_MODULE) {
            return true;
        }

        try {
            WGCSSJSModule mod = retrieveModule(context);
            if (mod != null) {
                return mod.getLastModified().equals(getModuleDate());
            }
            else {
                return false;
            }
        }
        catch (WGException e) {
            context.getlog().error("Error testing module action up-to-date", e);
            return false;
        }

    }

    public static TMLAction buildActionFromScriptModule(WGCSSJSModule mod) throws TMLActionException, WGAPIException {
        return buildActionFromScriptModule(mod, ObjectStrategy.SINGLE);
    }
    
    public static TMLAction buildActionFromScriptModule(WGCSSJSModule mod, ObjectStrategy defaultObjectStrategy) throws TMLActionException, WGAPIException {

        String code = mod.getCode().trim();
        TMLAction action = null;

        // See if code is surrounded by <tml:action> Tag. Parse it out to get
        // flags
        if (code.startsWith("<tml:action")) {
            List<String> codelines = WGUtils.deserializeCollection(code, "\n");
            if (codelines.size() < 2) {
                throw new TMLActionException(
                        "Too few lines for action script module. A script module that begins with <tml:action> must a) have this tag on a separate line and b) end with </tml:action> on a separate line");
            }

            // Take first and last line of code and try to parse them as XML
            String actionXML = ACTION_XML_PREFIX + ((String) codelines.get(0)) + ((String) codelines.get(codelines.size() - 1)) + ACTION_XML_SUFFIX;
            String actionCode = "";
            if (codelines.size() >= 3) {
                actionCode = WGUtils.serializeCollection(codelines.subList(1, codelines.size() - 1), "\n");
            }

            // Parse out action flags
            boolean master = false;
            boolean async = false;
            boolean debounce = true;
            int timeout = RhinoExpressionEngine.DEFAULT_SCRIPTTIMEOUT;
            try {

                Document actionDOM = DocumentHelper.parseText(actionXML);
                Element actionElement = actionDOM.getRootElement().element(new QName("action", new Namespace("tml", "urn:webtml")));
                master = WGUtils.stringToBoolean(actionElement.attributeValue("master", "false"));
                async = WGUtils.stringToBoolean(actionElement.attributeValue("async", "false"));
                debounce = WGUtils.stringToBoolean(actionElement.attributeValue("debounce", "true"));
                try {
                    timeout = Integer.parseInt(actionElement.attributeValue("timeout", new Integer(RhinoExpressionEngine.DEFAULT_SCRIPTTIMEOUT).toString()));
                }
                catch (NumberFormatException e) {
                    throw new TMLActionException("Unparseable action timeout: " + actionElement.attributeValue("timeout"));
                }
                action = new TMLAction(actionCode, master, async, debounce, ORIGIN_SCRIPT_MODULE);
                action.setTimeout(timeout);
            }
            catch (DocumentException e) {
                throw new TMLActionException("Unable to build action from script module", e);
            }
        }

        // No tag surrounds the action code. Take the code unmodified
        else {
            action = new TMLAction(code, false, false, true, ORIGIN_SCRIPT_MODULE);
        }

        if (action != null) {
            action.setDesignReference(new DesignResourceReference(mod));
            action.setModuleDate(mod.getLastModified());
            action.setID(action.getModuleDatabase() + "/" + action.getModuleName());
            
            // Determine object strategy: May be modified by strategy determined on module.
            ObjectStrategy objectStrategy = defaultObjectStrategy;
            String determinedObjectStrategyStr = (String) mod.getExtensionData(RhinoExpressionEngine.EXTDATA_OBJECTSTRATEGY);
            if (determinedObjectStrategyStr != null) {
                objectStrategy = ObjectStrategy.valueOf(determinedObjectStrategyStr);
            }
            action.setObjectStrategy(objectStrategy);
            
            // Determine name of eventually included object from original filename
            String sourceFileName = mod.getSourceFileName();
            if (sourceFileName != null) {
                String objectName = DesignDirectory.getTMLScriptObjectName(sourceFileName);
                action.setObjectName(objectName);
            }
            
            
        }
        return action;

    }


    public int getOrigin() {
        return _origin;
    }

    public String getDescription() {

        if (isDefaultAction()) {
            return "WebTML Default Action " + getID();
        }
        if (getOrigin() == TMLAction.ORIGIN_SCRIPT_MODULE) {
            return "TMLScript-Module " + getModuleDatabase() + "/" + getModuleName();
        }
        else if (getID() != null) {
            return "WebTML Action " + getID() + " from WebTML-Module " + getModuleDatabase() + "/" + getModuleName();
        }
        else {
            return "Anonymous WebTML-Action from WebTML-Module " + getModuleDatabase() + "/" + getModuleName();
        }

    }
    
    public TMLDesignContext createContextDelegate(TMLContext context, TMLDesignContext baseContext) throws WGException {
        return createDesignContext(context, baseContext);
    }

    public TMLDesignContext createDesignContext(TMLContext context, TMLDesignContext baseContext) throws WGException {
        
        // For default dbs just return the original design context
        if (getOrigin() == TMLAction.ORIGIN_DEFAULT) {
            return baseContext;
        }
        
        WGDatabase moduleDB = context.db();
        if (getModuleDatabase() != null && !getModuleDatabase().equals(moduleDB.getDbReference())) {
            moduleDB = context.db(getModuleDatabase());
        }
        
        // Page/Inline actions use their originating WebTML module as base reference (#00001443)
        if (getOrigin() == TMLAction.ORIGIN_TML) {
            return baseContext.createContextDelegate(moduleDB, getModuleName());
        }
        
        // Module actions use their module
        else {
            return baseContext.createContextDelegate(moduleDB, getID());
        }
        
    }

    public ObjectStrategy getObjectStrategy() {
        return _objectStrategy;
    }

    public void setObjectStrategy(ObjectStrategy objectStrategy) {
        _objectStrategy = objectStrategy;
    }

    public String getQualifier() {
        return _qualifier;
    }

    public void setQualifier(String qualifier) {
        _qualifier = qualifier;
    }

    public String getObjectName() {
        return _objectName;
    }

    public void setObjectName(String objectName) {
        _objectName = objectName;
    }



}
