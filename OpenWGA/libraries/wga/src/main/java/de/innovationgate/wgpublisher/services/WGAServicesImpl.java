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
package de.innovationgate.wgpublisher.services;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCreationException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGExpressionException;
import de.innovationgate.webgate.api.auth.AuthenticationException;
import de.innovationgate.wga.common.beans.csconfig.v1.CSConfig;
import de.innovationgate.wga.common.beans.csconfig.v1.RemoteAction;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgaservices.WGAServiceException;
import de.innovationgate.wgaservices.WGAServices;
import de.innovationgate.wgaservices.types.ActionResult;
import de.innovationgate.wgaservices.types.Form;
import de.innovationgate.wgaservices.types.RemoteSession;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.TMLFormInfo;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class WGAServicesImpl implements WGAServices {
	
	public static final String SCRIPTOBJ_WGASERVICESCONTEXT = "wgaservicescontext";
    protected WGACore _core;
    private WGAWebServicesContextProvider _contextProvider;

	public WGAServicesImpl(WGACore core, WGAWebServicesContextProvider contextProvider) {
		_core = core;
		_contextProvider = contextProvider;
	}


	public RemoteSession login(String domain, String user, String pwd) throws WGAServiceException {
		WGAConfiguration config = _core.getWgaConfiguration();

        WGADomain domainConfig = _core.getDomains(domain);
        if (domainConfig == null) {
            throw new WGAServiceException("Unknown domain '" + domain + "'");
        }
        
        if (domainConfig.getAuthModule() != null) {
            try {
                if (_core.getBruteForceLoginBlocker().login(domainConfig, user, pwd) == null) {
                    throw new WGAServiceException("Invalid login");   
                }
            }
            catch (AuthenticationException e) {
                throw new WGAServiceException("Authentication exception logging into domain '" + domain + "': " + e.getMessage());
            }
        }
        
        return new RemoteSession(domainConfig.getName(), user, pwd);
	}
	
	public ActionResult callAction(RemoteSession session, String dbKey, String actionID, String executionContext, List<Object> params, Form form) throws WGAServiceException {
        try {
            // Open db
            WGDatabase db = retrieveAndOpenDB(session, dbKey);
            
            // Retrieve remote action definition
            RemoteAction actionDef = _core.getSystemContainerManager().getRemoteAction(db, actionID);
            if (actionDef == null) {
                throw new WGAServiceException("Undefined remote action: " + actionID);
            }
            
            // Test permissions
            if (!mayCallAction(db, actionDef)) {
                throw new WGAServiceException("You are not permitted to call remote action '" + actionID + "'");
            }
            
            // Get root content and action
            TMLContext context = new TMLContext(db.getDummyContent(db.getDefaultLanguage()), _core, null, null);
            context.makeThreadMainContext();
            try {
                TMLAction action = context.getActionByID(actionDef.getModuleName(), null);
                if (action == null) {
                    throw new WGAServiceException("Remote action " + actionID + " is defined but does not exist");
                }
                
                // Calculate execution context
                if (executionContext != null) {
                    context = context.context(executionContext, false);
                    if (context == null) {
                        throw new WGAServiceException("Unretrievable execution context: " + executionContext);
                    }
                }
                
                // Create TMLForm, if form information was issued
                if (form != null) {
                    TMLFormInfo formInfo = new TMLFormInfo(form.getId(), TMLFormInfo.HTMLINPUT_FALSE, false, context.getDesignContext().getVersionCompliance());
                    formInfo.setSource("none");
                    formInfo.setKeepOnValidate(false);
                    de.innovationgate.wgpublisher.webtml.form.TMLForm tmlForm = context.createform(formInfo);
                    tmlForm.importServicesForm(form);
                }
                
                // Create map of parent scope objects
                Map<String, Object> parentScopeObjects = new HashMap<String, Object>();
                parentScopeObjects.put(SCRIPTOBJ_WGASERVICESCONTEXT, _contextProvider);
                
                // Call action
                ExpressionResult expressionResult = context.callCustomAction(action, action.createActionLink(null, params, context), parentScopeObjects);
                if (expressionResult.isError()) {
                    WGExpressionException ex = expressionResult.getException();
                    
                    // User defined TMLScript exception should be passed on unmodified
                    if (ex.getCause() instanceof TMLScriptException) {
                        throw ex.getCause();
                    }
                    else {
                        throw ex;    
                    }
                    
                }
                
                
                // If action result is an TMLForm, transform it back into a services form
                Object result = expressionResult.getResult();
                if (result instanceof de.innovationgate.wgpublisher.webtml.form.TMLForm) {
                    de.innovationgate.wgpublisher.webtml.form.TMLForm tmlForm = (de.innovationgate.wgpublisher.webtml.form.TMLForm) result;
                    Form servicesForm = tmlForm.exportServicesForm();
                    ActionResult actionResult = new ActionResult();
                    actionResult.setForm(servicesForm);
                    return actionResult;
                } else {
                	ActionResult actionResult = new ActionResult();
                	actionResult.setNativeResult(result);
                	return actionResult;
                }
            }
            finally {
                TMLContext.clearThreadMainContext();
            }
        }
        catch (TMLScriptException e) {
            throw new WGAServiceException(e.getMessage());
        }
        catch (WGCreationException e) {
            throw new WGAServiceException("Error creating context for action: " + e.getMessage());
        }
        catch (Throwable e) {
        	_core.getLog().error("Exception executing remote action '" + actionID + "'", e);
        	if (e instanceof WGAServiceException) {
        		throw (WGAServiceException) e;
        	} else {            
        		throw new WGAServiceException("Error executing action: " + e.getClass().getName() + " - " + e.getMessage());
        	}
        }
	}
	
    protected WGDatabase retrieveAndOpenDB(RemoteSession session, String dbkey) throws WGAServiceException {
        try {
            WGDatabase db = (WGDatabase) _core.getContentdbs().get(dbkey);
            if (db == null) {
            	throw new WGAServiceException("No database of key '" + dbkey + "'");
            }
            
            // check client access to db
            if (!_core.isClientPermitted(db, getRequest())) {
                throw new WGAServiceException("Client '" + (getRequest()).getRemoteAddr() + " is not permitted to access db '" + db.getDbReference() + "'.");
            }
            
            // Normal db user login
            if (session.getDomain() != null) {
               
                WGADomain domainConfig = _core.getDomainForDatabase(db);
                if (!domainConfig.getName().equals(session.getDomain())) {
                    throw new WGAServiceException("The database '" + db.getDbReference() + "' is not in domain '" + session.getDomain() + "'");
                }
                
                if (_core.getBruteForceLoginBlocker().login(db, session.getUsername(), session.getPassword()) <= WGDatabase.ACCESSLEVEL_NOACCESS) {
                    throw new WGAServiceException("Login is invalid or no access to database '" + dbkey + "'");
                }
                
                if (!db.getSessionContext().getUserAccess().mayAccessDirectly()) {
                    throw new WGAServiceException("You are not allowed to access the application directly");
                }
                
                return _core.prepareDB(db, null);    
            }
            
            // Administrative login
            else {
                
                HttpServletRequest request = getRequest();
                if (!_core.isAdministrativePort(request.getLocalPort())) {
                    throw new WGAServiceException("Administrative services are not enabled");
                }
               
                if (!_core.isAdminLogin(session.getUsername(), session.getPassword(), request)) {
                    throw new WGAServiceException("Administrative login is invalid");
                }
                
                if (!db.isSessionOpen()) {
                	db.openSession();
                }
                return _core.prepareDB(db, null);
            }
            
            
        }
        catch (WGAPIException e) {
            throw new WGAServiceException("Error retrieveAndOpenDB(). Exception: '" + e.getClass().getName() + "' message: '" + e.getMessage() + "'.");
        }
    }
    
    protected boolean mayCallAction(WGDatabase db, RemoteAction action) throws WGAPIException {
        
        if (db.isMemberOfUserList(action.getCallers())) {
            return true;
        }
        
        if (db.getSessionContext().getAccessLevel() >= action.getCallerLevel()) {
            return true;
        }
        
        return false;  
    }

	protected HttpServletRequest getRequest() {
		return _contextProvider.getRequest();
	}

	public int getAccessLevel(RemoteSession session, String dbKey) throws WGAServiceException {
        WGDatabase db = retrieveAndOpenDB(session, dbKey);
        if (db.isSessionOpen()) {
            return db.getSessionContext().getAccessLevel();
        }
        else {
            return WGDatabase.ACCESSLEVEL_NOACCESS;
        }
	}

}
