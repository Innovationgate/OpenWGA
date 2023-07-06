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

package de.innovationgate.wgpublisher.webtml.env;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.jsp.PageContext;

import org.apache.log4j.Logger;

import de.innovationgate.utils.TransientObjectWrapper;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGADomain;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.WGPRequestPath;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextEnvironment;
import de.innovationgate.wgpublisher.webtml.utils.TMLOption;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;
import de.innovationgate.wgpublisher.webtml.utils.Warning;

public class IndependentTMLScriptEnvironment implements TMLContextEnvironment {
    
    private WGDatabase _mainDesignDB;
    private WGAURLBuilder _urlBuilder;
    private boolean _userProfileBelongsToDomain;
    private Logger _log;
    private Map<String, TMLForm> _persistentForms;
    private Map<String, TMLForm> _transientForms;
    private Map<String, TMLOption> _options = new HashMap<String, TMLOption>();
    private HttpSession _session;
    private Map<String,TMLAction> _actionRegistration;
    private Map<String,Object> _vars = new HashMap<String,Object>();
    private Map<String,TransientObjectWrapper<Object>> _sessionVars;
    private TMLContext _mainContext = null;
    private ProcessContextRegistration _processContextRegistration;
    private List<Warning> _warnings;
    public HttpServletRequest _request = null;
    public HttpServletResponse _response = null;
    
    private TMLForm _form;
    private TMLUserProfile _userProfile = null;
    private WGACore _core = null;
    private RootEnvironmentUserData _rootEnvironmentUserData = null;
    
    public IndependentTMLScriptEnvironment(TMLContext mainContext, WGACore core, TMLUserProfile userProfile, TMLForm form, HttpServletRequest req, HttpServletResponse rsp, HttpSession session) {
        _mainContext = mainContext;
        _core = core;
        _userProfile = userProfile;
        if (_userProfile != null) {
            _userProfileBelongsToDomain = core.getPersonalisationdbs().values().contains(_userProfile.getprofile().getDatabase());
        }
        
        _form = form;
        _mainDesignDB = mainContext.db();
        _request = req;
        _response = rsp;
        _session = session;
        _urlBuilder = core.retrieveURLBuilder(null, mainContext.db());
        _log = core.getLog();
        
        if (req != null) {
            _warnings = TMLContext.fetchWarnings(req);
            _transientForms = TMLContext.fetchTransientForms(req);
        }
        else {
            _warnings = new ArrayList<Warning>();
            _transientForms =  new HashMap<String, TMLForm>();
        }
        
        if (session != null) {
            _sessionVars = TMLContext.fetchSessionVars(session);
            _processContextRegistration = TMLContext.fetchProcessContextRegistration(session);
            _actionRegistration = TMLContext.fetchActionRegistration(session);
            _persistentForms = TMLContext.fetchPersistentForms(session);
        }
        else {
            _sessionVars = new HashMap<String, TransientObjectWrapper<Object>>();
            _processContextRegistration = new ProcessContextRegistration();
            _actionRegistration = new HashMap<String, TMLAction>();
            _persistentForms = new HashMap<String, TMLForm>();
        }
        
        if (form != null) {
            setForm(form);
        }
        
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getMainContext()
     */
    public TMLContext getMainContext() {
        return _mainContext;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getActionRegistration()
     */
    public Map<String,TMLAction> getActionRegistration() {
        return _actionRegistration;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getWarnings()
     */
    public List<Warning> getWarnings() {
        return _warnings;
    }

    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getSessionVars()
     */
    public Map<String,TransientObjectWrapper<Object>> getSessionVars() {
        return _sessionVars;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getVars()
     */
    public Map<String,Object> getPageVars() {
        return _vars;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#importSettings(de.innovationgate.wgpublisher.webtml.utils.TMLContext)
     */
    @SuppressWarnings("unchecked")
    public void importEnvironmentData(TMLContext context, boolean includeIntrusiveData) {
        
        _processContextRegistration = context.getEnvironment().getProcessContextRegistration();
        _warnings = context.getEnvironment().getWarnings();
        
        if (includeIntrusiveData) {
            _vars = context.getEnvironment().getPageVars();
            _sessionVars = context.getEnvironment().getSessionVars();
            _actionRegistration = context.getEnvironment().getActionRegistration();
            
        }
        
        if (context.getEnvironment().getRootEnvironmentUserData() != null) {
            _rootEnvironmentUserData = context.getEnvironment().getRootEnvironmentUserData();
        }
        else {
            try {
                WGDatabase theDB = context.db(getMainContext().getdocument().getDatabase().getDbReference());
                // We try to take the root environent user data of our main context db. If the root user has no access to it we take the parent context dbs data instead. 
                if (theDB != null && theDB.isSessionOpen()) {
                    _rootEnvironmentUserData = new RootEnvironmentUserData(theDB.getDbReference(), theDB.getSessionContext().getUserAccess());
                }
                else if (context.db().isSessionOpen()) {
                    _rootEnvironmentUserData = new RootEnvironmentUserData(context.db().getDbReference(), context.db().getSessionContext().getUserAccess());
                }
                
                if (_rootEnvironmentUserData != null && context.iswebenvironment()) {
                    _rootEnvironmentUserData.setLocales(Collections.list(context.getrequest().getLocales()));
                }
                
                // We have no parent environment to import. Root environment data remains empty.
            }
            catch (WGException e) {
                getLog().error("Exception importing root environment user data", e);
            }
        }
    }
    
    
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getTMLContextForDocument(de.innovationgate.webgate.api.WGDocument)
     */
    public TMLContext getTMLContextForDocument(TMLContext parentContext, WGDocument doc) {
        TMLContext tmlContext = new TMLContext(doc, _core, _userProfile, _form, _request, _response, _session, parentContext);
        tmlContext.setLastError(null);
        return tmlContext;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getForm()
     */
    public TMLForm getForm() {
        return _form;
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getUserProfile()
     */
    public TMLUserProfile getUserProfile(WGDatabase db) {
        
        if (_request != null && _response != null) {
            return TMLContext.fetchUserProfile(getCore(), db, getRequest(), getResponse());
        }
        
        if (_userProfile == null) {
            return null;
        }
        
        // We only want to serve the profile when stored profile belongs to the given database (#00001781)
        if (_userProfileBelongsToDomain) {
            WGADomain targetDomain = _core.getDomainForDatabase(db);
            WGADomain profileDomain = _core.getDomainForDatabase(_userProfile.getprofile().getDatabase());
            if (targetDomain != null && profileDomain != null && targetDomain.getName().equals(profileDomain.getName())) {
                return _userProfile;
            }
            else {
                return null;
            }
        }
        else {
            WGDatabase persDB = (WGDatabase) db.getAttribute(WGACore.DBATTRIB_EXTERNAL_SELF_PERSONALISATION_DB);
            if (persDB == null) {
                persDB = db;
            }
            if (persDB.getDbReference().equals(_userProfile.getprofile().getDatabase().getDbReference())) {
                return _userProfile;
            }
            else {
                return null;
            }
            
        }
        
        
    }
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#getCore()
     */
    public WGACore getCore() {
        return _core;
    }
    /* (non-Javadoc)
     * @see de.innovationgate.wgpublisher.webtml.utils.TMLScriptEnvironment#setForm(de.innovationgate.wgpublisher.webtml.utils.TMLForm)
     */
    public void setForm(TMLForm form) {
        this._form = form;
        if (form.getforminfo().isPersistent()) {
            getPersistentForms().put(form.getformid(), form);
        }
        else {
            getTransientForms().put(form.getformid(), form);
        }
    }
    public boolean isPageContextAvailable() {
        return false;
    }
    public boolean isWebEnvironment() {
        return (_request != null && _response != null);
    }
    public Map<String,TMLForm> getPersistentForms() {
        return _persistentForms;
    }
    public WGDatabase getMainDesignDB() {
        return _mainDesignDB;
    }
    public Map<String,TMLForm> getTransientForms() {
        return _transientForms;
    }

    public WGDatabase openDB(WGDatabase db) throws WGException {
        if (_request != null) {
            return _core.openContentDB(db, _request, false);
        }
        
    	boolean useMaster = false;
    	WGDatabase hintDB = _mainContext.getdocument().getDatabase();        	
    	if (hintDB != null && hintDB.getSessionContext() != null) {
    		useMaster = hintDB.getSessionContext().isMasterSession();
    	}
        return _core.openContentDB(db, null, _session, useMaster, hintDB);
    }
    
    public WGDatabase fetchDB(String dbKey) {
        
        if (dbKey == null) {
            return null;
        }
        
        // Special behaviour for setup scripts. The main design db may not yet be registered at wgacore at this time
        if (dbKey != null && dbKey.equals(getMainDesignDB().getDbReference())) {
            return getMainDesignDB();
        }
        
        return (WGDatabase) _core.getContentdbs().get(dbKey);
    }
    public ProcessContextRegistration getProcessContextRegistration() {
        return _processContextRegistration;
    }
    public RootEnvironmentUserData getRootEnvironmentUserData() {
        return _rootEnvironmentUserData;
    }
    
	public void removeForm(String id) {
	    
	    TMLForm form = (TMLForm) getTransientForms().get(id);
        if (form != null) {
            try {
                form.reset();
            }
            catch (WGAPIException e) {
            }
            getTransientForms().remove(id);
        }
        form = (TMLForm) getPersistentForms().get(id);
        if (form != null) {
            try {
                form.reset();
            }
            catch (WGAPIException e) {
            }
            getPersistentForms().remove(id);
        }
	    
		if (_form != null && _form.getformid().equals(id)) {
			try {
                _form.reset();
            }
            catch (WGAPIException e) {
            }
			_form = null;
		}		
		
	}
    public PageContext getPageContext() {
        return null;
    }
    
    public String getPublisherURL() {
        
        if (_request != null) {
            WGPRequestPath path = (WGPRequestPath) _request.getAttribute(WGACore.ATTRIB_REQUESTPATH);
            if (path != null) {
                return path.getPublisherURL();
            }
            else {
                return WGPDispatcher.getPublisherURL(_request);
            }
        }
        
        WGAConfiguration config = _core.getWgaConfiguration();
        return config.getRootURL();
    }
    @Override
    public HttpServletRequest getRequest() {
        return _request;
    }
    @Override
    public HttpServletResponse getResponse() {
        return _response;
    }
    
    @Override
    public HttpSession getSession() {
        return _session;
    }
    
    @Override
    public WGAURLBuilder getURLBuilder() {
        return _urlBuilder;
    }
    public void setLog(Logger log) {
        _log = log;
    }
    @Override
    public Logger getLog() {
        return _log;
    }
    protected Map<String, TMLOption> getOptions() {
        return _options;
    }

    
}