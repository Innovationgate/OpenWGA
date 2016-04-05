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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletRequest;
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
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;
import de.innovationgate.wgpublisher.webtml.utils.ProcessContextRegistration;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLContextEnvironment;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;
import de.innovationgate.wgpublisher.webtml.utils.Warning;

public class WebTMLContextEnvironment implements TMLContextEnvironment {
    
    private TMLContext _mainContext;
    private Map<String,TransientObjectWrapper<Object>> _sessionVars;
    private Map<String,Object> _vars = new HashMap<String,Object>();
    private Map<String,TMLAction> _actionRegistration;
    private PageContext _pageContext;
    private WGACore _core;
    private WGDatabase _mainDesignDB;
    

    public WebTMLContextEnvironment(TMLContext mainContext, PageContext pageContext) {
        _mainContext = mainContext;
        _pageContext = pageContext;
        _core = WGACore.retrieve(pageContext);
        _mainDesignDB = mainContext.db();
        
        // Retrieve session vars
        _sessionVars = TMLContext.fetchSessionVars(_pageContext.getSession());
        _actionRegistration = TMLContext.fetchActionRegistration(_pageContext.getSession());
    }

    public Map<String,TMLAction> getActionRegistration() {
        return _actionRegistration;
    }

    public WGACore getCore() {
        return _core;
    }

    public TMLForm getForm() {
        
        ServletRequest request = _pageContext.getRequest();
        if (request == null) {
            return null;
        }
        
        TMLForm form = (TMLForm) request.getAttribute(WGACore.ATTRIB_LASTFORM);
        return form;
    }

    public TMLContext getMainContext() {
        return _mainContext;
    }

    public Map<String,TransientObjectWrapper<Object>> getSessionVars() {
        return _sessionVars;
    }

    public TMLContext getTMLContextForDocument(TMLContext parentContext, WGDocument doc) {
        
        if (doc == null) {
            return null;
        }

        TMLContext tmlContext = new TMLContext(doc, parentContext);
        tmlContext.setLastError(null);
        return tmlContext; 
    }

    public TMLUserProfile getUserProfile(WGDatabase db) {
        return TMLContext.fetchUserProfile(getCore(), db, getRequest(), getResponse());
    }

    public Map<String,Object> getPageVars() {
        return _vars;
    }

    public List<Warning> getWarnings() {
        return TMLContext.fetchWarnings(getRequest());        
    }

    public void importEnvironmentData(TMLContext context, boolean includeIntrusiveData) throws TMLException {

        // We only import vars as everything else comes from the HTTP session and JSP environment
        _vars = context.getEnvironment().getPageVars();
        
    }

    public void setForm(TMLForm form) {
        if (form.getforminfo().isPersistent()) {
            getPersistentForms().put(form.getformid(), form);
        }
        else {
            getTransientForms().put(form.getformid(), form);
        }
    }
    
    public Map<String,TMLForm> getPersistentForms() {
        return TMLContext.fetchPersistentForms(getRequest().getSession());
    }
    
    public Map<String,TMLForm> getTransientForms() {
        return TMLContext.fetchTransientForms(getRequest());
    }

    public boolean isPageContextAvailable() {
        return true;
    }

    public boolean isWebEnvironment() {
        return true;
    }

    public WGDatabase getMainDesignDB() {
        return _mainDesignDB;
    }
    
    public WGDatabase openDB(WGDatabase db) throws WGException {
        return _core.openContentDB(db, (HttpServletRequest) _pageContext.getRequest(), false);
    }
    
    public WGDatabase fetchDB(String dbKey) {
        if (dbKey != null) {
            return (WGDatabase) _core.getContentdbs().get(dbKey);
        }
        else {
            return null;
        }
    }

    public ProcessContextRegistration getProcessContextRegistration() {
        return TMLContext.fetchProcessContextRegistration(getSession());
    }

    public RootEnvironmentUserData getRootEnvironmentUserData() {
        return null;
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
	}

    public PageContext getPageContext() {
        return _pageContext;
    }

    public String getPublisherURL() {
        return _pageContext.getRequest().getAttribute(WGACore.ATTRIB_WGPPATH).toString();
    }

    @Override
    public HttpServletRequest getRequest() {
        return (HttpServletRequest) getPageContext().getRequest();
    }

    @Override
    public HttpServletResponse getResponse() {
        return (HttpServletResponse) getPageContext().getResponse();
    }
    
    @Override
    public HttpSession getSession() {
        return getRequest().getSession();
    }

    @Override
    public WGAURLBuilder getURLBuilder() {
        return (WGAURLBuilder) getRequest().getAttribute(WGACore.ATTRIB_URLBUILDER);
    }

    @Override
    public Logger getLog() {
        return _core.getLog();
    }

    @Override
    public void setLog(Logger log) {
    }


}