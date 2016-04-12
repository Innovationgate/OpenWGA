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

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.jsp.PageContext;

import org.apache.log4j.Logger;

import de.innovationgate.utils.TransientObjectWrapper;
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
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;
import de.innovationgate.wgpublisher.webtml.utils.Warning;


/**
 * Version of TMLScript environment which isolates the request resources
 */
public class IsolatedTMLContextEnvironment implements TMLContextEnvironment {

    private IndependentTMLScriptEnvironment _parentEnvironment;

    public IndependentTMLScriptEnvironment getParentEnvironment() {
        return _parentEnvironment;
    }

    public int hashCode() {
        return _parentEnvironment.hashCode();
    }

    public boolean equals(Object obj) {
        return _parentEnvironment.equals(obj);
    }

    public TMLContext getMainContext() {
        return _parentEnvironment.getMainContext().toIsolatedVersion();
    }

    public Map<String, TMLAction> getActionRegistration() {
        return _parentEnvironment.getActionRegistration();
    }

    public List<Warning> getWarnings() {
        return _parentEnvironment.getWarnings();
    }

    public Map<String, TransientObjectWrapper<Object>> getSessionVars() {
        return _parentEnvironment.getSessionVars();
    }

    public Map<String, Object> getPageVars() {
        return _parentEnvironment.getPageVars();
    }

    public void importEnvironmentData(TMLContext context, boolean includeIntrusiveData) {
        _parentEnvironment.importEnvironmentData(context, includeIntrusiveData);
    }

    public TMLContext getTMLContextForDocument(TMLContext parentContext, WGDocument doc) {
        return _parentEnvironment.getTMLContextForDocument(parentContext, doc).toIsolatedVersion();
    }

    public String toString() {
        return _parentEnvironment.toString();
    }

    public TMLForm getForm() {
        return _parentEnvironment.getForm();
    }

    public TMLUserProfile getUserProfile(WGDatabase db) {
        return _parentEnvironment.getUserProfile(db);
    }

    public WGACore getCore() {
        return _parentEnvironment.getCore();
    }

    public void setForm(TMLForm form) {
        _parentEnvironment.setForm(form);
    }

    public boolean isPageContextAvailable() {
        return _parentEnvironment.isPageContextAvailable();
    }

    public boolean isWebEnvironment() {
        return false;
    }

    public Map<String, TMLForm> getPersistentForms() {
        return _parentEnvironment.getPersistentForms();
    }

    public WGDatabase getMainDesignDB() {
        return _parentEnvironment.getMainDesignDB();
    }

    public Map<String, TMLForm> getTransientForms() {
        return _parentEnvironment.getTransientForms();
    }

    public WGDatabase openDB(WGDatabase db) throws WGException {
        return _parentEnvironment.openDB(db);
    }

    public WGDatabase fetchDB(String dbKey) {
        return _parentEnvironment.fetchDB(dbKey);
    }

    public ProcessContextRegistration getProcessContextRegistration() {
        return _parentEnvironment.getProcessContextRegistration();
    }

    public RootEnvironmentUserData getRootEnvironmentUserData() {
        return _parentEnvironment.getRootEnvironmentUserData();
    }

    public void removeForm(String id) {
        _parentEnvironment.removeForm(id);
    }

    public PageContext getPageContext() {
        return _parentEnvironment.getPageContext();
    }

    public String getPublisherURL() {
        return _parentEnvironment.getPublisherURL();
    }

    public HttpServletRequest getRequest() {
        return null;
    }

    public HttpServletResponse getResponse() {
        return null;
    }

    public HttpSession getSession() {
        return null;
    }

    public WGAURLBuilder getURLBuilder() {
        return _parentEnvironment.getURLBuilder();
    }

    public void setLog(Logger log) {
        _parentEnvironment.setLog(log);
    }

    public Logger getLog() {
        return _parentEnvironment.getLog();
    }

    public IsolatedTMLContextEnvironment(TMLContext mainContext, WGACore core, TMLUserProfile userProfile, TMLForm form, HttpServletRequest req, HttpServletResponse rsp, HttpSession session) {
        _parentEnvironment = new IndependentTMLScriptEnvironment(mainContext, core, userProfile, form, req, rsp, session);
    }
    
    
    
        

}
