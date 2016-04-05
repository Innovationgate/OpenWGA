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

import java.util.List;
import java.util.Locale;
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
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.url.WGAURLBuilder;
import de.innovationgate.wgpublisher.webtml.actions.TMLAction;
import de.innovationgate.wgpublisher.webtml.form.TMLForm;

/**
 * This represents the basic environment that a TMLContext is used in.
 * It implements all functionality to retrieve the neccessary resources in it's environment.
 *
 */
public interface TMLContextEnvironment {
    
    public static class RootEnvironmentUserData {
        
        private String _dbkey;
        private WGUserAccess _userAccess;
        private List<Locale> locales;

        public RootEnvironmentUserData(String dbkey, WGUserAccess userAccess) {
            _dbkey = dbkey;
            _userAccess = userAccess;
        }

        public List<Locale> getLocales() {
            return locales;
        }

        public void setLocales(List<Locale> locales) {
            this.locales = locales;
        }

        public String getDbkey() {
            return _dbkey;
        }

        public WGUserAccess getUserAccess() {
            return _userAccess;
        }
        
    }

    public abstract TMLContext getMainContext();

    public abstract Map<String,TMLAction> getActionRegistration();
    
    public abstract ProcessContextRegistration getProcessContextRegistration(); 

    public abstract Map<String,TransientObjectWrapper<Object>> getSessionVars();

    public abstract Map<String,Object> getPageVars();
    
    public abstract List<Warning> getWarnings();

    /**
     * Import data from another context. Useful if this environment cannot fetch data from the WebTML request, but relies
     * on its data
     * @param context The context to read data from.
     * @param includeIntrusiveData Whether to include data from the execution of WebTML code, like vars. False means that
     * just environment information is imported, so the context remains independent.
     * @throws TMLException
     */
    public abstract void importEnvironmentData(TMLContext context, boolean includeIntrusiveData) throws TMLException;

    public abstract TMLContext getTMLContextForDocument(TMLContext parentContext, WGDocument doc);

    /**
     * Return the "current" form of the WebTML environment
     */
    public abstract TMLForm getForm();

    public abstract TMLUserProfile getUserProfile(WGDatabase db);

    public abstract WGACore getCore();

    public abstract void setForm(TMLForm form);
    
    public boolean isPageContextAvailable();
    
    public boolean isWebEnvironment();

    public abstract Map<String,TMLForm> getPersistentForms();

    public abstract Map<String,TMLForm> getTransientForms();
    
    public WGDatabase getMainDesignDB();
    
    public WGDatabase openDB(WGDatabase db) throws WGException;
    
    public WGDatabase fetchDB(String dbKey);
    
    /**
     * Returns data about the user that is responsible for creating and using calling this environment
     * Must return null if there this is the root environment.
     */
    public RootEnvironmentUserData getRootEnvironmentUserData();
    
    public void removeForm(String id);
    
    public PageContext getPageContext();
    
    public String getPublisherURL();

    public HttpServletRequest getRequest();
    
    public HttpServletResponse getResponse();
    
    public HttpSession getSession();
    
    public WGAURLBuilder getURLBuilder();
    
    public Logger getLog();
    
    public void setLog(Logger log);

}
