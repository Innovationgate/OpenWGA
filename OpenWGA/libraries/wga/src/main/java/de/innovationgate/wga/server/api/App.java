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

package de.innovationgate.wga.server.api;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.config.ContentStore;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.server.api.tml.UserProfile;
import de.innovationgate.wgpublisher.PersonalisationManager;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.events.ApplicationEvent;
import de.innovationgate.wgpublisher.events.ApplicationEventPath;
import de.innovationgate.wgpublisher.events.EventPath;
import de.innovationgate.wgpublisher.events.ManagedGlobalEventReceiver;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.ManagedTMLScriptGlobalDefinition;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptObjectMetadata;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.url.RequestIndependentDefaultURLBuilder;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

/**
 * Object representing an OpenWGA application and its services
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public class App extends Database {

	public HDBModel HDBModel;
	
    protected App(WGA wga, WGDatabase db) throws WGException {
        super(wga, db);
        
        if (!db.hasFeature(WGDatabase.FEATURE_FULLCONTENTFEATURES)) {
            throw new WGAServerException("Database '" + db.getDbReference() + "' is no application");
        }
        HDBModel = de.innovationgate.wgpublisher.hdb.HDBModel.getModel(db);
    }

    /**
     * Returns a TMLScript global that is available for the current app
     * This method allows to retrieve TMLScript globals with their name as method parameter. It can retrieve normal globals just like globals of design scope.
     * This might contain native TMLScript objects which are not usable from Java.
     * @param name Name the global
     * @throws WGAPIException
     */
    public Object getGlobal(String name) throws WGException {
        TMLScriptGlobal global = _wga.getCore().getTmlscriptGlobalRegistry().getGlobal(name, db());
        if (global != null) {
            return global.getRef();
        }
        else {
            return null;
        }
    }
    
    /**
     * Returns the design context of the app
     * @throws UnavailableResourceException
     */
    public Design design() throws WGException {
        // If the current TMLContext is of the same app return the design directly from WGA object, as this contains the current design reference
        if (_wga.isTMLContextAvailable()) {
            if (((TMLContext) _wga.tmlcontext()).getDesignContext().getDesignDB() == _db) {
                return _wga.design();
            }
        }
        return new Design(_wga, _db);
    }
    
    /**
     * Returns the design context for a design resource in this app
     * @param refString The name of the design resource
     * @throws UnavailableResourceException
     */
    public Design design(String refString) throws WGException {
        return design().resolve(refString);
    }
    
    /**
     * Returns the user roles of the currently logged in user on the application
     * @throws WGAServerException
     */
    public List<String> getUserRoles() throws WGException  {
        try {
            WGUserDetails details = db().getSessionContext().getUserDetails();
            if (details != null) {
                return details.getRoles();
            }
            else {
                return Collections.emptyList();
            }
        }
        catch (WGAPIException e) {
            throw new WGAServerException("Exception retrieving user roles", e);
        }
    }
    
    /**
     * Returns a lucene object with this application in context.
     *  Throws an exception if the application is not contained in Lucene fulltext index.
     * @throws WGAServerException
     */
    public Lucene lucene() throws WGException {
        
        if (_wga.isTMLContextAvailable()) {
            return _wga.lucene(_wga.tmlcontext());
        }
        else {
            return _wga.lucene(createTMLContext());
        }
        
    }
    
    /**
     * Returns if a Lucene fulltext index is available for this application
     * @throws WGAServerException
     */
    public boolean isLuceneIndexed() throws WGException {
        ContentStore db = _wga.getCore().getWgaConfiguration().getContentStore(getDbKey());
        return (db != null && db.getLuceneIndexConfiguration() != null && db.getLuceneIndexConfiguration().isEnabled());
    }
    
    /**
     * Returns a list of names of the website areas of this app
     * @throws WGException
     */
    public List<String> getAreaNames() throws WGException {
        return new ArrayList<String>(db().getAreas().keySet());
    }
    
    /**
     * Returns a list of names of the languages (i.e. language codes) of this app
     * @throws WGException
     */
    public List<String> getLanguageNames() throws WGException {
        return new ArrayList<String>(db().getLanguages().keySet());
    }
    
    /**
     * Returns a list of names of the content types of this app
     * @throws WGException
     */
    public List<String> getContentTypeNames() throws WGException {
        
        List<String> names = new ArrayList<String>();
        for (WGContentType ct: db().getContentTypes()) {
            names.add(ct.getName());
        }
        return names;
        
    }
    
    public String getBaseURL() throws WGException {
        return _wga.server().getBaseURL() + "/" + getDbKey();
    }
    
    public Context getHomepage() throws WGException{
    	String homepageName = (String)getPublisherOption(WGACore.DBATTRIB_HOME_PAGE_NAME);
    	if(homepageName!=null){
    		return createTMLContext().context("name:"+homepageName, false);
    	}
    	return null;
    }
    
    public String getHomepageURL() throws WGException{
    	HttpServletRequest request = _wga.getRequest();
    	if(request!=null){
	    	RequestIndependentDefaultURLBuilder builder = new RequestIndependentDefaultURLBuilder();
	    	builder.newIndependentInstance(_wga.getCore());
	    	return builder.buildHomepageURL(db(), request);
    	}
    	return null;
    }
    
    public ApplicationEventBuilder createEvent(List<? extends Object> qualifiers) throws WGException {
        ApplicationEventPath eventPath = new ApplicationEventPath(getDbKey(), ApplicationEventPath.parseQualifiers(qualifiers));
        return new ApplicationEvent.Builder(_wga, this, eventPath);
    }
    
    public ApplicationEventBuilder createEvent(String eventPathStr) throws WGException {
        return createEvent(WGUtils.deserializeCollection(eventPathStr, "/", true));
    }

    /**
     * Registers a managed TMLScript app global variable, which will get created on demand from the TMLScript module of this Designs current base reference.
     * @param name
     * @param scope The creation scope which determines, for which scope entity individual global objects will be provided. For example: Scope {@link ObjectScope#PORTLET} creates individual global objects for every request.   
     * @throws WGException
     */
    public void managedGlobal(String name, Design design, ManagedGlobalConfig config) throws WGException {
        if (design.getTMLScriptModule() == null) {
            throw new WGAServerException("Design reference '" + design.getBaseReference().toString() + "' does not point to a TMLScript module");
        }
        if(config.getScope().needsWebsockets()){
        	// check if websockets are enabled
            if ((Boolean) _wga.getCore().getServicesServerOptionReader().readOptionValueOrDefault(WGAConfiguration.SERVEROPTION_SERVICE_WEBSOCKETS) == false) {
                _wga.getLog().error("Websocket network service is disabled - unable to create managed global '" + name + "' of scope " + config.getScope());
                return;
            }        	
        }

        _wga.getCore().getTmlscriptGlobalRegistry().registerAppGlobal(ExpressionEngineFactory.getTMLScriptEngine().createGlobal(name, TMLScriptGlobal.TYPE_MANAGED, new ManagedTMLScriptGlobalDefinition(name, design.getBaseReference(), config)), db());

        // Check if there is something we should process on the prototype
        TMLScriptObjectMetadata metaData = ExpressionEngineFactory.getTMLScriptEngine().getTmlscriptObjectMetadata(_wga, design);
        if (config.getScope().isApplicationEventReceiver()) {
            for (Map.Entry<EventPath,String> entry : metaData.getAppEventListeners().entrySet()) {
                _wga.getCore().getEventManager().registerEventReceiver(entry.getKey(), new ManagedGlobalEventReceiver(db().getDbReference(), name, entry.getValue()));
            }
        }
        
        
    }
    
    /**
     * Returns the user profile associated with the current user for this app.
     * May return null if the current user has no profile or the environment does not have the necessary profile assocation information.
     */
    public UserProfile userProfile() throws WGException {
        
        WGA unlocked = Unlocker.unlock(_wga);
        if (!unlocked.isHttpSessionAvailable()) {
            return null;
        }
        
        PersonalisationManager persManager = unlocked.getCore().getPersManager();
        WGPDispatcher dispatcher = unlocked.getCore().getDispatcher();
        WGDatabase persDB = persManager.fetchPersonalisationDatabase(db());
        Integer persMode =  persManager.getPersonalisationMode(persDB);
        
        WGUserProfile docProfile = persManager.getProfileFromSessionCache(unlocked.getHttpSession(), _db, persDB);
        if (docProfile != null && !docProfile.isDeleted() && docProfile.getDatabase().isReady()) {
            persManager.markProfileForAutoSave(docProfile, persMode, unlocked.getHttpSession());
            return new TMLUserProfile(docProfile, _wga.getCore(), persMode, unlocked.getHttpSession(), _db.getComplianceVersion());
        }
        
        return null;
        
    }
    
    /**
     * TMLContext handling for App
     * @throws WGException
     */
    public Context context() throws WGException{
    	return createTMLContext();
    }
    public Context context(WGContent c) throws WGException{
    	return createTMLContext().context(c);
    }
    public Context context(String expression) throws WGException{
    	return createTMLContext().context(expression);
    }
    public Context context(String expression, boolean returnContextOnError) throws WGException{
    	return createTMLContext().context(expression, returnContextOnError);
    }


}
