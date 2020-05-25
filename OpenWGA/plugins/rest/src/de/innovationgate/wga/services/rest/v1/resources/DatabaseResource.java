package de.innovationgate.wga.services.rest.v1.resources;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlValue;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;
import org.eclipse.persistence.oxm.annotations.XmlVariableNode;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGUserDetails;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.common.Constants;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.Plugin;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.RestApplication.DatabaseInfo;
import de.innovationgate.wga.services.rest.v1.resources.cms.CmsApiResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.UserProfileResource;
import de.innovationgate.wga.services.rest.v1.resources.custom.CustomApiResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelApiResource;
import de.innovationgate.wga.services.rest.v1.resources.query.QueryApiResource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@XmlRootElement(name=DatabaseResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(DatabaseResource.RESOURCE_TYPE)
public class DatabaseResource extends EnvelopeReturningResource<RootResource> {
    
    public static class LabeledName {
        
        @XmlTransient
        public String key;
        
        @XmlValue
        public String value;
        
    }

    public static final String RESOURCE_TYPE = "db";
    public static final String REF_LOGINUSERPROFILE = "loginUserProfile";
    public static final String REF_AUTORUSERPROFILES = "autoUserProfiles";
    private Database _database;
    private RestApplication.DatabaseInfo _dbInfo;
    
    private Boolean _isAdminLoggedIn=false;
    
    @XmlTransient
    public Database getDatabase() {
        return _database;
    }

    public DatabaseResource() {
        super();
    }

    public DatabaseResource(RootResource root, String dbKey) throws WGException {
        super(root, root.getURI().path(RootResource.REFLIST_DBS).path(dbKey));
        
        _isAdminLoggedIn = root.isAdminLoggedIn();
        
        _dbInfo = getRootResource().getApplication().getDatabaseInfo().get(dbKey); 
        if (_dbInfo == null) {
            throw new WebApplicationException("There is no REST API enabled for this database", 403);
        }

        if (_dbInfo.isForceRegularLogin()) {
            getRootResource().getWga().getRequest().setAttribute(WGACore.ATTRIB_FORCEREGULARLOGIN, Boolean.TRUE);
        }
        
        _database = root.getWga().database(dbKey);
        if (_database == null) {
            throw new WebApplicationException("Unknown database '" + dbKey + "'", 404);
        }
    }
    
    @Path(CmsApiResource.RESOURCE_TYPE)
    public CmsApiResource getCmsApi() throws WGException {

        if (!_database.db().isSessionOpen() || getAccessLevel()<WGDatabase.ACCESSLEVEL_AUTHOR) {
            throw new WebApplicationException("You have no authoring access to this database", 403);
        }
        if (!isApiEnabled(CmsApiResource.RESOURCE_TYPE)) {
            throw new WebApplicationException("This REST API is not enabled for this database", 403);
        }
        
        return fetchCmsApi();
    }

    public boolean isApiEnabled(String apiType) {
        return _dbInfo.getEnabledRestAPIs().contains(apiType);
    }

    public CmsApiResource fetchCmsApi() {
        return new CmsApiResource(this);
    }
    
    @Path(QueryApiResource.RESOURCE_TYPE)
    public QueryApiResource getQueryApi() throws WGException {

        if (!_database.db().isSessionOpen()) {
            throw new WebApplicationException("You have no access to this database", 403);
        }
        if (!isApiEnabled(QueryApiResource.RESOURCE_TYPE)) {
            throw new WebApplicationException("This REST API is not enabled for this database", 403);
        }
        
        if (_database instanceof App) {
            return new QueryApiResource(this);
        }
        else {
            throw new WebApplicationException("This REST API does not exist on this database", 404);
        }
    }
    
    @Path(CustomApiResource.RESOURCE_TYPE)
    public CustomApiResource getCustomApi() throws WGException {

        if (!_database.db().isSessionOpen()) {
            throw new WebApplicationException("You have no access to this database", 403);
        }
        if (!isApiEnabled(CustomApiResource.RESOURCE_TYPE)) {
            throw new WebApplicationException("This REST API is not enabled for this database", 403);
        }
        
        if (_database instanceof App) {
            return new CustomApiResource(this);
        }
        else {
            throw new WebApplicationException("This API does not exist on this database", 404);
        }
    }
    
    @Path(HdbmodelApiResource.RESOURCE_TYPE)
    public HdbmodelApiResource getHdbmodelApi() throws WGException {

        if (!_database.db().isSessionOpen()) {
            throw new WebApplicationException("You have no access to this database", 403);
        }
        if (!isApiEnabled(HdbmodelApiResource.RESOURCE_TYPE)) {
            throw new WebApplicationException("ThisREST API is not enabled for this database", 403);
        }
        
        return fetchHdbmodelApi();
    }

    public HdbmodelApiResource fetchHdbmodelApi() {
        HDBModel hdbModel = HDBModel.getModel(_database.db());
        if (hdbModel != null) {
            return new HdbmodelApiResource(this, hdbModel);
        }
        else {
            throw new WebApplicationException("This API does not exist on this database", 404);
        }
    }
    
    @XmlElement
    public Date getLastModified() {
        if (_database.db().isSessionOpen()) {
            return _database.db().getLastChanged();
        }
        else {
            return null;
        }
            
        
    }
    
    @XmlElement 
    public Integer getAccessLevel() {
        if (_database.db().isSessionOpen()) {
            return _database.db().getSessionContext().getAccessLevel();
        }
        else {
            return WGDatabase.ACCESSLEVEL_NOACCESS;
        }
    }
    
    @XmlElement 
    public String getUserName() {
        if (_database.db().isSessionOpen()) {
            return _database.db().getSessionContext().getUserAccess().getPrimaryName();
        }
        else {
            return null;
        }
    }
    
    @XmlElement 
    public List<String> getUserAliases() throws WGAPIException {
        if (_database.db().isSessionOpen()) {
            WGUserDetails userDetails = _database.db().getSessionContext().getUserDetails();
            if (userDetails != null) {
                return userDetails.getAliases();
            }
            else {
                return Collections.emptyList();
            }
        }
        else {
            return null;
        }
    }
    
    @XmlElement 
    public List<String> getUserGroups() throws WGAPIException {
        if (_database.db().isSessionOpen()) {
            WGUserDetails userDetails = _database.db().getSessionContext().getUserDetails();
            if (userDetails != null) {
                return userDetails.getGroups();
            }
            else {
                return Collections.emptyList();
            }
        }
        else {
            return null;
        }
    }
    
    @XmlElement 
    public List<String> getUserRoles() throws WGAPIException {
        if (_database.db().isSessionOpen()) {
            WGUserDetails userDetails = _database.db().getSessionContext().getUserDetails();
            if (userDetails != null) {
                return userDetails.getRoles();
            }
            else {
                return Collections.emptyList();
            }
        }
        else {
            return null;
        }
    }
        
    @XmlElementWrapper(name="userLabels")
    @XmlVariableNode(value="key")
    public List<LabeledName> getLabeledNames() throws WGAPIException {
        
        if (_database.db().isSessionOpen()) { 
            WGUserDetails userDetails = _database.db().getSessionContext().getUserDetails();
            if (userDetails != null) {
                List<LabeledName> names = new ArrayList<DatabaseResource.LabeledName>();
                for (Map.Entry<String,String> name : userDetails.getLabeledNames().entrySet()) {
                    LabeledName labeledName = new LabeledName();
                    labeledName.key = name.getKey();
                    labeledName.value = name.getValue();
                    names.add(labeledName);
                }
                return names;
            }
        }
        
        return Collections.emptyList();
    }
    
    @XmlElement 
    public String getDomain() throws WGException {
        if (_database.db().isSessionOpen()) {
            if (_database instanceof Plugin) {
                return null;
            }
            else {
                return _database.domain().getName();
            }
        }
        else {
            return null;
        }
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {
    }

    @Override
    protected void addReferences(References refs) throws WGException {

    	if(!_isAdminLoggedIn)
    		throw new WebApplicationException("admin login required for db references listing", 403);
    	
        if (_database.db().isSessionOpen()) {
            
            if (_dbInfo.getEnabledRestAPIs().contains(CmsApiResource.RESOURCE_TYPE) && getAccessLevel()>WGDatabase.ACCESSLEVEL_READER) {
                refs.add(new ResourceReference(CmsApiResource.RESOURCE_TYPE, autoSuffix(getCmsApi().getURI()), CmsApiResource.RESOURCE_TYPE));
            }
            
            if (_database instanceof App) {
                if (_dbInfo.getEnabledRestAPIs().contains(QueryApiResource.RESOURCE_TYPE)) {
                    refs.add(new ResourceReference(QueryApiResource.RESOURCE_TYPE, autoSuffix(getQueryApi().getURI()), QueryApiResource.RESOURCE_TYPE));
                }
                if (_dbInfo.getEnabledRestAPIs().contains(CustomApiResource.RESOURCE_TYPE)) {
                    refs.add(new ResourceReference(CustomApiResource.RESOURCE_TYPE, autoSuffix(getCustomApi().getURI()), CustomApiResource.RESOURCE_TYPE));
                }
            }
            HDBModel hdbModel = HDBModel.getModel(_database.db());
            if (hdbModel != null && _dbInfo.getEnabledRestAPIs().contains(HdbmodelApiResource.RESOURCE_TYPE)) {
                refs.add(new ResourceReference(HdbmodelApiResource.RESOURCE_TYPE, autoSuffix(getHdbmodelApi().getURI()), HdbmodelApiResource.RESOURCE_TYPE));
            }
            
            int persMode = getRootResource().getWga().getCore().getPersManager().getPersonalisationMode(_database.db());
            
            if (persMode == Constants.PERSMODE_LOGIN) {
                refs.add(new ResourceReference(REF_LOGINUSERPROFILE, autoSuffix(getURI().path(REF_LOGINUSERPROFILE)), UserProfileResource.RESOURCE_TYPE));
            }
            else if (persMode == Constants.PERSMODE_AUTO) {
                refs.add(new ResourceReference(REF_AUTORUSERPROFILES, autoSuffix(getURI().path(REF_AUTORUSERPROFILES)), UserProfileResource.RESOURCE_TYPE));
            }
        }
    }
    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Path(REF_LOGINUSERPROFILE)
    public UserProfileResource getLoginUserProfile() throws WGAPIException, UnavailableResourceException {
        
        if (!_database.db().isSessionOpen()) {
            throw new WebApplicationException("You have no access to this database", 403);
        }
        
        WGA wga = getRootResource().getWga();
        WGDatabase db = _database.db();
        WGDatabase persDB = wga.getCore().getPersManager().fetchPersonalisationDatabase(db);
        int persMode = wga.getCore().getPersManager().getPersonalisationMode(db);
        if (persMode == Constants.PERSMODE_LOGIN) {
            
            if (db.getSessionContext().isAnonymous()) {
                throw new WebApplicationException("No user profile in mode 'login' with anonymous access", 403);
            }
            String userName = db.getSessionContext().getUser();
            WGUserProfile userProfile = persDB.getUserProfile(userName);
            if (userProfile == null) {
                userProfile = wga.getCore().getPersManager().createUserProfile("OpenWGA REST Web Service", persDB, userName, Constants.PERSMODE_LOGIN, null);
            }
            
            return new UserProfileResource(this, userProfile);
            
        }
        
        else {
            
            throw new WebApplicationException("This user profile retrieval is only valid for personalisation mode 'login'", 403);
            
        }
        
    }
    
    @Path(REF_AUTORUSERPROFILES + "/{wgpid}")
    public UserProfileResource getUserProfile(@PathParam("wgpid") String wgpid) throws WGAPIException, UnavailableResourceException {
        
        if (!_database.db().isSessionOpen()) {
            throw new WebApplicationException("You have no access to this database", 403);
        }
        
        WGA wga = getRootResource().getWga();
        WGDatabase db = _database.db();
        WGDatabase persDB = wga.getCore().getPersManager().fetchPersonalisationDatabase(db);
        int persMode = wga.getCore().getPersManager().getPersonalisationMode(db);
        if (persMode == Constants.PERSMODE_AUTO) {
            
            if (db.getSessionContext().isAnonymous()) {
                throw new WebApplicationException("No user profile in mode 'login' with anonymous access", 403);
            }
            String userName = db.getSessionContext().getUser();
            WGUserProfile userProfile = persDB.getUserProfile(userName);
            if (userProfile == null) {
                userProfile = wga.getCore().getPersManager().createUserProfile("OpenWGA REST Web Service", persDB, userName, Constants.PERSMODE_LOGIN, null);
            }
            
            return new UserProfileResource(this, userProfile);
            
        }
        
        else {
            
            throw new WebApplicationException("This user profile retrieval is only valid for personalisation mode 'auto'", 403);
            
        }
        
    }
    
    public DatabaseInfo getDatabaseInfo() {
        return getRootResource().getApplication().getDatabaseInfo().get(_database.getDbKey());
    }

            
                
    public void customEnhanceResource(RESTEntry res, String enhancer, WGDocument contextDoc) throws TMLScriptException {
        try {
            
            if (_database instanceof App) {
                App app = (App) _database;
            
                StringBuffer scriptCode = new StringBuffer();
                scriptCode.append("var enhancerMod = WGA.design().resolveSystemScriptModule('rest:resource-enhancers', 'tmlscript', false);");
                scriptCode.append("if (enhancerMod == null) return;");
                scriptCode.append("var enhancers = WGA.createObject(enhancerMod);");         
                scriptCode.append("if (enhancers." + enhancer + ") enhancers." + enhancer + "(res, referenceDocument);");
                
                Map<String,Object> objects = new HashMap<String, Object>();
                objects.put("res", res);
                if (contextDoc != null) {
                    objects.put("referenceDocument", contextDoc);
                }
                
                execute(scriptCode.toString(), contextDoc, objects);
            }
        }
        catch (Throwable e) {
            RestApplication.LOG.error("Error calling custom REST enhancer '" + enhancer + "'", e);
        }
    }
    
    private void execute(String scriptCode, WGDocument contextDoc, Map<String,Object> objects) throws Throwable {
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
        
        Map<String,Object> params = new HashMap<String,Object>();
        params.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "WGA Web Services REST collection enhancer for app '" +  _database.getDbKey() + "'");
        params.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, ((App) _database).design().getDesignContext().getBaseReference());
        params.putAll(objects);
        
        Context tmlCx;
        if (contextDoc instanceof WGContent) {
            tmlCx = getRootResource().getWga().createTMLContext((WGContent) contextDoc);
        }
        else {
            tmlCx = _database.createTMLContext();
        }
        
        ExpressionResult result = engine.evaluateExpression(scriptCode.toString(), (TMLContext) tmlCx, RhinoExpressionEngine.TYPE_SCRIPT, params);
        if (result.isError()) {
            if (result.getException().getCause() != null) {
                throw result.getException().getCause();
            }
            else {
                throw result.getException();
            }
        }        
    }
    
    @Override
    public String getId() {
        return _database.getDbKey();
    }
    
    public WGA getContextWga() throws WGException {
        return WGA.get(getDatabase().createTMLContext());
    }
    
}
 