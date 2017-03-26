package de.innovationgate.wga.services.rest.v1.resources.query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import com.google.gson.JsonObject;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGResultSet;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.CollectionResult;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.TMLScript.ObjectType;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.v1.resources.ApiResource;
import de.innovationgate.wga.services.rest.v1.resources.DatabaseResource;
import de.innovationgate.wga.services.rest.v1.resources.ExternalRefsResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.resources.custom.CustomApiResource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wgpublisher.hdb.HDBModel;

@XmlRootElement(name=QueryApiResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(value=QueryApiResource.RESOURCE_TYPE)
public class QueryApiResource extends ExternalRefsResource<DatabaseResource> implements ApiResource {
    
    public static final String QUERY_MODULE_QUALIFIER = "rest:queries";
    public static final String RESOURCE_TYPE = "query";
    private ThreadLocal<Object> _apiObject = new ThreadLocal<Object>();
    
    public QueryApiResource() {
        super();
    }

    public QueryApiResource(DatabaseResource parent) {
        super(parent, parent.getURI().path(RESOURCE_TYPE));
    }

    @Override
    public Resource<?> wrapIntoResource(Object resourceObject) throws WGException {

        boolean useModel = false;
        
        WGContent content = null;
        if (resourceObject instanceof WGContent) {
            content = (WGContent) resourceObject;
        }
        else if (resourceObject instanceof Context) {
            content = ((Context) resourceObject).content();
        }
        
        if (content != null) {
            useModel = HDBModel.isContent(content);
        }
        
        if (useModel) {
            return getParentResource().fetchHdbmodelApi().wrapIntoResource(resourceObject);
        }
        else {
            return getParentResource().fetchCmsApi().wrapIntoResource(resourceObject);
        }
        
        
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws Exception {
        
        DatabaseResource dbRes = getParentResource();
        if (dbRes.getDatabase() instanceof App) {
            queryApi(list);
        }
    }
    
    private void fetchApiModule(WGA wga, String apiName) throws UnavailableResourceException, WGException {
        DatabaseResource dbRes = getParentResource();
        
        Design appDesign = ((App) dbRes.getDatabase()).design();
        Object apiObject = null;
        
        // V2 object
        String moduleName = QUERY_MODULE_QUALIFIER + ":" + apiName + "api";
        Design moduleDesign = appDesign.resolveSystemScriptModule(moduleName, WGScriptModule.CODETYPE_TMLSCRIPT, false);
        if (moduleDesign != null) {
            apiObject = wga.tmlscript().createObject(moduleDesign, ObjectType.V2_ISOLATED);
        }
        
        // Legacy object
        if (apiObject == null) {
            moduleName = QUERY_MODULE_QUALIFIER + ":" + apiName;
            moduleDesign = appDesign.resolveSystemScriptModule(moduleName, WGScriptModule.CODETYPE_TMLSCRIPT, false);
            if (moduleDesign != null) {
                apiObject = wga.tmlscript().createObject(moduleDesign, ObjectType.V1);
            }
        }

        if (apiObject == null) {
            throw new WebApplicationException("Unknown query api collection '" + apiName, 404);
        }


        _apiObject.set(apiObject);
    }

    private void queryApi(ReferenceCollection list) throws Exception {
        
        WGA wga = getRootResource().getWga();
        fetchApiModule(wga, list.getId().getName());
        try {

            Map<String,Object> extraObjects = new HashMap<String,Object>();
            extraObjects.put("$controller", this);
            extraObjects.put("$list", list);
            
            Database database = getDatabaseResource().getDatabase();
            if (database instanceof App) {
                list.getParameters().specialEnhancer = new QueryCollectionEnhancer(getRootResource().getWga(), (App) database, _apiObject.get());
            }
            
            try {
                TMLScript tmlScript = getDatabaseResource().getContextWga().tmlscript();
                if (!tmlScript.hasProperty(_apiObject.get(), "query")) {
                    throw new WebApplicationException("Unknown query api collection '" + list.getId().getName(), 404);
                }
                Object result = tmlScript.callMethod(_apiObject.get(), "query", extraObjects, Arrays.asList(new Object[] {list, this}));
                
                Iterator<?> it = extractIterator(result);
                if (it != null) {
                    if (it instanceof SkippingIterator) {
                        SkippingIterator<?> skipIt = (SkippingIterator<?>) it; 
                        list.addUntilFull(skipIt);
                    }
                    else {
                        list.noPaging();
                        while (it.hasNext()) {
                            Object element = it.next();
                            list.add(wrapIntoResource(element));
                        }
                    }
                }
            }
            catch (WGException e) {
                throw CustomApiResource.handleScriptException(e);
            }
            
        }
        finally {
            dropRequestResources();
        }
        
    }



    private void dropRequestResources() {
        _apiObject.remove();
    }

    public static Iterator<? extends Object> extractIterator(Object result) throws WGException {
        Iterator<? extends Object> it = null;
        if (result instanceof Iterator) {
            it = (Iterator<?>) result;
        }
        else if (result instanceof Collection<?>) {
            it = ((Collection<?>) result).iterator();
        }
        else if (result instanceof WGResultSet) {
            it = ((WGResultSet) result).getResultIterator();
        }
        else if (result instanceof CollectionResult) {
            it = ((CollectionResult) result).iterator();
        }
        return it;
    }

    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException, WGException {

        DatabaseResource dbRes = getParentResource();
        if (dbRes.getDatabase() instanceof App) {
            Design appDesign = ((App) dbRes.getDatabase()).design();
            
            Set<String> moduleNames = new HashSet<String>();
            moduleNames.addAll(appDesign.resolve("wga:" + QUERY_MODULE_QUALIFIER).getScriptModuleNames(WGScriptModule.CODETYPE_TMLSCRIPT, false));
            moduleNames.addAll(appDesign.resolve("overlay:wga:" + QUERY_MODULE_QUALIFIER).getScriptModuleNames(WGScriptModule.CODETYPE_TMLSCRIPT, false));
            List<String> moduleNameList = new ArrayList<String>(moduleNames);
            Collections.sort(moduleNameList);
            for (String moduleName : moduleNameList) {
                String apiId = moduleName.substring(moduleName.lastIndexOf(":") + 1);
                refs.add(new ReferenceCollection(this, apiId, getURI()));
            }
        }
        
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Override
    public String getId() throws WGException {
        return RESOURCE_TYPE;
    }

}
