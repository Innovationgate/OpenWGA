package de.innovationgate.wga.services.rest.v1.resources.custom;

import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.OPTIONS;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;
import org.glassfish.jersey.media.multipart.BodyPart;
import org.glassfish.jersey.media.multipart.ContentDisposition;
import org.glassfish.jersey.media.multipart.MultiPart;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.SkippingIteratorWrapper;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.TMLScript.ObjectType;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.ScriptContext;
import de.innovationgate.wga.services.rest.v1.resources.ApiResource;
import de.innovationgate.wga.services.rest.v1.resources.DatabaseResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.resources.query.QueryApiResource;
import de.innovationgate.wga.services.rest.v1.types.ElementFilter;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.Status;
import de.innovationgate.wgpublisher.WGAServerException;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@XmlRootElement(name=CustomApiResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(value=CustomApiResource.RESOURCE_TYPE)
public class CustomApiResource extends Resource<DatabaseResource> implements ApiResource {
    
    private ThreadLocal<Object> _apiObject = new ThreadLocal<Object>();
    
    public static class QueryResource {
        
        private Object _resourceObject;
        
        private String _collectionId;

        public QueryResource(Object resourceObject, String collectionId) {
            super();
            _resourceObject = resourceObject;
            _collectionId = collectionId;
        }

        public Object getResourceObject() {
            return _resourceObject;
        }

        public String getCollectionId() {
            return _collectionId;
        }
        
        
    }
    
    public static final String CUSTOM_MODULE_QUALIFIER = "rest:custom-api";
    public static final String RESOURCE_TYPE = "custom";
    
    public CustomApiResource() {
        super();
    }

    public CustomApiResource(DatabaseResource parent) {
        super(parent, parent.getURI().path(RESOURCE_TYPE));
    }

    @Override
    public Resource<?> wrapIntoResource(Object resourceObject) throws WGException {

        if (resourceObject instanceof QueryResource) {
            return new JsonResolveableResource(this, (QueryResource) resourceObject);
        }
        else if (resourceObject == null){
            throw new WebApplicationException("Null resource object", 500);
        }
        else {
            throw new WebApplicationException("Unwrappeable resource object: "  + resourceObject.getClass().getName(), 500);
        }
        

        
    }

    private void fetchApiModule(WGA wga, String apiName) throws UnavailableResourceException, WGException {
        DatabaseResource dbRes = getParentResource();
        
        Design appDesign = ((App) dbRes.getDatabase()).design();
        Object apiObject = getApiObject(wga, appDesign, apiName);

        if (apiObject == null) {
            throw new WebApplicationException("Unknown custom api collection '" + apiName, 404);
        }


        _apiObject.set(apiObject);
    }

    private Object getApiObject(WGA wga, Design appDesign, String apiName) throws WGException {
        Object apiObject = null;
        
        // V2 object
        String moduleName = CUSTOM_MODULE_QUALIFIER + ":" + apiName + ".api";
        Design moduleDesign = appDesign.resolveSystemScriptModule(moduleName, WGScriptModule.CODETYPE_TMLSCRIPT, false);
        if (moduleDesign != null) {
            apiObject = wga.tmlscript().createObject(moduleDesign, ObjectType.V2_ISOLATED);
        }
        
        // Legacy object
        if (apiObject == null) {
            moduleName = CUSTOM_MODULE_QUALIFIER + ":" + apiName;
            moduleDesign = appDesign.resolveSystemScriptModule(moduleName, WGScriptModule.CODETYPE_TMLSCRIPT, false);
            if (moduleDesign != null) {
                apiObject = wga.tmlscript().createObject(moduleDesign, ObjectType.V1);
            }
        }
        return apiObject;
    }
    
    @Path("{apiName}")
    @OPTIONS
    public Response collectionOptions(@PathParam("apiName") String apiName) throws WGException {
        
        WGA wga = getRootResource().getWga();
        fetchApiModule(wga, apiName);
        try {
            TMLScript tmlScript = getRootResource().getWga().tmlscript();
            
            List<String> options = new ArrayList<String>();
            options.add("OPTIONS");
            if (tmlScript.hasProperty(_apiObject.get(), "get")) {
                options.add("GET");
                options.add("HEAD");
            }
            if (tmlScript.hasProperty(_apiObject.get(), "post")) {
                options.add("POST");
            }
            return Response.ok().header("Allow", WGUtils.serializeCollection(options, ",")).build();
        }
        finally {
            dropThreadStatus();
        }
        
    }
    
    private void dropThreadStatus() {
        _apiObject.remove();
    }

    @Path("{apiName}/{id}")
    @OPTIONS
    public Response resourceOptions(@PathParam("apiName") String apiName, @PathParam("id") String id) throws WGException {
        
        WGA wga = getRootResource().getWga();
        fetchApiModule(wga, apiName);
        try {
            TMLScript tmlScript = getRootResource().getWga().tmlscript();
            List<String> options = new ArrayList<String>();
            options.add("OPTIONS");
            if (tmlScript.hasProperty(_apiObject.get(), "get")) {
                options.add("GET");
                options.add("HEAD");
            }
            if (tmlScript.hasProperty(_apiObject.get(), "put")) {
                options.add("PUT");
            }
            if (tmlScript.hasProperty(_apiObject.get(), "delete")) {
                options.add("DELETE");
            }
            return Response.ok().header("Allow", WGUtils.serializeCollection(options, ",")).build();
        }
        finally {
            dropThreadStatus();
        }
        
    }

    @Path("{apiName}/{id}")
    @GET
    public Response get(@PathParam("apiName") String apiName, @PathParam("id") String id) throws Exception {
        
        TMLContext mainContext = (TMLContext) getDatabaseResource().getDatabase().createTMLContext();
        mainContext.makeThreadMainContext();
        try {
        
            WGA wga = getRootResource().getWga();
            fetchApiModule(wga, apiName);
            ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this);
            scriptContext.setSelfUri(scriptContext.getUri());
            
            Map<String,Object> extraObjects = new HashMap<String,Object>();
            extraObjects.put("$id", id);
            extraObjects.put("$restContext", scriptContext);
            
            TMLScript tmlScript = getDatabaseResource().getContextWga().tmlscript();
            if (tmlScript.hasProperty(_apiObject.get(), "get")) {
                try {
                    Object result = tmlScript.callMethod(_apiObject.get(), "get", extraObjects, Arrays.asList(new Object[] {id, scriptContext}));
                    JsonObject jsonResult = (JsonObject) tmlScript.importJsonData(result);
                    return buildResponse(jsonResult, scriptContext);
                }
                catch (WGException e) {
                    throw handleScriptException(e);
                }
            }
            else {
                throw new WebApplicationException("Custom API '" + apiName + "' does not implement method GET on resource", 400);
            }
        }
        finally {
            mainContext.removeThreadMainContext();
            dropThreadStatus();
        }
            
    }

    private Response buildResponse(JsonObject jsonResult, ScriptContext scriptContext) {
        
        JsonObjectBuilder envelope = Json.createObjectBuilder();
        envelope.add("status", Status.SUCCESS.toString());
        if (scriptContext.getSelfUri() != null) {
            envelope.add("self", scriptContext.getSelfUri());
        }
        
        if (jsonResult != null) {
        envelope.add("resource", jsonResult);
        }
        
        
        ResponseBuilder responseBuilder = Response.status(scriptContext.getResponseCode()).entity(envelope.build());
        for (ScriptContext.Header header : scriptContext.getResponseHeaders()) {
            responseBuilder.header(header.getName(), header.getValue());
        }
        return responseBuilder.build();
    }
    
    @Path("{apiName}/{id}")
    @Consumes({"application/json","text/json"})
    @PUT
    public Response put(@PathParam("apiName") String apiName, @PathParam("id") String id, JsonObject input) throws Exception {
        return put(apiName, id, input, null);
    }
    
    
    private Response put(String apiName, String id, JsonObject input, Map<String,InputStream> files) throws Exception {    
        TMLContext mainContext = (TMLContext) getDatabaseResource().getDatabase().createTMLContext();
        mainContext.makeThreadMainContext();
        try {
            WGA wga = getRootResource().getWga();
            fetchApiModule(wga, apiName);
    
            TMLScript tmlScript = getDatabaseResource().getContextWga().tmlscript();
            
            ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this, files);
            scriptContext.setSelfUri(scriptContext.getUri());
            Map<String,Object> extraObjects = new HashMap<String,Object>();
            Object jsonInput = tmlScript.exportJsonData(input);
            extraObjects.put("$id", id);
            extraObjects.put("$input", jsonInput);
            extraObjects.put("$restContext", scriptContext);
            
            if (tmlScript.hasProperty(_apiObject.get(), "put")) {
                try {
                    Object result = tmlScript.callMethod(_apiObject.get(), "put", extraObjects, Arrays.asList(new Object[] {id, jsonInput, scriptContext}));
                    JsonObject jsonResult = (JsonObject) tmlScript.importJsonData(result);
                    return buildResponse(jsonResult, scriptContext);
                }
                catch (WGException e) {
                    throw handleScriptException(e);
                }
            }
            else {
                throw new WebApplicationException("Custom API '" + apiName + "' does not implement method PUT on resource", 400);
            }
        }
        finally {
            mainContext.removeThreadMainContext();
            dropThreadStatus();
        }
        
    }
    
    @Path("{apiName}/{id}")
    @Consumes({"multipart/mixed","multipart/form-data"})
    @Produces("application/json")
    @PUT
    public Response putMultipart(@PathParam("apiName") String apiName, @PathParam("id") String id, MultiPart multipart) throws Exception {
        
        
        JsonObject j = null;
        Map<String,InputStream> files = new HashMap<String, InputStream>();
        
        for (BodyPart p : multipart.getBodyParts()) {
            
            if (p.getMediaType().isCompatible(MediaType.APPLICATION_JSON_TYPE)) {
                if (j == null) {
                    j = p.getEntityAs(JsonObject.class);
                }
                else {
                    throw new WebApplicationException("Multipart put with multiple JSON parts not allowed", javax.ws.rs.core.Response.Status.BAD_REQUEST);
                }
            }
            else {
                ContentDisposition contentDisposition = p.getContentDisposition();
                if (contentDisposition == null) {
                    throw new WebApplicationException("Non-JSON parts on multipart put must have a file name in content disposition header", javax.ws.rs.core.Response.Status.BAD_REQUEST);
                }
                
                String fileName = contentDisposition.getFileName();
                if (fileName == null) {
                    fileName = contentDisposition.getParameters().get("filename"); // Hack for multipart/form-data, where the file name does not seem to get parsed
                    if (fileName == null) {
                        throw new WebApplicationException("Non-JSON parts on multipart post must have a file name in content disposition header", javax.ws.rs.core.Response.Status.BAD_REQUEST);
                    }
                }
                InputStream in = p.getEntityAs(InputStream.class);
                files.put(fileName, in);
            }
            
        }
        
        Response lastResponse;
        if (j != null || files.size() > 0) {
            return put(apiName, id, j, files);
        }
        else {
            throw new WebApplicationException("Multipart PUT request without processable parts", javax.ws.rs.core.Response.Status.BAD_REQUEST);
        }
        
    }
    
    @Path("{apiName}")
    @Consumes({"multipart/mixed","multipart/form-data"})
    @Produces("application/json")
    @POST
    public Response postMultipart(@PathParam("apiName") String apiName, MultiPart multipart) throws Exception {
        
        JsonObject j = null;
        Map<String,InputStream> files = new HashMap<String, InputStream>();
        
        for (BodyPart p : multipart.getBodyParts()) {
            
            if (p.getMediaType().isCompatible(MediaType.APPLICATION_JSON_TYPE)) {
                if (j == null) {
                    j = p.getEntityAs(JsonObject.class);
                }
                else {
                    throw new WebApplicationException("Multipart post with multiple JSON parts not allowed", javax.ws.rs.core.Response.Status.BAD_REQUEST);
                }
            }
            else {
                ContentDisposition contentDisposition = p.getContentDisposition();
                if (contentDisposition == null) {
                    throw new WebApplicationException("Non-JSON parts on multipart post must have a file name in content disposition header", javax.ws.rs.core.Response.Status.BAD_REQUEST);
                }
        
                String fileName = contentDisposition.getFileName();
                if (fileName == null) {
                    fileName = contentDisposition.getParameters().get("filename"); // Hack for multipart/form-data, where the file name does not seem to get parsed
                    if (fileName == null) {
                        throw new WebApplicationException("Non-JSON parts on multipart post must have a file name in content disposition header", javax.ws.rs.core.Response.Status.BAD_REQUEST);
                    }
                }
                InputStream in = p.getEntityAs(InputStream.class);
                files.put(fileName, in);
            }
            
        }
        
        Response lastResponse;
        if (j != null) {
            return post(apiName, j, files);
        }
        else {
            throw new WebApplicationException("Multipart POST request without JSON part", javax.ws.rs.core.Response.Status.BAD_REQUEST);
        }
        
    }
    
    @Path("{apiName}/{id}/files/{file}")
    @Consumes({"application/octet-stream"})
    @Produces("application/json")
    @PUT
    public Response putBinary(@PathParam("apiName") String apiName, @PathParam("id") String id, @PathParam("file") String fileName, InputStream input) throws Exception {
        
        TMLContext mainContext = (TMLContext) getDatabaseResource().getDatabase().createTMLContext();
        mainContext.makeThreadMainContext();
        try {
            WGA wga = getRootResource().getWga();
            fetchApiModule(wga, apiName);
            
            TMLScript tmlScript = getDatabaseResource().getContextWga().tmlscript();
            ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this);
            scriptContext.setSelfUri(scriptContext.getUri());
            Map<String,Object> extraObjects = new HashMap<String,Object>();
            extraObjects.put("$id", id);
            extraObjects.put("$input", input);
            extraObjects.put("$fileName", fileName);
            extraObjects.put("$restContext", scriptContext);
            
            if (tmlScript.hasProperty(_apiObject.get(), "putFile")) {
                Object result = tmlScript.callMethod(_apiObject.get(), "putFile", extraObjects, Arrays.asList(new Object[] {id, fileName, input, scriptContext}));
                JsonObject jsonResult = (JsonObject) tmlScript.importJsonData(result);
                return buildResponse(jsonResult, scriptContext);
            }
            else {
                throw new WebApplicationException("Method putFile not defined for this resource", 400);
            }
        }
        finally {
            mainContext.removeThreadMainContext();
            dropThreadStatus();
        }
        
    }
    
    @Path("{apiName}/{id}")
    @DELETE
    public Response delete(@PathParam("apiName") String apiName, @PathParam("id") String id) throws Exception {
        
        TMLContext mainContext = (TMLContext) getDatabaseResource().getDatabase().createTMLContext();
        mainContext.makeThreadMainContext();
        try {
            WGA wga = getRootResource().getWga();
            fetchApiModule(wga, apiName);

            ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this);
            Map<String,Object> extraObjects = new HashMap<String,Object>();
            extraObjects.put("$id", id);
            extraObjects.put("$restContext", scriptContext);
            
            TMLScript tmlScript = getDatabaseResource().getContextWga().tmlscript();
            if (tmlScript.hasProperty(_apiObject.get(), "delete")) {
                try {
                    Object result = tmlScript.callMethod(_apiObject.get(), "delete", extraObjects, Arrays.asList(new Object[] {id, scriptContext}));
                    JsonObject jsonResult = (JsonObject) tmlScript.importJsonData(result);
                    return buildResponse(jsonResult, scriptContext);
                }
                catch (WGException e) {
                    throw handleScriptException(e);
                }
            }
            else {
                throw new WebApplicationException("Custom API '" + apiName + "' does not implement method DELETE on resource", 400);
            }
        }
        finally {
            dropThreadStatus();
            mainContext.removeThreadMainContext();
        }
        
    }

    @Path("{apiName}")
    @GET
    public Response query(final @PathParam("apiName") String apiName, @QueryParam(RestApplication.URLPARAM_OFFSET) @DefaultValue(value="0") int offset, @QueryParam(RestApplication.URLPARAM_SIZE) @DefaultValue(value="10") int size) throws Exception {

        TMLContext mainContext = (TMLContext) getDatabaseResource().getDatabase().createTMLContext();
        mainContext.makeThreadMainContext();
        try {
            WGA wga = getRootResource().getWga();
            fetchApiModule(wga, apiName);
            ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this);
            JsonObject result = null;
            
            try {
                scriptContext.setSelfUri(scriptContext.getUri());
                Map<String,Object> extraObjects = new HashMap<String,Object>();
                extraObjects.put("$restContext", scriptContext);
                
                TMLScript tmlscript = getDatabaseResource().getContextWga().tmlscript();
                if (!tmlscript.hasProperty(_apiObject.get(), "query")) {
                    throw new WebApplicationException("Custom API '" + apiName + "' does not implement method GET on collection", 404);
                }
                
                Object scriptResult;
                try {
                    scriptResult = tmlscript.callMethod(_apiObject.get(), "query", extraObjects, Arrays.asList(new Object[] {scriptContext}));
                }
                catch (WGException e) {
                    throw handleScriptException(e);
                }
                
                RhinoExpressionEngine rhino = ExpressionEngineFactory.getTMLScriptEngine();
                int type = rhino.determineTMLScriptType(scriptResult);
                
                // Directly return of scriptable: This is our result
                
                if (type == RhinoExpressionEngine.TYPE_SCRIPTABLE) {
                    result = (JsonObject) Json.createReader(new StringReader(rhino.convertScriptableToJson(scriptResult))).read();
                    return buildResponse(result, scriptContext);
                }
                
                // Return of something iterable. Iterate over it and call resolve() on the api for each element to get JSON representation
                ReferenceCollection list = new ReferenceCollection(this, apiName, getURI());
                list.offset = offset;
                list.size = size;
                Iterator<? extends Object> it = QueryApiResource.extractIterator(scriptResult);
                if (it != null) {
                    if (!(it instanceof SkippingIterator)) {
                        it = new SkippingIteratorWrapper<Object>(it);
                    }
                    SkippingIterator<?> skipIt = (SkippingIterator<?>) it; 
                    list.setFilter(new ElementFilter() {
                        @Override
                        public Object filterElement(Object element) throws WGException {
                            return new QueryResource(element, apiName);
                        }
                        
                    });
                    list.addUntilFull(skipIt);
                    result = list.toJSON();
                }
                else if (scriptResult == null) {
                    throw new WebApplicationException("Custom API '" + apiName + "' query() method returned null", 500);
                }
                else if (scriptResult.equals(rhino.getUndefined())) {
                    throw new WebApplicationException("Custom API '" + apiName + "' does not implement method GET on collection", 404);
                }
                else {
                    throw new WebApplicationException("Invalid return type from custom API '" + apiName + "' query() method: " + scriptResult.getClass().getName(), 500);
                }
            }
            finally {
                _apiObject.remove();
            }
            
            
            return buildResponse(result, scriptContext);
        }
        catch (WGAServerException e) {
            throw handleScriptException(e);
        }
        finally {
            mainContext.removeThreadMainContext();
        }
        
    }

    @GET
    public Response getApis() throws WGAPIException, IllegalArgumentException, UriBuilderException, WGException {

        DatabaseResource dbRes = getParentResource();
        Design appDesign = ((App) dbRes.getDatabase()).design();
        
        Set<String> moduleNames = new HashSet<String>();
        moduleNames.addAll(appDesign.resolve("wga:" + CUSTOM_MODULE_QUALIFIER).getScriptModuleNames(WGScriptModule.CODETYPE_TMLSCRIPT, false));
        moduleNames.addAll(appDesign.resolve("overlay:wga:" + CUSTOM_MODULE_QUALIFIER).getScriptModuleNames(WGScriptModule.CODETYPE_TMLSCRIPT, false));
        List<String> moduleNameList = new ArrayList<String>(moduleNames);
        Collections.sort(moduleNameList);
        
        JsonObjectBuilder json = Json.createObjectBuilder();
        json.add("status", Status.SUCCESS.toString());
        
        JsonObjectBuilder refs = Json.createObjectBuilder();
        for (String moduleName : moduleNameList) {
            String apiId = moduleName.substring(moduleName.lastIndexOf(":") + 1);
            int dotPos = apiId.indexOf(".");
            if (dotPos != -1) {
                apiId = apiId.substring(0, dotPos);
            }
            JsonObjectBuilder ref = Json.createObjectBuilder();
            ref.add("id", apiId);
            ref.add("href", getURI().path(apiId).build().toASCIIString());
            refs.add(apiId, ref);
        }
        json.add("refs", refs.build());
        
        ResponseBuilder responseBuilder = Response.ok(json.build());
        return responseBuilder.build();
                
        
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Override
    public String getId() throws WGException {
        return RESOURCE_TYPE;
    }

    public JsonObject resolveToJson(Object contextObject, String collectionName) throws WGException {
        
        Map<String,Object> extraObjects = new HashMap<String,Object>();
        ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this);
        extraObjects.put("$restContext", scriptContext);
        extraObjects.put("$object", contextObject);
        
        Context cx;
        if (contextObject instanceof TMLContext) {
            cx = (Context) contextObject;
        }
        else if (contextObject instanceof WGContent) {
            cx = getDatabaseResource().getDatabase().createTMLContext().context((WGContent) contextObject);
        }
        else {
            cx = getDatabaseResource().getDatabase().createTMLContext();
        }
        
        TMLScript tmlScript = WGA.get(cx).tmlscript();
        
        try {
            Object result = tmlScript.callMethod(_apiObject.get(), "resolve", extraObjects, Arrays.asList(new Object[] {contextObject, scriptContext}));
            if (result != null) {
                return (JsonObject) tmlScript.importJsonData(result);
            }
            else {
                return null;
            }
        }
        catch (WGException e) {
            throw handleScriptException(e);
        }
        
        
        
    }

    @Path("{apiName}")
    @Consumes({"application/json","text/json"})
    @POST
    public Response post(@PathParam("apiName") String apiName, JsonObject input) throws Exception {
        return post(apiName, input, null);
    }
        
    private Response post(String apiName, JsonObject input, Map<String,InputStream> files) throws Exception {
        
        TMLContext mainContext = (TMLContext) getDatabaseResource().getDatabase().createTMLContext();
        mainContext.makeThreadMainContext();
        try {
            WGA wga = getRootResource().getWga();
            fetchApiModule(wga, apiName);
        
            TMLScript tmlScript = getDatabaseResource().getContextWga().tmlscript();
            
            ScriptContext scriptContext = new ScriptContext(getRootResource().getUriInfo().getRequestUriBuilder(), this, files);
            Map<String,Object> extraObjects = new HashMap<String,Object>();
            Object jsonInput = tmlScript.exportJsonData(input);
            extraObjects.put("$input", jsonInput);
            extraObjects.put("$restContext", scriptContext);
            
            if (tmlScript.hasProperty(_apiObject.get(), "post")) {
                try {
                    Object result = tmlScript.callMethod(_apiObject.get(), "post", extraObjects, Arrays.asList(new Object[] {jsonInput, scriptContext}));
                    JsonObject jsonResult = (JsonObject) tmlScript.importJsonData(result);
                    return buildResponse(jsonResult, scriptContext);
                }
                catch (WGException e) {
                    throw handleScriptException(e);
                }
            }
            else {
                throw new WebApplicationException("Custom API '" + apiName + "' does not implement method POST on collection", 400);
            }
        }
        finally {
            dropThreadStatus();
            mainContext.removeThreadMainContext();
        }
                        
        
    }

    public static String getTMLScriptErrorMessage(TMLScriptException exception) {
    
        Object errorValue = exception.getErrorValue();
        if (errorValue instanceof String) {
            return (String) errorValue;
        }
        else if (ExpressionEngineFactory.getTMLScriptEngine().determineTMLScriptType(errorValue) == RhinoExpressionEngine.TYPE_SCRIPTABLE) {
            try {
                JsonStructure errorJson = Json.createReader(new StringReader(ExpressionEngineFactory.getTMLScriptEngine().convertScriptableToJson(errorValue))).read();
                if (errorJson instanceof JsonObject) {
                    JsonObject errorObject = (JsonObject) errorJson;
                    JsonValue errorMsg = errorObject.get("message");
                    if (errorMsg instanceof JsonString) {
                        return ((JsonString) errorMsg).getString();
                    }
                }
            }
            catch (Throwable e) {}
        }
        
        return exception.getMessage();
        
    }

    public static int getTMLScriptErrorStatus(TMLScriptException exception) {
    
        Object errorValue = exception.getErrorValue();
        if (errorValue instanceof Number) {
            return ((Number) errorValue).intValue();
        }
        else if (ExpressionEngineFactory.getTMLScriptEngine().determineTMLScriptType(errorValue) == RhinoExpressionEngine.TYPE_SCRIPTABLE) {
            try {
                JsonStructure errorJson = Json.createReader(new StringReader(ExpressionEngineFactory.getTMLScriptEngine().convertScriptableToJson(errorValue))).read();
                if (errorJson instanceof JsonObject) {
                    JsonObject errorObject = (JsonObject) errorJson;
                    JsonValue errorCode = errorObject.get("code");
                    if (errorCode instanceof JsonNumber) {
                        return ((JsonNumber) errorCode).intValue();
                    }
                }
            }
            catch (Throwable e) {}
        }
        
        return 500;
        
    }
    
    public static WGException handleScriptException(WGException e) {
        TMLScriptException scriptException = WGUtils.getCauseOfType(e, TMLScriptException.class);
        if (scriptException != null) {
            int errorCode = getTMLScriptErrorStatus(scriptException);
            String errorMsg = getTMLScriptErrorMessage(scriptException);
            Object errorValue = scriptException.getErrorValue();
            throw new WebApplicationException(errorMsg, e, errorCode);
        }
        return e;
    }
    


}
