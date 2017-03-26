package de.innovationgate.wga.services.rest.v1.resources;

import java.net.URI;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorNode;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.services.rest.CustomCollectionEnhancer;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field;
import de.innovationgate.wga.services.rest.v1.resources.cms.AreaResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.CmsApiResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.CmsContentResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.CmsRelationGroupResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.ContentTypeResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.LanguageResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.PageResource;
import de.innovationgate.wga.services.rest.v1.resources.cms.UserProfileResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelApiResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelContentResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelDefinitionResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelModelDocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.hdbmodel.HdbmodelRelationGroupResource;
import de.innovationgate.wga.services.rest.v1.resources.query.QueryApiResource;
import de.innovationgate.wga.services.rest.v1.types.FieldList;
import de.innovationgate.wga.services.rest.v1.types.JSONFieldList;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;
import de.innovationgate.wga.services.rest.v1.types.Status;
import de.innovationgate.wga.services.rest.v1.types.XMLFieldList;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;

@XmlRootElement
@XmlDiscriminatorNode("@type")
@XmlSeeAlso({DocumentResource.class,DatabaseResource.class,AreaResource.class,PageResource.class, CmsApiResource.class, RootResource.class,CmsContentResource.class, CmsRelationGroupResource.class,ContentTypeResource.class,LanguageResource.class,UserProfileResource.class,HdbmodelContentResource.class,HdbmodelApiResource.class,HdbmodelRelationGroupResource.class,HdbmodelDefinitionResource.class,HdbmodelModelDocumentResource.class,QueryApiResource.class})
public abstract class Resource<ParentType extends Resource<?>> {
    
    public static class Parameters {
        public List<String> enhancers = new ArrayList<String>();
        
        public void addParameter(String name, String value) {
            
            if (RestApplication.URLPARAM_ENHANCER.equals(name)) {
                enhancers.add(value);
            }
            
        }
    }

    private ParentType _parentResource;
    private UriBuilder _uri;
    private RootResource _rootResource;
    private ApiResource _apiResource;
    private DatabaseResource _databaseResource;
    private boolean _empty = false;

    protected boolean isContainedRefs() {
        return true;
    }

    public Resource(ParentType parentResource, UriBuilder uri) {
        _parentResource = parentResource;
        _uri = uri;
        _rootResource = getAncestorResource(RootResource.class);
        if (!(this instanceof RootResource || this instanceof DatabaseResource)) {
            _apiResource = getAncestorResource(ApiResource.class);
        }
        if (!(this instanceof RootResource)) {
            _databaseResource = getAncestorResource(DatabaseResource.class);
        }
    }
    
    public Resource() {
    }

    @XmlTransient
    public UriBuilder getURI() {
        return _uri.clone();
    }
    
    @XmlTransient
    public ParentType getParentResource() {
        return _parentResource;
    }
    
    @SuppressWarnings("unchecked")
    @XmlTransient
    public <AncestorResource> AncestorResource getAncestorResource(Class<? extends AncestorResource> resClass) {
        
        Resource<?> parent = this;
        while (true) {
            if (resClass.isAssignableFrom(parent.getClass())) {
                return (AncestorResource) parent;
            }
            
            parent = parent.getParentResource();
            if (parent == null) {
                return null;
            }
        }
        
    }
    
    @XmlTransient
    public RootResource getRootResource() {
        return _rootResource;
    }
    
    @XmlTransient
    public ApiResource getApiResource() {
        return _apiResource;
    }
    
    protected Parameters parseResourceParameters() throws WGException {

        Parameters params = new Parameters();
        HttpServletRequest req = getRootResource().getWga().getRequest();
        @SuppressWarnings("unchecked")
        Enumeration<String> names = req.getParameterNames();
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            if (RestApplication.COLLECTION_ENHANCING_URLPARAMS.contains(name)) {
                for (String value : req.getParameterValues(name)) {
                    params.addParameter(name, value);
                }
            }
        }
        return params;
        
    }

    protected Map<String, ReferenceCollection.Parameters> parseExpandedRefs(List<String> expandedRefsStr) {
        
        Map<String, ReferenceCollection.Parameters> instructions = new HashMap<String, ReferenceCollection.Parameters>();
        for (String refStr : expandedRefsStr) {
            List<String> parts = WGUtils.deserializeCollection(refStr, ",", true);
            ReferenceCollection.Parameters instruction = new ReferenceCollection.Parameters();
            instruction.colName = parts.get(0);
            for (int partsIdx = 1; partsIdx < parts.size() ; partsIdx++) {
                String part = parts.get(partsIdx);
                String name;
                String value = null;
                int equalPos = part.indexOf("=");
                if (equalPos != -1) {
                    name = part.substring(0, equalPos);
                    value = part.substring(equalPos+1);
                }
                else {
                    name = part;
                }
                instruction.addParameter(name, value);
            }
            instructions.put(instruction.colName, instruction);
        }
        return instructions;
        
    }

    protected int getDefaultRefListSize() {
        return -1;
    }

    @XmlTransient
    public void fillResource(Envelope env) throws WGAPIException {
        env.resource = this;
    }

    @XmlTransient
    public Response buildAuthorisationExceptionResponse(WGAuthorisationException e, Envelope env) {
        env.status = Status.ERROR;
        env.error = new RestError();
        env.error.code = Response.Status.FORBIDDEN.getStatusCode();
        env.error.message = e.getMessage();
        env.error.type = e.getClass().getName();
        return buildResponse(Response.status(Response.Status.FORBIDDEN).entity(env));
    }

    @XmlTransient
    public Response buildLockedExceptionResponse(Envelope env, ResourceIsLockedException e) {
        env.status = Status.ERROR;
        env.error = new RestError();
        env.error.code = Response.Status.CONFLICT.getStatusCode();
        env.error.message = e.getMessage();
        env.error.type = e.getClass().getName();
        return buildResponse(Response.status(Response.Status.CONFLICT).entity(env));
    }

    @XmlTransient
    public abstract String getResourceType();
    
    @XmlTransient
    public Response buildResponse(Response.ResponseBuilder builder) {
        
        RootResource root = getRootResource();
        builder.type(root.getOutputMediaType()).encoding(root.getApplication().getEncoding());
        return builder.build(); 
    }

    @XmlTransient
    public FieldList createFieldList(List<Field> items) {
    
        FieldList fieldList;
        MediaType outputType = getRootResource().getOutputMediaType();
        if (outputType.equals(MediaType.APPLICATION_JSON_TYPE)) {
            fieldList = new JSONFieldList();
            
        }
        else {
            fieldList = new XMLFieldList();
        }
        
        if (items != null) {
            for (Field field : items) {
                fieldList.addField(field);
            }
        }
        return fieldList;
        
    }

    protected WGContentKey parseTargetContentKey(ResourceReference ref) {
    
        if (ref.href == null) {
            throw new WebApplicationException("Relation '" + ref.getIdString() + "' neither has uri property nor is marked as deleted", 409);
        }
        
        String[] path = ref.href.getPath().split("/");
    
        if (path.length < 3) {
            throw new WebApplicationException("Invalid content URI '" + ref.href.toString() + "' for relation '" + ref.getIdString() + "'", 409);
        }
        
        return new WGContentKey(path[path.length - 3], path[path.length - 2], Integer.parseInt(path[path.length - 1]));
        
    }

    @XmlTransient
    public URI autoSuffix(UriBuilder uriBuilder) {
        
        URI uri = uriBuilder.build();
        
        String url;
        try {
            url = getRootResource().getWga().getRequest().getRequestURI();
            if (url.endsWith(".xml")) {
                return UriBuilder.fromUri(uri).replacePath(uri.getPath() + ".xml").build();
            }
            else if (url.endsWith(".json")) {
                return UriBuilder.fromUri(uri).replacePath(uri.getPath() + ".json").build();
            }
        }
        catch (WGException e) {
        }
        
        return uri;
        
    }

    @XmlTransient
    public void enhanceReference(ResourceReference ref, ReferenceCollection.Parameters parameters) throws WGException {
    }
    
    @XmlTransient
    public void enhanceResource(Resource<?> ref, Parameters parameters) throws WGException {
    }
    
    @XmlTransient
    public Date getLastModified() throws WGException {
        return null;
    }

    @XmlTransient
    public DatabaseResource getDatabaseResource() {
        return _databaseResource;
    }
    
    @XmlTransient
    public abstract String getId() throws WGException;
    
    @XmlTransient
    public Reference createCollectionReference(ReferenceCollection list, String requestedId, boolean autoSuffix) throws WGException, TMLScriptException {
        if (requestedId == null) {
            requestedId = getId();
        }
        
        URI refUri = !isLinkable() ? null : autoSuffix ? autoSuffix(getURI()) : getURI().build();
        ResourceReference ref = new ResourceReference(requestedId, refUri, getResourceType());
        ReferenceCollection.Parameters parameters = list.getParameters();
        enhanceReference(ref, parameters);
        if (parameters != null) {
            
            if (ref.items == null) {
                ref.items = createFieldList(null);
            }
            if (ref.metaData == null) {
                ref.metaData = createFieldList(null);
            }
            
            // Special enhancer which may have been injected from the queried resource
            if (parameters.specialEnhancer != null) {
                try {
                    parameters.specialEnhancer.enhance(ref, getCustomEnhancerContextDoc());
               }
               catch (Throwable e) {
                   RestApplication.LOG.error("Error calling special REST enhancer", e);
               }   
            }
                
            // Custom collection enhancers from URL parameters
            for (String enhancerName : parameters.enhancers) {
                Database database = getDatabaseResource().getDatabase();
                if (database instanceof App) {
                    try {
                         CustomCollectionEnhancer enhancer = new CustomCollectionEnhancer(getRootResource().getWga(), (App) database, enhancerName);
                         enhancer.enhance(ref, getCustomEnhancerContextDoc());
                    }
                    catch (Throwable e) {
                        RestApplication.LOG.error("Error calling custom REST enhancer '" + enhancerName + "'", e);
                    }
                }
            }
            
            if (ref.items.size() == 0) {
                ref.items = null;
            }
            if (ref.metaData.size() == 0) {
                ref.metaData = null;
            }
            
        }
        return ref;
    }

    protected WGDocument getCustomEnhancerContextDoc() {
        return null;
    }
    
    @XmlTransient
    public boolean isLinkable() {
        return true;
    }

    @XmlTransient
    public boolean isEmpty() {
        return _empty;
    }

    @XmlTransient
    public void setEmpty(boolean empty) {
        _empty = empty;
    }

}
