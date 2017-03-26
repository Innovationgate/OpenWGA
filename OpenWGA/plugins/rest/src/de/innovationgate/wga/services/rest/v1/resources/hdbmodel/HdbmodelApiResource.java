package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.common.beans.hdbmodel.Storage;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.AbstractContentResource;
import de.innovationgate.wga.services.rest.v1.resources.ApiResource;
import de.innovationgate.wga.services.rest.v1.resources.DatabaseResource;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.resources.ExternalRefsResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.DocumentKey;
import de.innovationgate.wga.services.rest.v1.types.LookupReference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.hdb.HDBModelException;
import de.innovationgate.wgpublisher.hdb.HDBModelParams;
import de.innovationgate.wgpublisher.hdb.HDBModelProcess;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.UniqueNamePartFormatter;

@XmlRootElement
@XmlDiscriminatorValue(HdbmodelApiResource.RESOURCE_TYPE)
public class HdbmodelApiResource extends ExternalRefsResource<DatabaseResource> implements ApiResource {
    
    public static final String RESOURCE_TYPE = "hdbmodel";
    
    public static final String REFLOOKUP_PAGES = "$byPageKey";

    public static final String REF_MODEL = "$model";

    private HDBModel _model;
    
    public HdbmodelApiResource() {
        super();
    }
    
    public HdbmodelApiResource(DatabaseResource databaseResource, HDBModel model) {
        super(databaseResource, databaseResource.getURI().path(RESOURCE_TYPE));
        _model = model;
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {

        WGContent storage = getParentResource().getDatabase().db().getContentByName(list.getIdString());
        if (storage == null) {
            throw new WebApplicationException("Unknown storage '" + list.getIdString() + "'", 404);
        }
        
        SkippingIterator<WGContent> contents = storage.getChildContentIterator(getDefaultRefListSize());
        list.addUntilFull(contents);
        
    }
   

    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException, UnavailableResourceException {
        
        refs.add(new LookupReference(REFLOOKUP_PAGES, autoSuffix(getURI().path(REFLOOKUP_PAGES)), "/{pageKey}", HdbmodelContentResource.RESOURCE_TYPE));
        refs.add(new ResourceReference(REF_MODEL, autoSuffix(getURI().path(REF_MODEL)), HdbmodelDefinitionResource.RESOURCE_TYPE));
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGArea hdbArea = db.getArea(WGHierarchicalDatabase.AREA_CONTENT);
        Iterator<WGContent> contents = hdbArea.getRootContentIterator(db.getDefaultLanguage(), getDefaultRefListSize());
        while (contents.hasNext()) {
            WGContent content = contents.next();            
            String storageId = HDBModel.getID(content);
            refs.add(new ReferenceCollection(this, storageId, getURI()));
        }
    }
    
    @Path("{storage}/{content}")
    public HdbmodelContentResource getContent(@PathParam("storage") String storageId, @PathParam("content") String contentId) throws WGAPIException {
        
        String name = UniqueNamePartFormatter.INSTANCE.format(storageId) + "." + UniqueNamePartFormatter.INSTANCE.format(contentId);
        WGContent content = getParentResource().getDatabase().db().getContentByName(name);
        if (content != null) {
            return new HdbmodelContentResource(this, content);
        }
        else {
            throw new WebApplicationException("Unknown content '" + name + "'", 404);
        }
        
    }
    
    @Path(REF_MODEL)
    public AbstractModelResource<?> getModelResource() {
        return new HdbmodelDefinitionResource(this, _model.getDefinition());
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    protected HDBModel getModel() {
        return _model;
    }
    
    @Path(REFLOOKUP_PAGES + "/{key}")
    public HdbmodelContentResource getContentByPageKey(@PathParam("key") String structKey) throws WGAPIException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGStructEntry page = db.getStructEntryByKey(db.parseStructKey(structKey));
        if (page != null) {
            WGContent content = page.getReleasedContent(page.getDatabase().getDefaultLanguage());
            return (HdbmodelContentResource) wrapIntoResource(content);
        }
        else {
            throw new WebApplicationException("No page of key '" + structKey + "'", 404);
        }
        
    }
    
    @Override
    public Resource<?> wrapIntoResource(Object resourceObject) throws WGAPIException {
        
        if (resourceObject instanceof TMLContext) {
            resourceObject = ((TMLContext) resourceObject).content();
        }
        
        if (resourceObject instanceof WGContent) {
            WGContent content = (WGContent) resourceObject;
            List<WGContent> pathContents = new ArrayList<WGContent>();
            WGContent pathContent = content;
            while (pathContent != null) {
                pathContents.add(pathContent);
                pathContent = pathContent.getParentContent();
            }
            Collections.reverse(pathContents);
            
            Resource<?> resource = this;
            for (WGContent con : pathContents) {
                if (HDBModel.isContent(con)) {
                    resource = new HdbmodelContentResource(resource, con);
                }
            }
            
            return (AbstractContentResource<?>) resource;
        }
        else if (resourceObject == null){
            throw new WebApplicationException("Null resource object", 500);
        }
        else {
            throw new WebApplicationException("Unwrappeable resource object: "  + resourceObject.getClass().getName(), 500);
        }
    }
    
    public String determineContentClass(final HdbmodelContentResource contentResource) throws WGException {
        Iterator<Field> fields = contentResource.getMetaData().getFields();
        Field ccField = null;
        while (fields.hasNext()) {
            Field field = fields.next();
            if (WGContent.META_CONTENTCLASS.equalsIgnoreCase(field.name)) {
                ccField = field;
            }
        }
        
        if (ccField == null || ccField.values.size() == 0) {
            return null;
        }
        
        String contentClass = (String) ccField.values.get(0);
        return contentClass;
    }
    
    @POST
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postContent(final HdbmodelContentResource contentResource) throws WGException {
        return createChildContent(contentResource, null, null);
    }

    @Path("{storage}")
    @POST
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postContent(final HdbmodelContentResource contentResource, @PathParam("storage") String storageId) throws WGException {
        return createChildContent(contentResource, storageId, null);
    }

    protected Response createChildContent(final HdbmodelContentResource contentResource, String storageId, WGContent relContent) throws WGException, WGAPIException, HDBModelException {
        String contentClass = determineContentClass(contentResource);
        if (contentClass == null) { // No content class given. Try to determine it from the given storage parameter
        
            if (storageId == null) {
                throw new WebApplicationException("Metadata field '" + WGContent.META_CONTENTCLASS.toLowerCase() + "' necessary to create content on this URL", 409);
            }
            
            WGContent storage = _model.getDocument(storageId, relContent);
            if (storage == null) {
                throw new WebApplicationException("Unknown storage '" + storageId + "'", 404);
            }
            
            Document documentModel = _model.getModelForContent(storage);
            if (!(documentModel instanceof Storage)) {
                throw new WebApplicationException("Unknown storage '" + storageId + "'", 404);
            }
            
            Storage storageModel = (Storage) documentModel;
            if (storageModel.getChildContents().size() == 0) {
                throw new WebApplicationException("No child contents for storage '" + storageId + "' defined", 409);
            }
            
            if (storageModel.getChildContents().size() > 1) {
                throw new WebApplicationException("Multiple child content classes for storage '" + storageId + "' defined. Please indicate the one you want to create with metadata field '" + WGContent.META_CONTENTCLASS.toLowerCase() + "'.", 409);
            }
            
            Content content = storageModel.getChildContents().get(0);
            contentClass = content.getContentClass();
        
        }
        
        HDBModelParams hdbParams = _model.newCreateContentParams(contentClass, relContent);
        hdbParams.setProcess(new HDBModelProcess() {
            @Override
            public void run(WGContent content, Map<String, Object> params) throws Throwable {
                HdbmodelContentResource newContentResource = (HdbmodelContentResource) wrapIntoResource(content);
                newContentResource.writeDocumentData(contentResource);
            }
        });
        
        
        try {
            WGContent newContent = _model.createContent(hdbParams);
            HdbmodelContentResource newContentResource = (HdbmodelContentResource) wrapIntoResource(newContent);
            Envelope env = new Envelope();
            env.self = autoSuffix(newContentResource.getURI());
            
            if (getRootResource().getUriInfo().getQueryParameters().containsKey(RestApplication.URLPARAM_RETURNRESOURCE)) {
                newContentResource.fillResource(env);
            }
            else {
                env.key = new DocumentKey(newContentResource, newContent.getDatabase(), newContent.getDocumentKeyObj());
                env.key.hdbmodelContentId = HDBModel.getID(newContent);
            }
            
            return buildResponse(Response.ok(env));
        }
        catch (HDBModelException e) {
            throw new WebApplicationException(e.getMessage(), 409);
        }
    }
    
    @Override
    public String getId() throws WGException {
        return RESOURCE_TYPE;
    }

}
