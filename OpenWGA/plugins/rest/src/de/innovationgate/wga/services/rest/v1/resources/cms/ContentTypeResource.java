package de.innovationgate.wga.services.rest.v1.resources.cms;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.PUT;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

@XmlRootElement(name=ContentTypeResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(ContentTypeResource.RESOURCE_TYPE)
public class ContentTypeResource extends DocumentResource<WGContentType,CmsApiResource> {
    
    public static final String RESOURCE_TYPE = "contentType";

    public ContentTypeResource() {
        super();
    }

    public ContentTypeResource(CmsApiResource parent, WGContentType doc) throws WGAPIException {
        super(parent, doc, parent.getURI().path(CmsApiResource.REFLIST_CONTENTTYPES).path(doc.getName()));
    }
    
    public ContentTypeResource(CmsApiResource parent, String newDocName) throws WGAPIException {
        super(parent, newDocName, parent.getURI().path(CmsApiResource.REFLIST_CONTENTTYPES).path(newDocName));
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {
        throw new IllegalArgumentException("Unknown reference list: " + list.getIdString());
    }

    @Override
    protected void addReferences(References refs) throws UnavailableResourceException {
    }
    
    @Override
    protected Set<String> getDefaultMetaSet() {

        Set<String> metas = new HashSet<String>();
        metas.add(WGContentType.META_ALLOWED_POSITIONS);
        metas.add(WGContentType.META_AUTHORING);
        metas.add(WGContentType.META_DESCRIPTIONALIASES);
        metas.add(WGContentType.META_EDITORS);
        metas.add(WGContentType.META_INNER_LAYOUT);
        metas.add(WGContentType.META_NAMEALIASES);
        metas.add(WGContentType.META_OUTER_LAYOUT);
        metas.add(WGContentType.META_POSITIONING);
        metas.add(WGContentType.META_PREFERREDPARENT);
        metas.add(WGContentType.META_WORKFLOW);
        metas.add(WGContentType.META_DESCRIPTION);
        metas.add(WGContentType.META_EVENT_CREATECONTENT);
        metas.add(WGContentType.META_EVENT_SAVECONTENT);
        metas.add(WGContentType.META_EVENT_STATUSCHANGE);
        metas.add(WGContentType.META_EVENT_WORKFLOWMAIL);
        return metas;
        
    }
    
    @PUT
    @Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
    public Response put(ContentTypeResource res) throws WGException {
        return defaultPut(res);
    }
    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    @Override
    protected WGContentType putCreateDocument(Resource<?> res) throws WGAPIException {
        return getDatabaseResource().getDatabase().db().createContentType(getNewDocName());
    }
    
    @Override
    public String getId() throws WGException {
        return getDoc().getName();
    }
}


