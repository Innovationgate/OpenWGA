package de.innovationgate.wga.services.rest.v1.resources.cms;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.utils.UIDGenerator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchyContentIterator;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGStructEntryIterator;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.ElementFilter;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

@XmlRootElement(name=AreaResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(AreaResource.RESOURCE_TYPE)
public class AreaResource extends DocumentResource<WGArea,CmsApiResource> {
    
    public static final String REFLIST_ROOTPAGES = "rootPages";
    public static final String REFLIST_ROOTCONTENT = "rootContent";
    public static final String RESOURCE_TYPE = "area";

    public AreaResource() {
        super();
    }

    public AreaResource(CmsApiResource parent, String newDocName) {
        super(parent, newDocName, parent.getURI().path(CmsApiResource.REFLIST_AREAS).path(newDocName));
    }
    
    public AreaResource(CmsApiResource parent, WGArea doc) throws WGAPIException {
        super(parent, doc, parent.getURI().path(CmsApiResource.REFLIST_AREAS).path(doc.getName()));
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {

        if (REFLIST_ROOTPAGES.equals(list.getIdString())) {

            WGStructEntryIterator rootEntries = getDoc().getRootEntryIterator(getIteratorPageSize(list));
            list.addUntilFull(rootEntries);
        }
        
        else if (REFLIST_ROOTCONTENT.equals(list.getId().getName())) {
            
            final String role = getRootResource().getWga().getRequest().getParameter(RestApplication.URLPARAM_ROLE);
            String language = list.getId().getParams().get("language");
            if (language == null) {
                throw new WebApplicationException("Invalid root content URL: Parameter 'language' is missing", 400);
            }
                    
            WGHierarchyContentIterator rootEntries = getDoc().getRootContentIterator(language, getIteratorPageSize(list));
            list.setFilter(new ElementFilter() {

                @Override
                public Object filterElement(Object obj) throws WGException {
                    WGContent content = (WGContent) obj;
                    if (role == null || !content.isHiddenFrom().contains(role)) {
                        return content;
                    }
                    else {
                        return null;
                    }
                    
                }
            });
            list.addUntilFull(rootEntries);
        }
        
        else {
            throw new IllegalArgumentException("Unknown reference list: " + list.getIdString());
        }
        
    }
    
    @POST
    @Path(REFLIST_ROOTPAGES)
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postToRootPages(PageResource resource) throws WGException {
        return createRootPage(resource);
    }

    @GET
    @Path(REFLIST_ROOTPAGES)
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response getRootPages(@QueryParam("offset") @DefaultValue(value="0") int offset, @QueryParam("size") @DefaultValue(value="10") int size) throws Exception {
        return getSubCollection(REFLIST_ROOTPAGES, offset, size);
    }
    
    public Response createRootPage(PageResource page) throws WGException {
        
        Iterator<Field> fields = page.getMetaData().getFields();
        Field ctField = null;
        Field titleField = null;
        while (fields.hasNext()) {
            Field field = fields.next();
            if (WGStructEntry.META_CONTENTTYPE.equalsIgnoreCase(field.name)) {
                ctField = field;
            }
            if (WGStructEntry.META_TITLE.equalsIgnoreCase(field.name)) {
                titleField = field;
            }
        }
        
        if (ctField == null || ctField.values.size() == 0) {
            throw new WebApplicationException("Metadata field '" + WGStructEntry.META_CONTENTTYPE.toLowerCase() + "' necessary to create page", 409);
        }
        
        String ctName = (String) ctField.values.get(0);
        WGContentType ct = getDatabaseResource().getDatabase().db().getContentType(ctName);
        if (ct == null) {
            throw new WebApplicationException("Unknown content type '" + ctName + "'", 409);
        }
        
        String title = (titleField != null ? (String) titleField.values.get(0) : "Page " + UIDGenerator.generateUID());
        WGStructEntry newEntry = getDoc().createRootEntry(ct, title);
        PageResource pageResource = new PageResource(getParentResource(), newEntry);
        return pageResource.defaultPut(page);
        
    }


    @Override
    public void addReferences(References refs) throws WGAPIException, UnavailableResourceException {
        refs.add(new ReferenceCollection(this, REFLIST_ROOTPAGES, getURI()));
        for (WGLanguage l : getDoc().getDatabase().getLanguages().values()) {
            Reference.Id colId = Reference.Id.create(REFLIST_ROOTCONTENT).param("language", l.getName());
            refs.add(new ReferenceCollection(this, colId, getURI()));
        }

    }
    
    @Override
    protected Set<String> getDefaultMetaSet() {

        Set<String> metas = new HashSet<String>();
        metas.add(WGArea.META_EDITORS);
        metas.add(WGArea.META_READERS);
        metas.add(WGArea.META_SYSTEM);
        metas.add(WGArea.META_DESCRIPTION);
        return metas;
        
    }
    
    @PUT
    @Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
    public Response put(AreaResource res) throws WGException {
        return defaultPut(res);
    }
    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Override
    protected WGArea putCreateDocument(Resource<?> res) throws WGAPIException {
        return getDatabaseResource().getDatabase().db().createArea(getNewDocName());
    }
    
    @Override
    public String getId() throws WGException {
        return getDoc().getName();
    }

}

