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
import javax.ws.rs.PathParam;
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
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.webgate.api.WGStructEntryIterator;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;

@XmlRootElement(name=PageResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(PageResource.RESOURCE_TYPE)
public class PageResource extends DocumentResource<WGStructEntry,CmsApiResource> {
    
    public static class PageWriteContext extends WriteContext {
        
        public String newContentType = null;
        
    }
    
    public static final String REF_PARENTPAGE = "parentPage";
    public static final String REF_AREA = "area";
    public static final String REF_NEXT = "next";
    public static final String REF_PREVIOUS = "previous";
    public static final String REFLIST_NAVIGATION = "navigation";
    public static final String REFLIST_RELEASEDCONTENT = "releasedContent";
    public static final String REFLIST_CHILDPAGES = "childPages";
    public static final String RESOURCE_TYPE = "page";

    public PageResource() {
        super();
    }

    public PageResource(CmsApiResource parent, WGStructEntry doc) {
        super(parent, doc, parent.getURI().path(CmsApiResource.REFLOOKUP_PAGES).path(String.valueOf(doc.getStructKey())));
    }
    
    @Override
    protected de.innovationgate.wga.services.rest.v1.resources.DocumentResource.WriteContext createWriteContext() {
        return new PageWriteContext();
    }
    
    @Path("{language}/{version}")
    public CmsContentResource getContent(@PathParam("language") String language, @PathParam("version") int version) throws WGAPIException {
        
        WGContentKey contentKey = new WGContentKey(getDoc().getStructKey(), language, version);
        WGContent content = getDoc().getDatabase().getContentByKey(contentKey);
        if (content != null) {
            return new CmsContentResource(this, content);
        }
        else {
            throw new WebApplicationException("No content of key '" + contentKey.toString() + "'", 404);
        }
        
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {

        if (REFLIST_CHILDPAGES.equals(list.getIdString())) {
            WGStructEntryIterator childEntries = getDoc().getChildEntryIterator(getIteratorPageSize(list));
            list.addUntilFull(childEntries);
        }
        else if (REFLIST_RELEASEDCONTENT.equals(list.getIdString())) {
            list.noPaging();
            String role = getRootResource().getWga().getRequest().getParameter(RestApplication.URLPARAM_ROLE);
            if (role == null) {
                role = WGContent.DISPLAYTYPE_NONE;
            }
            for (WGContent content : getDoc().getContentSet(false).getReleasedContent().values()) {
                if (content.isHiddenFrom().contains(role)) {
                    continue;
                }
                
                list.add(content.getLanguage().getName(), new CmsContentResource(this, content));
            }
            
        }
        else if (REFLIST_NAVIGATION.equals(list.getIdString())) {
            list.noPaging();
            WGStructEntry prev = getDoc().getPreviousSibling();
            if (prev != null) {
                list.add(REF_PREVIOUS, new PageResource(getParentResource(), prev));
            }
            
            WGStructEntry next = getDoc().getNextSibling();
            if (next != null) {
                list.add(REF_NEXT, new PageResource(getParentResource(), next));
            }            
        }
        
        else {
            WGLanguage lang = getDoc().getDatabase().getLanguage(list.getIdString());
            if (lang == null || lang.isDummy()) {
                throw new IllegalArgumentException("Unknown reference list: " + list.getIdString());
            }
            
            list.noPaging();
            for (WGContent content : getDoc().getContentSet(true).getAllContent().values()) {
                if (content.getLanguage().getName().equals(list.getIdString())) {
                    list.add(new CmsContentResource(this, content));
                }
            }
        }
        
        
        
    }
    
    @POST
    @Path(REFLIST_CHILDPAGES)
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postToChildPages(PageResource pageResource) throws WGException {
            return createChildPage(pageResource);
    }
    
    @POST
    @Path("{language}")
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postToContent(CmsContentResource contentResource, @PathParam("language") String language) throws WGException {
        return createContent(contentResource, language);
    }
    

    private Response createContent(CmsContentResource content, String language) throws WGException {
        
        Iterator<Field> fields = content.getMetaData().getFields();
        Field titleField = null;
        while (fields.hasNext()) {
            Field field = fields.next();
            if (WGStructEntry.META_TITLE.equalsIgnoreCase(field.name)) {
                titleField = field;
            }
        }
        
        WGLanguage lang = getDatabaseResource().getDatabase().db().getLanguage(language);
        if (lang == null || lang.isDummy()) {
            throw new WebApplicationException("Unknown language '" + language + "'", 409);
        }
        
        String title = (titleField != null ? (String) titleField.values.get(0) : "Content " + UIDGenerator.generateUID());
        WGContent newContent;
        WGContent releasedContent = getDoc().getReleasedContent(lang.getName());
        if (releasedContent != null) {
            WGContent draftContent = getDoc().getContent(lang.getName(), WGContent.STATUS_DRAFT);
            if (draftContent != null) {
                throw new WebApplicationException("A draft for language '" + lang.getName() + " already exists", 409);
            }
            newContent = releasedContent.createDraftCopy();
        }
        else {
            newContent = getDoc().createContent(lang, title);
        }
        
        CmsContentResource contentResource = new CmsContentResource(this, newContent);
        return contentResource.defaultPut(content);
        
    }

    @GET
    @Path(REFLIST_CHILDPAGES)
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response getChildPages(@QueryParam("offset") @DefaultValue(value="0") int offset, @QueryParam("size") @DefaultValue(value="10") int size) throws Exception {
        return getSubCollection(REFLIST_CHILDPAGES, offset, size);
    }
    

    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException, UnavailableResourceException {
        refs.add(new ReferenceCollection(this, REFLIST_CHILDPAGES, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_RELEASEDCONTENT, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_NAVIGATION, getURI()));
        
        for (WGLanguage l : getDoc().getDatabase().getLanguages().values()) {
            refs.add(new ReferenceCollection(this, l.getName(), getURI()));
        }
        
        AreaResource areaResource = new AreaResource(getParentResource(), getDoc().getArea());
        refs.add(new ResourceReference(REF_AREA, autoSuffix(areaResource.getURI()), AreaResource.RESOURCE_TYPE));
        
        if (!getDoc().isRoot()) {
            PageResource parentPageResource = new PageResource(getParentResource(), getDoc().getParentEntry());
            refs.add(new ResourceReference(REF_PARENTPAGE, autoSuffix(parentPageResource.getURI()), RESOURCE_TYPE));
        }
    }
    
    public Response createChildPage(PageResource page) throws WGException {
        
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
        
        if (ctField == null) {
            throw new WebApplicationException("Metadata field '" + WGStructEntry.META_CONTENTTYPE.toLowerCase() + "' necessary to create page");
        }
        
        String ctName = (String) ctField.values.get(0);
        WGContentType ct = getDatabaseResource().getDatabase().db().getContentType(ctName);
        if (ct == null) {
            throw new WebApplicationException("Unknown content type '" + ctName + "'");
        }
        
        String title = (titleField != null ? (String) titleField.values.get(0) : "Page " + UIDGenerator.generateUID());
        WGStructEntry newEntry = getDoc().createChildEntry(ct, title);
        PageResource pageResource = new PageResource(getParentResource(), newEntry);
        return pageResource.defaultPut(page);
        
    }
    
    @Override
    protected Set<String> getDefaultMetaSet() {

        Set<String> metas = new HashSet<String>();
        metas.add(WGStructEntry.META_TITLE);
        metas.add(WGStructEntry.META_PAGE_EDITORS);
        metas.add(WGStructEntry.META_CHILD_EDITORS);
        metas.add(WGStructEntry.META_CONTENTTYPE);
        metas.add(WGStructEntry.META_POSITION);
        metas.add(WGStructEntry.META_READERS);
        metas.add(WGStructEntry.META_UNIQUENAME);
        metas.add(WGStructEntry.META_WORKFLOW_NAME);
        return metas;

        
    }
    
    @Override
    public void fillResource(Envelope env) throws WGAPIException {
        super.fillResource(env);
        env.timestamps.addPagePublishedDates(getDoc().getPublished());
    }
    

    @Override
    protected void writeMetaData(WriteContext writeContext, de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field f) throws WGAPIException {
        
        PageWriteContext pageContext = (PageWriteContext) writeContext;
        
        if (WGStructEntry.META_CONTENTTYPE.equalsIgnoreCase(f.name)) {
            String ct = (String) f.values.get(0);
            if (!getDoc().getContentType().getName().equals(ct)) {
                pageContext.newContentType = ct;
            }
        }
        else {
            super.writeMetaData(writeContext, f);
        }
    }
    
    @Override
    public void saveData(de.innovationgate.wga.services.rest.v1.resources.DocumentResource.WriteContext writeContext) throws WGAPIException {

        PageWriteContext pageContext = (PageWriteContext) writeContext;
        if (pageContext.newContentType != null) {
            WGContentType newCT = getDoc().getDatabase().getContentType(pageContext.newContentType);
            if (newCT != null) {
                getDoc().changeContentType(newCT);
            }
            else {
                throw new WebApplicationException("Cannot change to unknown content type '" + pageContext.newContentType + "'", 409);
            }
        }
        else {
            super.saveData(writeContext);
        }
        
        
    }
    
    @PUT
    @Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
    public Response put(PageResource res) throws WGException {
        return defaultPut(res);
    }
    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    @Override
    protected WGStructEntry putCreateDocument(Resource<?> res) {
        throw new WebApplicationException("Creating a page via PUT is not supported. POST to collection 'rootPages' on areas or 'childPages' on other pages instead.");
    }
    
    @Override
    public String getId() throws WGException {
        return getDoc().getStructKey().toString();
    }

}
