package de.innovationgate.wga.services.rest.v1.resources.cms;

import java.io.UnsupportedEncodingException;
import java.net.URISyntaxException;
import java.util.Collections;

import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGArea;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGContentType;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.server.api.Database;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.v1.resources.ApiResource;
import de.innovationgate.wga.services.rest.v1.resources.DatabaseResource;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.resources.ExternalRefsResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.resources.RestError;
import de.innovationgate.wga.services.rest.v1.types.LookupReference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.Status;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

@XmlRootElement(name=CmsApiResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(CmsApiResource.RESOURCE_TYPE)
public class CmsApiResource extends ExternalRefsResource<DatabaseResource> implements ApiResource {

    public static final String REFLIST_AREAS = "areas";
    public static final String REFLOOKUP_PAGES = "pages";
    public static final String REFLOOKUP_PAGESBYNAME = "pagesByName";
    public static final String REFLOOKUP_CONTENTBYNAME = "contentByName";
    public static final String REFLOOKUP_CONTENT = "content";
    public static final String REFLIST_CONTENTTYPES = "contentTypes";
    public static final String REFLIST_LANGUAGES = "languages";
    public static final String REFLOOKUP_CONTEXT = "context";
    public static final String RESOURCE_TYPE = "cms";
    
    public CmsApiResource(DatabaseResource parent) {
        super(parent, parent.getURI().path(RESOURCE_TYPE));
    }
    
    public CmsApiResource() {
        super();
    }
    
    @Path(REFLIST_AREAS + "/{area}")
    public ExternalRefsResource<CmsApiResource> getArea(@PathParam("area") String areaName) throws WGAPIException {
        
        WGArea area = getParentResource().getDatabase().db().getArea(areaName);
        if (area != null) {
            return new AreaResource(this, area);
        }
        else {
            return new AreaResource(this, areaName);
        }
        
    }
    
    @Path(REFLOOKUP_PAGES + "/{key}")
    public PageResource getPage(@PathParam("key") String structKey) throws WGAPIException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGStructEntry page = db.getStructEntryByKey(db.parseStructKey(structKey));
        if (page != null) {
            return new PageResource(this, page);
        }
        else {
            throw new WebApplicationException("No page of key '" + structKey + "'", 404);
        }
        
    }
    
    @Path(REFLOOKUP_CONTENT + "/{key}")
    public CmsContentResource getContent(@PathParam("key") String key) throws WGException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGContent content = db.getContentByKey(WGContentKey.parse(key, db));
        if (content != null) {
            return (CmsContentResource) getApiResource().wrapIntoResource(content);
        }
        else {
            throw new WebApplicationException("No content of key '" + key + "'", 404);
        }
        
    }
    
    @Path(REFLOOKUP_PAGESBYNAME + "/{name}")
    public PageResource getPageByName(@PathParam("name") String name) throws WGAPIException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGStructEntry page = db.getStructEntryByName(name);
        if (page != null) {
            return new PageResource(this, page);
        }
        else {
            throw new WebApplicationException("No page of name '" + name + "'", 404);
        }
        
    }
    
    @Path(REFLOOKUP_CONTENTBYNAME + "/{name},{language}")
    public CmsContentResource getContentByName(@PathParam("name") String name, @PathParam("language") String language) throws WGAPIException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGContent content = db.getContentByName(name,language);
        if (content != null) {
            PageResource page = new PageResource(this, content.getStructEntry());
            return new CmsContentResource(page, content);
        }
        else {
            throw new WebApplicationException("No content of name '" + name + "' in language '" + language + "'", 404);
        }
        
    }
    
    @Path(REFLIST_CONTENTTYPES + "/{key}")
    public ContentTypeResource getContentType(@PathParam("key") String name) throws WGAPIException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGContentType ct = db.getContentType(name);
        if (ct != null) {
            return new ContentTypeResource(this, ct);
        }
        else {
            return new ContentTypeResource(this, name);
        }
        
    }
    
    @Path(REFLIST_LANGUAGES + "/{key}")
    public LanguageResource getLanguage(@PathParam("key") String name) throws WGAPIException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        WGLanguage lang = db.getLanguage(name);
        if (lang != null && !lang.isDummy()) {
            return new LanguageResource(this, lang);
        }
        else {
            return new LanguageResource(this, name);
        }
        
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {
        
        WGDatabase db = getParentResource().getDatabase().db();
        list.noPaging();
        if (REFLIST_AREAS.equals(list.getIdString())) {
            for (WGArea area : db.getAreas().values()) {
                list.add(new AreaResource(this, area));
            }
        }
        else if (REFLIST_CONTENTTYPES.equals(list.getIdString())) {
            for (WGContentType ct : db.getContentTypes()) {
                list.add(new ContentTypeResource(this, ct));
            }
        }
        else if (REFLIST_LANGUAGES.equals(list.getIdString())) {
            for (WGLanguage lang : db.getLanguages().values()) {
                list.add(new LanguageResource(this, lang));
            }
        }
    }

    @Override
    protected void addReferences(References refs) throws UnavailableResourceException {
        
        refs.add(new ReferenceCollection(this, REFLIST_AREAS, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_CONTENTTYPES, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_LANGUAGES, getURI()));
        
        refs.add(new LookupReference(REFLOOKUP_PAGES, getURI().path(REFLOOKUP_PAGES).build(), "/{pageKey}", PageResource.RESOURCE_TYPE));
        refs.add(new LookupReference(REFLOOKUP_CONTENT, getURI().path(REFLOOKUP_CONTENT).build(), "/{contentKey}", CmsContentResource.RESOURCE_TYPE));
        refs.add(new LookupReference(REFLOOKUP_CONTENTBYNAME, getURI().path(REFLOOKUP_CONTENTBYNAME).build(), "/{name},{language}", CmsContentResource.RESOURCE_TYPE));
        refs.add(new LookupReference(REFLOOKUP_PAGESBYNAME, getURI().path(REFLOOKUP_PAGESBYNAME).build(), "/{name}", PageResource.RESOURCE_TYPE));
        refs.add(new LookupReference(REFLOOKUP_CONTEXT, autoSuffix(getURI().path(REFLOOKUP_CONTEXT)), "?expression={context-expression}", CmsContentResource.RESOURCE_TYPE));

    }
    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    
    @Override
    public Resource<?> wrapIntoResource(Object resourceObject) throws WGAPIException {

        if (resourceObject instanceof TMLContext) {
            resourceObject = ((TMLContext) resourceObject).content();
        }
        
        if (resourceObject instanceof WGContent) {
            WGContent content = (WGContent) resourceObject;
            PageResource page = new PageResource((CmsApiResource) getAncestorResource(CmsApiResource.class), content.getStructEntry());
            CmsContentResource contentResource = new CmsContentResource(page, content);
            return contentResource;
        }
        else if (resourceObject instanceof WGStructEntry) {
            return new PageResource(this, (WGStructEntry) resourceObject);
        }
        else if (resourceObject instanceof WGArea) {
            return new AreaResource(this, (WGArea) resourceObject);
        }
        else if (resourceObject instanceof WGContentType) {
            return new ContentTypeResource(this, (WGContentType) resourceObject);
        }
        else if (resourceObject instanceof WGLanguage) {
            return new LanguageResource(this, (WGLanguage) resourceObject);
        }
        else if (resourceObject == null){
            throw new WebApplicationException("Null resource object", 500);
        }
        else {
            throw new WebApplicationException("Unwrappeable resource object: "  + resourceObject.getClass().getName(), 500);
        }
    }
    
    
    @Path(REFLOOKUP_CONTEXT)
    public CmsContentResource getContext(@QueryParam("expression") String contextExpression) throws WGAPIException, WGException, IllegalArgumentException, UriBuilderException, UnsupportedEncodingException, URISyntaxException {
        
        Database database = getParentResource().getDatabase();
        if (!database.db().isSessionOpen()) {
            throw new WebApplicationException("You have no access to this database", 403);
        }
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        
        WGContent targetContent = null;
        if (contextExpression != null) {
            
            Context cx = getRootResource().getWga().createTMLContext(database.db());
            Context targetCx = cx. context(contextExpression);
            if (targetCx != null) {
                targetContent = targetCx.content();
                CmsContentResource contentResource = getPage(String.valueOf(targetContent.getStructKey())).getContent(targetContent.getLanguage().getName(), targetContent.getVersion());
                return contentResource;
            }
            else {
                env.status = Status.ERROR;
                env.error = new RestError();
                env.error.code = 404;
                env.error.message = "The context expression could not be executed";
                env.error.causes = Collections.singletonList(cx.getlasterror());
                throw new WebApplicationException(buildResponse(Response.status(404).entity(env)));
            }
            
        }
        else {
            env.status = Status.ERROR;
            env.error = new RestError();
            env.error.code = 400;
            env.error.message = "The query parameter 'expression' for specifying the context expression is missing";
            throw new WebApplicationException(buildResponse(Response.status(404).entity(env)));
        }
    }
    
    @Override
    public String getId() throws WGException {
        return null;
    }
    
    @Override
    public boolean isLinkable() {
        return getDatabaseResource().isApiEnabled(CmsApiResource.RESOURCE_TYPE);
    }


}

