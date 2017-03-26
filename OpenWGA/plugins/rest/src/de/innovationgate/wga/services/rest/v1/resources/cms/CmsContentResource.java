package de.innovationgate.wga.services.rest.v1.resources.cms;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGHierarchyContentIterator;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.AbstractContentResource;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.resources.RestError;
import de.innovationgate.wga.services.rest.v1.types.ElementFilter;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;
import de.innovationgate.wga.services.rest.v1.types.Status;

@XmlRootElement(name=CmsContentResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(CmsContentResource.RESOURCE_TYPE)
public class CmsContentResource extends AbstractContentResource<PageResource> {
    
    public static final String REF_PAGE = "page";
    public static final String REFLIST_RELATIONGROUPS = "relationGroups";
    public static final String REFLIST_CHILDCONTENT = "childContent";
    public static final String REF_PARENTCONTENT = "parentContent";
    public static final String REF_NEXT = "next";
    public static final String REF_PREVIOUS = "previous";
    public static final String REF_CONTEXT = "context";
    public static final String REFLIST_NAVIGATION = "navigation";
    public static final String REFLIST_FILES = "files";
    public static final String RESOURCE_TYPE = "content";
    public CmsContentResource(PageResource parent, WGContent content) throws WGAPIException {
        super(parent, content, parent.getURI().path(content.getLanguage().getName()).path(String.valueOf(content.getVersion())));
    }
    
    public CmsContentResource() {
        super();
    }
    
    @Path(REFLIST_FILES + "/{fileName}")
    public CmsFileResource getFile(@PathParam("fileName") String fileName) throws WGAPIException {
        return new CmsFileResource(this, fileName);
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {

         if (REFLIST_NAVIGATION.equals(list.getIdString())) {
            list.noPaging();
            Map<String,URI> mapRefs = new HashMap<String, URI>();
            
            WGContent prev = getDoc().getPreviousContent();
            if (prev != null) {
                PageResource prevPage = new PageResource((CmsApiResource) getAncestorResource(CmsApiResource.class), prev.getStructEntry());
                list.add(REF_PREVIOUS, new CmsContentResource(prevPage, prev));
            }
            
            WGContent next = getDoc().getNextContent();
            if (next != null) {
                PageResource nextPage = new PageResource((CmsApiResource) getAncestorResource(CmsApiResource.class), next.getStructEntry());
                list.add(REF_NEXT, new CmsContentResource(nextPage, next));
            }    
            WGContent parent = getDoc().getParentContent();
            if (parent != null) {
                PageResource parentPage = new PageResource((CmsApiResource) getAncestorResource(CmsApiResource.class), parent.getStructEntry());
                list.add(REF_PARENTCONTENT, new CmsContentResource(parentPage, parent));
            }
        }
        else if (REFLIST_CHILDCONTENT.equals(list.getIdString())) {
            final String role = getRootResource().getWga().getRequest().getParameter(RestApplication.URLPARAM_ROLE);
            WGHierarchyContentIterator contents = getDoc().getChildContentIterator(getIteratorPageSize(list));
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
            list.addUntilFull(contents);
        }
          
        else if (REFLIST_RELATIONGROUPS.equals(list.getIdString())) {
            list.noPaging();
            for (String relName : getDoc().getRelationGroups()) {
                list.add(new CmsRelationGroupResource(this, getDoc(), relName));
            }
        }
         
        else if (REFLIST_FILES.equals(list.getIdString())) {
            list.noPaging();
            list.setAutoSuffixRefURIs(false);
            for (String fileName : getDoc().getFileNames()) {
                list.add(new CmsFileResource(this, fileName));
            }
        }
          
        else {
            throw new IllegalArgumentException("Unknown reference list: " + list.getIdString());
        }

    }

    @Override
    protected void addReferences(References refs) throws UnavailableResourceException {

        refs.add(new ReferenceCollection(this, REFLIST_NAVIGATION, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_CHILDCONTENT, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_RELATIONGROUPS, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_FILES, getURI()));
        refs.add(new ResourceReference(REF_PAGE, autoSuffix(getParentResource().getURI()), PageResource.RESOURCE_TYPE));
        refs.add(new ResourceReference(REF_CONTEXT, autoSuffix(getURI().path(REF_CONTEXT)), CmsContentResource.RESOURCE_TYPE));
        
    }
    
    @Path(REFLIST_RELATIONGROUPS + "/{relname}")
    public CmsRelationGroupResource getRelationGroup(@PathParam("relname") String relName) throws WGAPIException {
        return new CmsRelationGroupResource(this, getDoc(), relName);        
    }

    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @GET
    @Path(REF_CONTEXT)
    public Response getContext(@QueryParam("expression") String contextExpression) throws Exception {
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        
        WGContent targetContent = null;
        if (contextExpression != null) {
            
            Context cx = getRootResource().getWga().createTMLContext(getDoc());
            Context targetCx = cx. context(contextExpression);
            if (targetCx != null) {
                targetContent = targetCx.content();
                CmsContentResource contentResource = ((CmsApiResource) getAncestorResource(CmsApiResource.class)).getPage(String.valueOf(targetContent.getStructKey())).getContent(targetContent.getLanguage().getName(), targetContent.getVersion());
                return contentResource.get(Collections.<String> emptyList(), Collections.<String> emptyList());
            }
            else {
                env.status = Status.ERROR;
                env.error = new RestError();
                env.error.code = 404;
                env.error.message = "The context expression could not be executed";
                env.error.causes = Collections.singletonList(cx.getlasterror());
                return buildResponse(Response.status(404).entity(env));
            }
            
        }
        else {
            env.status = Status.ERROR;
            env.error = new RestError();
            env.error.code = 400;
            env.error.message = "The query parameter 'expression' for specifying the context expression is missing";
            return buildResponse(Response.status(404).entity(env));
        }
        
    }
    
    @Override
    protected Set<String> getDefaultRelationSet() throws WGAPIException {
        return new HashSet<String>(getDoc().getRelationNames());
    }
    
    @PUT
    @Consumes({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
    public Response put(CmsContentResource res) throws WGException {
        return defaultPut(res);
    }
    
    @Override
    protected WGContent resolveContentReference(ResourceReference ref) throws WGAPIException {
        WGContentKey contentKey = parseTargetContentKey(ref);
        WGContent content = getDoc().getDatabase().getContentByKey(contentKey);
        return content;
    }

}
