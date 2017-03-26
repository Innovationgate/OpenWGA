package de.innovationgate.wga.services.rest.v1.resources;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.GET;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriBuilderException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

public abstract class EnvelopeReturningResource<ParentType extends Resource<?>> extends Resource<ParentType> {

    public EnvelopeReturningResource(ParentType parentResource, UriBuilder uri) {
        super(parentResource, uri);
    }

    public EnvelopeReturningResource() {
        super();
    }

    @GET
    public Response get(@QueryParam("ref") List<String> expandedRefsStr, @QueryParam("enhancer") List<String> enhancers) throws Exception {
        
        HttpServletRequest request = getRootResource().getWga().getRequest();
        Date lastModified = getLastModified();
        if (lastModified != null) {
            lastModified = new Date(WGUtils.cutoffTimeMillis(lastModified.getTime()));
            long modSince = request.getDateHeader("If-Modified-Since");
            if (modSince != -1) {
                if (lastModified.getTime() <= modSince) {
                    return buildResponse(Response.notModified());
                }
            }
        }
        
        Envelope env = new Envelope();
        fillResource(env);
        enhanceResource(this, parseResourceParameters());
        
        
        env.self = autoSuffix(getURI());
        if (getParentResource() != null) {
            env.parentResource = autoSuffix(getParentResource().getURI());
        }
        int listSize = getDefaultRefListSize();
        List<? extends Reference> refs = new ArrayList<Reference>(getReferences().values());
        Collections.sort(refs, new Comparator<Reference>() {
            @Override
            public int compare(Reference o1, Reference o2) {
                return o1.getIdString().toString().compareTo(o2.getIdString().toString());
            }
        });
        
        Map<String,ReferenceCollection.Parameters> expandedRefs = parseExpandedRefs(expandedRefsStr);
        for (Reference ref : refs) {
            
            if (ref instanceof ReferenceCollection && (isContainedRefs() || expandedRefs.containsKey(ref.getId().getName()))) {
                ReferenceCollection refList = (ReferenceCollection) ref;
                if (listSize != -1) {
                    refList.offset = 0;
                    refList.size = listSize;
                }
                refList.setParameters(expandedRefs.get(ref.getId().getName()));
                initAndFillReferenceList(refList);
            }
            
            env.addReference(ref);
        }
        
        Response.ResponseBuilder responseBuilder = Response.ok(env);
        if (lastModified != null) {
            responseBuilder.lastModified(lastModified);
        }
        return buildResponse(responseBuilder);        
        
    }

    protected abstract void addReferences(References refs) throws IllegalArgumentException, UriBuilderException, WGException;

    protected abstract void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException, Exception;
    
    protected void initAndFillReferenceList(ReferenceCollection list) throws Exception {
        list.refs = new ArrayList<Reference>();
        fillReferenceList(list);
    }
    
    protected References getReferences() throws WGAPIException, IllegalArgumentException, UriBuilderException, WGException {
        References refs = new References();
        addReferences(refs);
        return refs;
    }
    

}
