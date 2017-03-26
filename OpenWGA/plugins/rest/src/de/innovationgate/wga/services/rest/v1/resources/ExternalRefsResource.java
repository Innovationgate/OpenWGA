package de.innovationgate.wga.services.rest.v1.resources;

import java.util.Enumeration;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.PathSegment;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.types.LookupReference;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.Status;

public abstract class ExternalRefsResource<ParentType extends Resource<?>> extends EnvelopeReturningResource<ParentType> {
    
    public ExternalRefsResource(ParentType parent, UriBuilder uri) {
        super(parent, uri);
    }
    
    public ExternalRefsResource() {
        super();
    }

    @GET
    @Path("{refname}")
    public Response getSubCollection(@PathParam("refname") String refName, @QueryParam(RestApplication.URLPARAM_OFFSET) @DefaultValue(value="0") int offset, @QueryParam(RestApplication.URLPARAM_SIZE) @DefaultValue(value="10") int size) throws Exception {
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI().path(refName));
        env.parentResource = autoSuffix(getURI());
        
        References refs = getReferences();
        
        Reference.Id refId = new Reference.Id(refName);
        UriInfo uriInfo = getRootResource().getUriInfo();
        List<PathSegment> segments = uriInfo.getPathSegments();
        PathSegment lastSegment = segments.get(segments.size() - 1);
        for (Map.Entry<String,List<String>> param : lastSegment.getMatrixParameters().entrySet()) {
            for (String paramValue : param.getValue()) {
                refId.param(param.getKey(), paramValue);
            }
        }
        
        Reference ref = null;
        String idString = refId.toString();
        if (refs.containsKey(idString)) {
            ref = refs.get(idString);
        }
        
        if (ref instanceof LookupReference && ((LookupReference) ref).lookupType.equals("collection")) {
            ref = new ReferenceCollection(this, refId, getURI());
        }
        
        if (ref instanceof ReferenceCollection) {
            ReferenceCollection refList = (ReferenceCollection) ref;
            refList.offset = offset;
            refList.size = size;
            refList.setParameters(parseCollectionParameters(refList.getId().getName()));
            initAndFillReferenceList(refList);
            env.addReference(ref);
            return buildResponse(Response.ok(env));
        }
        
        env.status = Status.ERROR;
        env.error = new RestError();
        env.error.code = 404;
        env.error.message = "No subcollection of name '" + refId.getName() + "' available";
        return buildResponse(Response.status(Response.Status.NOT_FOUND).entity(env));
    }
    
    private ReferenceCollection.Parameters parseCollectionParameters(String colName) throws WGException {

        ReferenceCollection.Parameters params = new ReferenceCollection.Parameters();
        params.colName = colName;
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

    @Override
    protected boolean isContainedRefs() {
        return false;
    }
    
    @Override
    protected int getDefaultRefListSize() {
        return RestApplication.COLLECTION_PAGESIZE_DEFAULT;
    }

    protected int getIteratorPageSize(ReferenceCollection list) {
        return list.isCompleteList() ? RestApplication.COLLECTION_PAGESIZE_DEFAULT : list.size + 1;
    }
}

