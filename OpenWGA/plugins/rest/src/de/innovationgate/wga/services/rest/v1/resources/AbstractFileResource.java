package de.innovationgate.wga.services.rest.v1.resources;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.List;

import javax.activation.DataSource;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.PUT;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriBuilderException;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.server.api.tml.Context;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

public abstract class AbstractFileResource<ParentType extends AbstractContentResource<?>> extends EnvelopeReturningResource<ParentType> {

    protected String _fileName;

    public AbstractFileResource(ParentType parentResource, UriBuilder uri) {
        super(parentResource, uri);
    }

    public AbstractFileResource() {
        super();
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {
    }

    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException {
    }

    @Override
    public Response get(List<String> refs, List<String> enhancers) throws WGException, IllegalArgumentException, UriBuilderException, UnsupportedEncodingException, URISyntaxException {
        Context cx = getRootResource().getWga().createTMLContext(getParentResource().getDoc());
        URI uri = new URI(cx.fileurl(_fileName));
        return buildResponse(Response.temporaryRedirect(uri));
    }

    @Override
    public Date getLastModified() throws WGException {
        WGContent content = getParentResource().getDoc();
        if (content.hasFile(_fileName)) { 
            return content.getFileLastModified(_fileName);
        }
        else {
            return null;
        }
    }

    @PUT
    @Consumes({ MediaType.APPLICATION_OCTET_STREAM, MediaType.TEXT_PLAIN })
    public Response putFile(DataSource input) throws WGAPIException, IOException {
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        WGContent doc = getParentResource().getDoc();
        
        try {
            doc.performSaveCheck();
        }
        catch (WGAuthorisationException e) {
            return buildAuthorisationExceptionResponse(e, env);
        }
        catch (ResourceIsLockedException e) {
            return buildLockedExceptionResponse(env, e);
        }
        
        boolean fileExisted = false;
        if (doc.hasFile(_fileName)) {
            fileExisted = true;
            doc.removeFile(_fileName);
        }
        doc.attachFile(input.getInputStream(), _fileName);
        doc.save();
        
        if (getRootResource().getUriInfo().getQueryParameters().containsKey(RestApplication.URLPARAM_RETURNRESOURCE)) {
            fillResource(env);
        }
        
        return buildResponse(Response.ok(env));
        
    }

    @DELETE
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XHTML_XML })
    public Response delete() throws WGException {
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        
        if (!getParentResource().getDoc().hasFile(_fileName)) {
            return buildResponse(Response.ok(env));
        }
        
        try {
            getParentResource().getDoc().performSaveCheck();
        }
        catch (WGAuthorisationException e) {
            return buildAuthorisationExceptionResponse(e, env);
        }
        catch (ResourceIsLockedException e) {
            return buildLockedExceptionResponse(env, e);
        }
        
        getParentResource().getDoc().removeFile(_fileName);
        getParentResource().getDoc().save();
        
        return buildResponse(Response.ok(env));
        
    }

}