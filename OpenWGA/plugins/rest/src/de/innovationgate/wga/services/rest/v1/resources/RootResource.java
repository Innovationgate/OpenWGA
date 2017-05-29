package de.innovationgate.wga.services.rest.v1.resources;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ResourceContext;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilderException;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;
import org.glassfish.jersey.server.ExtendedResourceContext;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

@Path("/v1")
@Api(value="root")
@XmlRootElement(name=RootResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(RootResource.RESOURCE_TYPE)
public class RootResource extends ExternalRefsResource<RootResource> {
    
    public static final String RESOURCE_TYPE="root";

    public static final String REFLIST_DBS = "dbs";

    public WGA getWga() {
        return _wga;
    }

    public HttpHeaders getHeaders() {
        return _headers;
    }

    private WGA _wga;
    private HttpHeaders _headers;
    private MediaType _outputMediaType = null;
    private UriInfo _uriInfo;
    private Boolean _isAdminLoggedIn=false;

    private RestApplication _application;

    private ExtendedResourceContext _resourceContext;
    
    public RootResource() {
        super();
    }
    
    public RootResource(@Context UriInfo uriInfo, @Context Application application, @Context HttpServletRequest request, @Context HttpServletResponse response, @Context HttpHeaders headers, @Context ResourceContext resourceContext) {
        super(null, uriInfo.getBaseUriBuilder().path("v1"));
        _wga = WGA.get(request, response, ((RestApplication) application).getCore());
        _isAdminLoggedIn = _wga.getCore().isAdminLoggedIn(request);
        _application = (RestApplication) application;
        _headers = headers;
        _uriInfo = uriInfo;
        _resourceContext = (ExtendedResourceContext) resourceContext;
        if (!_application.isServiceEnabled()) {
            throw new WebApplicationException("REST Web Service is disabled", 403);
        }
    }

    
    @Path(REFLIST_DBS + "/{dbkey}")
    @ApiOperation(value="Retrieve a database resource")
    public DatabaseResource getDatabase(@ApiParam(name="dbkey",value="Database key") @PathParam("dbkey") String dbKey) throws WGException {
        return new DatabaseResource(this, dbKey);
    }


    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {
        list.noPaging();
        
        if(!_isAdminLoggedIn)
        	throw new WebApplicationException("admin login required for db listing", 403);
        
        for (RestApplication.DatabaseInfo dbInfo : _application.getDatabaseInfo().values()) {
            try {
                DatabaseResource dbResource = getDatabase(dbInfo.getDbKey());
                list.add(dbInfo.getDbKey(), dbResource);
            }
            catch (WebApplicationException e) {
                RestApplication.LOG.error("Exception retrieving database resource for dbkey '" + dbInfo.getDbKey() + "'", e);
            }
        }
        
    }

    @Override
    protected void addReferences(References refs) throws WGException {
        refs.add(new ReferenceCollection(this, REFLIST_DBS, getURI()));
    }
    
    public MediaType getOutputMediaType() {
        if (_outputMediaType == null) {
            _outputMediaType = _application.getOutputMediaType(_headers);
        }
        return _outputMediaType;
    }
    
    @XmlTransient
    public UriInfo getUriInfo() {
        return _uriInfo;
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @XmlAttribute
    public String getServerVersion() throws WGException {
        return _wga.server().getVersion().toString();
    }

    public RestApplication getApplication() {
        return _application;
    }
    
    @Override
    public String getId() {
        return null;
    }

    public ExtendedResourceContext getResourceContext() {
        return _resourceContext;
    }

    public Boolean isAdminLoggedIn(){
    	return _isAdminLoggedIn;
    }
    
}
