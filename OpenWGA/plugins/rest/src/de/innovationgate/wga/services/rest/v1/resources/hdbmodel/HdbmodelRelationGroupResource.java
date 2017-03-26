package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;


import javax.ws.rs.Consumes;
import javax.ws.rs.PUT;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGContentKey;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.AbstractRelationGroupResource;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;

@XmlRootElement(name=HdbmodelRelationGroupResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(HdbmodelRelationGroupResource.RESOURCE_TYPE)
public class HdbmodelRelationGroupResource extends AbstractRelationGroupResource<HdbmodelContentResource> {
    
    public static final String RESOURCE_TYPE = "hdbmodelRelationGroup";

    public HdbmodelRelationGroupResource() {
        super();
        _new = true;
    }
    
    public HdbmodelRelationGroupResource(HdbmodelContentResource parent, WGContent doc, String groupName) {
        super(parent, parent.getURI().path(HdbmodelContentResource.REFLIST_RELATIONGROUPS).path(groupName));
        _doc = doc;
        _groupName = groupName;
    }


    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @PUT
    @Consumes({ MediaType.APPLICATION_XML, MediaType.APPLICATION_JSON })
    public Response put(HdbmodelRelationGroupResource res) throws WGAPIException {
        
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
        
        doc.clearRelationGroup(_groupName);
        for (ResourceReference ref : res._incomingRelations) {
            
            if (ref.delete != null && ref.delete == true) {
                continue;
            }
            
            WGContentKey contentKey = parseTargetContentKey(ref);
            WGContent content = doc.getDatabase().getContentByKey(contentKey);
            if (content != null) {
                if (ref.protect != null && ref.protect == true) {
                    doc.addRelationToGroup(_groupName, content, WGContent.RELATIONTYPE_PROTECTED);
                }
                else {
                    doc.addRelationToGroup(_groupName, content);
                }
            }
        }
        
        doc.save();
        
        if (getRootResource().getUriInfo().getQueryParameters().containsKey(RestApplication.URLPARAM_RETURNRESOURCE)) {
            fillResource(env);
        }
        
        return buildResponse(Response.ok(env));
                
    }

}
