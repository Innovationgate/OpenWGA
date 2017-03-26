package de.innovationgate.wga.services.rest.v1.resources.cms;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGUserProfile;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.v1.resources.DatabaseResource;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

@XmlRootElement(name=UserProfileResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(UserProfileResource.RESOURCE_TYPE)
public class UserProfileResource extends DocumentResource<WGUserProfile,DatabaseResource> {
    
    public static final String RESOURCE_TYPE = "userProfile";

    public UserProfileResource() {
    }
    
    public UserProfileResource(DatabaseResource parent, WGUserProfile profile) {
        super(parent, profile, parent.getURI().path(DatabaseResource.REF_LOGINUSERPROFILE));
    }

    @Override
    protected Set<String> getDefaultMetaSet() {

        Set<String> metas = new HashSet<String>();
        metas.add(WGUserProfile.META_TYPE);
        metas.add(WGUserProfile.META_LASTIP);
        metas.add(WGUserProfile.META_LASTACCESS);
        metas.add(WGUserProfile.META_LASTSESSIONID);
        metas.add(WGUserProfile.META_LASTSESSIONDATE);
        metas.add(WGUserProfile.META_LASTSESSIONHITS);
        metas.add(WGUserProfile.META_PREVIOUSSESSIONID);
        metas.add(WGUserProfile.META_PREVIOUSSESSIONDATE);
        metas.add(WGUserProfile.META_PREVIOUSSESSIONHITS);
        metas.add(WGUserProfile.META_HITS);
        metas.add(WGUserProfile.META_SESSIONS);
        metas.add(WGUserProfile.META_CLIENT);
        metas.add(WGUserProfile.META_DBLOGIN);
        return metas;
        
    }

    @Override
    protected WGUserProfile putCreateDocument(Resource<?> res) throws WGAPIException {
        throw new WebApplicationException("Creating user profiles not supported", 403);
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {
    }

    @Override
    protected void addReferences(References refs) throws UnavailableResourceException {
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    @Override
    public String getId() throws WGException {
        return getDoc().getName();
    }
}
