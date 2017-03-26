package de.innovationgate.wga.services.rest.v1.resources.cms;

import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.PUT;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGLanguage;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

@XmlRootElement(name=LanguageResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(LanguageResource.RESOURCE_TYPE)
public class LanguageResource extends DocumentResource<WGLanguage,CmsApiResource> {
    
    public static final String RESOURCE_TYPE = "language";

    public LanguageResource() {
        super();
    }

    public LanguageResource(CmsApiResource parent, String name) throws WGAPIException {
        super(parent, name, parent.getURI().path(CmsApiResource.REFLIST_LANGUAGES).path(name));
    }
    
    public LanguageResource(CmsApiResource parent, WGLanguage doc) throws WGAPIException {
        super(parent, doc, parent.getURI().path(CmsApiResource.REFLIST_LANGUAGES).path(doc.getName()));
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {
        throw new IllegalArgumentException("Unknown reference list: " + list.getIdString());
    }

    @Override
    protected void addReferences(References refs) throws UnavailableResourceException {
    }
    
    @Override
    protected Set<String> getDefaultMetaSet() {

        Set<String> metas = new HashSet<String>();
        metas.add(WGLanguage.META_EDITORS);
        metas.add(WGLanguage.META_TITLE);
        metas.add(WGLanguage.META_DESCRIPTION);
        return metas;
    }

    
    @PUT
    @Consumes({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
    public Response put(LanguageResource res) throws WGException {
        return defaultPut(res);
    }
    
    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    @Override
    protected WGLanguage putCreateDocument(Resource<?> res) throws WGAPIException {
        return getDatabaseResource().getDatabase().db().createLanguage(getNewDocName());
    }
    
    @Override
    public String getId() throws WGException {
        return getDoc().getName();
    }

}



