package de.innovationgate.wga.services.rest.v1.resources;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlElement;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.services.rest.v1.types.DocumentKey;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;

public abstract class AbstractRelationGroupResource<ParentType extends AbstractContentResource<?>> extends EnvelopeReturningResource<ParentType> {

    protected String _groupName;
    protected WGContent _doc;
    protected List<ResourceReference> _incomingRelations;
    protected boolean _new = false;

    public AbstractRelationGroupResource(ParentType parentResource, UriBuilder uri) {
        super(parentResource, uri);
    }

    public AbstractRelationGroupResource() {
        super();
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws WGException, IllegalArgumentException, UriBuilderException {
    }

    @Override
    protected void addReferences(References refs) {
    }

    @XmlElement(name = "relations")
    public List<ResourceReference> getRelations() throws WGAPIException, WGException {
        
        if (_new) {
            return _incomingRelations;
        }
        
        List<ResourceReference> list = new ArrayList<ResourceReference>();
        List<WGRelationData> relDataList = _doc.getRelationsDataOfGroup(_groupName);
        Collections.sort(relDataList, new Comparator<WGRelationData>() {
            @Override
            public int compare(WGRelationData arg0, WGRelationData arg1) {
                return  arg0.getTargetStructkey().toString().compareTo(arg1.getTargetStructkey().toString());
            }
        });
        
        for (WGRelationData relData : relDataList) {
            list.add(getParentResource().createReferenceFromRelation(relData, false));
        }
        return list;
    }

    public void setRelations(List<ResourceReference> relations) {
        _incomingRelations = relations;
    }

    @Override
    public void fillResource(Envelope env) throws WGAPIException {
        super.fillResource(env);
        WGContent doc = getParentResource().getDoc();
        env.key = new DocumentKey();
        env.key.type = getResourceType();
        env.key.pageKey = String.valueOf(doc.getStructKey());
        env.key.language = doc.getLanguage().getName();
        env.key.version = doc.getVersion();
        env.key.name = _groupName;
    }

    @DELETE
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XHTML_XML })
    public Response delete() throws WGException {
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        
        if (_new) {
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
        
        getParentResource().getDoc().clearRelationGroup(_groupName);
        
        return buildResponse(Response.ok(env));
        
    }
    
    
    @Override
    public String getId() throws WGException {
        return _groupName;
    }

}