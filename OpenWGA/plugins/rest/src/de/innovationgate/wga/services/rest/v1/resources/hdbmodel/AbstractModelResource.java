package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;

import java.util.List;

import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.PathSegment;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriBuilderException;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.services.rest.v1.resources.ExternalRefsResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

public abstract class AbstractModelResource<ParentType extends Resource<?>> extends ExternalRefsResource<ParentType> {
    
    public static final String REFLIST_CHILDDOCUMENTS = "childDocuments";
    public static final String REF_DOCUMENT = "document";
    

    public AbstractModelResource(ParentType parent, UriBuilder uri) {
        super(parent, uri);
    }

    public AbstractModelResource() {
        super();
    }

    protected abstract List<Document> getChildDocuments();

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {
    
        if (HdbmodelModelDocumentResource.REFLIST_CHILDDOCUMENTS.equals(list.getIdString())) {
            list.noPaging();
            for (Document doc : getChildDocuments()) {
                HdbmodelModelDocumentResource docResource = new HdbmodelModelDocumentResource(this, doc);
                list.add(docResource.getCollectionId(), docResource);
            }
        }
        
        else {
            throw new WebApplicationException("Unknown reference collection '" + list.getIdString() + "'", 404);
        }
    
    }

    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException, WGException {
        refs.add(new ReferenceCollection(this, HdbmodelModelDocumentResource.REFLIST_CHILDDOCUMENTS, getURI()));
    }
    
    @Path(REFLIST_CHILDDOCUMENTS + "/{document}")
    public HdbmodelModelDocumentResource getChildDocument(@PathParam("document") PathSegment path) {
        
        String type = path.getPath();
        String id = path.getMatrixParameters().getFirst(HdbmodelModelDocumentResource.PARAM_ID);
        
        for (Document doc : getChildDocuments()) {
            if (WGUtils.nullSafeEquals(HdbmodelModelDocumentResource.getDocumentType(doc), type) && doc.getId().equals(id)) {
                return new HdbmodelModelDocumentResource(this, doc);
            }
        }
        
        throw new WebApplicationException("Unknown child model document '" + id + "' of type " + type, 404);
        
    }
    

}