package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;

import java.util.List;

import javax.ws.rs.MatrixParam;
import javax.ws.rs.Path;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGStructEntry;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.common.beans.hdbmodel.ModelDefinition;
import de.innovationgate.wga.services.rest.v1.types.LookupReference;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wgpublisher.hdb.HDBModel;

@XmlRootElement
@XmlDiscriminatorValue(HdbmodelDefinitionResource.RESOURCE_TYPE)
public class HdbmodelDefinitionResource extends AbstractModelResource<HdbmodelApiResource> {
    
    public static final String RESOURCE_TYPE = "hdbmodelDefinition";
    
    public static final String REFLOOKUP_LOOKUP = "lookup";
    
    private ModelDefinition _definition;
    
    public HdbmodelDefinitionResource() {
    }
    
    public HdbmodelDefinitionResource(HdbmodelApiResource parent, ModelDefinition definition) {
        super(parent, parent.getURI().path(HdbmodelApiResource.REF_MODEL));
        _definition = definition;
    }

    @Override
    protected List<Document> getChildDocuments() {
        return _definition.getChildDocuments();
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException, WGException {
        super.addReferences(refs);
        refs.add(new LookupReference(REFLOOKUP_LOOKUP, autoSuffix(getURI().path(REFLOOKUP_LOOKUP)), ";pageKey={pageKey}", HdbmodelModelDocumentResource.RESOURCE_TYPE));
    }
    
    @Path(REFLOOKUP_LOOKUP)
    public HdbmodelModelDocumentResource getDocumentModel(@MatrixParam("pageKey") String pageKey) throws WGAPIException {
        
        WGDatabase db = getApiResource().getParentResource().getDatabase().db();
        WGStructEntry entry = db.getStructEntryByKey(db.parseStructKey(pageKey));
        HDBModel model = getParentResource().getModel();
        WGContent content = model.getDocumentByStructKey(db.parseStructKey(pageKey));
        if (content == null) {
            throw new WebApplicationException("Unknown page of key '" + pageKey + "'", 404);
        }
        
        List<Document> models = model.getModelPathForContent(content);
        
        AbstractModelResource<?> modelResource = this;
        for (Document doc : models) {
            modelResource = new HdbmodelModelDocumentResource(modelResource, doc);
        }
        
        return (HdbmodelModelDocumentResource) modelResource;

    }
    
    @Override
    public String getId() throws WGException {
        return RESOURCE_TYPE;
    }

}
