package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;

import java.util.Collections;
import java.util.List;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Document;
import de.innovationgate.wga.common.beans.hdbmodel.DocumentParent;
import de.innovationgate.wga.common.beans.hdbmodel.Relation;
import de.innovationgate.wga.common.beans.hdbmodel.SingletonContent;
import de.innovationgate.wga.common.beans.hdbmodel.Storage;
import de.innovationgate.wga.services.rest.v1.types.LookupReference;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;

@XmlRootElement
@XmlDiscriminatorValue(HdbmodelModelDocumentResource.RESOURCE_TYPE)
public class HdbmodelModelDocumentResource extends AbstractModelResource<AbstractModelResource<?>> {
    
    public static final String PARAM_ID = "id";
    public static final String PARAM_TYPE = "type";
    public static final String TYPE_STORAGE = "storage";
    public static final String TYPE_CONTENT= "content";
    public static final String TYPE_SINGLETON_CONTENT = "singletonContent";
    
    public static final String RESOURCE_TYPE = "hdbmodelDocumentDefinition";
    
    public static final String REFLOOKUP_RELATIONTARGETS = "relationTargets";
    
    public static String getDocumentType(Document doc) {
        if (doc instanceof Content) {
            return TYPE_CONTENT;
        }
        else if (doc instanceof Storage) {
            return TYPE_STORAGE; 
        }
        else if (doc instanceof SingletonContent) {
            return TYPE_SINGLETON_CONTENT; 
        }
        else {
            return null;
        }
    }

    private Document _document;
    
    public HdbmodelModelDocumentResource() {
    }

    public HdbmodelModelDocumentResource(@SuppressWarnings("rawtypes") AbstractModelResource parent, Document document) {
        super(parent, parent.getURI().path(REFLIST_CHILDDOCUMENTS).path(getDocumentType(document)).matrixParam(PARAM_ID, document.getId()));
        _document = document;
    }
    
    @XmlAttribute
    public String getDocumentType() {
        return getDocumentType(_document);
    }
    
    @XmlElement
    public Content getContent() {
        if (_document instanceof Content) {
            return (Content) _document;
        }
        else {
            return null; 
        }
    }
    
    @XmlElement
    public Storage getStorage() {
        if (_document instanceof Storage) {
            return (Storage) _document;
        }
        else {
            return null; 
        }
    }
    
    @XmlElement
    public SingletonContent getSingletonContent() {
        if (_document instanceof SingletonContent) {
            return (SingletonContent) _document;
        }
        else {
            return null; 
        }
    }
    
    

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    @Override
    protected List<Document> getChildDocuments() {
        if (_document instanceof DocumentParent) {
            return ((DocumentParent) _document).getChildDocuments();
        }
        else {
            return Collections.emptyList();
        }
    }
    
    public String getCollectionId() {
        return getDocumentType(_document) + "-" + _document.getId();
    }
    
    @Override
    protected void addReferences(References refs) throws WGAPIException, IllegalArgumentException, UriBuilderException, WGException {
        super.addReferences(refs);
        if (_document instanceof Content) {
            for (Relation rel : ((Content) _document).getRelations()) {
                Reference.Id id =Reference.Id.create(REFLOOKUP_RELATIONTARGETS);
                id.param("relation", rel.getName());
                refs.add(new LookupReference(id, autoSuffix(getURI().path(id.toString())), ";parentContent={pageKey}", HdbmodelContentResource.RESOURCE_TYPE, "collection"));
            }
        }
    }
    
    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {
        
        if (REFLOOKUP_RELATIONTARGETS.equals(list.getId().getName())) {
            Content contentModel = getContent();
            if (contentModel == null) {
                throw new WebApplicationException("This lookup functionality is only available on content models", 400);
            }
            
            String relation = list.getId().getParams().get("relation");
            String parentPageKey = list.getId().getParams().get("parentContent");
            if (relation == null || parentPageKey == null) {
                throw new WebApplicationException("URL matrix parameter 'relation' or 'parentContent' missing", 400);
            }
            
            HdbmodelApiResource api = (HdbmodelApiResource) getApiResource();
            HdbmodelContentResource parentContentResource = api.getContentByPageKey(parentPageKey);
            SkippingIterator<WGContent> results = api.getModel().getRelationTargets(parentContentResource.getDoc(), contentModel.getContentClass(), relation).getResultIterator();
            list.addUntilFull(results);
        }
        else {
            super.fillReferenceList(list);
        }
    }
    
    @Override
    public String getId() throws WGException {
        return _document.getId();
    }

}
