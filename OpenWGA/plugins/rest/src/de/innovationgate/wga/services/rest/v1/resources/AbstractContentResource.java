package de.innovationgate.wga.services.rest.v1.resources;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import javax.xml.bind.annotation.XmlElement;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGRelationData;
import de.innovationgate.wga.services.rest.v1.types.JSONRelationList;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.RelationList;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;
import de.innovationgate.wga.services.rest.v1.types.XMLRelationList;

public abstract class AbstractContentResource<ParentResource extends Resource<?>> extends DocumentResource<WGContent,ParentResource> {

    public static class ContentWriteContext extends WriteContext {
        
        public boolean publish = false;
        public boolean archive = false;
        public boolean approve = false;
        public boolean reject = false;
        public boolean saveStruct = false;
        
    }

    private List<ResourceReference> _incomingRelations = new ArrayList<ResourceReference>();

    public AbstractContentResource(ParentResource parent, WGContent doc, UriBuilder uri) {
        super(parent, doc, uri);
    }

    public AbstractContentResource(ParentResource parent, String newDocName, UriBuilder uri) {
        super(parent, newDocName, uri);
    }

    public AbstractContentResource() {
        super();
    }

    @Override
    public void fillResource(Envelope env) throws WGAPIException {
        super.fillResource(env);
        env.timestamps.published = getDoc().getPublished();
    }

    @Override
    protected void writeMetaData(WriteContext writeContext, de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field f) throws WGAPIException {
        
        ContentWriteContext contentContext = (ContentWriteContext) writeContext;
        
        if (WGContent.META_STATUS.equalsIgnoreCase(f.name)) {
            String newStatus = (String) f.values.get(0);
            if (!getDoc().getStatus().equals(newStatus)) {
                determineStatusAction(contentContext, newStatus);
            }
        }
        // Unwriteable and therefor ignored metas
        else if (WGContent.META_AUTHOR.equalsIgnoreCase(f.name)) {
            
        }
        
        else {
            super.writeMetaData(writeContext, f);
        }
    }

    @Override
    public void saveData(de.innovationgate.wga.services.rest.v1.resources.DocumentResource.WriteContext writeContext) throws WGAPIException {
        ContentWriteContext contentContext = (ContentWriteContext) writeContext;
        
        if (contentContext.saveStruct) {
            getDoc().getStructEntry().save();
        }
        
        if (contentContext.publish) {
            getDoc().publish();
        }
        else if (contentContext.reject) {
            getDoc().reject("");
        }
        else if (contentContext.archive) {
            getDoc().archive("");
        }
        else if (contentContext.approve) {
            getDoc().approve("");
        }
        else {
            super.saveData(writeContext);
        }
    }

    public void determineStatusAction(ContentWriteContext contentContext, String newStatus) throws WGAPIException {
        if (WGContent.STATUS_RELEASE.equals(newStatus) || WGContent.STATUS_REVIEW.equals(newStatus)) {
            if (getDoc().getStatus().equals(WGContent.STATUS_DRAFT)) {
                contentContext.publish = true;
            }
            else if (getDoc().getStatus().equals(WGContent.STATUS_REVIEW) && WGContent.STATUS_RELEASE.equals(newStatus)) {
                if (getDoc().getWorkflow().isApprovableByUser()) {
                    contentContext.approve = true;
                }
                else {
                    throw new WebApplicationException("You are no approver of content '" + getDoc().getContentKey() + "'", 403);
                }
            }
            else {
                throw new WebApplicationException("Content '" + getDoc().getContentKey() + "' is in wrong status '" + getDoc().getStatus() + "' to be published", 409);
            }
        }
        else if (WGContent.STATUS_ARCHIVE.equals(newStatus)) {
            if (getDoc().getStatus().equals(WGContent.STATUS_RELEASE)) {
                contentContext.archive = true;
            }
            else {
                throw new WebApplicationException("Content '" + getDoc().getContentKey() + "' is in wrong status '" + getDoc().getStatus() + "' to be archived", 409);
            }
        }
        else if (WGContent.STATUS_DRAFT.equals(newStatus) && getDoc().getStatus().equals(WGContent.STATUS_REVIEW)) {
            if (getDoc().getWorkflow().isApprovableByUser()) {
                contentContext.reject = true;
            }
            else {
                throw new WebApplicationException("You are no approver of content '" + getDoc().getContentKey() + "'", 403);
            }
        }
    }

    @XmlElement
    public RelationList getRelations() throws WGException {
        
        String relSet = getRootResource().getWga().getRequest().getParameter("relationSet");
        String[] selectedRelations = getRootResource().getWga().getRequest().getParameterValues("relation");
        if (relSet == null) {
            if (selectedRelations == null || selectedRelations.length == 0) {
                relSet = "default";
            }
            else {
                relSet = "empty";
            }
        }
        Set<String> relNamesSet = new HashSet<String>();
        if (relSet.equals("default")) {
            relNamesSet.addAll(getDefaultRelationSet());
        }
        else if (relSet.equals("all")) {
            relNamesSet.addAll(getDoc().getRelationNames());
        }
        if (selectedRelations != null) {
            relNamesSet.addAll(Arrays.asList(selectedRelations));
        }
        
        List<String> relNames = new ArrayList<String>(relNamesSet);
        Collections.sort(relNames);
        
        List<ResourceReference> list = new ArrayList<ResourceReference>();
        for (String relName : relNames) {
            WGRelationData relData = getDoc().getRelationData(relName);
            if (relData == null || relData.getGroup() != null) {
                continue;
            }
    
            list.add(createReferenceFromRelation(relData, true));
        }
        return createRelationList(list).merge(_incomingRelations);
        
    }

    protected abstract Set<String> getDefaultRelationSet() throws WGAPIException;

    public void setRelations(RelationList list) throws WGAPIException {
        _incomingRelations = list.getRelations();
    }

    private RelationList createRelationList(List<ResourceReference> items) {
    
        RelationList fieldList;
        MediaType outputType = getRootResource().getOutputMediaType();
        if (outputType.equals(MediaType.APPLICATION_JSON_TYPE)) {
            fieldList = new JSONRelationList();
            
        }
        else {
            fieldList = new XMLRelationList();
        }
        
        for (ResourceReference ref : items) {
            fieldList.addRelation(ref);
        }
        return fieldList;
        
    }

    @Override
    protected void writeAdditionalData(WriteContext writeContext, Resource<?> res) throws WGException {
    
        AbstractContentResource<?> contentRes = (AbstractContentResource<?>) res;
        if (contentRes._incomingRelations != null) {
            for (ResourceReference ref : contentRes._incomingRelations) {
                
                if (ref.delete != null && ref.delete == true) {
                    getDoc().removeRelation(ref.getIdString());
                    continue;
                }
                
                WGContent content = resolveContentReference(ref);
                if (content != null) {
                    if (ref.protect != null && ref.protect == true) {
                        getDoc().setRelation(ref.getIdString(), content, WGContent.RELATIONTYPE_PROTECTED);
                    }
                    else {
                        getDoc().setRelation(ref.getIdString(), content);
                    }
                }
            }
        }
        
    }
    
    protected abstract WGContent resolveContentReference(ResourceReference ref) throws WGException;

    @Override
    protected WGContent putCreateDocument(Resource<?> res) {
        throw new WebApplicationException("Creating a content document via PUT is not supported. POST to language collection on pages instead.");
    }

    @Override
    protected de.innovationgate.wga.services.rest.v1.resources.DocumentResource.WriteContext createWriteContext() {
        return new ContentWriteContext();
    }
    

    @Override
    protected Set<String> getDefaultMetaSet() {
        Set<String> metas = new HashSet<String>();
        metas.add(WGContent.META_AUTHOR);
        metas.add(WGContent.META_BROWSERTITLE);
        metas.add(WGContent.META_COAUTHORS);
        metas.add(WGContent.META_CONTENTCLASS);
        metas.add(WGContent.META_DESCRIPTION);
        metas.add(WGContent.META_IS_HIDDEN_FROM);
        metas.add(WGContent.META_KEYWORDS);
        metas.add(WGContent.META_LINK_TARGET);
        metas.add(WGContent.META_OWNER);
        metas.add(WGContent.META_STATUS);
        metas.add(WGContent.META_TITLE);
        metas.add(WGContent.META_VALID_FROM);
        metas.add(WGContent.META_VALID_TO);
        metas.add(WGContent.META_VIRTUAL_LINK);
        metas.add(WGContent.META_VIRTUAL_LINK_TYPE);
        return metas;
    }
    
    
    protected ResourceReference createReferenceFromRelation(WGRelationData relData, boolean setIds) throws WGException {
        
        WGContent relTarget = relData.getTargetContent();
        AbstractContentResource<?> contentResource = (AbstractContentResource<?>) getApiResource().wrapIntoResource(relTarget);
        ResourceReference ref = new ResourceReference(setIds ? relData.getName() : null, autoSuffix(contentResource.getURI()));
        if (relData.getType() == WGContent.RELATIONTYPE_PROTECTED) {
            ref.protect = true;
        }
        return ref;
        
    }
    
    @Override
    public void enhanceReference(ResourceReference ref, ReferenceCollection.Parameters params) throws WGException {

        super.enhanceReference(ref, params);

        List<ResourceReference> relations = new ArrayList<ResourceReference>();
        for (String rel : params.relations) {
            WGRelationData relData = getDoc().getRelationData(rel);
            if (relData != null) {
                ResourceReference relReference = createReferenceFromRelation(relData, true);
                relations.add(relReference);
            }
        }
        if (relations.size() > 0) {
            ref.relations = createRelationList(relations);
        }
            
        
     
    }
    
    
    @Override
    public void addRelation(String name, String uriStr) throws URISyntaxException {
        ResourceReference relation = new ResourceReference(name, new URI(uriStr));
        _incomingRelations.add(relation);
    }
    
    
    @Override
    public String getId() throws WGException {
        return getDoc().getContentKey(false).toString();
    }


}