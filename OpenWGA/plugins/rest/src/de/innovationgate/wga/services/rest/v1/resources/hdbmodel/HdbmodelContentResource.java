package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;

import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriBuilderException;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.common.beans.hdbmodel.Content;
import de.innovationgate.wga.common.beans.hdbmodel.Item;
import de.innovationgate.wga.common.beans.hdbmodel.Relation;
import de.innovationgate.wga.services.rest.RestApplication.RelationInfo;
import de.innovationgate.wga.services.rest.v1.resources.AbstractContentResource;
import de.innovationgate.wga.services.rest.v1.resources.ApiResource;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.References;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;
import de.innovationgate.wgpublisher.hdb.HDBModel;
import de.innovationgate.wgpublisher.webtml.utils.UniqueNamePartFormatter;

@XmlRootElement
@XmlDiscriminatorValue(HdbmodelContentResource.RESOURCE_TYPE)
public class HdbmodelContentResource extends AbstractContentResource<Resource<?>> {
    
    public static final String RESOURCE_TYPE = "hdbmodelContent";
    
    public static final String REFLIST_FILES = "$files";
    public static final String REFLIST_RELATIONGROUPS = "$relgroups";
    public static final String REFLIST_RELATIONTARGETS = "$reltargets";
    public static final String REFLIST_RELATIONSOURCES = "$relsources";
    public static final String REF_MODEL = "$model";
    public static final String REF_DRAFT = "draft";
    public static final String REFLIST_VERSIONS = "$versions";
    
    public static final String VIRTUALMETA_NAME = "uniquename";
    public static final String VIRTUALMETA_READERS = "readers";
    public static final String VIRTUALMETA_EDITORS = "editors";
    
    public static final Set<String> VIRTUALMETAS = new HashSet<String>();
    static {
        VIRTUALMETAS.add(VIRTUALMETA_NAME);
        VIRTUALMETAS.add(VIRTUALMETA_READERS);
        VIRTUALMETAS.add(VIRTUALMETA_EDITORS);
    }

    public HdbmodelContentResource() {
        super();
    }
    
    public HdbmodelContentResource(@SuppressWarnings("rawtypes") Resource parent, WGContent content) throws WGAPIException {
        super(parent, content, createURI(parent, content));
    }

    protected static UriBuilder createURI(Resource<?> parent, WGContent content) throws WGAPIException {
        UriBuilder uri = parent.getURI().path(HDBModel.getID(content.getParentContent())).path(HDBModel.getID(content));
        if (!content.getStatus().equals(WGContent.STATUS_RELEASE)) {
            uri.path(REFLIST_VERSIONS).path(String.valueOf(content.getVersion()));
        }
        return uri; 
    }

    @Override
    protected void fillReferenceList(ReferenceCollection list) throws IllegalArgumentException, UriBuilderException, WGException {
        
        ApiResource api = getApiResource();
        
        // Files
        if (REFLIST_FILES.equals(list.getIdString())) {
            
            list.noPaging();
            list.setAutoSuffixRefURIs(false);
            for (String fileName : getDoc().getFileNames()) {
                list.add(fileName, new HdbmodelFileResource(this, fileName));
            }
            
        }
        
        // Relation groups
        else if (REFLIST_RELATIONGROUPS.equals(list.getIdString())) {
            list.noPaging();
            HDBModel model = HDBModel.getModel(getDoc().getDatabase());
            Content contentModel = (Content) model.getModelForContent(getDoc());
            for (Relation rel : contentModel.getRelations()) {
                if (rel.isGroup()) {
                    list.add(rel.getName(), new HdbmodelRelationGroupResource(this, getDoc(), rel.getName()));
                }
            }
        }
        else if (REFLIST_RELATIONTARGETS.equals(list.getId().getName())) {
            String relationName = list.getId().getParams().get("relation");
            if (relationName == null) {
                throw new WebApplicationException("Invalid relation target URL: Parameter 'relation' missing", 400);
            }
            
            HDBModel model = HDBModel.getModel(getDoc().getDatabase());
            SkippingIterator<WGContent> targets = model.getRelationTargets(getDoc(), getDoc().getContentClass(), relationName).getResultIterator();
            list.addUntilFull(targets);
        }
        
        // Relation sources
        else if (REFLIST_RELATIONSOURCES.equals(list.getId().getName())) {
            
            String contentClass = list.getId().getParams().get("ownerClass");
            String relationName = list.getId().getParams().get("relation");
            if (contentClass == null || relationName == null) {
                throw new WebApplicationException("Invalid relation source URL: Parameter 'ownerClass' or 'relation' missing", 400);
            }
            
            HDBModel model = ((HdbmodelApiResource) api).getModel();
            SkippingIterator<WGContent> sources = model.getRelationSources(getDoc(), contentClass, relationName).getResultIterator();
            list.addUntilFull(sources);
        }
        
        // Versions
        else if (REFLIST_VERSIONS.equals(list.getId().getName())) {
            list.noPaging();
            for (WGContent con : getDoc().getStructEntry().getAllContent(true)) {
                list.add(String.valueOf(con.getVersion()), api.wrapIntoResource(con));
            }
        }
        
        // Sub storages
        else {
            String contentName = getDoc().getUniqueName() + "." + UniqueNamePartFormatter.INSTANCE.format(list.getIdString());
            WGContent storage = getDoc().getDatabase().getContentByName(contentName);
            if (storage == null) {
                throw new WebApplicationException("Unknown storage '" + list.getIdString() + "'", 404);
            }
            
            SkippingIterator<WGContent> contents = storage.getChildContentIterator(getDefaultRefListSize());
            list.addUntilFull(contents);
        }
        
        
        
    }

    @Override
    protected void addReferences(References refs) throws WGException, IllegalArgumentException, UriBuilderException {
        
        refs.add(new ReferenceCollection(this, REFLIST_FILES, getURI()));
        refs.add(new ReferenceCollection(this, REFLIST_RELATIONGROUPS, getURI()));
        
        HdbmodelApiResource api = (HdbmodelApiResource) getApiResource();
        refs.add(new ResourceReference(REF_MODEL, autoSuffix(api.getURI().path(HdbmodelApiResource.REF_MODEL).path(HdbmodelDefinitionResource.REFLOOKUP_LOOKUP).matrixParam("pageKey", String.valueOf(getDoc().getStructKey()))), HdbmodelModelDocumentResource.RESOURCE_TYPE));
        
        WGContent draft = getDoc().getStructEntry().getContent(getDoc().getLanguage().getName(), WGContent.STATUS_DRAFT);
        if (draft != null) {
            Resource<?> draftResource = getApiResource().wrapIntoResource(draft);
            refs.add(new ResourceReference(REF_DRAFT, autoSuffix(draftResource.getURI()), HdbmodelContentResource.RESOURCE_TYPE));
        }
        
        refs.add(new ReferenceCollection(this, REFLIST_VERSIONS, getURI()));
        
        // Relation targets
        HDBModel model = HDBModel.getModel(getDoc().getDatabase());
        Content contentModel = (Content) model.getModelForContent(getDoc());
        for (Relation rel : contentModel.getRelations()) {
            Reference.Id colId = Reference.Id.create(REFLIST_RELATIONTARGETS).param("relation", rel.getName());
            refs.add(new ReferenceCollection(this, colId, getURI()));
        }
        
        // Relation sources
        List<RelationInfo> relations = getDatabaseResource().getDatabaseInfo().getRelationSourcesByContentClass().get(getDoc().getContentClass());
        if (relations != null) {
            for (RelationInfo rel : relations) {
                Reference.Id colId = Reference.Id.create(REFLIST_RELATIONSOURCES).param("ownerClass", rel.getOwnerClass()).param("relation", rel.getName());
                refs.add(new ReferenceCollection(this, colId, getURI()));
            }
        }
        
        // Child storages
        Iterator<WGContent> contents = getDoc().getChildContentIterator(getDefaultRefListSize());
        while (contents.hasNext()) {
            WGContent content = contents.next();            
            String storageId = HDBModel.getID(content);
            refs.add(new ReferenceCollection(this, storageId, getURI()));
        }
        

    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Override
    protected Set<String> getDefaultItemSet() throws WGAPIException {
        
        Set<String> itemNames = new HashSet<String>();
        
        HDBModel model = HDBModel.getModel(getDoc().getDatabase());
        Content content = (Content) model.getModelForContent(getDoc());
        for (Item item : content.getItems()) {
            itemNames.add(item.getName());
        }
        
        return itemNames;
        
    }
    
    @Path("{storage}/{content}")
    public HdbmodelContentResource getContent(@PathParam("storage") String storageId, @PathParam("content") String contentId) throws WGAPIException {
        
        String name = getDoc().getUniqueName() + "." + UniqueNamePartFormatter.INSTANCE.format(storageId) + "." + UniqueNamePartFormatter.INSTANCE.format(contentId);
        WGContent content = getDoc().getDatabase().getContentByName(name);
        if (content != null) {
            return new HdbmodelContentResource(this, content);
        }
        else {
            throw new WebApplicationException("Unknown content '" + name + "'", 404);
        }
        
    }
    
    @Path(REFLIST_FILES + "/{fileName}")
    public HdbmodelFileResource getFile(@PathParam("fileName") String fileName) throws WGAPIException {
        return new HdbmodelFileResource(this, fileName);
    }
    
    @Override
    protected Set<String> getDefaultMetaSet() {
        Set<String> metas = new HashSet<String>();
        metas.add(WGContent.META_AUTHOR);
        metas.add(WGContent.META_COAUTHORS);
        metas.add(WGContent.META_CONTENTCLASS);
        metas.add(WGContent.META_DESCRIPTION);
        metas.add(WGContent.META_IS_HIDDEN_FROM);
        metas.add(WGContent.META_KEYWORDS);
        metas.add(WGContent.META_OWNER);
        metas.add(WGContent.META_PENDINGRELEASE);
        metas.add(WGContent.META_READERS);
        metas.add(WGContent.META_STATUS);
        metas.add(WGContent.META_TITLE);
        metas.add(VIRTUALMETA_NAME);
        metas.add(VIRTUALMETA_READERS);
        metas.add(VIRTUALMETA_EDITORS);
        return metas;
    }
    
    @Override
    public void fillResource(Envelope env) throws WGAPIException {
        super.fillResource(env);
        env.key.hdbmodelContentId = HDBModel.getID(getDoc());
    }
    
    @Override
    protected Set<String> getVirtualMetaNames() {
        return VIRTUALMETAS;
    }
    
    @Override
    public de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field createMetaField(String metaName) throws WGAPIException {

        if (VIRTUALMETA_EDITORS.equals(metaName)) {
            List<String> editors = getDoc().getStructEntry().getChildEditors();
            if (editors.size() == 0) {
                return null;
            }
            Field f = new Field();
            f.name = metaName;
            f.multiValue = true;
            f.values = editors;
            return f;
        }
        else if (VIRTUALMETA_READERS.equals(metaName)) {
            List<String> readers = getDoc().getStructEntry().getReaders();
            if (readers.size() == 0) {
                return null;
            }
            Field f = new Field();
            f.name = metaName;
            f.multiValue = true;
            
            f.values = readers;
            return f;
        }
        else if (VIRTUALMETA_NAME.equals(metaName)) {
            String uniqueName = getDoc().getStructEntry().getUniqueName();
            if (uniqueName == null) {
                return null;
            }
            Field f = new Field();
            f.name = metaName;
            f.values = Collections.singletonList(uniqueName);
            return f;
        }
        else {
            return super.createMetaField(metaName);
        }
    }
    
    @Override
    protected void writeMetaData(de.innovationgate.wga.services.rest.v1.resources.DocumentResource.WriteContext writeContext, de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field f)
            throws WGAPIException {

        ContentWriteContext contentContext = (ContentWriteContext) writeContext;
        
        if (VIRTUALMETA_EDITORS.equals(f.name)) {
            getDoc().getStructEntry().setChildEditors(f.values);
            getDoc().getStructEntry().setPageEditors(f.values);
            contentContext.saveStruct = true;
        }
        else if (VIRTUALMETA_READERS.equals(f.name)) {
            getDoc().getStructEntry().setReaders(f.values);
            contentContext.saveStruct = true;
        }
        else if (VIRTUALMETA_NAME.equals(f.name)) {
            getDoc().getStructEntry().setUniqueName(f.values.size() > 0 ? (String) f.values.get(0) : null);
            contentContext.saveStruct = true;
        }
        
        // Only used on creation, unwritable for existing contents
        else if (WGContent.META_CONTENTCLASS.equals(f.name)) {
            
        }
        
        else {
            super.writeMetaData(writeContext, f);
        }
    }
    
    @Path(REFLIST_RELATIONGROUPS + "/{relname}")
    public HdbmodelRelationGroupResource getRelationGroup(@PathParam("relname") String relName) throws WGAPIException {
        return new HdbmodelRelationGroupResource(this, getDoc(), relName);
    }
    
    @Override
    protected Set<String> getDefaultRelationSet() throws WGAPIException {

        Set<String> rels = new HashSet<String>();
        HDBModel model = HDBModel.getModel(getDoc().getDatabase());
        Content contentModel = (Content) model.getModelForContent(getDoc());
        for (Relation rel : contentModel.getRelations()) {
            if (!rel.isGroup()) {
                rels.add(rel.getName());
            }
        }
        
        for (String relName : getDoc().getRelationNames()) {
            if (relName.startsWith(HDBModel.PARENT_RELATION_PREFIX)) {
                rels.add(relName);
            }
        }
        
        return rels;
        
    }
    
    @POST
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postContent(final HdbmodelContentResource contentResource) throws WGException {
        return ((HdbmodelApiResource) getApiResource()).createChildContent(contentResource, null, getDoc());
    }

    @Path("{storage}")
    @POST
    @Consumes({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
    public Response postContent(final HdbmodelContentResource contentResource, @PathParam("storage") String storageId) throws WGException {
        return ((HdbmodelApiResource) getApiResource()).createChildContent(contentResource, storageId, getDoc());
    }
    
    @Override
    public Response delete() throws WGException {
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        
        if (isNew()) {
            return buildResponse(Response.ok(env));
        }
        
        try {
            getDoc().performRemoveCheck();
        }
        catch (WGAuthorisationException e) {
            return buildAuthorisationExceptionResponse(e, env);
        }
        catch (ResourceIsLockedException e) {
            return buildLockedExceptionResponse(env, e);
        }
        
        ((HdbmodelApiResource) getApiResource()).getModel().deleteContent(getDoc());
        
        return buildResponse(Response.ok(env));
    }
    
    @PUT
    @Consumes({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
    public Response put(HdbmodelContentResource res) throws WGException {
        return defaultPut(res);
    }
    
    @Override
    public String getId() throws WGException {
        return getDoc().getContentKey(false).toString();
    }
    
    @Override
    public void enhanceReference(ResourceReference ref, de.innovationgate.wga.services.rest.v1.types.ReferenceCollection.Parameters params) throws WGException {
        super.enhanceReference(ref, params);
        ref.hdbmodelContentId = HDBModel.getID(getDoc());
    }
    
    @Override
    protected WGContent resolveContentReference(ResourceReference ref) throws WGException {
        
        URI pathUri = getRootResource().getUriInfo().getBaseUri().relativize(ref.href);
        List<String> pathElements = WGUtils.deserializeCollection(pathUri.getPath(), "/");
        List<String> nameElements = pathElements.subList(4, pathElements.size());
        String name = WGUtils.serializeCollection(nameElements, ".");
        return getDoc().getDatabase().getContentByName(name);
        
    }
    
    @Override
    public boolean isLinkable() {
        return getDatabaseResource().isApiEnabled(HdbmodelApiResource.RESOURCE_TYPE);
    }

}
