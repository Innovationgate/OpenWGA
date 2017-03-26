package de.innovationgate.wga.services.rest.v1.resources;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.ws.rs.DELETE;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.MetaInfo;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.locking.ResourceIsLockedException;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.types.DocumentKey;
import de.innovationgate.wga.services.rest.v1.types.FieldList;
import de.innovationgate.wga.services.rest.v1.types.JSONFieldList;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wga.services.rest.v1.types.ResourceReference;

public abstract class DocumentResource<DocType extends WGDocument, ParentType extends Resource<?>> extends ExternalRefsResource<ParentType> implements RESTEntry {
    
    public static class WriteContext {
        
    }
    
    protected WriteContext createWriteContext() {
        return new WriteContext();
    }
    
    @XmlRootElement
    public static class Field {
        
        @XmlAttribute
        public String name;
        
        @XmlAttribute
        public Boolean delete;
        
        @XmlElementWrapper(name="values")
        @XmlElement(name="value")
        public List<?> values;
        
        @XmlAttribute
        public boolean multiValue = false;
        
        public static Field create(String name, Object value) {
            Field field = new Field();
            field.name = name;
            if (value instanceof List<?>) {
                field.values = new ArrayList<Object>((List<?>) value); 
                field.multiValue = true;
            }
            else {
                field.values = Collections.singletonList(value);
            }
            return field;
        }
        
    }
    
    @XmlRootElement
    public static class File {
        
        @XmlAttribute
        public String name;
        
        @XmlAttribute
        public String hash;
        
        @XmlAttribute
        public URI href;
        
        @XmlAttribute
        public long size;
        
    }
    

    public DocType getDoc() {
        return _doc;
    }

    private DocType _doc;
    
    private FieldList _incomingItems = new JSONFieldList();
    private FieldList _incomingMetas = new JSONFieldList();

    private String _newDocName;

    public DocumentResource(ParentType parent, DocType doc, UriBuilder uri) {
        super(parent, uri);
        _doc = doc;
    }
    
    public DocumentResource(ParentType parent, String newDocName, UriBuilder uri) {
        super(parent, uri);
        _newDocName = newDocName;
    }
    
    public DocumentResource() {
        super();
    }
    
    public boolean isNew() {
        return _doc == null;
    }

    @XmlElement
    public FieldList getMetaData() throws WGException {
        
        if (isNew()) {
            return _incomingMetas;
        }

        String metaSet = getRootResource().getWga().getRequest().getParameter("metaSet");
        String[] selectedMetas = getRootResource().getWga().getRequest().getParameterValues("meta");
        if (metaSet == null) {
            if (selectedMetas == null || selectedMetas.length == 0) {
                metaSet = "default";
            }
            else {
                metaSet = "empty";
            }
        }
        Set<String> metaNamesSet = new HashSet<String>();
        if (metaSet.equals("default")) {
            metaNamesSet.addAll(getDefaultMetaSet());
        }
        else if (metaSet.equals("all")) {
            metaNamesSet.addAll(getDoc().getMetaNames());
            metaNamesSet.addAll(getVirtualMetaNames());
        }
        if (selectedMetas != null) {
            metaNamesSet.addAll(Arrays.asList(selectedMetas));
        }
        
        List<String> metaNames = new ArrayList<String>(metaNamesSet);
        Collections.sort(metaNames);
        List<Field> metaData = new ArrayList<Field>();
        for (String metaName : metaNames) {
            Field mf = createMetaField(metaName);
            if (mf != null) {
                metaData.add(mf);
            }
        }
        return createFieldList(metaData).merge(_incomingMetas);
    }
    
    protected Set<String> getVirtualMetaNames() {
        return Collections.emptySet();
    }

    protected abstract Set<String> getDefaultMetaSet();

    
    @XmlElement
    public void setMetaData(FieldList md) {
        _incomingMetas = md;
    }

    public Field createMetaField(String metaName) throws WGAPIException {
        
        MetaInfo metaInfo = getDoc().getMetaInfo(metaName);
        if (metaInfo == null) {
            return null;
        }
        
        Field mf = new Field();
        mf.name = metaName.toLowerCase();
        if (metaInfo.isMultiple()) {
            mf.multiValue = true;
            List<?> values = getDoc().getMetaDataList(metaName);
            if (WGUtils.nullSafeEquals(metaInfo.getDefaultValue(), values)) {
                return null;
            }
            
            mf.values = values;
        }
        else {
            Object value = getDoc().getMetaData(metaName);
            if (WGUtils.nullSafeEquals(metaInfo.getDefaultValue(), value)) {
                return null;
            }
            mf.values = Collections.singletonList(value);
        }
        
       
        return mf;
    }

    @XmlElement
    public FieldList getItems() throws WGException {
        
        if (isNew()) {
            return _incomingItems;
        }
        
        String itemSet = getRootResource().getWga().getRequest().getParameter("itemSet");
        
        String[] selectedItems = getRootResource().getWga().getRequest().getParameterValues("item");
        if (itemSet == null) {
            if (selectedItems == null || selectedItems.length == 0) {
                itemSet = "default";
            }
            else {
                itemSet = "empty";
            }
        }
        Set<String> itemNamesSet = new HashSet<String>();;
        if (itemSet.equals("default")) {
            itemNamesSet.addAll(getDefaultItemSet());
        }
        else if (itemSet.equals("all")) {
            itemNamesSet.addAll(getDoc().getItemNames());
        }
        
        if (selectedItems != null) {
            itemNamesSet.addAll(Arrays.asList(selectedItems));
        }
        
        List<Field> items = new ArrayList<Field>();
        
        List<String> itemNames = new ArrayList<String>(itemNamesSet);
        Collections.sort(itemNames);
        for (String itemName : itemNames) {
            Field mf = createItemField(itemName);
            if (mf != null) {
                items.add(mf);
            }
        }
        
        if (items.size() > 0) {
            return createFieldList(items).merge(_incomingItems);
        }
        else {
            return null;
        }
    }

    protected Set<String> getDefaultItemSet() throws WGAPIException {
        return new HashSet<String>(getDoc().getItemNames());
    }
    
    @XmlElement
    public void setItems(FieldList items) {
        _incomingItems = items;
    }
    
    public Field createItemField(String itemName) throws WGAPIException {
        
        Field item = new Field();
        item.name = itemName;
        Object itemValue = getDoc().getItemValue(itemName);
        if (itemValue instanceof Collection<?>) {
            item.multiValue = true;
            item.values = new ArrayList<Object>((Collection<?>) itemValue); 
        }
        else {
            item.values = Collections.singletonList(itemValue);
        }
        return item;
    }
    
    @Override
    public void fillResource(Envelope env) throws WGAPIException {
        
        if (isNew()) {
            throw new WebApplicationException("Non-existent document", 404);
        }
        
        env.resource = this;
        env.key = new DocumentKey(this, getDoc().getDatabase(), getDoc().getDocumentKeyObj());
        env.timestamps = new Timestamps();
        env.timestamps.created = getDoc().getCreated();
        env.timestamps.lastModified = getDoc().getLastModified();
    }
    
    public Response defaultPut(DocumentResource<?,?> res) throws WGException {
        
        Envelope env = new Envelope();
        env.self = autoSuffix(getURI());
        
        try {
            if (isNew()) {
                _doc = putCreateDocument(res);
            }
            else {
                getDoc().performSaveCheck();
            }
        }
        catch (WGAuthorisationException e) {
            return buildAuthorisationExceptionResponse(e, env);
        }
        catch (ResourceIsLockedException e) {
            return buildLockedExceptionResponse(env, e);
        }
        
        writeDocumentData(res);
        env.key = new DocumentKey(this, getDoc().getDatabase(), getDoc().getDocumentKeyObj());
        
        if (getRootResource().getUriInfo().getQueryParameters().containsKey(RestApplication.URLPARAM_RETURNRESOURCE)) {
            fillResource(env);
        }
        
        return buildResponse(Response.ok(env));
        
    }

    public void writeDocumentData(DocumentResource<?, ?> res) throws WGException, WGAPIException {
        DocType doc = getDoc();
        WriteContext writeContext = createWriteContext();
        Iterator<Field> items = res.getItems().getFields();
        while (items.hasNext()) {
            Field item = items.next();
            if (item.delete != null && item.delete == true) {
                doc.removeItem(item.name);
            }
            else if (item.multiValue) {
                doc.setItemValue(item.name, item.values);
            }
            else if (item.values != null && item.values.size() > 0) {
                doc.setItemValue(item.name, item.values.get(0));
            }
            else {
                doc.setItemValue(item.name, null);
            }
        }
        
        Iterator<Field> metas = res.getMetaData().getFields();
        Set<String> writableMetas = getDefaultMetaSet();
        while (metas.hasNext()) {
            Field meta = metas.next();
            
            if (meta.delete != null && meta.delete == true) {
                throw new WebApplicationException("Metadata fields cannot be deleted", 409);
            }
            
            if (!writableMetas.contains(meta.name.toUpperCase())) {
                continue;
            }
            
            writeMetaData(writeContext, meta);
        }
        
        
        writeAdditionalData(writeContext, res);
        saveData(writeContext);
    }
    
    @DELETE
    @Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XHTML_XML})
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
        
        getDoc().remove();
        
        return buildResponse(Response.ok(env));
        
    }

    protected abstract DocType putCreateDocument(Resource<?> res) throws WGAPIException;

    public void saveData(WriteContext writeContext) throws WGAPIException {
        getDoc().save();
    }

    protected void writeAdditionalData(WriteContext writeContext, Resource<?> es) throws WGException {
    }

    protected void writeMetaData(WriteContext writeContext, Field meta) throws WGAPIException {
        if (meta.multiValue) {
            getDoc().setMetaData(meta.name, meta.values);
        }
        else if (meta.values.size() > 0) {
            getDoc().setMetaData(meta.name, meta.values.get(0));
        }
        else {
            getDoc().setMetaData(meta.name, null);
        }
    }

    protected String getNewDocName() {
        return _newDocName;
    }

    @Override
    public void enhanceReference(ResourceReference ref, ReferenceCollection.Parameters params) throws WGException {

        List<Field> items = new ArrayList<DocumentResource.Field>();
        for (String item : params.items) {
            Field field = createItemField(item);
            if (field != null) {
                items.add(field);
            }
        }
        if (items.size() > 0) {
            ref.items = createFieldList(items);
        }
        
        List<Field> metas = new ArrayList<DocumentResource.Field>();
        for (String meta : params.metaData) {
            Field field = createMetaField(meta);
            if (field != null) {
                metas.add(field);
            }
        }
        if (metas.size() > 0) {
            ref.metaData = createFieldList(metas);
        }
        
    }

    @Override
    public void enhanceResource(Resource<?> ref, Parameters parameters) throws WGException {
        DatabaseResource dbRes = getDatabaseResource();
        for (String enhancer : parameters.enhancers) {
            dbRes.customEnhanceResource(this, enhancer, getDoc());
        }
    }

    @Override
    public Date getLastModified() throws WGException {
        return getDoc().getLastModified();
    }
    
    public void addItem(String name, Object value) {
        Field field = Field.create(name, value);
        _incomingItems.addField(field);
    }
    
    public void addMetaData(String name, Object value) {
        Field field = Field.create(name, value);
        _incomingMetas.addField(field);
    }
    
    @Override
    public void addRelation(String name, String value) throws URISyntaxException {
        throw new UnsupportedOperationException("Adding relations is not supported on this entry");
    }
    

    @Override
    protected WGDocument getCustomEnhancerContextDoc() {
        return getDoc();
    }

}    

