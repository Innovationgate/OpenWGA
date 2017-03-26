package de.innovationgate.wga.services.rest.v1.types;

import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;
import org.glassfish.jersey.uri.UriComponent;

import de.innovationgate.utils.CountReportingIterator;
import de.innovationgate.utils.PrefetchingIterator;
import de.innovationgate.utils.SkippingIterator;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.services.rest.CollectionEnhancer;
import de.innovationgate.wga.services.rest.RestApplication;
import de.innovationgate.wga.services.rest.v1.resources.ApiResource;
import de.innovationgate.wga.services.rest.v1.resources.Resource;

@XmlRootElement
@XmlDiscriminatorValue("collection")
public class ReferenceCollection extends Reference {
    
    public static class Parameters {
        public String colName;
        public List<String> items = new ArrayList<String>();
        public List<String> metaData = new ArrayList<String>();
        public List<String> relations = new ArrayList<String>();
        public List<String> enhancers = new ArrayList<String>();
        public CollectionEnhancer specialEnhancer = null; 
        
        public void addParameter(String name, String value) {
            
            if (RestApplication.URLPARAM_ITEM.equals(name)) {
                items.add(value);
            }
            else if (RestApplication.URLPARAM_META.equals(name)) {
                metaData.add(value);
            }
            else if (RestApplication.URLPARAM_RELATION.equals(name)) {
                relations.add(value);
            }
            else if (RestApplication.URLPARAM_ENHANCER.equals(name)) {
                enhancers.add(value);
            }
            
        }
    }

    private transient Resource<?> _resource;
    private transient Parameters _parameters = null; 
    
    private transient ElementFilter _elementFilter = null;
    
    @XmlTransient
    public ElementFilter getFilter() {
        return _elementFilter;
    }

    @XmlTransient
    public void setFilter(ElementFilter filter) {
        _elementFilter = filter;
    }

    private boolean _autoSuffixRefURIs = true;
    
    @XmlElementWrapper(name="collection")
    @XmlElement(name="ref")
    public List<Reference> refs = null;

    @XmlAttribute
    public Integer offset;
    
    @XmlAttribute
    public Integer size;
    
    @XmlAttribute
    public Integer endIndex;
    
    @XmlAttribute
    public Integer total;
    
    @XmlAttribute
    public URI nextPage;
    
    @XmlAttribute
    public URI previousPage;
    
    @XmlAttribute
    public Boolean pageable;
    
    @XmlAttribute
    public Boolean supportsAutoScroll;
   

    
    public ReferenceCollection() {
    }
    
    public ReferenceCollection(Resource<?> resource, String id) throws UnavailableResourceException {
        this(resource, new Id(id), null);
    }
    
    public ReferenceCollection(Resource<?> resource, String idParam, UriBuilder baseUri) throws UnavailableResourceException {
        this(resource, new Id(idParam), baseUri);
    }
    
    public ReferenceCollection(Resource<?> resource, Id idParam, UriBuilder baseUri) throws UnavailableResourceException {
        _resource = resource;
        setId(idParam);
        if (baseUri != null) {
            UriBuilder uriBuilder = baseUri.path(idParam.getName());
            for (Map.Entry<String,String> param : idParam.getParams().entrySet()) {
                uriBuilder.matrixParam(param.getKey(), param.getValue());
            }
            this.href = resource.autoSuffix(uriBuilder);
        }
        
    }

    public Reference add(String id, Resource<?> resource) throws WGException {
        if (refs == null) {
            refs = new ArrayList<Reference>();
        }
        if (pageable == null) {
            pageable = true;
        }
        
        Reference ref = resource.createCollectionReference(this, id, _autoSuffixRefURIs);
        if (ref != null) {
            int colOffset = (offset != null ? offset : 0);
            ref.index = new Integer(colOffset + refs.size() + 0);
            this.refs.add(ref);
        }
        return ref;
        
    }


    
    public JsonObjectReference add(JsonObject obj) throws WGException {
        
        if (refs == null) {
            refs = new ArrayList<Reference>();
        }
        if (pageable == null) {
            pageable = true;
        }
        
        JsonObjectReference ref = new JsonObjectReference(obj);
        refs.add(ref);
        return ref;
        
    }
    
    public Reference add(Resource<?> resource) throws WGException {
        return add(null, resource);
    }
    
    public boolean isCompleteList() {
        
        if (offset == null || size == null) {
            return true;
        }
        else if (offset == 1 && size == -1) {
            return true;
        }
        else {
            return false;
        }
        
    }
    
    public boolean isFull() {
        return (size != null && (size == -1 || refs.size() >= size));
    }
    
    public void noPaging() {
        pageable = false;
        size = null;
        offset = null;
        
    }

    @XmlTransient
    public Parameters getParameters() {
        return _parameters;
    }

    public void setParameters(Parameters parameters) {
        _parameters = parameters;
    }

    @XmlTransient
    public boolean isAutoSuffixRefURIs() {
        return _autoSuffixRefURIs;
    }

    public void setAutoSuffixRefURIs(boolean disableAutoSuffix) {
        _autoSuffixRefURIs = disableAutoSuffix;
    }

    protected void afterPagedCollection(Iterator<?> iterator) throws WGException {
        
        size = refs != null ? refs.size() : 0;
        
        if (iterator instanceof CountReportingIterator) {
            endIndex = ((CountReportingIterator<?>) iterator).getCurrentOffset() - 1;
        }
        else {
            endIndex = offset + size - 1;
        }
        
        if (iterator.hasNext()) {
            UriBuilder nextPageURI = UriBuilder.fromUri(href);
            nextPageURI.queryParam(RestApplication.URLPARAM_OFFSET, endIndex + 1);
            
            // Autoscroll: Determine if capable and add to paging URL if requested
            if (iterator instanceof PrefetchingIterator<?>) {
                Resource<?> nextRes = _resource.getApiResource().wrapIntoResource(doFilter(((PrefetchingIterator<?>) iterator).previewNextValue()));
                if (nextRes != null) {
                    String elementId = nextRes.getId();
                    if (elementId != null) {
                        supportsAutoScroll = true;
                        String autoScrollStr = _resource.getRootResource().getWga().getRequest().getParameter(RestApplication.URLPARAM_AUTOSCROLL);
                        if (autoScrollStr != null ) {
                            nextPageURI.queryParam(RestApplication.URLPARAM_AUTOSCROLLTARGET, elementId);
                        }
                    }
                }
            }
            
            
            nextPage = buildPagingURI(nextPageURI);
        }
        
        
        if (offset != null && offset > 0) {
            int previousPageOffset = (offset < size ? 0 : offset - size);
            UriBuilder prevPageURI = UriBuilder.fromUri(href);
            prevPageURI.queryParam(RestApplication.URLPARAM_OFFSET, previousPageOffset);
            previousPage = buildPagingURI(prevPageURI);
        }
        
        if (iterator instanceof CountReportingIterator<?>) {
            int count = ((CountReportingIterator<?>) iterator).getCount();
            if (count != -1) {
                total = count;
            }
        }
        
        
        
        
    }
    
    public URI buildPagingURI(UriBuilder uriBuilder) {
        
        UriInfo uriInfo = _resource.getRootResource().getUriInfo();
        MultivaluedMap<String,String> queryParams = uriInfo.getQueryParameters();
        
        for (Map.Entry<String,List<String>> param : queryParams.entrySet()) {
            if (!RestApplication.NONINHERITED_PAGING_URLPARAMS.contains(param.getKey())) {
                for (String value : param.getValue()) {
                    uriBuilder.queryParam(param.getKey(), UriComponent.encode(value, UriComponent.Type.QUERY_PARAM_SPACE_ENCODED));
                }
            }
        }
        
        return _resource.autoSuffix(uriBuilder);
        
    }

    protected void beforePagedCollection(SkippingIterator<?> iterator) throws WGException {
        
        if (offset != null && offset > 0) {
            iterator.skip(offset);
        }
        if (iterator instanceof PrefetchingIterator<?>) {
    
            PrefetchingIterator<?> prefetcher = (PrefetchingIterator<?>) iterator;
    
            HttpServletRequest request = _resource.getRootResource().getWga().getRequest();
            String autoScrollStr = request.getParameter(RestApplication.URLPARAM_AUTOSCROLL);
            if (autoScrollStr != null) {
                int autoScroll = Integer.parseInt(autoScrollStr);
                String autoScrollTarget = request.getParameter(RestApplication.URLPARAM_AUTOSCROLLTARGET);
                if (autoScrollTarget != null) {
                
                    while (true) {
                        if (!prefetcher.hasNext()) {
                            throw new WebApplicationException("AutoScroll did reach the end of the collection without finding the target", 412);
                        }
                        
                        Resource<?> res = _resource.getApiResource().wrapIntoResource(doFilter(prefetcher.previewNextValue()));
                        if (res != null && res.getId().equals(autoScrollTarget)) {
                            break;
                        }
                        
                        if (autoScroll == 0) {
                            throw new WebApplicationException("AutoScroll did not reach the intended target in " + Integer.parseInt(autoScrollStr) + " steps", 412);
                        }
                        
                        prefetcher.next();
                        offset++;
                        autoScroll--;
                    }
                    
                }
            }
        }
    }

    public <T extends Object> void addUntilFull(SkippingIterator<T> it) throws WGException {
        
        ApiResource api = _resource.getApiResource();
        
        beforePagedCollection(it);
        
        while (it.hasNext()) {
            T element = it.next();

            Object filteredElement = doFilter(element);
            Resource<?> res = api.wrapIntoResource(filteredElement);
            if (!res.isEmpty()) {
                Reference ref = add(res);
                if (it instanceof CountReportingIterator) {
                    ref.index = ((CountReportingIterator<?>) it).getCurrentOffset() - 1;
                }
                if (isFull()) {
                    break;
                }
            }
        }
        
        afterPagedCollection(it);
        
        
        
    }
    
    public JsonObject toJSON() {
        
        JsonObjectBuilder obj = Json.createObjectBuilder();
        addIfFilled(obj, "offset", offset);
        addIfFilled(obj, "size", size);
        addIfFilled(obj, "endIndex", endIndex);
        addIfFilled(obj, "total", total);
        addIfFilled(obj, "pageable", pageable);
        addIfFilled(obj, "nextPage", nextPage);
        addIfFilled(obj, "previousPage", previousPage);
        addIfFilled(obj, "supportsAutoScroll", supportsAutoScroll);
        if (refs != null) {
            JsonArrayBuilder refArray = Json.createArrayBuilder();
            for (Reference ref : refs) {
                if (ref instanceof JsonObjectReference) {
                    refArray.add(((JsonObjectReference) ref).getJsonObject());
                }
                else {
                    JsonObjectBuilder refObj = Json.createObjectBuilder();
                    refObj.add("id", ref.getIdString());
                    addIfFilled(refObj, "href", ref.href);
                    refArray.add(refObj.build());
                }
            }
            obj.add("collection", refArray.build());
        }
        
        return obj.build();
        
        
    }

    private void addIfFilled(JsonObjectBuilder json, String name, URI nextPage2) {
        if (nextPage2 != null) {
            json.add(name, nextPage2.toString());
        }
    }
    
    private void addIfFilled(JsonObjectBuilder json, String name, Boolean value) {
        if (value != null) {
            json.add(name, value);
        }
    }
    
    private void addIfFilled(JsonObjectBuilder json, String name, Integer value) {
        if (value != null) {
            json.add(name, value);
        }
    }
    
    private Object doFilter(Object element) throws WGException {
        
        if (_elementFilter != null) {
            return _elementFilter.filterElement(element);
        }
        else {
            return element;
        }
            
        
    }
    

}
