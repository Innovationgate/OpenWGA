package de.innovationgate.wga.services.rest.v1.resources.custom;

import javax.json.JsonObject;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.services.rest.v1.resources.Resource;
import de.innovationgate.wga.services.rest.v1.resources.custom.CustomApiResource.QueryResource;
import de.innovationgate.wga.services.rest.v1.types.JsonObjectReference;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.ReferenceCollection;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;

public class JsonResolveableResource extends Resource<CustomApiResource> {
    
    public static final String RESOURCE_TYPE = "json";
    
    private QueryResource _queryResource;
    private JsonObject _jsonObject;

    public JsonResolveableResource() {
    }
    
    public JsonResolveableResource(CustomApiResource parent, QueryResource queryResource) throws WGException {
        super(parent, null);
        _queryResource = queryResource;
        _jsonObject = getParentResource().resolveToJson(_queryResource.getResourceObject(), _queryResource.getCollectionId());
        if (_jsonObject == null) {
            setEmpty(true);
        }
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }

    @Override
    public String getId() throws WGException {
        if (_jsonObject != null) {
            return String.valueOf(_jsonObject.get("id"));
        }
        else {
            return null;
        }
        
    }
    
    @Override
    public Reference createCollectionReference(ReferenceCollection list, String id, boolean autoSuffix) throws WGException,
            TMLScriptException {
        return new JsonObjectReference(_jsonObject);
        
    }

}
