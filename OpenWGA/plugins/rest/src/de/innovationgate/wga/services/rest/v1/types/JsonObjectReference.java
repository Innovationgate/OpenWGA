package de.innovationgate.wga.services.rest.v1.types;

import javax.json.JsonObject;

import de.innovationgate.utils.UIDGenerator;

public class JsonObjectReference extends Reference {
    
    private JsonObject _jsonObject;
    
    public JsonObjectReference(JsonObject jsonObject) {
        super(UIDGenerator.generateUID(), null, null);
        _jsonObject = jsonObject;
    }

    public JsonObject getJsonObject() {
        return _jsonObject;
    }

}
