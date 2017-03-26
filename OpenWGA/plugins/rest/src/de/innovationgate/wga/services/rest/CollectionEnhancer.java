package de.innovationgate.wga.services.rest;

import de.innovationgate.webgate.api.WGDocument;
import de.innovationgate.wga.services.rest.v1.resources.RESTEntry;

public interface CollectionEnhancer {
    
    public void enhance(RESTEntry ref, WGDocument contextDoc) throws Throwable;

}
