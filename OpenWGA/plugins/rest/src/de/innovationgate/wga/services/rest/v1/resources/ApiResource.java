package de.innovationgate.wga.services.rest.v1.resources;

import de.innovationgate.webgate.api.WGException;

public interface ApiResource {
    
    public Resource<?> wrapIntoResource(Object resourceObject) throws WGException;
    
    public DatabaseResource getParentResource();

}
