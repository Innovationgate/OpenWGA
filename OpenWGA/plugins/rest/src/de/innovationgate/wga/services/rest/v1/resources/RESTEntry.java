package de.innovationgate.wga.services.rest.v1.resources;

import java.net.URISyntaxException;

/**
 * Interface representing the RESTEntry object in TMLScript
 */
public interface RESTEntry {
    
    public void addItem(String name, Object value);
    public void addMetaData(String name, Object value);
    public void addRelation(String name, String uri) throws URISyntaxException;

}
