package de.innovationgate.wga.services.rest;

import java.io.IOException;
import java.util.Map;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.core.Configuration;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;

import org.glassfish.jersey.server.filter.UriConnegFilter;

public class V1UriConnegFilter extends UriConnegFilter {

    public V1UriConnegFilter(@Context Configuration rc) {
        super(rc);
    }
    
    public V1UriConnegFilter(Map<String, MediaType> mediaTypeMappings, Map<String, String> languageMappings) {
        super(mediaTypeMappings, languageMappings);
    }
    
    @Override
    public void filter(ContainerRequestContext rc) throws IOException {
        if (rc.getUriInfo().getPath().equals("v1") || rc.getUriInfo().getPath().startsWith("v1/")) {
            super.filter(rc);
        }
    }

}
