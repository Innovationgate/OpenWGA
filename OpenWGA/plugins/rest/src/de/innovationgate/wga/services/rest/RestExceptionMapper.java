package de.innovationgate.wga.services.rest;

import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import de.innovationgate.webgate.api.WGAuthorisationException;
import de.innovationgate.webgate.api.WGDuplicateKeyException;
import de.innovationgate.wga.services.rest.v1.resources.Envelope;
import de.innovationgate.wga.services.rest.v1.resources.RestError;
import de.innovationgate.wga.services.rest.v1.types.Status;

@Provider
@Produces({MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML})
public class RestExceptionMapper implements ExceptionMapper<Throwable> {
    
    @Context
    private HttpHeaders _headers;
    
    @Context
    private Application _application;

    @Override
    public Response toResponse(Throwable exception) {

        // Determine status and message
        int status = 500;
        String message = exception.getMessage();
        
        if (exception instanceof WebApplicationException) {
            status = ((WebApplicationException) exception).getResponse().getStatus();
        }
        else if (exception instanceof WGDuplicateKeyException) {
            status = 409;
        }
        else if (exception instanceof WGAuthorisationException) {
            status = 403;
        }
        
        
        if (status == 500) {
            RestApplication.LOG.error("Exception in REST web service", exception);
        }
        
        // Create and fill envelope
        Envelope env = new Envelope();
        env.status = Status.ERROR;
        
        env.error = new RestError();
        env.error.code = status;
        
        
        env.error.message = message;
        env.error.type = exception.getClass().getName();
        
        Throwable cause = exception;
        while (true) {
            
            Throwable newCause = cause.getCause();
            if (newCause == null || newCause == cause) {
                break;
            }
            
            cause = newCause;
            if (cause.getMessage() != null) {
                env.error.causes.add(cause.getClass().getName() + ": " + cause.getMessage());
            }
            else {
                env.error.causes.add(cause.getClass().getName());
            }
            
        }
        
        return Response.status(status).entity(env).type(((RestApplication) _application).getOutputMediaType(_headers)).build();
    
    }

}
