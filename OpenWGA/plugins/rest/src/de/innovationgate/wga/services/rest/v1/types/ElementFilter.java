package de.innovationgate.wga.services.rest.v1.types;

import de.innovationgate.webgate.api.WGException;

public interface ElementFilter {
    
    /** Should return null to omit items. May return wrapped/converted items to be used for resource construction.
     * @param element
     * @return
     * @throws WGException
     */
    public Object filterElement(Object element) throws WGException;
    
}