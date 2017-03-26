package de.innovationgate.wga.services.rest.v1.resources.hdbmodel;


import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.services.rest.v1.resources.AbstractFileResource;

@XmlRootElement(name=HdbmodelFileResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(HdbmodelFileResource.RESOURCE_TYPE)
public class HdbmodelFileResource extends AbstractFileResource<HdbmodelContentResource> {
    
    public static final String RESOURCE_TYPE = "hdbmodelFile";
    
    public HdbmodelFileResource(HdbmodelContentResource parent, String fileName) {
        super(parent, parent.getURI().path(HdbmodelContentResource.REFLIST_FILES).path(fileName));
        _fileName = fileName;
    }

    @Override
    public String getResourceType() {
        return RESOURCE_TYPE;
    }
    
    @Override
    public String getId() throws WGException {
        return _fileName;
    }

}
