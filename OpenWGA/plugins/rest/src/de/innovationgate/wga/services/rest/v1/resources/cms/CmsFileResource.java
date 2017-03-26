package de.innovationgate.wga.services.rest.v1.resources.cms;


import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.services.rest.v1.resources.AbstractFileResource;

@XmlRootElement(name=CmsFileResource.RESOURCE_TYPE)
@XmlDiscriminatorValue(CmsFileResource.RESOURCE_TYPE)
public class CmsFileResource extends AbstractFileResource<CmsContentResource> {
    
    public static final String RESOURCE_TYPE = "file";
    
    public CmsFileResource(CmsContentResource parent, String fileName) {
        super(parent, parent.getURI().path(CmsContentResource.REFLIST_FILES).path(fileName));
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
