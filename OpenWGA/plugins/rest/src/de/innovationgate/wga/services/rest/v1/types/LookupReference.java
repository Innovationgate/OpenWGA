package de.innovationgate.wga.services.rest.v1.types;

import java.net.URI;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

@XmlRootElement
@XmlDiscriminatorValue("lookup")
public class LookupReference extends Reference {
    
    @XmlAttribute
    public String uriExtension;
    
    public String lookupType;

    public LookupReference() {
    }
    
    public LookupReference(String id, URI uri, String extensionParam, String resourceTypeParam) {
        this(id, uri, extensionParam, resourceTypeParam, "resource");
    }
    
    public LookupReference(String id, URI uri, String extensionParam, String resourceTypeParam, String lookupTypeParam) {
        this(Reference.Id.create(id), uri, extensionParam, resourceTypeParam, lookupTypeParam);  
    }
    
    public LookupReference(Reference.Id id, URI uri, String extensionParam, String resourceTypeParam, String lookupTypeParam) {
        super(id, uri, resourceTypeParam);
        this.uriExtension = extensionParam;
        this.lookupType = lookupTypeParam;
    }
    
    public LookupReference(String id, URI uri) {
        super(id, uri, null);
    }



}
