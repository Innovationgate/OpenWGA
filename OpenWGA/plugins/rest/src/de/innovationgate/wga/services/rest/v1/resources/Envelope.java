package de.innovationgate.wga.services.rest.v1.resources;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.eclipse.persistence.oxm.annotations.XmlVariableNode;

import de.innovationgate.wga.services.rest.v1.types.DocumentKey;
import de.innovationgate.wga.services.rest.v1.types.Reference;
import de.innovationgate.wga.services.rest.v1.types.Status;

@XmlRootElement
@XmlType(propOrder={"status","error","self","parentResource","key","resource", "refs","timestamps"})
public class Envelope {
    
    @XmlAttribute
    public Status status = Status.SUCCESS;
    
    @XmlAttribute
    public URI self;
    
    @XmlElement
    public URI parentResource;
    
    @XmlElement
    @XmlElementWrapper(name="refs")
    @XmlVariableNode("idString")
    public List<Reference> refs;
    
    @XmlElement
    public DocumentKey key;
    
    @XmlElement
    public Timestamps timestamps;
    
    @XmlElement
    public Resource<?> resource;
    
    @XmlElement
    public RestError error;
    
    public void addReference(Reference ref) {
        
        if (this.refs == null) {
            this.refs = new ArrayList<Reference>();
        }
        
        refs.add(ref);
        
    }
    
}
