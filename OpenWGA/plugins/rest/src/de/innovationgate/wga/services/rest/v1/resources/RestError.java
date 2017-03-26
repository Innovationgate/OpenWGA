package de.innovationgate.wga.services.rest.v1.resources;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class RestError {
    
    @XmlAttribute
    public int code;
    
    @XmlElement
    public String message;
    
    @XmlAttribute
    public String type;
    
    public List<String> causes = new ArrayList<String>();
    


}
