package de.innovationgate.wga.services.rest.v1.types;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorNode;
import org.glassfish.jersey.uri.UriComponent;
import org.glassfish.jersey.uri.UriComponent.Type;

@XmlRootElement
@XmlDiscriminatorNode("@reftype")
@XmlSeeAlso({ReferenceCollection.class,ResourceReference.class,LookupReference.class})
public class Reference {
    
    public static class Id {
        
        public static Id create(String name) {
            return new Id(name);
        }
        
        private String _name;
        public String getName() {
            return _name;
        }

        public Map<String, String> getParams() {
            return _params;
        }

        private Map<String,String> _params = new HashMap<String, String>();
        
        public Id() {
        }
        
        
        public Id(String name) {
            _name = name;
        }
        
        public Id param(String name, String value) {
            _params.put(name, value);
            return this;
        }
        
        @Override
        public String toString() {
            
            if (_name == null) {
                return null;
            }

            StringBuilder str = new StringBuilder();
            str.append(_name);
            for (Map.Entry<String,String> param : _params.entrySet()) {
                str.append(";");
                str.append(UriComponent.encode(param.getKey(), Type.MATRIX_PARAM));
                str.append("=");
                str.append(UriComponent.encode(param.getValue(), Type.MATRIX_PARAM));
            }
            return str.toString();
            
        }
        
    }
    
    private Id _id;
    
    @XmlAttribute
    public URI href;

    @XmlAttribute
    public String resourceType;
    
    @XmlAttribute
    public Integer index;

    private String _idString;
    
    public Reference() {
    }
    
    public Reference(String idParam, URI uriParam, String resourceTypeParam) {
        this(new Id(idParam), uriParam, resourceTypeParam);
    }
    
    public Reference(Id idParam, URI uriParam, String resourceTypeParam) {
        setId(idParam);
        this.href = uriParam;
        this.resourceType = resourceTypeParam;
    }

    protected void setId(Id idParam) {
        _id = idParam;
        _idString = idParam.toString();
    }

    @XmlTransient
    public Id getId() {
        return _id;
    }
    
    @XmlAttribute(name="id")
    public String getIdString() {
        return _idString;
    }

}
