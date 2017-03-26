package de.innovationgate.wga.services.rest.v1.types;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;
import org.eclipse.persistence.oxm.annotations.XmlVariableNode;

@XmlRootElement
@XmlDiscriminatorValue("json")
public class JSONRelationList extends RelationList {
    
    public static class JSONRelation {
        
        @XmlTransient
        public String key;
        
        @XmlAttribute
        public URI href;
        
        @XmlAttribute
        public Boolean delete;
        
        @XmlAttribute(name="protected")
        public Boolean protect;
        
        
    }

    @XmlVariableNode("key")
    public List<JSONRelation> relations = new ArrayList<JSONRelation>();

    @Override
    public void addRelation(ResourceReference ref) {
        JSONRelation rel = new JSONRelation();
        rel.key = ref.getIdString();
        rel.href = ref.href;
        rel.delete = ref.delete;
        rel.protect = ref.protect;

        relations.add(rel);
        
    }

    @Override
    public List<ResourceReference> getRelations() {

        List<ResourceReference> refs = new ArrayList<ResourceReference>();
        
        for (JSONRelation rel : relations) {
            
            if (rel.key.equals("listType")) {
                continue;
           }
            
            ResourceReference ref = new ResourceReference(rel.key, rel.href);
            ref.delete = rel.delete;
            ref.protect = rel.protect;
            refs.add(ref);
        }
        
        return refs;
        
    }
    
    @Override
    public RelationList merge(List<ResourceReference> incomingRelations) {
        return this;
    }

}
