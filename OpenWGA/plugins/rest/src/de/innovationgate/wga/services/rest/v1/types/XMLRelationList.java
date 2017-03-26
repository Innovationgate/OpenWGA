package de.innovationgate.wga.services.rest.v1.types;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

@XmlRootElement
@XmlDiscriminatorValue("xml")
public class XMLRelationList extends RelationList {
    
    @XmlElement(name="relation")
    public List<ResourceReference> relations = new ArrayList<ResourceReference>();

    @Override
    public void addRelation(ResourceReference relation) {
        relations.add(relation);
    }

    @Override
    public List<ResourceReference> getRelations() {
        return relations;
    }
    
    @Override
    public RelationList merge(List<ResourceReference> incomingRelations) {
        XMLRelationList xmlList = (XMLRelationList) incomingRelations;
        this.relations.addAll(xmlList.relations);
        return this;
        
    }
    
}
