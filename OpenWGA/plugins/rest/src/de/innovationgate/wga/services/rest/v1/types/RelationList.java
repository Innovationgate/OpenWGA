package de.innovationgate.wga.services.rest.v1.types;

import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorNode;

@XmlRootElement
@XmlDiscriminatorNode(value="@listType")
@XmlSeeAlso({XMLRelationList.class,JSONRelationList.class})
public abstract class RelationList {

    public abstract void addRelation(ResourceReference relation);
    
    public abstract List<ResourceReference> getRelations();

    public abstract RelationList merge(List<ResourceReference> incomingRelations);
    
}
