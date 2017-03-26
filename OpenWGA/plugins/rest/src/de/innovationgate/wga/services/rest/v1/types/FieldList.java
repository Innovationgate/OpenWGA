package de.innovationgate.wga.services.rest.v1.types;

import java.util.Iterator;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorNode;

import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field;

@XmlRootElement
@XmlDiscriminatorNode(value="@listType")
@XmlSeeAlso({JSONFieldList.class, XMLFieldList.class})
public abstract class FieldList {
    
    public abstract void addField(DocumentResource.Field field);
    
    public abstract Iterator<Field> getFields();

    public abstract FieldList merge(FieldList incomingItems);
    
    public abstract int size();

}
