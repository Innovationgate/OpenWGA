package de.innovationgate.wga.services.rest.v1.types;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field;

@XmlRootElement
@XmlDiscriminatorValue(value="xml")
public class XMLFieldList extends FieldList {
    
    @XmlElement(name="field")
    public List<DocumentResource.Field> fields = new ArrayList<DocumentResource.Field>();

    @Override
    public void addField(Field field) {
        fields.add(field);
    }

    @Override
    public Iterator<Field> getFields() {
        return fields.iterator();
    }
    
    @Override
    public FieldList merge(FieldList incomingItems) {
        XMLFieldList xmlList = (XMLFieldList) incomingItems;
        this.fields.addAll(xmlList.fields);
        return this;
    }
    
    @Override
    public int size() {
        return fields.size();
    }
    

}
