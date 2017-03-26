package de.innovationgate.wga.services.rest.v1.types;

import java.net.URI;
import java.net.URISyntaxException;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;

import de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field;
import de.innovationgate.wga.services.rest.v1.resources.RESTEntry;

@XmlRootElement
@XmlDiscriminatorValue("resource")
public class ResourceReference extends Reference implements RESTEntry {
    
    @XmlElement
    public FieldList metaData;

    @XmlElement
    public FieldList items;
    
    @XmlElement
    public RelationList relations;
    
    @XmlAttribute
    public Boolean delete;
    
    @XmlAttribute(name="protected")
    public Boolean protect;
    
    public String hdbmodelContentId;
    
    public ResourceReference() {
    }
    
    public ResourceReference(String id, URI uri, String resourceTypeParam) {
        super(id, uri, resourceTypeParam);
    }
    
    public ResourceReference(String id, URI uri) {
        super(id, uri, null);
    }
    
    public void addItem(String name, Object value) {
        Field field = Field.create(name, value);
        items.addField(field);
    }
    
    public void addMetaData(String name, Object value) {
        Field field = Field.create(name, value);
        metaData.addField(field);
    }
    
    public void addRelation(String name, String uriStr) throws URISyntaxException {
        ResourceReference relation = new ResourceReference(name, new URI(uriStr));
        relations.addRelation(relation);
    }

}
