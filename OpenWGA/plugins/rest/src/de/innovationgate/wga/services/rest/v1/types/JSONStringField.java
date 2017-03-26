package de.innovationgate.wga.services.rest.v1.types;

import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.eclipse.persistence.oxm.annotations.XmlValueExtension;

@XmlRootElement(name="string")
@XmlType(propOrder={"string"})
public class JSONStringField extends JSONField<String> {
    
    @XmlValue
    @XmlValueExtension
    public String string;

    @Override
    public void addValues(List<?> v) {
        string = (String) v.get(0);
    }
    
    
    public List<?> getValues() {
        return Collections.singletonList(string);
    }
    
    @Override
    public boolean isMultiValue() {
        return false;
    }
    
}