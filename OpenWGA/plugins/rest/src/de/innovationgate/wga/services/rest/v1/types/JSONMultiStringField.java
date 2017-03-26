package de.innovationgate.wga.services.rest.v1.types;

import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name="strings")
public class JSONMultiStringField extends JSONField<String> {
    
    @XmlElement(name="values")
    public List<String> strings;

    @SuppressWarnings("unchecked")
    @Override
    public void addValues(List<?> v) {
        strings = (List<String>) v;
    }
    
    
    public List<?> getValues() {
        return strings;
    }
    
    @Override
    public boolean isMultiValue() {
        return true;
    }
    
}