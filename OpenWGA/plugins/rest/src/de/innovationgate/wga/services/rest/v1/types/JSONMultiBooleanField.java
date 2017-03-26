package de.innovationgate.wga.services.rest.v1.types;

import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name="bools")
@XmlType(propOrder={"bools"})
public class JSONMultiBooleanField extends JSONField<Boolean> {
    
    @XmlElement(name="values")
    public List<Boolean> bools;

    @SuppressWarnings("unchecked")
    @Override
    public void addValues(List<?> v) {
        bools = (List<Boolean>) v;
    }
    
    
    public List<?> getValues() {
        return bools;
    }
    
    @Override
    public boolean isMultiValue() {
        return true;
    }
    
}