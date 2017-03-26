package de.innovationgate.wga.services.rest.v1.types;

import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.eclipse.persistence.oxm.annotations.XmlValueExtension;

@XmlRootElement
@XmlType(propOrder={"nullValue"})
public class JSONEmptyField extends JSONField<String> {
    
    @XmlValue
    @XmlValueExtension
    public Object nullValue = null;

    @Override
    public void addValues(List<?> v) {
    }
    
    
    public List<?> getValues() {
        return null;
    }
    
    @Override
    public boolean isMultiValue() {
        return false;
    }
    
}