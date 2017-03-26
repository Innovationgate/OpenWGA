package de.innovationgate.wga.services.rest.v1.types;

import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.eclipse.persistence.oxm.annotations.XmlValueExtension;

@XmlRootElement(name="bool")
@XmlType(propOrder={"bool"})
public class JSONBooleanField extends JSONField<Boolean> {
    
    @XmlValue
    @XmlValueExtension
    public Boolean bool;

    @Override
    public void addValues(List<?> v) {
        bool = (Boolean) v.get(0);
    }
    
    public List<?> getValues() {
        return Collections.singletonList(bool);
    }
    
    @Override
    public boolean isMultiValue() {
        return false;
    }
}