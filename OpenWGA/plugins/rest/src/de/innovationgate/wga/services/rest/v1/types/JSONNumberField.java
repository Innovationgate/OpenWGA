package de.innovationgate.wga.services.rest.v1.types;

import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.eclipse.persistence.oxm.annotations.XmlValueExtension;

@XmlRootElement(name="number")
@XmlType(propOrder={"number"})
public class JSONNumberField extends JSONField<Number> {
    
    @XmlValue
    @XmlValueExtension
    public Double number;

    @Override
    public void addValues(List<?> v) {
        number = ((Number) v.get(0)).doubleValue();            
    }
    
    
    public List<?> getValues() {
        return Collections.singletonList(number);
    }
    
    @Override
    public boolean isMultiValue() {
        return false;
    }

}