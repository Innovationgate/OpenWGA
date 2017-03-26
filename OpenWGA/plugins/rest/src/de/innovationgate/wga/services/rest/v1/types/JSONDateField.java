package de.innovationgate.wga.services.rest.v1.types;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.eclipse.persistence.oxm.annotations.XmlValueExtension;

@XmlRootElement(name="date")
@XmlType(propOrder={"date"})
public class JSONDateField extends JSONField<Date> {
    
    @XmlValue
    @XmlValueExtension
    public Date date;

    @Override
    public void addValues(List<?> v) {
        date = (Date) v.get(0);            
    }
    
    
    public List<?> getValues() {
        return Collections.singletonList(date);
    }
    
    @Override
    public boolean isMultiValue() {
        return false;
    }
}