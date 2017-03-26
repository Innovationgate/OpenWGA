package de.innovationgate.wga.services.rest.v1.types;

import java.util.Date;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name="dates")
@XmlType(propOrder={"dates"})
public class JSONMultiDateField extends JSONField<Date> {
    
    @XmlElement(name="values")
    public List<Date> dates;

    @SuppressWarnings("unchecked")
    @Override
    public void addValues(List<?> v) {
        dates = (List<Date>) v;            
    }
    
    
    public List<?> getValues() {
        return dates;
    }
    
    @Override
    public boolean isMultiValue() {
        return true;
    }
}