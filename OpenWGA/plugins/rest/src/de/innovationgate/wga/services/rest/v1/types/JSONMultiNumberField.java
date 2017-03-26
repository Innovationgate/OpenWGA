package de.innovationgate.wga.services.rest.v1.types;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name="numbers")
@XmlType(propOrder={"numbers"})
public class JSONMultiNumberField extends JSONField<Number> {
    
    @XmlElement(name="values")
    public List<Number> numbers;

    @SuppressWarnings("unchecked")
    @Override
    public void addValues(List<?> v) {
        this.numbers = (List<Number>) v;            
    }
    
    
    public List<?> getValues() {
        
        
        if (this.numbers == null) {
            return null;
        }
        
        List<Double> doubles = new ArrayList<Double>();
        for (Number n : this.numbers) {
            doubles.add(n.doubleValue());
        }
        
        return doubles;
    }
    
    @Override
    public boolean isMultiValue() {
        return true;
    }

}