package de.innovationgate.wga.services.rest.v1.types;

import java.util.List;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;

@XmlRootElement
@XmlSeeAlso({JSONStringField.class,JSONNumberField.class,JSONDateField.class,JSONBooleanField.class,JSONEmptyField.class,JSONMultiBooleanField.class,JSONMultiStringField.class,JSONMultiDateField.class,JSONMultiNumberField.class})
public abstract class JSONField<FieldType>  {
    public abstract void addValues(List<?> v);
    public abstract List<?> getValues();
    public abstract boolean isMultiValue();
}