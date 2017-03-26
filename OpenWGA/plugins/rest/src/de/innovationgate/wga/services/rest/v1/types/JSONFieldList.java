package de.innovationgate.wga.services.rest.v1.types;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlTransient;

import org.eclipse.persistence.oxm.annotations.XmlDiscriminatorValue;
import org.eclipse.persistence.oxm.annotations.XmlVariableNode;

import de.innovationgate.wga.services.rest.v1.resources.DocumentResource;
import de.innovationgate.wga.services.rest.v1.resources.DocumentResource.Field;


@XmlRootElement
@XmlDiscriminatorValue("json")
@XmlSeeAlso({JSONStringField.class, JSONDateField.class, JSONNumberField.class, JSONBooleanField.class, JSONMultiStringField.class,JSONMultiDateField.class,JSONMultiNumberField.class,JSONMultiBooleanField.class,JSONEmptyField.class})
public class JSONFieldList extends FieldList {
    
    @XmlRootElement
    public static class JSONFieldWrapper {
        
        @XmlTransient
        public String key;
        
        public Boolean delete;
        
        @XmlElementRefs({
            @XmlElementRef(type=JSONMultiBooleanField.class),
            @XmlElementRef(type=JSONMultiNumberField.class),
            @XmlElementRef(type=JSONMultiDateField.class),
            @XmlElementRef(type=JSONMultiStringField.class),
            @XmlElementRef(type=JSONStringField.class),
            @XmlElementRef(type=JSONNumberField.class),
            @XmlElementRef(type=JSONDateField.class),
            @XmlElementRef(type=JSONBooleanField.class)
        })
        public JSONField<?> field;
        
        public JSONFieldWrapper() {
        }

        public JSONFieldWrapper(String keyParam, JSONField<?> fieldParam) {
            this.key = keyParam;
            this.field = fieldParam;
        }
        
    }
    
    @XmlVariableNode("key")
    public List<JSONFieldWrapper> fields = new ArrayList<JSONFieldWrapper>();

    @Override
    public void addField(DocumentResource.Field field) {
        
        JSONField<?> f;
        
        Object determinationValue;
        if (field.values.size() == 0) {
            determinationValue = null;
        }
        else {
            determinationValue = field.values.get(0);
        }
        
        if (determinationValue == null) {
            f = new JSONEmptyField();
        }
        else if (determinationValue instanceof String) {
            if (field.multiValue) {
                f = new JSONMultiStringField();
            }
            else {
                f = new JSONStringField();
            }
               
        }
        else if (determinationValue instanceof Date) {
            if (field.multiValue) {
                f = new JSONMultiDateField();
            }
            else {
                f = new JSONDateField();
            }
        }
        else if (determinationValue instanceof Number) {
            if (field.multiValue) {
                f = new JSONMultiNumberField();
            }
            else {
                f = new JSONNumberField();
            }
        }
        else if (determinationValue instanceof Boolean) {
            if (field.multiValue) {
                f = new JSONMultiBooleanField();
            }
            else {
                f = new JSONBooleanField();
            }
        }
        else {
            return;
        }
            
        f.addValues((List<?>) field.values);
        fields.add(new JSONFieldWrapper(field.name, f));
        
    }

    @Override
    public Iterator<Field> getFields() {

        List<Field> out = new ArrayList<Field>();
        for (JSONFieldWrapper f : fields) {
            if (f.key.equals("listType")) {
                continue;
            }
            
            Field field = new Field();
            field.name = f.key;
            if (f.field != null) {
                field.values = f.field.getValues();
                field.multiValue = f.field.isMultiValue();
            }
            field.delete = f.delete;
            out.add(field);
        }
        return out.iterator();
        
        
    }
    
    @Override
    public FieldList merge(FieldList incomingItems) {

        JSONFieldList jsonList = (JSONFieldList) incomingItems;
        this.fields.addAll(jsonList.fields);
        return this;
        
    }

    @Override
    public int size() {
        return fields.size();
    }

}
