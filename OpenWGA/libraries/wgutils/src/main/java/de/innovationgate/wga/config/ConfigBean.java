/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wga.config;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.core.Validate;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.model.ValidationError;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.CachingOptionReader;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionDefinition;
import de.innovationgate.wga.modules.options.OptionReader;

/**
 * Base class for all configuration beans, defining shared functionality
 */
public abstract class ConfigBean implements Serializable {

    public static final long serialVersionUID = 1L;

    private transient boolean _changed = false;
    
    @Attribute(required = false)
    @NormalizeEmptyValue
    private String description;
    
    public final List<ValidationError> validate() {
        return validate(false);
    }

    public final List<ValidationError> validate(boolean integrityCheckOnly) {
        List<ValidationError> errors = new ArrayList<ValidationError>();
        validate(errors, integrityCheckOnly);
        return errors;
    }

    protected void validate(List<ValidationError> errors, boolean integrityCheckOnly) {

        // validate this bean based upon constraint annotations
        Map<Field, Set<Annotation>> fieldConstraints = getConstraints(this.getClass());
        Iterator<Field> fields = fieldConstraints.keySet().iterator();
        while (fields.hasNext()) {
            Field field = fields.next();
            Iterator<Annotation> constraints = fieldConstraints.get(field).iterator();
            while (constraints.hasNext()) {
                Annotation constraint = constraints.next();
                handleConstraint(errors, field, constraint);
            }
        }

        // cascade validation to referenced configbeans
        PropertyDescriptor[] descs;
        try {
            descs = Introspector.getBeanInfo(this.getClass()).getPropertyDescriptors();
            for (PropertyDescriptor desc : descs) {
                if (ConfigBean.class.isAssignableFrom(desc.getPropertyType())) {
                    try {
                        Method readMethod = desc.getReadMethod();
                        if (readMethod != null) {
                            if (readMethod.getAnnotation(SkipValidation.class) == null) {
                                Object value = readMethod.invoke(this, new Object[0]);
                                if (value != null) {
                                    ((ConfigBean) value).validate(errors, integrityCheckOnly);
                                }
                            }
                        }
                        else {
                            errors.add(new ValidationError("Unable to validate field '" + desc.getName() + "' of '" + this.getClass().getName() + "' bc. it is not accessible via getMethod.",
                                    new String[] { desc.getName() }));
                        }
                    }
                    catch (Exception e) {
                        errors.add(new ValidationError("Unable to validate field '" + desc.getName() + "' of '" + this.getClass().getName() + "'.", new String[] { desc.getName() }));
                    }
                }
                else if (List.class.isAssignableFrom(desc.getPropertyType())) {
                    try {
                        Method readMethod = desc.getReadMethod();
                        if (readMethod != null) {
                            if (readMethod.getAnnotation(SkipValidation.class) == null) {
                                Object value = readMethod.invoke(this, new Object[0]);
                                if (value != null) {
                                    Iterator it = ((List) value).iterator();
                                    Type[] typeArguments = retrieveGenericsOfReturnType(readMethod);
                                    while (it.hasNext()) {
                                        Object listValue = it.next();

                                        // check list value against generics
                                        if (listValue != null) {
                                            if (typeArguments != null && typeArguments.length > 0) {
                                                Class typeArgClass = (Class) typeArguments[0];
                                                if (!typeArgClass.isAssignableFrom(listValue.getClass())) {
                                                    errors
                                                            .add(new ValidationError("Unsupported type '" + listValue.getClass() + "' in list '" + desc.getName() + "'.",
                                                                    new String[] { desc.getName() }));
                                                }
                                            }
                                        }

                                        if (listValue instanceof ConfigBean) {
                                            ((ConfigBean) listValue).validate(errors, integrityCheckOnly);
                                        }
                                    }
                                }
                            }
                        }
                        else {
                            errors.add(new ValidationError("Unable to validate field '" + desc.getName() + "' of '" + this.getClass().getName() + "' bc. it is not accessible via getMethod.",
                                    new String[] { desc.getName() }));
                        }
                    }
                    catch (Exception e) {
                        errors.add(new ValidationError("Unable to validate field '" + desc.getName() + "' of '" + this.getClass().getName() + "'.", new String[] { desc.getName() }));
                    }
                }
                else if (Map.class.isAssignableFrom(desc.getPropertyType())) {
                    try {
                        Method readMethod = desc.getReadMethod();
                        if (readMethod != null) {
                            if (readMethod.getAnnotation(SkipValidation.class) == null) {
                                Object value = readMethod.invoke(this, new Object[0]);
                                if (value != null) {

                                    Type[] typeArguments = retrieveGenericsOfReturnType(readMethod);
                                    Iterator it = ((Map) value).keySet().iterator();
                                    while (it.hasNext()) {
                                        Object mapKey = it.next();
                                        Object mapValue = ((Map) value).get(mapKey);

                                        // check mapKey against generics
                                        if (mapKey != null) {
                                            if (typeArguments != null && typeArguments.length > 0) {
                                                Class typeArgClass = (Class) typeArguments[0];
                                                if (!typeArgClass.isAssignableFrom(mapKey.getClass())) {
                                                    errors
                                                            .add(new ValidationError("Unsupported keytype '" + mapKey.getClass() + "' in map '" + desc.getName() + "'.",
                                                                    new String[] { desc.getName() }));
                                                }
                                            }
                                        }

                                        // check mapValue against generics
                                        if (mapValue != null) {
                                            if (typeArguments != null && typeArguments.length > 1) {
                                                Class typeArgClass = (Class) typeArguments[1];
                                                if (!typeArgClass.isAssignableFrom(mapValue.getClass())) {
                                                    errors.add(new ValidationError("Unsupported value '" + mapValue.getClass() + "' in map '" + desc.getName() + "' for key '" + mapKey + "'.",
                                                            new String[] { desc.getName() }));
                                                }
                                            }
                                        }

                                        if (mapValue instanceof ConfigBean) {
                                            ((ConfigBean) mapValue).validate(errors, integrityCheckOnly);
                                        }
                                    }
                                }
                            }
                        }
                        else {
                            errors.add(new ValidationError("Unable to validate field '" + desc.getName() + "' of '" + this.getClass().getName() + "' bc. it is not accessible via getMethod.",
                                    new String[] { desc.getName() }));
                        }
                    }
                    catch (Exception e) {
                        errors.add(new ValidationError("Unable to validate field '" + desc.getName() + "' of '" + this.getClass().getName() + "'.", new String[] { desc.getName() }));
                    }
                }
            }
        }
        catch (IntrospectionException e) {
        }
    }

    private Type[] retrieveGenericsOfReturnType(Method method) {
        Type returnType = method.getGenericReturnType();
        if (returnType instanceof ParameterizedType) {
            ParameterizedType type = (ParameterizedType) returnType;
            Type[] typeArguments = type.getActualTypeArguments();
            return typeArguments;
        }
        else {
            return null;
        }
    }

    private void handleConstraint(List<ValidationError> errors, Field field, Annotation annotation) {
        if (annotation.annotationType().equals(NotNull.class)) {
            try {
                PropertyDescriptor[] descs = Introspector.getBeanInfo(this.getClass()).getPropertyDescriptors();
                for (PropertyDescriptor desc : descs) {
                    if (field.getName().equals(desc.getName())) {
                        Method readMethod = desc.getReadMethod();
                        if (readMethod != null) {
                            Object value = readMethod.invoke(this, new Object[0]);
                            if (normalize(value) == null) {
                                errors.add(new ValidationError("Field '" + field.getName() + "' of '" + this.getClass().getName() + "' cannot be 'null'.", new String[] { field.getName() }));
                            }
                        }
                        else {
                            errors.add(new ValidationError("Unable to validate field '" + field.getName() + "' of '" + this.getClass().getName() + "' bc. it is not accessible via getMethod.",
                                    new String[] { field.getName() }));
                        }
                    }
                }
            }
            catch (Exception e) {
                errors.add(new ValidationError("Unable to validate field '" + field.getName() + "' of '" + this.getClass().getName() + "' " + e.getMessage() + ".", new String[] { field.getName() }));
            }
        }
        else if (annotation.annotationType().equals(NormalizeEmptyValue.class)) {
            try {
                PropertyDescriptor[] descs = Introspector.getBeanInfo(this.getClass()).getPropertyDescriptors();
                for (PropertyDescriptor desc : descs) {
                    if (field.getName().equals(desc.getName())) {
                        Method readMethod = desc.getReadMethod();
                        Method writeMethod = desc.getWriteMethod();
                        if (readMethod != null && writeMethod != null) {
                            Object value = readMethod.invoke(this, new Object[0]);
                            value = normalize(value);
                            writeMethod.invoke(this, value);
                        }
                        else {
                            errors.add(new ValidationError("Unable to normalize field '" + field.getName() + "' of '" + this.getClass().getName() + "' bc. setter or getter is not accessible.",
                                    new String[] { field.getName() }));
                        }
                    }
                }
            }
            catch (Exception e) {
                errors.add(new ValidationError("Unable to normalize field '" + field.getName() + "' of '" + this.getClass().getName() + "' " + e.getMessage() + ".", new String[] { field.getName() }));
            }
        }
    }

    private Map<Field, Set<Annotation>> getConstraints(Class<?> theClass) {
        Map<Field, Set<Annotation>> annotationsMap = new HashMap<Field, Set<Annotation>>();
        List<Field> fieldList = new ArrayList<Field>();

        Field[] fields = theClass.getDeclaredFields();
        fieldList.addAll(Arrays.asList(fields));

        Class<?> superClass = theClass.getSuperclass();
        while (superClass != null) {
            Field[] superFields = superClass.getDeclaredFields();
            fieldList.addAll(Arrays.asList(superFields));
            superClass = superClass.getSuperclass();
        }

        Iterator<Field> it = fieldList.iterator();
        while (it.hasNext()) {
            Field field = it.next();
            Annotation[] annotations = field.getAnnotations();
            for (Annotation annotation : annotations) {
                if (isConstraint(annotation)) {
                    Set<Annotation> constraints = annotationsMap.get(field);
                    if (constraints == null) {
                        constraints = new HashSet<Annotation>();
                        annotationsMap.put(field, constraints);
                    }
                    constraints.add(annotation);
                }
            }
        }
        return annotationsMap;
    }

    private static boolean isConstraint(Annotation annotation) {
        if (annotation == null)
            throw new IllegalArgumentException("null is not a legal value for annotation");
        return annotation.annotationType().isAnnotationPresent(Constraint.class);
    }

    private static final Object normalize(Object value) {
        if (value instanceof String) {
            if (value != null && !((String) value).trim().equals("")) {
                return value;
            }
            else {
                return null;
            }
        }
        else if (value instanceof Map && value != null) {
            Map map = (Map) value;
            Iterator<Object> keys = map.keySet().iterator();
            while (keys.hasNext()) {
                Object mapValue = map.get(keys.next());
                if (mapValue == null) {
                    keys.remove();
                }
            }
            return value;
        }
        else if (value instanceof Collection && value != null) {
            Collection col = (Collection) value;
            Iterator<Object> values = col.iterator();
            while (values.hasNext()) {
                Object singleValue = values.next();
                if (singleValue == null) {
                    values.remove();
                }
            }
            return value;
        }
        else {
            return value;
        }
    }

    public boolean isDefaultResource() {
        return WGAConfiguration.isDefaultResource(this);
    }

    public boolean isChanged() {
        return _changed;
    }

    public void setChanged(boolean changed) {
        _changed = changed;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public void visit(ConfigBeanVisitor visitor) throws Exception {

        visitor.visit(this);

        PropertyDescriptor[] descs;

        descs = Introspector.getBeanInfo(this.getClass()).getPropertyDescriptors();
        for (PropertyDescriptor desc : descs) {

            if (ConfigBean.class.isAssignableFrom(desc.getPropertyType())) {
                Method readMethod = desc.getReadMethod();
                if (readMethod != null) {
                    Object value = readMethod.invoke(this, new Object[0]);
                    if (value != null) {
                        ((ConfigBean) value).visit(visitor);
                    }
                }

            }

            else if (Collection.class.isAssignableFrom(desc.getPropertyType())) {
                Method readMethod = desc.getReadMethod();
                if (readMethod != null) {
                    Object value = readMethod.invoke(this, new Object[0]);
                    if (value != null) {
                        Iterator it = ((Collection) value).iterator();
                        while (it.hasNext()) {
                            Object listValue = it.next();
                            if (listValue instanceof ConfigBean) {
                                ((ConfigBean) listValue).visit(visitor);
                            }
                        }
                    }
                }
            }

            else if (Map.class.isAssignableFrom(desc.getPropertyType())) {
                Method readMethod = desc.getReadMethod();
                if (readMethod != null) {
                    Object value = readMethod.invoke(this, new Object[0]);
                    if (value != null) {
                        Iterator it = ((Map) value).values().iterator();
                        while (it.hasNext()) {
                            Object mapValue = it.next();
                            if (mapValue instanceof ConfigBean) {
                                ((ConfigBean) mapValue).visit(visitor);
                            }
                        }
                    }
                }
            }
        }
    }
    

    
    /**
     * Should retrieve the module definitions for the option stored as the given property.
     * Override this method to let utilizing functionalities (like option sorting) be able to automatically retrieve definitions for options.
     * @param registry The module registry
     * @param property Descriptor of the property holding options
     * @param config The current OpenWGA configuration object
     * @return A list of module definitions that define options for this option property
     */
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        return Collections.emptyList();
    }
    
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, String propName, WGAConfiguration config) throws IntrospectionException {
        PropertyDescriptor desc = new PropertyDescriptor(propName, getClass());
        return getOptionDefinitions(registry, desc, config);
    }


}
