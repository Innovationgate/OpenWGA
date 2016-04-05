/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.webgate.api.schemadef;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

/**
 * Predefinition of an item that is available on content documents of a content type 
 */
@Root(strict=false)
public class WGContentItemDefinition {
    
    /**
     * Content items data types
     */
    public enum Type {
        BOOLEAN(Boolean.class),
        TEXT(String.class),
        NUMBER(Double.class),
        DATE(Date.class);
        
        
        private Class<?> _clazz;
        private Type(Class<?> clazz) {
            _clazz = clazz;
        }
        
        public Class<?> getTypeClass() {
            return _clazz;
        }
        
    }
    
    @Attribute(name="name")
    private String _name;
    
    @Attribute(name="type")
    private Type _type;
    
    @Attribute(name="list")
    private boolean _list = false;
    
    @ElementList(name="initialValues", required=false)
    private List<Object> _initialValues = null;
    
    /**
     * Returns the name of the item
     */
    public String getName() {
        return _name;
    }
    
    /**
     * Sets the name of the item
     */
    public void setName(String name) {
        _name = name;
    }
    
    /**
     * Returns the data type of the defined content item 
     */
    public Type getType() {
        return _type;
    }
    /**
     * Sets the data type of the defined content item
     */
    public void setType(Type type) {
        _type = type;
    }
    
    /**
     * Returns the initial value(s) to set for the item when the content is created. Either a List of values or a single value, according to {@link #isList()}
     */
    public Object getInitialValue() {
        if (_initialValues != null) {
            if (isList()) {
                return _initialValues;
            }
            else if (_initialValues.size() > 0) {
                return _initialValues.get(0);
            }
        } 

        return null;

    }
    
    /**
     * Returns if an initial value is defined
     */
    public boolean hasInitialValue() {
        return _initialValues != null && !(!isList() && _initialValues.size() == 0);
    }
    
    /**
     * Sets the initial value for the item as a value list. USe when {@link #isList()} is true.
     */
    public void setInitialValueList(List<Object> initialValues) {
        _initialValues = initialValues;
    }
    
    /**
     * Sets the initial value for the item as a single value. Use when {@link #isList()} is false.
     */
    public void setInitialValue(Object value) {
        _initialValues = new ArrayList<Object>();
        _initialValues.add(value);
    }

    /**
     * Returns if the item should be a list item.
     */
    public boolean isList() {
        return _list;
    }

    /**
     * Sets if the item should be a list item.
     */
    public void setList(boolean list) {
        _list = list;
    }

}
