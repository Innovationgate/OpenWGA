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
import java.util.Collections;
import java.util.List;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.ElementList;
import org.simpleframework.xml.Root;

/**
 * Definition of a metadata field value for a schema document 
 */
@Root(strict=false)
public class WGMetaFieldDefinition {
    
    @Attribute(name="name")
    private String _name;
    
    @ElementList(name="values")
    private List<Object> _values;
    
    /**
     * Default constructor for serialisation
     */
    private WGMetaFieldDefinition() {
    }
    
    /**
     * Constructor taking metadata field name and value
     * @param name The name of the metadata field
     * @param value The value of the field. Use {@link List} for multivalue fields
     */
    public WGMetaFieldDefinition(String name, Object value) {
        _name = name;
        if (value instanceof List) {
            _values = (List) value;
        }
        else {
            _values = new ArrayList<Object>();
            _values.add(value);
        }
    }
    
    /**
     * Returns the name of the metadata field
     */
    public String getName() {
        return _name;
    }
    /**
     * Returns the value(s) of the metadata field as a list
     */
    public List<?> getValues() {
        return _values;
    }
    
    /**
     * Returns the value of the metadata field if it is a single value field
     */
    public Object getSingleValue() {
        if (_values.size() >= 1) {
            return _values.get(0);
        }
        else {
            return null;
        }
    }
    
    /**
     * Sets the value(s) of the metadata field. Use {@link List} when it is a multivalue field.
     */
    public void setValue(Object value) {
        if (value instanceof List) {
            _values = (List) value;
        }
        else {
            _values = new ArrayList<Object>();
            _values.add(value);
        }
    }
}
