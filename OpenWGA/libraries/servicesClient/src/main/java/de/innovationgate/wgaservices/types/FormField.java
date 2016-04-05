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

package de.innovationgate.wgaservices.types;

import java.util.List;
import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Represents a field on a {@link Form}
  */
public class FormField {

    private String _name;
    private List<Object> _values;
    
    
    
    /**
     * Default constructor.
     */
    public FormField() {
    }
    
    protected FormField(String name, List<Object> values) {
    	_name = name;
    	_values = values;
    }
    
    /**
     * Returns the name
     */
    public String getName() {
        return _name;
    }

    public void setName(String name) {
        _name = name;
    }
    
    /**
     * Returns the field values
     */
    public List<Object> getValues() {
        return _values;
    }

    public void setValues(List<Object> values) {
        _values = values;
    }

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		} else {
			return obj.hashCode() == hashCode();
		}
	}

	@Override
	public int hashCode() {
		HashCodeBuilder builder = new HashCodeBuilder();
		builder.append(_name);
		return builder.toHashCode();
	}
    
    

}
