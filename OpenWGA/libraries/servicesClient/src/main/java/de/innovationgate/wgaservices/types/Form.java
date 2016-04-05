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

import java.io.File;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

/**
 * A lightweight object representing a TMLForm that is to be sent to the WGA server when calling a remote action.
 * The form can be equipped with fields and file attachments.
 * When posted to the server it is available to the remote action as normal TMLForm-Object in TMLScript
 * Only plain field values (String, Number, Date) can be posted.
 */
public class Form {
    
    //private Map<String, FormField> _fields = new HashMap<String, FormField>();
	
	Set<FormField> _fields = new HashSet<FormField>();
	
    private Map<String, Object> _attachments = new HashMap<String, Object>();
    private String _id;
    private boolean _trim = false;
    
    /**
     * Creates a form with the given id.
     * @param id The form id, just like the id-attribute on &lt;tml:form&gt;
     */
    public Form(String id) {
        _id = id;
    }
    
    /**
     * Creates a form without id
     */
    public Form() {
    }
    
    /**
     * Returns the set that contains the stored fields.
     */
    public Set<FormField> getFields() {
        return _fields;
    }

    /**
     * JavaBean setter for fields table. Do not use directly, instead use {@link #setField(String, List)} and variants.
     * @param fields
     */
    public void setFields(Set<FormField> fields) {        
        _fields = fields;        
    }
    
    
    /**
     * Sets a field with a string value
     * @param name Name of the field
     * @param value Value of the field
     */
    public void setField(String name, String value) {
        List<Object> values = new ArrayList<Object>();
        values.add(value);
        _fields.add(new FormField(name, values));
    }
    
    /**
     * Sets a field with multiple values
     * @param name Name of the field
     * @param values The values as List
     */
    public void setField(String name, List<Object> values) {
        _fields.add(new FormField(name,values));
    }
    
    /**
     * Sets a field with a number value
     * @param name The name of the field
     * @param value The number value
     */
    public void setField(String name, Number value) {
    	Vector<Object> values = new Vector<Object>();
        values.add(value);
        _fields.add(new FormField(name, values));
    }
    
    /**
     * Sets a field with a date value
     * @param name The name of the field
     * @param value The date value
     */
    public void setField(String name, Date value) {
    	Vector<Object> values = new Vector<Object>();
        values.add(value);
        _fields.add(new FormField(name, values));
    }
    
    /**
     * Removes a field from the form
     * @param name The name of the field
     */
    public void removeField(String name) {
        _fields.remove(name);
    }
    
    /**
     * Returns the names of all stored fields
     */
    public Set<String> fieldNames() {
    	Set<String> names = new HashSet<String>();
    	Iterator<FormField> fields = _fields.iterator();
    	while (fields.hasNext()) {
    		names.add(fields.next().getName());
    	}
        return names;
    }
        
    /**
     * Adds a file from file system to the form as attachment, leaving you the choice to specify the attachment file name.
     * @param file The file to attach
     * @param name The file name of the attachment at the form
     */
    public void addFileAsAttachment(File file, String name) {
        _attachments.put(name, new FileDataSource(file));
        
    }
    
    /**
     * Adds a file from file system to the form as attachment.
     * @param file The file to attach
     */
    public void addFileAsAttachment(File file) {
        addFileAsAttachment(file, file.getName());
    }
    
    /**
     * Returns the names of all attachments stored to the form
     */
    public Set<String> attachmentNames() {
        return _attachments.keySet();
    }
    
    /**
     * Returns a map containing all attachments at the form.
     * The attachment file names are keys and the attachments themselves form the values of type
     */
    public Map<String,Object> getAttachments() {
        return _attachments;
    }

    /**
     * JavaBean setter for attachments table. Do not use directly. Instead use {@link #addFileAsAttachment(File)}.
     * @param attachments
     */
    public void setAttachments(Map<String,Object> attachments) {
        _attachments = attachments;
    }
    
    /**
     * Returns the data of an attachment file as DataSource
     * @param name The name of the attachment file
     */
    public DataSource attachmentData(String name) {
    	Object data = _attachments.get(name);
    	if (data != null) {
	        if (data instanceof DataHandler) {
	        	return ((DataHandler)data).getDataSource();
	        } else if (data instanceof DataSource) {
	        	return (DataSource)data;
	        } else {
	        	throw new IllegalArgumentException("Unsupported data type '" + data.getClass().getName() + "'.");
	        }
    	} else {
    		return null;
    	}
    }

    /**
     * Returns the id of the form
     */
    public String getId() {
        return _id;
    }

    /**
     * Sets the id of the form, just like attribute "id" at &lt;tml:form&gt;
     */
    public void setId(String id) {
        _id = id;
    }

    /**
     * Returns if this form is to be trimmed
     */
    public boolean isTrim() {
        return _trim;
    }

    /**
     * Sets if this form is to be trimmed, like attribute "trim" for &lt;tml:form&gt;
     */
    public void setTrim(boolean trim) {
        _trim = trim;
    }
    
    /**
     * Returns the values stored for a field
     * @param name The field name
     * @return The values as list
     */
    public List<Object> fieldValues(String name) {
    	FormField field = getField(name);
    	if (field != null) {
    		return field.getValues();
    	} else {
    		return null;
    	}
    }
    
    /**
     * Returns if a field is stored at the form
     * @param name The name of the field
     */
    public boolean hasField(String name) {
        return getField(name) != null;
    }    
    
    public FormField getField(String name) {
    	Iterator<FormField> fields = _fields.iterator();
    	while (fields.hasNext()) {
    		FormField field = fields.next();
    		if (field.getName().equals(name)) {
    			return field;
    		}
    	}
    	return null;
    }
}
