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
package de.innovationgate.wgpublisher.webtml.form;

import java.security.NoSuchAlgorithmException;
import java.text.Format;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.utils.security.HashedPassword;
import de.innovationgate.utils.security.HashingException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGIllegalDataException;

/**
 * represent a tmlform field
 * contains information about the user entered field value and 
 * the parsed value by inputtag attribute type
 */
public class TMLFormField {
    
    private String _name;
    private List _parsedValues;
    private List _enteredValues;
    
    // field types to switch parsing
    public static final String TYPE_NUMBER = "number";    
    public static final String TYPE_TEXTAREA = "textarea";
    public static final String TYPE_TEXT = "text";
    public static final String TYPE_DATE = "date";
    public static final String TYPE_BOOLEAN = "boolean";
    public static final String TYPE_HASHEDPASSWORD = "hashedpassword";
    
    //field type
    private String _type = TYPE_TEXT;
    
    // parseFormat - transient to prevent from transport vi XStream and FormInfo
    private transient Format _format;
    
    private boolean _multiple;
    private String _multipleDivider;
    
    /**
     * no-arg constructor for XStream on JRockitVM
     * @param name
     */
    private TMLFormField() {
        _enteredValues = new ArrayList();
        _parsedValues= new ArrayList();
    }    
    
    public TMLFormField(String name) {
        _name = name;
        _enteredValues = new ArrayList();
        _parsedValues= new ArrayList();
    }
    
    public void addValue(Object value) {
        if (value instanceof List) {
             for (Object elem : (List<?>) value) {
                 addValue(elem);
             }
            
        }
        else {
            value = ensureValidValue(value);
            _enteredValues.add(value);
            _parsedValues.add(value);
        }
    }
    
    private Object ensureValidValue(Object value) {
        if (value instanceof String) {
            value = WGUtils.toValidXmlString((String) value);
        }
        return value;
    }
    
    public void setValue(Object value) {
        _enteredValues.clear();
        _parsedValues.clear();
        addValue(value);
    }
    
    public List getEnteredValues() {
        return new ArrayList(_enteredValues);
    }
    
    /**
     * returns all entered values as strings
     * if value is an object the corresponding string will be retrieved by 
     * formatting the object with the given format
     * if the object cannot be formatted the value will be an empty string
     * @return
     */
    public List getEnteredStringValues() {
        List stringValues = new ArrayList();
        Iterator it = _enteredValues.iterator();
        while (it.hasNext()) {
            Object value = it.next();
            if (value != null) {
                if (value instanceof String) {
                    stringValues.add(value);
                } else {
                    if (_type.equals(TYPE_NUMBER) || _type.equals(TYPE_DATE)) {                    
                        // convert value to string
                        if (_format != null) {
                            try {
                                stringValues.add(_format.format(value));
                            } catch (Exception e) {
                                stringValues.add("");
                            }
                        } else {
                            stringValues.add("");
                        }
                    } else if (_type.equals(TYPE_BOOLEAN)) {
                        if (value instanceof Boolean) {
                            stringValues.add(((Boolean)value).toString());
                        } else {
                            stringValues.add("");
                        }
                    } else {
                        stringValues.add(value.toString());
                    }
                }
            } else {
                stringValues.add("");
            }
        } 
        return stringValues;
    }
    
    /**
     * returns the first entered value as string
     * if value is an object the corresponding string will be retrieved by 
     * formatting the object with the given format
     * if the object cannot be formatted the value will be an empty string
     * if no value is entered an empty string is returned
     * @return
     */
    public String getFirstEnteredStringValue() {
        List values = this.getEnteredStringValues();
        if (values.size() > 0) {
            return (String) values.get(0);
        } else {
            return "";
        }
    }
    
    public Object getFirstEnteredValue() {
        if (_enteredValues.size() > 0) {
            return _enteredValues.get(0);
        } else {
            return null;
        }
    }
    
    public Object getFirstParsedValue() {
        if (_parsedValues.size() > 0) {
            return _parsedValues.get(0);
        } else {
            return null;
        }
    }    
    
    public String getName() {
        return _name;
    }
    public void setName(String name) {
        _name = name;
    }
    public List getParsedValues() {
        return new ArrayList(_parsedValues);
    }
    
    
    
    private void parse(Format format) throws WGException { 
        _parsedValues.clear();
        for (int i = 0; i < _enteredValues.size(); i++) {
            Object value = _enteredValues.get(i);
            if (value != null) {
                if (value instanceof String) {
                    // try to parse
                    if (!((String)value).trim().equals("")) {
                        try {
                            _parsedValues.add(format.parseObject((String)value));
                        }
                        catch (ParseException e) {
                            _parsedValues.add(null);
                            throw new TMLFormParsingException("Parse exception parsing field value with format", e);
                        }
                    }
                    else {
                        _parsedValues.add(null);
                    }
                }
                else {
                    // check if given format can format the value
                    try {
                        format.format(value);
                        // if value can be formatted - add it as parsed
                        _parsedValues.add(value);
                    }
                    catch (Exception e) {
                        _parsedValues.add(null);
                        throw new TMLFormParsingException("Exception parsing field value with format", e);
                    }
                }                
            }
            else {
                _parsedValues.add(null);
            }
        }       
    }
    
    
    public void parse(TMLForm form) throws WGException {
        if ( (_type.equals(TYPE_DATE)) || (_type.equals(TYPE_NUMBER)) ) {
            if (_format != null) {
                parse(_format);
            }
        } else if (_type.equals(TYPE_BOOLEAN)) {
            parseBoolean();            
        } else if (_type.equals(TYPE_HASHEDPASSWORD)) {
            parseHashedPassword(form);
        }
        else if (_type.equals(TYPE_TEXTAREA) && _multiple) {
            parseMultiple(_multipleDivider);
        }
    }
    
    private void parseBoolean() throws WGException {
        Object currentValue;    
        _parsedValues.clear();
        
        Iterator enteredValuesIt = _enteredValues.iterator();
        while (enteredValuesIt.hasNext()) {
            currentValue = enteredValuesIt.next();
            if (currentValue != null) {
                if (currentValue instanceof Boolean) {
                    _parsedValues.add(currentValue);
                } else if (currentValue instanceof String) {            
                    if (!((String)currentValue).trim().equals("")) {
                        _parsedValues.add(Boolean.valueOf((String)currentValue));
                    } else {
                        _parsedValues.add(null);
                    }
                } else {
                    throw new TMLFormParsingException("Cannot parse '" + currentValue.getClass().getName() + "' as Boolean.");
                }                
            } else {
                _parsedValues.add(null);
            }
        }        
    }
    
    private void parseMultiple(String divider) {
        // if value was submitted via HTML
        // we should have only one string
        // so we can divide the string by given divider
        if (_enteredValues.size() == 1) {
            String allValues = (String) _enteredValues.get(0);
            if (allValues == null) {
                allValues = "";
            }
            
            _enteredValues.clear();
            _parsedValues.clear();
            StringTokenizer valueTokens = new StringTokenizer( allValues, divider );
            while( valueTokens.hasMoreTokens() ) {
                String token = (valueTokens.nextToken()).trim();
                _enteredValues.add(token);
                _parsedValues.add(token);
            }
        } else {
            // this field was already parsed
        }
    }
    
    /**
     * trims entered values
     */   
    public void trim() {
        ArrayList trimmed = new ArrayList();
        List enteredValues = this.getEnteredValues();
        for (int i=0; i < enteredValues.size(); i++) {
            Object obj = enteredValues.get(i);
            if (obj instanceof String) {
            	String value = (String)obj;
	            if (value != null) {
	                value = value.trim();
	            }
	            trimmed.add(value);
            } else {
            	trimmed.add(obj);
            }
        }
        _enteredValues = trimmed;       
    }
    
    private void parseHashedPassword(TMLForm form) throws WGException {
        String currentValue;
        _parsedValues.clear();
        Iterator enteredValuesIt = _enteredValues.iterator();
        while (enteredValuesIt.hasNext()) {
            currentValue = (String) enteredValuesIt.next();
            if (currentValue != null) {
                if (!currentValue.trim().equals("")) {
                   _parsedValues.add(form.createPasswordHash(currentValue).toString());
                }
                else {
                    _parsedValues.add(null);
                }
            }
            else {
                _parsedValues.add(null);
            }
        }           
    }
    
    public void clear() {
        _enteredValues.clear();
        _parsedValues.clear();
    }

    public Format getFormat() {
        return _format;
    }

    public void setFormat(Format format) {
        _format = format;
    }

    public String getType() {
        return _type;
    }

    public void setType(String type) {
        _type = type;
    }

    public boolean isMultiple() {
        return _multiple;
    }

    public void setMultiple(boolean multiple) {
        _multiple = multiple;
    }

    public String getMultipleDivider() {
        return _multipleDivider;
    }

    public void setMultipleDivider(String multipleDivider) {
        _multipleDivider = multipleDivider;
    }
    
    
    public boolean couldBeParsed() {
        if (getParsedValues().size() == 0 || getParsedValues().contains(null)) {
            return false;
        } else {
            return true;
        }
    }
}
