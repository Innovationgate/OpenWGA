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

package de.innovationgate.wga.modules.options;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

import de.innovationgate.wga.config.ConfigBean;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.OptionDefinitionsMap;

/**
 * Tool class to read/write options from an options map to directly use them in application code.
 * Therefor the methods of this class use the appropriate option definitions to:
 * <ul>
 * <li>Directly convert/unconvert the option value to it's desired datatype given by {@link OptionType#getDataTypeHint()}
 * <li>Directly do custom conversion/unconversion if the option type is a {@link ConvertingOptionType}
 * <li>Return the default value of an option if that option is "optional" and not available in the options map
 */
public class OptionReader {
    
    public static final DateFormat DATE_FORMAT = new SimpleDateFormat("yyyyMMdd-HHmmssSSS-Z");
    
    /**
     * Convert a string option to the data type declared by the option type
     * @param optionType The option type whose {@link OptionType#getDataTypeHint()} will be used as target type
     * @param stringValue The string value of the option
     * @return The option value converted to the right data type
     * @throws OptionConversionException
     */
    public static Object toDataType(OptionType optionType, String stringValue) throws OptionConversionException {
        
        Class hintType = optionType.getDataTypeHint();   
        
        if (Integer.class.isAssignableFrom(hintType)) {
            try {
                return Integer.parseInt(stringValue);
            }
            catch (NumberFormatException e) {
                throw new OptionConversionException("Option value not parseable as integer: " + stringValue);
            }
        }
        else if (Double.class.isAssignableFrom(hintType)) {
            try {
                return Double.parseDouble(stringValue);
            }
            catch (NumberFormatException e) {
                throw new OptionConversionException("Option value not parseable as double float: " + stringValue);
            }
        }
        else if (Boolean.class.isAssignableFrom(hintType)) {
            try {
                return Boolean.parseBoolean(stringValue);
            }
            catch (NumberFormatException e) {
                throw new OptionConversionException("Option value not parseable as boolean: " + stringValue);
            }
        }
        else if (Date.class.isAssignableFrom(hintType)) {
            try {
                return DATE_FORMAT.parse(stringValue);
            }
            catch (ParseException e) {
                throw new OptionConversionException("Option value not parseable as date: " + stringValue);
            }
            
        }
        
        return stringValue;
        
    }
    
    /**
     * Converts an option value in its native data type to it's string representation
     * @param obj The option value in the data type given by {@link OptionType#getDataTypeHint()}
     */
    public static String fromDataType(OptionType optionType, Object obj) {
        
        Class hintType = optionType.getDataTypeHint();
        
        if (obj instanceof Date) {
            return DATE_FORMAT.format((Date) obj);
        }
        else if (obj instanceof Number){
            
            if (hintType == Integer.class) {
                Integer integer = new java.lang.Integer(((Number) obj).intValue());
                return integer.toString();
            }
            else {
                return obj.toString();
            }
            
        }
        else {
            return obj.toString();
        }
        
    }
    
    /**
     * Create a new Option reader for the given options map and definitions object
     * @param options The options map
     * @param modDef The module definition whose option definitions are used to read/write options
     */
    public static OptionReader create(Map<String,String> options, ModuleDefinition modDef) {
        return new OptionReader(options, modDef);
    }

    private Map<String, String> _options;
    private ModuleDefinition _moduleDefinition;
    
    protected OptionReader(Map<String,String> options, ModuleDefinition modDef) {
        _options = options;
        _moduleDefinition = modDef;
    }
    
    
    /**
     * Reads an option value and directly returns the native datatype, or the default value if the option is not available
     * @param optionName Then name of the option
     * @return Correctly typed option value or default value
     * @throws ClassNotFoundException
     * @throws OptionConversionException
     */
    public Object readOptionValueOrDefault(String optionName) throws OptionConversionException {
        
        // Get option definition map if available
        OptionDefinition optDef = null;
        if (_moduleDefinition != null) {
            OptionDefinitionsMap optDefMap = _moduleDefinition.getOptionDefinitions();
            if (optDefMap != null) {
                optDef = optDefMap.get(optionName);
            }
        }
        
        // If we have no option definition we can bypass all the fuzz and do a simple get
        if (optDef == null) {
            return _options.get(optionName);
        }
        
        // Read the option value, or - if unavailable - the default value
        Object value = null;
        if (_options.containsKey(optionName)) {
            value = _options.get(optionName);
        }
        else {
            value = optDef.getDefaultValue();
        }
            
        // Convert the value if neccessary
        value = unconvertOptionValue(optDef, value);
            
        return value;
        
    }

    /**
     * Unconverts an option value to the native data type, based on the given option definition
     * @param optDef The option definition
     * @param value The value
     * @return The option value in native data type
     * @throws OptionConversionException
     */
    public static Object unconvertOptionValue(OptionDefinition optDef, Object value) throws OptionConversionException {
        if (value != null) {
            if (optDef.getOptionType() instanceof ConvertingOptionType) {
                ConvertingOptionType oType = (ConvertingOptionType) optDef.getOptionType();
                value =  oType.unconvert((String) value);
            }
        }
        return value;
    }
    
    /**
     * Writes a native datatype option value
     * @param optionName The name of the option to write
     * @param optionValue The value to write in its native data type, like given by {@link OptionType#getDataTypeHint()}
     * @throws OptionConversionException
     */
    public void writeOptionValue(String optionName, Object optionValue) throws OptionConversionException {

        if (optionValue == null) {
            return;
        }
        
        String stringValue = null;
        if (_moduleDefinition != null) {
            OptionDefinition def = _moduleDefinition.getOptionDefinitions().get(optionName);
            if (def != null && def.getOptionType() instanceof ConvertingOptionType) {
                ConvertingOptionType oType = (ConvertingOptionType) def.getOptionType();
                stringValue =  oType.convert(optionValue);
            }
            else {
                stringValue = String.valueOf(optionValue);
            }
        }
        else {
             stringValue = String.valueOf(optionValue);
        }
        
        _options.put(optionName, stringValue);
        
    }

    public Map<String, String> getOptions() {
        return _options;
    }

    public ModuleDefinition getModuleDefinition() {
        return _moduleDefinition;
    }

}
