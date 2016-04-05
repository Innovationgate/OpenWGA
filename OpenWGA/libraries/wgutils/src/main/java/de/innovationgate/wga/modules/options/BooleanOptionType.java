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

import java.util.Locale;

import de.innovationgate.wga.config.WGAConfiguration;

/**
 * Option type storing boolean values as "true" and "false"
 *
 */
public class BooleanOptionType implements OptionType, ConvertingOptionType {
    
    public static final BooleanOptionType INSTANCE = new BooleanOptionType();
    
    protected BooleanOptionType() {
    }

    public Class<? extends Object> getDataTypeHint() {
        return Boolean.class;
    }

    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return null;
    }

    public boolean isRestricted() {
        return false;
    }

    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {
        if (!value.equals(Boolean.FALSE.toString()) && !value.equals(Boolean.TRUE.toString())) {
            throw new OptionValueValidationException("Option value must be either '" + Boolean.TRUE.toString() + "' or '" + Boolean.FALSE.toString() + "'");
        }
    }

    public boolean isEmptyAllowed() {
        return false;
    }

    public String convert(Object value) throws OptionConversionException {
        return OptionReader.fromDataType(this, value);
    }

    public Object unconvert(String value) throws OptionConversionException {
        return OptionReader.toDataType(this, value);
    }

    public boolean isMultiValue() {
        return false;
    }

}
