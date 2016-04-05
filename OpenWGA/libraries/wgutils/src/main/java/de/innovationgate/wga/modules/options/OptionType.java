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

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.model.ValidationError;

/**
 * Definition of a type of option, which determines the data type of the option and it's possible values.
 * 
 * Generally all options are stored as key/value pairs of strings where the key is the name of the option and the value is some kind of string.
 * However, many option values do not represent strings. They may be used as numbers instead or as boolean values to which their string values are converted.
 * The option type gives a "hint" about the data type in which an option is actually used.
 * 
 * Additionally an option type may define a range of allowed values for an option that may be presented by some GUI for choosing. In that case it provides
 * an {@link OptionValueProvider} via {@link #getValueProvider(WGAConfiguration)}, which offers these allowed values.
 * 
 * Finally an option type may define a validation method which validates if a given value is valid for options of this type.
 */
public interface OptionType {
    /**
     * Returns the data type which values of this option type actually represent.
     */
    public Class<? extends Object> getDataTypeHint();
    
    /**
     * May return an option provider that provides allowed values for options of this type
     * @param configCopy If the current functionality involves the WGA configuration it may pass over the current working copy of it so the provider may use it 
     */
    public OptionValueProvider getValueProvider(WGAConfiguration configCopy);
    /**
     * If the type has a value provider: Determines if the option values are restricted to the values returned by it. If this is false the values from the value provider merely represent suggestions for values and other values may be used.
     */
    public boolean isRestricted();
    
    /**
     * Validates the given value for this option type. Should throw an {@link OptionValueValidationException} if validation failes
     * @param value The value to be validated
     * @param locale A locale that may be used to give back an error message in the thrown validation exception
     * @param cx Context information for the validation
     * @throws OptionValueValidationException if validation fails
     */
    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException; 
    
    /**
     * Returns if the option maybe empty. This is determined separate from validation to allow UIs to directly determine if input is needed.
     */
    public boolean isEmptyAllowed();
    
    /**
     * Returns if the option type may return/receive multiple values.
     * Marks options that by some custom mechanism store multiple values into the string option.
     * If that is the case it should be expected that the option value is returned as {@link List} of the data type given in {@link #getDataTypeHint()} when it is read and converted.
     */
    public boolean isMultiValue();
}
