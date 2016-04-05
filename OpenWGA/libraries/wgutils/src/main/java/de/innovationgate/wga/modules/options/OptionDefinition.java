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

import de.innovationgate.wga.modules.ReferenceType;

/**
 * Iterface for classes that define options for modules.
 * 
 */
public interface OptionDefinition {
    
    /**
     * Returns the name of the option, which is used as key on option maps
     */
    public String getName();
    /**
     * Returns if this option is "optional" i.e. it can be omitted from module options
     */
    public boolean isOptional();
    
    /**
     * Returns if this option is an "expert option" which only should be offered in "expert mode"
     */
    public boolean isExpert();
    
    /**
     * Returns a - maybe localized - description for this option
     */
    public String getDescription(Locale locale);
    
    /**
     * Returns the type of this option which determines the values that are accepted and the data type in which it is used
     */
    public OptionType getOptionType();
    
    /**
     * Returns a default value for this option. On "optional" options this should be the value that is effective when the option is omitted. On mandatory options this should be the value that is recommended.
     */
    public String getDefaultValue();
    
    /**
     * Returns a - maybe localized - title for this option
     */
    public String getTitle(Locale locale);
    
    
    /**
     * May return a category for this option, which may help some GUI to group options. Should return a category name/key which can be looked by as {@link OptionCategoryDefinition} containing further information. Return null if no category applies.
     */
    public String getCategory();
    
    
    /**
     * Returns a list of options that need to be present - and optionally of some special value - for this option to be applicable 
     */
    public List<DependentOption> getDependentOptions();
    
    /**
     * Validates the given value against the option definition
     * Implementations are obliged to pass the call to method {@link OptionType#validate(String, Locale, ValidationContext)} of their {@link OptionType} internally 
     * @param value The value to validate
     * @param locale A locale which is used to choose the right language of validation error message
     * @param cx A validation context
     * @throws OptionValueValidationException
     */
    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException;
    
    /**
     * Returns if the option maybe empty. This is determined separate from validation to allow UIs to directly determine if input is needed.
     * This method may pass the call to {@link OptionType#isEmptyAllowed()} if the option definition itself does not enforce emptiness allowance
     */
    public boolean isEmptyAllowed();
    
    /**
     * Returns if an option has a special flag, which may cause it to be specially treated by some functionalities
     */
    public boolean hasFlag(String flagName);
    
    /**
     * Returns a reference type if values of this option represent references to well known resources in WGA, or null if they don't
     */
    public Class<? extends ReferenceType> getReferenceType();
}
