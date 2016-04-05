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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ReferenceType;


/**
 * A very basic implementation of {@link OptionDefinition} which merely is a bean containing all those retrievable information in an un-localized form and offering simple setters for them.
 *
 * Options of this class are by default:
 * - Non-optional
 * - Non-expert
 * - have no default value, category or dependent options
 *
 */
public class BasicOptionDefinition implements OptionDefinition {
    
    private static final LocalisationBundleLoader BASIC_LOADER = new LocalisationBundleLoader(WGUtils.getPackagePath(LocalizedOptionDefinition.class) + "/optiondefinitions", LocalizedOptionDefinition.class.getClassLoader());
    
    private String _name;
    private OptionType _optionType;
    private String _defaultValue = null;
    private String _description = null;
    private String _title = "";
    private String _category = null;
    private boolean _optional = false;
    private boolean _expert = false;
    private Set<String> _flags = new HashSet<String>();
    
    public boolean hasFlag(String flagName) {
        return _flags.contains(flagName);
    }
    
    public void addFlag(String flagName) {
        _flags.add(flagName);
    }
    
    public void removeFlag(String flagName) {
        _flags.remove(flagName);
    }

    private List<DependentOption> _dependentOptions = new ArrayList<DependentOption>();

    public boolean isExpert() {
        return _expert;
    }

    public void setExpert(boolean expert) {
        _expert = expert;
    }

    public BasicOptionDefinition(String name, OptionType optionType) {
        _name = name;
        _title = name;
        _optionType = optionType;
    }

    public String getDefaultValue() {
        return _defaultValue;
    }

    public String getDescription(Locale locale) {
        return _description;
    }

    public String getName() {
        return _name;
    }

    public OptionType getOptionType() {
        return _optionType;
    }

    public String getTitle(Locale locale) {
        return _title;
    }

    public boolean isOptional() {
        return _optional;
    }

    public void setOptionType(OptionType optionType) {
        _optionType = optionType;
    }

    public void setDefaultValue(String defaultValue) {
        _defaultValue = defaultValue;
    }

    public void setDescription(String description) {
        _description = description;
    }

    public void setTitle(String title) {
        _title = title;
    }

    public void setOptional(boolean optional) {
        _optional = optional;
    }

    public List<DependentOption> getDependentOptions() {
        return _dependentOptions;
    }

    public void setDependentOptions(List<DependentOption> dependentOptions) {
        _dependentOptions = dependentOptions;
    }
    
    public void addDependentOption(String option) {
        
        DependentOption depOpt = new DependentOption();
        depOpt.setName(option);
        _dependentOptions.add(depOpt);
    }
    
    public void addDependentOption(String option, String neededValue) {
        DependentOption depOpt = new DependentOption();
        depOpt.setName(option);
        depOpt.setNeededValue(neededValue);
        _dependentOptions.add(depOpt);
    }

    public String getCategory() {
        return _category;
    }

    public void setCategory(String category) {
        _category = category;
    }

    public void validate(String value, Locale locale, ValidationContext cx) throws OptionValueValidationException {

        
        if (!isEmptyAllowed() && WGUtils.isEmpty(value)) {
            throw new OptionValueValidationException(BASIC_LOADER.getBundle(locale).getString("message.emptyfield"));
        }
        
        getOptionType().validate(value, locale, cx);
        
    }
    
    public boolean isEmptyAllowed() {
        return getOptionType().isEmptyAllowed();
    }

    public Class<? extends ReferenceType> getReferenceType() {
        if (_optionType instanceof ReferenceOptionType) {
            return ((ReferenceOptionType) _optionType).getReferenceType();
        }
        
        return null;
    }
    
    

}
