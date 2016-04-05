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
import java.util.MissingResourceException;
import java.util.Set;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ReferenceType;

/**
 * Implementation of an option definition providing localized information.
 * 
 * The bundles retrieved by the given bundle loader in the constructor are expected to contain the following label keys with localized information:
 * <ul>
 * <li>option.&lt;optionname&gt;.title containing the option title
 * <li>option.&lt;optionname&gt;.description containing the option description
 * </ul>
 *
 * Options of this class are by default:
 * - Non-optional
 * - Non-expert
 * - have no default value, category or dependent options
 *
 */
public class LocalizedOptionDefinition implements OptionDefinition {
    
    private static final LocalisationBundleLoader BASIC_LOADER = new LocalisationBundleLoader(WGUtils.getPackagePath(LocalizedOptionDefinition.class) + "/optiondefinitions", LocalizedOptionDefinition.class.getClassLoader());
    
    
    private String _name;
    private OptionType _optionType;
    private String _defaultValue = null;
    private boolean _optional = false;
    private boolean _expert = false;
    private String _category = null;
    private Boolean _emptyAllowedOverride = null;
    private Class<? extends ReferenceType> _referenceTypeOverride = null;
    private LocalisationBundleLoader _bundleLoader;
    private List<DependentOption> _dependentOptions = new ArrayList<DependentOption>();
    private Set<String> _flags = new HashSet<String>();
    
    /**
     * @param name The name/key of the option
     * @param optionType The type of the option
     * @param bundleLoader The bundle loader to load localized information
     */
    public LocalizedOptionDefinition(String name, OptionType optionType, LocalisationBundleLoader bundleLoader) {
        _name = name;
        _bundleLoader = bundleLoader;
        _optionType = optionType;
    }

    public String getDefaultValue() {
        return _defaultValue;
    }

    public String getDescription(Locale locale) {
        try {
            return _bundleLoader.getBundle(locale).getString("option." + _name + ".description");
        }
        catch (MissingResourceException e) {
            return null;
        }
    }

    public String getName() {
        return _name;
    }

    public OptionType getOptionType() {
        return _optionType;
    }

    public String getTitle(Locale locale) {
        try {
            return _bundleLoader.getBundle(locale).getString("option." + _name + ".title");
        }
        catch (MissingResourceException e) {
            return _name;
        }
    }

    public boolean isOptional() {
        return _optional;
    }

    /**
     * Sets the default value of this option
     */
    public void setDefaultValue(String defaultValue) {
        _defaultValue = defaultValue;
    }

    /**
     * Sets if this option is optional.
     */
    public void setOptional(boolean optional) {
        _optional = optional;
    }

    /**
     * Sets if this option is an expert option
     */
    public void setExpert(boolean expert) {
        _expert = expert;
    }

    public boolean isExpert() {
        return _expert;
    }

    public List<DependentOption> getDependentOptions() {
        return _dependentOptions;
    }
    
    /**
     * Adds an option on whose existence the current option's applicability depends on
     * @param option The name of the option we depend on
     */
    public void addDependentOption(String option) {
        DependentOption dependentOption = new DependentOption();
        dependentOption.setName(option);
        _dependentOptions.add(dependentOption);
    }
    
    /**
     * Adds an option on that needs to be existent with a special value for the current option to be applicable
     * @param option The name of the option we depend on
     * @param neededValue The value that the option needs to have
     */
    public void addDependentOption(String option, String neededValue) {
        DependentOption dependentOption = new DependentOption();
        dependentOption.setName(option);
        dependentOption.setNeededValue(neededValue);
        _dependentOptions.add(dependentOption);
    }

    public String getCategory() {
        return _category;
    }

    /**
     * Sets the category of this option which should be declared as {@link OptionCategoryDefinition} to the registry
     */
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
        
        if (_emptyAllowedOverride != null) {
            return _emptyAllowedOverride;
        }
        
        return getOptionType().isEmptyAllowed();
    }

    /**
     * Explicitly chooses the empty allowed setting for this option definition while overriding this setting from the option type
     * @param emptyAllowed
     */
    public void setEmptyAllowedOverride(boolean emptyAllowed) {
        _emptyAllowedOverride = emptyAllowed;
    }

    public Class<? extends ReferenceType> getReferenceType() {
        
        if (_referenceTypeOverride != null) {
            return _referenceTypeOverride;
        }
        
        if (_optionType instanceof ReferenceOptionType) {
            return ((ReferenceOptionType) _optionType).getReferenceType();
        }
        
        return null;
        
    }

    /**
     * Explicitly chooses a reference type for this option definition while overriding this setting from the option type
     * @param referenceTypeOverride The reference type that this option should declare
     */
    public void setReferenceTypeOverride(Class<? extends ReferenceType> referenceTypeOverride) {
        _referenceTypeOverride = referenceTypeOverride;
    }

    public boolean hasFlag(String flagName) {
        return _flags.contains(flagName);
    }
    
    public void addFlag(String flagName) {
        _flags.add(flagName);
    }
    
    public void removeFlag(String flagName) {
        _flags.remove(flagName);
    }
    

}
