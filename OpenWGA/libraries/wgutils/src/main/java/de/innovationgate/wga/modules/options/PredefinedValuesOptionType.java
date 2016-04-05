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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.model.ValidationError;
import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * Tool class for building option types that offer some predefined values
 * 
 * To use simply instantiate the class and add all values to offer via method {@link #addValue(String)}. Pass a bundle loader to the constructor which serves titles for the values.
 * 
 * Look class {@link LocalizedOptionValueProvider} - which is internally used by this type - which label names are expected to be served by the bundle loader that is given to this class.
 * 
 */
public class PredefinedValuesOptionType extends StringOptionType {
    

    /**
     * @param loader An bundle loader to load localized information
     * @param optionName A name that is used to lookup value titles from the bundle loader
     */
    public PredefinedValuesOptionType(LocalisationBundleLoader loader, String optionName) {
        if (loader != null && optionName != null) {
            _valueProvider = new LocalizedOptionValueProvider(optionName, loader);
        }
        else  {
            _valueProvider = new BasicPredefinedValuesProvider();
        }
            
    }
    
    public PredefinedValuesOptionType() {
        this(null,null);
    }

    private boolean _restricted = true;
    private PredefinedValuesValueProvider _valueProvider = null;
    private boolean _emptyAllowed;
    
    /**
     * Add a value to be offered by this option type
     * @param value
     */
    public void addValue(String value, String title) {
         _valueProvider.addValue(value, title);
    }
    
    public void addValue(String value) {
        _valueProvider.addValue(value, null);
    }

    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return _valueProvider;
    }

    public boolean isRestricted() {
        return _restricted;
    }

    public void validate(String value, Locale locale, WGAConfiguration config) throws OptionValueValidationException {
        
        if (!isRestricted()) {
            return;
        }
        
        List<String> values = _valueProvider.getProvidedValues();
        if (!values.contains(value)) {
            throw new OptionValueValidationException("Value '" + value + "' is no valid value for this option");
        }
        
    }

    /**
     * Sets if this option type is restricted
     */
    public void setRestricted(boolean restricted) {
        _restricted = restricted;
    }

    @Override
    public boolean isEmptyAllowed() {
        return _emptyAllowed;
    }

    public void setEmptyAllowed(boolean emptyAllowed) {
        _emptyAllowed = emptyAllowed;
    }
}
