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
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.wga.modules.LocalisationBundleLoader;

/**
 * An implementation of {@link OptionValueProvider} providing localized value titles for a given set of predefined values.
 * Use {@link #addValue(String)} to add all values that this provider should provider.
 * 
 * The value provider is given some name which is used in localized information lookup.
 * The bundles retrieved by the given bundle loader in the constructor are expected to contain the following label keys with localized information for each provided value:
 * <ul>
 * <li>option.&lt;optionname&gt;.value.&gt;stringvalue&lt;.title containing the option value title
 * <li>option.&lt;optionname&gt;.emptylist.message (optionally) containing a message to show when the provided values are empty
 * </ul>
 *
 */
public class LocalizedOptionValueProvider implements PredefinedValuesValueProvider {

    private List<String> _values = new ArrayList<String>();
    private LocalisationBundleLoader _loader;
    private String _optionName;


    /**
     * @param optionName Some name of this value provider used to lookup localized labels in the bundle loader
     * @param loader The bundle loader to load localized information from
     */
    public LocalizedOptionValueProvider(String optionName, LocalisationBundleLoader loader) {
        _optionName = optionName;
        _loader = loader;
    }


    public List<String> getProvidedValues() {
        return _values;
    }


    public String getValueTitle(String value, Locale locale) {
        try {
            return _loader.getBundle(locale).getString("option." + _optionName + ".value." + value + ".title");
        }
        catch (Exception e) {
            // As the default charset may not be titled we catch exceptions here and return the value
            return value;
        } 
    }
    
    /* (non-Javadoc)
     * @see de.innovationgate.wga.modules.options.PredefinedValuesValueProvider#addValue(java.lang.String)
     */
    @Override
    public void addValue(String value, String title) {
        _values.add(value);
    }
    
    public void addValue(String value) {
        _values.add(value);
    }

    public String getEmptyListMessage(Locale locale) {

        try {
            String msg = _loader.getBundle(locale).getString("option." + _optionName + ".emptylist.message");
            if (msg != null) {
                return msg;
            }
        }
        catch (Exception e) {
        }
        
        return null;
    }

}
