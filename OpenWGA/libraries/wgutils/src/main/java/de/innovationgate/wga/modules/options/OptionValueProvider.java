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

/**
 * An object that provides possible/allowed values for some option type 
 */
public interface OptionValueProvider {
    /**
     * Returns the possible/allowed values
     */
    public List<String> getProvidedValues();
    
    /**
     * Returns a - maybe localized - title for a given value
     * @param value The value
     * @param locale The locale for localisation
     */
    public String getValueTitle(String value, Locale locale);
    
    /**
     * Returns a - maybe localized - message to display when the list of provided values is empty
     * @param locale The locale for localisation
     */
    public String getEmptyListMessage(Locale locale);
}
