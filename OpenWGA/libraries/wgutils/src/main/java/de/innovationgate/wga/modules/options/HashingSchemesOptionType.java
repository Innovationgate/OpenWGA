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
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.types.HashingSchemeType;

/**
 * An option type that offers the registered ways to hash data (modules of type {@link HashingSchemeType})
 *
 */
public class HashingSchemesOptionType extends StringOptionType {
    
    private ModuleRegistry _reg;

    public HashingSchemesOptionType(ModuleRegistry reg) {
        _reg = reg;
    }

    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return new HashingSchemesValueProvider(_reg);
    }

    public boolean isRestricted() {
        return true;
    }

    public void validate(String value, Locale locale, WGAConfiguration configCopy) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(null);
        if (!provider.getProvidedValues().contains(value)) {
            throw new OptionValueValidationException("The hashing scheme type '" + value + "' in unknown");
        }
    }

}
