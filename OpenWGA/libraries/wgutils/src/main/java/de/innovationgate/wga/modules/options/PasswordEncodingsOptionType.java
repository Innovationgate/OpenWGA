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
import de.innovationgate.wga.modules.types.PasswordEncodingType;

/**
 * An option type that offers the registered ways to encode passwords (modules of type {@link PasswordEncodingType})
 *
 */
public class PasswordEncodingsOptionType extends StringOptionType {
    
    private ModuleRegistry _reg;

    public PasswordEncodingsOptionType(ModuleRegistry reg) {
        _reg = reg;
    }

    public OptionValueProvider getValueProvider(WGAConfiguration configCopy) {
        return new PasswordEncodingsValueProvider(_reg);
    }

    public boolean isRestricted() {
        return true;
    }

    public void validate(String value, Locale locale, WGAConfiguration configCopy) throws OptionValueValidationException {
        OptionValueProvider provider = getValueProvider(null);
        if (!provider.getProvidedValues().contains(value)) {
            throw new OptionValueValidationException("The password encoding type '" + value + "' in unknown");
        }
    }

}
