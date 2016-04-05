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
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.types.HashingSchemeType;

/**
 * An option value provider providing the registered ways to hash data (modules of type {@link HashingSchemeType})
 */
public class HashingSchemesValueProvider implements OptionValueProvider {

    private ModuleRegistry _reg;

    public HashingSchemesValueProvider(ModuleRegistry reg) {
        _reg = reg;
    }

    public String getEmptyListMessage(Locale locale) {
        return "No hashing schemes registered"; // Cannot happen
    }

    public List<String> getProvidedValues() {
        List<String> vals = new ArrayList<String>();
        Iterator<ModuleDefinition> defs = _reg.getModulesForType(HashingSchemeType.class).values().iterator();
        while (defs.hasNext()) {
            KeyBasedModuleDefinition moduleDefinition = (KeyBasedModuleDefinition) defs.next();
            vals.add(moduleDefinition.getRegistrationKey());
        }
        return vals;
        
    }

    public String getValueTitle(String value, Locale locale) {
        
        ModuleDefinition def = _reg.getModuleDefinitionByKey(HashingSchemeType.class, value);
        if (def != null) {
            return def.getTitle(locale);
        }
        return value;
        
    }

}
