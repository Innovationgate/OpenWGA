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

import org.apache.log4j.Logger;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * A generic value provider providing titles and implementation classes of a given module type as as title/values
 *
 */
public abstract class ModuleDefinitionsValueProvider implements OptionValueProvider {
    
    private ModuleRegistry _reg;
    private String _moduleType;

    public ModuleDefinitionsValueProvider(ModuleRegistry reg, String moduleType) {
        _reg = reg;
        _moduleType = moduleType;
    }

    public List<String> getProvidedValues() {
        
        List<String> engines = new ArrayList<String>();
        Iterator<ModuleDefinition> types = _reg.getModulesForType(_moduleType).values().iterator();
        while (types.hasNext()) {
            ModuleDefinition moduleDefinition = (ModuleDefinition) types.next();
            engines.add(moduleDefinition.getImplementationClass().getName());
        }
        return engines;
        
    }

    public String getValueTitle(String value, Locale locale) {
        try {
            ModuleDefinition def = _reg.getModuleDefinition(_moduleType, value);
            if (def != null) {
                return def.getTitle(locale);
            }
            else {
                return value;
            }
        }
        catch (Exception e) {
            Logger.getLogger("wga.modules").error("Exception retrieving title for module implementation" + value + " (type " + _moduleType +")", e);
            return value;
        }
    }

}
