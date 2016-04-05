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

package de.innovationgate.wga.modules;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.options.ConvertingOptionType;
import de.innovationgate.wga.modules.options.OptionConversionException;
import de.innovationgate.wga.modules.options.OptionDefinition;
import de.innovationgate.wga.modules.options.OptionReader;
import de.innovationgate.wga.modules.options.OptionType;
import de.innovationgate.wga.modules.options.OptionValueValidationException;

/**
 * A map containing option definitions and providing some tool functions to register them, or to read/write option values regarding their definitions
 *
 */
public class OptionDefinitionsMap extends LinkedHashMap<String, OptionDefinition>{
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * Adds an option definition to the map
     * @param def The option definition
     */
    public void addOption(OptionDefinition def) {
        put(def.getName(), def);
    }

    @Override
    public OptionDefinition put(String key, OptionDefinition value) {
        
        if (!(value.getName().equals(key))) {
            throw new IllegalArgumentException("Key must match option name");
        }
        
        return super.put(key, value);
    }
    
    /**
     * Sort options by the order in which they are defined in this map
     * @param options
     */
    public void sort(LinkedHashMap<String,String> options) {
        
        LinkedHashMap<String,String> unsorted = new LinkedHashMap<String,String>(options);
        options.clear();
        
        // Iterate thru definitions and add options as they are available
        for (Map.Entry<String,OptionDefinition> def : entrySet()) {
            if (unsorted.containsKey(def.getKey())) {
                options.put(def.getKey(), unsorted.get(def.getKey()));
                unsorted.remove(def.getKey());
            }
        }
        
        // Add remaining options
        options.putAll(unsorted);
        
    }
    
    
}
