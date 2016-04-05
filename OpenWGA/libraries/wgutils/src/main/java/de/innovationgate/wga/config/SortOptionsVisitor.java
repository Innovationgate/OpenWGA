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

package de.innovationgate.wga.config;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.options.OptionDefinition;

/**
 * A configuration visitor sorting options by their definition order
 */
public class SortOptionsVisitor implements ConfigBeanVisitor {
    
    public static final Logger LOG = Logger.getLogger("wga.config.removedefaults");
    
    private ModuleRegistry _registry;
    private WGAConfiguration _config;

    public SortOptionsVisitor(ModuleRegistry registry, WGAConfiguration config) {
        _registry = registry;
        _config = config;
    }

    public void visit(ConfigBean configBean) throws ClassNotFoundException {

        
        try {
            // Find all options properties
            PropertyDescriptor[] descs = Introspector.getBeanInfo(configBean.getClass()).getPropertyDescriptors();
            for (PropertyDescriptor desc : descs) {
                
                // Get the field of the property to read its annotation
                Field field = null;
                try {
                    field = WGUtils.getClassField(configBean.getClass(), desc.getName());
                }
                catch (Exception e) {
                }
                
                if (field == null) {
                    continue;
                }
                
                Options optionsAnn = field.getAnnotation(Options.class);
                if (optionsAnn != null) {
                    
                    // Get the option definitions for the options property
                    // For each definition we sort the options (which might only be really effective if we have only one definition)
                    List<ModuleDefinition> defs = configBean.getOptionDefinitions(_registry, desc, _config);
                    for (ModuleDefinition def : defs) {
                        if (def == null) {
                            continue;
                        }
                        
                        OptionDefinitionsMap optionDefs = def.getOptionDefinitions();
                        LinkedHashMap<String,String> options = (LinkedHashMap<String, String>) desc.getReadMethod().invoke(configBean);
                        optionDefs.sort(options);
                        
                        
                    }
                }
            }
        }
        catch (Exception e) {
            LOG.error("Exception removing option defaults", e);
        }
        
        
        
        

    }

}
