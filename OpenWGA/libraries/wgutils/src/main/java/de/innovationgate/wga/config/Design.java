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

import java.beans.PropertyDescriptor;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.simpleframework.xml.Attribute;
import org.simpleframework.xml.Element;
import org.simpleframework.xml.ElementMap;
import org.simpleframework.xml.Root;

import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.properties.DesignSourceProperties;

/**
 * An OpenWGA design pointer for OpenWGA applications
 */
@Root(strict = false)
public class Design extends ConfigBean {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    @ElementMap(entry = "option", key = "name", attribute = true, required = false)
    @NotNull
    @Options
    private Map<String, String> options = new LinkedHashMap<String, String>();

    @Element
    @NotNull
    private String source;

    @Element
    @NotNull
    private String name;

    public Design() {
    }

    public Design(String reference) throws URISyntaxException {
        this(new DesignReference(reference));
    }

    public Design(DesignReference ref) {
        this.source = ref.getSourceName();
        this.name = ref.getDesignName();
    }

    public Design(String source, String name) {
        this.source = source;
        this.name = name;
    }

    public Map<String, String> getOptions() {
        return options;
    }

    public void setOptions(Map<String, String> options) {
        if (options == null) {
            this.options = new HashMap<String, String>();
        }
        else {
            this.options = options;
        }
    }

    public String getModuleImplementationClassName(WGAConfiguration config, ModuleRegistry registry) {
        
        ModuleDefinition sourceDef = null;
        
        // We must find the design source for this design, so it can tell us which provider implementation class we have
        if (config != null) {
            DesignSource sourceObj = config.getDesignConfiguration().getDesignSource(source);
            if (sourceObj != null) {
                sourceDef = registry.getModuleDefinition("de.innovationgate.wga.modules.types.DesignSourceModuleType", sourceObj.getImplClassName());
            }
        }

        // If design source not found it may be singleton. We try to find the right one in registry
        if (sourceDef == null) {
            Iterator<ModuleDefinition> modDefs = registry.getModulesForType("de.innovationgate.wga.modules.types.DesignSourceModuleType").values().iterator();
            while (modDefs.hasNext()) {
                ModuleDefinition moduleDefinition = (ModuleDefinition) modDefs.next();
                DesignSourceProperties props = (DesignSourceProperties) moduleDefinition.getProperties();
                if (props != null && props.isSingleton() && props.getSingletonUID().equals(getSource())) {
                    sourceDef = moduleDefinition;
                    break;
                }
            }
        }
        
        // Ask the source definition about the type of design provider
        if (sourceDef != null) {
            DesignSourceProperties props = (DesignSourceProperties) sourceDef.getProperties();
            if (props != null && props.getDesignProviderClass() != null) {
                return props.getDesignProviderClass().getName();
            }
        }
        
        return null;
        
    }
    
    @Override
    public List<ModuleDefinition> getOptionDefinitions(ModuleRegistry registry, PropertyDescriptor property, WGAConfiguration config) {
        
        List<ModuleDefinition> list = new ArrayList<ModuleDefinition>();
        
        
        if (property.getName().equals("options")) {
            String designClassName = getModuleImplementationClassName(config, registry);
            if (designClassName != null) {
                list.add(registry.getModuleDefinition("de.innovationgate.wga.modules.types.DesignProviderModuleType", designClassName));
            }
        }
        
        
        return list;
        
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

}
