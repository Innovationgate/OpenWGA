/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package de.innovationgate.wgpublisher.modules.serveroptions;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.RegistryAwareModuleDefinition;
import de.innovationgate.wga.modules.options.BasicOptionDefinition;
import de.innovationgate.wga.modules.options.DefaultOptionDefinition;
import de.innovationgate.wga.modules.options.PredefinedValuesOptionType;
import de.innovationgate.wga.modules.types.ServiceApiProperties;
import de.innovationgate.wga.modules.types.ServiceApiType;
import de.innovationgate.wga.modules.types.WGAServerOptionsModuleType;

public class ServiceApisModuleDefinition implements ModuleDefinition, RegistryAwareModuleDefinition {

    protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/serviceapis", getClass().getClassLoader());
    private ModuleRegistry _reg;
    
    @Override
    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("options.title");
    }

    @Override
    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("options.title");
    }

    @Override
    public OptionDefinitionsMap getOptionDefinitions() {
        
        OptionDefinitionsMap options = new OptionDefinitionsMap();

        for (ModuleDefinition def : _reg.getModulesForType(ServiceApiType.class).values()) {
            
            ServiceApiProperties props = (ServiceApiProperties) def.getProperties();
            if (!props.isImplementable()) {
                continue;
            }
            
            @SuppressWarnings("unchecked")
            Map<String,ModuleDefinition> implementors = _reg.getModulesForType((Class<? extends ModuleType>) def.getImplementationClass());
            if (implementors.size() > 0) {
                
                PredefinedValuesOptionType implementorsType = new PredefinedValuesOptionType();
                implementorsType.setEmptyAllowed(true);
                implementorsType.addValue("", "Use system default");
                for (ModuleDefinition implementor : implementors.values()) {
                    try {
                        implementor.testDependencies();
                        implementorsType.addValue(implementor.getImplementationClass().getName(), implementor.getTitle(Locale.getDefault()));
                    }
                    catch (ModuleDependencyException e) {
                    }
                }
                
                BasicOptionDefinition serviceOption = new BasicOptionDefinition(WGAConfiguration.SERVEROPTION_SERVICE_APIS_PREFIX + def.getImplementationClass().getName(), implementorsType);
                serviceOption.setOptional(props.isOptionalConfig());
                serviceOption.setTitle(def.getTitle(Locale.getDefault()));
                serviceOption.setDescription(def.getDescription(Locale.getDefault()));
                
                if (props.getDefaultImplementation() != null) {
                    serviceOption.setDefaultValue(props.getDefaultImplementation().getName());
                }
                
                options.addOption(serviceOption);
                
            }
            
            
        }
        
        return options;
        
    }

    @Override
    public Class<? extends ModuleType> getModuleType() {
        return WGAServerOptionsModuleType.class;
    }

    @Override
    public Class<? extends Object> getImplementationClass() {
        return ServiceApisModuleDefinition.class;
    }

    @Override
    public void testDependencies() throws ModuleDependencyException {
    }

    @Override
    public Object getProperties() {
        return null;
    }

    @Override
    public void injectRegistry(ModuleRegistry registry) {
        _reg = registry;
    }

}
