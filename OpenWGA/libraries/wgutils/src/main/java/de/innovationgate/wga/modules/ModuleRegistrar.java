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

/**
 * A class that registers module definitions with the registry.
 * 
 * ModulRegistrars automatically get instantiated, and their method {@link #registerModules(ModuleRegistry)} gets called, when the {@link ModuleRegistry} searches for module definitions.
 * For this to work there should be:
 * - A text file resource "de/innovationgate/wga/modules/registrar.cfg" in the JAR/classes folder that contains the module(s)
 * - The text file should contain the full qualified classname of the registrar implementation and NOTHING ELSE
 *
 * ModuleRegistrars are obliged to have a default constructor.
 */
public interface ModuleRegistrar {
    
    /**
     * Gets called when the {@link ModuleRegistry} finds the registrar and instantiates it, so the registrar can register all modules known to it with the registry.
     * Call {@link ModuleRegistry#addModuleDefinition(ModuleDefinition)} for each module to do this.
     * @param registry
     */
    public void registerModules(ModuleRegistry registry);

}
