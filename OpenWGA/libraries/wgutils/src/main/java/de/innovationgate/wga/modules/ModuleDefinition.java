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

import java.util.Locale;

/**
 * Defines some abstact "Module" to be used by WGA.
 * Modules consist of
 * - A module type which determines ... well ... the type of module, i.e. for what purpose the module is usable 
 * - Some implementation class which is the class that implements the module. If there is no implementation it should return the class of the module definition.
 * - A varying bunch of option definitions defining which options can be given to the module implementation
 * - Descriptive title and description
 * - A custom properties object whose type depends on the module type. May also be null if no custom properties are needed 
 *
 * Modules definitions are obliged to have a default constructor. 
 * Module implementations ALSO should have a default constructor, whose usage in fact is dependent on the way the module type is used
 * 
 */
public interface ModuleDefinition {
    
    /**
     * Returns the title, maybe localized
     */
    public String getTitle(Locale locale);
    
    /**
     * Returns the description, maybe localized
     */
    public String getDescription(Locale locale);

    /**
     * Returns the map of option definitions
     */
    public OptionDefinitionsMap getOptionDefinitions();
    
    /**
     * Returns the module type. A module type is a any class implementing {@link ModuleType}.
     */
    public Class<? extends ModuleType> getModuleType();
    
    /**
     * Returns the implementation class of the module
     */
    public Class<? extends Object> getImplementationClass();
    
    /**
     * May test for dependencies that need to be met for this module to work.
     * Typically a module tests if all needed classes are available in classpath.
     * @throws ModuleDependencyException If a dependency is not met
     */
    public void testDependencies() throws ModuleDependencyException;
    
    /**
     * Returns a custom properties object or null
     */
    public Object getProperties();


}
