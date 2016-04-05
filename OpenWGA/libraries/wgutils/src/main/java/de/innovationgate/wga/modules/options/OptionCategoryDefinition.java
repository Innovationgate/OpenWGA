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

import de.innovationgate.wga.modules.ModuleRegistry;
import de.innovationgate.wga.modules.ModuleType;

/**
 * Interface for the definition of an option category.
 * 
 * For each category name that is returned by {@link OptionDefinition#getCategory()} on any option there should be an option category in registry.
 * Option categories depend on some module type. They are only used for options of module definitions that belong to the module type they are registered for.
 * That way it is possible to register one category name for two different module types with differing information.
 * 
 * Register option category implementations via {@link ModuleRegistry#addOptionCategoryDefinition(OptionCategoryDefinition)}
 */
public interface OptionCategoryDefinition {
    
    /**
     * Returns the key of the option category, which is the string returned by {@link OptionDefinition#getCategory()}
     */
    public String getKey();
    
    /**
     * Returns a localized title for the category
     */
    public String getTitle(Locale locale);
    
    /**
     * Returns a localized description for the category
     */
    public String getDescription(Locale locale);
    
    /**
     * Returns the module type for which this category is registered
     */
    public Class<? extends ModuleType> getModuleType();

}
