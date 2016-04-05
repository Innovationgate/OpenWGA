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

import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleType;

/**
 * Implementation of an option category with localized information
 *
 * The bundles retrieved by the given bundle loader in the constructor are expected to contain the following label keys with localized information:
 * <ul>
 * <li>category.&lt;categoryname&gt;.title containing the category title
 * <li>category.&lt;categoryname&gt;.description containing the category description
 * </ul>
 *
 */
public class LocalizedOptionCategoryDefinition implements OptionCategoryDefinition {
    
    private String _key;
    private Class<? extends ModuleType> _moduleType;
    private LocalisationBundleLoader _bundleLoader;
    /**
     * @param moduleType The module type for which the option category is to be registered
     * @param key The name of the category
     * @param bundleLoader A bundle loader to load localized information from
     */
    public LocalizedOptionCategoryDefinition(Class<? extends ModuleType> moduleType, String key, LocalisationBundleLoader bundleLoader) {
        _moduleType = moduleType;
        _key = key;
        _bundleLoader = bundleLoader;
    }

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("category." + getKey() + ".description");
    }

    public String getKey() {
        return _key;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("category." + getKey() + ".title");
    }
    public Class<? extends ModuleType> getModuleType() {
        return _moduleType;
    }

}
