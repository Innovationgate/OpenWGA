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

package de.innovationgate.wgpublisher.modules.serviceapis;

import java.util.Locale;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.KeyBasedModuleDefinition;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleDependencyException;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.OptionDefinitionsMap;
import de.innovationgate.wga.modules.types.PasswordEncodingType;
import de.innovationgate.wgpublisher.AESPasswordEncoder;

public class AESEncodingModuleDefinition implements ModuleDefinition, KeyBasedModuleDefinition {
    
	protected LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/apis", getClass().getClassLoader());

    public String getDescription(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("encoding.description");
    }

    public Class<? extends Object> getImplementationClass() {
        return AESPasswordEncoder.class;
    }

    public Class<? extends ModuleType> getModuleType() {
        return PasswordEncodingType.class;
    }

    public OptionDefinitionsMap getOptionDefinitions() {
        return null;
    }

    public Object getProperties() {
        return null;
    }

    public String getTitle(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("encoding.title");
    }

    public void testDependencies() throws ModuleDependencyException {
    }

    public String getRegistrationKey() {
        return "AES";
    }

}
