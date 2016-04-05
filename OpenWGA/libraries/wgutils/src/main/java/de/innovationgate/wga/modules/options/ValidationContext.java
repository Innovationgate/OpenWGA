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

import de.innovationgate.wga.config.WGAConfiguration;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * Serves all available context for option validations
 */
public class ValidationContext {
    
    public ValidationContext(ModuleRegistry registry, WGAConfiguration configCopy) {
        super();
        _registry = registry;
        _configCopy = configCopy;
    }

    private WGAConfiguration _configCopy = null;
    private ModuleRegistry _registry = null;

    public WGAConfiguration getConfigCopy() {
        return _configCopy;
    }

    public ModuleRegistry getRegistry() {
        return _registry;
    }

}
