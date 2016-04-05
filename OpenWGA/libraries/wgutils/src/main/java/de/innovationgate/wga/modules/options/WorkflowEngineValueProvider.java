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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.apache.log4j.Logger;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.modules.LocalisationBundleLoader;
import de.innovationgate.wga.modules.ModuleDefinition;
import de.innovationgate.wga.modules.ModuleRegistry;

/**
 * Value provider providing registered workflow engines (modules of WGAPI type WorkflowEngineModuleType}
 *
 */
public class WorkflowEngineValueProvider extends ModuleDefinitionsValueProvider {
    
    private LocalisationBundleLoader _bundleLoader = new LocalisationBundleLoader(WGUtils.getPackagePath(getClass()) + "/emptylistmessages", getClass().getClassLoader());

    public WorkflowEngineValueProvider(ModuleRegistry reg) {
        super(reg, "de.innovationgate.wga.modules.types.WorkflowEngineModuleType");
    }

    public String getEmptyListMessage(Locale locale) {
        return _bundleLoader.getBundle(locale).getString("option.workflowengine.emptylist.message");
    }

}
