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

package de.innovationgate.wga.modules.types;

import de.innovationgate.webgate.api.workflow.WGWorkflowEngine;
import de.innovationgate.wga.modules.DeclaringModuleType;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wga.modules.properties.WorkflowEngineProperties;

/**
 * Type for implementations of OpenWGA workflow engines
 */
public class WorkflowEngineModuleType implements DeclaringModuleType {

    public String getDescription() {
        return "Provides workflow capabilities for WGA Content Stores";
    }

    public Class getPropertiesClass() {
        return null;
    }

    public String getTitle() {
        return "workflow engine type";
    }

    public boolean isKeyBased() {
        return false;
    }

    public boolean isSelfRegistered() {
        return false;
    }

    public Class<? extends Object> getImplementationBaseClass() {
        return WGWorkflowEngine.class;
    }

    public boolean isPropertiesNeeded() {
        return true;
    }

    public Class<? extends Object> getPropertyClass() {
        return WorkflowEngineProperties.class;
    }

}
