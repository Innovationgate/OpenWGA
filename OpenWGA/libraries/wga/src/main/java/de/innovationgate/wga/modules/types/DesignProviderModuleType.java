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

import de.innovationgate.wga.modules.DeclaringModuleType;
import de.innovationgate.wga.modules.ModuleType;
import de.innovationgate.wgpublisher.design.WGADesignProvider;

/**
 * Type to define a provider of an OpenWGA desing
 */
public class DesignProviderModuleType implements DeclaringModuleType {

    public String getDescription() {
        return "A design for a WGA Content Store";
    }

    public Class getPropertiesClass() {
        return null;
    }

    public String getTitle() {
        return "design type";
    }

    public boolean isKeyBased() {
        return false;
    }

    public boolean isSelfRegistered() {
        return false;
    }

    public Class<? extends Object> getImplementationBaseClass() {
        return WGADesignProvider.class;
    }

    public boolean isPropertiesNeeded() {
        return false;
    }

    public Class<? extends Object> getPropertyClass() {
        return null;
    }

}
