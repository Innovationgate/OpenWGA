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
import de.innovationgate.wgpublisher.shares.ShareProperties;

/**
 * Type for implementations of OpenWGA content shares
 */
public class ShareModuleType implements DeclaringModuleType {

    public String getDescription() {
        return "A type of share, offering WGA data via a special protocol";
    }

    public String getTitle() {
        return "Share type";
    }

    public boolean isKeyBased() {
        return false;
    }

    public boolean isSelfRegistered() {
        return true;
    }

    public Class<? extends Object> getImplementationBaseClass() {
        return null;
    }

    public boolean isPropertiesNeeded() {
        return true;
    }

    public Class<? extends Object> getPropertyClass() {
        return ShareProperties.class;
    }

}
