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
import de.innovationgate.wga.modules.options.PasswordOptionEncoder;

/**
 * Type to define a way to store passwords in OpenWGA configuration, which need to be re-decodable
 */
public class PasswordEncodingType implements DeclaringModuleType {

    public String getDescription() {
        return "A way to encode and decode passwords for secure storage";
    }

    public String getTitle() {
        return "Password encoding type";
    }

    public boolean isKeyBased() {
        return true;
    }

    public boolean isSelfRegistered() {
        return false;
    }

    public Class<? extends Object> getImplementationBaseClass() {
        return PasswordOptionEncoder.class;
    }

    public boolean isPropertiesNeeded() {
        return false;
    }

    public Class<? extends Object> getPropertyClass() {
        return null;
    }

}
