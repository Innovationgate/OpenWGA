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

/**
 * Interface to be used by {@link ModuleDefinition} instances that also want to be retrievable from registry by an arbitrary key.
 * Should return the key to use in method {@link #getRegistrationKey()}.
 * Retrieve key based modules by {@link ModuleRegistry#getModuleDefinitionByKey(Class, String)}.
 */
public interface KeyBasedModuleDefinition extends ModuleDefinition {
    
    /**
     * Returns an arbitrary string key by which the definition should be registrered
     */
    public String getRegistrationKey();

}
