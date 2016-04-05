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
 * A type of module which determines for which purpose a module is usable.
 * Module type classes are rarely instantiated. Mostly the class itself is used to identify a module type on a module definition.
 * Each WGA functionality may create own {@link ModuleType} implementations to use the registry for a very own registration purpose
 */
public interface ModuleType {
    
    /**
     * Returns some descriptive title of the module typew
     */
    public String getTitle();
    
    /**
     * Returns some longer description of the module type and it's purpose
     */
    public String getDescription();
}
