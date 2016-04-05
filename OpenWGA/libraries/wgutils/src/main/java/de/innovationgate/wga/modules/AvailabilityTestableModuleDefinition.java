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
 * Interface for {@link ModuleDefinition}s that have a method for availablility testing.
 * A module is declared "available" if the WGA runtime is in a situation where the module can be actually used.
 * This is to differ from module dependencies, which determine if the module is able to run. 
 *
 * The content store authentication module has no "dependencies" which might prevent it from running correctly,
 * but when the WGA runtime has no databases connected that can be used as authentication source it should
 * be regarded "not available".
 */
public interface AvailabilityTestableModuleDefinition {
    
    /**
     * Called to test availability of this module
     * @param reg The registry, so the method can retrieve neccessary context for the operation
     * @throws ModuleAvailabilityException if the module is not available
     */
    public void testAvailability(ModuleRegistry reg) throws ModuleAvailabilityException;

}
