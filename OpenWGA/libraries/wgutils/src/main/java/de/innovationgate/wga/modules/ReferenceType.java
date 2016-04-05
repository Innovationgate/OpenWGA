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
 * Interface for classes that represent reference types, i.e. references to well known resources in WGA
 * Implementors of this interface are hardly ever used by instance. Rather their class definitions are used to declare the reference type of an option.
 *
 */
public interface ReferenceType {
    
    /**
     * Returns some descriptive title of the reference type
     */
    public String getTitle();
    
    /**
     * Returns some longer description of the reference type and it's purpose
     */
    public String getDescription();

}
