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

package de.innovationgate.utils;

import java.io.Serializable;

/**
 * An empty object, used to represent NULL in cache lists.
 * Cache lists cannot cache NULL directly, because this would mostly be interpreted as empty cache.
 */
public class NullPlaceHolder implements Serializable {

    private static final long serialVersionUID = 1L;

    public NullPlaceHolder() {
    }
    
    /**
     * Prepares a value for writing, replacing null by a NullPlaceHolder in case
     * @param value The value to prepare
     * @return The value to be written
     */
    public static Object write(Object value) {
            return (value != null ? value : new NullPlaceHolder());
    }
    
    /**
     * Converts a value retrieved from a place that may contain a real value or a NullPlaceHolder,
     * which is converted to a null.
     * @param value The value retrieved
     * @return A value or null
     */
    public static Object read(Object value) {
        return (value instanceof NullPlaceHolder ? null :value);
}

}
