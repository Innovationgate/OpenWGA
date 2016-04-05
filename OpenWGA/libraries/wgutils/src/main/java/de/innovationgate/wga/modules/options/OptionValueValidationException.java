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

/**
 * Thrown when an option validation fails
 *
 */
public class OptionValueValidationException extends Exception {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    public OptionValueValidationException(String string) {
        super(string);
    }

    public OptionValueValidationException(String string, NumberFormatException e) {
        super(string, e);
    }

}
