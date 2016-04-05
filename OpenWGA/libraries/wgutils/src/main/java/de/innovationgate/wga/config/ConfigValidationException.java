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
package de.innovationgate.wga.config;

import java.util.ArrayList;
import java.util.List;

import de.innovationgate.wga.model.ValidationError;

/**
 * Thrown on validation errors in WGA configuration
 */
public class ConfigValidationException extends Exception {
	
	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    List<ValidationError> _validationErrors = new ArrayList<ValidationError>();

	public ConfigValidationException() {
		super();
	}

	public ConfigValidationException(String message, Throwable cause) {
		super(message, cause);
	}

	public ConfigValidationException(String message) {
		super(message);
	}

	public ConfigValidationException(Throwable cause) {
		super(cause);
	}

	public List<ValidationError> getValidationErrors() {
		return _validationErrors;
	}

	public void setValidationErrors(List<ValidationError> validationErrors) {
		_validationErrors = validationErrors;
	}

}
