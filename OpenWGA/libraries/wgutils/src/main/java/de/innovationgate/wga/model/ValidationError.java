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
package de.innovationgate.wga.model;

/**
 * bean for model/configuration validation errors
 * a validation error might have a property hint to the model property to let the gui know
 * which input field has validation errors
 *
 */
public class ValidationError extends Throwable {

	/**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _message;
	private String[] _propertyHints;
	private Object _bean;

	public ValidationError(String message, String[] propertyHints) {
		_message = message;
		_propertyHints = propertyHints;
	}
	
	public ValidationError(String message, String[] propertyHints, Object bean) {
		_message = message;
		_propertyHints = propertyHints;
		_bean = bean;
	}

	public String getMessage() {
		return _message;
	}

	public void setMessage(String message) {
		_message = message;
	}
	
	/**
	 * name of the model properties which causes the error
	 */
	public String[] getPropertyHints() {
		return _propertyHints;
	}

	public void setPropertyHints(String[] propertyHints) {
		_propertyHints = propertyHints;
	}
	
	/**
	 * returns the bean which causes the validation error - can be 'null'
	 */
	public Object getBean() {
		return _bean;
	}

	public void setBean(Object bean) {
		_bean = bean;
	}
	
}
