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
package de.innovationgate.wgaservices.types;

/**
 * The result of a remote action
 */
public class ActionResult {

	private Object _nativeResult = null;
	private Form _form = null;
	
	/**
	 * Returns the return value of the action
	 */
	public Object getNativeResult() {
		return _nativeResult;
	}
	/**
	 * Sets the return value of the action
	 */
	public void setNativeResult(Object nativeResult) {
		_nativeResult = nativeResult;
	}
	/**
	 * Returns a WebTML form returned as return value of the action
	 */
	public Form getForm() {
		return _form;
	}
	/**
	 * Sets a WebTML form returned as return value of the action
	 */
	public void setForm(Form form) {
		_form = form;
	}

	
}
