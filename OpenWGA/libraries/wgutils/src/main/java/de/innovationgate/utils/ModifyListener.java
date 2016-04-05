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

/**
 * interface for ModifyListeners
 * A modifiyListener is notified on modifications of monitorable maps, lists and objects
 * @see ModifyListenerFactory
 *
 */
public interface ModifyListener {
    
	/**
	 * called when the target monitorable object has been modified
	 */
    public void hasBeenModified();
    
}
