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

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

public abstract class AbstractModel implements Model  {
	
	public boolean _listenerNotificationEnabled = true;
	
	private Set<ModelListener> _listeners = Collections.synchronizedSet(new HashSet<ModelListener>());

	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#addListener(de.innovationgate.wga.model.ModelListener)
     */
	public void addListener(ModelListener listener) {
	    synchronized (_listeners) {
	        _listeners.add(listener);    
        }		
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#removeListener(de.innovationgate.wga.model.ModelListener)
     */
	public void removeListener(ModelListener listener) {
	    synchronized (_listeners) {
	        _listeners.remove(listener);    
        }		
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#fireModelChanged()
     */
	public void fireModelChanged() {
		if (isListenerNotificationEnabled()) {
		    Set<ModelListener> current = new HashSet<ModelListener>();
		    synchronized (_listeners) {		        
		        current.addAll(_listeners);
		    }
			Iterator<ModelListener> listeners = current.iterator();
			while (listeners.hasNext()) {
				try {
				    ModelListener listener = listeners.next();
				    // check if listener is still valid - might has been removed during previous listener calls
				    if (_listeners.contains(listener)) {
				        listener.modelChanged();
				    }
                }
                catch (Exception e) {
                    e.printStackTrace();
                    // ignore exceptions on listener
                }			
			}	    
		}
	}
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#saveChanges()
     */
	public abstract void saveChanges() throws IOException, IllegalAccessException, InvocationTargetException;
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#reload()
     */
	public abstract void reload() throws IOException;
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#validate()
     */
	public abstract List<ValidationError> validate();
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#isEditable(java.lang.String)
     */
	public abstract boolean isEditable(String propertyName);
	
	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#restoreDefaults()
     */
	public void restoreDefaults() throws IOException {		
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#isListenerNotificationEnabled()
     */
	public boolean isListenerNotificationEnabled() {
		return _listenerNotificationEnabled;
	}

	/* (non-Javadoc)
     * @see de.innovationgate.wga.model.IModel#setListenerNotificationEnabled(boolean)
     */
	public void setListenerNotificationEnabled(boolean listenerNotificationEnabled) {
		_listenerNotificationEnabled = listenerNotificationEnabled;
	}
}
