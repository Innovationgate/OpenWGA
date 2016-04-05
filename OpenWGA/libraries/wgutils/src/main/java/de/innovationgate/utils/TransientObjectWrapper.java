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
 * Wrapper for objects which should not be serialized.
 * f.e. as session attributes
 *
 */
public class TransientObjectWrapper<T> implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private boolean _transient = true;
	
	private transient T _object;
	private T _persistentObject = null;
	
	public static <Y extends Object> TransientObjectWrapper<Y> create(Y value, boolean transientMode) {
	    TransientObjectWrapper<Y> t = new TransientObjectWrapper<Y>(transientMode);
	    t.set(value);
	    return t;
	}
	
	public static <Y extends Object> TransientObjectWrapper<Y> create(Y value) {
	    return create(value, true);
    }
	
	/**
	 * Creates a normal transient object wrapper
	 */
	public TransientObjectWrapper() {
	    this(true);
	}
	
	/**
	 * Creates an object wrapper where the argument decides if it should really store its wrapped object transient.
	 * That way the transient storage can be made dependent to some other condition.
	 * @param transientMode Determines if the object should really be stored transient or persistent (meaning it will be serializable)
	 */
	public TransientObjectWrapper(boolean transientMode) {
        _transient = transientMode;
    }

	public void set(T object) {
	    if (_transient) {
	        _object = object;
	    }
	    else {
	        _persistentObject = object;
	    }
	}
	
	public T get() {
	    if (_transient) {
	        return _object;
	    }
	    else {
	        return _persistentObject;
	    }
	}

    /**
     * Returns if the object stored by the wrapper is transient and is not serialized
     */
    public boolean isTransient() {
        return _transient;
    }

}
