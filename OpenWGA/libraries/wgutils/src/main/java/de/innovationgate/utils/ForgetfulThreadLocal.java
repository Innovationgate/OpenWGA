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

import java.util.HashMap;
import java.util.Map;

/**
 * A ThreadLocal implementation whose contents should be easier collectable by GC.
 * It contains a clear() method the removes all values stored for all threads.
 * Huge drawback: On normal ThreadLocals, the threadlocal value will be cleared when
 * a thread is collected. This is not provided by this implementation.
 * @deprecated 
 */
public class ForgetfulThreadLocal {

	private Map values = new HashMap();
	
	public void set(Object value) {
		values.put(new Integer(Thread.currentThread().hashCode()), value);
	}
	
	public Object get() {
		return values.get(new Integer(Thread.currentThread().hashCode()));
	}
	
	public void clear() {
		values.remove(new Integer(Thread.currentThread().hashCode()));
	}


}
