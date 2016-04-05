/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 * 
 * This file is part of the OpenWGA server platform.
 * 
 * OpenWGA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * In addition, a special exception is granted by the copyright holders
 * of OpenWGA called "OpenWGA plugin exception". You should have received
 * a copy of this exception along with OpenWGA in file COPYING.
 * If not, see <http://www.openwga.com/gpl-plugin-exception>.
 * 
 * OpenWGA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with OpenWGA in file COPYING.
 * If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package de.innovationgate.webgate.api.simple;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import de.innovationgate.webgate.api.WGDatabase;

/**
 * A bean creation handler for {@link BeanAdapter} which uses a bean constructor taking three parameters:
 * <ul>
 * <li> The bean adapter itself of type {@link BeanAdapter}
 * <li> The user name logged in to the OpenWGA domain (String)
 * <li> The password of that user
 * </ul>
 */
public class LoginBeanCreationHandler implements BeanCreationHandler {
	
	private Method _destructor;
	private Constructor _constructor = null;
	private BeanAdapter _adapter = null;
	private boolean _includeAdapter = false; 

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.simple.BeanCreationHandler#init(java.lang.Class, de.innovationgate.webgate.api.simple.BeanAdapter)
	 */
	public void init(Class beanClass, BeanAdapter adapter) throws SecurityException, NoSuchMethodException {

		_adapter = adapter;

		// First try to fetch constructor with BeanAdapter, String, String
		try {
			_constructor = beanClass.getConstructor(new Class[] { BeanAdapter.class, String.class, String.class });

			int modifiers = _constructor.getModifiers();
			if (!Modifier.isPublic(modifiers)) {
				throw new IllegalArgumentException("Cannot use bean class because the default constructor is not public");
			}
		}
		catch (SecurityException e) {}
		catch (NoSuchMethodException e) {}

		if (_constructor != null) {
			_includeAdapter = true;
		}
		else {
			// Next try constructor with String, String

			_constructor = beanClass.getConstructor(new Class[] { String.class, String.class });

			int modifiers = _constructor.getModifiers();
			if (!Modifier.isPublic(modifiers)) {
				throw new IllegalArgumentException("Cannot use bean class because the default constructor is not public");
			}
		}

		// try to find method destroy
		try {
			_destructor = beanClass.getMethod("destroy", new Class[] {});
		}
		catch (SecurityException e1) {}
		catch (NoSuchMethodException e1) {}

	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.simple.BeanCreationHandler#createBean(java.lang.String, java.lang.String)
	 */
	public Object createBean(String userName, String password)
		throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
		
		if (_includeAdapter) {
			return _constructor.newInstance(new Object[] {_adapter, userName, password});
		}
		else {
			return _constructor.newInstance(new Object[] {userName, password});
		}
		
		
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.simple.BeanCreationHandler#destroyBean(java.lang.Object)
	 */
	public void destroyBean(Object bean) throws IllegalArgumentException, IllegalAccessException, InvocationTargetException {
	
		if (_destructor != null) {
			_destructor.invoke(bean, new Object[] {});
		}

	
	}

    public Object createMasterBean() throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
        return createBean(WGDatabase.MASTER_USERNAME, "");
    }

}
