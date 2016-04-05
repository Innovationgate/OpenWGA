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
import java.lang.reflect.Modifier;

/**
 * The default bean creation handler for {@link BeanAdapter} which just uses the default constructor of the bean
 */
public class DefaultBeanCreationHandler implements BeanCreationHandler {


	private Constructor _constructor;

	/**
	 * 
	 */
	public DefaultBeanCreationHandler() {
		super();
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.simple.BeanCreationHandler#createBean(de.innovationgate.webgate.api.simple.BeanAdapter, java.lang.String, java.lang.String)
	 */
	public Object createBean( String userName, String password) throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
		return _constructor.newInstance(new Object[] {});
	}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.simple.BeanCreationHandler#init(java.lang.Class, de.innovationgate.webgate.api.simple.BeanAdapter)
	 */
	public void init(Class beanClass, BeanAdapter adapter) throws SecurityException, NoSuchMethodException {

		_constructor = beanClass.getConstructor(new Class[] {});
		
		int modifiers = _constructor.getModifiers();
		if (!Modifier.isPublic(modifiers)) {
			throw new IllegalArgumentException("Cannot use bean class because the default constructor is not public");
		}

}

	/* (Kein Javadoc)
	 * @see de.innovationgate.webgate.api.simple.BeanCreationHandler#destroyBean(java.lang.Object)
	 */
	public void destroyBean(Object bean) {}

    public Object createMasterBean() throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException {
        return createBean(null, null);
    }

}
