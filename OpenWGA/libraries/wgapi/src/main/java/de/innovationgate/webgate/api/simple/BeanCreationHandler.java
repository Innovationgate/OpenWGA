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

import java.lang.reflect.InvocationTargetException;

/**
 * Interface of a class that is responsible for creating beans for the {@link BeanAdapter}
 */
public interface BeanCreationHandler {
    
	/**
	 * Method called on connection of the bean adapter
	 * @param beanClass The class of the Java bean from configuration
	 * @param adapter The {@link BeanAdapter} instance used
	 * @throws SecurityException
	 * @throws NoSuchMethodException
	 */
	public void init(Class beanClass, BeanAdapter adapter) throws SecurityException, NoSuchMethodException;
	
	/**
	 * Called to create a bean when it is needed
	 * @param userName The name of the user of the OpenWGA domain that the bean adapter belongs to
	 * @param password The password of that user
	 * @return A created bean
	 * @throws IllegalArgumentException
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws InvocationTargetException
	 */
	public Object createBean(String userName, String password) throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException;
	
	/**
	 * Called to create a bean for master sessions
	 * @throws IllegalArgumentException
	 * @throws InstantiationException
	 * @throws IllegalAccessException
	 * @throws InvocationTargetException
	 */
	public Object createMasterBean() throws IllegalArgumentException, InstantiationException, IllegalAccessException, InvocationTargetException;
	
	/**
	 * Called when a bean goes "out of life", so resources on it can be freed.
	 * @param bean The bean that is no more
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws InvocationTargetException
	 */
	public void destroyBean(Object bean)  throws IllegalArgumentException, IllegalAccessException, InvocationTargetException;
}
