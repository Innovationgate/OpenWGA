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
package de.innovationgate.webgate.api.templates;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.wga.common.ImmutableObject;

/**
 * Represents a (complete) key to a bean, stored in a SimpleContentSource. 
 * The key consists of the folder containing the bean and the folder-dependent key of the bean.  
 */
public class BeanKey implements ImmutableObject {

	private String _folder = null;
	private Object _key = null;

	/**
	 * Constructor, taking folder and folder-dependent key
	 * @param folder
	 * @param key
	 */
	public BeanKey(String folder, Object key) {
		this._folder = folder;
		this._key = key;
	}

	/**
	 * Returns the folder
	 */
	public String getFolder() {
		return _folder;
	}

	/**
	 * Returns the folder-dependent key
	 */
	public Object getKey() {
		return _key;
	}

	/**
	 * Returns the string representation (including encodings) for this bean key
	 */
	public String toString() {
		return encodeKey(_folder + "," + _key);
	}

	/**
	 * Performs encodings of keys that are to be used in URLs
	 * @param key The key to encode
	 * @return The encoded key, ready to be used in URLs
	 */

	public static String encodeKey(String key) {
		
		return WGUtils.strReplace(key, ".", "§", true);
		
	}
	
	/**
	 * Performs decoding of keys that were used in an URL
	 * @param key
	 * The original key including special characters
	 */
	public static String decodeKey(String key) {
		
		return WGUtils.strReplace(key, "§", ".", true);
		
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		EqualsBuilder builder = new EqualsBuilder();
		
		BeanKey otherkey = (BeanKey) obj;
		builder.append(getFolder(), otherkey.getFolder());
		builder.append(getKey(), otherkey.getKey());
		return builder.isEquals();
	}
	
	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		HashCodeBuilder builder = new HashCodeBuilder();
		builder.append(getFolder());
		builder.append(getKey());
		return builder.toHashCode();
	}

}
