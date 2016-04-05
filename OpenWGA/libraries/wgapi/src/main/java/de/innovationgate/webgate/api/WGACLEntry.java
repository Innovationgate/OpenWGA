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
package de.innovationgate.webgate.api;

/**
 * Represents a single ACL entry
 */
public interface WGACLEntry {
	
	/**
	 * Entry of type user (which may also contain a group)
	 */
	public static int TYPE_USER = 1;
	/**
	 * Entry of type ROLE. This is not used yet.
	 */
	public static int TYPE_ROLE = 2;	

	/**
	 * Returns the user/group name for this entry.
	 */
	public String getName();
	/**
	 * Returns the entry type. This is a value of WGACLEntry.TYPE_...
	 */
	public int getType();
	/**
	 * Returns the access level for this entry. This is a value of WGDatabase.ACCESSLEVEL_....
	 */
	public int getLevel();
	/**
	 * Sets the access level for this entry which should be a value of WGDatabase.ACCESSLEVEL_...
	 * @param level
	 */
	public void setLevel(int level);
	/**
	 * Returns custom flags for this entry.
	 */
	public String getFlags();
	/**
	 * Sets custom flags for this entry in form of a string.
	 * @param flags
	 */
	public void setFlags(String flags);
}
