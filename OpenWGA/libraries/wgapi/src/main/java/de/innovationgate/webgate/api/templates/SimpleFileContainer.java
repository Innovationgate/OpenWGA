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

import java.io.File;
import java.io.InputStream;
import java.util.Date;
import java.util.List;

import de.innovationgate.webgate.api.WGAPIException;

/**
 * Interface that should be implemented by objects that are to be used as file containers by SimpleFileRepository.
 */
public interface SimpleFileContainer {

	/**
	 * Returns the name of this container
	 */
	public String getName();
	/**
	 * Returns the names of all files in this container
	 */
	public List getFileNames();
	
	/**
     * Returns if a file of the given name is available
     */
    public boolean hasFile(String file);
    
	/**
	 * Tells the container to remove a file
 	 * @param fileName The file to remove
	 * @throws WGAPIException
	 */
	public abstract void removeFile(String fileName) throws WGAPIException;
	/**
	 * Called to attach a file to the container
	 * @param file The file to attach.
	 * @throws WGAPIException
	 */
	public abstract void attachFile(File file) throws WGAPIException;
	/**
	 * Returns the data of a file in this container as stream
	 * @param fileName The file to server
	 * @return Data stream containing file data
	 * @throws WGAPIException
	 */
	public abstract InputStream getFileData(String fileName) throws WGAPIException;
	/**
	 * Returns the size of a file in bytes
	 * @param fileName The file
	 * @return file size in bytes.
	 * @throws WGAPIException
	 */
	public abstract long getFileSize(String fileName) throws WGAPIException;
	/**
	 * Returns the creation data of the container
	 */
	public Date getCreated();
	/**
	 * Returns the date of the last modification of this container
	 */
	public Date getLastModified();

}
