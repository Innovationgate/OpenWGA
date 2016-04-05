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

import java.util.List;

/**
 * Shared annotation data of file attachments and derivates
 */
public interface WGFileAnnotations {
    
    /**
     * An image representation representing the file type of the original file
     */
    public static final String USAGE_ICON = "icon";
    
    /**
     * An image representation of any file displaying the files contents in graphical form
     */
    public static final String USAGE_POSTER = "poster";
    

    /**
     * Returns the mime type
     * @throws WGAPIException
     */
    public String getMimeType() throws WGAPIException;

    /**
     * Sets the mime type
     * @param mimeType
     * @throws WGAPIException
     */
    public void setMimeType(String mimeType) throws WGAPIException;

    /**
     * Returns the display height in pixels for images or videos
     * @throws WGAPIException
     */
    public int getDisplayHeight() throws WGAPIException;

    /**
     * Sets the display height in pixels for images or videos
     * @param displayHeight
     * @throws WGAPIException
     */
    public void setDisplayHeight(int displayHeight) throws WGAPIException;

    /**
     * Returns the display width in pixels for images or videos
     * @throws WGAPIException
     */
    public int getDisplayWidth() throws WGAPIException;

    /**
     * Sets the display width in pixels for images or videos
     * @param displayWidth
     * @throws WGAPIException
     */
    public void setDisplayWidth(int displayWidth) throws WGAPIException;
    
    /**
     * Returns the usage of a file derivate, using constants #USAGE_...
     * This is readonly and determined by the derivate creator.
     * @throws WGAPIException
     */
    public String getUsage() throws WGAPIException;

    /**
     * Returns the names of custom fields annotated for this file
     * @throws WGAPIException
     */
    public List<String> getCustomFieldNames() throws WGAPIException;

    /**
     * Returns the value of a custom field annotated for this file
     * @param fieldName Name of the field
     * @return Value of the field
     * @throws WGAPIException
     */
    public Object getCustomFieldValue(String fieldName) throws WGAPIException;

    /**
     * Sets the value of a custom field annotated for this file
     * @param fieldName Name of the field
     * @param value Value of the field
     * @throws WGAPIException
     */
    public void setCustomFieldValue(String fieldName, Object value) throws WGAPIException;

    /**
     * Removes a custom field annotated for this file
     * @param fieldName Name of the field
     * @throws WGAPIException
     */
    public void removeCustomField(String fieldName) throws WGAPIException;
    
    /**
     * Returns the size of the file in bytes
     * @throws WGAPIException
     */
    public long getSize() throws WGAPIException;
    
    /**
     * Returns the parent document to which the file, described by these file annotations, belong
     * @throws WGAPIException
     */
    public WGDocument getParentDocument() throws WGAPIException;
    
    /**
     * returns a plain text representation of the file
     * @throws WGAPIException
     */
    public BinaryFieldData getPlainText() throws WGAPIException;
    
    /**
     * sets the plain text representation of this file
     * @param data plain text data
     * @throws WGAPIException
     */
    public void setPlainText(BinaryFieldData data) throws WGAPIException;

}