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
package de.innovationgate.wgpublisher.webtml.utils;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.webtml.Base;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.Root;


public class Warning {

	private String message;
	private String tagType;
	private String tagId;
	private String database;
	private String contextWGKey;
	private String resource;
	private int sourceLine;
	private boolean severe;
	
	public Warning (BaseTagStatus tag, TMLContext context, String message, boolean severe) {
		
		this.message = message;
		
		// Defaults
		this.sourceLine = 0;
		this.tagType = "(No tag)";
        this.tagId = "(none)";
        this.resource = "(unknown)";
        this.contextWGKey = "(unknown)";
        this.database = "(unknown)";

        // Tag specifics
        if (tag != null) {
            this.sourceLine = tag.sourceLine;
            this.tagType = tag.tagClass.getName();
            this.tagType = this.tagType.substring(this.tagType.lastIndexOf(".") + 1);

            if (tag.id != null) {
                this.tagId = tag.id;
            }
		
             this.resource = tag.getDesignDBKey() + "/" + tag.getTMLModuleName() + " (" + tag.getTMLModuleMediaKey() + ")";
            
        }
        
        // Context specifics
		if (context != null && context.content() != null) {			
			try {
                this.contextWGKey = context.content().getContentKey().toString();
            }
            catch (WGAPIException e) {
                this.contextWGKey = "Error retrieving contentKey bc. of exception: " + e.getClass().getName() + " message: " + e.getMessage(); 
            }
			this.database = (String) context.content().getDatabase().getAttribute(WGACore.DBATTRIB_DBKEY);
		}
        
		this.severe = severe;
	}

	/**
	 * Gets the message
	 * @return Returns a String
	 */
	public String getMessage() {
		return message;
	}
	/**
	 * Sets the message
	 * @param message The message to set
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * Gets the tagId
	 * @return Returns a String
	 */
	public String getTagId() {
		return tagId;
	}
	/**
	 * Sets the tagId
	 * @param tagId The tagId to set
	 */
	public void setTagId(String tagId) {
		this.tagId = tagId;
	}

	/**
	 * Gets the tagType
	 * @return Returns a String
	 */
	public String getTagType() {
		return tagType;
	}
	/**
	 * Sets the tagType
	 * @param tagType The tagType to set
	 */
	public void setTagType(String tagType) {
		this.tagType = tagType;
	}

	/**
	 * Gets the contextWGKey
	 * @return Returns a String
	 */
	public String getContextWGKey() {
		return contextWGKey;
	}
	/**
	 * Sets the contextWGKey
	 * @param contextWGKey The contextWGKey to set
	 */
	public void setContextWGKey(String contextWGKey) {
		this.contextWGKey = contextWGKey;
	}

	/**
	 * Gets the resource
	 * @return Returns a String
	 */
	public String getResource() {
		return resource;
	}
	/**
	 * Sets the resource
	 * @param resource The resource to set
	 */
	public void setResource(String resource) {
		this.resource = resource;
	}
	
	public String getConsoleText() {
		return "TML " + (this.severe ? "severe " : "") + "warning - Layout: " + this.getResource() + " (Line " + getSourceLine() + ") - Tag type: " + this.getTagType() + " - Tag id: "  + this.getTagId() + " - Context WGKey: " + this.getContextWGKey() + " (" + this.database + ") - Message: " + this.getMessage();
		
	}

	/**
	 * Gets the database
	 * @return Returns a String
	 */
	public String getDatabase() {
		return database;
	}
	/**
	 * Sets the database
	 * @param database The database to set
	 */
	public void setDatabase(String database) {
		this.database = database;
	}
	
	public boolean isSevere() {
		return this.severe;
	}

	/**
	 * @return
	 */
	public String getSourceLine() {
		return (sourceLine == 0 ? "(none)" : String.valueOf(sourceLine));
	}

}

