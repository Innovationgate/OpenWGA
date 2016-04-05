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

import java.util.List;

import de.innovationgate.utils.NullPlaceHolder;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.beans.csconfig.v1.Version;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.webtml.BaseTagStatus;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;

/**
 * This object represents the current design context of a TMLContext. 
 */
public interface TMLDesignContext {
    
    /**
     * Returns the associated WebTML tag with this design context 
     */
    public BaseTagStatus getTag();
    
    /**
     * Returns the portlet associated with this design context 
     * @throws WGAPIException
     */
    public TMLPortlet getPortlet() throws WGAPIException;
    
    /**
     * Sets a local WebTML variable to WebTML module scope (if available).
     * @param name Variable name
     * @param value Variable value
     * @throws WGAPIException
     */
    public void setLocalVarOnModule(String name, Object value) throws WGAPIException;
    
    /**
     * Sets a local WebTML variable to the scope of this design context
     * @param name Variable name
     * @param value Variable value
     * @throws WGAPIException
     */
    public void setLocalVar(String name, Object value) throws WGAPIException;
    
    /**
     * Retrieves the value of a local WebTML variable. Must return {@link NullPlaceHolder} instance of no variable was found.
     * @param name Variable name
     * @throws WGAPIException
     */
    public Object retrieveLocalVar(String name) throws WGAPIException;
    
    /**
     * Removes a local WebTML variable. In an environment with cascaded scopes removes the variable
     * of the given name on the nearest scope in the hierarchy. Must return {@link NullPlaceHolder} instance of no variable was found.
     * @param name Variable name
     * @return Value of removed variable
     * @throws WGAPIException
     */
    public Object removeLocalVar(String name) throws WGAPIException;
    
    /**
     * Returns a WebTML option of the current design context
     * @param name Option name
     * @throws WGException
     */
    public TMLOption getOption(String name) throws WGException;
    
    /**
     * Defines a WebTML option on WebTML module scope (if available).
     * @param name Option name
     * @param value Option value
     * @param scope Option inheritance scope. Use TMLOption.SCOPE_... constants
     * @throws WGException
     */
    public void setOption(String name, Object value, String scope)  throws WGException;
    
    /**
     * Defines a WebTML option on the scope of the current design context
     * @param name Option name
     * @param value Option value
     * @param scope Option inheritance scope. Use TMLOption.SCOPE_... constants
     * @throws WGException
     */    
    public void setDownwardOption(String name, Object value, String scope) throws WGException;
    
    /**
     * Returns the names of WebTML options known on this design context
     * @throws WGException
     */
    public List<String> getOptionNames() throws WGException;
    
    /**
     * @param Removes a WebTML option on the current design context
     * @throws WGException
     */
    public void removeOption(String name) throws WGException;
    
    /**
     * Returns the design application for this design context
     */
    public WGDatabase getDesignDB();
    
    /**
     * Returnrs the base reference of the design that uses this design context
     */
    public DesignResourceReference getBaseReference();
    
    /**
     * Gets the version compliance of the design that uses this design context
     */
    public Version getVersionCompliance();
    
    /**
     * Gets the minimum version for OpenWGA of the design that uses this design context
     */
    public Version getMinimumWGAVersion();
    
    /**
     * Returns the WebTML media key of the design that uses this design context
     */
    public String getMediaKey() throws WGException;

    /**
     * Adds a WebTML warning to the design context
     * @param cx WebTML context
     * @param msg Message
     * @param severe Whether the message indicates a cancelled tag execution
     * @param cause Optional cause exception
     */
    public void addWarning(TMLContext cx, String msg, boolean severe, Throwable cause);
    
    /**
     * Creates a delegate of this design context type for the given design reference information
     * @param designdb The design application
     * @param baseReference The base reference
     */
    public TMLDesignContext createContextDelegate(WGDatabase designdb, String baseReference);
    
    
    
}
