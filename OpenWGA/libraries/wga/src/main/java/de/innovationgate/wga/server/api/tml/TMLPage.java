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

package de.innovationgate.wga.server.api.tml;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wga.server.api.Design;

/**
 * Represents the WebTML page being currently rendered
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public interface TMLPage {

    /**
     * Returns whether the WebTML page is actually available
     */
    public abstract boolean isAvailable();

    /**
     * Renders the given WebTML module design und the current context and the current media key
     * @param design The design object pointing to the WebTML module to render
     * @throws WGException
     */
    public abstract void render(Design design) throws WGException;

    /**
     * Renders the given WebTML module design und the given context and the current media key
     * @param design The design object pointing to the WebTML module to render
     * @param cx The WebTML context under which to render the WebTML module
     * @throws WGException
     */
    public abstract void render(Design design, Context cx) throws WGException;

    /**
     * Renders the given WebTML module design und the current context and the given media key
     * @param design The design object pointing to the WebTML module to render
     * @param mediaKey The media key of the WebTML module to render
     * @throws WGException
     */
    public abstract void render(Design design, String mediaKey) throws WGException;

    /**
     * Renders the default WebTML module for the current renderer under current context and media key
     * @param options WebTML options to set for the module to render
     */
    public abstract void renderDefault(Map<Object,Object> options) throws WGException;
    
    /**
     * Renders the default WebTML module for the current renderer under current context and media key
     */
    public abstract void renderDefault() throws WGException;

    /**
     * Renders the given WebTML module design und the given context and the given media key
     * @param design The design object pointing to the WebTML module to render
     * @param mediaKey The media key of the WebTML module to render
     * @param cx The WebTML context under which to render the WebTML module
     * @throws WGException
     */
    public abstract void render(Design design, String mediaKey, Context cx) throws WGException;
    
    
    /**
     * Renders the given WebTML module design und the given context and the given media key
     * @param design The design object pointing to the WebTML module to render
     * @param mediaKey The media key of the WebTML module to render
     * @param cx The WebTML context under which to render the WebTML module
     * @param options WebTML options to set for the module to render
     * @throws WGException
     */
    public abstract void render(Design design, String mediaKey, Context cx, Map<Object,Object> options) throws WGException;

    /**
     * Renders with parameters specified as map
     * @param map
     * @throws WGException
     */
    public abstract void render(HashMap<String,Object> map) throws WGException;

    /**
     * Renders with all values default == renderDefault()
     * @throws WGException
     */
    public abstract void render() throws WGException;

    /**
     * Writes the given string to the output
     * @param out The string to write
     * @throws WGException
     * @throws IOException
     */
    public abstract void write(String out) throws WGException, IOException;

    /**
     * Writes the given bytes to the output
     * When this is the first output then the WebTML page output mode will be set to binary. Following textual outputs will be ignored.
     * This method is ineffective if the page already had textual output.
     * @param out The bytes to write
     * @throws WGException
     * @throws IOException
     */
    public abstract void write(byte[] out) throws WGException, IOException;

    /**
     * Writes all data from the given input stream to the output
     * When this is the first output then the WebTML page output mode will be set to binary. Following textual outputs will be ignored.
     * This method is ineffective if the page already had textual output.
     * @param in InputStream
     * @throws WGException
     * @throws IOException
     */
    public abstract void write(InputStream in) throws WGException, IOException;
    
    
    /**
     * Writes all text from the given reader to the output
     * @param in Reader
     * @throws WGException
     * @throws IOException
     */
    public abstract void write(Reader in) throws WGException, IOException;
    
    /**
     * Writes the given byte to the output
     * When this is the first output then the WebTML page output mode will be set to binary. Following textual outputs will be ignored.
     * This method is ineffective if the page already had textual output.
     * @param out The bytes to write
     * @throws WGException
     * @throws IOException
     */
    public abstract void write(byte out) throws WGException, IOException;
    
    /**
     * Writes a WebTML page variable, that will be available to the rest of the call
     * @param name Name of the variable
     * @param value Value of the variable
     */
    public void setVar(String name, Object value) throws WGException;
    
    /**
     * Determines if a WebTML page variable of the given name is available
     * @param name Name of the variable
     */
    public boolean hasVar(String name) throws WGException;

    /**
     * Returns the value of a WebTML page variable
     * @param name Name of the variable
     */
    public Object getVar(String name) throws WGException;
    
    /**
     * Removes a WebTML page variable
     * @param name Name of the variable
     * @return The value stored in the removed variable
     */
    public Object removeVar(String name) throws WGException;
    

}