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

package de.innovationgate.wgpublisher.so;

import com.google.gson.JsonObject;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

/**
 * A context for a scope object that provides binding to the scope objects environment
 */
public interface ScopeObjectContext {
    
    /**
     * Called after extracting state from the scope object, so the scope can optionally do some transformation
     * before setting the state to the registry.
     * @param state The original state
     * @param ref TODO
     * @param ref Reference of the object with changed state
     * @return Transformed state.
     * @throws WGException 
     */
    public JsonObject transformStateToExtract(JsonObject state, DesignResourceReference ref) throws WGException;

    /**
     * Called before injecting state to the scope object, so the scope can optionally do some transformation
     * before setting it to the object. As the given state is the original from the registry this method should
     * not directly modify it but modify and return a clone of it.
     * @param state The original state
     * @param ref TODO
     * @param ref Reference of the object with changed state
     * @return Transformed state.
     */
    public JsonObject transformStateToInject(JsonObject state, DesignResourceReference ref) throws WGException;

    /**
     * Called after the scope object has retrieved from the registry
     * @param so The scope object
     * @throws WGException
     */
    public void beforeUsage(ScopeObject so) throws WGException;

    /**
     * Called after the scope object was used
     * @param so The scope object
     * @throws WGException
     */
    public void afterUsage(ScopeObject so) throws WGException;

}
