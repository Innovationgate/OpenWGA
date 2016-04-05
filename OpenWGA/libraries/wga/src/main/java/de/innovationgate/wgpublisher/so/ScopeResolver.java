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

import java.util.List;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

/**
 * A resolver for a managed global scope
 */
public interface ScopeResolver {
    
    /**
     * Resolves the object registry for the current scope in the current environment
     * @param wga The environment
     * @param ref Reference of an object that is to be created. Resolvers may choose their resolving strategy based on this.
     * @return A registry
     * @throws WGException
     */
    public ScopeObjectRegistry resolveObjectRegistry(WGA wga, DesignResourceReference ref) throws WGException;
    
    /**
     * Should be called when a scope object has changed its state, so the resolver can trigger necessary actions to persist this state
     * @param scopeObject TODO
     * @param wga The environment
     * @param ref Reference of the object with changed state
     * @throws WGException
     */
    public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException;
    
    /**
     * Resolves object registries that receive application events in this scope
     * @param wga The environment
     * @param dbKey Key of the app throwing the event
     * @param eventScope Scope of the event
     * @return A list of object registries plus eventually additional scope-dependent params for the event processing
     * @throws WGException
     */
    public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException;
    
    /**
     * Returns if this scope receives events of the given event scope 
     */
    public boolean isApplicationEventReceiver(Event.Scope scope);

}