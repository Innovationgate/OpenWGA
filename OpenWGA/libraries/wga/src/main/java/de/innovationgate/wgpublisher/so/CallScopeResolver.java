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
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.server.api.Call;
import de.innovationgate.wga.server.api.ObjectScope;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.api.Unlocker;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public class CallScopeResolver implements ScopeResolver {

    @Override
    public ScopeObjectRegistry resolveObjectRegistry(final WGA wga, DesignResourceReference ref) throws WGException {
        
        Call call = Unlocker.unlock(wga).call();
        if (call.isAvailable()) {
            ScopeObjectRegistry sessionRegistry = (ScopeObjectRegistry) call.getJavaRequest().getAttribute(WGACore.ATTRIB_SCOPEOBJECTREGISTRY);
            if (sessionRegistry == null) {
                sessionRegistry = new ScopeObjectRegistry(ObjectScope.CALL, "Call " + call.getJavaRequest().getRequestURI(), new NoopScopeObjectContextCreator());
                call.getJavaRequest().setAttribute(WGACore.ATTRIB_SCOPEOBJECTREGISTRY, sessionRegistry);
            }
            return sessionRegistry;
        }
        else {
            throw new UnavailableResourceException("A web call is not available but needed to fetch a managed object of scope CALL");
        }
    }
    
    
    @Override
    public boolean isApplicationEventReceiver(Event.Scope scope) {
        return false;
    }
    
    @Override
    public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException {
        throw new WGNotSupportedException("No event receivers on this scope");
    }
    
    @Override
    public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException {
    }

}
