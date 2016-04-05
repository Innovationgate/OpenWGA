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

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGNotSupportedException;
import de.innovationgate.wga.server.api.UnavailableResourceException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wga.server.api.tml.Portlet;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortlet;
import de.innovationgate.wgpublisher.webtml.portlet.TMLPortletStateTransientStorage;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class PortletPathScopeResolver implements ScopeResolver {

    @Override
    public ScopeObjectRegistry resolveObjectRegistry(WGA wga, DesignResourceReference ref) throws WGException {

        TMLContext cx = (TMLContext) wga.tmlcontext();
        if (!(cx.getPortletStateStorage() instanceof TMLPortletStateTransientStorage)) {
            throw new UnavailableResourceException("Scope PORTLETPATH is only available when using the transient portlet registry");
        }
        TMLPortlet targetPortlet = getTargetPortlet(ref, cx);
        return targetPortlet.getState().getScopeObjectRegistry();
    }


    private TMLPortlet getTargetPortlet(DesignResourceReference ref, TMLContext cx) throws WGAPIException {
        Portlet p = cx.getportlet();
        while (true) {
            if (p.isroot()) {
                p = null;
                break;
            }
            if (((TMLPortlet) p).getState().getScopeObjectRegistry().isScopeObjectExisting(ref)) {
                break;
            }
            p = p.parent();
        }
        
        TMLPortlet targetPortlet;
        if (p != null) {
            targetPortlet = (TMLPortlet) p;
            
        }
        else {
            targetPortlet = (TMLPortlet) cx.getportlet();
        }
        return targetPortlet;
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
        TMLContext cx = (TMLContext) wga.tmlcontext();
        TMLPortlet targetPortlet = getTargetPortlet(ref, cx);
        targetPortlet.markedited();
    }


}
