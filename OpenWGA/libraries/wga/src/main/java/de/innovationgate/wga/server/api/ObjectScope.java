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

package de.innovationgate.wga.server.api;

import java.util.List;

import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.common.CodeCompletion;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.so.AppScopeResolver;
import de.innovationgate.wgpublisher.so.ApplicationEventReceiver;
import de.innovationgate.wgpublisher.so.CallScopeResolver;
import de.innovationgate.wgpublisher.so.FormScopeResolver;
import de.innovationgate.wgpublisher.so.PageScopeResolver;
import de.innovationgate.wgpublisher.so.PortletPathScopeResolver;
import de.innovationgate.wgpublisher.so.PortletScopeResolver;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;
import de.innovationgate.wgpublisher.so.ScopeResolver;
import de.innovationgate.wgpublisher.so.ServerScopeResolver;
import de.innovationgate.wgpublisher.so.SessionScopeResolver;
import de.innovationgate.wgpublisher.so.WebsocketScopeResolver;

/**
 * Enum declaring the valid settings for scopes of managed objects.
 * This setting determines how much objects may be created if the object is retrieved from different environments.
 * Generally, the enum value declares the entity for which one individual managed object may be created. Different
 * entities therefor receive different objects.
 * How many objects actually are created depends on the number of entities for which managed objects are actually retrieved. 
 */
@CodeCompletion(methodMode=CodeCompletion.MODE_EXCLUDE)
public enum ObjectScope {
    
        /**
         * There will only be one scope object for the whole server. Currently beta.
         */
        @CodeCompletion
        SERVER(new ServerScopeResolver()),
        
        /**
         * There will only be one scope object for every OpenWGA application using this object 
         */
        APP(new AppScopeResolver()),
        
        /**
         * There will be one scope object for every user session 
         */
        SESSION(new SessionScopeResolver()),
        
        /**
         * One scope for every rendered WebTML page which will stay active after rendering for subsequent AJAX calls and WebSocket communication.
         */
        TMLPAGE(new PageScopeResolver()),
        
        /**
         * There will be one scope object for every web call (request/response)
         */
        CALL(new CallScopeResolver()),
        
        /**
         * There will be one scope object for every individual WebTML portlet. The current portlet of the WebTML environment is used.
         */
        PORTLET(new PortletScopeResolver()),
        
        /**
         * Like portlet scope, but the first portlet to create the object will also serve the same object to all subprojects down the portlet hierarchy
         */
        PORTLETPATH(new PortletPathScopeResolver()),
        
        /**
         * There will be one scope object for every individual WebTML form.  The current form of the WebTML environment is used.
         */
        @CodeCompletion
        FORM(new FormScopeResolver()),
        
        
        /**
         * There will be one scope object for every WebSocket connection to OpenWGA. Objects on this scope and their methods will be freely available
         * for WebSocket clients to call. 
         */
        WEBSOCKET(new WebsocketScopeResolver());
        
        
        private ScopeResolver _resolver;
        
        private ObjectScope(ScopeResolver resolver) {
            _resolver = resolver;
        }

        public ScopeObjectRegistry resolve(WGA wga, DesignResourceReference ref) throws WGException {
            return _resolver.resolveObjectRegistry(wga, ref);
        }

        public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException {
            return _resolver.resolveApplicationEventReceivers(wga, dbKey, event);
        }
        
        public boolean isApplicationEventReceiver(Event.Scope scope) {
            return _resolver.isApplicationEventReceiver(scope);
        }

        public boolean needsWebsockets() {
            return _resolver.needsWebsockets();
        }

        public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException {
            _resolver.notifyScopeObjectStateChange(scopeObject, wga, ref);
        }

        public boolean isApplicationEventReceiver() {

            for (Event.Scope scope : Event.Scope.values()) {
                if (_resolver.isApplicationEventReceiver(scope)) {
                    return true;
                }
            }
            
            return false;
            
        }
        
        
        
}

