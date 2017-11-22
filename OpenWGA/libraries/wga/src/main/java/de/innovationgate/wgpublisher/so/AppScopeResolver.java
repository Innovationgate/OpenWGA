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

import java.util.Collections;
import java.util.List;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.events.Event;
import de.innovationgate.wgpublisher.events.Event.Scope;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public class AppScopeResolver implements ScopeResolver {
    
    @Override
    public ScopeObjectRegistry resolveObjectRegistry(final WGA wga, DesignResourceReference ref) throws WGException {
    	WGDatabase db;
    	String appkey = ref.getDesignApp();
    	if(appkey!=null)	// should normaly not happen bc. of the way the DesignResourceReference is created.
    		db = wga.db(appkey);
    	else db=wga.app().db();
        return ((ScopeObjectRegistry) db.getAttribute(WGACore.DBATTRIB_SCOPEOBJECTREGISTRY));
    }
    
    
    @Override
    public boolean isApplicationEventReceiver(Event.Scope scope) {
        return (scope == Event.Scope.CLUSTER || scope == Event.Scope.SERVER);
    }
    
    @Override
    public List<ApplicationEventReceiver> resolveApplicationEventReceivers(WGA wga, String dbKey, Event event) throws WGException {
        if (event.getScope() == Scope.SERVER || event.getScope() == Scope.CLUSTER) {
            return Collections.singletonList(new ApplicationEventReceiver(wga, (ScopeObjectRegistry) wga.db(dbKey).getAttribute(WGACore.DBATTRIB_SCOPEOBJECTREGISTRY)));
        }
        
        return Collections.emptyList();
    }
    
    @Override
    public void notifyScopeObjectStateChange(ScopeObject scopeObject, WGA wga, DesignResourceReference ref) throws WGException {
    }


	@Override
	public boolean needsWebsockets() {
		// TODO Auto-generated method stub
		return false;
	}


}
