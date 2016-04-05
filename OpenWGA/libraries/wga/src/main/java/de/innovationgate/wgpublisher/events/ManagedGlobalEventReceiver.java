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

package de.innovationgate.wgpublisher.events;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpSession;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGIllegalDataException;
import de.innovationgate.wga.server.api.App;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.tmlscript.ManagedTMLScriptGlobalDefinition;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptGlobal;
import de.innovationgate.wgpublisher.so.ApplicationEventReceiver;
import de.innovationgate.wgpublisher.so.ScopeObjectRegistry.ScopeObject;

public class ManagedGlobalEventReceiver implements EventReceiver {
    
    private static final String PARAMNAME_EVENT = "$event";
    private String _globalName;
    private String _method;
    private String _dbKey;

    public ManagedGlobalEventReceiver(String dbKey, String globalName, String method) {
        _dbKey = dbKey;
        _globalName = globalName;
        _method = method;
    }

    @Override
    public List<Object> execute(EventManager eventManager, Event event) throws WGException {

        WGA wga = WGA.get();
        
        WGDatabase db = wga.db(_dbKey, false);
        if (db == null) {
            throw new WGIllegalDataException("Unknown database for Managed App Global Event Receiver:" + _dbKey);
        }
        if (db.isSessionOpen()) { // Just in case
            db.closeSession();
        }
        
        TMLScriptGlobal global = eventManager.getCore().getTmlscriptGlobalRegistry().getGlobal(_globalName, db);
        if (global == null) {
            throw new WGIllegalDataException("Unknown Managed App Global Event Receiver:" + _globalName + " for db " + db.getDbReference());
        }
        
        if (!(global.getRef() instanceof ManagedTMLScriptGlobalDefinition)) {
            throw new WGIllegalDataException("Managed Global Event Receiver is no App Global:" + _globalName);
        }
        
        ManagedTMLScriptGlobalDefinition managedGlobal = (ManagedTMLScriptGlobalDefinition) global.getRef();
        if (!managedGlobal.getConfig().getScope().isApplicationEventReceiver(event.getScope())) {
            return Collections.emptyList();
        }
        
        List<Object> results = new ArrayList<Object>();
        for (ApplicationEventReceiver receiver : managedGlobal.getConfig().getScope().resolveApplicationEventReceivers(wga, event.getDatabaseKey(), event)) {
            try {
                WGA eventEnvironment = receiver.getEventEnvironment();
                
                ScopeObject obj = managedGlobal.provideGlobal(eventEnvironment, receiver.getScopeObjectRegistry(), managedGlobal.getConfig().isCreateOnEvent());
                
                Map<String,Object> namedParams = new HashMap<String,Object>();
                namedParams.putAll(receiver.getScopeEventParams());
                namedParams.put(PARAMNAME_EVENT, event);
                
                EventManager.LOG.debug("Managed global event receiver calling managed global '" + _globalName + "' (" + System.identityHashCode(obj.getObject()) + ") method '" + _method + "' for receiver " + receiver.getScopeObjectRegistry().getName());
                
                obj.beforeUsage();
                try {
                    Object result = wga.tmlscript().descriptify(eventEnvironment.tmlscript().callMethod(obj.getObject(), _method, namedParams, null), Object.class);
                    if (result != null) {
                        results.add(result);
                    }
                }
                finally {
                    obj.afterUsage();
                }

                
            }
            catch (Exception e) {
                EventManager.LOG.error("Error executing event method '" + _method + "' on global '" + _globalName + "'", e);
            }
            finally {
                WGFactory.getInstance().closeSessions();
            }
        }
        
        return results;
        
    }

    @Override
    public String getDescription() throws WGException {
        return "Managed Global Method on database " + _dbKey + ": " + _globalName + "." + _method;
    }

}
