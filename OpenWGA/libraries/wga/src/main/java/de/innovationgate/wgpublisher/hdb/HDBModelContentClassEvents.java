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
package de.innovationgate.wgpublisher.hdb;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import de.innovationgate.utils.WGUtils;
import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCancelledException;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;
import de.innovationgate.wga.server.api.Design;
import de.innovationgate.wga.server.api.TMLScript;
import de.innovationgate.wga.server.api.TMLScript.ObjectType;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

/**
 * Tool class to execute HDBModel content class specific events
 */
public class HDBModelContentClassEvents {
            
    private String _contentClass;
    private WGACore _core;
    private DesignResourceReference _module;
    private ObjectType _objectType;

    public HDBModelContentClassEvents(WGACore core, String contentClass, Design module, ObjectType objectType) {
        _core = core;
        _contentClass = contentClass;
        _module = module.getBaseReference();
        _objectType = objectType;
    }

	public String getName() {
		return _contentClass;
	}
    
    private void execute(HDBModelEvent event) throws Throwable {
        
        WGA wga = WGA.get();
        
        Map<String,Object> eventObjects = event.getParameter().getCcEventObjects();
        Object eventObject = eventObjects.get(_contentClass);
        if (eventObject == null) {
            eventObject = createEventObject(wga, event);
            eventObjects.put(_contentClass, eventObject);
        }
        
        Map<String,Object> params = new HashMap<String,Object>();
        params.put("$event", event);

        TMLScript tmlScript = WGA.get(retrieveContext(event)).tmlscript();
        if (tmlScript.hasProperty(eventObject, event.getType())) {
            tmlScript.callMethod(eventObject, event.getType(), params, Arrays.asList(new Object[] {event}));
        }
        
    }
    
    private TMLContext retrieveContext(HDBModelEvent event) throws WGAPIException, TMLException {
        
            TMLContext context = new TMLContext(event.getEventReceiver(), _core, null, null);
        	TMLContext threadMainContext = TMLContext.getThreadMainContext();
        	if (threadMainContext != null) {
        	    context.importEnvironmentData(threadMainContext);
        	}
        	
        	return context;
               
    }
    
    public void callModuleEventFunction(HDBModelEvent event) throws WGHierarchicalDatabaseEventCanceledException, TMLScriptException, WGCancelledException {
    	try {
	        execute(event);
    	}
    	catch (WGHierarchicalDatabaseEventCanceledException e) {
    	    throw e;
    	}
    	catch (WGCancelledException e) {
    	    throw e;
    	}
    	catch (Throwable e) {
    	    WGHierarchicalDatabaseEventCanceledException hdbCancelException = WGUtils.getCauseOfType(e, WGHierarchicalDatabaseEventCanceledException.class);
    	    if (hdbCancelException != null) {
    	        throw hdbCancelException;
    	    }
    	    
    	    WGCancelledException ctEventCancelException = WGUtils.getCauseOfType(e, WGCancelledException.class);
    	    if (ctEventCancelException != null) {
    	        throw ctEventCancelException;
    	    }
    	    
    	    throw new TMLScriptException("Error calling event listener '" + _contentClass + "." + event.getType() + "()'.", e);
    	    
    	}
    }
    
    private Object createEventObject(WGA wga, HDBModelEvent event) throws Throwable {
        Design moduleDesign = wga.design(_module);
        return wga.tmlscript().createObject(moduleDesign, _objectType);
    }

}
