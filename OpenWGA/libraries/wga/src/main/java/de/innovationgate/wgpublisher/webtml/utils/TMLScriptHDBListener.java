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

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEvent;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseEventCanceledException;
import de.innovationgate.webgate.api.WGHierarchicalDatabaseListenerV2;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;

public class TMLScriptHDBListener implements WGHierarchicalDatabaseListenerV2 {
            
    private String _moduleName;
    private WGACore _core;

    public TMLScriptHDBListener(WGACore core, String moduleName) {
        _core = core;
        _moduleName = moduleName;
    }

    public void preCreateContent(WGHierarchicalDatabaseEvent event) throws Throwable {                                                                                         
        callModuleEventFunction(event, "preCreateContent");
    }
        
    public void postCreateContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        callModuleEventFunction(event, "postCreateContent");      
    }
    
    public void preUpdateContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        callModuleEventFunction(event, "preUpdateContent");          
    }

    public void postUpdateContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        callModuleEventFunction(event, "postUpdateContent");        
    }

    public void preDeleteContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        callModuleEventFunction(event, "preDeleteContent");        
    }

    public void postDeleteContent(WGHierarchicalDatabaseEvent event) throws Throwable {
        callModuleEventFunction(event, "postDeleteContent");        
    }

	public void postMoveContentTo(WGHierarchicalDatabaseEvent event) throws Throwable {
		callModuleEventFunction(event, "postMoveContentTo");   
		
	}

	public void preMoveContentTo(WGHierarchicalDatabaseEvent event) throws Throwable {
		callModuleEventFunction(event, "preMoveContentTo");		
	}

	public void postMoveContentFrom(WGHierarchicalDatabaseEvent event) throws Throwable {
		callModuleEventFunction(event, "postMoveContentFrom");   
		
	}

	public void preMoveContentFrom(WGHierarchicalDatabaseEvent event) throws Throwable {
		callModuleEventFunction(event, "preMoveContentFrom");		
	}

	public String getName() {
		return _moduleName;
	}
    
    private void execute(String scriptCode, WGHierarchicalDatabaseEvent event) throws Throwable {
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);
        
        Map params = new HashMap();
        params.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "HDBModel event module '" + event.getDb().getWrappedDB().getDbReference() + "/" + _moduleName  + "', event " + event.getType());
        
        ExpressionResult result = engine.evaluateExpression(scriptCode.toString(), retrieveContext(event), RhinoExpressionEngine.TYPE_SCRIPT, params);
        if (result.isError()) {
            if (result.getException().getCause() != null) {
                throw result.getException().getCause();
            }
            else {
                throw result.getException();
            }
        }        
    }
    
    
    private WGCSSJSModule retrieveModule(WGHierarchicalDatabaseEvent event) throws WGAPIException {
        WGCSSJSModule module = event.getDb().getWrappedDB().getScriptModule(_moduleName, WGScriptModule.CODETYPE_TMLSCRIPT);
        if (module == null) {
            throw new WGIllegalArgumentException("Cannot find listener module '" + _moduleName + "'.");
        } else {
            return module;
        }
    }
    
    
    private TMLContext retrieveContext(WGHierarchicalDatabaseEvent event) throws WGAPIException, TMLException {
        // determine content for context
        WGContent content = null;
        if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_CREATE_CONTENT) || 
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_DELETE_CONTENT) || 
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_UPDATE_CONTENT) || 
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_UPDATE_CONTENT) ||
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_MOVE_CONTENT_TO) ||
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_MOVE_CONTENT_TO) || 
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_MOVE_CONTENT_FROM) ||
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_MOVE_CONTENT_FROM)) {
            content = event.getContent(); 
        } else if (event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_PRE_CREATE_CONTENT) ||
                event.getType().equals(WGHierarchicalDatabaseEvent.TYPE_POST_DELETE_CONTENT)) {
            content = event.getParentContent();
        }
        if (content != null) {
        	TMLContext context = new TMLContext(content, _core, null, null);
        	TMLContext threadMainContext = TMLContext.getThreadMainContext();
        	if (threadMainContext != null) {
        	    context.importEnvironmentData(threadMainContext);
        	}
        	
        	context.setvar("event", event);
        	return context;
        } else {
        	throw new WGIllegalArgumentException("Unable to retrieve event context.");
        }        
    }
    
    private void callModuleEventFunction(WGHierarchicalDatabaseEvent event, String functionName) throws Throwable {
    	try {
	        StringBuffer scriptCode = new StringBuffer();
	        scriptCode.append(retrieveModule(event).getCode());
	        scriptCode.append("if ("+ functionName + ") {");
	        scriptCode.append(functionName + "(event);");
	        scriptCode.append("}");                
	        execute(scriptCode.toString(), event);
    	}
    	catch (WGHierarchicalDatabaseEventCanceledException e) {
    	    throw e;
    	}
    	catch (Exception e) {
    		throw new TMLScriptException("Error calling event listener '" + _moduleName + "." + functionName + "()'.", e);
    	}
    }






}
