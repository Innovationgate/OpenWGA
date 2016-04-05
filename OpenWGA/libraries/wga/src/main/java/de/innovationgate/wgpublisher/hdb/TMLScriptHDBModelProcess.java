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

import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGHierarchicalDatabase;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.expressions.tmlscript.TMLScriptException;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLException;

/**
 * A {@link HDBModelProcess} which executes the code of a TMLScript module
 */
public class TMLScriptHDBModelProcess implements HDBModelProcess, WGACoreAwareHDBModelProcess {
	
    private WGACore _core;
	private String _moduleName;
	private WGDatabase _db;

	/**
	 * Constructor
     * @param db App containing the module
     * @param moduleName Name of a TMLScript module to execute
	 */
	public TMLScriptHDBModelProcess(WGDatabase db,  String moduleName) {
        _db = db;
        _moduleName = moduleName;
    }

	public void run(WGContent content, Map<String, Object> params) throws Throwable {
    	try {
	        StringBuffer scriptCode = new StringBuffer();
	        scriptCode.append(retrieveModule().getCode());
	        scriptCode.append("if (run) {");
	        scriptCode.append("run(params);");
	        scriptCode.append("}");                
	        execute(scriptCode.toString(), retrieveContext(content, params));
    	} catch (Exception e) {
    		throw new TMLScriptException("Error calling hdbmodel process '" + _moduleName + "'.", e);
    	}
	}

    private void execute(String scriptCode, TMLContext context) throws Throwable {
        ExpressionEngine engine = ExpressionEngineFactory.getEngine(ExpressionEngineFactory.ENGINE_TMLSCRIPT);  
        ExpressionResult result = engine.evaluateExpression(scriptCode.toString(), context, RhinoExpressionEngine.TYPE_SCRIPT, null);
        if (result.isError()) {
            if (result.getException().getCause() != null) {
                throw result.getException().getCause();
            } else {
                throw result.getException();
            }
        }        
    }
    
    
    private WGCSSJSModule retrieveModule() throws WGAPIException {
        WGCSSJSModule module = _db.getScriptModule(_moduleName, WGScriptModule.CODETYPE_TMLSCRIPT);
        if (module == null) {
            throw new WGIllegalArgumentException("Cannot find hdb process module '" + _moduleName + "'.");
        } else {
            return module;
        }
    }
    
    private TMLContext retrieveContext(WGContent content, Map<String,Object> params) throws WGAPIException, TMLException {
       	TMLContext context = new TMLContext(content, _core, null, null);
       	TMLContext threadMainContext = TMLContext.getThreadMainContext();
       	if (threadMainContext != null) {
       	    context.importEnvironmentData(threadMainContext);
       	}            	
       	context.setvar("params", params);
       	if (params.containsKey("hdbParams")) {
       	    context.setvar("hdbParams", params.get("hdbParams"));
       	}
       	return context;    	
    }

    public void setCore(WGACore core) {
        _core = core;
    }
}
