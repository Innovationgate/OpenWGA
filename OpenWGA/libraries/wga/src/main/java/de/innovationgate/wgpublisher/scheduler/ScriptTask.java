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
package de.innovationgate.wgpublisher.scheduler;

import java.util.HashMap;
import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGCSSJSModule;
import de.innovationgate.webgate.api.WGContent;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGScriptModule;
import de.innovationgate.webgate.api.WGUnavailableException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.design.DesignResourceReference;
import de.innovationgate.wgpublisher.expressions.ExpressionEngine;
import de.innovationgate.wgpublisher.expressions.ExpressionEngineFactory;
import de.innovationgate.wgpublisher.expressions.ExpressionResult;
import de.innovationgate.wgpublisher.expressions.tmlscript.RhinoExpressionEngine;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;

public class ScriptTask extends Task {

    public static final String OPTION_MODULEFROMPLUGIN = "moduleFromPlugin";
	public static final String OPTION_MODULE = "module";
    public static final String OPTION_DATABASE = "database";
    public static final String OPTION_PLUGINDB = "plugindb";
    private String _database = null;
	private String _module = null;
    private boolean _moduleFromPlugin;

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.scheduler.Task#execute(de.innovationgate.wgpublisher.scheduler.JobContext)
	 */
	public void execute(JobContext jobContext) throws TaskException {
	
	    WGA wga = WGA.get(jobContext);
	    
	    
		WGDatabase db;
		try {
			db = wga.getCore().openContentDB(_database, null, true);
		}
		catch (WGUnavailableException e) {
			throw new TaskException("Database '" + _database + "' is unavailable");
		}
        catch (WGException e) {
            throw new TaskException(e.getMessage());
        }
		if (db == null) {
			throw new TaskException("Database '" + _database + "' does not exist");
		}
		else if (db.isSessionOpen() == false) {
			throw new TaskException("Cannot open database '" + _database + "'");
		}

		WGCSSJSModule module;
		DesignResourceReference locator;
        try {
            module = db.getCSSJSModule(_module, WGScriptModule.CODETYPE_TMLSCRIPT);
            if (module == null) {
                throw new TaskException("Script module '" + _module + "' does not exist in database '" + _database + "'");
            } 
            locator = new DesignResourceReference(module);
        }
        catch (WGAPIException e) {
            throw new TaskException("Error retrieving script module '" + _module + "' from database '" + _database + "'", e);
        }
		
		
		ExpressionEngine engine = ExpressionEngineFactory.getTMLScriptEngine();
		
		
		
		
		// Create necessary context objects
        WGContent dummyContent;
        try {
            dummyContent = db.getDummyContent(db.getDefaultLanguage());
        }
        catch (WGAPIException e) {
            throw new TaskException("Unable to retrieve dummy content context from database " + db.getDbReference(), e);
        }
        
		TMLContext context;
        try {
            context = new TMLContext(dummyContent, wga.getCore(), null, null);
            context.getEnvironment().setLog(jobContext.getLog());
        }
        catch (WGAPIException e) {
            throw new TaskException("Unable to create context." + db.getDbReference(), e);
        }
		Map<String,Object> objects = new HashMap<String,Object>();
		objects.put("jobContext", jobContext);
		objects.put(RhinoExpressionEngine.PARAM_SCRIPTNAME, "TMLScript-Task running module " + _database + "/" + _module + " for scheduler job '" +  jobContext.getCurrentJob().getName() + "'");
		objects.put(RhinoExpressionEngine.PARAM_ACTIONLOCATOR, locator);
		
        String code;
        try {
            code = module.getCode();
        }
        catch (WGAPIException e) {
            throw new TaskException("Unable to retrieve code of module '" + _module + "' from database " + db.getDbReference(), e);
        }
        
		ExpressionResult result = engine.evaluateExpression(code, context, ExpressionEngine.TYPE_SCRIPT, objects);
		if (result.isError()) {
			throw new TaskException("Error executing TMLScript task", result.getException());
		}
		
		if (jobContext.getResult() == null) {
			jobContext.setResult(result.getResult());
		}
	
	}

	/**
	 * @return
	 */
	public String getDatabase() {
		return _database;
	}

	/**
	 * @return
	 */
	public String getModule() {
		return _module;
	}

	/**
	 * @param string
	 */
	public void setDatabase(String string) {
		_database = string;
	}

	/**
	 * @param string
	 */
	public void setModule(String string) {
		_module = string;
	}

	@Override
	public void configure(WGACore core)
			throws ConfigurationException {
	    _moduleFromPlugin = Boolean.parseBoolean(getOption(OPTION_MODULEFROMPLUGIN, "false"));
	    if (_moduleFromPlugin) {
	        _database = getOption(OPTION_PLUGINDB);
	    }
	    else {
	        _database = getOption(OPTION_DATABASE);
	    }
		
		_module = getOption(OPTION_MODULE);
		
		if (_database == null) {
			throw new ConfigurationException("Database was not configured");
		}
		else if (_module == null) {
			throw new ConfigurationException("Module was not configured");
		}
		
	}
	


}
