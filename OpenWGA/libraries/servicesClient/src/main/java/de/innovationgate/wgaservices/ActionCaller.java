/*******************************************************************************
 * Copyright 2009, 2010 Innovation Gate GmbH
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ******************************************************************************/
package de.innovationgate.wgaservices;

import java.util.Collections;
import java.util.List;

import de.innovationgate.wgaservices.types.ActionResult;
import de.innovationgate.wgaservices.types.Form;
import de.innovationgate.wgaservices.types.RemoteSession;

/**
* A helper object for calling remote actions on a WGA server.
* An ActionCaller object can be used to call remote actions that are stored on a specific db on the WGA server. 
* If the database hosts a design configuration (File "csconfig.xml" in file container "system") we can only call actions
* that are declared as remote actions in it.<br>
* If it does not we need access level DESIGNER on the database to call any action as remote action.<br>
* ActionCallers should be created using {@link ClientFactory#createActionCaller(WGAServices, RemoteSession, String)}.
*/
public class ActionCaller {

	    private WGAServices _services;
	    private String _db;
	    private RemoteSession _session;
	    private String _executionContext = null;

	    /**
	     * constructor that takes the neccessary parameters.
	     * @param services The WGAServices object to use
	     * @param session The remote session to use
	     * @param db The database containing the actions
	     */
	    protected ActionCaller(WGAServices services, RemoteSession session, String db) {
	        _services = services;
	        _session = session;
	        _db = db;
	    }
	    
	    /**
	     * Call an action via ID without parameters
	     * @param id The action id, i.e. the Name of the TMLScript module that stores it's code
	     * @return The return value of the action
	     * @throws WGAServiceException
	     */
	    public Object callAction(String id) throws WGAServiceException {
	        return callAction(id, null, null);
	    }
	    
	    /**
	     * Call an action via ID and with parameters
	     * @param id The action id, i.e. the Name of the TMLScript module that stores it's code
	     * @param params The action parameters in a List. List element 0 becomes tmlparam1 and so on.
	     * @return The return value of the action
	     * @throws WGAServiceException
	     */
	    @SuppressWarnings("unchecked")
		public Object callAction(String id, List params) throws WGAServiceException {
	        return callAction(id, params, null);
	    }
	    
	    /**
	     * Call an action via ID, posting a TMLForm object
	     * @param id The action id, i.e. the Name of the TMLScript module that stores it's code
	     * @param form The TMLForm object to post
	     * @return The return value of the action
		 * @throws WGAServiceException
	     */
	    public Object callAction(String id, Form form) throws WGAServiceException {
	        return callAction(id, null, form);
	    }
	    
	    /**
	     * Call an action via ID with parameters and a posted TMLForm object
	     * @param id The action id, i.e. the Name of the TMLScript module that stores it's code
	     * @param params The action parameters in a List. List element 0 becomes tmlparam1 and so on.
	     * @param form The TMLForm object to post
	     * @return The return value of the action
	     * @throws WGAServiceException
	     */
	    @SuppressWarnings("unchecked")
		public Object callAction(String id, List params, Form form) throws WGAServiceException {
	    	ActionResult result = _services.callAction(_session, _db, id, _executionContext, params, form);
	    	if (result.getForm() != null) {
	    		return result.getForm();
	    	} else {
	    		return result.getNativeResult();
	    	}	 
	    }

	    /**
	     * Returns the path of the execution context for called actions.
	     */
	    public String getExecutionContext() {
	        return _executionContext;
	    }

	    /**
	     * Sets the path for the execution context for called actions.
	     * Use WebTML context expressions when setting this property. The actions that you call will be executed under the specified context.
	     */
	    public void setExecutionContext(String executionContext) {
	        _executionContext = executionContext;
	    }

	}
