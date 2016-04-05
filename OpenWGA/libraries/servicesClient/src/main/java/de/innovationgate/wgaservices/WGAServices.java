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

import java.util.List;

import de.innovationgate.wgaservices.types.ActionResult;
import de.innovationgate.wgaservices.types.Form;
import de.innovationgate.wgaservices.types.RemoteSession;


/**
 * super interface for all WGAServices
 *
 */
public interface WGAServices {
	

	/**
	 * Logs in to the WGA server and returns an object that represents a remote database session.
	 * @param domain The WGA domain to log in to
	 * @param user The login user
	 * @param pwd The login password
	 * @return A remote session object if the login succeeds
	 */
	public RemoteSession login(String domain, String user, String pwd) throws WGAServiceException;
	
	/**
     * Calls a remote action on the WGA server.
     * Since this method call is quite complex because of its numerous parameters it is often
     * more comfortable to use an {@link ActionCaller} instead.
     * @param session The session to use
     * @param dbKey Key of the database that contains the session
     * @param actionID ID of the action to call
     * @param executionContext Context expression specifying the execution context of the action
     * @param params Action parameters. First element in the list becomes tmlparam1 and so on.
     * @param form TMLForm object to post with the action.
     * @return Return value of the action
     */
    public ActionResult callAction(RemoteSession session, String dbKey, String actionID, String executionContext, List<Object> params, Form form) throws WGAServiceException;

    
    /**
     * Returns the access level of the current session for a given database
     * @param session The session to use
     * @param dbKey The database
     * @return A constant of WGDatabase.ACCESSLEVEL_.... (from the WGAPI)
     */
    public int getAccessLevel(RemoteSession session, String dbKey) throws WGAServiceException;
}

