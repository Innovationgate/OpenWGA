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

package de.innovationgate.wgpublisher.webtml.actions;

import java.util.Map;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGUserAccess;
import de.innovationgate.wgpublisher.WGPDispatcher;
import de.innovationgate.wgpublisher.webtml.utils.TMLContext;
import de.innovationgate.wgpublisher.webtml.utils.TMLUserProfile;

public abstract class MasterAction implements Runnable {
    
	protected TMLContext _context;

	protected TMLActionLink _actionLink;

	protected TMLAction _action = null;

    protected Map<String, Object> _objects;
	
	public MasterAction(TMLContext context, TMLAction action, TMLActionLink actionLink, Map<String, Object> objects) throws WGAPIException {
	    // Fill fields
        _actionLink = actionLink;
        _action = action;
        _objects = objects;
        
        // Init master action context
         _context = TMLContext.createMasterSessionContext(context);
		
	}

	/* (Kein Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
        
		try {
		    
		    WGUserAccess originalUser = _context.getoriginaluserdata();
		    
            String taskDescr = "TMLScript Master Action: " + _action.getID();
            
            // Open database of target and main context in master thread
            WGDatabase db = _context.getdocument().getDatabase();
			db.openSession();
			if (originalUser != null) {
			    db.getSessionContext().setMasterTenantUser(originalUser.getPrimaryName());
			}
			WGDatabase mainContextDb = _context.getmaincontext().getdocument().getDatabase();
			if (mainContextDb != db) {
			    mainContextDb.openSession();
			}
			
            db.getSessionContext().setTask(taskDescr);
            
            // Eventually open pers db too so profile is available
            TMLUserProfile profile = _context.getprofile();
            
            if (profile != null && !profile.getprofile().getDatabase().isSessionOpen()) {
                WGDatabase persDB = profile.getprofile().getDatabase();
                persDB.openSession();
                persDB.getSessionContext().setTask(taskDescr);
            }

            callAction();
			
			if (profile != null) {
			    WGPDispatcher.saveUserProfile(_context.getwgacore(), profile, null);
			}

		}
		catch (Exception e) {
			_context.getwgacore().getLog().error("Error creating TML Context for master action", e);
		}
		finally {
			WGFactory.getInstance().closeSessions();
		}
		
	}
	
	public abstract void callAction() throws WGException;
	
	public void start() {
		
		Thread actionThread = new Thread(this);
		actionThread.start();
		if (_action == null || !_action.isAsync()) {
			try {
				actionThread.join();
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
	}

}