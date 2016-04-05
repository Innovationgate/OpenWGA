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

import java.util.Collections;
import java.util.List;
import java.util.concurrent.Future;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.webgate.api.WGIllegalArgumentException;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.cluster.tasks.ClusterTask;

public class ExecuteRemoteEventTask extends ClusterTask<List<Object>> {

    private Event _event;
    private EventPath _path;
    private boolean _returnResult;
    
    public ExecuteRemoteEventTask(EventPath path, Event event, boolean returnResult) throws WGException {
        _path = path;
        _event = event;
        _returnResult = returnResult;
        if (event.getScope() == Event.Scope.CLUSTER) {
            throw new WGIllegalArgumentException("Event for ExecuteEventTask may not be of scope CLUSTER");
        }
    }

    @Override
    public List<Object> execute() throws Exception {
        try {
            WGDatabase db = WGA.get(getContext().getWGACore()).db(_event.getDatabaseKey());
            if (db == null) {
                throw new WGException("Database '" + _event.getDatabaseKey() + "' of remote event is unknown on this cluster node");
            }
            if (db.getRevisionObject().compareTo(_event.getDatabaseRevision()) < 0) {
                WGFactory.getInstance().getEventThread().nextRun();
            }
            
            Future<List<Object>> eventFuture = getContext().getWGACore().getEventManager().executeLocalAsyncEvent(_path, _event);
            if (_returnResult) {
                return eventFuture.get();
            }
            else {
                return Collections.emptyList();
            }
            
        }
        catch (Exception e) {
            EventManager.LOG.error("Exception executing event from remote cluster member", e);
            return Collections.emptyList();
        }
        finally {
            WGFactory.getInstance().closeSessions();
        }
        
    }

}
