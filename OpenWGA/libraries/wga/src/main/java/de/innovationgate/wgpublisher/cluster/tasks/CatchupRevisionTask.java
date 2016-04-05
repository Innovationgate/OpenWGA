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

package de.innovationgate.wgpublisher.cluster.tasks;

import de.innovationgate.webgate.api.WGAPIException;
import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.wga.server.api.WGA;

/**
 * Performs a cache update on the given database to ensure backend changes has been processed
 * at least up to the given revision.
 * Returns the currents revision of the database.
 * 
 * Note:
 * This task does not ensure that the given revision is processed by the remote db.
 * The caller should handle the case where the revision returned is smaller than the given revision even if
 * all backend changes has been processed without errors.
 * This might be the case in situations where the backend database cluster is not in sync.
 */
public class CatchupRevisionTask extends ClusterTask<Comparable> {

    private static final long serialVersionUID = 1L;
    
    private String _dbkey;
    private Comparable _revision;

    public CatchupRevisionTask(String dbkey, Comparable revision) {
        _dbkey = dbkey;
        _revision = revision;
    }

    @Override
    public Comparable execute() throws Exception {
        WGDatabase db = WGA.get(getContext().getWGACore()).db(_dbkey);
        if (db != null) {     
            Comparable revision = db.getRevision();
            if (revision != null) {
                if (revision.compareTo(_revision) < 0) {
                    return db.catchupBackendChanges();    
                } else {
                    return revision;
                }                                
            } else {
              throw new WGAPIException("Failed to retrieve revision of database '" + _dbkey + "'");  
            }            
        } else {
            throw new IllegalArgumentException("Unknown database '" + _dbkey + "'.");
        }
    }

}
