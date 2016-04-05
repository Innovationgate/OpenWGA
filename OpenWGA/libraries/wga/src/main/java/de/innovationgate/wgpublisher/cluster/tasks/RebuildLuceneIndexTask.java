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

/**
 * performs an index rebuild for the given dbkey or the whole index if no dbkey is specified
 */
public class RebuildLuceneIndexTask extends ClusterTask<Boolean> {

    private static final long serialVersionUID = 1L;

    private String _dbkey = null;
    
    public RebuildLuceneIndexTask() {        
    }
    
    public RebuildLuceneIndexTask(String dbkey) {
        _dbkey = dbkey;
    }
    
    @Override
    public Boolean execute() throws Exception {
        if (_dbkey != null) {
            getContext().getWGACore().getLuceneManager().rebuildIndex(_dbkey);
        } else {
            getContext().getWGACore().getLuceneManager().rebuildIndex();
        }
        return true;
    }

    public String getDbkey() {
        return _dbkey;
    }

    public void setDbkey(String dbkey) {
        _dbkey = dbkey;
    }

}
