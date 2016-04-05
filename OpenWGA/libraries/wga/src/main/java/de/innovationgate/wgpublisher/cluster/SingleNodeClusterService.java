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

package de.innovationgate.wgpublisher.cluster;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;


import de.innovationgate.wga.config.ClusterConfiguration;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cluster.tasks.ClusterTask;

/**
 * single (local) node only implementation of a cluster service
 */
public class SingleNodeClusterService extends ClusterService {
    
    public SingleNodeClusterService(WGACore core) {
        super(core);
    }

    private ClusterMember _localMember;
    private ExecutorService _executor; 

    @Override
    protected void init(ClusterConfiguration config) throws Exception {
        _executor = Executors.newFixedThreadPool(1);
        _localMember = new ClusterMember("local", this);
    }

    @Override
    public void shutdown() {
        _executor.shutdown();
    }

    @Override
    public Set<ClusterMember> getMembers() {
        Set<ClusterMember> members = new HashSet<ClusterMember>();
        members.add(getLocalMember());
        return members;
    }

    @Override
    protected <V> Map<ClusterMember, Future<V>> submit(Set<ClusterMember> members, ClusterTask<V> task) {
        Map<ClusterMember, Future<V>> results = new HashMap<ClusterMember, Future<V>>();
        if (members.contains(getLocalMember())) {
            task.getContext().setWGACore(getWGACore());
            results.put(getLocalMember(), _executor.submit(task));
        }
        return results;
    }

    @Override
    public ClusterMember getLocalMember() {
        return _localMember;
    }

    @Override
    public boolean isInitialized() {
        return _executor != null;
    }

    @Override
    public String getName() {
        return "SingleNodeCluster";
    }
}
