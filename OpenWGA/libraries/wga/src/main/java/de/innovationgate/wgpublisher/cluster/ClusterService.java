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

import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Future;

import de.innovationgate.utils.security.HashingService;
import de.innovationgate.wga.config.ClusterConfiguration;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.WGACoreTimer.ProblemDeterminationTaskOccasion;
import de.innovationgate.wgpublisher.cluster.tasks.ClusterTask;
import de.innovationgate.wgpublisher.cluster.tasks.ClusterTaskContext;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.GlobalScope;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemOccasion;
import de.innovationgate.wgpublisher.problems.ProblemScope;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;
import de.innovationgate.wgpublisher.problems.ProblemType;

public abstract class ClusterService {
    
    static class Occasion implements ProblemOccasion {

        @Override
        public ProblemScope getDefaultScope() {
            return GlobalScope.INSTANCE;
        }

        @Override
        public Class<? extends ProblemType> getDefaultType() {
            return AdministrativeProblemType.class;
        }

        @Override
        public Class<?> getDefaultRefClass() {
            return ClusterService.class;
        }

        @Override
        public boolean isClearedAutomatically() {
            return true;
        }
        
    }
    
    private WGACore _core;
    
    private boolean _debug = false;

    private String _lbRoute;

    public ClusterService(WGACore core) {
        _core = core;
    }

    
    public void startup() throws Exception {
        ClusterConfiguration config = _core.getWgaConfiguration().getClusterConfiguration();
        _lbRoute = config.getLbRoute();
        init(config);
    }    

    protected abstract void init(ClusterConfiguration config) throws Exception;

    public abstract void shutdown();        
    
    public abstract Set<ClusterMember> getMembers();
    
    protected abstract <V> Map<ClusterMember,Future<V>> submit(Set<ClusterMember> members, ClusterTask<V> task);
    
    public abstract ClusterMember getLocalMember();
    
    public <V> Map<ClusterMember,Future<V>> submitTo(Set<ClusterMember> members, ClusterTask<V> task) {
        ClusterTaskContext context = task.getContext();
        context.setCaller(getLocalMember());
        return submit(members, task);
    }
    
    public <V> Map<ClusterMember,Future<V>> submitToOthers(ClusterTask<V> task) {
        Set<ClusterMember> membersForExecution = new HashSet<ClusterMember>();
        Iterator<ClusterMember> it = getMembers().iterator();
        String myUID = null;
        ClusterMember local = getLocalMember();
        if (local != null) {
            myUID = local.getUID();
        }
        while (it.hasNext()) {
            ClusterMember member = it.next();
            if (member.getUID() != null && !member.getUID().equals(myUID)) {
                membersForExecution.add(member);
            }
        }
        return submitTo(membersForExecution, task);
    }
    
    public <V> Map<ClusterMember,Future<V>> submitToAll(ClusterTask<V> task) {
        return submitTo(getMembers(), task);
    }
    
    public <V> Future<V> submitTo(ClusterMember member, ClusterTask<V> task) {
        Map<ClusterMember, Future<V>> results = submitTo(Collections.singleton(member), task);

        Iterator<Future<V>> futures = results.values().iterator(); 
        if (futures.hasNext()) {
            return futures.next();
        }
        
        return null;
    }    

    protected WGACore getWGACore() {
        return _core;
    }
    
    public abstract String getName();
    
    public InputStream readResource(ClusterMember member, URI resource) {
        return new ClusterResourceInputStream(this, member, resource);
    }
    
    public OutputStream writeResource(ClusterMember member, URI resource) {
        return new ClusterResourceOutputStream(this, member, resource);
    }
    
    public ClusterMember getMember(String uid) {
        Iterator<ClusterMember> members = getMembers().iterator();
        while (members.hasNext()) {
            ClusterMember member = members.next();
            if (member.getUID() != null && member.getUID().equals(uid)) {
                return member;
            }
        }
        return null;
    }

    public abstract boolean isInitialized();
    

    public boolean isDebug() {
        return _debug;
    }


    public void setDebug(boolean debug) {
        _debug = debug;
    }

    public String getLBRoute() {
        return _lbRoute;
    }
    
    public void checkHealth() {
        
        Occasion occ = new Occasion();
        _core.getProblemRegistry().clearProblemOccasion(occ);
        
        try {
            for (ClusterMember m : getMembers()) {
                MemberInformation i = m.getMemberInformation();
                
                if (i == null) {
                    _core.getProblemRegistry().addProblem(Problem.create(occ,  "clusterProblem.noMemberInformation", ProblemSeverity.HIGH, Problem.var("node", m.getUID())));
                    continue;
                }

                HashingService hashingService = WGA.get(_core).service(HashingService.class);
                if (!hashingService.checkPassword(_core.getSymmetricEncryptionEngine().getKeySpec().getEncoded(), i.getEncryptionKeyHash())) {
                    _core.getProblemRegistry().addProblem(Problem.create(occ,  "clusterProblem.differingKeys", ProblemSeverity.HIGH, Problem.var("node", i.getName())));
                }
            
            }
        }
        catch (Throwable t) {
            _core.getProblemRegistry().addProblem(Problem.create(occ,  "clusterProblem.clusterServiceDead", ProblemSeverity.HIGH, t));
        }
    }

}