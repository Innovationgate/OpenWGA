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

import java.io.Serializable;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import org.apache.commons.lang.builder.HashCodeBuilder;

import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.cluster.tasks.CollectMemberInformationTask;

public class ClusterMember implements Serializable {

    private static final long serialVersionUID = 1L;

    private String _uid;

    private MemberInformation _memberInformation;

    private transient ClusterService _clusterService;

    private long _memberInformationLastFetched = 0;
    
    private static int MEMBERINFORMATION_LATENCY_SECONDS = 30;
    private static int MEMBERINFORMATION_FETCHTIMEOUT_SECONDS = 5;

    public ClusterMember(String uid, ClusterService clusterService) {
        _uid = uid;
        _clusterService = clusterService;
    }
    
    public String getUID() {
        return _uid;           
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        } else {
            return hashCode() == obj.hashCode();
        }
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder().append(_uid).toHashCode();
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(_uid);
        if (_memberInformation != null) {
            if (_memberInformation.getName() != null) {
                buf.append(" - " + _memberInformation.getName());
            }
        }
        return buf.toString();
    }

    public MemberInformation getMemberInformation() {
        refreshMemberInformation();
        return _memberInformation;
    }
    
    private void refreshMemberInformation() {
        if (System.currentTimeMillis() - _memberInformationLastFetched > MEMBERINFORMATION_LATENCY_SECONDS * 1000 || _memberInformation == null) {
            try {
                CollectMemberInformationTask task = new CollectMemberInformationTask();
                Future<MemberInformation> result = _clusterService.submitTo(this, task);
                if (result != null) {
                    _memberInformation = result.get(MEMBERINFORMATION_FETCHTIMEOUT_SECONDS, TimeUnit.SECONDS);
                    _memberInformationLastFetched = System.currentTimeMillis();
                }
            } catch (Throwable e) {
                WGFactory.getLogger().error("Failed to fetch clusterMemberInformation for '" + this + "'.", e);
            }
        }
    }

}
