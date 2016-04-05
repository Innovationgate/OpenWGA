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

import java.util.Iterator;

import de.innovationgate.utils.security.BCrypt10HashingScheme;
import de.innovationgate.utils.security.HashingService;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.cluster.MemberInformation;
import de.innovationgate.wgpublisher.problems.AdministrativeProblemType;
import de.innovationgate.wgpublisher.problems.Problem;
import de.innovationgate.wgpublisher.problems.ProblemSeverity;

/**
 * internal task to collect memberInformation of current cluster members
 */
public class CollectMemberInformationTask extends ClusterTask<MemberInformation> {

    private static final long serialVersionUID = 1L;

    @Override
    public MemberInformation execute() throws Exception {
        if (getContext().getWGACore() != null) {
            WGACore core = getContext().getWGACore();
            MemberInformation info = new MemberInformation();
            info.setName(core.getWgaConfiguration().getServerName());
            info.setActiveSince(core.getInstanceActiveSince());
            
            HashingService hashingService = WGA.get(core).service(HashingService.class);
            Object salt = hashingService.generateSalt();
            String keyHash = hashingService.createHash(core.getSymmetricEncryptionEngine().getKeySpec().getEncoded(), salt);
            info.setEncryptionKeyHash(keyHash);
            
            int high = 0;
            int low = 0;
            Iterator<Problem> problems = core.getProblemRegistry().getProblems(AdministrativeProblemType.class).iterator();
            while (problems.hasNext()) {
                Problem problem = problems.next();
                if (problem.getSeverity().equals(ProblemSeverity.HIGH)) {
                    high++;
                }
                if (problem.getSeverity().equals(ProblemSeverity.LOW)) {
                    low++;
                }
            }            
            info.setHighSeverityProblemCount(high);
            info.setLowSeverityProblemCount(low);
            
            return info;
        }
        return null;
    }

}
