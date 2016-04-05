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

package de.innovationgate.wgpublisher.auth;

import de.innovationgate.webgate.api.WGDatabase;
import de.innovationgate.webgate.api.WGException;
import de.innovationgate.webgate.api.auth.AuthenticationModule;
import de.innovationgate.wga.server.api.Domain;
import de.innovationgate.wga.server.api.WGA;
import de.innovationgate.wgpublisher.WGACore;
import de.innovationgate.wgpublisher.scheduler.ConfigurationException;
import de.innovationgate.wgpublisher.scheduler.JobContext;
import de.innovationgate.wgpublisher.scheduler.JobFailedException;
import de.innovationgate.wgpublisher.scheduler.Task;
import de.innovationgate.wgpublisher.scheduler.TaskException;
import de.innovationgate.wgpublisher.scheduler.TaskImplementation;

public class MigrateMembershipsToRelationsTask extends Task {

    @Override
    public void execute(JobContext jobContext) throws TaskException {
        
        try {
            String domainName = jobContext.getOption("domain");
            if (domainName == null) {
                throw new JobFailedException("No domain configured");
            }
            
            Domain domain = WGA.get(jobContext).domain(domainName);
            if (domain == null) {
                throw new JobFailedException("Unknown domain: " + domainName);
            }
            
            AuthenticationModule authMod = domain.auth().getModule();
            if (authMod == null) {
                throw new JobFailedException("Domain '" + domainName + "' uses no authentication");
            }
            
            if (!(authMod instanceof CSAuthModule)) {
                throw new JobFailedException("Domain '" + domainName + "' uses no content store authentication");
            }
            
            CSAuthModule csAuth = (CSAuthModule) authMod;
            csAuth.migrateMembershipsToRelations(jobContext.getLog());
            
            
        }
        catch (WGException e) {
            throw new TaskException("Exception migrating memberships to relations", e);
        }
         
        
        
    }

    @Override
    public void configure(WGACore core) throws ConfigurationException {
    }

}
