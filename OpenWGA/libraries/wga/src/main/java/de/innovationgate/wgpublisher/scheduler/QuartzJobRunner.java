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

package de.innovationgate.wgpublisher.scheduler;

import org.quartz.JobDataMap;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.quartz.StatefulJob;

import de.innovationgate.webgate.api.WGFactory;
import de.innovationgate.wgpublisher.WGACore;

public class QuartzJobRunner implements StatefulJob {

    public void execute(JobExecutionContext arg0) throws JobExecutionException {
        
        JobDataMap dataMap = arg0.getJobDetail().getJobDataMap();
        de.innovationgate.wgpublisher.scheduler.Job job = (de.innovationgate.wgpublisher.scheduler.Job) dataMap.get("Job");
        if (job == null) {
            throw new JobExecutionException("Job does not exist");
        }
        
        WGACore core = (WGACore) dataMap.get("WGACore"); 

        Thread.currentThread().setName(("WGA Scheduler running Job '" + job.getName() + "'"));
        try {
            job.run(core, null, arg0, null);
        }
        finally {
            WGFactory.getInstance().closeSessions();
        }

    }

}
