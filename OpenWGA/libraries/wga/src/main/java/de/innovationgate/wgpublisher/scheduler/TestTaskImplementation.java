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

import java.util.Iterator;

public class TestTaskImplementation implements TaskImplementation {

	/* (Kein Javadoc)
	 * @see de.innovationgate.wgpublisher.scheduler.TaskImplementation#execute(de.innovationgate.wgpublisher.scheduler.JobContext)
	 */
	public void execute(JobContext jobContext) throws JobFailedException {
	
		jobContext.getLog().info("This is java class TestTaskImplementation");
		jobContext.getLog().info("The job options:");
		Iterator opts = jobContext.getCustomOptions().keySet().iterator();
		while (opts.hasNext()) {
			Object opt = opts.next();
			jobContext.getLog().info(String.valueOf(opt) + " := " + jobContext.getCustomOptions().get(opt)); 
		}
	
	}

}
