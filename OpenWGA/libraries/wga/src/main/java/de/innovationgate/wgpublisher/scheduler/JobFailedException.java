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

import de.innovationgate.webgate.api.WGException;

/**
 * Thrown when a Job fails because of various reasons.
 */
public class JobFailedException extends WGException {
    
    /**
     * 
     */
    private static final long serialVersionUID = 1L;
    private String _failingTask = null;

	/**
	 * @param msg
	 */
	public JobFailedException(String msg) {
		super(msg);
	}
	
	
   public JobFailedException(String msg, Throwable cause) {
        super(msg, cause);
   }
   
   
   public JobFailedException(String msg, String failingTask, Throwable cause) {
       super(msg, cause);
       _failingTask = failingTask;
  }


public String getFailingTask() {
    return _failingTask;
}
   
}
